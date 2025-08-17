const std = @import("std");

pub fn Soap(
    T: type,
    comptime options: struct {
        /// Maximum number of positionals that can be parsed. If a maximum is
        /// specified, then positionals will be stored in a buffer rather than
        /// requiring an allocated slice.
        /// By default, unlimited.
        max_positionals: ?comptime_int = null,
    },
) type {
    const ParseOptions = if (comptime needsAllocator(T) or
        options.max_positionals == null)
        struct {
            allocator: std.mem.Allocator,
            diagnostics: ?*Diagnostics(T) = null,
            separators: type = enum { @"--" },
            options_details: type = void,
        }
    else
        struct {
            diagnostics: ?*Diagnostics(T) = null,
            separators: type = enum { @"--" },
            options_details: type = void,
        };

    const Inner = struct {
        const OptionField = std.meta.FieldEnum(T);

        pub fn init(Self: type) Self {
            var result: Self = .{
                .options = undefined,
                .filled = .initEmpty(),
                .positionals = &.{},
            };
            const ti = @typeInfo(T).@"struct";
            inline for (ti.fields) |field| {
                if (comptime field.defaultValue()) |val| {
                    @field(result.options, field.name) = val;
                    result.filled.insert(@field(OptionField, field.name));
                    comptime continue;
                }
                if (comptime field.is_comptime) {
                    result.filled.insert(@field(OptionField, field.name));
                    comptime continue;
                }

                // Initialize slices to empty so they can be `realloc`ed.
                const field_ti = @typeInfo(field.type);
                if (field_ti == .pointer and
                    field_ti.pointer.size == .slice)
                {
                    @field(result.options, field.name) = &.{};
                }
            }

            return result;
        }

        pub fn parse(
            self: anytype,
            /// Arguments iterator. If using `std.process.ArgIterator`, skip
            /// the executable name that appears as the first argument.
            iterator: anytype,
            opts: ParseOptions,
        ) !?opts.separators {
            if (comptime options.max_positionals) |max| {
                if (self.positionals.len >= max) return null;
            }

            const allocator = if (comptime @hasField(ParseOptions, "allocator"))
                opts.allocator
            else
                undefined;

            const OptionsMap = opts.options_details;
            const options_ti = @typeInfo(T).@"struct";

            var positionals: std.ArrayListUnmanaged([]const u8) = .empty;
            if (comptime @hasField(ParseOptions, "allocator")) {
                errdefer positionals.deinit(allocator);
            }

            var raw_arg: []const u8 = "";
            const ret = parse_loop: while (true) {
                raw_arg = iterator.next() orelse break :parse_loop null;
                if (raw_arg.len == 0) continue :parse_loop;

                // Check separators.
                inline for (@typeInfo(opts.separators).@"enum".fields) |val| {
                    if (std.mem.eql(u8, val.name, raw_arg)) {
                        break :parse_loop @field(opts.separators, val.name);
                    }
                }

                // Parse long option.
                if (raw_arg.len > 2 and
                    raw_arg[0] == '-' and raw_arg[1] == '-')
                {
                    var remaining = raw_arg[2..];
                    while (remaining.len > 0) {
                        const end = std.mem.indexOfAny(
                            u8,
                            remaining,
                            "=,",
                        ) orelse remaining.len;
                        const arg = remaining[0..end];
                        if (opts.diagnostics) |diag| {
                            diag.last_encountered_option = .{ .long = arg };
                        }
                        inline for (options_ti.fields) |field| {
                            if (comptime field.is_comptime) comptime continue;
                            if (comptime OptionsMap != void and
                                (@hasDecl(OptionsMap, field.name) or
                                    @hasField(OptionsMap, field.name)))
                            {
                                const field_map =
                                    comptime @field(OptionsMap, field.name);
                                if (comptime !@hasDecl(field_map, "long") and
                                    !@hasField(field_map, "long"))
                                {
                                    comptime continue;
                                }
                                if (!std.mem.eql(
                                    u8,
                                    @field(field_map, "long"),
                                    arg,
                                )) {
                                    comptime continue;
                                }
                            } else if (!std.mem.eql(u8, field.name, arg)) {
                                comptime continue;
                            }

                            const FieldType = @FieldType(T, field.name);

                            // Handle value(s) provided after '='.
                            if (end < remaining.len - 1 and
                                remaining[end] == '=')
                            {
                                var start = end + 1;
                                while (remaining.len > 0) {
                                    const field_ti = @typeInfo(FieldType);
                                    const value_end =
                                        if (comptime field_ti == .pointer and
                                        field_ti.pointer.size == .slice and
                                        FieldType != []const u8)
                                            std.mem.indexOfScalarPos(
                                                u8,
                                                remaining,
                                                start,
                                                ',',
                                            ) orelse remaining.len
                                        else
                                            remaining.len;
                                    const value =
                                        remaining[start..value_end];
                                    try parseValue(
                                        &@field(self.options, field.name),
                                        allocator,
                                        value,
                                    );
                                    const rem_start =
                                        @min(value_end + 1, remaining.len);
                                    remaining = remaining[rem_start..];
                                    start = 0;
                                }
                            }
                            // Handle boolean flag with no value string.
                            else if (FieldType == bool) {
                                @field(self.options, field.name) = true;
                                const new_rem = @min(end + 1, remaining.len);
                                remaining = remaining[new_rem..];
                            }
                            // Handle value provided in next argument.
                            else if (end == remaining.len) {
                                const value = iterator.next() orelse
                                    return ParseError.MissingValue;

                                try parseValue(
                                    &@field(self.options, field.name),
                                    allocator,
                                    value,
                                );

                                const new_rem = @min(end + 1, remaining.len);
                                remaining = remaining[new_rem..];
                            } else return ParseError.MissingValue;

                            self.filled.insert(
                                @field(OptionField, field.name),
                            );
                            comptime break;
                        } else {
                            return ParseError.UnknownOption;
                        }
                    }
                }

                // Parse short option.
                else if (raw_arg.len > 1 and raw_arg[0] == '-') {
                    const args = raw_arg[1..];
                    var i: usize = 0;
                    while (i < args.len) {
                        defer i += 1;
                        const arg = args[i];
                        if (opts.diagnostics) |diag| {
                            diag.last_encountered_option = .{ .short = arg };
                        }
                        inline for (options_ti.fields) |field| {
                            if (comptime field.is_comptime) comptime continue;
                            if (comptime OptionsMap == void or
                                (!@hasDecl(OptionsMap, field.name) and
                                    !@hasField(OptionsMap, field.name)))
                            {
                                comptime continue;
                            }
                            const field_map =
                                comptime @field(OptionsMap, field.name);
                            if (comptime !@hasDecl(field_map, "short") and
                                !@hasField(field_map, "short"))
                            {
                                comptime continue;
                            }
                            if (@field(field_map, "short") != arg) {
                                comptime continue;
                            }

                            const FieldType = @FieldType(T, field.name);

                            // Handle value provided through '='; must be last
                            // short option in combined.
                            // E.g.
                            //   -abc=foo
                            //   -abc=foo1,foo2,foo3
                            if (args.len > 2 and i < args.len - 2 and
                                args[i + 1] == '=')
                            {
                                var start = i + 2;
                                while (i < args.len) {
                                    const field_ti = @typeInfo(FieldType);
                                    const end =
                                        if (comptime field_ti == .pointer and
                                        field_ti.pointer.size == .slice and
                                        FieldType != []const u8)
                                            std.mem.indexOfScalarPos(
                                                u8,
                                                args,
                                                start,
                                                ',',
                                            ) orelse args.len
                                        else
                                            args.len;
                                    const value = args[start..end];
                                    try parseValue(
                                        &@field(self.options, field.name),
                                        allocator,
                                        value,
                                    );
                                    i = end + 1;
                                    start = i;
                                }
                            }
                            // Handle boolean flag with no value string.
                            else if (FieldType == bool) {
                                @field(self.options, field.name) = true;
                            }
                            // Handle value provided in next argument.
                            else if (i == args.len - 1) {
                                const value = iterator.next() orelse
                                    return ParseError.MissingValue;

                                try parseValue(
                                    &@field(self.options, field.name),
                                    allocator,
                                    value,
                                );
                            } else return ParseError.MissingValue;

                            self.filled.insert(
                                @field(OptionField, field.name),
                            );
                            comptime break;
                        } else {
                            return ParseError.UnknownOption;
                        }
                    }
                }

                // Parse positional
                else {
                    if (comptime options.max_positionals) |max| {
                        const buf = &self._positionals_buffer;
                        buf[self.positionals.len] = raw_arg;
                        self.positionals = buf[0 .. self.positionals.len + 1];
                        if (self.positionals.len == max) {
                            return null;
                        }
                    } else {
                        try positionals.append(allocator, raw_arg);
                    }
                }
            };

            if (comptime options.max_positionals == null) {
                const old_len = self.positionals.len;
                if (old_len > 0) {
                    self.positionals = try allocator.realloc(
                        self.positionals,
                        old_len + positionals.items.len,
                    );
                    @memcpy(self.positionals[old_len..], positionals.items);
                    positionals.deinit(allocator);
                } else {
                    self.positionals =
                        try positionals.toOwnedSlice(allocator);
                }
            }

            return ret;
        }
    };

    const NoAllocator = struct {
        /// Parsed option results.
        options: T,
        /// Set of fields in `options` that have been parsed and filled.
        filled: std.EnumSet(OptionField),
        /// Parsed positional parameters.
        positionals: [][]const u8,
        _positionals_buffer: if (options.max_positionals) |max|
            [max][]const u8
        else
            void = undefined,

        pub const OptionField = Inner.OptionField;

        const Self = @This();

        pub fn init() Self {
            return Inner.init(Self);
        }

        pub fn parse(
            self: *Self,
            /// Any iterator type that returns an optional string on `next()`.
            /// If using `std.process.ArgIterator`, skip the executable name
            /// that appears as the first argument before passing here.
            iterator: anytype,
            opts: ParseOptions,
        ) !?opts.separators {
            return Inner.parse(
                self,
                iterator,
                opts,
            );
        }
    };

    const NeedsAllocator = struct {
        /// Parsed option results.
        options: T,
        /// Set of fields in `options` that have been parsed and filled.
        filled: std.EnumSet(OptionField),
        /// Parsed positional parameters.
        positionals: [][]const u8,
        _positionals_buffer: if (options.max_positionals) |max|
            [max][]const u8
        else
            void = undefined,

        pub const OptionField = Inner.OptionField;

        const Self = @This();

        pub fn init() Self {
            return Inner.init(Self);
        }

        fn deinitSlice(slice: anytype, allocator: std.mem.Allocator) void {
            const ti = comptime @typeInfo(@TypeOf(slice)).pointer;
            const child_ti = comptime @typeInfo(ti.child);
            if (comptime child_ti == .pointer and
                child_ti.pointer.size == .slice and
                ti.child != []const u8)
            {
                for (slice) |item| {
                    deinitSlice(item, allocator);
                }
            }
            allocator.free(slice);
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            if (comptime options.max_positionals == null) {
                if (self.positionals.len > 0) {
                    allocator.free(self.positionals);
                }
            }
            const ti = comptime @typeInfo(@TypeOf(self.options)).@"struct";
            inline for (ti.fields) |field| {
                if (comptime field.is_comptime) comptime continue;
                if (!self.filled.contains(@field(OptionField, field.name))) {
                    comptime continue;
                }
                const field_ti = comptime @typeInfo(field.type);
                if (comptime field_ti != .pointer or
                    field_ti.pointer.size != .slice)
                {
                    comptime continue;
                }
                if (comptime field.type == []const u8) comptime continue;
                deinitSlice(@field(self.options, field.name), allocator);
            }
            self.* = undefined;
        }

        pub fn parse(
            self: *Self,
            /// Arguments iterator. If using `std.process.ArgIterator`, skip
            /// the executable name that appears as the first argument.
            iterator: anytype,
            opts: ParseOptions,
        ) !?opts.separators {
            return Inner.parse(
                self,
                iterator,
                opts,
            );
        }
    };

    return if (options.max_positionals == null or needsAllocator(T))
        NeedsAllocator
    else
        NoAllocator;
}

const ParseError = error{
    MissingRequiredOptions,
    UnknownOption,
    MissingValue,
};

fn parseValue(
    result: anytype,
    allocator: std.mem.Allocator,
    value_string: []const u8,
) !void {
    const T = @typeInfo(@TypeOf(result)).pointer.child;
    const ti = @typeInfo(T);

    val_parse: switch (comptime ti) {
        .bool => {
            if (std.mem.eql(u8, "true", value_string)) {
                result.* = true;
            } else if (std.mem.eql(u8, "false", value_string)) {
                result.* = false;
            } else return error.InvalidBoolean;
        },
        .int => result.* = std.fmt.parseInt(T, value_string, 0),
        .float => result.* = std.fmt.parseFloat(T, value_string),
        .@"enum" => result.* = std.meta.stringToEnum(value_string) orelse
            return error.InvalidEnum,
        .optional => |opt| {
            const child_ti = @typeInfo(opt.child);
            break :val_parse child_ti;
        },
        .pointer => |pointer| {
            // Special case string types.
            if (comptime T == []const u8) {
                result.* = value_string;
                return;
            }

            if (comptime pointer.size != .slice) {
                break :val_parse .void;
            }

            // Only support non-nested slices (except when child is string).
            const child_ti = @typeInfo(pointer.child);
            if (comptime child_ti == .pointer and
                child_ti.pointer.size == .slice and
                pointer.child != []const u8)
            {
                @compileError("nested slice option types not supported");
            }

            result.* = try allocator.realloc(result.*, result.len + 1);
            return parseValue(
                &result.*[result.len - 1],
                allocator,
                value_string,
            );
        },
        else => {
            @compileError(std.fmt.comptimePrint(
                "option field type {s} unsupported",
                .{@tagName(ti)},
            ));
        },
    }
}

pub fn Diagnostics(T: type) type {
    return struct {
        parsed_options: std.EnumSet(std.meta.FieldEnum(T)) = .initEmpty(),
        last_encountered_option: ?union(enum) {
            short: u8,
            long: []const u8,
        } = null,
    };
}

fn needsAllocator(T: type) bool {
    const ti = @typeInfo(T).@"struct";
    inline for (ti.fields) |field| {
        const field_ti = @typeInfo(field.type);
        switch (field_ti) {
            .pointer => {
                if (field.type == []const u8) {
                    comptime continue;
                }
                return true;
            },
            else => comptime continue,
        }
    }
    return false;
}

test "initialization" {
    const allocator = std.testing.allocator;

    const MyOpts = struct {
        optional_flag: bool = false,
        required_string: []const u8,
    };

    { // Unlimited positionals.
        var parsed: Soap(MyOpts, .{}) = .init();
        defer parsed.deinit(allocator);

        try std.testing.expect(parsed.filled.contains(.optional_flag));
        try std.testing.expect(!parsed.filled.contains(.required_string));
        try std.testing.expectEqual(0, parsed.positionals.len);
    }

    { // Limited positionals.
        var parsed: Soap(MyOpts, .{ .max_positionals = 2 }) = .init();

        try std.testing.expect(parsed.filled.contains(.optional_flag));
        try std.testing.expect(!parsed.filled.contains(.required_string));
        try std.testing.expectEqual(2, parsed._positionals_buffer.len);
        try std.testing.expectEqual(0, parsed.positionals.len);
    }
}

test "unknown option parsing error" {
    const MyOpts = struct {
        optional_flag: bool = false,
        required_string: []const u8,
    };

    var parsed: Soap(MyOpts, .{}) = .init();
    defer parsed.deinit(std.testing.allocator);

    var args = std.mem.splitScalar(u8, "-o", ' ');

    try std.testing.expectError(
        ParseError.UnknownOption,
        parsed.parse(&args, .{
            .allocator = std.testing.allocator,
        }),
    );
}

test "default name long option parsing" {
    const MyOpts = struct {
        optional_flag: bool = false,
        required_string: []const u8,
    };

    var parsed: Soap(MyOpts, .{}) = .init();
    defer parsed.deinit(std.testing.allocator);

    var args = std.mem.splitScalar(u8, "--optional_flag", ' ');

    const separator = try parsed.parse(&args, .{
        .allocator = std.testing.allocator,
    });

    try std.testing.expectEqual(null, separator);
    try std.testing.expect(parsed.options.optional_flag);
}

test "short flag parsing" {
    const MyOpts = struct {
        optional_flag: bool = false,
        required_string: []const u8,
    };

    var parsed: Soap(MyOpts, .{}) = .init();
    defer parsed.deinit(std.testing.allocator);

    var args = std.mem.splitScalar(u8, "-o", ' ');
    const separator = try parsed.parse(&args, .{
        .allocator = std.testing.allocator,
        .options_details = struct {
            const optional_flag = struct {
                const short = 'o';
            };
        },
    });

    try std.testing.expectEqual(null, separator);
    try std.testing.expect(parsed.options.optional_flag);
}

test "short option boolean value parsing" {
    { // Parse true.
        const MyOpts = struct {
            optional_flag: bool = false,
            required_string: []const u8,
        };

        var parsed: Soap(MyOpts, .{}) = .init();
        defer parsed.deinit(std.testing.allocator);

        var args = std.mem.splitScalar(u8, "-o=true", ' ');
        const separator = try parsed.parse(&args, .{
            .allocator = std.testing.allocator,
            .options_details = struct {
                const optional_flag = struct {
                    const short = 'o';
                };
            },
        });

        try std.testing.expectEqual(null, separator);
        try std.testing.expect(parsed.options.optional_flag);
    }
    { // Parse false.
        const MyOpts = struct {
            optional_flag: bool = true,
            required_string: []const u8,
        };

        var parsed: Soap(MyOpts, .{}) = .init();
        defer parsed.deinit(std.testing.allocator);

        var args = std.mem.splitScalar(u8, "-o=false", ' ');
        const separator = try parsed.parse(&args, .{
            .allocator = std.testing.allocator,
            .options_details = struct {
                const optional_flag = struct {
                    const short = 'o';
                };
            },
        });

        try std.testing.expectEqual(null, separator);
        try std.testing.expect(!parsed.options.optional_flag);
    }
}

test "long flag parsing" {
    const allocator = std.testing.allocator;

    const MyOpts = struct {
        optional_flag: bool = false,
        required_string: []const u8,
    };

    {
        var parsed: Soap(MyOpts, .{}) = .init();
        defer parsed.deinit(allocator);

        var args = std.mem.splitScalar(u8, "--custom_long", ' ');
        const separator = try parsed.parse(&args, .{
            .allocator = std.testing.allocator,
            .options_details = struct {
                const optional_flag = struct {
                    const long = "custom_long";
                };
            },
        });

        try std.testing.expectEqual(null, separator);
        try std.testing.expect(parsed.options.optional_flag);
    }
}

test "long option boolean value parsing" {
    const allocator = std.testing.allocator;

    { // Parse true.
        const MyOpts = struct {
            optional_flag: bool = false,
            required_string: []const u8,
        };

        var parsed: Soap(MyOpts, .{}) = .init();
        defer parsed.deinit(allocator);

        var args = std.mem.splitScalar(u8, "--custom_long=true", ' ');
        const separator = try parsed.parse(&args, .{
            .allocator = std.testing.allocator,
            .options_details = struct {
                const optional_flag = struct {
                    const long = "custom_long";
                };
            },
        });

        try std.testing.expectEqual(null, separator);
        try std.testing.expect(parsed.options.optional_flag);
    }

    { // Parse false.
        const MyOpts = struct {
            optional_flag: bool = true,
            required_string: []const u8,
        };

        var parsed: Soap(MyOpts, .{}) = .init();
        defer parsed.deinit(allocator);

        var args = std.mem.splitScalar(u8, "--custom_long=false", ' ');
        const separator = try parsed.parse(&args, .{
            .allocator = std.testing.allocator,
            .options_details = struct {
                const optional_flag = struct {
                    const long = "custom_long";
                };
            },
        });

        try std.testing.expectEqual(null, separator);
        try std.testing.expect(!parsed.options.optional_flag);
    }
}

test "short option string parsing" {
    const MyOpts = struct {
        optional_flag: bool = false,
        required_string: []const u8,
    };

    var parsed: Soap(MyOpts, .{}) = .init();
    defer parsed.deinit(std.testing.allocator);

    var args = std.mem.splitScalar(u8, "-r foo", ' ');
    const separator = try parsed.parse(&args, .{
        .allocator = std.testing.allocator,
        .options_details = struct {
            const required_string = struct {
                const short = 'r';
            };
        },
    });

    try std.testing.expectEqual(null, separator);
    try std.testing.expect(parsed.filled.contains(.required_string));
    try std.testing.expectEqualStrings(
        "foo",
        parsed.options.required_string,
    );
}

test "allocated positionals" {
    const MyOpts = struct {
        optional_flag: bool = false,
        required_string: []const u8,
    };

    var parsed: Soap(MyOpts, .{}) = .init();
    defer parsed.deinit(std.testing.allocator);

    var args = std.mem.splitScalar(u8, "foo bar baz", ' ');
    const separator = try parsed.parse(&args, .{
        .allocator = std.testing.allocator,
    });

    try std.testing.expectEqual(null, separator);
    try std.testing.expectEqual(3, parsed.positionals.len);
    try std.testing.expectEqualStrings("foo", parsed.positionals[0]);
    try std.testing.expectEqualStrings("bar", parsed.positionals[1]);
    try std.testing.expectEqualStrings("baz", parsed.positionals[2]);
}

test "buffered positionals" {
    const MyOpts = struct {
        optional_flag: bool = false,
        required_string: []const u8,
    };

    var parsed: Soap(MyOpts, .{ .max_positionals = 3 }) = .init();

    var args = std.mem.splitScalar(u8, "foo bar baz", ' ');
    const separator = try parsed.parse(&args, .{});

    try std.testing.expectEqual(null, separator);
    try std.testing.expectEqual(3, parsed.positionals.len);
    try std.testing.expectEqualStrings("foo", parsed.positionals[0]);
    try std.testing.expectEqualStrings("bar", parsed.positionals[1]);
    try std.testing.expectEqualStrings("baz", parsed.positionals[2]);
}

test "combined short flags" {
    const MyOpts = struct {
        flag1: bool = false,
        flag2: bool = false,
        flag3: bool = false,
    };

    var parsed: Soap(MyOpts, .{ .max_positionals = 1 }) = .init();

    var args = std.mem.splitScalar(u8, "-ac", ' ');
    const separator = try parsed.parse(&args, .{
        .options_details = struct {
            const flag1 = struct {
                const short = 'a';
            };
            const flag2 = struct {
                const short = 'b';
            };
            const flag3 = struct {
                const short = 'c';
            };
        },
    });

    try std.testing.expectEqual(null, separator);
    try std.testing.expect(parsed.options.flag1);
    try std.testing.expect(!parsed.options.flag2);
    try std.testing.expect(parsed.options.flag3);
}

test "combined short options" {
    const MyOpts = struct {
        flag1: bool = false,
        flag2: bool = false,
        option: []const u8,
    };

    var parsed: Soap(MyOpts, .{ .max_positionals = 1 }) = .init();

    var args = std.mem.splitScalar(u8, "-ao foo", ' ');
    const separator = try parsed.parse(&args, .{
        .options_details = struct {
            const flag1 = struct {
                const short = 'a';
            };
            const flag2 = struct {
                const short = 'b';
            };
            const option = struct {
                const short = 'o';
            };
        },
    });

    try std.testing.expectEqual(null, separator);
    try std.testing.expect(parsed.options.flag1);
    try std.testing.expect(!parsed.options.flag2);
    try std.testing.expectEqualStrings("foo", parsed.options.option);
}

test "combined long options" {
    const MyOpts = struct {
        flag1: bool = false,
        flag2: bool = false,
        option: []const u8,
    };

    var parsed: Soap(MyOpts, .{ .max_positionals = 1 }) = .init();

    var args = std.mem.splitScalar(u8, "--flag1,opt foo", ' ');
    const separator = try parsed.parse(&args, .{
        .options_details = struct {
            const flag1 = struct {
                const long = "flag1";
            };
            const flag2 = struct {
                const long = "flag2";
            };
            const option = struct {
                const long = "opt";
            };
        },
    });

    try std.testing.expectEqual(null, separator);
    try std.testing.expect(parsed.options.flag1);
    try std.testing.expect(!parsed.options.flag2);
    try std.testing.expectEqualStrings("foo", parsed.options.option);
}

test "long option slice multiple redeclaration" {
    const MyOpts = struct {
        flags: []bool = &.{},
        options: [][]const u8,
    };

    var parsed: Soap(MyOpts, .{ .max_positionals = 1 }) = .init();
    defer parsed.deinit(std.testing.allocator);

    var args = std.mem.splitScalar(
        u8,
        "--flags=true,true --opt foo --flags false --opt=bar ",
        ' ',
    );
    const separator = try parsed.parse(&args, .{
        .allocator = std.testing.allocator,
        .options_details = struct {
            const flags = struct {
                const long = "flags";
            };
            const nested_flags = struct {
                const long = "nf";
            };
            const options = struct {
                const long = "opt";
            };
        },
    });

    try std.testing.expectEqual(null, separator);
    try std.testing.expectEqual(3, parsed.options.flags.len);
    try std.testing.expect(parsed.options.flags[0]);
    try std.testing.expect(parsed.options.flags[1]);
    try std.testing.expect(!parsed.options.flags[2]);
    try std.testing.expectEqual(2, parsed.options.options.len);
    try std.testing.expectEqualStrings("foo", parsed.options.options[0]);
    try std.testing.expectEqualStrings("bar", parsed.options.options[1]);
}
