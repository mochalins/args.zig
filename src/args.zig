const std = @import("std");

pub fn Args(
    T: type,
    options: struct {
        /// Stop parsing on encountering a separator (default "--").
        /// Caller may directly step through remaining `iterator` to retrieve
        /// arguments provided after the separator.
        /// The `iterator` may also be subsequently used to parse a different
        /// command (subcommand).
        comptime separators: type = enum { @"--" },
    },
) type {
    return struct {
        /// Parsed option results.
        options: T,
        /// Set of fields in `options` that have been parsed and filled.
        filled: std.EnumSet(OptionField),
        /// Parsed positional parameters.
        positionals: [][]const u8 = &.{},

        pub const OptionField = std.meta.FieldEnum(T);
        pub const Separator = options.separators;

        const Self = @This();

        pub fn init() Self {
            var result: Self = .{
                .options = undefined,
                .filled = .initEmpty(),
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
                }
            }

            return result;
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            if (self.positionals.len > 0) {
                allocator.free(self.positionals);
            }
            self.* = undefined;
        }

        pub fn parse(
            self: *Self,
            /// Any container (struct, union, enum) type or instantiation containing
            /// fields/declarations with the same names as fields in `T`.
            options_map: anytype,
            allocator: std.mem.Allocator,
            /// Any iterator providing arguments. If using `std.process.ArgIterator`,
            /// skip the executable name that appears as the first argument.
            iterator: anytype,
            opts: struct {
                diagnostics: ?*Diagnostics(T) = null,
            },
        ) !?Separator {
            const OptionsMap = if (comptime @TypeOf(options_map) == type)
                options_map
            else
                @TypeOf(options_map);

            const options_ti = @typeInfo(T).@"struct";

            var positionals: std.ArrayListUnmanaged([]const u8) = .empty;
            errdefer positionals.deinit(allocator);

            var raw_arg: []const u8 = "";
            const ret = parse_loop: while (true) {
                raw_arg = iterator.next() orelse break :parse_loop null;
                if (raw_arg.len == 0) continue :parse_loop;

                // Check separators.
                inline for (@typeInfo(Separator).@"enum".fields) |separator| {
                    if (std.mem.eql(u8, separator.name, raw_arg)) {
                        break :parse_loop @field(Separator, separator.name);
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
                                    comptime @field(options_map, field.name);
                                if (comptime @hasDecl(field_map, "long") or
                                    @hasField(field_map, "long"))
                                {
                                    if (!std.mem.eql(
                                        u8,
                                        @field(field_map, "long"),
                                        arg,
                                    )) {
                                        comptime continue;
                                    }
                                } else {
                                    // Skip if no "long" decl/field
                                    comptime continue;
                                }
                            } else if (!std.mem.eql(u8, field.name, arg)) {
                                comptime continue;
                            }

                            const FieldType = @FieldType(T, field.name);

                            // Handle value provided after '='.
                            if (end < remaining.len - 1 and
                                remaining[end] == '=')
                            {
                                const value_end = std.mem.indexOfScalar(
                                    u8,
                                    remaining[end + 1 ..],
                                    ',',
                                ) orelse remaining.len;
                                const value = remaining[end + 1 .. value_end];
                                @field(self.options, field.name) =
                                    try parseValue(FieldType, value);
                                remaining = remaining[value_end..];
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

                                @field(self.options, field.name) =
                                    try parseValue(FieldType, value);

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
                                comptime @field(options_map, field.name);
                            if (comptime !@hasDecl(field_map, "short") and
                                !@hasField(field_map, "short"))
                            {
                                comptime continue;
                            }

                            const FieldType = @FieldType(T, field.name);

                            // Handle value provided through '='; must last
                            // short option in combined, or terminated with a
                            // comma.
                            // E.g.
                            //   -abc=foo
                            //   -ab=foo,c=bar,d
                            if (args.len > 2 and i < args.len - 2 and
                                args[i + 1] == '=')
                            {
                                const end = std.mem.indexOfScalar(
                                    u8,
                                    args[i + 2 ..],
                                    ',',
                                ) orelse args.len;
                                const value = args[i + 2 .. end];
                                @field(self.options, field.name) =
                                    try parseValue(FieldType, value);
                                i = end;
                            }
                            // Handle boolean flag with no value string.
                            else if (FieldType == bool) {
                                @field(self.options, field.name) = true;
                            }
                            // Handle value provided in next argument.
                            else if (i == args.len - 1) {
                                const value = iterator.next() orelse
                                    return ParseError.MissingValue;

                                @field(self.options, field.name) =
                                    try parseValue(FieldType, value);
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
                    try positionals.append(allocator, raw_arg);
                }
            };

            const old_len = self.positionals.len;
            if (old_len > 0) {
                self.positionals = try allocator.realloc(
                    self.positionals,
                    old_len + positionals.items.len,
                );
                @memcpy(self.positionals[old_len..], positionals.items);
                positionals.deinit(allocator);
            } else {
                self.positionals = try positionals.toOwnedSlice(allocator);
            }

            return ret;
        }
    };
}

const ParseError = error{
    MissingRequiredOptions,
    UnknownOption,
    MissingValue,
};

fn parseValue(T: type, value_string: []const u8) !T {
    const ti = @typeInfo(T);
    val_parse: switch (ti) {
        .bool => {
            if (std.mem.eql(u8, "true", value_string)) {
                return true;
            }
            if (std.mem.eql(u8, "false", value_string)) {
                return false;
            }
            return error.InvalidBoolean;
        },
        .int => {
            return std.fmt.parseInt(T, value_string, 0);
        },
        .float => {
            return std.fmt.parseFloat(T, value_string);
        },
        .@"enum" => {
            return std.meta.stringToEnum(T, value_string) orelse
                error.InvalidEnum;
        },
        .optional => |opt| {
            const child_ti = @typeInfo(opt.child);
            break :val_parse child_ti;
        },
        .pointer => |pointer| {
            if (pointer.size == .slice and pointer.child == u8) {
                return value_string;
            } else break :val_parse .void;
        },
        else => {
            @compileError(std.fmt.comptimePrint(
                "option field type {s} unsupported",
                .{@tagName(ti)},
            ));
        },
    }
    unreachable;
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

test "initialization" {
    const allocator = std.testing.allocator;

    const MyOpts = struct {
        optional_flag: bool = false,
        required_string: []const u8,
    };

    var parsed: Args(MyOpts, .{}) = .init();
    defer parsed.deinit(allocator);

    try std.testing.expect(parsed.filled.contains(.optional_flag));
    try std.testing.expect(!parsed.filled.contains(.required_string));
}

test "unknown option parsing error" {
    const allocator = std.testing.allocator;

    const MyOpts = struct {
        optional_flag: bool = false,
        required_string: []const u8,
    };

    var parsed: Args(MyOpts, .{}) = .init();
    defer parsed.deinit(allocator);

    var args = std.mem.splitScalar(u8, "-o", ' ');

    try std.testing.expectError(
        ParseError.UnknownOption,
        parsed.parse({}, std.testing.allocator, &args, .{}),
    );
}

test "default name long option parsing" {
    const allocator = std.testing.allocator;

    const MyOpts = struct {
        optional_flag: bool = false,
        required_string: []const u8,
    };

    var parsed: Args(MyOpts, .{}) = .init();
    defer parsed.deinit(allocator);

    var args = std.mem.splitScalar(u8, "--optional_flag", ' ');

    const separator = try parsed.parse(
        {},
        std.testing.allocator,
        &args,
        .{},
    );

    try std.testing.expectEqual(null, separator);
    try std.testing.expect(parsed.options.optional_flag);
}

test "short flag parsing" {
    const allocator = std.testing.allocator;

    const MyOpts = struct {
        optional_flag: bool = false,
        required_string: []const u8,
    };

    var parsed: Args(MyOpts, .{}) = .init();
    defer parsed.deinit(allocator);

    var args = std.mem.splitScalar(u8, "-o", ' ');
    const separator = parsed.parse(struct {
        const optional_flag = struct {
            const short = 'o';
        };
    }, std.testing.allocator, &args, .{});

    try std.testing.expectEqual(null, separator);
    try std.testing.expect(parsed.options.optional_flag);
}

test "short option boolean value parsing" {
    const allocator = std.testing.allocator;

    { // Parse true.
        const MyOpts = struct {
            optional_flag: bool = false,
            required_string: []const u8,
        };

        var parsed: Args(MyOpts, .{}) = .init();
        defer parsed.deinit(allocator);

        var args = std.mem.splitScalar(u8, "-o=true", ' ');
        const separator = parsed.parse(struct {
            const optional_flag = struct {
                const short = 'o';
            };
        }, std.testing.allocator, &args, .{});

        try std.testing.expectEqual(null, separator);
        try std.testing.expect(parsed.options.optional_flag);
    }
    { // Parse false.
        const MyOpts = struct {
            optional_flag: bool = true,
            required_string: []const u8,
        };

        var parsed: Args(MyOpts, .{}) = .init();
        defer parsed.deinit(allocator);

        var args = std.mem.splitScalar(u8, "-o=false", ' ');
        const separator = parsed.parse(struct {
            const optional_flag = struct {
                const short = 'o';
            };
        }, std.testing.allocator, &args, .{});

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
        var parsed: Args(MyOpts, .{}) = .init();
        defer parsed.deinit(allocator);

        var args = std.mem.splitScalar(u8, "--custom_long", ' ');
        const separator = parsed.parse(struct {
            const optional_flag = struct {
                const long = "custom_long";
            };
        }, std.testing.allocator, &args, .{});

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

        var parsed: Args(MyOpts, .{}) = .init();
        defer parsed.deinit(allocator);

        var args = std.mem.splitScalar(u8, "--custom_long=true", ' ');
        const separator = parsed.parse(struct {
            const optional_flag = struct {
                const long = "custom_long";
            };
        }, std.testing.allocator, &args, .{});

        try std.testing.expectEqual(null, separator);
        try std.testing.expect(parsed.options.optional_flag);
    }

    { // Parse false.
        const MyOpts = struct {
            optional_flag: bool = true,
            required_string: []const u8,
        };

        var parsed: Args(MyOpts, .{}) = .init();
        defer parsed.deinit(allocator);

        var args = std.mem.splitScalar(u8, "--custom_long=false", ' ');
        const separator = parsed.parse(struct {
            const optional_flag = struct {
                const long = "custom_long";
            };
        }, std.testing.allocator, &args, .{});

        try std.testing.expectEqual(null, separator);
        try std.testing.expect(!parsed.options.optional_flag);
    }
}

test "short option string parsing" {
    const allocator = std.testing.allocator;
    const MyOpts = struct {
        optional_flag: bool = false,
        required_string: []const u8,
    };

    var parsed: Args(MyOpts, .{}) = .init();
    defer parsed.deinit(allocator);

    var args = std.mem.splitScalar(u8, "-r foo", ' ');
    const separator = parsed.parse(struct {
        const required_string = struct {
            const short = 'r';
        };
    }, std.testing.allocator, &args, .{});

    try std.testing.expectEqual(null, separator);
    try std.testing.expect(parsed.filled.contains(.required_string));
    try std.testing.expectEqualStrings(
        "foo",
        parsed.options.required_string,
    );
}
