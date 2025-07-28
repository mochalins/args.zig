# args.zig

> Robust runtime parsing of compile time defined structured arguments.

# TODO

1. Recursive depth field match scanning for options maps
2. Allow for zero-allocation parsing of arguments
3. Formatted help messages
  a. Add support for `summary`, `description`, `examples`
4. Implement WTF-16 to UTF-8 equality comparison to support windows argument
   iterator without allocation.
5. Allow for custom value parsers
6. Auto-generated completions for popular shells
