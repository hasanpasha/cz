// const std = @import("std");

// pub fn lastIndexOfScalar(comptime slice: []const u8, comptime value: u8) comptime_int {
//     comptime {
//         var i: comptime_int = slice.len;
//         while (i != 0) {
//             i -= 1;
//             if (slice[i] == value) return i;
//         }
//         return -1;
//     }
// }

// fn is_uppercase(comptime c: u8) bool {
//     return (c >= 'A' and c <= 'Z');
// }

// pub fn accept_fn_name_for_type(comptime T: type) []const u8 {
//     const full_name = @typeName(T);
//     const dot_index = lastIndexOfScalar(full_name, '.');

//     const name = comptime if (dot_index >= 0)
//         full_name[(dot_index + 1)..] // drop prefix before last `.`
//     else
//         full_name;

//     const accept_prefix = "accept_";

//     var extra: comptime_int = 0;
//     for (name, 0..) |c, i| {
//         if (is_uppercase(c)) {
//             if (i > 0 and !(is_uppercase(name[i - 1])))
//                 extra += 1;
//         }
//     }

//     var buffer: [name.len + accept_prefix.len + extra]u8 = undefined;
//     for (accept_prefix, 0..) |c, i| {
//         buffer[i] = c;
//     }

//     var i: usize = 0;
//     var ci: usize = accept_prefix.len;
//     while (ci < buffer.len) {
//         const c = name[i];
//         if (is_uppercase(c)) {
//             if (i > 0 and !(is_uppercase(name[i - 1]))) {
//                 buffer[ci] = '_';
//                 buffer[ci + 1] = c + 32;
//                 i += 1;
//                 ci += 2;
//             } else {
//                 buffer[ci] = c + 32;
//                 i += 1;
//                 ci += 1;
//             }
//         } else {
//             buffer[ci] = c;
//             i += 1;
//             ci += 1;
//         }
//     }

//     return buffer[0..];
// }

pub fn AcceptParentFn(P: type) fn (self: P, visitor: anytype, ReturnType: type) anyerror!type {
    return struct {
        pub fn f(self: P, visitor: anytype, ReturnType: type) anyerror!ReturnType {
            return switch (self) {
                inline else => |node, tag| {
                    const name = comptime "accept_" ++ @tagName(tag);
                    const visitor_T = @TypeOf(visitor);
                    const type_info = @typeInfo(visitor_T);
                    const impl_T = if (type_info == .pointer)
                        type_info.pointer.child
                    else
                        visitor_T;

                    if (!@hasDecl(impl_T, name))
                        @compileError("Visitor `" ++ @typeName(impl_T) ++ "` does not impelemnt `" ++ name ++ "`.");

                    const field = @field(impl_T, name);
                    return field(visitor, node);
                },
            };
        }
    }.f;
}
