#include "c_backend.h"
#include "internal.h"
#include "ast.h"
#include "file.h"
#include "options.h"
#include "ui.h"
#include "globals.h"
#include "type.h"
#include "parse.h"
#include "array.h"

#define EMIT_C(c)                                \
do {                                             \
    fputc((c), output_file);                     \
} while (0)

#define EMIT_STRING_F(_fmt, ...)                 \
do {                                             \
    fprintf(output_file, (_fmt), ##__VA_ARGS__); \
} while (0)

#define EMIT_STRING_N(_s, _len)                  \
do {                                             \
    fwrite((_s), (_len), 1, output_file);        \
} while (0)

#define EMIT_STRING(_s)     EMIT_STRING_N((_s), strlen((_s)))
#define EMIT_STRING_ID(_id) EMIT_STRING(get_string((_id)))

static void emit_prelude(void) {
    const char *prelude =
"typedef unsigned char      u8;\n"
"typedef unsigned short     u16;\n"
"typedef unsigned int       u32;\n"
"typedef unsigned long long u64;\n"
"typedef signed char        s8;\n"
"typedef short              s16;\n"
"typedef int                s32;\n"
"typedef long long          s64;\n"
"typedef float              f32;\n"
"typedef double             f64;\n"
"typedef char*              str;\n"
"\n"
"#define _builtin_stack_alloc(_n) (__builtin_alloca((_n)))\n"
"#ifdef __x86_64__\n"
"__attribute__((used, always_inline))\n"
"static inline void _builtin_outb(u8* addr, u8 b) { asm volatile ( \"outb %0, %1\" : : \"a\"(b), \"Nd\"((u16)(u64)addr) );                      }\n"
"__attribute__((used, always_inline))\n"
"static inline u8   _builtin_inb(u8* addr)        { u8 ret; asm volatile ( \"inb %1, %0\" : \"=a\"(ret) : \"Nd\"((u16)(u64)addr) ); return ret; }\n"
"#endif\n"
"\n";

    EMIT_STRING(prelude);
}

#define INDENT(_lvl)                                            \
do {                                                            \
    int _i;                                                     \
    for (_i = 0; _i < (_lvl); _i += 1) { EMIT_STRING("    "); } \
} while (0)

void do_c_backend(void) {
    u64  start_us;
    int  err;
    char exe_name[128];
    char cmd_buff[4096];

    start_us = measure_time_now_us();

    emit_prelude();

    fflush(output_file);

    verb_message("C generation took %lu us\n", measure_time_now_us() - start_us);

    if (options.c_source) { return; }

    strcpy(exe_name, options.output_name);
    if (exe_name[strlen(exe_name) - 1] == 'c'
    &&  exe_name[strlen(exe_name) - 2] == '.') {
        exe_name[strlen(exe_name) - 2] = 0;
    }

    sprintf(cmd_buff, "cat %s && cc -o %s %s -O0 -g -nostdlib -ffreestanding -fno-builtin -Wl,-e,%s",
            options.output_name, exe_name, options.output_name, get_string(program_entry->full_name));

#ifdef __APPLE__
    strcat(cmd_buff, " -lSystem");
#endif

    err = system(cmd_buff);

    if (err != 0) {
        report_simple_err("c backend failed");
    }
}
