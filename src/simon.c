#include "internal.h"
#include "globals.h"
#include "options.h"
#include "strings.h"
#include "parse.h"
#include "tls.h"
#include "ui.h"
#include "file.h"
#include "array.h"
#include "ast.h"
#include "memory.h"

int do_options(int argc, char **argv);
void do_init(void);
void do_parse(void);

int main(int argc, char **argv) {
    u64 start_us;

    start_us = measure_time_now_us();

    if (do_options(argc, argv)) { return 1; }
    do_init();

    if (array_len(options.input_files) == 0) {
        report_vague_err("no input files");
        return 1;
    }

    do_parse();

    if (options.dump_symbols) {
        show_scope(&global_scope);
    }

    verb_message("total time: %lu us\n", measure_time_now_us() - start_us);

    return 0;
}

int do_options(int argc, char **argv) {
    if (parse_options(argc, argv)) {
        print_usage();
        return 1;
    } else if (options.help) {
        print_usage();
    }

    return 0;
}

void do_init(void) {
    u64 start_us;

    start_us = measure_time_now_us();

#ifdef USE_LIBC_MALLOC
    verb_message("using libc malloc\n");
#else
    verb_message("using our own allocator\n");
#endif
    init_mem();
    init_tls();

    if (options.n_threads > 1) {
        verb_message("setting up threadpool with %d threads\n", options.n_threads);
        tp = tp_make(options.n_threads);
    } else {
        verb_message("running single-threaded\n");
    }

    init_strings();
    init_ui();
    init_file_table();

    roots        = array_make(ast_t*);
    global_scope = create_scope(NULL, AST_INVALID, NULL);

    verb_message("init took %lu us\n", measure_time_now_us() - start_us);
}

void do_parse(void) {
    u64    start_us;
    char **it;

    verb_message("parsing %d files...\n", array_len(options.input_files));

    start_us = measure_time_now_us();

    if (tp == NULL) {
        array_traverse(options.input_files, it) {
            parse_file(*it);
        }
    } else {
        array_traverse(options.input_files, it) {
            start_parsing_file_async(*it);
        }
        wait_for_parsing_async();
    }

    verb_message("total lines: %lu\n", n_lines);
    verb_message("parsing took %lu us\n", measure_time_now_us() - start_us);
}
