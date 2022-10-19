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
#include "scope.h"
#include "type.h"
#include "c_backend.h"

void do_sanity_checks(void);
int  do_options(int argc, char **argv);
void do_init(void);
void do_parse(void);
void do_check(void);
void do_backend(void);

int main(int argc, char **argv) {
    u64 start_us;

    start_us = measure_time_now_us();

    do_sanity_checks();

    set_output_is_tty();

    if (do_options(argc, argv)) { return 1; }
    if (options.help)           { return 0; }

    do_init();

    if (array_len(options.input_files) == 0) {
        report_simple_err("no input files");
        return 1;
    }

    do_parse();

    if (options.dump_symbols) {
        show_scope(global_scope);
    }

    do_check();

    do_backend();

    verb_message("total time: %lu us\n", measure_time_now_us() - start_us);

    return 0;
}

void do_sanity_checks(void) {
    ASSERT(sizeof(ast_ident_t) <= sizeof(ast_bin_expr_t), "ident doesn't fit into bin_expr");
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
    init_ifile_table();
    init_types();
    init_scopes();

    roots = array_make(ast_t*);

    verb_message("init took %lu us\n", measure_time_now_us() - start_us);
}

void do_parse(void) {
    int    n_files;
    u64    start_us;
    char **it;

    n_files = array_len(options.input_files);
    verb_message("parsing %d file%s...\n", n_files, n_files > 1 ? "s" : "");

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

void do_check(void) {
    u64 start_us;

    start_us = measure_time_now_us();

    check_all();

    verb_message("type-checking and semantic analysis took %lu us\n", measure_time_now_us() - start_us);
}

void do_backend(void) {
    u64 start_us;
    int err;

    start_us = measure_time_now_us();

    if (get_ifile(get_string_id(options.output_name)) != NULL) {
        report_simple_err("can't use '%s' as an output file because it is also being used as input",
                          options.output_name);
        return;
    }

    err = checked_open_FILE(options.output_name, "w", &output_file, NULL);

    if (err != FILE_NO_ERR) {
        report_file_err(options.output_name, err);
    }

    if (strcmp(options.backend, "c") == 0) {
        do_c_backend();
    } else {
        ASSERT(0, "invalid backend");
    }

    fclose(output_file);

    verb_message("backend took %lu us\n", measure_time_now_us() - start_us);
}
