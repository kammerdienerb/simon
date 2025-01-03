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
#include "tls.h"

void do_sanity_checks(void);
int  do_options(int argc, char **argv);
void do_init(void);
void do_parse(void);
void do_check(void);
void do_backend(void);

int main(int argc, char **argv) {
    u64 start_us;
    u64 elapsed_us;

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

    do_check();

    if (options.dump_symbols) {
        show_scope(global_scope);
    }

    if (options.verbose) {
        report_type_stats();
    }

    do_backend();

    elapsed_us = measure_time_now_us() - start_us;
    verb_message("Total time: %lu us (%lu lines/s)\n", elapsed_us, (u64)(((double)n_lines) / (((double)elapsed_us) / 1000000.0)));
    verb_message("Compilation succeeded.\n");

    return 0;
}

void do_sanity_checks(void) {
    ASSERT(sizeof(ast_ident_t) == sizeof(ast_bin_expr_t), "ident doesn't fit into bin_expr");
    ASSERT(sizeof(ast_macro_arg_expand_t) == sizeof(ast_decl_t), "decl doesn't fit into macro_arg_expand");

    ASSERT(offsetof(ast_module_t, scope) == offsetof(ast_scoped_t, scope), "scope misalign");
    ASSERT(offsetof(ast_proc_t,   scope) == offsetof(ast_scoped_t, scope), "scope misalign");
    ASSERT(offsetof(ast_struct_t, scope) == offsetof(ast_scoped_t, scope), "scope misalign");
    ASSERT(offsetof(ast_block_t,  scope) == offsetof(ast_scoped_t, scope), "scope misalign");
    ASSERT(offsetof(ast_if_t,     scope) == offsetof(ast_scoped_t, scope), "scope misalign");
    ASSERT(offsetof(ast_loop_t,   scope) == offsetof(ast_scoped_t, scope), "scope misalign");
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
/*     verb_message("using libc malloc\n"); */
#else
/*     verb_message("using our own allocator\n"); */
#endif
    init_mem();
    init_tls();

    if (options.n_threads > 1) {
/*         verb_message("setting up threadpool with %d threads\n", options.n_threads); */
        tp = tp_make(options.n_threads);
    } else {
/*         verb_message("running single-threaded\n"); */
    }

    init_strings();
    init_ui();
    init_ifile_table();
    init_types();
    init_scopes();
    init_tags();

    roots            = array_make(ast_t*);
    macro_calls      = array_make(ast_macro_call_t*);
    all_types        = array_make(ast_decl_t*);
    all_procs        = array_make(ast_decl_t*);
    all_vars         = array_make(ast_decl_t*);
    cycle_check_path = array_make(ast_t*);

    verb_message("Initialization took %lu us.\n", measure_time_now_us() - start_us);
}

void do_parse(void) {
    u64                start_us;
    char             **it;
    u64                parse_time;
    ast_macro_call_t **macro_it;
    int                n_files;

    verb_message("Parsing...\n");

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

    macro_expand_stack = array_make(ast_t*);
    array_traverse(macro_calls, macro_it) {
        expand_macro(*macro_it);
    }

    parse_time = measure_time_now_us() - start_us;

    n_files = num_ifiles();

    verb_message("Parsed       %d file%s.\n", n_files, n_files > 1 ? "s" : "");
    verb_message("Total lines: %lu\n", n_lines);
    verb_message("Parsing took %lu us.\n", parse_time);
    verb_message("Parse speed: %lu lines/s\n", (u64)(((double)n_lines) / (((double)parse_time) / 1000000.0)));
}

void do_check(void) {
    u64 start_us;

    start_us = measure_time_now_us();

    check_all();

    verb_message("Type-checking and semantic analysis took %lu us.\n", measure_time_now_us() - start_us);
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

    verb_message("Backend took %lu us.\n", measure_time_now_us() - start_us);
}
