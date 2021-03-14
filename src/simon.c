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

void do_sanity_checks(void);
int  do_options(int argc, char **argv);
void do_init(void);
void do_parse(void);
void do_resolve_symbols(void);
void do_check(void);

int main(int argc, char **argv) {
    u64 start_us;

    start_us = measure_time_now_us();

    do_sanity_checks();

    if (do_options(argc, argv)) { return 1; }
    if (options.help)           { return 0; }

    do_init();

    if (array_len(options.input_files) == 0) {
        report_simple_err("no input files");
        return 1;
    }

    do_parse();
    do_resolve_symbols();
    do_check();

    if (options.dump_symbols) {
        show_scope(global_scope);
    }

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
    init_file_table();
    init_types();
    init_scopes();

    roots = array_make(ast_t*);

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

void do_resolve_symbols(void) {
    u64 start_us;

    start_us = measure_time_now_us();

    scopes_find_origins(global_scope);

    verb_message("symbol origin resolution took %lu us\n", measure_time_now_us() - start_us);
}

void do_check(void) {
    u64       start_us;
    scope_t  *entry_scope;
    ast_t   **rootp;

    start_us = measure_time_now_us();

    if (program_entry == NULL) {
        report_simple_err("at least one procedure must be tagged as 'program_entry'");
        return;
    }

    entry_scope = get_subscope_from_node(global_scope, program_entry->val);
    if (entry_scope         == NULL
    ||  entry_scope->parent == NULL
    ||  entry_scope->parent != global_scope) {

        report_range_err(&ASTP(program_entry)->loc,
                         "'program_entry' procedure must be in global scope");
        return;
    }

    array_traverse(roots, rootp) {
        check_node(*rootp, global_scope, NULL);
    }

    verb_message("type-checking and semantic analysis took %lu us\n", measure_time_now_us() - start_us);
}
