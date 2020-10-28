#include "internal.h"
#include "globals.h"
#include "options.h"
#include "strings.h"
#include "parse.h"
#include "tls.h"

int do_options(int argc, char **argv);
void do_init(void);
void do_parse(void);

int main(int argc, char **argv) {
    if (do_options(argc, argv)) { return 1; }
    do_init();
    do_parse();

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
    tp = tp_make(options.n_threads);
    init_tls();
    init_strings();
}

void do_parse(void) {
    char **it;

    array_traverse(options.input_files, it) {
        start_parsing_file(*it);
    }
    wait_for_parsing();
}
