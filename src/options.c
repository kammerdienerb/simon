#include "options.h"
#include "platform.h"
#include "strings.h"
#include "ui.h"

options_t options;

void print_usage(void) {
    char *usage =
"usage: simon [options] FILES\n"
"\n"
"options:\n"
"\n"
"--verbose, -v\n"
"    Print additional information about the compilation.\n"
"--dump-symbols\n"
"    Print a textual representation of the symbol tables.\n"
"--threads=NUM\n"
"    Run compilation with NUM threads.\n"
"--output=NAME, -o NAME\n"
"    Output to file NAME.\n"
"--help\n"
"    Show this information.\n"
"\n"
;
    fprintf(stderr, "%s", usage);
}

int parse_options(int argc, char **argv) {
    int i;

    options.input_files  = array_make(char*);
    options.verbose      = 0;
    options.dump_symbols = 0;
    options.n_threads    = platform_get_num_hw_threads();
    options.backend      = "c";
    options.output_name  = NULL;

    for (i = 1; i < argc; i += 1) {
        if (strcmp(argv[i], "--help") == 0) {
            options.help = 1;
        } else if (strcmp(argv[i], "--verbose") == 0) {
            options.verbose = 1;
        } else if (strcmp(argv[i], "-v") == 0) {
            options.verbose = 1;
        } else if (strcmp(argv[i], "--dump-symbols") == 0) {
            options.dump_symbols = 1;
        } else if (strncmp(argv[i], "--threads=", 10) == 0) {
            if (sscanf(argv[i] + 10, "%d", &options.n_threads) != 1) {
                return 1;
            }
        } else if (strncmp(argv[i], "--backend=", 10) == 0) {
            options.backend = strdup(argv[i] + 10);
        } else if (strcmp(argv[i], "-b") == 0) {
            i += 1;
            if (i == argc) { return 1; }
            options.backend = strdup(argv[i]);
        } else if (strncmp(argv[i], "--output=", 9) == 0) {
            options.output_name = strdup(argv[i] + 9);
        } else if (strcmp(argv[i], "-o") == 0) {
            i += 1;
            if (i == argc) { return 1; }
            options.output_name = strdup(argv[i]);
        } else if (strncmp(argv[i], "-", 1) == 0 || strncmp(argv[i], "--", 2) == 0) {
            return 1;
        } else {
            array_push(options.input_files, argv[i]);
        }
    }

    if (strcmp(options.backend, "c")) {
        report_simple_err_no_exit("invalid backend value '%s'", options.backend);
        report_simple_info("options are: c");
        return 1;
    }

    return 0;
}
