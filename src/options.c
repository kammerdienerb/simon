#include "options.h"

options_t options;

void print_usage(void) {
    char *usage =
"usage: simon [options] FILES\n"
"\n"
"options:\n"
"\n"
"--help\n"
"    Show this information.\n"
"\n"
;
    fprintf(stderr, "%s", usage);
}

int parse_options(int argc, char **argv) {
    int i;

    options.input_files = array_make(char*);

    for (i = 1; i < argc; i += 1) {
        if (strcmp(argv[i], "--help") == 0) {
            options.help = 1;
        } else if (strncmp(argv[i], "-", 1) == 0 || strncmp(argv[i], "--", 2) == 0) {
            return 0;
        } else {
            array_push(options.input_files, argv[i]);
        }
    }

    return 1;
}
