#include "internal.h"
#include "options.h"

int do_options(int argc, char **argv);

int main(int argc, char **argv) {
    if (do_options(argc, argv)) { return 1; }

    return 0;
}

int do_options(int argc, char **argv) {
    if (!parse_options(argc, argv)) {
        print_usage();
        return 1;
    } else if (options.help) {
        print_usage();
    }

    return 0;
}
