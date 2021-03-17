#ifndef __OPTIONS_H__
#define __OPTIONS_H__

#include "array.h"

typedef struct {
    int         help;
    array_t     input_files;
    int         verbose;
    int         dump_symbols;
    int         n_threads;
    int         interp;
    array_t     interp_args;
    const char *output_name;
} options_t;

extern options_t options;

void print_usage(void);
int parse_options(int argc, char **argv);

#endif
