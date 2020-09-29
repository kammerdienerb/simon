#ifndef __OPTIONS_H__
#define __OPTIONS_H__

#include "array.h"

typedef struct {
    int     help;
    array_t input_files;
} options_t;

extern options_t options;

void print_usage(void);
int parse_options(int argc, char **argv);

#endif
