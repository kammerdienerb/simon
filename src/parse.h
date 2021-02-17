#ifndef __PARSE_H__
#define __PARSE_H__

#include "internal.h"

void parse_file(const char *path);
void start_parsing_file_async(const char *path);
void start_parsing_file(const char *path);
void wait_for_parsing_async(void);

#endif
