#ifndef __UI_H__
#define __UI_H__

#include "internal.h"
#include "file.h"

void report_vague_err(const char *msg);
void report_file_err(file_t *file, int err);

#endif
