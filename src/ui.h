#ifndef __UI_H__
#define __UI_H__

#include "internal.h"
#include "file.h"
#include "src_range.h"

void init_ui(void);
void verb_message(const char *fmt, ...);
void report_vague_err(const char *fmt, ...);
void report_loc_err(src_point_t pt, const char *fmt, ...);
void report_loc_info(src_point_t pt, const char *fmt, ...);
void report_range_err(src_range_t *range, const char *fmt, ...);
void report_range_info(src_range_t *range, const char *fmt, ...);
void report_file_err(file_t *file, int err);

#endif
