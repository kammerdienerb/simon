#ifndef __UI_H__
#define __UI_H__

#include "internal.h"
#include "file.h"
#include "src_range.h"

void init_ui(void);
void verb_message(const char *fmt, ...);
void _report_simple_err(int should_exit, const char *fmt, ...);
void _report_simple_info(int should_exit, const char *fmt, ...);
void _report_loc_err(int should_exit, src_point_t pt, const char *fmt, ...);
void _report_loc_info(int should_exit, src_point_t pt, const char *fmt, ...);
void _report_range_err(int should_exit, src_range_t *range, const char *fmt, ...);
void _report_range_info(int should_exit, src_range_t *range, const char *fmt, ...);
void _report_loc_err_no_context(int should_exit, src_point_t pt, const char *fmt, ...);
void _report_loc_info_no_context(int should_exit, src_point_t pt, const char *fmt, ...);
void _report_range_err_no_context(int should_exit, src_range_t *range, const char *fmt, ...);
void _report_range_info_no_context(int should_exit, src_range_t *range, const char *fmt, ...);
void report_file_err(file_t *file, int err);

#define report_simple_err(...)                     (_report_simple_err(1, __VA_ARGS__))
#define report_simple_info(...)                    (_report_simple_info(1, __VA_ARGS__))
#define report_loc_err(...)                        (_report_loc_err(1, __VA_ARGS__))
#define report_loc_info(...)                       (_report_loc_info(1, __VA_ARGS__))
#define report_range_err(...)                      (_report_range_err(1, __VA_ARGS__))
#define report_range_info(...)                     (_report_range_info(1, __VA_ARGS__))
#define report_loc_err_no_context(...)             (_report_loc_err_no_context(1, __VA_ARGS__))
#define report_loc_info_no_context(...)            (_report_loc_info_no_context(1, __VA_ARGS__))
#define report_range_err_no_context(...)           (_report_range_err_no_context(1, __VA_ARGS__))
#define report_range_info_no_context(...)          (_report_range_info_no_context(1, __VA_ARGS__))

#define report_simple_err_no_exit(...)             (_report_simple_err(0, __VA_ARGS__))
#define report_simple_info_no_exit(...)            (_report_simple_info(0, __VA_ARGS__))
#define report_loc_err_no_exit(...)                (_report_loc_err(0, __VA_ARGS__))
#define report_loc_info_no_exit(...)               (_report_loc_info(0, __VA_ARGS__))
#define report_range_err_no_exit(...)              (_report_range_err(0, __VA_ARGS__))
#define report_range_info_no_exit(...)             (_report_range_info(0, __VA_ARGS__))
#define report_loc_err_no_context_no_exit(...)     (_report_loc_err_no_context(0, __VA_ARGS__))
#define report_loc_info_no_context_no_exit(...)    (_report_loc_info_no_context(0, __VA_ARGS__))
#define report_range_err_no_context_no_exit(...)   (_report_range_err_no_context(0, __VA_ARGS__))
#define report_range_info_no_context_no_exit(...)  (_report_range_info_no_context(0, __VA_ARGS__))

#endif
