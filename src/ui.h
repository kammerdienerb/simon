#ifndef __UI_H__
#define __UI_H__

#include "internal.h"
#include "file.h"
#include "src_range.h"

#define BREADCRUMB_POINT (1)
#define BREADCRUMB_RANGE (2)

typedef struct {
    int         kind;
    src_range_t range;
    char        buff[1024];
} breadcrumb_t;

void init_ui(void);
void set_output_is_tty(void);
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
void _report_fixit(int should_exit, src_point_t pt, const char *fmt, ...);
void report_file_err(const char *path, int err);
void push_loc_breadcrumb(src_point_t pt, const char *fmt, ...);
void push_range_breadcrumb(src_range_t *range, const char *fmt, ...);
void pop_breadcrumb(void);

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
#define report_fixit(...)                          (_report_fixit(1, __VA_ARGS__))

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
#define report_fixit_no_exit(...)                  (_report_fixit(0, __VA_ARGS__))


#define I(...)     (report_simple_info_no_exit(__VA_ARGS__))
#define S(id)      (get_string(id))
#define T(ty)      (S(get_type_string_id(ty)))
#define V(val, ty) (S(value_to_string_id(val, ty)))

#endif
