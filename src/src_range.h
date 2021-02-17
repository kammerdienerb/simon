#ifndef __SOURCE_LOCATION_H__
#define __SOURCE_LOCATION_H__

#include "internal.h"
#include "strings.h"

typedef struct {
    string_id  path_id;
    u32        line;
    u32        col;
    char      *buff_ptr;
} src_point_t;

typedef struct {
    src_point_t beg;
    src_point_t end;
} src_range_t;

void validate_range(src_range_t *range);

#endif
