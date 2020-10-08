#ifndef __SOURCE_LOCATION_H__
#define __SOURCE_LOCATION_H__

#include "internal.h"
#include "strings.h"

typedef struct {
    u32   line;
    u32   col;
    char *buff_ptr;
} src_point_t;

typedef struct {
    string_id   path_id;
    src_point_t beg;
    src_point_t end;
} src_range_t;

void end_range_before(src_range_t *l, src_range_t *r, src_range_t *wtspc);

#endif
