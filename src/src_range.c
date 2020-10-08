#include "src_range.h"

void end_range_before(src_range_t *l, src_range_t *r, src_range_t *wtspc) {
    if (wtspc) {
        if ((wtspc->beg.line == r->end.line && wtspc->beg.col < r->beg.col)
        ||  wtspc->beg.line < r->beg.line) {
            l->end = r->end;
        } else {
            l->end = wtspc->end;
        }
    } else {
        l->end = r->end;
    }

    if (l->end.col > 1) {
        l->end.col -= 1;
    }
}
