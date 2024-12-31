typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;
typedef signed char        s8;
typedef short              s16;
typedef int                s32;
typedef long long          s64;
typedef float              f32;
typedef double             f64;

typedef struct { void *data; s64 len; } __si_slice;

#define __si_slice_data(_s)         ((_s).data)
#define __si_slice_len(_s)          ((_s).len)
#define __si_slice_idx(_s, _i, _t)  (((_t*)__si_slice_data(_s)) + (_i))
#define _builtin_slice_from(_p, _l) ((__si_slice){ (_p), (_l) })

#define _builtin_stack_alloc(_n) (__builtin_alloca((_n)))
#ifdef __x86_64__
#define _builtin_stack_pointer() ({ \
    register unsigned long esp __asm("esp"); \
    __asm("" : "=r"(esp)); \
    (void*)esp; \
})
#else
#define _builtin_stack_pointer() ({ \
    register unsigned long esp __asm("sp"); \
    __asm("" : "=r"(esp)); \
    (void*)esp; \
})
#endif
#define _builtin_base_pointer() (__builtin_frame_address(0))
#ifdef __x86_64__
__attribute__((used, always_inline))
static inline void _builtin_outb(u8* addr, u8 b) { __asm__ volatile ( "outb %0, %1" : : "a"(b), "Nd"((u16)(u64)addr) );                      }
__attribute__((used, always_inline))
static inline u8   _builtin_inb(u8* addr)        { u8 ret; __asm__ volatile ( "inb %1, %0" : "=a"(ret) : "Nd"((u16)(u64)addr) ); return ret; }
#endif



struct Error_struct;
typedef struct Error_struct Error;
struct Allocator__Bundle_struct;
typedef struct Allocator__Bundle_struct Allocator__Bundle;
struct String_Builder_struct;
typedef struct String_Builder_struct String_Builder;
typedef u64 Format__Format_Info;
struct Heap_Allocator_struct;
typedef struct Heap_Allocator_struct Heap_Allocator;
struct Fixed_Allocator_struct;
typedef struct Fixed_Allocator_struct Fixed_Allocator;
struct Singleton_Allocator_struct;
typedef struct Singleton_Allocator_struct Singleton_Allocator;




typedef void (*__proc_type_32)(u8*);
typedef void (*__proc_type_34)(s64);
typedef void (*__proc_type_36)(u8);
typedef u8* (*__proc_type_38)(u64);
typedef u8* (*__proc_type_39)(void);
typedef void (*__proc_type_41)(u8*, u8);
typedef u8 (*__proc_type_42)(u8*);
typedef u8* (*__proc_type_47)(u8*, u64);
typedef u8* (*__proc_type_49)(u8*, u8*, u64);
typedef void (*__proc_type_51)(u8*, u8*);
typedef u8* (*__proc_type_54)(Allocator__Bundle*, u64);
typedef u8* (*__proc_type_56)(Allocator__Bundle*, u8*, u64);
typedef void (*__proc_type_58)(Allocator__Bundle*, u8*);
typedef s64 (*__proc_type_73)(String_Builder*, u64, Format__Format_Info);
typedef s64 (*__proc_type_76)(String_Builder*, u8);
typedef void (*__proc_type_78)(String_Builder*);
typedef s64 (*__proc_type_80)(String_Builder*, u32, Format__Format_Info);
typedef s64 (*__proc_type_82)(String_Builder*, u16, Format__Format_Info);
typedef s64 (*__proc_type_84)(String_Builder*, u8, Format__Format_Info);
typedef s64 (*__proc_type_86)(String_Builder*, s64, Format__Format_Info);
typedef s64 (*__proc_type_88)(String_Builder*, s32, Format__Format_Info);
typedef s64 (*__proc_type_90)(String_Builder*, s16, Format__Format_Info);
typedef s64 (*__proc_type_92)(String_Builder*, s8, Format__Format_Info);
typedef s64 (*__proc_type_94)(String_Builder*, u8*, Format__Format_Info);
typedef s64 (*__proc_type_96)(String_Builder*, __si_slice, Format__Format_Info);
typedef void (*__proc_type_98)(__si_slice, u8, s64, u8);
typedef s32 (*__proc_type_100)(s32, u8*, u64);
typedef void (*__proc_type_102)(__si_slice);
typedef s64 (*__proc_type_105)(s64);
typedef s64 (*__proc_type_112)(String_Builder*, Error, Format__Format_Info);
typedef s64 (*__proc_type_114)(String_Builder*, __si_slice);
typedef Error (*__proc_type_115)(void);
typedef s64 (*__proc_type_120)(__si_slice, Error);
typedef s64 (*__proc_type_123)(__si_slice);
typedef s64 (*__proc_type_126)(__si_slice, __si_slice, s64);
typedef u8* (*__proc_type_130)(Heap_Allocator*, u64);
typedef u8* (*__proc_type_132)(Heap_Allocator*, u8*, u64);
typedef void (*__proc_type_134)(Heap_Allocator*, u8*);
typedef Allocator__Bundle (*__proc_type_136)(Heap_Allocator*);
typedef Fixed_Allocator (*__proc_type_138)(u8*, u64);
typedef u8* (*__proc_type_141)(Fixed_Allocator*, u64);
typedef void (*__proc_type_143)(Fixed_Allocator*, u8*);
typedef Singleton_Allocator (*__proc_type_145)(u8*, u64);
typedef u8* (*__proc_type_148)(Singleton_Allocator*, u64);
typedef u8* (*__proc_type_150)(Singleton_Allocator*, u8*, u64);
typedef void (*__proc_type_152)(Singleton_Allocator*, u8*);
typedef Allocator__Bundle (*__proc_type_154)(Singleton_Allocator*);
typedef s32 (*__proc_type_156)(u8*, s32);
typedef s32 (*__proc_type_158)(s32);
typedef __si_slice (*__proc_type_159)(String_Builder*);
typedef s64 (*__proc_type_160)(u8*);
typedef s64 (*__proc_type_161)(u8);
typedef s64 (*__proc_type_163)(Heap_Allocator*, __si_slice, __si_slice, s64);
typedef String_Builder (*__proc_type_164)(Heap_Allocator*);
typedef s64 (*__proc_type_166)(String_Builder*, __si_slice, __si_slice, s64);
typedef s64 (*__proc_type_168)(Heap_Allocator*, __si_slice);
typedef s64 (*__proc_type_170)(Heap_Allocator*, __si_slice, Error);
typedef s64 (*__proc_type_172)(String_Builder*, __si_slice, Error);
typedef s64 (*__proc_type_173)(Error);


struct Error_struct {
    s64 which;
};

struct Allocator__Bundle_struct {
    u8* self;
    __proc_type_47 alloc;
    __proc_type_49 realloc;
    __proc_type_51 free;
};

struct String_Builder_struct {
    Allocator__Bundle allocator;
    u8* data;
    u8* cursor;
    s64 len;
};

struct Heap_Allocator_struct {
};

struct Fixed_Allocator_struct {
    u8* base;
    u8* end;
};

struct Singleton_Allocator_struct {
    u8* base;
    u8* end;
};



_Static_assert(sizeof(String_Builder) == 56, "incorrect sizeof");
_Static_assert(sizeof(Singleton_Allocator) == 16, "incorrect sizeof");



u8* Allocator__bundle_alloc(Allocator__Bundle* bundle, u64 n_bytes) __asm__ ("Allocator.bundle_alloc");
u8* Allocator__bundle_realloc(Allocator__Bundle* bundle, u8* ptr, u64 n_bytes) __asm__ ("Allocator.bundle_realloc");
void Allocator__bundle_free(Allocator__Bundle* bundle, u8* ptr) __asm__ ("Allocator.bundle_free");
void string_builder_reset(String_Builder* sb) __asm__ ("string_builder_reset");
s64 string_builder_append_c(String_Builder* sb, u8 c) __asm__ ("string_builder_append_c");
s64 Format__u64_to_formatted_str(String_Builder* sb, u64 arg, Format__Format_Info f) __asm__ ("Format.u64_to_formatted_str");
s64 Format__u32_to_formatted_str(String_Builder* sb, u32 arg, Format__Format_Info f) __asm__ ("Format.u32_to_formatted_str");
s64 Format__u16_to_formatted_str(String_Builder* sb, u16 arg, Format__Format_Info f) __asm__ ("Format.u16_to_formatted_str");
s64 Format__u8_to_formatted_str(String_Builder* sb, u8 arg, Format__Format_Info f) __asm__ ("Format.u8_to_formatted_str");
s64 Format__s64_to_formatted_str(String_Builder* sb, s64 arg, Format__Format_Info f) __asm__ ("Format.s64_to_formatted_str");
s64 Format__s32_to_formatted_str(String_Builder* sb, s32 arg, Format__Format_Info f) __asm__ ("Format.s32_to_formatted_str");
s64 Format__s16_to_formatted_str(String_Builder* sb, s16 arg, Format__Format_Info f) __asm__ ("Format.s16_to_formatted_str");
s64 Format__s8_to_formatted_str(String_Builder* sb, s8 arg, Format__Format_Info f) __asm__ ("Format.s8_to_formatted_str");
s64 Format__ptr_to_formatted_str(String_Builder* sb, u8* arg, Format__Format_Info f) __asm__ ("Format.ptr_to_formatted_str");
s64 Format__str_to_formatted_str(String_Builder* sb, __si_slice arg, Format__Format_Info f) __asm__ ("Format.str_to_formatted_str");
s32 write(s32 fd, u8* bytes, u64 len);
void write_string(__si_slice s) __asm__ ("write_string");
void Format___final_write(__si_slice s, u8 just, s64 width, u8 pad_c) __asm__ ("Format._final_write");
s64 Format___get_width_from_arg__p1(__si_slice arg) __asm__ ("Format._get_width_from_arg(T: [u8])");
s64 Format___get_width_from_arg__p2(Error arg) __asm__ ("Format._get_width_from_arg(T: Error)");
s64 Format___get_width_from_s64(s64 arg) __asm__ ("Format._get_width_from_s64");
s64 Format___sbfmt__p0(String_Builder* sb, __si_slice fmt, __si_slice __varg0, s64 __varg1) __asm__ ("Format._sbfmt(...: ([u8], s64))");
s64 Format___sbfmt__p1(String_Builder* sb, __si_slice fmt) __asm__ ("Format._sbfmt(...: ())");
s64 Format___sbfmt__p2(String_Builder* sb, __si_slice fmt, Error __varg0) __asm__ ("Format._sbfmt(...: (Error))");
s64 Format___fmt__p0(Heap_Allocator* allocator, __si_slice fmt, __si_slice __varg0, s64 __varg1) __asm__ ("Format._fmt(allocator_type: Heap_Allocator, ...: ([u8], s64))");
s64 Format___fmt__p1(Heap_Allocator* allocator, __si_slice fmt) __asm__ ("Format._fmt(allocator_type: Heap_Allocator, ...: ())");
s64 Format___fmt__p2(Heap_Allocator* allocator, __si_slice fmt, Error __varg0) __asm__ ("Format._fmt(allocator_type: Heap_Allocator, ...: (Error))");
s64 string_builder_append(String_Builder* sb, __si_slice s) __asm__ ("string_builder_append");
s64 error_to_formatted_str(String_Builder* sb, Error arg, Format__Format_Info f) __asm__ ("error_to_formatted_str");
void exit(s64 code);
s64 printf__p0(__si_slice fmt, Error __varg0) __asm__ ("printf(...: (Error))");
s64 printf__p1(__si_slice fmt) __asm__ ("printf(...: ())");
s64 printf__p2(__si_slice fmt, __si_slice __varg0, s64 __varg1) __asm__ ("printf(...: ([u8], s64))");
Error main(void) __asm__ ("main");
u8* malloc(u64 size);
u8* heap_alloc(Heap_Allocator* allocator, u64 n_bytes) __asm__ ("heap_alloc");
u8* realloc(u8* ptr, u64 size);
u8* heap_realloc(Heap_Allocator* allocator, u8* ptr, u64 n_bytes) __asm__ ("heap_realloc");
void free(u8* ptr);
void heap_free(Heap_Allocator* allocator, u8* ptr) __asm__ ("heap_free");
Allocator__Bundle heap_bundle(Heap_Allocator* allocator) __asm__ ("heap_bundle");
Fixed_Allocator fixed_allocator_new(u8* buff, u64 size) __asm__ ("fixed_allocator_new");
u8* fixed_alloc(Fixed_Allocator* allocator, u64 n_bytes) __asm__ ("fixed_alloc");
void fixed_free(Fixed_Allocator* allocator, u8* ptr) __asm__ ("fixed_free");
Singleton_Allocator singleton_allocator_new(u8* buff, u64 size) __asm__ ("singleton_allocator_new");
u8* singleton_alloc(Singleton_Allocator* allocator, u64 n_bytes) __asm__ ("singleton_alloc");
u8* singleton_realloc(Singleton_Allocator* allocator, u8* ptr, u64 n_bytes) __asm__ ("singleton_realloc");
void singleton_free(Singleton_Allocator* allocator, u8* ptr) __asm__ ("singleton_free");
Allocator__Bundle singleton_bundle(Singleton_Allocator* allocator) __asm__ ("singleton_bundle");
s32 open(u8* path, s32 oflag);
s32 read(s32 fd, u8* bytes, u64 len);
s32 close(s32 fd);
String_Builder string_builder_new__p0(Heap_Allocator* allocator) __asm__ ("string_builder_new(allocator_type: Heap_Allocator)");
void string_builder_free(String_Builder* sb) __asm__ ("string_builder_free");
__si_slice string_builder_get(String_Builder* sb) __asm__ ("string_builder_get");
s64 cstr_len(u8* cstr) __asm__ ("cstr_len");
s64 is_space(u8 c) __asm__ ("is_space");
s64 is_digit(u8 c) __asm__ ("is_digit");
s64 is_alpha(u8 c) __asm__ ("is_alpha");
s64 is_alnum(u8 c) __asm__ ("is_alnum");
s64 aprintf__p0(Heap_Allocator* allocator, __si_slice fmt, __si_slice __varg0, s64 __varg1) __asm__ ("aprintf(allocator_type: Heap_Allocator, ...: ([u8], s64))");
s64 aprintf__p1(Heap_Allocator* allocator, __si_slice fmt) __asm__ ("aprintf(allocator_type: Heap_Allocator, ...: ())");
s64 aprintf__p2(Heap_Allocator* allocator, __si_slice fmt, Error __varg0) __asm__ ("aprintf(allocator_type: Heap_Allocator, ...: (Error))");


Heap_Allocator heap_allocator;
extern u8** __argv;


#line 17 "basic.si"
u8* Allocator__bundle_alloc(Allocator__Bundle* bundle, u64 n_bytes) {
    u8* __si_ret;

#line 18 "basic.si"
    __si_ret = ((bundle->alloc)((bundle->self), n_bytes));
    #line 18 "basic.si"
goto __si_scope_0_exit;
#line 19 "basic.si"

__si_scope_0_exit:;
    #line 19 "basic.si"
return __si_ret;
#line 19 "basic.si"
}

#line 21 "basic.si"
u8* Allocator__bundle_realloc(Allocator__Bundle* bundle, u8* ptr, u64 n_bytes) {
    u8* __si_ret;

#line 22 "basic.si"
    __si_ret = ((bundle->realloc)((bundle->self), ptr, n_bytes));
    #line 22 "basic.si"
goto __si_scope_1_exit;
#line 23 "basic.si"

__si_scope_1_exit:;
    #line 23 "basic.si"
return __si_ret;
#line 23 "basic.si"
}

#line 25 "basic.si"
void Allocator__bundle_free(Allocator__Bundle* bundle, u8* ptr) {
#line 26 "basic.si"
    ((bundle->free)((bundle->self), ptr));
#line 27 "basic.si"

__si_scope_2_exit:;
#line 27 "basic.si"
}

#line 160 "basic.si"
void string_builder_reset(String_Builder* sb) {
#line 161 "basic.si"
    (((sb->data) = (((u8*)(0ULL)))));
#line 162 "basic.si"
    (((sb->cursor) = (((u8*)(0ULL)))));
#line 163 "basic.si"
    (((sb->len) = 0));
#line 164 "basic.si"

__si_scope_3_exit:;
#line 164 "basic.si"
}

#line 180 "basic.si"
s64 string_builder_append_c(String_Builder* sb, u8 c) {
    s64 __si_ret;

#line 181 "basic.si"
    s64 was_null = ((sb->data) == (((u8*)(0ULL))));
#line 183 "basic.si"
    (((sb->data) = (Allocator__bundle_realloc(&((sb->allocator)), (sb->data), ((sb->len) + 1)))));
#line 185 "basic.si"
    if (was_null) {
#line 186 "basic.si"
        (((sb->cursor) = (sb->data)));
#line 187 "basic.si"
    }

#line 189 "basic.si"
    if (((sb->data) == (((u8*)(0ULL))))) {
#line 190 "basic.si"
        (string_builder_reset(sb));
#line 191 "basic.si"
        __si_ret = -2;
        #line 191 "basic.si"
goto __si_scope_4_exit;
#line 192 "basic.si"
    }

#line 194 "basic.si"
    (((sb->len) += 1));
#line 195 "basic.si"
    (((*((sb->cursor))) = c));
#line 196 "basic.si"
    (((sb->cursor) += 1ULL));
#line 198 "basic.si"
    __si_ret = 0ULL;
    #line 198 "basic.si"
goto __si_scope_4_exit;
#line 199 "basic.si"

__si_scope_4_exit:;
    #line 199 "basic.si"
return __si_ret;
#line 199 "basic.si"
}

#line 288 "basic.si"
s64 Format__u64_to_formatted_str(String_Builder* sb, u64 arg, Format__Format_Info f) {
    s64 __si_ret;

#line 289 "basic.si"
    __si_slice buff;
#line 290 "basic.si"
    s64 len = 1;
#line 291 "basic.si"
    u64 save_arg = arg;
#line 293 "basic.si"
    if ((((u8)((f & 0x18) >> 3)))) {
#line 294 "basic.si"
        __si_slice digits = ((__si_slice){ "0123456789abcdef", (sizeof "0123456789abcdef") - 1 });
#line 295 "basic.si"
        if (((((u8)((f & 0x18) >> 3))) == 2ULL)) {
#line 296 "basic.si"
            ((digits = ((__si_slice){ "0123456789ABCDEF", (sizeof "0123456789ABCDEF") - 1 })));
#line 297 "basic.si"
        }

#line 299 "basic.si"
        for (; (arg >= 16ULL); ((arg = (arg >> 4ULL)))){
#line 299 "basic.si"
            ((len += 1));
#line 299 "basic.si"
        }

#line 300 "basic.si"
        ((arg = save_arg));
#line 302 "basic.si"
        s64 idx = (len - 1);
#line 303 "basic.si"
        ((buff = (_builtin_slice_from((_builtin_stack_alloc(len)), len))));
#line 304 "basic.si"
        for (; (arg >= 16ULL); ((arg = (arg >> 4ULL)))){
#line 305 "basic.si"
            ((((*__si_slice_idx(buff, idx, u8))) = ((*__si_slice_idx(digits, (arg & 15ULL), u8)))));
#line 306 "basic.si"
            ((idx -= 1));
#line 307 "basic.si"
        }

#line 308 "basic.si"
        ((((*__si_slice_idx(buff, idx, u8))) = ((*__si_slice_idx(digits, (arg & 15ULL), u8)))));
#line 309 "basic.si"
    } else {
#line 310 "basic.si"
        for (; (arg >= 10ULL); ((arg /= 10ULL))){
#line 310 "basic.si"
            ((len += 1));
#line 310 "basic.si"
        }

#line 311 "basic.si"
        ((arg = save_arg));
#line 313 "basic.si"
        s64 idx = (len - 1);
#line 314 "basic.si"
        ((buff = (_builtin_slice_from((_builtin_stack_alloc(len)), len))));
#line 315 "basic.si"
        for (; (arg >= 10ULL); ((arg /= 10ULL))){
#line 316 "basic.si"
            ((((*__si_slice_idx(buff, idx, u8))) = (48ULL + (arg % 10ULL))));
#line 317 "basic.si"
            ((idx -= 1));
#line 318 "basic.si"
        }

#line 319 "basic.si"
        ((((*__si_slice_idx(buff, idx, u8))) = (48ULL + (arg % 10ULL))));
#line 320 "basic.si"
    }

#line 322 "basic.si"
    for (s64 idx = 0; (idx < len); ((idx += 1))){
#line 323 "basic.si"
        s64 err = (string_builder_append_c(sb, ((*__si_slice_idx(buff, idx, u8)))));
#line 324 "basic.si"
        if (err) {
#line 324 "basic.si"
            __si_ret = err;
            #line 324 "basic.si"
goto __si_scope_7_exit;
#line 324 "basic.si"
        }

#line 325 "basic.si"
    }

#line 327 "basic.si"
    __si_ret = 0ULL;
    #line 327 "basic.si"
goto __si_scope_7_exit;
#line 328 "basic.si"

__si_scope_7_exit:;
    #line 328 "basic.si"
return __si_ret;
#line 328 "basic.si"
}

#line 331 "basic.si"
s64 Format__u32_to_formatted_str(String_Builder* sb, u32 arg, Format__Format_Info f) {
    s64 __si_ret;

#line 332 "basic.si"
    __si_ret = (Format__u64_to_formatted_str(sb, (((u64)(arg))), f));
    #line 332 "basic.si"
goto __si_scope_17_exit;
#line 333 "basic.si"

__si_scope_17_exit:;
    #line 333 "basic.si"
return __si_ret;
#line 333 "basic.si"
}

#line 335 "basic.si"
s64 Format__u16_to_formatted_str(String_Builder* sb, u16 arg, Format__Format_Info f) {
    s64 __si_ret;

#line 336 "basic.si"
    __si_ret = (Format__u64_to_formatted_str(sb, (((u64)(arg))), f));
    #line 336 "basic.si"
goto __si_scope_18_exit;
#line 337 "basic.si"

__si_scope_18_exit:;
    #line 337 "basic.si"
return __si_ret;
#line 337 "basic.si"
}

#line 339 "basic.si"
s64 Format__u8_to_formatted_str(String_Builder* sb, u8 arg, Format__Format_Info f) {
    s64 __si_ret;

#line 340 "basic.si"
    if ((((u8)((f & 0x60) >> 5)))) {
#line 341 "basic.si"
        __si_ret = (string_builder_append_c(sb, arg));
        #line 341 "basic.si"
goto __si_scope_19_exit;
#line 342 "basic.si"
    } else {
#line 343 "basic.si"
        __si_ret = (Format__u64_to_formatted_str(sb, (((u64)(arg))), f));
        #line 343 "basic.si"
goto __si_scope_19_exit;
#line 344 "basic.si"
    }

#line 345 "basic.si"

__si_scope_19_exit:;
    #line 345 "basic.si"
return __si_ret;
#line 345 "basic.si"
}

#line 348 "basic.si"
s64 Format__s64_to_formatted_str(String_Builder* sb, s64 arg, Format__Format_Info f) {
    s64 __si_ret;

#line 349 "basic.si"
    if ((((u8)((f & 0x18) >> 3)))) {
#line 349 "basic.si"
        __si_ret = (Format__u64_to_formatted_str(sb, (((u64)(arg))), f));
        #line 349 "basic.si"
goto __si_scope_22_exit;
#line 349 "basic.si"
    }

#line 351 "basic.si"
    s64 len = 1;
#line 352 "basic.si"
    s64 neg = 0;
#line 354 "basic.si"
    if ((arg < 0)) {
#line 355 "basic.si"
        ((len += 1));
#line 356 "basic.si"
        ((neg = 1));
#line 357 "basic.si"
        ((arg = -arg));
#line 358 "basic.si"
    }

#line 360 "basic.si"
    s64 save_arg = arg;
#line 362 "basic.si"
    for (; (arg >= 10); ((arg /= 10))){
#line 362 "basic.si"
        ((len += 1));
#line 362 "basic.si"
    }

#line 364 "basic.si"
    ((arg = save_arg));
#line 366 "basic.si"
    s64 idx = (len - 1);
#line 367 "basic.si"
    __si_slice buff = (_builtin_slice_from((_builtin_stack_alloc(len)), len));
#line 368 "basic.si"
    for (; (arg >= 10); ((arg /= 10))){
#line 369 "basic.si"
        ((((*__si_slice_idx(buff, idx, u8))) = (48ULL + (arg % 10))));
#line 370 "basic.si"
        ((idx -= 1));
#line 371 "basic.si"
    }

#line 372 "basic.si"
    ((((*__si_slice_idx(buff, idx, u8))) = (48ULL + (arg % 10))));
#line 373 "basic.si"
    ((idx -= 1));
#line 375 "basic.si"
    if (neg) {
#line 376 "basic.si"
        ((((*__si_slice_idx(buff, idx, u8))) = 45ULL));
#line 377 "basic.si"
    }

#line 379 "basic.si"
    ((idx = 0));
#line 380 "basic.si"
    for (; (idx < len); ((idx += 1))){
#line 381 "basic.si"
        s64 err = (string_builder_append_c(sb, ((*__si_slice_idx(buff, idx, u8)))));
#line 382 "basic.si"
        if (err) {
#line 382 "basic.si"
            __si_ret = err;
            #line 382 "basic.si"
goto __si_scope_22_exit;
#line 382 "basic.si"
        }

#line 383 "basic.si"
    }

#line 385 "basic.si"
    __si_ret = 0ULL;
    #line 385 "basic.si"
goto __si_scope_22_exit;
#line 386 "basic.si"

__si_scope_22_exit:;
    #line 386 "basic.si"
return __si_ret;
#line 386 "basic.si"
}

#line 389 "basic.si"
s64 Format__s32_to_formatted_str(String_Builder* sb, s32 arg, Format__Format_Info f) {
    s64 __si_ret;

#line 390 "basic.si"
    __si_ret = (Format__s64_to_formatted_str(sb, (((s64)(arg))), f));
    #line 390 "basic.si"
goto __si_scope_30_exit;
#line 391 "basic.si"

__si_scope_30_exit:;
    #line 391 "basic.si"
return __si_ret;
#line 391 "basic.si"
}

#line 393 "basic.si"
s64 Format__s16_to_formatted_str(String_Builder* sb, s16 arg, Format__Format_Info f) {
    s64 __si_ret;

#line 394 "basic.si"
    __si_ret = (Format__s64_to_formatted_str(sb, (((s64)(arg))), f));
    #line 394 "basic.si"
goto __si_scope_31_exit;
#line 395 "basic.si"

__si_scope_31_exit:;
    #line 395 "basic.si"
return __si_ret;
#line 395 "basic.si"
}

#line 397 "basic.si"
s64 Format__s8_to_formatted_str(String_Builder* sb, s8 arg, Format__Format_Info f) {
    s64 __si_ret;

#line 398 "basic.si"
    __si_ret = (Format__s64_to_formatted_str(sb, (((s64)(arg))), f));
    #line 398 "basic.si"
goto __si_scope_32_exit;
#line 399 "basic.si"

__si_scope_32_exit:;
    #line 399 "basic.si"
return __si_ret;
#line 399 "basic.si"
}

#line 402 "basic.si"
s64 Format__ptr_to_formatted_str(String_Builder* sb, u8* arg, Format__Format_Info f) {
    s64 __si_ret;

#line 403 "basic.si"
    if (!((((u8)((f & 0x18) >> 3))))) {
#line 404 "basic.si"
        (f = (f & ~0x18) | ((((u64)2ULL) << 3ULL) & 0x18));
#line 405 "basic.si"
    }

#line 407 "basic.si"
    (string_builder_append_c(sb, 48ULL));
#line 408 "basic.si"
    (string_builder_append_c(sb, 120ULL));
#line 410 "basic.si"
    s64 err = (Format__u64_to_formatted_str(sb, (((u64)(arg))), f));
#line 412 "basic.si"
    __si_ret = err;
    #line 412 "basic.si"
goto __si_scope_33_exit;
#line 413 "basic.si"

__si_scope_33_exit:;
    #line 413 "basic.si"
return __si_ret;
#line 413 "basic.si"
}

#line 416 "basic.si"
s64 Format__str_to_formatted_str(String_Builder* sb, __si_slice arg, Format__Format_Info f) {
    s64 __si_ret;

#line 417 "basic.si"
    s64 arg_len = (arg).len;
#line 419 "basic.si"
    for (s64 i = 0; (i < arg_len); ((i += 1))){
#line 420 "basic.si"
        s64 err = (string_builder_append_c(sb, ((*__si_slice_idx(arg, i, u8)))));
#line 421 "basic.si"
        if (err) {
#line 421 "basic.si"
            __si_ret = err;
            #line 421 "basic.si"
goto __si_scope_35_exit;
#line 421 "basic.si"
        }

#line 422 "basic.si"
    }

#line 424 "basic.si"
    __si_ret = 0ULL;
    #line 424 "basic.si"
goto __si_scope_35_exit;
#line 425 "basic.si"

__si_scope_35_exit:;
    #line 425 "basic.si"
return __si_ret;
#line 425 "basic.si"
}

#line 147 "basic.si"
void write_string(__si_slice s) {
#line 148 "basic.si"
    (write(1, &(((*__si_slice_idx(s, 0, u8)))), (s).len));
#line 149 "basic.si"

__si_scope_38_exit:;
#line 149 "basic.si"
}

#line 432 "basic.si"
void Format___final_write(__si_slice s, u8 just, s64 width, u8 pad_c) {
#line 433 "basic.si"
    if (width) {
#line 434 "basic.si"
        if ((just == 1ULL)) {
#line 435 "basic.si"
            s64 l = (width - (s).len);
#line 436 "basic.si"
            for (s64 j = 0; (j < l); ((j += 1))){
#line 436 "basic.si"
                (write(1, &(pad_c), 1ULL));
#line 436 "basic.si"
            }

#line 437 "basic.si"
        } else if ((just == 2ULL)) {
#line 438 "basic.si"
            s64 l = ((width - (s).len) >> 1);
#line 439 "basic.si"
            for (s64 j = 0; (j < l); ((j += 1))){
#line 439 "basic.si"
                (write(1, &(pad_c), 1ULL));
#line 439 "basic.si"
            }

#line 440 "basic.si"
        }

#line 441 "basic.si"
    }

#line 442 "basic.si"
    (write_string(s));
#line 443 "basic.si"
    if (width) {
#line 444 "basic.si"
        if ((just == 0ULL)) {
#line 445 "basic.si"
            s64 l = (width - (s).len);
#line 446 "basic.si"
            for (s64 j = 0; (j < l); ((j += 1))){
#line 446 "basic.si"
                (write(1, &(pad_c), 1ULL));
#line 446 "basic.si"
            }

#line 447 "basic.si"
        } else if ((just == 2ULL)) {
#line 448 "basic.si"
            s64 l = ((width - ((width - (s).len) >> 1)) - (s).len);
#line 449 "basic.si"
            for (s64 j = 0; (j < l); ((j += 1))){
#line 449 "basic.si"
                (write(1, &(pad_c), 1ULL));
#line 449 "basic.si"
            }

#line 450 "basic.si"
        }

#line 451 "basic.si"
    }

#line 452 "basic.si"

__si_scope_39_exit:;
#line 452 "basic.si"
}

#line 454 "basic.si"
s64 Format___get_width_from_arg__p1(__si_slice arg) {
    s64 __si_ret;

#line 454 "basic.si"
    __si_ret = 0ULL;
    #line 454 "basic.si"
goto __si_scope_50_exit;
#line 454 "basic.si"

__si_scope_50_exit:;
    #line 454 "basic.si"
return __si_ret;
#line 454 "basic.si"
}

#line 454 "basic.si"
s64 Format___get_width_from_arg__p2(Error arg) {
    s64 __si_ret;

#line 454 "basic.si"
    __si_ret = 0ULL;
    #line 454 "basic.si"
goto __si_scope_51_exit;
#line 454 "basic.si"

__si_scope_51_exit:;
    #line 454 "basic.si"
return __si_ret;
#line 454 "basic.si"
}

#line 456 "basic.si"
s64 Format___get_width_from_s64(s64 arg) {
    s64 __si_ret;

#line 456 "basic.si"
    __si_ret = arg;
    #line 456 "basic.si"
goto __si_scope_52_exit;
#line 456 "basic.si"

__si_scope_52_exit:;
    #line 456 "basic.si"
return __si_ret;
#line 456 "basic.si"
}

#line 458 "basic.si"
s64 Format___sbfmt__p0(String_Builder* sb, __si_slice fmt, __si_slice __varg0, s64 __varg1) {
    s64 __si_ret;

#line 459 "basic.si"
    s64 fmt_len = (fmt).len;
#line 460 "basic.si"
    s64 which_arg = 0;
#line 462 "basic.si"
    for (s64 i = 0; (i < fmt_len); ((i += 1))){
#line 463 "basic.si"
        if ((((*__si_slice_idx(fmt, i, u8))) == 123ULL)) {
#line 464 "basic.si"
            Format__Format_Info info = 0ULL;
#line 465 "basic.si"
            (info = (info & ~0x7) | ((((u64)0ULL) << 0ULL) & 0x7));
#line 467 "basic.si"
            for (; ((i < fmt_len) && (((*__si_slice_idx(fmt, i, u8))) != 125ULL)); ((i += 1))){
#line 468 "basic.si"
                if ((((*__si_slice_idx(fmt, i, u8))) == 120ULL)) {
#line 468 "basic.si"
                    (info = (info & ~0x18) | ((((u64)1ULL) << 3ULL) & 0x18));
#line 468 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 88ULL)) {
#line 469 "basic.si"
                    (info = (info & ~0x18) | ((((u64)2ULL) << 3ULL) & 0x18));
#line 469 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 97ULL)) {
#line 470 "basic.si"
                    (info = (info & ~0x60) | ((((u64)1ULL) << 5ULL) & 0x60));
#line 470 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 45ULL)) {
#line 471 "basic.si"
                    (info = (info & ~0x7) | ((((u64)0ULL) << 0ULL) & 0x7));
#line 471 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 61ULL)) {
#line 472 "basic.si"
                    (info = (info & ~0x7) | ((((u64)2ULL) << 0ULL) & 0x7));
#line 472 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 43ULL)) {
#line 473 "basic.si"
                    (info = (info & ~0x7) | ((((u64)1ULL) << 0ULL) & 0x7));
#line 473 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 42ULL)) {
#line 475 "basic.si"
                    s64 a = 0;
#line 476 "basic.si"
                    #line 477 "basic.si"
                        if ((a == which_arg)) {
#line 478 "basic.si"
                            s64 w = (Format___get_width_from_arg__p1((__varg0)));
#line 479 "basic.si"
                            if ((w < 0)) {
#line 480 "basic.si"
                                (info = (info & ~0x7) | ((((u64)0ULL) << 0ULL) & 0x7));
#line 481 "basic.si"
                                ((w = -w));
#line 482 "basic.si"
                            }

#line 483 "basic.si"
                            (info = (info & ~0xfffffe00) | ((((u64)w) << 9ULL) & 0xfffffe00));
#line 484 "basic.si"
                        }

#line 485 "basic.si"
                        ((a += 1));
#line 486 "basic.si"
                    

#line 476 "basic.si"
                    #line 477 "basic.si"
                        if ((a == which_arg)) {
#line 478 "basic.si"
                            s64 w = (Format___get_width_from_s64((__varg1)));
#line 479 "basic.si"
                            if ((w < 0)) {
#line 480 "basic.si"
                                (info = (info & ~0x7) | ((((u64)0ULL) << 0ULL) & 0x7));
#line 481 "basic.si"
                                ((w = -w));
#line 482 "basic.si"
                            }

#line 483 "basic.si"
                            (info = (info & ~0xfffffe00) | ((((u64)w) << 9ULL) & 0xfffffe00));
#line 484 "basic.si"
                        }

#line 485 "basic.si"
                        ((a += 1));
#line 486 "basic.si"
                    

#line 487 "basic.si"
                    ((which_arg += 1));
#line 488 "basic.si"
                } else if ((is_digit(((*__si_slice_idx(fmt, i, u8)))))) {
#line 489 "basic.si"
                    for (; ((i < fmt_len) && (((*__si_slice_idx(fmt, i, u8))) == 48ULL)); ((i += 1))){
#line 490 "basic.si"
                        (info = (info & ~0x180) | ((((u64)1ULL) << 7ULL) & 0x180));
#line 491 "basic.si"
                    }

#line 492 "basic.si"
                    s64 seen_dig = 0;
#line 493 "basic.si"
                    for (; ((i < fmt_len) && (is_digit(((*__si_slice_idx(fmt, i, u8)))))); ((i += 1))){
#line 494 "basic.si"
                        (info = (info & ~0xfffffe00) | ((((u64)(((((s64)((info & 0xfffffe00) >> 9))) * 10) + (((*__si_slice_idx(fmt, i, u8))) - 48ULL))) << 9ULL) & 0xfffffe00));
#line 495 "basic.si"
                        ((seen_dig = 1));
#line 496 "basic.si"
                    }

#line 497 "basic.si"
                    if (seen_dig) {
#line 498 "basic.si"
                        ((i -= 1));
#line 499 "basic.si"
                    } else {
#line 500 "basic.si"
                        if ((((u8)((info & 0x180) >> 7)))) {
#line 500 "basic.si"
                            ((i -= 1));
#line 500 "basic.si"
                        }

#line 501 "basic.si"
                        (info = (info & ~0x180) | ((((u64)0ULL) << 7ULL) & 0x180));
#line 502 "basic.si"
                        (info = (info & ~0xfffffe00) | ((((u64)0) << 9ULL) & 0xfffffe00));
#line 503 "basic.si"
                    }

#line 504 "basic.si"
                }

#line 505 "basic.si"
            }

#line 507 "basic.si"
            s64 a = 0;
#line 508 "basic.si"
            #line 509 "basic.si"
                if ((a == which_arg)) {
#line 510 "basic.si"
                    s64 err = (Format__str_to_formatted_str(sb, (__varg0), info));
#line 511 "basic.si"
                    if (err) {
#line 511 "basic.si"
                        __si_ret = err;
                        #line 511 "basic.si"
goto __si_scope_53_exit;
#line 511 "basic.si"
                    }

#line 512 "basic.si"
                }

#line 513 "basic.si"
                ((a += 1));
#line 514 "basic.si"
            

#line 508 "basic.si"
            #line 509 "basic.si"
                if ((a == which_arg)) {
#line 510 "basic.si"
                    s64 err = (Format__s64_to_formatted_str(sb, (__varg1), info));
#line 511 "basic.si"
                    if (err) {
#line 511 "basic.si"
                        __si_ret = err;
                        #line 511 "basic.si"
goto __si_scope_53_exit;
#line 511 "basic.si"
                    }

#line 512 "basic.si"
                }

#line 513 "basic.si"
                ((a += 1));
#line 514 "basic.si"
            

#line 517 "basic.si"
            __si_slice s = (string_builder_get(sb));
#line 519 "basic.si"
            if (((s).len > (((s64)((info & 0xfffffe00) >> 9))))) {
#line 519 "basic.si"
                (info = (info & ~0xfffffe00) | ((((u64)0) << 9ULL) & 0xfffffe00));
#line 519 "basic.si"
            }

#line 520 "basic.si"
            u8 pad_c = 32ULL;
#line 521 "basic.si"
            if ((((u8)((info & 0x180) >> 7)))) {
#line 521 "basic.si"
                ((pad_c = 48ULL));
#line 521 "basic.si"
            }

#line 522 "basic.si"
            (Format___final_write(s, (((u8)((info & 0x7)))), (((s64)((info & 0xfffffe00) >> 9))), pad_c));
#line 524 "basic.si"
            (string_builder_reset(sb));
#line 526 "basic.si"
            ((which_arg += 1));
#line 527 "basic.si"
        } else {
#line 528 "basic.si"
            (write_string((_builtin_slice_from(&(((*__si_slice_idx(fmt, i, u8)))), 1))));
#line 529 "basic.si"
        }

#line 530 "basic.si"
    }

#line 532 "basic.si"
    __si_ret = 0ULL;
    #line 532 "basic.si"
goto __si_scope_53_exit;
#line 533 "basic.si"

__si_scope_53_exit:;
    #line 533 "basic.si"
return __si_ret;
#line 533 "basic.si"
}

#line 458 "basic.si"
s64 Format___sbfmt__p1(String_Builder* sb, __si_slice fmt) {
    s64 __si_ret;

#line 459 "basic.si"
    s64 fmt_len = (fmt).len;
#line 460 "basic.si"
    s64 which_arg = 0;
#line 462 "basic.si"
    for (s64 i = 0; (i < fmt_len); ((i += 1))){
#line 463 "basic.si"
        if ((((*__si_slice_idx(fmt, i, u8))) == 123ULL)) {
#line 464 "basic.si"
            Format__Format_Info info = 0ULL;
#line 465 "basic.si"
            (info = (info & ~0x7) | ((((u64)0ULL) << 0ULL) & 0x7));
#line 467 "basic.si"
            for (; ((i < fmt_len) && (((*__si_slice_idx(fmt, i, u8))) != 125ULL)); ((i += 1))){
#line 468 "basic.si"
                if ((((*__si_slice_idx(fmt, i, u8))) == 120ULL)) {
#line 468 "basic.si"
                    (info = (info & ~0x18) | ((((u64)1ULL) << 3ULL) & 0x18));
#line 468 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 88ULL)) {
#line 469 "basic.si"
                    (info = (info & ~0x18) | ((((u64)2ULL) << 3ULL) & 0x18));
#line 469 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 97ULL)) {
#line 470 "basic.si"
                    (info = (info & ~0x60) | ((((u64)1ULL) << 5ULL) & 0x60));
#line 470 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 45ULL)) {
#line 471 "basic.si"
                    (info = (info & ~0x7) | ((((u64)0ULL) << 0ULL) & 0x7));
#line 471 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 61ULL)) {
#line 472 "basic.si"
                    (info = (info & ~0x7) | ((((u64)2ULL) << 0ULL) & 0x7));
#line 472 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 43ULL)) {
#line 473 "basic.si"
                    (info = (info & ~0x7) | ((((u64)1ULL) << 0ULL) & 0x7));
#line 473 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 42ULL)) {
#line 475 "basic.si"
                    s64 a = 0;
#line 487 "basic.si"
                    ((which_arg += 1));
#line 488 "basic.si"
                } else if ((is_digit(((*__si_slice_idx(fmt, i, u8)))))) {
#line 489 "basic.si"
                    for (; ((i < fmt_len) && (((*__si_slice_idx(fmt, i, u8))) == 48ULL)); ((i += 1))){
#line 490 "basic.si"
                        (info = (info & ~0x180) | ((((u64)1ULL) << 7ULL) & 0x180));
#line 491 "basic.si"
                    }

#line 492 "basic.si"
                    s64 seen_dig = 0;
#line 493 "basic.si"
                    for (; ((i < fmt_len) && (is_digit(((*__si_slice_idx(fmt, i, u8)))))); ((i += 1))){
#line 494 "basic.si"
                        (info = (info & ~0xfffffe00) | ((((u64)(((((s64)((info & 0xfffffe00) >> 9))) * 10) + (((*__si_slice_idx(fmt, i, u8))) - 48ULL))) << 9ULL) & 0xfffffe00));
#line 495 "basic.si"
                        ((seen_dig = 1));
#line 496 "basic.si"
                    }

#line 497 "basic.si"
                    if (seen_dig) {
#line 498 "basic.si"
                        ((i -= 1));
#line 499 "basic.si"
                    } else {
#line 500 "basic.si"
                        if ((((u8)((info & 0x180) >> 7)))) {
#line 500 "basic.si"
                            ((i -= 1));
#line 500 "basic.si"
                        }

#line 501 "basic.si"
                        (info = (info & ~0x180) | ((((u64)0ULL) << 7ULL) & 0x180));
#line 502 "basic.si"
                        (info = (info & ~0xfffffe00) | ((((u64)0) << 9ULL) & 0xfffffe00));
#line 503 "basic.si"
                    }

#line 504 "basic.si"
                }

#line 505 "basic.si"
            }

#line 507 "basic.si"
            s64 a = 0;
#line 517 "basic.si"
            __si_slice s = (string_builder_get(sb));
#line 519 "basic.si"
            if (((s).len > (((s64)((info & 0xfffffe00) >> 9))))) {
#line 519 "basic.si"
                (info = (info & ~0xfffffe00) | ((((u64)0) << 9ULL) & 0xfffffe00));
#line 519 "basic.si"
            }

#line 520 "basic.si"
            u8 pad_c = 32ULL;
#line 521 "basic.si"
            if ((((u8)((info & 0x180) >> 7)))) {
#line 521 "basic.si"
                ((pad_c = 48ULL));
#line 521 "basic.si"
            }

#line 522 "basic.si"
            (Format___final_write(s, (((u8)((info & 0x7)))), (((s64)((info & 0xfffffe00) >> 9))), pad_c));
#line 524 "basic.si"
            (string_builder_reset(sb));
#line 526 "basic.si"
            ((which_arg += 1));
#line 527 "basic.si"
        } else {
#line 528 "basic.si"
            (write_string((_builtin_slice_from(&(((*__si_slice_idx(fmt, i, u8)))), 1))));
#line 529 "basic.si"
        }

#line 530 "basic.si"
    }

#line 532 "basic.si"
    __si_ret = 0ULL;
    #line 532 "basic.si"
goto __si_scope_81_exit;
#line 533 "basic.si"

__si_scope_81_exit:;
    #line 533 "basic.si"
return __si_ret;
#line 533 "basic.si"
}

#line 458 "basic.si"
s64 Format___sbfmt__p2(String_Builder* sb, __si_slice fmt, Error __varg0) {
    s64 __si_ret;

#line 459 "basic.si"
    s64 fmt_len = (fmt).len;
#line 460 "basic.si"
    s64 which_arg = 0;
#line 462 "basic.si"
    for (s64 i = 0; (i < fmt_len); ((i += 1))){
#line 463 "basic.si"
        if ((((*__si_slice_idx(fmt, i, u8))) == 123ULL)) {
#line 464 "basic.si"
            Format__Format_Info info = 0ULL;
#line 465 "basic.si"
            (info = (info & ~0x7) | ((((u64)0ULL) << 0ULL) & 0x7));
#line 467 "basic.si"
            for (; ((i < fmt_len) && (((*__si_slice_idx(fmt, i, u8))) != 125ULL)); ((i += 1))){
#line 468 "basic.si"
                if ((((*__si_slice_idx(fmt, i, u8))) == 120ULL)) {
#line 468 "basic.si"
                    (info = (info & ~0x18) | ((((u64)1ULL) << 3ULL) & 0x18));
#line 468 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 88ULL)) {
#line 469 "basic.si"
                    (info = (info & ~0x18) | ((((u64)2ULL) << 3ULL) & 0x18));
#line 469 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 97ULL)) {
#line 470 "basic.si"
                    (info = (info & ~0x60) | ((((u64)1ULL) << 5ULL) & 0x60));
#line 470 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 45ULL)) {
#line 471 "basic.si"
                    (info = (info & ~0x7) | ((((u64)0ULL) << 0ULL) & 0x7));
#line 471 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 61ULL)) {
#line 472 "basic.si"
                    (info = (info & ~0x7) | ((((u64)2ULL) << 0ULL) & 0x7));
#line 472 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 43ULL)) {
#line 473 "basic.si"
                    (info = (info & ~0x7) | ((((u64)1ULL) << 0ULL) & 0x7));
#line 473 "basic.si"
                } else if ((((*__si_slice_idx(fmt, i, u8))) == 42ULL)) {
#line 475 "basic.si"
                    s64 a = 0;
#line 476 "basic.si"
                    #line 477 "basic.si"
                        if ((a == which_arg)) {
#line 478 "basic.si"
                            s64 w = (Format___get_width_from_arg__p2((__varg0)));
#line 479 "basic.si"
                            if ((w < 0)) {
#line 480 "basic.si"
                                (info = (info & ~0x7) | ((((u64)0ULL) << 0ULL) & 0x7));
#line 481 "basic.si"
                                ((w = -w));
#line 482 "basic.si"
                            }

#line 483 "basic.si"
                            (info = (info & ~0xfffffe00) | ((((u64)w) << 9ULL) & 0xfffffe00));
#line 484 "basic.si"
                        }

#line 485 "basic.si"
                        ((a += 1));
#line 486 "basic.si"
                    

#line 487 "basic.si"
                    ((which_arg += 1));
#line 488 "basic.si"
                } else if ((is_digit(((*__si_slice_idx(fmt, i, u8)))))) {
#line 489 "basic.si"
                    for (; ((i < fmt_len) && (((*__si_slice_idx(fmt, i, u8))) == 48ULL)); ((i += 1))){
#line 490 "basic.si"
                        (info = (info & ~0x180) | ((((u64)1ULL) << 7ULL) & 0x180));
#line 491 "basic.si"
                    }

#line 492 "basic.si"
                    s64 seen_dig = 0;
#line 493 "basic.si"
                    for (; ((i < fmt_len) && (is_digit(((*__si_slice_idx(fmt, i, u8)))))); ((i += 1))){
#line 494 "basic.si"
                        (info = (info & ~0xfffffe00) | ((((u64)(((((s64)((info & 0xfffffe00) >> 9))) * 10) + (((*__si_slice_idx(fmt, i, u8))) - 48ULL))) << 9ULL) & 0xfffffe00));
#line 495 "basic.si"
                        ((seen_dig = 1));
#line 496 "basic.si"
                    }

#line 497 "basic.si"
                    if (seen_dig) {
#line 498 "basic.si"
                        ((i -= 1));
#line 499 "basic.si"
                    } else {
#line 500 "basic.si"
                        if ((((u8)((info & 0x180) >> 7)))) {
#line 500 "basic.si"
                            ((i -= 1));
#line 500 "basic.si"
                        }

#line 501 "basic.si"
                        (info = (info & ~0x180) | ((((u64)0ULL) << 7ULL) & 0x180));
#line 502 "basic.si"
                        (info = (info & ~0xfffffe00) | ((((u64)0) << 9ULL) & 0xfffffe00));
#line 503 "basic.si"
                    }

#line 504 "basic.si"
                }

#line 505 "basic.si"
            }

#line 507 "basic.si"
            s64 a = 0;
#line 508 "basic.si"
            #line 509 "basic.si"
                if ((a == which_arg)) {
#line 510 "basic.si"
                    s64 err = (error_to_formatted_str(sb, (__varg0), info));
#line 511 "basic.si"
                    if (err) {
#line 511 "basic.si"
                        __si_ret = err;
                        #line 511 "basic.si"
goto __si_scope_101_exit;
#line 511 "basic.si"
                    }

#line 512 "basic.si"
                }

#line 513 "basic.si"
                ((a += 1));
#line 514 "basic.si"
            

#line 517 "basic.si"
            __si_slice s = (string_builder_get(sb));
#line 519 "basic.si"
            if (((s).len > (((s64)((info & 0xfffffe00) >> 9))))) {
#line 519 "basic.si"
                (info = (info & ~0xfffffe00) | ((((u64)0) << 9ULL) & 0xfffffe00));
#line 519 "basic.si"
            }

#line 520 "basic.si"
            u8 pad_c = 32ULL;
#line 521 "basic.si"
            if ((((u8)((info & 0x180) >> 7)))) {
#line 521 "basic.si"
                ((pad_c = 48ULL));
#line 521 "basic.si"
            }

#line 522 "basic.si"
            (Format___final_write(s, (((u8)((info & 0x7)))), (((s64)((info & 0xfffffe00) >> 9))), pad_c));
#line 524 "basic.si"
            (string_builder_reset(sb));
#line 526 "basic.si"
            ((which_arg += 1));
#line 527 "basic.si"
        } else {
#line 528 "basic.si"
            (write_string((_builtin_slice_from(&(((*__si_slice_idx(fmt, i, u8)))), 1))));
#line 529 "basic.si"
        }

#line 530 "basic.si"
    }

#line 532 "basic.si"
    __si_ret = 0ULL;
    #line 532 "basic.si"
goto __si_scope_101_exit;
#line 533 "basic.si"

__si_scope_101_exit:;
    #line 533 "basic.si"
return __si_ret;
#line 533 "basic.si"
}

#line 535 "basic.si"
s64 Format___fmt__p0(Heap_Allocator* allocator, __si_slice fmt, __si_slice __varg0, s64 __varg1) {
    s64 __si_ret;

#line 536 "basic.si"
    String_Builder sb = (string_builder_new__p0(allocator));
#line 539 "basic.si"
    __si_ret = (Format___sbfmt__p0(&(sb), fmt, __varg0, __varg1));
    #line 539 "basic.si"
goto __si_scope_125_exit;
#line 540 "basic.si"

__si_scope_125_exit:;
    { /* DEFER SCOPE 125 */
#line 537 "basic.si"
        {
#line 537 "basic.si"
            (string_builder_free(&(sb)));
#line 537 "basic.si"
        }

    }
    #line 540 "basic.si"
return __si_ret;
#line 540 "basic.si"
}

#line 535 "basic.si"
s64 Format___fmt__p1(Heap_Allocator* allocator, __si_slice fmt) {
    s64 __si_ret;

#line 536 "basic.si"
    String_Builder sb = (string_builder_new__p0(allocator));
#line 539 "basic.si"
    __si_ret = (Format___sbfmt__p1(&(sb), fmt));
    #line 539 "basic.si"
goto __si_scope_127_exit;
#line 540 "basic.si"

__si_scope_127_exit:;
    { /* DEFER SCOPE 127 */
#line 537 "basic.si"
        {
#line 537 "basic.si"
            (string_builder_free(&(sb)));
#line 537 "basic.si"
        }

    }
    #line 540 "basic.si"
return __si_ret;
#line 540 "basic.si"
}

#line 535 "basic.si"
s64 Format___fmt__p2(Heap_Allocator* allocator, __si_slice fmt, Error __varg0) {
    s64 __si_ret;

#line 536 "basic.si"
    String_Builder sb = (string_builder_new__p0(allocator));
#line 539 "basic.si"
    __si_ret = (Format___sbfmt__p2(&(sb), fmt, __varg0));
    #line 539 "basic.si"
goto __si_scope_129_exit;
#line 540 "basic.si"

__si_scope_129_exit:;
    { /* DEFER SCOPE 129 */
#line 537 "basic.si"
        {
#line 537 "basic.si"
            (string_builder_free(&(sb)));
#line 537 "basic.si"
        }

    }
    #line 540 "basic.si"
return __si_ret;
#line 540 "basic.si"
}

#line 201 "basic.si"
s64 string_builder_append(String_Builder* sb, __si_slice s) {
    s64 __si_ret;

#line 202 "basic.si"
    s64 l = (s).len;
#line 204 "basic.si"
    if ((l == 0)) {
#line 205 "basic.si"
        __si_ret = 0ULL;
        #line 205 "basic.si"
goto __si_scope_131_exit;
#line 206 "basic.si"
    }

#line 208 "basic.si"
    s64 was_null = ((sb->data) == (((u8*)(0ULL))));
#line 210 "basic.si"
    (((sb->data) = (Allocator__bundle_realloc(&((sb->allocator)), (sb->data), ((sb->len) + l)))));
#line 212 "basic.si"
    if (was_null) {
#line 213 "basic.si"
        (((sb->cursor) = (sb->data)));
#line 214 "basic.si"
    }

#line 216 "basic.si"
    if (((sb->data) == (((u8*)(0ULL))))) {
#line 217 "basic.si"
        (string_builder_reset(sb));
#line 218 "basic.si"
        __si_ret = -2;
        #line 218 "basic.si"
goto __si_scope_131_exit;
#line 219 "basic.si"
    }

#line 221 "basic.si"
    for (s64 i = 0; (i < l); ((i += 1))){
#line 222 "basic.si"
        (((sb->len) += 1));
#line 223 "basic.si"
        (((*((sb->cursor))) = ((*__si_slice_idx(s, i, u8)))));
#line 224 "basic.si"
        (((sb->cursor) += 1ULL));
#line 225 "basic.si"
    }

#line 227 "basic.si"
    __si_ret = 0ULL;
    #line 227 "basic.si"
goto __si_scope_131_exit;
#line 228 "basic.si"

__si_scope_131_exit:;
    #line 228 "basic.si"
return __si_ret;
#line 228 "basic.si"
}

#line 8 "julie/julie.si"
s64 error_to_formatted_str(String_Builder* sb, Error arg, Format__Format_Info f) {
    s64 __si_ret;

#line 9 "julie/julie.si"
    if (((arg.which) == 0)) {
#line 10 "julie/julie.si"
        __si_ret = (string_builder_append(sb, ((__si_slice){ "No error.", (sizeof "No error.") - 1 })));
        #line 10 "julie/julie.si"
goto __si_scope_136_exit;
#line 11 "julie/julie.si"
    } else if (((arg.which) == -1)) {
#line 12 "julie/julie.si"
        __si_ret = (string_builder_append(sb, ((__si_slice){ "Too small.", (sizeof "Too small.") - 1 })));
        #line 12 "julie/julie.si"
goto __si_scope_136_exit;
#line 13 "julie/julie.si"
    } else if (((arg.which) == -2)) {
#line 14 "julie/julie.si"
        __si_ret = (string_builder_append(sb, ((__si_slice){ "Failed to allocate.", (sizeof "Failed to allocate.") - 1 })));
        #line 14 "julie/julie.si"
goto __si_scope_136_exit;
#line 15 "julie/julie.si"
    }

#line 17 "julie/julie.si"
    __si_ret = (string_builder_append(sb, ((__si_slice){ "Unknown error.", (sizeof "Unknown error.") - 1 })));
    #line 17 "julie/julie.si"
goto __si_scope_136_exit;
#line 18 "julie/julie.si"

__si_scope_136_exit:;
    #line 18 "julie/julie.si"
return __si_ret;
#line 18 "julie/julie.si"
}

#line 547 "basic.si"
s64 printf__p0(__si_slice fmt, Error __varg0) {
    s64 __si_ret;

#line 548 "basic.si"
    __si_ret = (aprintf__p2(&(heap_allocator), fmt, __varg0));
    #line 548 "basic.si"
goto __si_scope_140_exit;
#line 549 "basic.si"

__si_scope_140_exit:;
    #line 549 "basic.si"
return __si_ret;
#line 549 "basic.si"
}

#line 547 "basic.si"
s64 printf__p1(__si_slice fmt) {
    s64 __si_ret;

#line 548 "basic.si"
    __si_ret = (aprintf__p1(&(heap_allocator), fmt));
    #line 548 "basic.si"
goto __si_scope_141_exit;
#line 549 "basic.si"

__si_scope_141_exit:;
    #line 549 "basic.si"
return __si_ret;
#line 549 "basic.si"
}

#line 547 "basic.si"
s64 printf__p2(__si_slice fmt, __si_slice __varg0, s64 __varg1) {
    s64 __si_ret;

#line 548 "basic.si"
    __si_ret = (aprintf__p0(&(heap_allocator), fmt, __varg0, __varg1));
    #line 548 "basic.si"
goto __si_scope_142_exit;
#line 549 "basic.si"

__si_scope_142_exit:;
    #line 549 "basic.si"
return __si_ret;
#line 549 "basic.si"
}

#line 41 "julie/julie.si"
Error main(void) {
    Error __si_ret;

#line 44 "julie/julie.si"
    s64 tmp;
#line 47 "julie/julie.si"
    Error result = ({
        Error __si_block_val;

#line 23 "julie/julie.si"
        Error foo = ({
            Error __si_block_val;

#line 34 "julie/julie.si"
            Error tmp;
#line 35 "julie/julie.si"
            (((tmp.which) = -2));
            __si_block_val = tmp;
            __si_block_val;
#line 47 "julie/julie.si"
        });
#line 24 "julie/julie.si"
        if (((foo.which) == -2)) {
#line 25 "julie/julie.si"
            (printf__p0(((__si_slice){ "Got an error! code = {}\n", (sizeof "Got an error! code = {}\n") - 1 }), foo));
#line 26 "julie/julie.si"
            __si_ret = foo;
            #line 26 "julie/julie.si"
goto __si_scope_143_exit;
#line 27 "julie/julie.si"
        }

        __si_block_val = foo;
        __si_block_val;
#line 47 "julie/julie.si"
    });
#line 50 "julie/julie.si"
    u8* bp = (_builtin_base_pointer());
#line 51 "julie/julie.si"
    u32 argc = (*((((u32*)((bp + 16ULL))))));
#line 52 "julie/julie.si"
    u8** argv = (((u8**)((bp + 24ULL))));
#line 54 "julie/julie.si"
    if ((argc != 2ULL)) {
#line 55 "julie/julie.si"
        (printf__p1(((__si_slice){ "expected one argument: a julie file path\n", (sizeof "expected one argument: a julie file path\n") - 1 })));
#line 56 "julie/julie.si"
        (exit(1));
#line 57 "julie/julie.si"
    }

#line 59 "julie/julie.si"
    s64 x = ({
        s64 __si_block_val;

#line 61 "julie/julie.si"
        (printf__p1(((__si_slice){ "hello, world!\n", (sizeof "hello, world!\n") - 1 })));
#line 63 "julie/julie.si"
        s64 foo = 0;
#line 65 "julie/julie.si"
        if (argc) {
#line 66 "julie/julie.si"
            ((foo = 123));
#line 67 "julie/julie.si"
        } else {
#line 68 "julie/julie.si"
            ((foo = 456));
#line 69 "julie/julie.si"
        }

        __si_block_val = foo;
#line 72 "julie/julie.si"

__si_scope_148_exit:;
        { /* DEFER SCOPE 148 */
#line 60 "julie/julie.si"
            {
#line 60 "julie/julie.si"
                (printf__p1(((__si_slice){ "what does this do?\n", (sizeof "what does this do?\n") - 1 })));
#line 60 "julie/julie.si"
            }

        }
        __si_block_val;
#line 72 "julie/julie.si"
    });
#line 73 "julie/julie.si"
    (printf__p2(((__si_slice){ "{} = {}\n", (sizeof "{} = {}\n") - 1 }), ((__si_slice){ "x", (sizeof "x") - 1 }), x));
#line 75 "julie/julie.si"
    __si_ret = ({
        Error __si_block_val;

#line 34 "julie/julie.si"
        Error tmp;
#line 35 "julie/julie.si"
        (((tmp.which) = 0));
        __si_block_val = tmp;
        __si_block_val;
#line 75 "julie/julie.si"
    });
    #line 75 "julie/julie.si"
goto __si_scope_143_exit;
#line 76 "julie/julie.si"

__si_scope_143_exit:;
    { /* DEFER SCOPE 143 */
#line 42 "julie/julie.si"
        {
#line 42 "julie/julie.si"
            (exit(0));
#line 42 "julie/julie.si"
        }

    }
    #line 76 "julie/julie.si"
return __si_ret;
#line 76 "julie/julie.si"
}

#line 40 "basic.si"
u8* heap_alloc(Heap_Allocator* allocator, u64 n_bytes) {
    u8* __si_ret;

#line 40 "basic.si"
    __si_ret = (malloc(n_bytes));
    #line 40 "basic.si"
goto __si_scope_154_exit;
#line 40 "basic.si"

__si_scope_154_exit:;
    #line 40 "basic.si"
return __si_ret;
#line 40 "basic.si"
}

#line 43 "basic.si"
u8* heap_realloc(Heap_Allocator* allocator, u8* ptr, u64 n_bytes) {
    u8* __si_ret;

#line 43 "basic.si"
    __si_ret = (realloc(ptr, n_bytes));
    #line 43 "basic.si"
goto __si_scope_155_exit;
#line 43 "basic.si"

__si_scope_155_exit:;
    #line 43 "basic.si"
return __si_ret;
#line 43 "basic.si"
}

#line 46 "basic.si"
void heap_free(Heap_Allocator* allocator, u8* ptr) {
#line 46 "basic.si"
    (free(ptr));
#line 46 "basic.si"

__si_scope_156_exit:;
#line 46 "basic.si"
}

#line 49 "basic.si"
Allocator__Bundle heap_bundle(Heap_Allocator* allocator) {
    Allocator__Bundle __si_ret;

#line 50 "basic.si"
    Allocator__Bundle ret;
#line 51 "basic.si"
    (((ret.self) = (((u8*)(allocator)))));
#line 52 "basic.si"
    (((ret.alloc) = (((__proc_type_47)(heap_alloc)))));
#line 53 "basic.si"
    (((ret.realloc) = (((__proc_type_49)(heap_realloc)))));
#line 54 "basic.si"
    (((ret.free) = (((__proc_type_51)(heap_free)))));
#line 55 "basic.si"
    __si_ret = ret;
    #line 55 "basic.si"
goto __si_scope_157_exit;
#line 56 "basic.si"

__si_scope_157_exit:;
    #line 56 "basic.si"
return __si_ret;
#line 56 "basic.si"
}

#line 63 "basic.si"
Fixed_Allocator fixed_allocator_new(u8* buff, u64 size) {
    Fixed_Allocator __si_ret;

#line 64 "basic.si"
    Fixed_Allocator ret;
#line 66 "basic.si"
    (((ret.base) = buff));
#line 67 "basic.si"
    (((ret.end) = (buff + size)));
#line 69 "basic.si"
    __si_ret = ret;
    #line 69 "basic.si"
goto __si_scope_158_exit;
#line 70 "basic.si"

__si_scope_158_exit:;
    #line 70 "basic.si"
return __si_ret;
#line 70 "basic.si"
}

#line 73 "basic.si"
u8* fixed_alloc(Fixed_Allocator* allocator, u64 n_bytes) {
    u8* __si_ret;

#line 76 "basic.si"
    if ((((allocator->base) + n_bytes) > (allocator->end))) {
#line 76 "basic.si"
        __si_ret = (((u8*)(0ULL)));
        #line 76 "basic.si"
goto __si_scope_159_exit;
#line 76 "basic.si"
    }

#line 78 "basic.si"
    u8* ret = (allocator->base);
#line 80 "basic.si"
    (((allocator->base) += n_bytes));
#line 82 "basic.si"
    __si_ret = ret;
    #line 82 "basic.si"
goto __si_scope_159_exit;
#line 83 "basic.si"

__si_scope_159_exit:;
    #line 83 "basic.si"
return __si_ret;
#line 83 "basic.si"
}

#line 86 "basic.si"
void fixed_free(Fixed_Allocator* allocator, u8* ptr) {
#line 86 "basic.si"

__si_scope_161_exit:;
#line 86 "basic.si"
}

#line 93 "basic.si"
Singleton_Allocator singleton_allocator_new(u8* buff, u64 size) {
    Singleton_Allocator __si_ret;

#line 94 "basic.si"
    Singleton_Allocator ret;
#line 96 "basic.si"
    (((ret.base) = buff));
#line 97 "basic.si"
    (((ret.end) = (buff + size)));
#line 99 "basic.si"
    __si_ret = ret;
    #line 99 "basic.si"
goto __si_scope_162_exit;
#line 100 "basic.si"

__si_scope_162_exit:;
    #line 100 "basic.si"
return __si_ret;
#line 100 "basic.si"
}

#line 103 "basic.si"
u8* singleton_alloc(Singleton_Allocator* allocator, u64 n_bytes) {
    u8* __si_ret;

#line 106 "basic.si"
    if ((((allocator->base) + n_bytes) > (allocator->end))) {
#line 106 "basic.si"
        __si_ret = (((u8*)(0ULL)));
        #line 106 "basic.si"
goto __si_scope_163_exit;
#line 106 "basic.si"
    }

#line 108 "basic.si"
    __si_ret = (allocator->base);
    #line 108 "basic.si"
goto __si_scope_163_exit;
#line 109 "basic.si"

__si_scope_163_exit:;
    #line 109 "basic.si"
return __si_ret;
#line 109 "basic.si"
}

#line 112 "basic.si"
u8* singleton_realloc(Singleton_Allocator* allocator, u8* ptr, u64 n_bytes) {
    u8* __si_ret;

#line 115 "basic.si"
    if (((ptr != (((u8*)(0ULL)))) && (ptr != (allocator->base)))) {
#line 115 "basic.si"
        __si_ret = (((u8*)(0ULL)));
        #line 115 "basic.si"
goto __si_scope_165_exit;
#line 115 "basic.si"
    }

#line 116 "basic.si"
    if ((((allocator->base) + n_bytes) > (allocator->end))) {
#line 116 "basic.si"
        __si_ret = (((u8*)(0ULL)));
        #line 116 "basic.si"
goto __si_scope_165_exit;
#line 116 "basic.si"
    }

#line 118 "basic.si"
    __si_ret = (allocator->base);
    #line 118 "basic.si"
goto __si_scope_165_exit;
#line 119 "basic.si"

__si_scope_165_exit:;
    #line 119 "basic.si"
return __si_ret;
#line 119 "basic.si"
}

#line 122 "basic.si"
void singleton_free(Singleton_Allocator* allocator, u8* ptr) {
#line 122 "basic.si"

__si_scope_168_exit:;
#line 122 "basic.si"
}

#line 125 "basic.si"
Allocator__Bundle singleton_bundle(Singleton_Allocator* allocator) {
    Allocator__Bundle __si_ret;

#line 126 "basic.si"
    Allocator__Bundle ret;
#line 127 "basic.si"
    (((ret.self) = (((u8*)(allocator)))));
#line 128 "basic.si"
    (((ret.alloc) = (((__proc_type_47)(singleton_alloc)))));
#line 129 "basic.si"
    (((ret.realloc) = (((__proc_type_49)(singleton_realloc)))));
#line 130 "basic.si"
    (((ret.free) = (((__proc_type_51)(singleton_free)))));
#line 131 "basic.si"
    __si_ret = ret;
    #line 131 "basic.si"
goto __si_scope_169_exit;
#line 132 "basic.si"

__si_scope_169_exit:;
    #line 132 "basic.si"
return __si_ret;
#line 132 "basic.si"
}

#line 166 "basic.si"
String_Builder string_builder_new__p0(Heap_Allocator* allocator) {
    String_Builder __si_ret;

#line 167 "basic.si"
    String_Builder ret;
#line 168 "basic.si"
    (string_builder_reset(&(ret)));
#line 169 "basic.si"
    (((ret.allocator) = (heap_bundle(allocator))));
#line 170 "basic.si"
    __si_ret = ret;
    #line 170 "basic.si"
goto __si_scope_170_exit;
#line 171 "basic.si"

__si_scope_170_exit:;
    #line 171 "basic.si"
return __si_ret;
#line 171 "basic.si"
}

#line 173 "basic.si"
void string_builder_free(String_Builder* sb) {
#line 174 "basic.si"
    if (((sb->data) != (((u8*)(0ULL))))) {
#line 175 "basic.si"
        (Allocator__bundle_free(&((sb->allocator)), (sb->data)));
#line 176 "basic.si"
    }

#line 177 "basic.si"
    (string_builder_reset(sb));
#line 178 "basic.si"

__si_scope_171_exit:;
#line 178 "basic.si"
}

#line 230 "basic.si"
__si_slice string_builder_get(String_Builder* sb) {
    __si_slice __si_ret;

#line 231 "basic.si"
    __si_ret = (_builtin_slice_from((sb->data), (sb->len)));
    #line 231 "basic.si"
goto __si_scope_173_exit;
#line 232 "basic.si"

__si_scope_173_exit:;
    #line 232 "basic.si"
return __si_ret;
#line 232 "basic.si"
}

#line 234 "basic.si"
s64 cstr_len(u8* cstr) {
    s64 __si_ret;

#line 235 "basic.si"
    s64 l = 0;
#line 236 "basic.si"
    for (; (*((cstr + l))); ((l += 1))){
#line 236 "basic.si"
    }

#line 237 "basic.si"
    __si_ret = l;
    #line 237 "basic.si"
goto __si_scope_174_exit;
#line 238 "basic.si"

__si_scope_174_exit:;
    #line 238 "basic.si"
return __si_ret;
#line 238 "basic.si"
}

#line 244 "basic.si"
s64 is_space(u8 c) {
    s64 __si_ret;

#line 245 "basic.si"
    u32 d = ((((u32)(c))) - 9ULL);
#line 246 "basic.si"
    __si_ret = ((8388639ULL >> (d & 31ULL)) & (1ULL >> (d >> 5ULL)));
    #line 246 "basic.si"
goto __si_scope_176_exit;
#line 247 "basic.si"

__si_scope_176_exit:;
    #line 247 "basic.si"
return __si_ret;
#line 247 "basic.si"
}

#line 249 "basic.si"
s64 is_digit(u8 c) {
    s64 __si_ret;

#line 250 "basic.si"
    __si_ret = ((((47ULL - c) & (c - 58ULL)) >> 31ULL) != 0ULL);
    #line 250 "basic.si"
goto __si_scope_177_exit;
#line 251 "basic.si"

__si_scope_177_exit:;
    #line 251 "basic.si"
return __si_ret;
#line 251 "basic.si"
}

#line 253 "basic.si"
s64 is_alpha(u8 c) {
    s64 __si_ret;

#line 254 "basic.si"
    __si_ret = ((((96ULL - (c | 32ULL)) & ((c | 32ULL) - 123ULL)) >> 31ULL) != 0ULL);
    #line 254 "basic.si"
goto __si_scope_178_exit;
#line 255 "basic.si"

__si_scope_178_exit:;
    #line 255 "basic.si"
return __si_ret;
#line 255 "basic.si"
}

#line 257 "basic.si"
s64 is_alnum(u8 c) {
    s64 __si_ret;

#line 258 "basic.si"
    __si_ret = ((is_alpha(c)) || (is_digit(c)));
    #line 258 "basic.si"
goto __si_scope_179_exit;
#line 259 "basic.si"

__si_scope_179_exit:;
    #line 259 "basic.si"
return __si_ret;
#line 259 "basic.si"
}

#line 543 "basic.si"
s64 aprintf__p0(Heap_Allocator* allocator, __si_slice fmt, __si_slice __varg0, s64 __varg1) {
    s64 __si_ret;

#line 544 "basic.si"
    __si_ret = (Format___fmt__p0(allocator, fmt, __varg0, __varg1));
    #line 544 "basic.si"
goto __si_scope_180_exit;
#line 545 "basic.si"

__si_scope_180_exit:;
    #line 545 "basic.si"
return __si_ret;
#line 545 "basic.si"
}

#line 543 "basic.si"
s64 aprintf__p1(Heap_Allocator* allocator, __si_slice fmt) {
    s64 __si_ret;

#line 544 "basic.si"
    __si_ret = (Format___fmt__p1(allocator, fmt));
    #line 544 "basic.si"
goto __si_scope_181_exit;
#line 545 "basic.si"

__si_scope_181_exit:;
    #line 545 "basic.si"
return __si_ret;
#line 545 "basic.si"
}

#line 543 "basic.si"
s64 aprintf__p2(Heap_Allocator* allocator, __si_slice fmt, Error __varg0) {
    s64 __si_ret;

#line 544 "basic.si"
    __si_ret = (Format___fmt__p2(allocator, fmt, __varg0));
    #line 544 "basic.si"
goto __si_scope_182_exit;
#line 545 "basic.si"

__si_scope_182_exit:;
    #line 545 "basic.si"
return __si_ret;
#line 545 "basic.si"
}

