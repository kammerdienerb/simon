typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;
typedef signed char        s8;
typedef short              s16;
typedef int                s32;
typedef long long          s64;


s32 open(char*, s32);
s32 read(s32, char*, u64);
s32 write(s32, char*, u64);
u64 __util_slen(char*);
void __util_puts(char*);
void hello(void);

u64 __util_slen(char* s)
{
    u64 len = (u64)(0);
    for (; *s != (char)(0); s += 1)
    {
        len += 1;
    }
    return len;
}

void __util_puts(char* s)
{
    sys.write((s32)(1), s, slen(s));
}

void hello(void)
{
    util.puts("this is a config file parser\n");
}

