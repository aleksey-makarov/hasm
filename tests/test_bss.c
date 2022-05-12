// https://stackoverflow.com/questions/38570495/aarch64-relocation-prefixes
// https://maskray.me/blog/2021-08-29-all-about-global-offset-table
// https://stackoverflow.com/questions/55587313/why-use-the-global-offset-table-for-symbols-defined-in-the-shared-library-itself

#include <stddef.h>
#include <stdint.h>

#define UINT32_LENGTH 8
#define ARR_LENGTH    (UINT32_LENGTH * 2 + 1)

extern int write(int, void *, int);
extern int read(int, void *, int);
extern void exit(int);

static char arr[ARR_LENGTH];
static uint32_t a;
static uint32_t b;
static uint32_t res;

uint32_t char_to_int(char c)
{
    if ('a' <= c && c <= 'f')
        return c - 'a' + 0xa;
    else if ('A' <= c && c <= 'F')
        return c - 'A' + 0xa;
    else
        return c - '0';
}

char int_to_char(uint32_t n)
{
    if (n <= 9)
        return n + '0';
    else
        return n - 0xa + 'a';
}

void read_uint32(char *p, uint32_t *n)
{
    int i;
    uint32_t res = 0;

    for (i = 0; i < UINT32_LENGTH; i++) {
        res = (res << 4) + char_to_int(p[i]);
    }

    *n = res;
}

void write_uint32(uint32_t n, char p[UINT32_LENGTH])
{
    int i;

    for (i = 0; i < UINT32_LENGTH; i++) {
        p[i] = int_to_char( (n >> ((7 - i) * 4)) & 0xf );
    }
}

int main(void) {

    read(0, arr, ARR_LENGTH);

    read_uint32(arr, &a);
    read_uint32(arr + UINT32_LENGTH + 1, &b);

    res = a + b;

    write_uint32(res, arr);

    write(1, arr, UINT32_LENGTH);

    return 0;
}
