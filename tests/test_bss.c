// https://stackoverflow.com/questions/38570495/aarch64-relocation-prefixes
// https://maskray.me/blog/2021-08-29-all-about-global-offset-table
// https://stackoverflow.com/questions/55587313/why-use-the-global-offset-table-for-symbols-defined-in-the-shared-library-itself

#define ARR_LENGTH 16

extern int write(int, void *, int);
extern void exit(int);

void fi(int *i) {}
void fd(double *d) {}

static double d1;
static char arr[ARR_LENGTH];
static int i1;
static int i2;

int main(void) {
    int i;
    for (i = 0; i < ARR_LENGTH; i++) {
        arr[i] = 'a' + i;
    }
    write(1, arr, ARR_LENGTH);
    fi(&i1);
    fi(&i2);
    fd(&d1);

    return 0;
}
