PREFIX = aarch64-unknown-linux-musl-
CFLAGS = -Wall -Wextra -pedantic
CC = $(PREFIX)gcc
LDFLAGS = -nostdlib -static

TEST_BSS = test_bss.S test_bss.o test_bss.o.dis test_bss.o.dump test_bss.gcc test_bss.gcc.dis test_bss.gcc.dump
TEST_LOCAL_TXT = test_local_txt.o test_local_txt.o.dump test_local_txt.o.dis test_local_txt.gcc

ALL = $(TEST_BSS) $(TEST_LOCAL_TXT)

all : $(ALL)

test_bss.gcc : LDFLAGS = -static

%.S : %.c
	$(CC) $(CFLAGS) -S $< -o $@

%.gcc : %.o
	$(CC) $(LDFLAGS) $< -o $@

%.dummy: %.o
	hld $< $@

%.dis : %
	$(PREFIX)objdump -D $< > $@

%.layout : %
	hobjlayout $< > $@

%.dump : %
	hobjdump -f $< > $@

# https://pandoc.org/MANUAL.html
%.html : %.md
	pandoc --standalone -o $@ $<

clean:
	rm -rf *.html $(ALL)
