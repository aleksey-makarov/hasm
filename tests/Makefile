PREFIX = aarch64-unknown-linux-gnu-
CFLAGS = -Wall
CC = $(PREFIX)gcc
LDFLAGS = -nostdlib

all: test_bss.o test_bss.o.dis test_bss.o.dump test_bss.gcc test_bss.gcc.dis test_bss.gcc.dump

test_bss.o : CFLAGS += -O3
test_bss.gcc : LDFLAGS =

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
	rm -rf *.html *.dump *.layout *.dis *.dummy *.gcc *.o
