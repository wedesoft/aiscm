.SUFFIXES: .s .c .o .txt

CFLAGS = -O

all: assembler magicktest

assembler: t1.s t1.txt
	cat t1.txt

magicktest: writeimg
	./writeimg

writeimg: writeimg.c
	gcc $(shell pkg-config --cflags ImageMagick) -o $@ $< $(shell pkg-config --libs ImageMagick)

.o.txt:
	objdump -d -M intel -S $< > $@

.c.o:
	gcc $(CFLAGS) -march=native -g -c $<

.c.s:
	gcc $(CFLAGS) -fverbose-asm -S $<

clean:
	rm -f t *.txt *.o *.s
