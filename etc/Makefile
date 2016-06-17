.SUFFIXES: .s .c .o .txt

CFLAGS = -O

all: assembler

assembler: t1.s t1.txt
	cat t1.txt

.o.txt:
	objdump -d -M intel -S $< > $@

.c.o:
	gcc $(CFLAGS) -march=native -g -c $<

.c.s:
	gcc $(CFLAGS) -fverbose-asm -S $<

clean:
	rm -f *.txt *.o *.s t1.txt
