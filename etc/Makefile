.SUFFIXES: .s .c .o .txt

CFLAGS = -O

all: t1 t1.s t1.txt t2 t2.s t2.txt

t1: t1.o
	gcc -o $@ $<

t2: t2.o
	gcc -o $@ $<

.o.txt:
	objdump -d -M intel -S $< > $@

.c.o:
	gcc $(CFLAGS) -march=native -g -c $<

.c.s:
	gcc $(CFLAGS) -fverbose-asm -S $<

clean:
	rm -f *.txt *.o *.s

