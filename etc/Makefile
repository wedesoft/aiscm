.SUFFIXES: .s .c .o .txt

CFLAGS = -O

all: t t1.s t1.txt t2.s t2.txt

t: t1.o t2.o
	gcc -o $@ t1.o t2.o

.o.txt:
	objdump -d -M intel -S $< > $@

.c.o:
	gcc $(CFLAGS) -march=native -g -c $<

.c.s:
	gcc $(CFLAGS) -fverbose-asm -S $<

clean:
	rm -f t *.txt *.o *.s
