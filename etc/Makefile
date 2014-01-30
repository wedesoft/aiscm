.SUFFIXES: .s .c .o .txt

all: t1 t1.s t1.txt

t1: t1.o
	gcc -o $@ $<

.o.txt:
	objdump -d -M intel -S $< > $@

.c.o:
	gcc -O -march=native -g -c $<

.c.s:
	gcc -O -fverbose-asm -S $<

clean:
	rm -f *.txt *.o *.s

