.SUFFIXES: .s .c .o .txt

CFLAGS = -O

all: assembler pacat parec

assembler: t1.s t1.txt
	cat t1.txt

pacat: pacat.o
	gcc -o $@ $< -lpulse-simple -lpulse

parec: parec.o
	gcc -o $@ $< -lpulse-simple -lpulse

.o.txt:
	objdump -d -M intel -S $< > $@

.c.o:
	gcc $(CFLAGS) -march=native -g -c $<

.c.s:
	gcc $(CFLAGS) -fverbose-asm -S $<

clean:
	rm -f *.txt *.o *.s
