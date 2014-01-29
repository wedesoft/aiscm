.SUFFIXES: .c .o .txt

all: t1.txt

clean:
	rm -f *.txt *.o

.o.txt:
	objdump -d -M intel -S $< > $@

.c.o:
	gcc -g -c $<
