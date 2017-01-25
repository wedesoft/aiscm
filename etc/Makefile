.SUFFIXES: .s .c .o .txt .mp4

CFLAGS = -O

all: assembler muxing.mp4

assembler: t1.txt
	cat t1.txt

muxing.mp4: muxing
	./$< $@

muxing: muxing.c
	gcc -o $@ muxing.c -lavcodec -lavformat -lswresample -lswscale -lavutil -lm

.o.txt:
	objdump -d -M intel -S $< > $@

.c.o:
	gcc $(CFLAGS) -march=native -g -c $<

clean:
	rm -f muxing muxing.mp4 *.txt *.o
