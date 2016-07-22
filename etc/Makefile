.SUFFIXES: .s .c .o .txt

CFLAGS = -O

all: assembler pa-beep pa-mic-2-speaker

assembler: t1.s t1.txt
	cat t1.txt

pa-beep: pa-beep.c
	gcc $(shell pkg-config --cflags --libs libpulse) $< -o $@ -lm

pa-mic-2-speaker: pa-mic-2-speaker.c
	gcc $(shell pkg-config --cflags --libs libpulse) $< -o $@ -lm

.o.txt:
	objdump -d -M intel -S $< > $@

.c.o:
	gcc $(CFLAGS) -march=native -g -c $<

.c.s:
	gcc $(CFLAGS) -fverbose-asm -S $<

clean:
	rm -f *.txt *.o *.s t1.txt pa-beep pa-mic-2-speaker
