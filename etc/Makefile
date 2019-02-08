.SUFFIXES: .s .c .o .txt .mp4

CFLAGS = -O
CXXFLAGS = -I/usr/local/include/opencv4

all: assembler cc

assembler: t1.txt
	cat t1.txt

cc: cc.o
	g++ -o $@ cc.o -lopencv_imgproc -lopencv_core

.o.txt:
	objdump -d -M intel -S $< > $@

.c.o:
	gcc $(CFLAGS) -march=native -g -c $<

.cpp.o:
	g++ -c $< -o $@ $(CXXFLAGS) 

clean:
	rm -f muxing muxing.mp4 *.txt *.o
