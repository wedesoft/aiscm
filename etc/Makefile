.SUFFIXES: .s .c .o .txt .mp4

CFLAGS = -O
CXXFLAGS = -I/usr/local/include/opencv4

all: cc

cc: cc.o
	g++ -o $@ cc.o -lopencv_aruco -lopencv_imgcodecs -lopencv_core

.c.o:
	gcc $(CFLAGS) -march=native -g -c $<

.cpp.o:
	g++ -c $< -o $@ $(CXXFLAGS)

clean:
	rm -f cc *.o
