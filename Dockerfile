# Docker file to build and test AIscm on different versions of Debian
#
# configure /etc/default/docker
# DOCKER_OPTS="--dns 8.8.8.8 --dns 8.8.4.4 --ip-masq=true"
FROM debian:sid
MAINTAINER Jan Wedekind <jan@wedesoft.de>
RUN echo "deb http://httpredir.debian.org/debian unstable main" > /etc/apt/sources.list
RUN apt-get update  # Forced update Mo 7. Sep 10:29:43 BST 2020
RUN apt-get -q -y dist-upgrade
RUN apt-get install -q -y apt-utils
RUN apt-get install -q -y build-essential
RUN apt-get install -q -y autoconf automake libtool
RUN apt-get install -q -y devscripts equivs
RUN apt-get install -q -y linux-libc-dev
RUN apt-get install -q -y guile-3.0-dev
RUN apt-get install -q -y libx11-dev libxext-dev libxv-dev libxmu-dev libxi-dev libglu1-mesa-dev libgl1-mesa-dev
RUN apt-get install -q -y gettext
RUN apt-get install -q -y pandoc
RUN apt-get install -q -y libjpeg-dev libmjpegtools-dev
RUN apt-get install -q -y libpulse-dev
RUN apt-get install -q -y imagemagick libmagickcore-dev
RUN apt-get install -q -y libswresample-dev
RUN apt-get install -q -y libswscale-dev
RUN apt-get install -q -y libavutil-dev
RUN apt-get install -q -y libavcodec-dev
RUN apt-get install -q -y libavformat-dev
RUN apt-get install -q -y libxpm-dev
RUN apt-get install -q -y llvm-9
RUN apt-get install -q -y llvm-9-dev
RUN apt-get install -q -y clang-9
RUN apt-get install -q -y libomp5-9
RUN apt-get install -q -y libomp-9-dev
RUN apt-get install -q -y clearsilver-dev
RUN apt-get install -q -y cmake
RUN apt-get install -q -y wget
RUN mkdir -p /usr/src/aiscm
WORKDIR /usr/src
RUN wget -q https://github.com/protocolbuffers/protobuf/releases/download/v3.10.0/protobuf-all-3.10.0.tar.gz
RUN wget -q https://github.com/protobuf-c/protobuf-c/releases/download/v1.3.2/protobuf-c-1.3.2.tar.gz
RUN wget -q https://github.com/opencv/opencv/archive/4.1.2.tar.gz -O opencv-4.1.2.tar.gz
RUN wget -q https://github.com/opencv/opencv_contrib/archive/4.1.2.tar.gz -O opencv_contrib-4.1.2.tar.gz
RUN wget -q https://storage.googleapis.com/tensorflow/libtensorflow/libtensorflow-cpu-linux-x86_64-1.14.0.tar.gz
RUN tar xzf protobuf-all-3.10.0.tar.gz
RUN tar xzf protobuf-c-1.3.2.tar.gz
RUN tar xzf opencv-4.1.2.tar.gz
RUN tar xzf opencv_contrib-4.1.2.tar.gz
RUN tar xz -C /usr -f libtensorflow-cpu-linux-x86_64-1.14.0.tar.gz
WORKDIR /usr/src/protobuf-3.10.0
RUN ./configure --prefix=/usr
RUN make -j `nproc`
RUN make install
WORKDIR /usr/src/protobuf-c-1.3.2
RUN ./configure --prefix=/usr
RUN make -j `nproc`
RUN make install
RUN mkdir /usr/src/opencv-4.1.2/build
WORKDIR /usr/src/opencv-4.1.2/build
RUN cmake -DCMAKE_INSTALL_PREFIX=/usr -DOPENCV_GENERATE_PKGCONFIG=YES -DOPENCV_EXTRA_MODULES_PATH=../../opencv_contrib-4.1.2/modules ..
RUN make -j `nproc`
RUN make install
WORKDIR /usr/src/aiscm
ADD debian/control debian/control
RUN mk-build-deps --install --remove --tool 'apt-get -q --yes' debian/control
COPY aiscm.tar.xz .
COPY aiscm.tar.xz.asc .
ADD configure.ac .
ADD debian debian
ADD Makefile.package .
RUN make -f Makefile.package
RUN dpkg --install pkg/aiscm_*.deb
