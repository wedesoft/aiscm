# docker build -t wedesoft/aiscm-xenial .
# docker run -t -i wedesoft/aiscm-xenial /bin/bash
# FROM debian:jessie
FROM ubuntu:xenial
MAINTAINER Jan Wedekind <jan@wedesoft.de>
RUN apt-get update
RUN apt-get install -q -y build-essential
RUN apt-get install -q -y autoconf automake libtool
RUN apt-get install -q -y devscripts equivs
RUN apt-get install -q -y gettext
RUN apt-get install -q -y guile-2.0-dev
RUN apt-get install -q -y linux-libc-dev
RUN apt-get install -q -y libmjpegtools-dev
RUN apt-get install -q -y imagemagick
RUN apt-get install -q -y libmagickcore-dev
RUN apt-get install -q -y libpulse-dev
RUN apt-get install -q -y libjpeg-dev
RUN apt-get install -q -y libx11-dev
RUN apt-get install -q -y libxext-dev
RUN apt-get install -q -y libxv-dev
RUN apt-get install -q -y libxmu-dev
RUN apt-get install -q -y libxi-dev
RUN apt-get install -q -y libglu1-mesa-dev
RUN apt-get install -q -y libgl1-mesa-dev
RUN apt-get install -q -y libswscale-dev
RUN apt-get install -q -y libavutil-dev
RUN apt-get install -q -y libavcodec-dev
RUN apt-get install -q -y libavformat-dev
RUN apt-get install -q -y pandoc
RUN mkdir -p /usr/src/aiscm
COPY . /usr/src/aiscm/
WORKDIR /usr/src/aiscm
RUN mk-build-deps --install --remove --tool 'apt-get -q --yes' debian/control
RUN ./bootstrap
RUN ./configure
RUN make -j
RUN make check -j
