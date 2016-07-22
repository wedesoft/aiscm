# Docker file to build and test AIscm on different versions of Debian
#
# docker build -t wedesoft/aiscm-debian .
# docker run -t -i -v /home/jan/test/aiscm:/mnt wedesoft/aiscm-debian /bin/bash
FROM debian:jessie
# FROM debian:sid
# FROM ubuntu:trusty
# FROM ubuntu:xenial
MAINTAINER Jan Wedekind <jan@wedesoft.de>
RUN apt-get update
# RUN apt-get upgrade -q -y
RUN apt-get install -q -y build-essential
RUN apt-get install -q -y autoconf automake libtool
RUN apt-get install -q -y devscripts equivs
RUN apt-get install -q -y linux-libc-dev
RUN apt-get install -q -y guile-2.0-dev
RUN apt-get install -q -y libx11-dev libxext-dev libxv-dev libxmu-dev libxi-dev libglu1-mesa-dev libgl1-mesa-dev
RUN apt-get install -q -y gettext
RUN apt-get install -q -y pandoc
RUN apt-get install -q -y libjpeg-dev libmjpegtools-dev
RUN apt-get install -q -y libpulse-dev
RUN apt-get install -q -y imagemagick libmagickcore-dev
RUN apt-get install -q -y libswscale-dev libavutil-dev libavcodec-dev libavformat-dev
RUN mkdir -p /usr/src/aiscm/debian
WORKDIR /usr/src/aiscm
ADD debian/control debian/control
RUN mk-build-deps --install --remove --tool 'apt-get -q --yes' debian/control
ENV VERSION 0.5.1
ADD aiscm-$VERSION.tar.xz .
WORKDIR /usr/src/aiscm/aiscm-$VERSION
RUN ./configure
RUN make -j
RUN make check -j
