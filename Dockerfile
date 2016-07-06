# docker build -t wedesoft/aiscm-xenial .
# docker run -v /home/jan/test/aiscm:/mnt/aiscm -t -i wedesoft/aiscm /bin/bash
# FROM debian:jessie
FROM ubuntu:xenial
MAINTAINER Jan Wedekind <jan@wedesoft.de>
RUN apt-get update
RUN apt-get install -q -y build-essential
RUN apt-get install -q -y autoconf automake libtool
RUN apt-get install -q -y devscripts equivs
RUN apt-get install -q -y gettext guile-2.0-dev linux-libc-dev libmjpegtools-dev imagemagick libmagickcore-dev \
  libpulse-dev libjpeg-dev libx11-dev libxext-dev libxv-dev libxmu-dev libxi-dev libglu1-mesa-dev libgl1-mesa-dev \
     libswscale-dev libavutil-dev libavcodec-dev libavformat-dev pandoc
RUN mkdir -p /usr/src/aiscm
COPY . /usr/src/aiscm/
WORKDIR /usr/src/aiscm
RUN mk-build-deps --install --remove --tool 'apt-get -q --yes' debian/control
RUN ./bootstrap
RUN ./configure
RUN make -j
RUN make check -j
