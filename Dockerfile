# Docker file to build and test AIscm on different versions of Debian
#
# configure /etc/default/docker
# DOCKER_OPTS="--dns 8.8.8.8 --dns 8.8.4.4 --ip-masq=true"
FROM ubuntu:bionic
MAINTAINER Jan Wedekind <jan@wedesoft.de>
RUN apt-get update
RUN apt-get -q -y dist-upgrade
RUN apt-get install -q -y apt-utils
RUN apt-get install -q -y build-essential
RUN apt-get install -q -y autoconf automake libtool
RUN apt-get install -q -y devscripts equivs
RUN apt-get install -q -y linux-libc-dev
RUN apt-get install -q -y guile-2.2-dev
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
RUN mkdir -p /usr/src/aiscm
WORKDIR /usr/src/aiscm
ADD debian/control debian/control
RUN mk-build-deps --install --remove --tool 'apt-get -q --yes' debian/control
COPY aiscm.tar.gz .
COPY aiscm.tar.gz.asc .
ADD configure.ac .
ADD debian debian
ADD Makefile.package .
RUN make -f Makefile.package
RUN dpkg --install pkg/aiscm_*.deb
