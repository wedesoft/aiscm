# sudo docker build -t wedesoft/aiscm .
# sudo docker run -v /home/jan/test/aiscm:/aiscm -t -i wedesoft/aiscm /bin/bash
# You can use "FROM debian:jessie" instead if you are working under Ubuntu
FROM ubuntu:trusty
MAINTAINER Jan Wedekind <jan@wedesoft.de>

RUN apt-get update && apt-get install -y ansible python-apt

RUN mkdir -p /usr/src/aiscm
WORKDIR /usr/src/aiscm
COPY aiscm.yml .

RUN ansible-playbook -i "localhost," -c local --tags tools,dependencies aiscm.yml
