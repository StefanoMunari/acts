# base image
FROM m0bius/city-base-py:latest

ENV TERM xterm

########PACKAGES
RUN DEBIAN_FRONTEND=noninteractive apt-get update &&\
   apt-get upgrade -y &&\
   apt-get install -y \
   python-requests \
   python-pika

RUN git clone https://github.com/msabramo/requests-unixsocket

WORKDIR requests-unixsocket

RUN git fetch --all &&\
   git checkout -f tags/0.1.5 -b 0.1.5

RUN python2.7 setup.py install

ADD exec.sh /
# ADD swarmer.py /home
# ADD swarmer.config.json /home

ENTRYPOINT ["sh","/exec.sh","&"]