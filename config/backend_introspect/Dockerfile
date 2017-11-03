################################################################################
# Enable to use UDS socket through python
# - allows the container to query its on docker environment
# (its is necessary to mount docker.sock as volume inside the container)
################################################################################
FROM m0bius/city-base1:latest

########DEPENDENCIES
RUN apt-get update && apt-get upgrade -y \
&& apt-get install -y \
   git \
   python-setuptools \
   python-requests

########INSTALL
# python Unix Domain Socket library wrapper
RUN git clone https://github.com/msabramo/requests-unixsocket

WORKDIR requests-unixsocket

RUN git fetch --all &&\
   git checkout -f tags/0.1.5 -b 0.1.5

RUN python2.7 setup.py install