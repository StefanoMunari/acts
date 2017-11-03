################################################################################
# Provide a image to check the generated city configurations:
# - install Tarjan algorithm implementation in python
# (you can check the city configuration by mounting them as volumes inside
# the container and running the tarjan checker on them to know if they are
# all SCC)
################################################################################
# base image
FROM m0bius/city-base-py:latest

ENV TERM xterm
ENV CITY_ROOT /home

RUN git clone https://github.com/bwesterb/py-tarjan

WORKDIR py-tarjan

RUN git fetch --all &&\
   git checkout -f tags/0.2.3.2 -b 0.2.3.2

RUN python2.7 setup.py install

ENTRYPOINT []