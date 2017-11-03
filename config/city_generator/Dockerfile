################################################################################
# Provide a image with a city generator:
# - the generator creates a set of city configuration files and writes
# them to a mounted volume
# (which you have to specify when creating the container)
################################################################################
# @see base_python
FROM m0bius/city-base-py:latest

ENV TERM xterm
ENV CITY_ROOT /home

RUN DEBIAN_FRONTEND=noninteractive \
   apt-get install -y \
   python-pip &&\
   pip install enum34

RUN git clone https://bitbucket.org/gmidena/districtjsongenerator

WORKDIR districtjsongenerator

RUN git fetch --all &&\
   git checkout -f -b release origin/release

ENTRYPOINT []