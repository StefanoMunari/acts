################################################################################
# Provide a base image which enables to install python projects
################################################################################
FROM debian:latest

RUN DEBIAN_FRONTEND=noninteractive \
   apt-get update &&\
   apt-get upgrade -y

RUN DEBIAN_FRONTEND=noninteractive \
   apt-get install -y \
   python2.7 \
   python-setuptools \
   git