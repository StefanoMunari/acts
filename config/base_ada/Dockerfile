################################################################################
# Install common deps necessary to develop a project in Ada:
# gnat4.9 : the Ada compiler
# gprbuild : necessary to build gpr projects
# asis-program : necessary to use gnattest
# gnatcoll : external library which contains a JSON parser
# aunit : a testing framework for Ada
################################################################################
FROM debian:latest

RUN apt-get update && apt-get upgrade -y \
&& apt-get install -y build-essential \
                        gnat \
                        gprbuild \
                        asis-programs \
                        libgnatcoll1.7-dev \
                        libaunit3.7.2-dev \
