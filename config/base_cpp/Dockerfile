################################################################################
# Install common deps necessary to develop a project with path finder
# (the AI of the ACTS system):
# g++ : need this dependency to compile path_finder
# libboost-all-dev : the boost library, need this dependency
# to compile path_finder
# jsoncpp : a json parser, need this dependency to compile path_finder
################################################################################
# @see base_ada
FROM m0bius/city-base0:latest

RUN apt-get update && apt-get upgrade -y \
&& apt-get install -y	g++ \
						libboost-all-dev \
						libjsoncpp-dev