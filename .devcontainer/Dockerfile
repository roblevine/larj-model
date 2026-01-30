FROM mcr.microsoft.com/devcontainers/base:ubuntu

# https://www.swi-prolog.org/build/PPA.html
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get install -y --no-install-recommends software-properties-common \
    && apt-add-repository ppa:swi-prolog/devel \
    && apt-get update \
    && apt-get install -y --no-install-recommends swi-prolog