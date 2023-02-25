FROM ubuntu:20.04

# Change the default shell to Bash
SHELL [ "/bin/bash" , "-c" ]

ENV WORKDIR=/usr/app/src \
    TZ=America \
    DEBIAN_FRONTEND=noninteractive

RUN mkdir /usr/deps
WORKDIR /usr/app/src

# Instalations
RUN apt-get update && apt-get install -y \
    git \
    wget \
    make \
    tzdata \
    nano 

RUN apt-get update && apt-get install -y leiningen