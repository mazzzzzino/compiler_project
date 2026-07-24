# base image on top of which others are layered
FROM ubuntu:24.04

# "don't ask questions just usedefaults"
# no interactive terminal 
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    build-essential \
    gcc \
    gcc-multilib \
    g++ \
    make \
    nasm \
    libc6-dev-i386 \
    guile-3.0 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

CMD ["/bin/bash"]

