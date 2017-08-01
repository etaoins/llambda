# Use Ubuntu 17.04
FROM ubuntu:17.04

# Install packages
RUN \
  apt-get update && \
  apt-get -y upgrade && \
  apt-get -y install clang llvm openjdk-8-jre-headless cmake ninja-build curl && \
  apt-get clean

# Install sbt
RUN \
  cd /opt && \
  curl -L https://cocl.us/sbt01316tgz | tar -zx

ENV PATH $PATH:/opt/sbt/bin

# Copy Llambda in to the image
ADD . /root/llambda

# Build the runtime
RUN \
  cd /root/llambda && \
  mkdir build && \
  cd build && \
  cmake -GNinja ../runtime && \
  ninja

# Pre-build the compiler
RUN cd /root/llambda && sbt compile

WORKDIR /root/llambda
CMD ["bash"]
