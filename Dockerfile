# Use Ubuntu 15.04
FROM ubuntu:15.04

# Install packages
RUN \
  apt-get update && \
  apt-get -y upgrade && \
  apt-get -y install clang clang-3.6 llvm-3.6 openjdk-7-jre-headless cmake ninja-build curl

# Install sbt
RUN \
  cd /opt && \
  curl -L https://dl.bintray.com/sbt/native-packages/sbt/0.13.8/sbt-0.13.8.tgz | tar -zxv

# Alias llc and opt to their 3.5 versions
RUN \
  mkdir /opt/bin && \
  ln -s /usr/bin/llc-3.6 /opt/bin/llc && \
  ln -s /usr/bin/opt-3.6 /opt/bin/opt

ENV PATH $PATH:/opt/sbt/bin:/opt/bin

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
