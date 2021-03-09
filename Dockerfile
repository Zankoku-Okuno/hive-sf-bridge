FROM ubuntu:20.04
RUN apt-get update \
    && apt-get install -y software-properties-common \
    && add-apt-repository ppa:hvr/ghc \
    && apt-get update && apt-get install -y ghc-8.10.4 cabal-install-3.2 \
    && apt-get remove -y software-properties-common
ENV PATH="${PATH}:/opt/ghc/bin"
RUN cabal update

# build the application
RUN apt-get update && apt-get install -y zlib1g-dev
RUN mkdir -p /root/hive-sf-bridge
WORKDIR /root/hive-sf-bridge
COPY cabal.project CHANGELOG.md hive-sf-bridge.cabal LICENSE README.md Setup.hs \
    /root/hive-sf-bridge/
RUN cabal build --only-dependencies
RUN mkdir app src
COPY src src/
COPY app app/
RUN cabal build
WORKDIR /

# install the application
RUN mkdir -p /opt/bin/
RUN mkdir -p /etc/hive-sf-bridge/
RUN apt-get update && apt-get install -y curl zlib1g
RUN cp /root/hive-sf-bridge/dist-newstyle/build/x86_64-linux/ghc-8.10.4/hive-sf-bridge-0.1.0.0/x/hive-sf-bridge/build/hive-sf-bridge/hive-sf-bridge /opt/bin

# minimize the image...?
RUN apt-get remove -y --autoremove \
      ghc-8.10.4 cabal-install-3.2 \
      zlib1g-dev
RUN rm -r /root/hive-sf-bridge

# Prepare to Run
RUN mkdir -p /job
CMD ["/opt/bin/hive-sf-bridge","--customers","/etc/hive-sf-bridge/customers.json"]
