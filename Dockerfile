FROM ubuntu:20.04
RUN mkdir -p /opt/bin/
RUN mkdir -p /etc/hive-sf-bridge/
RUN apt update && apt install -y curl
COPY dist-newstyle/build/x86_64-linux/ghc-8.10.4/hive-sf-bridge-0.1.0.0/x/hive-sf-bridge/build/hive-sf-bridge/hive-sf-bridge /opt/bin
COPY deploy/customers.json /etc/hive-sf-bridge
COPY deploy/notifications.json /etc/hive-sf-bridge
VOLUME /job
CMD ["/opt/bin/hive-sf-bridge","--customers","/etc/hive-sf-bridge/customers.json","--notifications","/etc/hive-sf-bridge/notifications.json","/job"]
