FROM ubuntu:16.04 as builder

RUN apt-get update && \ 
    apt-get install -y libmosquitto-dev curl && \
    curl -sSL https://get.haskellstack.org/ | sh
ADD . /usr/src/traze/
RUN cd /usr/src/traze && \
    stack test && \
    stack build --copy-bins

FROM ubuntu:16.04
COPY --from=builder /usr/lib/x86_64-linux-gnu/libmosquitto.so /usr/lib/x86_64-linux-gnu/libmosquitto.so
COPY --from=builder /usr/include/mosquitto.h /usr/include/mosquitto.h
COPY --from=builder /usr/lib/x86_64-linux-gnu/libmosquitto.so.1 /usr/lib/x86_64-linux-gnu/libmosquitto.so.1
COPY --from=builder /usr/lib/x86_64-linux-gnu/libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so.10
COPY --from=builder /lib/x86_64-linux-gnu/librt.so.1 /lib/x86_64-linux-gnu/librt.so.1
COPY --from=builder /lib/x86_64-linux-gnu/libdl.so.2 /lib/x86_64-linux-gnu/libdl.so.2 
COPY --from=builder /lib/x86_64-linux-gnu/libssl.so.1.0.0 /lib/x86_64-linux-gnu/libssl.so.1.0.0 
COPY --from=builder /lib/x86_64-linux-gnu/libcrypto.so.1.0.0 /lib/x86_64-linux-gnu/libcrypto.so.1.0.0
COPY --from=builder /usr/lib/x86_64-linux-gnu/libcares.so.2 /usr/lib/x86_64-linux-gnu/libcares.so.2 

COPY --from=builder /usr/src/traze/traze.yml /
COPY --from=builder /root/.local/bin/traze-exe /usr/bin/traze

ENTRYPOINT ["traze"]

