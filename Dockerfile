FROM ubuntu:trusty

RUN apt-get update && apt-get install -y --no-install-recommends build-essential make golang &&\
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ADD . /usr/src/olegdb

RUN cd /usr/src/olegdb && make && make install &&\
    mkdir /oleg

ADD olegdb.conf.sample /oleg/db.conf

EXPOSE 38080

WORKDIR /oleg
CMD /usr/local/bin/olegdb -config /oleg/db.conf
