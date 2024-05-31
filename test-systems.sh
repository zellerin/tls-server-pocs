#!/bin/bash

for system in tls-server/core tls-server/synchronous tls-server/async tls-server/async-openssl tls-server tls-server/test tls-server/clip tls-server/async-openssl/full-http
do
    echo -n "$system: "
    if sbcl --eval "(ql:quickload :$system)" --disable-debugger --quit > $(basename $system).log 2>&1
    then
        echo "OK"
    else
        echo Failed, see log
    fi
done
