<a id="x-28MINI-HTTP2-3A-40HTTP2-SERVER-POCS-20MGL-PAX-3ASECTION-29"></a>

# Experiments with HTTP/2 server

## Table of Contents

- [1 HTTP/2 protocol, very simplified and incorrect][b208]
- [2 Serving HTTP/2][5e42]
- [3 Nonblocking server][7d12]
- [4 Interface to poll(2)][be4c]

###### \[in package MINI-HTTP2\]
I wanted to play with different options for HTTP/2 server implementations. While
I have a more correct implementation of HTTP/2, I wanted something
simple to test different client handling options, as well as speed limits and impact of different choices.

So this repository implements:

- very simplified (and indeed incorrect in few ways) but server side of HTTP/2 protocol, and based of that

- several versions of TCP server that accept and handle the request.


<a id="x-28MINI-HTTP2-3A-40HTTP2-PROTOCOL-20MGL-PAX-3ASECTION-29"></a>

## 1 HTTP/2 protocol, very simplified and incorrect

Simplified HTTP/2 protocol overview is as follows:

<a id="x-28MINI-HTTP2-3A-2BCLIENT-PREFACE-START-2B-20VARIABLE-29"></a>

- [variable] **+CLIENT-PREFACE-START+** *#(80 82 73 32 42 32 72 84 84 80 47 50 46 48 13 10 13 10 83 77 13 10 13 10)*

    Clients send 24 octets of `+CLIENT-PREFACE-START+`, which in hex
    notation is this. That is, the connection preface starts with the string "PRI \*
     HTTP/2.0\r\n\r\nSM\r\n\r\n".

<a id="x-28MINI-HTTP2-3A-2ASETTINGS-FRAME-2A-20VARIABLE-29"></a>

- [variable] **\*SETTINGS-FRAME\*** *#(0 0 0 4 0 0 0 0 0)*

    After client preface, both client and server send their settings frame
    (`*SETTINGS-FRAME*`). The frame here is empty settings frame.

<a id="x-28MINI-HTTP2-3A-2AACK-FRAME-2A-20VARIABLE-29"></a>

- [variable] **\*ACK-FRAME\*** *#(0 0 0 4 1 0 0 0 0)*

    Settings frame should be acknowledged by sending `*ACK-FRAME*` (type+flag)

<a id="x-28MINI-HTTP2-3ASTREAM-ID-20TYPE-29"></a>

- [type] **STREAM-ID**

    Client sends HTTP2 requests as a headers (and possibly data) frame, with last
    packet having END-OF-HEADERS flag
    particular stream. Each stream is a 23 bit integer.

<a id="x-28MINI-HTTP2-3A-2ADATA-FRAME-2A-20VARIABLE-29"></a>

- [variable] **\*DATA-FRAME\*** *#(0 0 12 0 1 0 0 0 1 72 101 108 108 111 32 87 111 114 108 100 10)*

    Server replies to header frame with header and data frames with same stream
    ID. The data frame here is a data frame with zero stream ID (to be patched
    before sending) and payload short ASCII text from *result-text*.

<a id="x-28MINI-HTTP2-3A-2BGOAWAY-FRAME-TYPE-2B-20VARIABLE-29"></a>

- [variable] **+GOAWAY-FRAME-TYPE+** *7*

    When client is done (or after an error) it sends goaway frame, and both client
    and server terminate the connection socket. This is kind of courtesy, and any
    side should be ready for the other side terminating connection without goaway
    frame.
    
    Server can send goaway frame as well, but our servers do not.

<a id="x-28MINI-HTTP2-3A-40SERVER-ACTIONS-20MGL-PAX-3ASECTION-29"></a>

## 2 Serving HTTP/2

<a id="x-28MINI-HTTP2-3ACREATE-SERVER-20FUNCTION-29"></a>

- [function] **CREATE-SERVER** *PORT TLS DISPATCH-METHOD &KEY (HOST "127.0.0.1") (ANNOUNCE-URL-CALLBACK (CONSTANTLY NIL))*

    Create a server on `HOST` and `PORT` that handles connections (possibly with `TLS`) using
    `DISPATCH-METHOD`.
    
    `ANNOUNCE-URL-CALLBACK` is called when server is set up and receives one
    parameter, URL server listens to. The idea is to be able to connect to server
    when `PORT` is 0, that is, random port.
    
    Calls [`DO-NEW-CONNECTION`][00c4] to handle the connections with restart [`KILL-SERVER`][a95f] available.

<a id="x-28MINI-HTTP2-3AKILL-SERVER-20RESTART-29"></a>

- [restart] **KILL-SERVER** *&OPTIONAL VALUE*

    When a http2 server is running, terminate it properly.
    
    Create-server returns `VALUE` in such case. 

<a id="x-28MINI-HTTP2-3ADO-NEW-CONNECTION-20GENERIC-FUNCTION-29"></a>

- [generic-function] **DO-NEW-CONNECTION** *LISTENING-SOCKET TLS DISPATCH-METHOD*

    Wait on new (possibly tls) connection to the `LISTENING-SOCKET` and start handling it
    using `DISPATCH-METHOD`.
    
    Note that when using HTTP/2 without `TLS`, most clients have to be instructed to
    use tls - e.g., --http2-prior-knowledge for curl.
    
    This would be extended for new dispatch methods.

<a id="x-28MINI-HTTP2-3ADO-NEW-CONNECTION-20-28METHOD-20NIL-20-28T-20T-20-28EQL-20-3ANONE-29-29-29-29"></a>

- [method] **DO-NEW-CONNECTION** *LISTENING-SOCKET TLS (DISPATCH-METHOD (EQL NONE))*

    Handle the connection while doing nothing else.
    
    This version (`DISPATCH-METHOD` being :none) gives up on trying to serve more
    clients: when it gets connection, it reads the requests and handles them as they
    arrive. When the client sends go-away frame, it closes connection and is ready
    to serve another client.
    
    Obviously, there is little overhead and this version is pretty fast.

<a id="x-28MINI-HTTP2-3ADO-NEW-CONNECTION-20-28METHOD-20NIL-20-28T-20T-20-28EQL-20-3ATHREAD-29-29-29-29"></a>

- [method] **DO-NEW-CONNECTION** *LISTENING-SOCKET TLS (DISPATCH-METHOD (EQL THREAD))*

    Handle the connection in a new thread.
    
    This version (`DISPATCH-METHOD` being :thread) has supposedly disadvantage when
    there are too many clients/threads (RAM for threads needed, .
    
    The speed for single client is comparable to the single-client version.

<a id="x-28MINI-HTTP2-3ADO-CONNECTION-20FUNCTION-29"></a>

- [function] **DO-CONNECTION** *STREAM*

    Process a HTTP2 connection naively: handle preface, and read frames till there
      is end of stream; write static response in that case.
    
    Terminate if either `SSL` error occurs, or go-away restart is invoked.

<a id="x-28MINI-HTTP2-3A-2ABUFFER-2A-20VARIABLE-29"></a>

- [variable] **\*BUFFER\*** *NIL*

    Preallocated buffer for reading from stream. This is initialized for each
    connection depending on the dispatch method.

<a id="x-28MINI-HTTP2-3AREAD-CLIENT-PREFACE-20FUNCTION-29"></a>

- [function] **READ-CLIENT-PREFACE** *STREAM*

<a id="x-28MINI-HTTP2-3ASEND-RESPONSE-20FUNCTION-29"></a>

- [function] **SEND-RESPONSE** *STREAM STREAM-ID*

    Write response to the request with `STREAM-ID`.

Each frame starts with a header that contains it size, type, and stream id.

<a id="x-28MINI-HTTP2-3AGET-FRAME-SIZE-20FUNCTION-29"></a>

- [function] **GET-FRAME-SIZE** *HEADER*

    Get frame size of a frame from frame header.

<a id="x-28MINI-HTTP2-3AGET-STREAM-ID-20FUNCTION-29"></a>

- [function] **GET-STREAM-ID** *HEADER*

<a id="x-28MINI-HTTP2-3AGET-FRAME-TYPE-20FUNCTION-29"></a>

- [function] **GET-FRAME-TYPE** *HEADER*

<a id="x-28MINI-HTTP2-3AGET-FRAME-FLAGS-20FUNCTION-29"></a>

- [function] **GET-FRAME-FLAGS** *HEADER*

<a id="x-28MINI-HTTP2-3AGET-STREAM-ID-IF-ENDS-20FUNCTION-29"></a>

- [function] **GET-STREAM-ID-IF-ENDS** *HEADER*

    Stream id when header closes the stream on client side.

<a id="x-28MINI-HTTP2-3ABUFFER-WITH-CHANGED-STREAM-20FUNCTION-29"></a>

- [function] **BUFFER-WITH-CHANGED-STREAM** *BUF STREAM-ID*

    Change stream id of a frame in `BUF` to `STREAM-ID`.

<a id="x-28TLS-SERVER-2FNONBLOCK-3A-40NONBLOCK-SERVER-20MGL-PAX-3ASECTION-29"></a>

## 3 Nonblocking server

###### \[in package TLS-SERVER/NONBLOCK\]
<a id="x-28TLS-SERVER-2FPOLL-DISPATCHER-3ACLIENT-DATA-20CLASS-29"></a>

- [class] **TLS-SERVER/POLL-DISPATCHER::CLIENT-DATA** *[STRUCTURE-OBJECT][2038]*

    All data needed for polling.
    
    POLLFD is allocated and filled array of POLLFD-SIZE pollfd objects to pass to poll.
    
    SOCKET-LIST is list of information about monitored sockets.
    
    AVAILABLE-LIST is list of unused indices in the POLLFD. It is used when adding
    new fd to monitor.

<a id="x-28TLS-SERVER-2FPOLL-DISPATCHER-3ACALL-WITH-POLLFDS-20FUNCTION-29"></a>

- [function] **CALL-WITH-POLLFDS** *ACTION N &REST ARGS*

    Call ACTION with a new `CLIENT-DATA` object and additional ARGS.
    
    Intention is to have and keep allocated pollfd array during the lifetime of the
    call to ACTION, and clean it up afterwards.

<a id="x-28TLS-SERVER-2FPOLL-DISPATCHER-3A-40POLL-DISPATCHER-20MGL-PAX-3ASECTION-29"></a>

## 4 Interface to poll(2)

###### \[in package TLS-SERVER/POLL-DISPATCHER\]
Interface to poll:
- allocate C fdinfo array and on stack with [`CALL-WITH-POLLFDS`][ac5b],
- keep data relevant to each client in [`FD-INFO`][87eb], possibly after extending it to suit needs of the application,
- Add new clients with [`ADD-FD`][b0a4]
- Wait for activity with [`WAIT-FOR-FD`][bf1b]
- Remove clients with [`REMOVE-FD`][fa1a],

<a id="x-28TLS-SERVER-2FPOLL-DISPATCHER-3ACALL-WITH-POLLFDS-20FUNCTION-29"></a>

- [function] **CALL-WITH-POLLFDS** *ACTION N &REST ARGS*

    Call `ACTION` with a new [`CLIENT-DATA`][ec8e] object and additional `ARGS`.
    
    Intention is to have and keep allocated pollfd array during the lifetime of the
    call to `ACTION`, and clean it up afterwards.

<a id="x-28TLS-SERVER-2FPOLL-DISPATCHER-3AFD-INFO-20CLASS-29"></a>

- [class] **FD-INFO** *[STRUCTURE-OBJECT][2038]*

    Information related to a client. This includes:
    - `READ-ACTION` and `WRITE-ACTION` that would be called when data for read are available,

<a id="x-28TLS-SERVER-2FPOLL-DISPATCHER-3AADD-FD-20FUNCTION-29"></a>

- [function] **ADD-FD** *CLIENT-DATA NEW-FD FD-INFO*

    Add client with `NEW-FD` to the `CLIENT-DATA`.
    
    Add FD to an empty slot in pollfd array, with `EVENTS` value based on direction slot
      in `FD-INFO`,
    
    Fix FD slot in `FD-INFO` to the `NEW-FD`.
    
    Update list of connected clients.

<a id="x-28TLS-SERVER-2FPOLL-DISPATCHER-3AWAIT-FOR-FD-20FUNCTION-29"></a>

- [function] **WAIT-FOR-FD** *CLIENT-DATA*

    Wait until any of monitored clients is available as required.
    
    Call appropriate callback for such connection.

<a id="x-28TLS-SERVER-2FPOLL-DISPATCHER-3AREMOVE-FD-20FUNCTION-29"></a>

- [function] **REMOVE-FD** *CLIENT-DATA FD-INFO*

    Remove monitored fd identified by `FD-INFO` from the `CLIENT-DATA`.
    
    Remove the appropriate pollfd record, and make necessary bookkeeping in
    `CLIENT-DATA`.

<a id="x-28TLS-SERVER-2FPOLL-DISPATCHER-3AFD-INFO-STREAM-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29"></a>

- [structure-accessor] **FD-INFO-STREAM**

For SBCL: `SB-IMPL::FD-STREAM` has `IBUF` slot that is `SB-IMPL::BUFFER` structure of size (\* 8 1024) that has buffered read data; slots HEAD and TAIL indicate how much space is inside.

So what should work when we get data is to read once, and then check HEAD and TAIL if we have enough. Of course, we might have a problem if we need more than what fits the buffer. `SB-IMPL::ALLOC-BUFFER` can be used to allocate bigger buffer.

  [00c4]: #x-28MINI-HTTP2-3ADO-NEW-CONNECTION-20GENERIC-FUNCTION-29 "MINI-HTTP2:DO-NEW-CONNECTION GENERIC-FUNCTION"
  [2038]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stu_ob.htm "STRUCTURE-OBJECT (MGL-PAX:CLHS CLASS)"
  [5e42]: #x-28MINI-HTTP2-3A-40SERVER-ACTIONS-20MGL-PAX-3ASECTION-29 "Serving HTTP/2"
  [7d12]: #x-28TLS-SERVER-2FNONBLOCK-3A-40NONBLOCK-SERVER-20MGL-PAX-3ASECTION-29 "Nonblocking server"
  [87eb]: #x-28TLS-SERVER-2FPOLL-DISPATCHER-3AFD-INFO-20CLASS-29 "TLS-SERVER/POLL-DISPATCHER:FD-INFO CLASS"
  [a95f]: #x-28MINI-HTTP2-3AKILL-SERVER-20RESTART-29 "MINI-HTTP2:KILL-SERVER RESTART"
  [ac5b]: #x-28TLS-SERVER-2FPOLL-DISPATCHER-3ACALL-WITH-POLLFDS-20FUNCTION-29 "TLS-SERVER/POLL-DISPATCHER:CALL-WITH-POLLFDS FUNCTION"
  [b0a4]: #x-28TLS-SERVER-2FPOLL-DISPATCHER-3AADD-FD-20FUNCTION-29 "TLS-SERVER/POLL-DISPATCHER:ADD-FD FUNCTION"
  [b208]: #x-28MINI-HTTP2-3A-40HTTP2-PROTOCOL-20MGL-PAX-3ASECTION-29 "HTTP/2 protocol, very simplified and incorrect"
  [be4c]: #x-28TLS-SERVER-2FPOLL-DISPATCHER-3A-40POLL-DISPATCHER-20MGL-PAX-3ASECTION-29 "Interface to poll(2)"
  [bf1b]: #x-28TLS-SERVER-2FPOLL-DISPATCHER-3AWAIT-FOR-FD-20FUNCTION-29 "TLS-SERVER/POLL-DISPATCHER:WAIT-FOR-FD FUNCTION"
  [ec8e]: #x-28TLS-SERVER-2FPOLL-DISPATCHER-3ACLIENT-DATA-20CLASS-29 "TLS-SERVER/POLL-DISPATCHER:CLIENT-DATA CLASS"
  [fa1a]: #x-28TLS-SERVER-2FPOLL-DISPATCHER-3AREMOVE-FD-20FUNCTION-29 "TLS-SERVER/POLL-DISPATCHER:REMOVE-FD FUNCTION"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
