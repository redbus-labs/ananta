# Ananta
Ananta is nextGen KV store built in based on Erlang actors for builing massively scalable and fault tolernat. 
It has built with various features like clusters, duplication with quorum logic, WAL, Snapshots, LRU based on size etc. 

## Overview


## Introduction

Why build another KV Store when there are so many already?
It's the only question isn't it? Well just say that the original motivation
of the creator was to make the end consumers aware of the costs and benefits
running beneath the software. This will allow the users of this Key-Value Store
efficiently and effectively use it to their benefit.

Try to utilize maxmimum benifits from Erlang/OTP.

## Vision

The high level plan is as follows:

* Easy way to scale horizontally  
* Cheap and powerful distributed KV Store
* Store large volume of data with cheaper price
* A lot more realtime monitoring

## Quick Tour

Write:
curl -v -X PUT http://localhost:8001/kv/mykey      -H "Content-Type: application/json"      -d '{"value": "myvalue"}'

Read:
curl -X GET http://localhost:8002/kv/mykey


Delete:

curl -X DELETE http://localhost:8001/kv/mykey


## Building and Running Ananta


## Building on Ubuntu

> Download and install Erlang from
> https://www.erlang-solutions.com/resources/download.html
 
    sudo apt-get install build-essential
    sudo apt-get install libssl-dev

## Building on Microsoft Windows

> Download and install Erlang from
> https://www.erlang-solutions.com/resources/download.html

## Build

### Clean 

    ./rebar3.1 clean ananta

### Clean all 

    ./rebar3.1 clean -a

### Release build

    ./rebar3.1 release

### Prod build

    ./rebar3.1 as prod tar
    
## Running app in shell mode

    ./rebar3.1 shell --apps ananta

## Running shell will all the code

    erl -pa _build/default/lib/*/ebin
    
## Testing and Creating Code Coverage

**TODO**
    
### EUnit Testing

**TODO**

## Generating Code Documentation

The code documentation is generated via [edoc](http://erlang.org/doc/apps/edoc/chapter.html) as follows:

    ./rebar3 edoc

The output is generated in doc/ subfolder.

## observer ananta erlang node

> cookiename is mentioned in vm.args

    erl -name ananta@127.0.0.1 -setcookie 'SomeCookie' -run observer

## Code Style

The code style must be validated by [elvis](https://github.com/inaka/elvis),
which does a good job at style reviewing. The repository will be setup (in the
future) such that each of the commit must be automatically reviewed by
elvis before it can be submitted.

### To run elvis for project

    ./elvis rock

### To run elvis for single file

    ./elvis rock <filename>

## References





