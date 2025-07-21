# Ananta
Ananta is a next-generation key-value store. It is built on Erlang actors, enabling massive scalability and fault tolerance.
    
Key features include:  
  Clustering  
  Duplication with quorum logic  
  Write-Ahead Logging (WAL)  
  Snapshots  
  Size-based Least Recently Used (LRU) caching  

## Overview


## Introduction

A fundamental question arises: why develop another Key-Value Store when numerous options already exist? The primary impetus behind its creation was to transparently communicate the underlying costs and benefits of the software to end consumers. This empowers users to leverage the Key-Value Store with maximum efficiency and effectiveness, drawing significant advantages from Erlang/OTP.

## Vision

The high-level plan encompasses several key objectives:  
Horizontal Scalability: Implement an easy and efficient method for horizontal scaling.  
Cost-Effective Distributed KV Store: Utilize a cheap yet powerful distributed Key-Value store.  
Large Volume Data Storage: Store a significant volume of data at a reduced cost.  
Enhanced Real-time Monitoring: Provide more comprehensive real-time monitoring capabilities.  

## Quick Tour

Write:  
curl -v -X PUT http://localhost:8001/kv/mykey      -H "Content-Type: application/json"      -d '{"value": "myvalue"}'  
Read:  
curl -X GET http://localhost:8001/kv/mykey  
Delete:  
curl -X DELETE http://localhost:8001/kv/mykey  

## Latency   
Running 30s test @ http://localhost:8001/kv/mykey  
  4 threads and 100000 connections  
  Thread Stats   Avg      Stdev     Max   +/- Stdev  
  &nbsp;&nbsp;Latency     6.27ms    8.43ms   1.62s    99.23%  
  &nbsp;&nbsp;Req/Sec    36.95k    11.64k   68.75k    65.02%  
  Latency Distribution  
  &nbsp;&nbsp;50%    5.81ms  
  &nbsp;&nbsp;75%    7.64ms  
  &nbsp;&nbsp;90%    9.57ms  
  &nbsp;&nbsp;99%   14.14ms  
  4407404 requests in 30.10s, 575.91MB read  
  Requests/sec: 146410.59  
  Transfer/sec:     19.13MB  
  

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

