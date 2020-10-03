#!/bin/bash

./tony < $1
llc a.ll -o a.s
clang -g -o a.out a.s lib.a
