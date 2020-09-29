#!/bin/bash

./tony < $1
llc a.ll -o a.s
clang -o a.out a.s lib.a
