#!/usr/bin/env bash

OPTS="-l 100 -t 1000"

runtest() {
  echo "Method: $1"
  time strace -c ./writeperf $OPTS $1 > test
  md5sum test
  rm test
  echo ""
}

runtest write
runtest fwrite
runtest fwrite-line
runtest fwrite-full
runtest writev
