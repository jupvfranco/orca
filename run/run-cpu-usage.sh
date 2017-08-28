#!/bin/bash 

benchmark=$1
input=$2
corecount=$3

cd benchmarks/$benchmark/pony 
rm -f pony

/home/vagrant/ponyc/build/release-telemetry/ponyc --pic

logfile="orca.cpuusage.release-telemetry.log"
rm $logfile

# no repetition
./pony $input --ponythreads=$corecount \
  | grep "gc_intervals:\|send_scan_intervals:\|rcv_scan_intervals:\|cpu cycles\|behaviors_intervals:" > $logfile
# these results must be used for gc times and cpu usage
