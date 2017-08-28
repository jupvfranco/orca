#!/bin/bash 

whichpony=$1 # release or release-telemetry or release-nogc
input=$2

cd benchmarks/serverSimulation/pony 
rm -f pony

/home/vagrant/ponyc/build/$whichpony/ponyc --pic

logfile="orca.responsivness.$whichpony.log"
rm -f $logfile

./pony $input --ponynoblock | python /run/massage.py $input >> $logfile
