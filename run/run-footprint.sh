#!/bin/bash 

whichpony=$1 # release or release-telemetry or release-nogc
benchmark=$2
input=$3

cd benchmarks/$benchmark/pony 
rm -f pony

/home/vagrant/ponyc/build/$whichpony/ponyc --pic

logfile="orca.footprint.$whichpony.log"
outfile="orca.footprint.$whichpony.txt"
rm -f $logfile
rm -f $outfile

repetition=1
i=1
while [ $i -le $repetition ]
do
  echo "Iteration "$i >> $logfile
  echo "Iteration "$i >> $outfile
  /usr/bin/time -f "%M" -o "tmp" ./pony $input --ponynoblock >> $outfile
  cat tmp >> $logfile
  i=$[$i+1]
done
rm tmp
