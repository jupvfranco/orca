#!/bin/bash 

whichpony=$1 # release or release-telemetry or release-nogc
benchmark=$2
input=$3

cd benchmarks/$benchmark/pony 
rm -f pony

/home/vagrant/ponyc/build/$whichpony/ponyc --pic

logfile="orca.scalability.$whichpony.log"
outfile="orca.scalability.$whichpony.txt"
rm -f $logfile
rm -f $outfile

repetition=1
corecount=1
maxcorecount=1
while [ $corecount -le $maxcorecount ]
do 
  i=1
  while [ $i -le $repetition ]
  do
    echo $corecount" cores :: iteration "$i >> $outfile
    /usr/bin/time -f "%e" -o "tmp" ./pony $input --ponythreads=$corecount --ponynoblock >> $outfile
    t=$(cat tmp) >> $logfile
    echo $corecount" cores :: iteration "$i" :: "$t >> $logfile
    i=$[$i+1]
  done
  corecount=$[$corecount + $corecount]
done
rm tmp

