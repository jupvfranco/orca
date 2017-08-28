#!/bin/bash 

benchmark=$1
input=$2

cd benchmarks/$benchmark/pony 
rm -f pony

/home/vagrant/ponyc/build/release-telemetry/ponyc --pic

logfile="orca.overhead.release-telemetry.log"
rm -f $logfile

repetition=1
i=1
while [ $i -le $repetition ]
do
  echo "Iteration "$i >> $logfile
  ./pony $input --ponynoblock --ponythreads=1 > tmp

  x=$(cat tmp | grep "count_gc_passes")
  echo "Number of GC cycles: "$x >> $logfile

  x=$(cat tmp | grep "count_alloc_actors")
  echo "Number of actors: "$x >> $logfile

  x=$(cat tmp | grep "count_msg_app")
  echo "Number of application messages: "$x >> $logfile

  x=$(cat tmp | grep "count_msg_acquire")
  echo "Number of INC messages: "$x >> $logfile

  x=$(cat tmp | grep "count_msg_release")
  echo "Number of DEC messages: "$x >> $logfile

  x=$(cat tmp | grep "time_in_behaviour")
  echo "CPU cycles @ behaviour: "$x >> $logfile

  x=$(cat tmp | grep "time_in_gc")
  echo "CPU cycles @ GC: "$x >> $logfile

  x=$(cat tmp | grep "time_in_send_scan")
  echo "CPU cycles @ sending trace: "$x >> $logfile

  x=$(cat tmp | grep "time_in_recv_scan")
  echo "CPU cycles @ receiving trace: "$x >> $logfile

  x=$(cat tmp | grep "execution used")
  echo $x >> $logfile

  rm tmp
  i=$[$i+1]
done
