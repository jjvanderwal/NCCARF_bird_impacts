#!/bin/bash

BASEDIR=/home/jc148322/scripts/NARP_birds/birds_tmp_sh/
cd $BASEDIR
pwd

for SPP in `find . -type f -name '*.sh'`
do
	echo $SPP
	numjobs=$(( $(qstat -u jc148322 | grep ' Q ' | wc -l) + $(qstat -u jc148322 | grep ' R ' | wc -l) )) 
	while [ $numjobs -gt 20 ] 
	do 
		sleep 60
		numjobs=$(( $(qstat -u jc48322 | grep ' Q ' | wc -l) + $(qstat -u jc148322 | grep ' R ' | wc -l) ))
	done
	qsub -m n -l nodes=1:ppn=1 -l pmem=3gb $SPP 
done
