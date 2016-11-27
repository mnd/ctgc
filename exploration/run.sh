#!/bin/bash

TRAILSZ=$((96*1024)) LOCALSZ=$((32*1024)) GLOBALSZ=$((800 * 1024)) ./eval | \
  awk 'BEGIN {print "all\tstack\tqueue"}
       {PS=$1; getline; split($0, SQ, /,|\(|\)/); print PS "\t" SQ[2] "\t" SQ[3]; SUM+=PS; SUMS+=SQ[2]; SUMQ+=SQ[3]}
       END {print "\nsum:"; print SUM "\t" SUMS "\t" SUMQ}'
