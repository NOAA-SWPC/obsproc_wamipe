#!/bin/bash
#
# Script name:   bufr_latloncq.sh   Driver for bufr_latlonqc program
#
# RFC Contact:   JWhiting           org: EMC         date: 2019-06-13
#
# Abstract: This script sets up and runs program bufr_latlonqc which checks 
#   reports within an input BUFR file for missing position (latitude &
#   longitude) content and generates a "clean" copy of the file w/ these
#   deficient reports removed.
# 
# History log:
#   2019-06-13  JWhiting  Original version for implementation.
#
# Usage:
#   bufr_latlonqc.sh  <bufrfile>
#
#
# Parameters:
#
#   $1: <bufrfile> - full path to BUFR data file to be checked for content.
#
# Input files:
#   <bufrfile> as script argument
#
# Output files: 
#   ./<bufrfile>.2 : copy of input <bufrfile> w/ bad reports removed
#
#
# Condition codes:
#
#    0: proper run, no errors
#   88: required environment variable(s) missing
#   99: missing or invalid <bufrfile> argument;
# 
#   rc=88 or 99 results in script exiting w/o attempting to run $LLQCX
#
#
# User controllable options:
#
#   Imported Variables that must exist:
#     one of these must be provided (script exits otherwise)
#
#     EXECobsproc_global : specifies directory path for bufr_latlonqc executable
#                  LLQCX : specifies path to bufr_latlonqc executable
#
# Attributes:
#
#   Language: bash script
#   Machine:  NCEP WCOSS
#
####
# Check for one required argument
# -------------------------------
usage='
 bufr_latlonqc.sh 

  Usage:  bufr_latlonqc.sh  <dumpfile>

  *** ERROR *** Required argument missing - exiting
'
[ $# -eq 1 ] || { echo "$usage" ; exit 99 ; }

#set -xa
set -e

rc=0

dfile=$1

echo 
echo "blq: starting bufr_latlonqc.sh ($(date))"
echo "blq:   running lat/lon check on ..."
echo "blq:     dfile='$dfile'"
echo 

# check for valid input file
# --------------------------
here=$(pwd)
[ ."`echo $dfile | cut -c1`" != ./ ] && dfile="${here}/$dfile"
[ -f $dfile ] || { echo 'blq: dfile not found - exiting' ; exit 99 ; }
[ -s $dfile ] || { echo 'blq: dfile found empty - exiting' ; exit 99 ; }
#echo "blq:   dfile=`ls -l $dfile | awk '{print $NF}'`"


# Check for required environment vars
# ---------------------------------
[ -z "$EXECobsproc_global" -a -z "$EMCobsproc_global" -a -z "$LLQCX" ] && \
  { echo "blq:   ERROR - no path to EXECobsproc_global nor LLQCX exec - exiting" ; exit 88 ; }

EXECobsproc_global=${EXECobsproc_global:-$EMCobsproc_global/exec}
LLQCX=${LLQCX:-$EXECobsproc_global/bufr_latlonqc}


# run the exec
# ------------
unset FORT000 `env | grep "FORT[0-9]\{1,\}" | awk -F= '{print $1}'`
export FORT50=$dfile
ofile=$(basename ${dfile}).2
export FORT51=$ofile

olog=bllqc.out.$(basename $dfile)
echo "blq: running exec '$LLQCX' ($(date))"
echo "blq:   output (filtered) file: $ofile"

$LLQCX $dfile $cymd $hm |& tee $olog
rc=$?

echo "blq: run complete, rc='$rc'"
ls -l $ofile

/bin/rm -f FORT50 FORT51

echo "blq: normal end of script"
exit $rc
