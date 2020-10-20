#!/bin/bash
# Run this script from the command line after the mif-array.sh has run on sapelo2
# first cd to top level project directory, then:
# >bash cleandir.sh
rm covstates*
rm -r *.sapelo2
rm *.Rout
