#  @(#)Makefile	5.1 11/6/94
#
# Makefile for making the propagator codes
# This assumes the following directory system as well as makefiles
#
# You should set your environment variables for the fortran compiler
# and the various flags and libraries needed.

include $(MODULES)/Makefile.inc
.SUFFIXES: .f90 .F90 $(SUFFIXES)
#

all: $(ALL_MODULES)

.RECURSIVE: $(ALL_MODULES)

$(ALL_MODULES): FORCE 
	cd $@ ; $(MAKE) $(MFLAGS)

make: FORCE
	cd $(GENERAL_MODULES) ; $(MAKE) $(MFLAGS) 
	cd $(MATHEMATICS_MODULES) ; $(MAKE) $(MFLAGS) 


clean: FORCE
	cd $(GENERAL_MODULES) ; $(MAKE) $(MFLAGS) clean
	cd $(MATHEMATICS_MODULES) ; $(MAKE) $(MFLAGS) clean
	rm -f *.a *~
FORCE:

