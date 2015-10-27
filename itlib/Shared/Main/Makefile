#   @(#)Makefile	1.9   8/7/91
#
#  Makefile for the davidson eigenvalue and linear system routines
#
TOP_DIR = $(LIBRARY)/itlib
include $(MESA_HOME)/Makefile.inc
include $(TOP_DIR)/Makefile.inc

CPPSRCS = \
	diagnl.F \
	frmres.F \
	genvec.F \
	hsmall.F \
	init.F \
	invec.F \
	outvec.F \
	ntrial.F \
	preig.F \
	rdciv.F \
	rdiag.F \
	trilin.F \
	tstovl.F

OBJECTS = \
	$(CPPSRCS:.F=.o)	

all: $(BLIULIB)
$(BLIULIB): $(CPPSRCS:.F=.o)
	$(AR) $(ARFLAGS) $(BLIULIB) $(OBJECTS)
	$(RANLIB) $(BLIULIB)
	$(AR) $(ARFLAGS) $(ITDVRLIB) $(OBJECTS)
	$(RANLIB) $(ITDVRLIB)

sources: $(CPPSRCS)
	co -l RCS/*.F
