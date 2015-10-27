#   @(#)Makefile	1.9   8/7/91
#
#  Makefile for the dvr iterative routines
#
include $(MESA_HOME)/Makefile.inc
ITDVRLIB = $(LIBRARY)/itlib/itdvrlib.a

GET = 
REL = 

SRCS = \
	h3v.f \
	h3onv.f \
	h32h0.f \
	h3e.f \
	h32dvr.f \
	rham3.f \
	v32dvr.f \
	v32h0.f \
	v3e.f 

all: $(ITDVRLIB)
$(ITDVRLIB): $(SRCS:.f=.o)
	$(AR) $(ARFLAGS) $(ITDVRLIB) $(?:.f=.o)



