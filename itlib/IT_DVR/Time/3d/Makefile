#   @(#)Makefile	1.9   8/7/91
#
#  Makefile for the dvr iterative routines
#
include $(MESA_HOME)/Makefile.inc
ITDVRLIB = $(LIBRARY)/itlib/itdvrlib.a

GET = sccs get
REL = 

SRCS = \
	h3tonv.f \
	cham3.f \
	setnd4.f \
	setd4.f \
	v3t2dvr.f \
	v3t2h0.f \
	v3te.f 

all: $(ITDVRLIB)
$(ITDVRLIB): $(SRCS:.f=.o)
	$(AR) $(ARFLAGS) $(ITDVRLIB) $(?:.f=.o)



