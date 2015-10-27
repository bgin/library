#   @(#)Makefile	1.9   8/7/91
#
#  Makefile for the dvr iterative routines
#
include $(MESA_HOME)/Makefile.inc
ITDVRLIB = $(LIBRARY)/itlib/itdvrlib.a

GET = 
REL = 

SRCS = \
	cham1.f \
	h1tonv.f \
	setnd2.f \
	setd2.f \
	v1t2dvr.f \
	v1t2h0.f \
	v1te.f 

all: $(ITDVRLIB)
$(ITDVRLIB): $(SRCS:.f=.o)
	$(AR) $(ARFLAGS) $(ITDVRLIB) $(?:.f=.o)



