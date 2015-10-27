#   @(#)Makefile	1.9   8/7/91
#
#  Makefile for the dvr iterative routines
#
include $(MESA_HOME)/Makefile.inc
ITDVRLIB = $(LIBRARY)/itlib/itdvrlib.a
GET = 
REL = 

SRCS = \
	h2v.f \
	h2onv.f \
	h22h0.f \
	h2e.f \
	h22dvr.f \
	rham2.f \
	v22dvr.f \
	v22h0.f \
	v2e.f

all: $(ITDVRLIB)
$(ITDVRLIB): $(SRCS:.f=.o)
	$(AR) $(ARFLAGS) $(ITDVRLIB) $(?:.f=.o)



