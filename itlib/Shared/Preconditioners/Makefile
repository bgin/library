#   @(#)Makefile	1.9   8/7/91
#
#  Makefile for the preconditioners library
#
TOP_DIR = $(LIBRARY)/itlib
include $(MESA_HOME)/Makefile.inc
include $(TOP_DIR)/Makefile.inc
GET = sccs get
REL = 

SRCS = \
	blkout.f \
	blkpre.f \
	diddle.f \
	dslve.f \
	dcslve.f \
	escale.f \
	h0prec.f \
	h0cprec.f \
	rbldag.f \
	rblslv.f \
	rblvec.f \
	rfctrs.f \
	rsublu.f \
	seppre.f
OBJECTS = \
	$(SRCS:.f=.o)

all: $(BLIULIB)
$(BLIULIB): $(OBJECTS)
	$(AR) $(ARFLAGS) $(BLIULIB) $(OBJECTS)
	$(RANLIB) $(BLIULIB) 
	$(AR) $(ARFLAGS) $(ITDVRLIB) $(OBJECTS)
	$(RANLIB) $(ITDVRLIB) 
