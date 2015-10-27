#   @(#)Makefile	1.9   8/7/91
#
#  Makefile for the dvr iterative routines
#
TOP_DIR = $(LIBRARY)/itlib
include $(MESA_HOME)/Makefile.inc
include $(TOP_DIR)/Makefile.inc

GET = sccs get
REL = 

SRCS = \
	h22dvr.f \
	h2e.f \
	h22h0.f 

OBJECTS = \
	$(SRCS:.f=.o)	

all: $(BLIULIB)
$(BLIULIB): $(OBJECTS)
	$(AR) $(ARFLAGS) $(BLIULIB) $(OBJECTS)
	$(RANLIB) $(BLIULIB)
	$(AR) $(ARFLAGS) $(ITDVRLIB) $(OBJECTS)
	$(RANLIB) $(ITDVRLIB)
