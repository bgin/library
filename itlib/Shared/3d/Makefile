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
	h32dvr.f \
	h3e.f \
	h32h0.f 

OBJECTS = \
	$(SRCS:.f=.o)	

all: $(BLIULIB)
$(BLIULIB): $(OBJECTS)
	$(AR) $(ARFLAGS) $(BLIULIB) $(OBJECTS)
	$(RANLIB) $(BLIULIB) 
	$(AR) $(ARFLAGS) $(ITDVRLIB) $(OBJECTS)
	$(RANLIB) $(ITDVRLIB) 
