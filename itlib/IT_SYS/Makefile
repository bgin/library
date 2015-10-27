#  @(#)Makefile	5.1 11/6/94
#
TOP_DIR = $(LIBRARY)/itlib
include $(MESA_HOME)/Makefile.inc
include $(TOP_DIR)/Makefile.inc

CPPSRCS = \
        bsmall.F \
        dvddat.F \
        honv.F \
        lares.F \
        lavec.F \
        lindvd.F \
        lsolve.F \
        rsdvd.F

all: $(BLIULIB)
$(BLIULIB): $(CPPSRCS:.F=.o)
	$(AR) $(ARFLAGS) $(BLIULIB) $(CPPSRCS:.F=.o)
	$(RANLIB) $(BLIULIB)

sources: $(CPPSRCS)
	co -l RCS/*.F
