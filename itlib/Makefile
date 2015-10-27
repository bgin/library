#  @(#)Makefile	5.1 11/6/94
#
TOP_DIR = $(LIBRARY)/itlib
include $(TOP_DIR)/Makefile.inc
IT_SYS		=	$(TOP_DIR)/IT_SYS
SHARED		=	$(TOP_DIR)/Shared
DIRECTORIES =	$(IT_SYS) $(SHARED)
MAKE	= make
all: $(DIRECTORIES)

.RECURSIVE: $(DIRECTORIES)

$(DIRECTORIES): FORCE
	cd $@ ; $(MAKE) $(MFLAGS)

make: FORCE
	cd $(IT_SYS) ; $(MAKE) $(MFLAGS) make
	cd $(SHARED) ; $(MAKE) $(MFLAGS) make

clean: FORCE
	cd $(IT_SYS) ; $(MAKE) $(MFLAGS) clean
	cd $(SHARED) ; $(MAKE) $(MFLAGS) clean
	rm -fR *.a
FORCE:

