#  @(#)Makefile	5.1 11/6/94
#
# Makefile for making all of the library routines and modules used in the codes
# Enviroment variables need to be set properly as the makefiles have dependencies on their
# definition.
#
#
#
#
#                The main directories
#
MESALIB			=	$(LIBRARY)/Mesalib
UTILITIES		=	$(LIBRARY)/Utilities
MODULES			=	$(LIBRARY)/Modules
POTENTIAL		=	$(LIBRARY)/Potential
ITLIB			=	$(LIBRARY)/itlib
#
LIBRARY_DIRECTORIES	=	$(MESALIB) $(UTILITIES) $(MODULES) $(POTENTIAL) $(ITLIB)
#
MAKE	=	make
#
all: $(LIBRARY_DIRECTORIES)

.RECURSIVE: $(LIBRARY_DIRECTORIES) 
$(LIBRARY_DIRECTORIES): FORCE
	cd $@ ; $(MAKE) $(MFLAGS)

make: FORCE
	cd $(MESALIB) ; $(MAKE) $(MFLAGS) make
	cd $(UTILITIES) ; $(MAKE) $(MFLAGS) make
	cd $(MODULES) ; $(MAKE) $(MFLAGS) make
	cd $(POTENTIAL) ; $(MAKE) $(MFLAGS) make
	cd $(ITLIB) ; $(MAKE) $(MFLAGS) make

clean: FORCE
	cd $(MESALIB) ; $(MAKE) $(MFLAGS) clean
	cd $(UTILITIES) ; $(MAKE) $(MFLAGS) clean
	cd $(MODULES) ; $(MAKE) $(MFLAGS) clean
	cd $(POTENTIAL) ; $(MAKE) $(MFLAGS) clean
	cd $(ITLIB) ; $(MAKE) $(MFLAGS) clean

clean_rep: FORCE
	rm -fR RCS* svn* .svn*
	cd $(MESALIB) ; rm -fR RCS* svn* .svn* ; $(MAKE) $(MFLAGS) clean_rep
	cd $(UTILITIES) ; rm -fR RCS* svn* .svn* ; $(MAKE) $(MFLAGS) clean_rep
	cd $(MODULES) ; rm -fR RCS* svn* .svn* ; $(MAKE) $(MFLAGS) clean_rep
	cd $(POTENTIAL) ; rm -fR RCS* svn* .svn* ; $(MAKE) $(MFLAGS) clean_rep
	cd $(ITLIB) ; rm -fR RCS* svn* .svn* ; $(MAKE) $(MFLAGS) clean_rep

FORCE:

