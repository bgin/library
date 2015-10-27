#  @(#)Makefile	5.1 11/6/94
#
# Makefile for making Mesalib
# This assumes the following directory system as well as makefiles
#
# You should set your environment variables for the fortran compiler
# and the various flags and libraries needed.
#
#.SUFFIXES: .f90 .mod $(SUFFIXES)
#
CHR	=	Character_Manipulation_Subroutines
IOSYS	=	IOsys_Subroutines
MATH	=	Mathematical_Subroutines
CLAMS	=	Common_Los_Alamos_Mathematical_Subroutines
MACH	=	Machine_Dependent_Subroutines/opteron
GEN	=	General_Utility_Subroutines
INT	=	Integral_Library
DPINT	=	Double_Precision_Integral_Library
SYM	=	Symmetry_Library
#
MESALIB_DIRECTORIES	=	$(CHR) $(IOSYS) $(MATH) $(CLAMS) $(MACH) $(GEN) $(INT) $(DPINT) $(SYM)
MAKE	=	make
MFLAGS	=	
#
all: $(MESALIB_DIRECTORIES)
.RECURSIVE: $(MESALIB_DIRECTORIES)
$(MESALIB_DIRECTORIES): FORCE
	cd $@ ; $(MAKE) $(MFLAGS) 

make: FORCE
	cd $(CHR) ; $(MAKE) $(MFLAGS) make
	cd $(IOSYS) ; $(MAKE) $(MFLAGS) make
	cd $(MATH) ; $(MAKE) $(MFLAGS) make
	cd $(CLAMS) ; $(MAKE) $(MFLAGS) make
	cd $(MACH) ; $(MAKE) $(MFLAGS) make
	cd $(GEN) ; $(MAKE) $(MFLAGS) make
	cd $(INT) ; $(MAKE) $(MFLAGS) make
	cd $(DPINT) ; $(MAKE) $(MFLAGS) make
	cd $(SYM) ; $(MAKE) $(MFLAGS) make

clean: FORCE
	cd $(CHR) ; $(MAKE) $(MFLAGS) clean
	cd $(IOSYS) ; $(MAKE) $(MFLAGS) clean
	cd $(MATH) ; $(MAKE) $(MFLAGS) clean
	cd $(CLAMS) ; $(MAKE) $(MFLAGS) clean
	cd $(MACH) ; $(MAKE) $(MFLAGS) clean
	cd $(GEN) ; $(MAKE) $(MFLAGS) clean
	cd $(INT) ; $(MAKE) $(MFLAGS) clean
	cd $(DPINT) ; $(MAKE) $(MFLAGS) clean
	cd $(SYM) ; $(MAKE) $(MFLAGS) clean
	rm -f *.a *~
clean_rep: FORCE
	rm -fR RCS* svn* .svn*
	cd $(CHR) ; rm -fR RCS* svn* .svn* ; $(MAKE) $(MFLAGS) clean_rep
	cd $(IOSYS) ; rm -fR RCS* svn* .svn* ; $(MAKE) $(MFLAGS) clean_rep
	cd $(MATH) ; rm -fR RCS* svn* .svn* ; $(MAKE) $(MFLAGS) clean_rep
	cd $(CLAMS) ; rm -fR RCS* svn* .svn* ; $(MAKE) $(MFLAGS) clean_rep
	cd $(MACH) ; rm -fR RCS* svn* .svn* ; $(MAKE) $(MFLAGS) clean_rep
	cd $(GEN) ; rm -fR RCS* svn* .svn* ; $(MAKE) $(MFLAGS) clean_rep
	cd $(INT) ; rm -fR RCS* svn* .svn* ; $(MAKE) $(MFLAGS) clean_rep
	cd $(DPINT) ; rm -fR RCS* svn* .svn* ; $(MAKE) $(MFLAGS) clean_rep
	cd $(SYM) ; rm -fR RCS* svn* .svn* ; $(MAKE) $(MFLAGS) clean_rep
	rm -f *.a *~

FORCE:

