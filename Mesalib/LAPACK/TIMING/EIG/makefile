FORTRAN = f77
OPTS    = -O -u
NOOPT   = -u
LOADER  = f77
LOADOPTS = 
TMGLIB  = ../../tmglib.a
LAPACK  = ../../lapack.a
EIGSRC  = eigsrc.a
BLAS    = ../../blas.a

#######################################################################
#  This makefile creates the timing programs for the eigensystem
#  routines in LAPACK.  The timing files are grouped as follows:
#
#       AEIGTIM -- General auxiliary timing routines
#       SCIGTIM -- Single precision auxiliary timing routines
#       DZIGTIM -- Double precision auxiliary timing routines
#
#       SEIGTIM -- Single precision real timing routines
#       CEIGTIM -- Single precision complex timing routines
#       DEIGTIM -- Double precision real timing routines
#       ZEIGTIM -- Double precision complex timing routines
#
#  Timing programs can be generated for all or some of the four
#  data types.  First, modify the FORTRAN and OPTS definitions to refer
#  to the compiler and desired compiler options for your machine, and
#  define LOADER and LOADOPTS to refer to the loader and desired load
#  options for your machine.  Also replace the definitions for BLAS and
#  LAPACK with the names of these libraries on your system if necessary.
#  Then to enter make followed by one or more of the data types desired.
#  Some examples:
#       make single
#       make single complex
#       make single double complex complex16
#  Alternatively, the command
#       make
#  without any arguments creates all four timing programs.
#  The executable files are called
#       xeigtims, xeigtimd, xeigtimc, and xeigtimz
#  and are created in the next higher directory level.
#
#  To remove the object files after the executable files have been
#  created, enter
#       make clean
#  On some systems, you can force the source files to be recompiled by
#  entering (for example)
#       make single FRC=FRC
#
#----------------------------------------------------------------------
#
#  Edward Anderson, University of Tennessee
#  March 26, 1990
#  Susan Ostrouchov, Last updated January 19, 1993
#
#######################################################################

AEIGTIM = atimin.o ilaenv.o xlaenv.o

SCIGTIM = \
   smflop.o sopla.o sopla2.o \
   sprtbe.o sprtbg.o sprtbr.o sprtbs.o sprtbv.o

DZIGTIM = \
   dmflop.o dopla.o dopla2.o \
   dprtbe.o dprtbg.o dprtbr.o dprtbs.o dprtbv.o

SEIGTIM = stimee.o \
   slaqzh.o slatm4.o stim21.o stim22.o stim26.o stim51.o seispack.o

CEIGTIM = ctimee.o \
   claqzh.o clatm4.o ctim21.o ctim22.o ctim26.o ctim51.o ceispack.o

DEIGTIM = dtimee.o \
   dlaqzh.o dlatm4.o dtim21.o dtim22.o dtim26.o dtim51.o deispack.o

ZEIGTIM = ztimee.o \
   zlaqzh.o zlatm4.o ztim21.o ztim22.o ztim26.o ztim51.o zeispack.o

all:  single double complex complex16

single: ../xeigtims
double: ../xeigtimd
complex: ../xeigtimc
complex16: ../xeigtimz

../xeigtims:$(AEIGTIM) $(SCIGTIM) $(SEIGTIM)
	$(LOADER) $(LOADOPTS)  $(AEIGTIM) $(SCIGTIM) $(SEIGTIM) \
        $(TMGLIB) $(EIGSRC) $(LAPACK) $(BLAS)  -o ../xeigtims

../xeigtimc:$(AEIGTIM) $(SCIGTIM) $(CEIGTIM)
	$(LOADER) $(LOADOPTS)  $(AEIGTIM) $(SCIGTIM) $(CEIGTIM) \
        $(TMGLIB) $(EIGSRC) $(LAPACK) $(BLAS) -o ../xeigtimc

../xeigtimd:$(AEIGTIM) $(DZIGTIM) $(DEIGTIM)
	$(LOADER) $(LOADOPTS)  $(AEIGTIM) $(DZIGTIM) $(DEIGTIM) \
        $(TMGLIB) $(EIGSRC) $(LAPACK) $(BLAS)  -o ../xeigtimd

../xeigtimz:$(AEIGTIM) $(DZIGTIM) $(ZEIGTIM)
	$(LOADER) $(LOADOPTS)  $(AEIGTIM) $(DZIGTIM) $(ZEIGTIM) \
        $(TMGLIB) $(EIGSRC) $(LAPACK) $(BLAS)  -o ../xeigtimz

$(AEIGTIM): $(FRC)
$(SCIGTIM): $(FRC)
$(DZIGTIM): $(FRC)
$(SEIGTIM): $(FRC)
$(CEIGTIM): $(FRC)
$(DEIGTIM): $(FRC)
$(ZEIGTIM): $(FRC)
 
FRC: ; \
        @FRC=$(FRC)
 
clean: ; \
        rm -f *.o
 
.f.o : ; $(FORTRAN) $(OPTS) -c $<
