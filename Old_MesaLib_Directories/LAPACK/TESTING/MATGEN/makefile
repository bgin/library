FORTRAN = f77
OPTS    = -O -u
NOOPT   = -u

#######################################################################
#  This is the makefile to create a library of the test matrix
#  generators used in LAPACK.  The files are organized as follows:
#
#     SCATGEN  -- Auxiliary routines called from both REAL and COMPLEX
#     DZATGEN  -- Auxiliary routines called from both DOUBLE PRECISION
#                 and COMPLEX*16
#     SMATGEN  -- Single precision real matrix generation routines
#     CMATGEN  -- Single precision complex matrix generation routines
#     DMATGEN  -- Double precision real matrix generation routines
#     ZMATGEN  -- Double precision complex matrix generation routines
#
#  The library can be set up to include routines for any combination
#  of the four precisions.  First, modify the FORTRAN and OPTS 
#  definitions to match your compiler and the options to be used.
#  Then to create or add to the library, enter make followed by one or
#  more of the precisions desired.  Some examples:
#       make single
#       make single complex
#       make single double complex complex16
#  Alternatively, the command
#       make
#  without any arguments creates a library of all four precisions.
#  The library is called
#       tmglib.a
#  and is created at the LAPACK directory level.
#
#  To remove the object files after the library is created, enter
#       make clean
#  On some systems, you can force the source files to be recompiled by
#  entering (for example)
#       make single FRC=FRC
#
#  ***Note***
#  On some systems, the ranlib command is not needed and the ranlib
#  command statements must be commented out or deleted.
#
#----------------------------------------------------------------------
#
#  Edward Anderson, University of Tennessee
#  Susan Ostrouchov, Last updated January 31, 1992
#
#######################################################################
 
SCATGEN = slatm1.o slaran.o slarnd.o

SMATGEN = slatms.o slatme.o slatmr.o \
   slagge.o slagsy.o slarge.o slaror.o slarot.o slatm2.o slatm3.o

CMATGEN = clatms.o clatme.o clatmr.o \
   clagge.o claghe.o clagsy.o clarge.o claror.o clarot.o clatm1.o \
   clarnd.o clatm2.o clatm3.o

DZATGEN = dlatm1.o dlaran.o dlarnd.o

DMATGEN = dlatms.o dlatme.o dlatmr.o \
   dlagge.o dlagsy.o dlarge.o dlaror.o dlarot.o dlatm2.o dlatm3.o

ZMATGEN = zlatms.o zlatme.o zlatmr.o \
   zlagge.o zlaghe.o zlagsy.o zlarge.o zlaror.o zlarot.o zlatm1.o \
   zlarnd.o zlatm2.o zlatm3.o

all: single complex double complex16

single: $(SMATGEN) $(SCATGEN)
	ar cr ../../tmglib.a $(SMATGEN) $(SCATGEN)
	ranlib ../../tmglib.a

complex: $(CMATGEN) $(SCATGEN)
	ar cr ../../tmglib.a $(CMATGEN) $(SCATGEN)
	ranlib ../../tmglib.a

double: $(DMATGEN) $(DZATGEN)
	ar cr ../../tmglib.a $(DMATGEN) $(DZATGEN)
	ranlib ../../tmglib.a

complex16: $(ZMATGEN) $(DZATGEN)
	ar cr ../../tmglib.a $(ZMATGEN) $(DZATGEN)
	ranlib ../../tmglib.a

$(SCATGEN): $(FRC)
$(SMATGEN): $(FRC)
$(CMATGEN): $(FRC)
$(DZATGEN): $(FRC)
$(DMATGEN): $(FRC)
$(ZMATGEN): $(FRC)
 
FRC:
	@FRC=$(FRC)

clean: ; \
        rm -f *.o

.f.o: ; $(FORTRAN) -c $(OPTS) $<
