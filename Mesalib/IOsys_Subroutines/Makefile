#   @(#)Makefile	5.1   11/6/94
#
#  Makefile for the I/O portion of MesaLib
#
.SUFFIXES: .f90 .mod $(SUFFIXES)
include $(MESALIB)/Makefile.inc
include $(LIBRARY)/Makefile.inc
GET = sccs get
REL = 

SRCS = \
	dirsrt.f \
	gettok.f \
	ioabor.f \
	ioclos.f \
	iocopy.f \
	iodest.f \
	iodump.f \
	ioeof.f \
	ioexst.f \
	iofile.f \
	ioflno.f \
	iolen.f \
	ioopen.f \
	ioread.f \
	iorew.f \
	iosize.f \
	iosys.f \
	iounit.f \
	iowait.f \
	iowrit.f \
	iowt.f \
	iowtab.f \
	unitwt.f  \
	wind.f

OBJECTS = \
	$(SRCS:.f=.o) 

all: $(IOLIB)
$(IOLIB): $(OBJECTS)
	$(AR) $(ARFLAGS) $(IOLIB) $(OBJECTS)
	$(RANLIB) $(IOLIB)
	$(AR) $(ARFLAGS) $(MESA_LIB) $(OBJECTS)
	$(RANLIB) $(MESA_LIB)
	cp $(SRCS:.f=.o) $(FULL_DIR)

sources: $(SRCS)
	$(CO) $(RCS)/*.f

clean:
	rm -f *.o

