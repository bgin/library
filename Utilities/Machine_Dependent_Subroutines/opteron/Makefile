#   @(#)Makefile	5.2   4/18/95
#
#  Makefile for the machine-dependent portion of MesaLib
#
.SUFFIXES: .f90 .mod $(SUFFIXES)
include $(UTILITIES)/Makefile.inc
include $(LIBRARY)/Makefile.inc
GET = sccs get
REL = 

SRCS = \
	abortmsg.f \
	closeabs.f \
	asinh.f \
	chain.f \
	comand.f \
	d1mach.f \
	dattim.f \
	dcsevl.f \
	getscm.f \
	getmem.f \
	iadtwp.f \
	initds.f \
	intowp.f \
	iocls.f \
	ioget.f \
	iogetc.f \
	ioinq.f \
	ioopn.f \
	ioput.f \
	ioputc.f \
	iorm.f \
	itobyt.f \
	lnkerr.f \
	lxopen.f \
	nchrpw.f \
	openabs.f \
	rdabs.f \
	rmachsp.f \
	rmachdp.f \
	rmachr4.f \
	rmachr8.f \
	shiftl.f \
	shiftr.f \
	second.f \
	stderr.f \
	timing.f \
	traktm.f \
	tsumry.f \
	unqfil.f \
	usrnam.f \
	versn.f \
	wind.f \
	wpadti.f \
	wptbyt.f \
	wptoin.f \
	wrabs.f 

SRCS.F90 =	\
	Set_Mesa_Parameters.F90

OBJECTS = \
	$(SRCS.F90:.F90=.o) $(SRCS:.f=.o)

all: $(MACHLIB)
$(MACHLIB): $(OBJECTS)
	$(AR) $(ARFLAGS) $(MACHLIB) $(OBJECTS)
	$(RANLIB) $(MACHLIB)
	$(AR) $(ARFLAGS) $(UTILITY_LIB) $(OBJECTS)
	$(RANLIB) $(UTILITY_LIB)

sources: $(SRCS)
$(SRCS):
	$(GET) $(RELEASE) $@
clean:
	rm -f *.o
