#   @(#)Makefile	1.9   8/7/91
#
#  Makefile for the CLAMS (common Los Alamos mathematical subroutines)
#  portion of MesaLib
#
.SUFFIXES: .f90 .F90 .mod $(SUFFIXES)
GET = sccs get
REL = 
include $(LIBRARY)/Makefile.inc
include $(MESALIB)/Makefile.inc
#
SRCS.f = \
	alnrel.f\
	bakvec.f \
	balanc.f \
	balbak.f \
	bisect.f \
	bsaxpy.f \
	bscattr.f \
	bscopy.f \
	bsdot.f \
	c9lgmc.f \
	c_arg.f \
	catan.f \
	cbabk2.f \
	cbabk22.f \
	cbabk3.f \
	cbal.f \
	ccot.f \
	cdiv.f \
	cgamma.f \
	cgeev.f \
	cgevlrv.f \
	clngam.f \
	clnrel.f \
	comqr.f \
	comqr2.f \
	corth.f \
	cpsi.f \
	csevl.f \
	csroot.f \
	dcabs1.f \
	fdump.f \
	figi.f \
	figi2.f \
	gamlim.f \
	gamma.f \
	hqr.f \
	hqr2.f \
	i1mach.f \
	inits.f \
	interv.f \
	imtqlv.f \
	j4save.f \
	lzit.f \
	lzhes.f \
	orthes.f \
	ortran.f \
	psi.f \
	rabs.f \
	r1mach.f \
	r9lgmc.f \
	s88fmt.f \
	scopym.f \
	sgbfa.f \
	sgbsl.f \
	sgeev.f \
	sgmm.f \
	sgmv.f \
	sgtcon.f \
        sgtrmv.f \
	sgttrf.f \
	sgttrs.f \
	sgtsl.f \
	slacon.f \
	slangt.f \
	slassq.f \
	snrm2.f \
	sqrdc.f \
	sqrsl.f \
	sqrst.f \
	srot.f  \
	srotg.f \
	sspev.f \
	ssvdc.f \
	strdi.f \
	strsl.f \
	thtw.f \
	tinvit.f \
	tqlrat.f \
	tridib.f \
	xerclr.f \
	xermsg.f \
	xerprn.f \
	xersve.f \
	xerhlt.f \
	xercnt.f \
	xgetua.f

CPPSRCS.F = \
	caxpy.F \
	ccopy.F \
	cdotc.F \
	cdotu.F \
	cgefa.F \
	cgemm.F \
	cgemv.F \
	cgesl.F \
	cgetrf.F \
	cgetrs.F \
	cscal.F \
	cswap.F \
	epslon.F \
	icamax.F \
	icopy.F \
	imtql1.F \
	imtql2.F \
	isamax.F \
	ismin.F \
	mxma.F \
	pythag.F \
	rsp.F \
	sasum.F \
	saxpy.F \
	scatter.F \
	scopy.F \
	sdot.F \
	sgeco.F \
	sgedi.F \
	sgefa.F \
	sgefs.F \
	sgemm.F \
	sgemv.F \
	sgesl.F \
	sgetrf.F \
	sgetrs.F \
	sscal.F \
	sspfa.F \
	sspsl.F \
	sswap.F \
	tql1.F \
	tql2.F \
	tqlrat.F \
	trbak1.F \
	trbak3.F \
	tred1.F \
	tred2.F \
	tred3.F 

CPPSRCS.F90 = \
	dztpsv.F90 

OBJECT.f = \
	$(SRCS.f:.f=.o) 
OBJECT.F = \
	$(CPPSRCS.F:.F=.o)
OBJECT.F90 = \
	$(CPPSRCS.F90:.F90=.o) 

all: $(CLAMLIB) $(OBJECT.f) $(OBJECT.F) $(OBJECT.F90)
$(CLAMLIB): $(OBJECT.f) $(OBJECT.F) $(OBJECT.F90)
	$(AR) $(ARFLAGS) $(CLAMLIB) $(OBJECT.f)
	$(RANLIB) $(CLAMLIB)
	$(AR) $(ARFLAGS) $(MESA_LIB) $(OBJECT.f)
	$(RANLIB) $(MESA_LIB)
	$(AR) $(ARFLAGS) $(CLAMLIB) $(OBJECT.F)
	$(RANLIB) $(CLAMLIB)
	$(AR) $(ARFLAGS) $(MESA_LIB) $(OBJECT.F)
	$(RANLIB) $(MESA_LIB)
	$(AR) $(ARFLAGS) $(CLAMLIB) $(OBJECT.F90)
	$(RANLIB) $(CLAMLIB)
	$(AR) $(ARFLAGS) $(MESA_LIB) $(OBJECT.F90)
	$(RANLIB) $(MESA_LIB)
	cp $(OBJECT.f) $(FULL_DIR)
	cp $(OBJECT.F) $(FULL_DIR)
	cp $(OBJECT.F90) $(FULL_DIR)
#	rm -f $(SRCS:.f=.o) $(CPPSRCS:.F=.o) 
clean:
	rm -f *.o
