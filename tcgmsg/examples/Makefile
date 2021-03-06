
    TOPDIR = /n/srv2/MESA/library/tcgmsg
    LIBDIR = $(TOPDIR)/ipcv4.0
    TCGMSG = $(LIBDIR)/libtcgmsg.a

   INSTALL = echo "$@" is built


######################################
# Should not need to modify below here
######################################

.SUFFIXES:	
.SUFFIXES: .o .f .c .s

.s.o:
	$(AS) $*.s
.f.o:
	$(FC) $(FFLAGS) -c $*.f
.c.o:
	$(CC) $(CFLAGS) -c $*.c

all everything:	scf15 scf30 scf60 md mc jacobi grid template.p

extra:	scf60lin scf120lin scf240lin

clean:
	/bin/rm -f *.o *.p core event* 
	/bin/rm -f scf15 scf30 scf60 scf60lin scf120lin scf240lin 
	/bin/rm -f md mc jacobi grid

realclean:	clean
	/bin/rm -f Makefile fexit.f *~ *.trace *.V demo TAGS msgtypesf.h #*#


msgtypesf.h:	$(LIBDIR)/msgtypesf.h
	cp $(LIBDIR)/msgtypesf.h .

############
#SCF example
############

# Note rather messy use of same fortran source but different
# include/block data files and copy in make rule.
# This screws up normal simple dependency of object on include files
# and makes targets scf15/30/60 etc. unable to be made in parallel.

SOBJ = $(SCFBLAS) integ.o output.o timer.o fexit.o diagon.o

scf15:	cscf15.h blkdat15.f scf.f $(SOBJ)
	cp cscf15.h cscf.h
	$(FC) $(FFLAGS) -o $@ scf.f blkdat15.f $(SOBJ) $(TCGMSG) $(LIBS)
	$(INSTALL)

scf30:	cscf30.h blkdat30.f scf.f $(SOBJ)
	cp cscf30.h cscf.h
	$(FC) $(FFLAGS) -o $@ scf.f blkdat30.f $(SOBJ) $(TCGMSG) $(LIBS)
	$(INSTALL)

scf60:	cscf60.h blkdat60.f scf.f $(SOBJ)
	cp cscf60.h cscf.h
	$(FC) $(FFLAGS) -o $@ scf.f blkdat60.f $(SOBJ) $(TCGMSG) $(LIBS)
	$(INSTALL)

scf60lin:	cscf60lin.h blkdat60lin.f scf.f $(SOBJ)
	cp cscf60lin.h cscf.h
	$(FC) $(FFLAGS) -o $@ scf.f blkdat60lin.f $(SOBJ) $(TCGMSG) $(LIBS)
	$(INSTALL)

scf120lin:	cscf120lin.h blkdat120lin.f scf.f $(SOBJ)
	cp cscf120lin.h cscf.h
	$(FC) $(FFLAGS) -o $@ scf.f blkdat120lin.f $(SOBJ) $(TCGMSG) $(LIBS)
	$(INSTALL)

scf240lin:	cscf240lin.h blkdat240lin.f scf.f $(SOBJ)
	cp cscf240lin.h cscf.h
	$(FC) $(FFLAGS) -o $@ scf.f blkdat240lin.f $(SOBJ) $(TCGMSG) $(LIBS)
	$(INSTALL)

$(SOBJ) scf15 scf30 scf60 scf60lin scf120lin scf240lin:	msgtypesf.h

###########
#MD example
###########

md:	md.o dscal.o timer.o fexit.o
	$(FC) $(FFLAGS) -o $@ md.o dscal.o timer.o fexit.o \
			$(TCGMSG) $(LIBS)
	$(INSTALL)

md.o:	msgtypesf.h

###########
#MC example
###########

mc:	mc.o timer.o fexit.o
	$(FC) $(FFLAGS) -o $@ mc.o timer.o fexit.o $(TCGMSG) $(LIBS)
	$(INSTALL)

mc.o:	msgtypesf.h

################
# Jacobi example
################

  JOBJ = fexit.o timer.o getmem.o mxv_fortran.o

jacobi:	jacobi.o $(JOBJ)
	$(FC) $(FFLAGS) -o $@ jacobi.o $(JOBJ) $(TCGMSG) $(LIBS)
	$(INSTALL)

jacobi.o:	msgtypesf.h

#############
#Grid example
#############

grid:	grid.o
	$(CC) $(CFLAGS) -o $@ grid.o $(TCGMSG) -lm $(LIBS)
	$(INSTALL)

grid.o:	$(LIBDIR)/msgtypesc.h

###########
#template.p
###########

template.p:
	echo "# This template is piped thru sed to replace" >  template.p
	echo "# PROGRAM with the name of the program."      >> template.p
	echo "# Add other hosts and processes as desired."  >> template.p
	echo "`whoami` `hostname` 4 `pwd`/PROGRAM /tmp"   >> template.p
