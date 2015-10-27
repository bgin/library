#    @(#)Makefile	5.2   4/17/95
#
#    Makefile for the mathematics library of MesaLib
#    
include $(MODULES)/Makefile.inc
.SUFFIXES: .f90 .F90 $(SUFFIXES)
#
SRCS = \
	Angular_Module.f90 \
	Special_Functions.f90 \
	Lentz_Thompson.f90 \
	Prolate_Functions.f90 \
	Regular_Associated_Legendre_Functions.f90 \
	Associated_Legendre_Functions.f90 
OBJECTS = \
	$(SRCS:.f90=.o) 
#
$(MATHEMATICS_MODULE_LIBRARY): $(OBJECTS)
	$(AR) $(ARFLAGS) $(MATHEMATICS_MODULE_LIBRARY) $(OBJECTS)
	$(RANLIB) $(MATHEMATICS_MODULE_LIBRARY)
	$(AR) $(ARFLAGS) $(ALL_MODULE_LIBRARY) $(OBJECTS)
	$(RANLIB) $(ALL_MODULE_LIBRARY)
sources: $(SRCS)

clean:
	rm -f *.o *.mod *.a
