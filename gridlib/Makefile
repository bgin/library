#   @(#)Makefile	1.4   8/2/91
#
#
TOP_DIR=$(LIBRARY)/gridlib
GRIDLIB=$(TOP_DIR)/grid_lib.a
include $(MESA_HOME)/Makefile.inc

SRCS = \
		Lebedev_Data.f90 \
		Data.f90 \
		Grid_Defined_Types.f90 \
		Gauss_Quadrature.f90 \
		Gauss_Angular_Quadrature.f90 \
		Lebedev_Quadrature.f90 \
		Renormalization.f90 \
		Shell_Info.f90 \
		Matrix_Elements.f90

OBJECTS = \
	$(SRCS:.f90=.o)	

all: $(GRIDLIB)
$(GRIDLIB): $(OBJECTS)
	$(AR) $(ARFLAGS) $(GRIDLIB) $(OBJECTS)
	$(RANLIB) $(GRIDLIB)
