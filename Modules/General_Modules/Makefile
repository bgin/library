#    @(#)Makefile	5.2   4/17/95
#
#    Makefile for the mathematics library of MesaLib
#    
include $(MODULES)/Makefile.inc
.SUFFIXES: .f90 .F90 $(SUFFIXES)
#
SRCS = \
	Accuracy.f90 \
	Input_Output.f90 \
	Matrix_Print.f90 \
	Data_Module.f90 \
	Potential.f90 \
	Grid_Global.f90 \
	Dvr_Shared.f90 \
	Dvr_Global.f90 \
	Atomic_Matrices.f90 \
	Global_Time_Propagation_Module.f90 \
	Prop_Prnt.f90 \
	Prop_Global.f90 \
	Arnoldi_Global.f90\
	Lanczos_Global.f90\
	Lanczos_Prnt.f90\
	Arnoldi_Global_it.f90 \
	Arnoldi_Global_rt.f90 \
	Dvd_Prnt.f90 \
	Dvr_Prnt.f90 \
	FD_Prnt.f90 \
	FD_Global.f90 \
	Dvrprop_Global_rt.f90 \
	Dvrprop_Global_it.f90 \
	Dvrprop_Global.f90 \
	Pack_Global.f90 \
	Iterative_Global.f90 \
	Prop_Derived_Types.f90 \
	Matrix_Utility_Module.f90 \
	Pack_Hamiltonian_Module.f90 \
	Pass.f90 \
	R_Matrix_Global.f90 \
	Mesa_Global.f90 \
	Dvr_Matrix_Vector_Multiply_Module.f90 \
	Full_Matrix_Vector_Multiply_Module.f90 \
	Full_Matrix_Vector_Iteration_Module.f90 \
	Packed_Matrix_Vector_Multiply_Module.f90 \
	Legendre_Data.f90 \
	FEDVR_Global.f90 \
	FEDVR_Shared.f90 \
	Dvr_Global.f90 \
	Coulomb_Shared.f90 \
	FEDVR_Derived_Types.f90 \
	Matrix_Defined_Types.f90 \
	Schmidt_Orthogonalization.f90 \
	Master_Module.f90 \
	Two_Electron_Shared.f90 \
	Set_Mesa_Parameters.F90

#
all: $(GENERAL_MODULE_LIBRARY) $(ALL_MODULE_LIBRARY) $(SRCS.f)

OBJECTS = \
	$(SRCS:.f90=.o)	$(SRCS:.F90=.o)
#
$(GENERAL_MODULE_LIBRARY): $(OBJECTS)
	$(AR) $(ARFLAGS) $(GENERAL_MODULE_LIBRARY) $(OBJECTS)
	$(RANLIB) $(GENERAL_MODULE_LIBRARY)
	$(AR) $(ARFLAGS) $(ALL_MODULE_LIBRARY) $(OBJECTS)
	$(RANLIB) $(ALL_MODULE_LIBRARY)
sources: $(SRCS)

clean:
	rm -f *.o *.mod *.a
