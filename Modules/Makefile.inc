#
# Including Makefiles must define TOP_DIR appropriately.
#
# System-dependent definitions
#
#
# Suffix rules
.SUFFIXES: .f90 .F90 $(SUFFIXES)
#
NOLINK = -c
COMPILE = $(F90) $(INCLUDE_LIST) $(NOLINK) $(F90FLAGS)
.F90.o:
	$(COMPILE) $(CPPFLAGS) $<
.f90.o:
	$(COMPILE) $<
#
GENERAL_MODULES = $(MODULES)/General_Modules
MATHEMATICS_MODULES = $(MODULES)/Mathematics_Modules 

#
ALL_MODULES = \
	    $(GENERAL_MODULES) \
	    $(MATHEMATICS_MODULES)


#
GENERAL_MODULE_LIBRARY = $(MODULES)/gen_mod_lib.a
MATHEMATICS_MODULE_LIBRARY = $(MODULES)/math_mod_lib.a
ALL_MODULE_LIBRARY = $(MODULES)/all_mod_lib.a
#
