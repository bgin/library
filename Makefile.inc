#
#                   Define the compile and load statements
#
# System-dependent definitions
#
# Suffix rules
#
.SUFFIXES: .f90 .F90 $(SUFFIXES)
NOLINK = -c
#
COMPILE=$(F90) $(INCLUDE_LIST) $(NOLINK) $(F90FLAGS)
#
.F90.o:
	$(COMPILE) $(CPPFLAGS) $<
.f90.o:
	$(COMPILE) $<
.f.o:
	$(COMPILE) $<
.for.o:
	$(COMPILE) $<
.F.o:
	$(COMPILE) $(CPPFLAGS) $<
#
LOAD_LIB = $(MOD_LIB) $(POTENTIAL_LIB) $(UTILITY_LIB)
#
