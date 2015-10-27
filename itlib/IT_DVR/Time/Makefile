#   @(#)Makefile	1.9   8/7/91
#
#  Makefile for the dvr iterative routines
#
include $(MESA_HOME)/Makefile.inc
ITDVRLIB = ../../itdvrlib.a

GET = sccs get
REL = 

SRCS = \
	cblslv.f \
	cfctrs.f \
	csublu.f \
	cv2rv.f \
	rv2cv.f

all: $(ITDVRLIB)
$(ITDVRLIB): $(SRCS:.f=.o)
	$(AR) $(ARFLAGS) $(ITDVRLIB) $(?:.f=.o)



