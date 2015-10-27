#  @(#)Makefile	5.1 11/6/94
#
TOP_DIR = $(LIBRARY)/itlib
include $(TOP_DIR)/Makefile.inc
SHARED = $(TOP_DIR)/Shared
SHARED_DIRECTORIES = 	$(SHARED)/1d $(SHARED)/2d $(SHARED)/3d $(SHARED)/Preconditioners $(SHARED)/Main 
MAKE = make
all: $(SHARED_DIRECTORIES)

.RECURSIVE: $(SHARED_DIRECTORIES)

$(SHARED_DIRECTORIES): FORCE
	cd $@ ; $(MAKE) $(MFLAGS)

make: FORCE
	cd $(SHARED)/1d ; $(MAKE) $(MFLAGS) make \
	cd $(SHARED)/2d ; $(MAKE) $(MFLAGS) make \
	cd $(SHARED)/3d ; $(MAKE) $(MFLAGS) make \
	cd $(SHARED)/Preconditioners ; $(MAKE) $(MFLAGS) make \
	cd $(SHARED)/Main ; $(MAKE) $(MFLAGS) make

clean: FORCE
	cd $(SHARED)/1d ; $(MAKE) $(MFLAGS) clean
	cd $(SHARED)/2d ; $(MAKE) $(MFLAGS) clean
	cd $(SHARED)/3d ; $(MAKE) $(MFLAGS) clean
	cd $(SHARED)/Preconditioners ; $(MAKE) $(MFLAGS)  clean
	cd $(SHARED)/Main ; $(MAKE) $(MFLAGS) clean
FORCE:
