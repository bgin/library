!
MODULE Set_Mesa_Parameters
!
     USE input_output
     IMPLICIT NONE
     INTEGER                             :: BUFLEN = 2048
     INTEGER                             :: MAXCOR=500000000
     INTEGER                             :: MAXBIG=800
     INTEGER                             :: MAXNBFKOHN=300
     INTEGER                             :: MAXPRIMKOHN=500
     INTEGER                             :: MAXCHAN=20
     INTEGER                             :: MAXLTOP=10
     INTEGER                             :: MAXLMTOP=40
     INTEGER                             :: MAXSMALL=200
     INTEGER, DIMENSION(:), ALLOCATABLE  :: buffer

!
!################################################################################################
!
                                   Contains
!
!################################################################################################
  Subroutine Defaults
     IMPLICIT NONE
!
!           Default is for intel fortran compiler with 32 bit integers
!
     BUFLEN=2048
     MAXCOR=500000000
     MAXBIG=800
     MAXNBFKOHN=300   ! maximum number of basis functions             
     MAXPRIMKOHN=500  ! maximum number of gaussian primitives          
     MAXCHAN=20       ! maximum number of coupled channels
     MAXLTOP=10       ! maximum asymptotic L                            
     MAXLMTOP=40      ! maximum number of partial waves 
                                                      ! (with different l and m) in any channel
     MAXSMALL=200     ! maximum total number of partial waves 
     write(iout,*) 'DEFAULTS'
     write(iout,*) BUFLEN, MAXCOR, MAXSMALL, MAXBIG, MAXNBFKOHN, MAXPRIMKOHN, MAXCHAN, MAXLTOP, MAXLMTOP
      
!
  END Subroutine  Defaults
!
!################################################################################################
!
  Subroutine Gfortran
!
     IMPLICIT NONE
     MAXCHAN=20       ! maximum number of coupled channels
     MAXLTOP=10       ! maximum asymptotic L                            
     MAXLMTOP=40      ! maximum number of partial waves 
                                                      ! (with different l and m) in any channel
     MAXSMALL=200     ! maximum total number of partial waves 
                                                      ! summed over all channels, i.e. size of the 
                                                      ! scattering matrix
     MAXNBFKOHN=300   ! maximum number of basis functions             
     MAXPRIMKOHN=500  ! maximum number of gaussian primitives          
# ifdef apple
#       ifdef i8
           Call GFI8
           write(iout,*) 'GFORTRAN APPLE I8'
           write(iout,*) BUFLEN, MAXCOR, MAXSMALL, MAXBIG, MAXNBFKOHN, MAXPRIMKOHN, MAXCHAN, MAXLTOP, MAXLMTOP
#       endif 
#       ifdef i4
           Call GFI4
           write(iout,*) 'GFORTRAN APPLE I4'
           write(iout,*) BUFLEN, MAXCOR, MAXSMALL, MAXBIG, MAXNBFKOHN, MAXPRIMKOHN, MAXCHAN, MAXLTOP, MAXLMTOP
#       endif 
# endif
!
  END Subroutine  Gfortran
!################################################################################################
  Subroutine GFI8
!
     IMPLICIT NONE
     BUFLEN=1024
     MAXCOR=625000000
     MAXBIG=3500
!
  END Subroutine  GFI8
!################################################################################################
  Subroutine GFI4
!
     IMPLICIT NONE
     BUFLEN=2048
     MAXCOR=120000000
     MAXBIG=800
!
  END Subroutine  GFI4
!################################################################################################      
  Subroutine F77
     IMPLICIT NONE
!
     MAXNBFKOHN=300   ! maximum number of basis functions             
     MAXPRIMKOHN=500  ! maximum number of gaussian primitives          
     MAXCHAN=20       ! maximum number of coupled channels
     MAXLTOP=10       ! maximum asymptotic L                            
     MAXLMTOP=40      ! maximum number of partial waves 
                                                      ! (with different l and m) in any channel
     MAXSMALL=200     ! maximum total number of partial waves 
# ifdef apple
#       ifdef i8
          Call F77I8
          write(iout,*) 'F77 APPLE I8'
          write(iout,*) BUFLEN, MAXCOR, MAXBIG
#       endif 
#       ifdef i4
          Call F77I4
          write(iout,*) 'F77 APPLE I4'
          write(iout,*) BUFLEN, MAXCOR, MAXBIG
#       endif 
#       ifdef applen
          Call F77I4
          write(iout,*) 'F77 APPLEN'
          write(iout,*) BUFLEN, MAXCOR, MAXBIG
#       endif 
# endif 
!
  END Subroutine F77
!################################################################################################
  Subroutine F77I8
     IMPLICIT NONE
     BUFLEN=1024
     MAXCOR=200000000
     MAXBIG=800
!
  END Subroutine  F77I8
!################################################################################################
  Subroutine F77I4
     IMPLICIT NONE
     BUFLEN=2048
     MAXCOR=120000000
     MAXBIG=800
!
  END Subroutine  F77I4
!################################################################################################
  Subroutine G77
!
     IMPLICIT NONE
     BUFLEN=2048
     MAXCOR=120000000
     MAXBIG=800  
     MAXNBFKOHN=300   ! maximum number of basis functions             
     MAXPRIMKOHN=500  ! maximum number of gaussian primitives          
     MAXCHAN=20       ! maximum number of coupled channels
     MAXLTOP=10       ! maximum asymptotic L                            
     MAXLMTOP=40      ! maximum number of partial waves 
                                                  ! (with different l and m) in any channel
     MAXSMALL=200     ! maximum total number of partial waves 
!
  END Subroutine G77
!################################################################################################
!
  Subroutine Ifort
!
  IMPLICIT NONE
# ifdef apple
          Call Apple
# endif

# ifdef intelmkl
#       ifdef intelc
          Call Intelci8
#       endif intelc
#       ifdef i8
          Call IntelmklI8
#       endif i8
#       ifdef i4
          Call IntelmklI4
#       endif i4
#       ifdef i4L
          Call IntelmklI4
#       endif I4L
# endif 
!
# ifdef intelmkb
#       ifdef i8 
          Call IntelmklbI8
#       endif i8
#       ifdef i4
          Call IntelmklbI4
#       endif i4
# endif 
  END Subroutine Ifort
!################################################################################################
  Subroutine Apple
!
     IMPLICIT NONE
     BUFLEN=1024
     MAXCOR=625000000
     MAXBIG=2000  
     MAXNBFKOHN=500
     MAXPRIMKOHN=700
     MAXCHAN=20
     MAXLTOP=10
     MAXLMTOP=40
     MAXSMALL=400
!
  END Subroutine Apple
!################################################################################################
  Subroutine IntelcI8
!
     IMPLICIT NONE
     BUFLEN=1024
     MAXCOR=625000000
     MAXBIG=2000  
     MAXNBFKOHN=500
     MAXPRIMKOHN=700
     MAXCHAN=20
     MAXLTOP=10
     MAXLMTOP=60
     MAXSMALL=400
!
  END Subroutine IntelcI8
!################################################################################################
  Subroutine IntelmklI8
!
     IMPLICIT NONE
     BUFLEN=1024
     MAXCOR=625000000
     MAXBIG=800  
     MAXNBFKOHN=300
     MAXPRIMKOHN=500
     MAXCHAN=20
     MAXLTOP=10
     MAXLMTOP=40
     MAXSMALL=200
!
  END Subroutine IntelmklI8
!################################################################################################
  Subroutine IntelmklI4
!
     IMPLICIT NONE
     BUFLEN=2048
     MAXCOR=500000000
     MAXBIG=800  
     MAXNBFKOHN=300
     MAXPRIMKOHN=500
     MAXCHAN=20
     MAXLTOP=10
     MAXLMTOP=40
     MAXSMALL=200
!
  END Subroutine IntelmklI4
!################################################################################################
  Subroutine IntelmklbI8
!
     IMPLICIT NONE
     BUFLEN=1024
     MAXCOR=625000000
     MAXBIG=1000  
     MAXNBFKOHN=500
     MAXPRIMKOHN=700
     MAXCHAN=30
     MAXLTOP=20
     MAXLMTOP=60
     MAXSMALL=400
!
  END Subroutine IntelmklbI8
!################################################################################################
  Subroutine IntelmklbI4
!
     IMPLICIT NONE
     BUFLEN=1024
     MAXCOR=625000000
     MAXBIG=800  
     MAXNBFKOHN=300
     MAXPRIMKOHN=500
     MAXCHAN=20
     MAXLTOP=10
     MAXLMTOP=40
     MAXSMALL=200
!
  END Subroutine IntelmklbI4
!################################################################################################
!
END MODULE  Set_Mesa_Parameters
