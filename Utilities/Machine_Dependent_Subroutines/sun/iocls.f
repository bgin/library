*deck %W%  %G%
      function iocls(unit,status)
c
c***begin prologue     iocls
c***date written       870706   (yymmdd)
c***revision date      yymmdd   (yymmdd)
c
c***keywords           iosys dependent routine
c***author             saxe, paul (lanl)
c***source             %W%   %G%
c
c***purpose            to close a unit.
c
c***description        
c
c***references         
c
c***routines called    (none)
c
c***end prologue       iocls
c
      implicit integer (a-z)
      integer*4 iocls, unituse
c
      character*(*) status
      integer unit
      unituse = unit
c
      close (unit=unituse,status=status,iostat=iocls)
c
c
      return
      end
