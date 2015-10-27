*deck %W%  %G%
 
      subroutine ioopn(unit,file,status,iostat,extra)
c
c***begin prologue     ioopn
c***date written       870706   (yymmdd)
c***revision date      yymmdd   (yymmdd)
c
c***keywords           iosys dependent routine
c***author             saxe, paul (lanl)
c***source             %W%   %G%
c
c***purpose            to open a unit to a disc file 
c
c***description        
c
c***references         
c
c***routines called    (none)
c
c***end prologue       ioopn
c
      implicit integer (a-z)
c
      parameter (buflen=8192)
c
      character*(*) file
      character*(*) status
      character*(*) extra
      integer unit
      integer iostat
      integer*4 iostat4, unit4
c
c     ----- open the unit as a fortran unit -----
c
      unit4 = unit
      if (status.eq.'scratch') then
c
c        note that the record length is in bytes on the sun.
         open (unit=unit4,
     $        recl=buflen,
     $        access='direct',
     $        status=status,
     $        iostat=iostat4)
      else
         open (unit=unit4,
     $        file=file,
     $        recl=buflen,
     $        access='direct',
     $        status=status,
     $        iostat=iostat4)
      end if
      iostat = iostat4
c
c
      return
      end
