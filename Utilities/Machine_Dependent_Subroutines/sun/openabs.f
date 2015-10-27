      subroutine openabs(number,ifile,irecl)
      character*(*) ifile
      open(number,file=ifile,access='direct',recl=irecl,
     $ status='unknown')
      return
      end
