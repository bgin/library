*deck %W%  %G%
      function winstr(nchars)
c***begin prologue     winstr
c***date written       850601  (yymmdd)
c***revision date      870207  (yymmdd)
c
c   7 february 1987   pws at lanl
c       32 bit integer version
c
c***keywords           words in string, character
c***author             martin, richard (lanl)
c***source             %W%   %G%
c***purpose            returns the number of integer words required to store
c                      a character string.
c***description
c                      winstr is an integer function used as:
c                        len=winstr(nchars)
c                          nchars   the number of characters in the string.
c
c***references
c***routines called    nchrpw(mdutil), note that winstr need not be in mdutil.
c***end prologue       winstr
      implicit integer(a-z)
      integer winstr
c
c
      nc=nchrpw(0)
      winstr=(nchars-1+nc)/nc
c
c
      return
      end
