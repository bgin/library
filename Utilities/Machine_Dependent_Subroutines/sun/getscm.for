*deck %W%  %G%
      SUBROUTINE getscm(need,core,ngot,name,fail)
c***begin prologue     getscm
c***date written       850601  (yymmdd)
c***revision date      870208  (yymmdd)
c
c   8 february 1987   pws at lanl
c      for the sun 3/50 and 3/160 under bsd 4.2 unix, making a large
c      blank COMMON and letting paging DO the job.
c
c  21 may 1986 pws at lanl
c              fixing bug by subtracting 'curavl' from 'mxaval' IF need=-1
c              and in the check for going over the top of core. this should
c              prevent getting more core than have asked for.
c
c***keywords           memory adjustment, DYNAMIC memory, core adjustment
c***author             martin, richard and saxe, paul (lanl)
c***source             %W%   %G%
c***description
c                      CALL getscm(need,core,ngot,name,fail)
c
c                      MODULE to increment blank COMMON length.
c                      input arguments:
c                         need ... additional number of words requested.
c                         core ... starting address of memory region.
c                         name ... the name of the routine requesting memory.
c                                  used for posting output messages.
c                         fail ... what to DO IF you can't have that much.
c                                  .eq.0  ... abort if request exceeds availabil
c                                  .ne.0  ... get all you can.
c
c                      output arguments.
c                         ngot ... the number of words obtained.
c
c                      special cases:
c                         if need=-1, get as much core as possible.
c                         if need=0,  return possible expansion relative to 'cor
c                                     in 'ngot'.
c
c***references
c
c***iosys i/o          
c                      mxcore         INTEGER     written   1
c
c***routines called    lnkerr(mdutil),
c***END prologue       getscm
c
      IMPLICIT INTEGER(a-z)
c
c     note that this PARAMETER refers to the maximum number of
c     INTEGER words available for USE.
c      PARAMETER (maxcor=120000000)
      PARAMETER (maxcor=#maxcor)
c
      CHARACTER name*8
      LOGICAL called
      DIMENSION core(*)  
c
      COMMON // a(maxcor)
      COMMON/memory/ioff
      COMMON/io/inp,iout
      EXTERNAL itobyt
c
      DATA called/.FALSE./
      SAVE mxcore
c
 1000 FORMAT(1x,8a1,'requested ',i7,' words of memory;',
     $       i7,' available.')
c
      ioff=1
      locdif=loc(a)-loc(core) 
      mxaval=locdif/itobyt(1)+maxcor-1
      curavl=mxaval
c
      IF (need.EQ.0) THEN
         ngot=mxaval-1000
         RETURN
      END IF
c
c
      nwant=need-curavl
c
      IF(need.EQ.-1) nwant=mxaval-curavl-1000
      IF(nwant.GT.mxaval-curavl) THEN
         IF(fail.EQ.0) THEN
            WRITE(iout,1000) (name(i:i),i=1,8), nwant, mxaval
            CALL lnkerr('user requested abort: need exceeds '
     $                  //'availability.')
         ELSE
            nwant=mxaval
         ENDIF
      ENDIF
c
c     get the memory.
      ngot=curavl
      IF(nwant.LE.0) RETURN
c
c
      loccoreneed = core(need)
      mxcore=loccoreneed/itobyt(1)
      ngot=need
      CALL iosys('write integer mxcore on rwf',1,mxcore,0,' ')
c
c
      RETURN
      END
