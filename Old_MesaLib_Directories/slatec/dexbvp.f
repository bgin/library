*deck dexbvp
      subroutine dexbvp (y, nrowy, xpts, a, nrowa, alpha, b, nrowb,
     +   beta, iflag, work, iwork)
c***begin prologue  dexbvp
c***subsidiary
c***purpose  subsidiary to dbvsup
c***library   slatec
c***type      double precision (exbvp-s, dexbvp-d)
c***author  watts, h. a., (snla)
c***description
c
c  this subroutine is used to execute the basic technique for solving
c  the two-point boundary value problem.
c
c***see also  dbvsup
c***routines called  dbvpor, xermsg
c***common blocks    dml15t, dml17b, dml18j, dml5mc, dml8sz
c***revision history  (yymmdd)
c   750601  date written
c   890531  changed all specific intrinsics to generic.  (wrb)
c   890831  modified array declarations.  (wrb)
c   890911  removed unnecessary intrinsics.  (wrb)
c   890921  realigned order of variables in certain common blocks.
c           (wrb)
c   890921  revision date from version 3.2
c   891214  prologue converted to version 4.0 format.  (bab)
c   900328  added type section.  (wrb)
c   900510  convert xerrwv calls to xermsg calls.  (rwc)
c   910722  updated author section.  (als)
c***end prologue  dexbvp
c
      integer icoco, iexp, iflag, igofx, inc, indpvt, info, inhomo,
     1     integ, istkop, ivp, iwork(*), k1, k10, k11, k2, k3,
     2     k4, k5, k6, k7, k8, k9, kkkint, kkkzpw, knswot, kop, kotc,
     3     l1, l2, lllint, lotjp, lpar, mnswot, mxnon, ncomp, ndisk,
     4     neediw, needw, neq, neqivp, nfc, nfcc, nic, nopg,
     5     nps, nrowa, nrowb, nrowy, nsafiw, nsafw, nswot, ntape, ntp,
     6     numort, nxpts
      double precision a(nrowa,*), ae, alpha(*), b(nrowb,*), beta(*),
     1     c, eps, fouru, pwcnd, px, re, sqovfl, sru, tnd, tol, twou,
     2     uro, work(*), x, xbeg, xend, xl, xop, xot, xpts(*), xsav,
     3     y(nrowy,*), zquit
      character*8 xern1, xern2
c
c     ******************************************************************
c
      common /dml8sz/ c,xsav,igofx,inhomo,ivp,ncomp,nfc
      common /dml18j/ ae,re,tol,nxpts,nic,nopg,mxnon,ndisk,ntape,neq,
     1                indpvt,integ,nps,ntp,neqivp,numort,nfcc,
     2                icoco
      common /dml15t/ px,pwcnd,tnd,x,xbeg,xend,xot,xop,info(15),istkop,
     1                knswot,kop,lotjp,mnswot,nswot
      common /dml17b/ kkkzpw,needw,neediw,k1,k2,k3,k4,k5,k6,k7,k8,k9,
     1                k10,k11,l1,l2,kkkint,lllint
c
      common /dml5mc/ uro,sru,eps,sqovfl,twou,fouru,lpar
c
c***first executable statement  dexbvp
      kotc = 1
      iexp = 0
      if (iwork(7) .eq. -1) iexp = iwork(8)
c
c     compute orthonormalization tolerances.
c
   10 tol = 10.0d0**((-lpar - iexp)*2)
c
      iwork(8) = iexp
      mxnon = iwork(2)
c
c **********************************************************************
c **********************************************************************
c
      call dbvpor(y,nrowy,ncomp,xpts,nxpts,a,nrowa,alpha,nic,b,
     1            nrowb,beta,nfc,iflag,work(1),mxnon,work(k1),ntp,
     2            iwork(18),work(k2),iwork(16),work(k3),work(k4),
     3            work(k5),work(k6),work(k7),work(k8),work(k9),
     4            work(k10),iwork(l1),nfcc)
c
c **********************************************************************
c **********************************************************************
c     if dmgsbv returns with message of dependent vectors, we reduce
c     orthonormalization tolerance and try again. this is done
c     a maximum of 2 times.
c
      if (iflag .ne. 30) go to 20
      if (kotc .eq. 3  .or.  nopg .eq. 1) go to 30
      kotc = kotc + 1
      iexp = iexp - 2
      go to 10
c
c **********************************************************************
c     if dbvpor returns message that the maximum number of
c     orthonormalizations has been attained and we cannot continue, then
c     we estimate the new storage requirements in order to solve problem
c
   20 if (iflag .ne. 13) go to 30
      xl = abs(xend-xbeg)
      zquit = abs(x-xbeg)
      inc = 1.5d0*xl/zquit * (mxnon+1)
      if (ndisk .ne. 1) then
         nsafw = inc*kkkzpw + needw
         nsafiw = inc*nfcc + neediw
      else
         nsafw = needw + inc
         nsafiw = neediw
      endif
c
      write (xern1, '(i8)') nsafw
      write (xern2, '(i8)') nsafiw
      call xermsg ('slatec', 'dexbvp',
     *   'in dbvsup, predicted storage allocation for work array is ' //
     *   xern1 // ', predicted storage allocation for iwork array is '
     *   // xern2, 1, 0)
c
   30 iwork(1) = mxnon
      return
      end