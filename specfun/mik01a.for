        program mik01a
c
c       =============================================================
c       purpose: this program computes the modified bessel functions 
c                i0(x), i1(x), k0(x), k1(x), and their derivatives 
c                using subroutine ik01a
c       input :  x   --- argument ( x � 0 )
c       output:  bi0 --- i0(x)
c                di0 --- i0'(x)
c                bi1 --- i1(x)
c                di1 --- i1'(x)
c                bk0 --- k0(x)
c                dk0 --- k0'(x)
c                bk1 --- k1(x)
c                dk1 --- k1'(x)
c       example:
c
c         x      i0(x)         i0'(x)        i1(x)         i1'(x)
c       -------------------------------------------------------------
c        1.0  .1266066d+01  .5651591d+00  .5651591d+00  .7009068d+00
c       10.0  .2815717d+04  .2670988d+04  .2670988d+04  .2548618d+04
c       20.0  .4355828d+08  .4245497d+08  .4245497d+08  .4143553d+08
c       30.0  .7816723d+12  .7685320d+12  .7685320d+12  .7560546d+12
c       40.0  .1489477d+17  .1470740d+17  .1470740d+17  .1452709d+17
c       50.0  .2932554d+21  .2903079d+21  .2903079d+21  .2874492d+21
c
c         x      k0(x)         k0'(x)        k1(x)         k1'(x)
c       -------------------------------------------------------------
c        1.0  .4210244d+00 -.6019072d+00  .6019072d+00 -.1022932d+01
c       10.0  .1778006d-04 -.1864877d-04  .1864877d-04 -.1964494d-04
c       20.0  .5741238d-09 -.5883058d-09  .5883058d-09 -.6035391d-09
c       30.0  .2132477d-13 -.2167732d-13  .2167732d-13 -.2204735d-13
c       40.0  .8392861d-18 -.8497132d-18  .8497132d-18 -.8605289d-18
c       50.0  .3410168d-22 -.3444102d-22  .3444102d-22 -.3479050d-22
c       =============================================================
c
        implicit double precision (a-h,o-z)
        write(*,*)'please enter x '
        read(*,*)x
        write(*,10)x
        write(*,*)'  x       i0(x)          i0''(x)         i1(x)',
     &            '          i1''(x)'
        write(*,*)'-------------------------------------------',
     &            '----------------------'
        call ik01a(x,bi0,di0,bi1,di1,bk0,dk0,bk1,dk1)
        write(*,20)x,bi0,di0,bi1,di1
        write(*,*)
        write(*,*)'  x       k0(x)          k0''(x)         k1(x)',
     &            '          k1''(x)'
        write(*,*)'-------------------------------------------',
     &            '----------------------'
        write(*,20)x,bk0,dk0,bk1,dk1
10      format(3x 'x =',f5.1)
20      format(1x,f4.1,4d15.7)
        end


        subroutine ik01a(x,bi0,di0,bi1,di1,bk0,dk0,bk1,dk1)
c
c       =========================================================
c       purpose: compute modified bessel functions i0(x), i1(1),
c                k0(x) and k1(x), and their derivatives
c       input :  x   --- argument ( x � 0 )
c       output:  bi0 --- i0(x)
c                di0 --- i0'(x)
c                bi1 --- i1(x)
c                di1 --- i1'(x)
c                bk0 --- k0(x)
c                dk0 --- k0'(x)
c                bk1 --- k1(x)
c                dk1 --- k1'(x)
c       =========================================================
c
        implicit double precision (a-h,o-z)
        dimension a(12),b(12),a1(8)
        pi=3.141592653589793d0
        el=0.5772156649015329d0
        x2=x*x
        if (x.eq.0.0d0) then
           bi0=1.0d0
           bi1=0.0d0
           bk0=1.0d+300
           bk1=1.0d+300
           di0=0.0d0
           di1=0.5d0
           dk0=-1.0d+300
           dk1=-1.0d+300
           return
        else if (x.le.18.0d0) then
           bi0=1.0d0
           r=1.0d0
           do 15 k=1,50
              r=0.25d0*r*x2/(k*k)
              bi0=bi0+r
              if (dabs(r/bi0).lt.1.0d-15) go to 20
15         continue
20         bi1=1.0d0
           r=1.0d0
           do 25 k=1,50
              r=0.25d0*r*x2/(k*(k+1))
              bi1=bi1+r
              if (dabs(r/bi1).lt.1.0d-15) go to 30
25         continue
30         bi1=0.5d0*x*bi1
        else
           data a/0.125d0,7.03125d-2,
     &            7.32421875d-2,1.1215209960938d-1,
     &            2.2710800170898d-1,5.7250142097473d-1,
     &            1.7277275025845d0,6.0740420012735d0,
     &            2.4380529699556d01,1.1001714026925d02,
     &            5.5133589612202d02,3.0380905109224d03/
           data b/-0.375d0,-1.171875d-1,
     &            -1.025390625d-1,-1.4419555664063d-1,
     &            -2.7757644653320d-1,-6.7659258842468d-1,
     &            -1.9935317337513d0,-6.8839142681099d0,
     &            -2.7248827311269d01,-1.2159789187654d02,
     &            -6.0384407670507d02,-3.3022722944809d03/
           k0=12
           if (x.ge.35.0) k0=9
           if (x.ge.50.0) k0=7
           ca=dexp(x)/dsqrt(2.0d0*pi*x)
           bi0=1.0d0
           xr=1.0d0/x
           do 35 k=1,k0
35            bi0=bi0+a(k)*xr**k
           bi0=ca*bi0
           bi1=1.0d0
           do 40 k=1,k0
40            bi1=bi1+b(k)*xr**k
           bi1=ca*bi1
        endif
        if (x.le.9.0d0) then
           ct=-(dlog(x/2.0d0)+el)
           bk0=0.0d0
           w0=0.0d0
           r=1.0d0
           do 65 k=1,50
              w0=w0+1.0d0/k
              r=0.25d0*r/(k*k)*x2
              bk0=bk0+r*(w0+ct)
              if (dabs((bk0-ww)/bk0).lt.1.0d-15) go to 70
65            ww=bk0
70         bk0=bk0+ct
        else
           data a1/0.125d0,0.2109375d0,
     &             1.0986328125d0,1.1775970458984d01,
     &             2.1461706161499d02,5.9511522710323d03,
     &             2.3347645606175d05,1.2312234987631d07/
           cb=0.5d0/x
           xr2=1.0d0/x2
           bk0=1.0d0
           do 75 k=1,8
75            bk0=bk0+a1(k)*xr2**k
           bk0=cb*bk0/bi0
        endif
        bk1=(1.0d0/x-bi1*bk0)/bi0
        di0=bi1
        di1=bi0-bi1/x
        dk0=-bk1
        dk1=-bk0-bk1/x
        return
        end