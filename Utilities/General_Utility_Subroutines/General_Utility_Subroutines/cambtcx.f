*deck cambtcx
c***begin prologue     cambtcx
c***date written       880423   (yymmdd)
c***revision date      yymmdd   (yymmdd)
c***keywords           complex matrix multiply
c***author             schneider, barry (lanl)
c***source             mylib
c***                                             t
c***purpose            matrix multiply  a = a - b * c
c***description        vectorized matrix multiply
c***                   for complex a, b and c
c***                  
c***references         none
c
c***routines called    czero(mylib)
c***end prologue       cambtcx
      subroutine cambtcx(a,na,b,nb,c,nc,ni,nk,nj)
      implicit integer (a-z)
      complex *16 a, b, c, zero, one
      dimension a(na,nj), b(nb,ni), c(nc,nj)
      zero=cmplx(0.d0,0.d0)
      one=cmplx(1.d0,0.d0)
c
      call cgemm('t','n',ni,nj,nk,-one,b,nb,c,nc,one,a,na)
      return
      end