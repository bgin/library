c \documentclass{article}
c \usepackage{graphicx}
c \setkeys{Gin}{width=\linewidth}
c \title{Subroutine for Gaussian Wavepacket Normalization}
c \author{Barry I. Schneider}
c \date{}
c \def \<{\langle}
c \def \>{\rangle}
c \begin{document}
c \maketitle

*deck nrpaket.f
***begin prologue     nrpaket
***date written       960723   (yymmdd)
***revision date      yymmdd   (yymmdd)
***keywords           time development
***author             schneider, barry (nsf)
***source             
***purpose            calculate normailzation integrals for gaussian wavepacket 
***references         
***routines called    
***end prologue       nrpaket
      subroutine nrpaket(norm,q1,q2,q3,wt1,wt2,wt3,sigma,x_0,beta,
     1                   nd,dim,coord)
      implicit integer (a-z)
      real*8 norm, q1, q2, q3, wt1, wt2, wt3
      real*8 sigma, beta, x_0
      real*8 ov
      character*(*) coord
      character*80 title
      dimension nd(3)
      dimension q1(nd(1)), q2(nd(2)), q3(nd(3))
      dimension wt1(nd(1)), wt2(nd(2)), wt3(nd(3))
      dimension sigma(3), beta(3), x_0(3), ov(3)
      common/io/inp, iout
      if(coord.eq.'cartesian') then
         norm=1.d0
         if(dim.ge.1) then
            ov(1)=0.d0
            do 10 i=1,nd(1)
               ov(1) = ov(1) + wt1(i)*exp(- ( q1(i) - x_0(1) ) *
     1                                      ( q1(i) - x_0(1) )
     2                                    / ( sigma(1)*sigma(1) ) )
 10         continue
            norm=norm*ov(1)
         endif
         if(dim.ge.2) then
            ov(2)=0.d0
            do 20 i=1,nd(2)
               ov(2) = ov(2) + wt2(i)*exp(- ( q2(i) - x_0(2) ) *
     1                                      ( q2(i) - x_0(2) )
     2                                     / ( sigma(2)*sigma(2) ) )
 20         continue   
            norm=norm*ov(2)
         endif
         if(dim.ge.3) then
            ov(3)=0.d0
            do 30 i=1,nd(3)
               ov(3) = ov(3) + wt3(i)*exp(- ( q3(i) - x_0(3) ) *
     1                                      ( q3(i) - x_0(3) )
     2                                     / ( sigma(3)*sigma(3) ) )
 30         continue   
            norm=norm*ov(3)
         endif            
      else
         norm=0.d0
         do 40 i=1,nd(1)
            norm = norm + wt1(i)* q1(i) * q1(i) * 
     1                    exp(- ( q1(i) - x_0(1) ) *
     2                          ( q1(i) - x_0(1) )
     3                          / ( sigma(1)*sigma(1) ) )
 40      continue   
      endif
      norm=1.d0/sqrt(norm)
      write(iout,1) norm
 1    format(/,1x,'normalization factor for gaussian packet = ',e15.8)
      return
      end       







