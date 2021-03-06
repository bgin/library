*deck @(#)sgedi.F	5.1  11/29/95
      subroutine sgedi(a,lda,n,ipvt,det,work,job)                       
c***begin prologue  sgedi                                               
c***date written   780814   (yymmdd)                                    
c***revision date  820801   (yymmdd)                                    
c***category no.  d2a1,d3a1                                             
c***keywords  determinant,factor,inverse,linear algebra,linpack,matrix  
c***author  moler, c. b., (u. of new mexico)                            
c***purpose  computes the determinant and inverse of a matrix           
c            using the factors computed by sgeco or sgefa.              
c***description                                                         
c                                                                       
c     sgedi computes the determinant and inverse of a matrix            
c     using the factors computed by sgeco or sgefa.                     
c                                                                       
c     on entry                                                          
c                                                                       
c        a       real(lda, n)                                           
c                the output from sgeco or sgefa.                        
c                                                                       
c        lda     integer                                                
c                the leading dimension of the array  a .                
c                                                                       
c        n       integer                                                
c                the order of the matrix  a .                           
c                                                                       
c        ipvt    integer(n)                                             
c                the pivot vector from sgeco or sgefa.                  
c                                                                       
c        work    real(n)                                                
c                work vector.  contents destroyed.                      
c                                                                       
c        job     integer                                                
c                = 11   both determinant and inverse.                   
c                = 01   inverse only.                                   
c                = 10   determinant only.                               
c                                                                       
c     on return                                                         
c                                                                       
c        a       inverse of original matrix if requested.               
c                otherwise unchanged.                                   
c                                                                       
c        det     real(2)                                                
c                determinant of original matrix if requested.           
c                otherwise not referenced.                              
c                determinant = det(1) * 10.0**det(2)                    
c                with  1.0 .le. abs(det(1)) .lt. 10.0                   
c                or  det(1) .eq. 0.0 .                                  
c                                                                       
c     error condition                                                   
c                                                                       
c        a division by zero will occur if the input factor contains     
c        a zero on the diagonal and the inverse is requested.           
c        it will not occur if the subroutines are called correctly      
c        and if sgeco has set rcond .gt. 0.0 or sgefa has set           
c        info .eq. 0 .                                                  
c                                                                       
c     linpack.  this version dated 08/14/78 .                           
c     cleve moler, university of new mexico, argonne national lab.      
c                                                                       
c     subroutines and functions                                         
c                                                                       
c     blas saxpy,sscal,sswap                                            
c     fortran abs,mod                                                   
c***references  dongarra j.j., bunch j.r., moler c.b., stewart g.w.,    
c                 *linpack users  guide*, siam, 1979.                   
c***routines called  saxpy,sscal,sswap                                  
c***end prologue  sgedi                                                 
      implicit real*8 (a-h,o-z)
      integer lda,n,ipvt(1),job                                         
      real*8 a(lda,1),det(2),work(1)                                    71
c                                                                       
      real*8 t                                                          73
      real*8 ten                                                        74
      integer i,j,k,kb,kp1,l,nm1                                        
c                                                                       
c     compute determinant                                               
c                                                                       
c***first executable statement  sgedi                                   

      if (job/10 .eq. 0) go to 70                                       
         det(1) = 1.0d0                                                 
         det(2) = 0.0d0                                                 
         ten = 10.0d0                                                   
         do 50 i = 1, n                                                 
            if (ipvt(i) .ne. i) det(1) = -det(1)                        
            det(1) = a(i,i)*det(1)                                      
c        ...exit                                                        
            if (det(1) .eq. 0.0d0) go to 60                             
   10       if (abs(det(1)) .ge. 1.0d0) go to 20                        
               det(1) = ten*det(1)                                      
               det(2) = det(2) - 1.0d0                                  
            go to 10                                                    
   20       continue                                                    
   30       if (abs(det(1)) .lt. ten) go to 40                          
               det(1) = det(1)/ten                                      
               det(2) = det(2) + 1.0d0                                  
            go to 30                                                    
   40       continue                                                    
   50    continue                                                       
   60    continue                                                       
   70 continue                                                          
c                                                                       
c     compute inverse(u)                                                
c                                                                       
      if (mod(job,10) .eq. 0) go to 150                                 
         do 100 k = 1, n                                                
            a(k,k) = 1.0d0/a(k,k)                                       
            t = -a(k,k)                                                 
            call sscal(k-1,t,a(1,k),1)                                  
            kp1 = k + 1                                                 
            if (n .lt. kp1) go to 90                                    
            do 80 j = kp1, n                                            
               t = a(k,j)                                               
               a(k,j) = 0.0d0                                           
               call saxpy(k,t,a(1,k),1,a(1,j),1)                        
   80       continue                                                    
   90       continue                                                    
  100    continue                                                       
c                                                                       
c        form inverse(u)*inverse(l)                                     
c                                                                       
         nm1 = n - 1                                                    
         if (nm1 .lt. 1) go to 140                                      
         do 130 kb = 1, nm1                                             
            k = n - kb                                                  
            kp1 = k + 1                                                 
            do 110 i = kp1, n                                           
               work(i) = a(i,k)                                         
               a(i,k) = 0.0d0                                           
  110       continue                                                    
            do 120 j = kp1, n                                           
               t = work(j)                                              
               call saxpy(n,t,a(1,j),1,a(1,k),1)                        
  120       continue                                                    
            l = ipvt(k)                                                 
            if (l .ne. k) call sswap(n,a(1,k),1,a(1,l),1)               
  130    continue                                                       
  140    continue                                                       
  150 continue                                                          

      return                                                            
      end                                                               
