c*deck rsdvd.f
c***begin prologue     rsdvd
c***date written       960723   (yymmdd)
c***revision date      yymmdd   (yymmdd)
c***keywords           iterative eigenvalue
c***author             schneider, barry (nsf)
c***source             
c***purpose            find the eigenvalues of a real symmetric matrix
c***                   having diagonal dominance using the davidson 
c***                   algorithm.  
c***
c***description        the davidson algorithm as programmed here is
c***                   capable of getting the lowest m roots or of
c***                   climbing the eigenvalue tree in a piecewise fashion
c***                   by getting the first m1 roots, then the next m2 roots
c***                   then the next.... roots until all are obtained.
c***                   it can often happen that roots will be missed, 
c***                   especially if one tries to compute too many 
c***                   at each pass.  so, the routine must be used 
c***                   intelligently.  it is typically simple to see if a
c***                   root has been missed by inspection.  the method avoids
c***                   diagonalizing a large matrix for the higher roots by
c***                   explicitly orthogonalizing the guesses to the converged
c***                   eigenvectors of the lower roots.
c***
c***     variable      type       size             description
c***     _______       ____       ____             ___________
c***      ibuf          integer 2*lenbuf      buffer to hold non-zero values of
c***                                          hamiltonian indices
c***      rbuf          real    lenbuf        buffer to hold non-zero values of
c***                                          hamiltonian values
c***      diag          real    n             diagonals of hamiltonian

c***      vec           real    n*maxvec      davidson vectors
c***                                          can be same storage as trials
c***      hvec          real    n*maxvec      action of the hamiltonian on vec
c***      b, bwrk       real    maxvec*maxvec small hamiltonian matrix and a 
c***                                          copy
c***      eigwrk        real    maxvec        eigenvalues of small matrix
c***      work          real    5*maxvec      scratch array
c***      svec          real    maxvec*maxvec small scratch matrix
c***      resid         real    n*maxvec      residual vectors
c***      eig           real    nroot         converged eigenvalues
c***      rep           real                  nuclear repulsion
c***      cnverg        real                  convergence criterion for a root
c***      thresh        real                  overlap tolerance for accepting 
c***                                          a new davidson vector
c***      n             integer               matrix size
c***      nroot         integer               number of roots to get
c***      ntrial        integer               number of trial vectors
c***      nattim        integer               number of roots to converge at a 
c***                                          pass
c***      maxit         integer               maximum size of davidson space
c***      maxvec        integer               maximum number of vectors 
c***                                          available for storage
c***      lenbuf        integer               size of buffer for non-zero 
c***                                          labels and matrix elements
c***      header        character             labels for reading buffers
c***      prnt          logical               print flags   
c***references         
c
c***routines called    
c***end prologue       rsdvd
      subroutine rsdvd(ibuf,rbuf,diag,eig,vec,hvec,resid,b,bwrk,
     1                 eigwrk,work,svec,rep,cnverg,thresh,n,nroot,
     2                 ntrial,nattim,maxit,maxvec,lenbuf,
     3                 headr,prnt,code)
      implicit integer (a-z)
      real*8 rbuf, diag, eig, vec, hvec, resid, b, bwrk, eigwrk
      real*8 work, svec, rep, error, cnverg, thresh, sdot, tmp
      real*8 maxerr, zero, one, nrzero, eci
      real*4 time(2), secnds, delta(10), total
      integer*8 pre
      logical prnt, incore
      character*5 itoc
      character*8 cntrl, pascod, precon
      character*80 title
      character*(*) headr, code
      dimension rbuf(lenbuf), ibuf(2,lenbuf), diag(n), eig(nroot)
      dimension vec(n,maxvec), hvec(n,maxvec), resid(n,maxvec)
      dimension b(maxvec,maxvec), bwrk(maxvec,maxvec)
      dimension eigwrk(maxvec), work(5*maxvec), svec(maxvec,*)
      dimension headr(3), pascod(2)
      dimension prnt(11)
      common/io/inp, iout
      data zero, one/ 0.d0, 1.d0/
      data nrzero / 1.0d-06 /
      data incore/.false./
      data precon /'diagonal' /
      len=length(code)
      pascod(1)=code(1:len)//'-e:'
      pascod(2)=code(1:len)//'-v:'
      call iosys('open bliu as scratch',0,0,0,' ')
      call iosys('read integer '//headr(2)//' from hamiltonian',
     1            1,ntot,0,' ')
      if(ntot.le.lenbuf) then
         incore=.true.
      endif
      write(iout,1) nroot, nattim, ntrial, maxit, maxvec, cnverg
      do 2000 trial=1,ntrial
         call iosys('read real "guess:'//itoc(trial)//'" from guess',
     1               n,vec(1,trial),0,' ') 
 2000 continue   
      call iosys('write real trials to bliu without rewinding',
     1            n*ntrial,vec,0,' ')
c
c     find out how many passes are needed to get all the roots
c      
      npass=nroot/nattim
      nleft=nroot-npass*nattim
      if(nleft.ne.0) then
         npass=npass+1
      else
         nleft=nattim
      endif            
      count=0
      root0=0
      rootn=0
      num2do=nattim
      numvec=ntrial 
      do 3000 i=1,10 
         delta(i)=0.0
 3000 continue   
      do 1000 ipass=1,npass
         time(1)=secnds(0.0)
         if(ipass.eq.npass) then
            num2do=nleft
         endif
         root0=rootn+1
         rootn=rootn+num2do    
         write(iout,2) ipass, root0, rootn      
c-----------------------------------------------------------------------c
c                                                                       c
c                    Initialization Section                             c
c                                                                       c
c-----------------------------------------------------------------------c
c
c        get the initial set of vectors by orthogonalizing the trials
c        to the set of roots which have been converged.  this will ensure
c        that the subspace is orthogonal to previously converged vectors. 
c
         num=numvec
         if(count.ne.0) then
            call rdciv(resid,pascod(2),n,count)
            call abschm(resid,vec,thresh,n,count,num,nout,
     1                  .true.,.false.)
            num=nout
         endif
         if(num.ne.0) then
            call gschmt(vec,thresh,n,1,num,nout,.true.,.false.)
         endif
         if(nout.eq.0) then
            write(iout,3)
            call lnkerr('quit davidson. no more trial vectors '//
     1                  'possible')
         endif                  
         if(prnt(2)) then
            title='initial vectors'
            call prntfm(title,vec,n,nout,n,nout,iout)
         endif
c
c        initialize the effect of the hamiltonian on these vectors.
c
         title='h on initial vectors'
         call honv(ibuf,rbuf,diag,vec(1,1),hvec(1,1),
     1             n,nout,headr(1),lenbuf,ntot,incore,
     2             title,prnt(3))
c
c        initialize the small hamiltonian matrix.
c            
         call hsmall(b,bwrk,vec,hvec,n,ndum,nout,maxvec,
     1               'initialize',.false.)
         time(2)=secnds(0.0)
         delta(1)=delta(1) + time(2) - time(1)
c----------------------------------------------------------------------c
c                                                                      c
c                    Iteration Sequence                                c
c                                                                      c
c     iteration is continued until all of the roots are converged      c 
c     or if convergence is not achieved some preset maximum number of  c
c     iterations are performed.                                        c
c                                                                      c
c----------------------------------------------------------------------c
         nend=nout 
         iter=0
         error=1.d+10
         write(iout,4) error
         cntrl='continue'
         do while ( cntrl.eq.'continue'.and.iter.lt.maxit )
            iter = iter + 1
c        Step 1:
c
c           get eigenvalues and eigenvectors of the small matrix.
c
c                bwrk holds the initial matrix which is destroyed.
c                svec has the transformation matrix.
c                note that resid is used as temporary storage in rdiag
c                and vscale.
c
c            title='iteration = '//itoc(iter)//' diagonalizing '//
c     1            'matrix of dimension N = '//itoc(nend)
c
            time(1)=secnds(0.0)
            if(prnt(4)) then
               title='initial small matrix'
               call prntfm(title,bwrk,nend,nend,maxvec,maxvec,iout)
            endif
c        
            call rdiag(bwrk,eigwrk,svec,work,rep,iter,maxvec,
     1                 nend,prnt(5))
            time(2)=secnds(0.0)
            delta(2) = delta(2) + time(2) - time(1)
c      
c        Step 2:
c
c           transform vec and hvec to the new basis defined by the
c           diagonalization in rdiag.  the small matrix becomes diagonal under
c           this transformation and we fill it with the eigenvalues.
c           then form the residuals and check for convergence.  the converged
c           eigenvalues are stored and the eigenpairs written to disk.  the
c           unconverged residuals are moved from their current positions in 
c           resid to the leading positions.
c
            write(iout,5) iter, nend
            time(1)=secnds(0.0)
            call frmres(vec,hvec,resid,svec,eig,eigwrk,b,bwrk,rep,
     1                  cnverg,maxerr,n,nend,num2do,con,uncon,
     2                  maxvec,iter,prnt(5),count,pascod)
            time(2)=secnds(0.0)
            delta(3) = delta(3) + time(2) - time(1)
c
c     
c           check to see if all num2do roots are converged
c           if so, we are done and can print results and then proceed to 
c           the next set of roots.
c
            if(con.eq.num2do) then
c
               nlft=min(nend-num2do,ntrial)
               remain=ntrial-nlft
               write(iout,6)
               do 10 i=1,num2do
                  actual=i+count
                  write(iout,7) actual, eig(actual)+rep
 10            continue
c
c              begin next set of roots with the best available
c              vectors.  these are the the remaining vectors coming 
c              from the diagonalization of the small matrix augmented
c              with the remaining trials.
               if(actual.lt.nroot) then
                  write(iout,8) nlft, remain
                  if(nlft.gt.0) then
                     write(iout,*) ' Doing the vectors moves'
                     call copy(vec(1,num2do+1),vec,n*nlft)  
                     if(remain.gt.0) then
                        call iosys('read real trials from bliu',
     1                              n*remain,vec(1,nlft+1),n*nlft,' ')
                     endif
                  else
                     call iosys('read real trials from bliu',n*ntrial,
     1                           vec,0,' ')
                  endif
               endif
               numvec=ntrial
               cntrl='finished'
            else
c
c           all roots are not converged.  set the error to the largest
c           current error and either restart or continue the 
c           iteration sequence.
c
               error=min(error,maxerr)
c
c           how many new vectors could be added in principle
c
               numnew = maxvec - nend
c                                             
c           how many will we add
c
               addvec = min(numnew,uncon)
c              
c           check if the number of old plus new vectors will exceed
c           the maxvec limit to determine if a restart is needed.
c
               chkno = nend + addvec    
               if(chkno.ge.maxvec) then
c             
c              the maximum number of vectors is exceeded.
c
                  nend = min(2*num2do,nend)
                  write(iout,9) nend
                  numnew = maxvec - nend
                  addvec = min(numnew,uncon)
               endif
               write(iout,11) addvec
c
c              maximum number of vectors is still within the allowed
c              limits.  add vectors to the set from the unconverged
c              residuals and put them after the current vectors.
c
               if(addvec.ne.0) then
                  oldnum = nend   
                  nbeg = nend + 1
                  time(1)=secnds(0.0)
                  call genvec(vec(1,nbeg),resid,diag,eigwrk,eigwrk,dum,
     1                        dum,dum,n,dum,pre,addvec,iter,
     2                        precon,dum,prnt(9))
                  time(2)=secnds(0.0)
                  delta(4) = delta(4) + time(2) - time(1)
c
c              orthonormalize the new trials to the converged roots
c              and then to the old vectors to get an additional 
c              nout vectors.
c        
                  num=addvec
                  nout=num
                  time(1)=secnds(0.0)
                  if(count.ne.0) then
                     call rdciv(resid,pascod(2),n,count)
                     call abschm(resid,vec(1,nbeg),thresh,n,count,
     1                           addvec,num,.true.,.false.)
                  endif
                  nend=nend+num
                  if(num.ne.0) then
                     call gschmt(vec,thresh,n,nbeg,nend,
     1                           nout,.true.,.false.)
                  endif
                  if(num.eq.0.or.nout.eq.0) then
                     write(iout,12) num, nout
                    return
                  endif
                  time(2)=secnds(0.0)
                  delta(5) = delta(5) + time(2) - time(1)
c                
                  if(nout.eq.0) then
c
c                 no more vectors write out unconverged results.
c               
                     write(iout,13)
                     return
                  endif   
                  nend=nbeg+nout-1
                  if(prnt(10)) then
                     call tstovl(vec,n,nend)
                  endif
               endif            
c
c           we now have either a reconstituted set of vectors or
c           an augmented set of vectors.               
c
c              operate with hamiltonian on these vectors
c
               time(1)=secnds(0.0)
               title='h on initial vectors iteration-'//itoc(iter)
               call honv(ibuf,rbuf,diag,vec(1,nbeg),hvec(1,nbeg),
     1                   n,nout,headr(1),lenbuf,ntot,incore,
     2                   title,prnt(3))
               time(2)=secnds(0.0)
               delta(6) = delta(6) + time(2) - time(1)
c
c           update the curent small hamiltonian matrix.
c            
               time(1)=secnds(0.0)
               call hsmall(b,bwrk,vec,hvec,n,nbeg,nend,maxvec,
     1                     'fill',.false.)
               time(2)=secnds(0.0)
               delta(7) = delta(7) + time(2) - time(1)
            endif
         enddo
         if(iter.ge.maxit) then
            write(iout,14)   
            return
         endif        
         count=count+num2do
 1000 continue
      write(iout,15)
      do 20 i=1,nroot
         eci=eig(i)+rep
         write(iout,7) i, eci
 20   continue           
      total=delta(1) + delta(2) + delta(3) + delta(4) +
     1      delta(5) + delta(6) + delta(7)
      write(iout,16) ( delta(i), i=1,7), total
      return
 1    format(/,1x,'davidson eigenvalue solver',/,10x,
     1            'number of roots               = ',i4,/,10x,
     2            'number of roots at a time     = ',i4,/,10x,
     3            'number of trials              = ',i4,/,10x,
     4            'maximum number of iterations  = ',i4,/,10x,
     5            'maximum number of vectors     = ',i4,/,10x,
     6            'convergence criterion for rms = ',e15.8)      
 2    format(/,5x,'pass = ',i3,/,5x,
     1            'processing root = ',i4,' to root =',i4)
 3    format(/,5x,'cannot even begin davidson calculation:',/,5x,
     1            'orthonormalization of initial vectors yields null '
     2            'set')      
 4    format(/,5x,'beginning davidson iterations:',/,5x,
     1            'initial error = 'e15.8) 
 5    format (/,5x,'cycle = ',i4,2x,'no. vectors = ',i4)
 6    format(/,30x,'summary',/,17x,'root',22x,'energy')
 7    format(15x,i4,20x,f15.8)
 8    format(/,5x,'number of added vectors from unconverged roots = ',
     1                                                        i5,/,5x,
     2            'number of added vectors from original trial set = ',
     3                                                        i5)
 9    format(/,10x,'***** maximum number of vectors exceeded *****',
     1        /10x,'      contract back to ',1x,i4,' vectors')
 11   format(/,5x,'number of vectors added = ',i4)                   
 12   format(/,5x,'cannot continue',/,5x,
     1            'num = ',i5,2x,'nout = ',i5,/,5x,
     2            'write out results and return')
 13   format(/1x,'no more orthonormal vectors can be added.',/,5x,
     1           'quit and return unconverged results')
 14   format(/,5x,'iteration limit exceeded.',/,5x,
     1            'quit and return to main')
 15   format(///,30x,'final summary',/,17x,'root',22x,'energy')
 16   format(/,25x,'Time Summary for Rsdvd',
     1       /,5x,'Initialization         = ',e15.8,
     2       /,5x,'Small diagonalization  = ',e15.8,
     3       /,5x,'Residual Formation     = ',e15.8,
     4       /,5x,'Vector Generation      = ',e15.8,
     5       /,5x,'Schmidt Process        = ',e15.8,
     6       /,5x,'Matrix Vector Multiply = ',e15.8,
     7       /,5x,'Small Matrix Update    = ',e15.8,
     8       /,5x,'                 Total = ',e15.8)
      end       


