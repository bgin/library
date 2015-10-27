*deck spofs
      subroutine spofs (a, lda, n, v, itask, ind, work)
c***begin prologue  spofs
c***purpose  solve a positive definite symmetric system of linear
c            equations.
c***library   slatec
c***category  d2b1b
c***type      single precision (spofs-s, dpofs-d, cpofs-c)
c***keywords  hermitian, linear equations, positive definite, symmetric
c***author  voorhees, e. a., (lanl)
c***description
c
c    subroutine spofs solves a real positive definite symmetric
c    nxn system of single precision linear equations using
c    linpack subroutines spoco and sposl.  that is, if a is an
c    nxn real positive definite symmetric matrix and if x and b
c    are real n-vectors, then spofs solves the equation
c
c                          a*x=b.
c
c    the matrix a is first factored into upper and lower tri-
c    angular matrices r and r-transpose.  these factors are used to
c    find the solution vector x.  an approximate condition number is
c    calculated to provide a rough estimate of the number of
c    digits of accuracy in the computed solution.
c
c    if the equation a*x=b is to be solved for more than one vector
c    b, the factoring of a does not need to be performed again and
c    the option to solve only (itask .gt. 1) will be faster for
c    the succeeding solutions.  in this case, the contents of a,
c    lda, and n must not have been altered by the user following
c    factorization (itask=1).  ind will not be changed by spofs
c    in this case.
c
c  argument description ***
c
c    a      real(lda,n)
c             on entry, the doubly subscripted array with dimension
c               (lda,n) which contains the coefficient matrix.  only
c               the upper triangle, including the diagonal, of the
c               coefficient matrix need be entered and will subse-
c               quently be referenced and changed by the routine.
c             on return, contains in its upper triangle an upper
c               triangular matrix r such that  a = (r-transpose) * r .
c    lda    integer
c             the leading dimension of the array a.  lda must be great-
c             er than or equal to n.  (terminal error message ind=-1)
c    n      integer
c             the order of the matrix a.  n must be greater
c             than or equal to 1.  (terminal error message ind=-2)
c    v      real(n)
c             on entry, the singly subscripted array(vector) of di-
c               mension n which contains the right hand side b of a
c               system of simultaneous linear equations a*x=b.
c             on return, v contains the solution vector, x .
c    itask  integer
c             if itask = 1, the matrix a is factored and then the
c               linear equation is solved.
c             if itask .gt. 1, the equation is solved using the existing
c               factored matrix a.
c             if itask .lt. 1, then terminal error message ind=-3 is
c               printed.
c    ind    integer
c             gt. 0  ind is a rough estimate of the number of digits
c                     of accuracy in the solution, x.
c             lt. 0  see error message corresponding to ind below.
c    work   real(n)
c             a singly subscripted array of dimension at least n.
c
c  error messages printed ***
c
c    ind=-1  terminal   n is greater than lda.
c    ind=-2  terminal   n is less than 1.
c    ind=-3  terminal   itask is less than 1.
c    ind=-4  terminal   the matrix a is computationally singular or
c                         is not positive definite.  a solution
c                         has not been computed.
c    ind=-10 warning    the solution has no apparent significance.
c                         the solution may be inaccurate or the
c                         matrix a may be poorly scaled.
c
c               note-  the above terminal(*fatal*) error messages are
c                      designed to be handled by xermsg in which
c                      level=1 (recoverable) and iflag=2 .  level=0
c                      for warning error messages from xermsg.  unless
c                      the user provides otherwise, an error message
c                      will be printed followed by an abort.
c
c***references  j. j. dongarra, j. r. bunch, c. b. moler, and g. w.
c                 stewart, linpack users' guide, siam, 1979.
c***routines called  r1mach, spoco, sposl, xermsg
c***revision history  (yymmdd)
c   800509  date written
c   890531  changed all specific intrinsics to generic.  (wrb)
c   890831  modified array declarations.  (wrb)
c   890831  revision date from version 3.2
c   891214  prologue converted to version 4.0 format.  (bab)
c   900315  calls to xerror changed to calls to xermsg.  (thj)
c   900510  convert xerrwv calls to xermsg calls.  (rwc)
c   920501  reformatted the references section.  (wrb)
c***end prologue  spofs
c
      integer lda,n,itask,ind,info
      real a(lda,*),v(*),work(*),r1mach
      real rcond
      character*8 xern1, xern2
c***first executable statement  spofs
      if (lda.lt.n) then
         ind = -1
         write (xern1, '(i8)') lda
         write (xern2, '(i8)') n
         call xermsg ('slatec', 'spofs', 'lda = ' // xern1 //
     *      ' is less than n = ' // xern2, -1, 1)
         return
      endif
c
      if (n.le.0) then
         ind = -2
         write (xern1, '(i8)') n
         call xermsg ('slatec', 'spofs', 'n = ' // xern1 //
     *      ' is less than 1', -2, 1)
         return
      endif
c
      if (itask.lt.1) then
         ind = -3
         write (xern1, '(i8)') itask
         call xermsg ('slatec', 'spofs', 'itask = ' // xern1 //
     *      ' is less than 1', -3, 1)
         return
      endif
c
      if (itask.eq.1) then
c
c        factor matrix a into r
c
         call spoco(a,lda,n,rcond,work,info)
c
c        check for positive definite matrix
c
         if (info.ne.0) then
            ind = -4
            call xermsg ('slatec', 'spofs',
     *         'singular or not positive definite - no solution', -4, 1)
            return
         endif
c
c        compute ind (estimate of no. of significant digits)
c        and check for ind greater than zero
c
         ind = -log10(r1mach(4)/rcond)
         if (ind.le.0) then
            ind = -10
            call xermsg ('slatec', 'spofs',
     *         'solution may have no significance', -10, 0)
         endif
      endif
c
c     solve after factoring
c
      call sposl(a,lda,n,v)
      return
      end
