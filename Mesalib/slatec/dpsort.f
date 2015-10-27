*deck dpsort
      subroutine dpsort (dx, n, iperm, kflag, ier)
c***begin prologue  dpsort
c***purpose  return the permutation vector generated by sorting a given
c            array and, optionally, rearrange the elements of the array.
c            the array may be sorted in increasing or decreasing order.
c            a slightly modified quicksort algorithm is used.
c***library   slatec
c***category  n6a1b, n6a2b
c***type      double precision (spsort-s, dpsort-d, ipsort-i, hpsort-h)
c***keywords  number sorting, passive sorting, singleton quicksort, sort
c***author  jones, r. e., (snla)
c           rhoads, g. s., (nbs)
c           wisniewski, j. a., (snla)
c***description
c
c   dpsort returns the permutation vector iperm generated by sorting
c   the array dx and, optionally, rearranges the values in dx.  dx may
c   be sorted in increasing or decreasing order.  a slightly modified
c   quicksort algorithm is used.
c
c   iperm is such that dx(iperm(i)) is the ith value in the
c   rearrangement of dx.  iperm may be applied to another array by
c   calling ipperm, spperm, dpperm or hpperm.
c
c   the main difference between dpsort and its active sorting equivalent
c   dsort is that the data are referenced indirectly rather than
c   directly.  therefore, dpsort should require approximately twice as
c   long to execute as dsort.  however, dpsort is more general.
c
c   description of parameters
c      dx - input/output -- double precision array of values to be
c           sorted.  if abs(kflag) = 2, then the values in dx will be
c           rearranged on output; otherwise, they are unchanged.
c      n  - input -- number of values in array dx to be sorted.
c      iperm - output -- permutation array such that iperm(i) is the
c              index of the value in the original order of the
c              dx array that is in the ith location in the sorted
c              order.
c      kflag - input -- control parameter:
c            =  2  means return the permutation vector resulting from
c                  sorting dx in increasing order and sort dx also.
c            =  1  means return the permutation vector resulting from
c                  sorting dx in increasing order and do not sort dx.
c            = -1  means return the permutation vector resulting from
c                  sorting dx in decreasing order and do not sort dx.
c            = -2  means return the permutation vector resulting from
c                  sorting dx in decreasing order and sort dx also.
c      ier - output -- error indicator:
c          =  0  if no error,
c          =  1  if n is zero or negative,
c          =  2  if kflag is not 2, 1, -1, or -2.
c***references  r. c. singleton, algorithm 347, an efficient algorithm
c                 for sorting with minimal storage, communications of
c                 the acm, 12, 3 (1969), pp. 185-187.
c***routines called  xermsg
c***revision history  (yymmdd)
c   761101  date written
c   761118  modified by john a. wisniewski to use the singleton
c           quicksort algorithm.
c   870423  modified by gregory s. rhoads for passive sorting with the
c           option for the rearrangement of the original data.
c   890619  double precision version of spsort created by d. w. lozier.
c   890620  algorithm for rearranging the data vector corrected by r.
c           boisvert.
c   890622  prologue upgraded to version 4.0 style by d. lozier.
c   891128  error when kflag.lt.0 and n=1 corrected by r. boisvert.
c   920507  modified by m. mcclain to revise prologue text.
c   920818  declarations section rebuilt and code restructured to use
c           if-then-else-endif.  (smr, wrb)
c***end prologue  dpsort
c     .. scalar arguments ..
      integer ier, kflag, n
c     .. array arguments ..
      double precision dx(*)
      integer iperm(*)
c     .. local scalars ..
      double precision r, temp
      integer i, ij, indx, indx0, istrt, j, k, kk, l, lm, lmt, m, nn
c     .. local arrays ..
      integer il(21), iu(21)
c     .. external subroutines ..
      external xermsg
c     .. intrinsic functions ..
      intrinsic abs, int
c***first executable statement  dpsort
      ier = 0
      nn = n
      if (nn .lt. 1) then
         ier = 1
         call xermsg ('slatec', 'dpsort',
     +    'the number of values to be sorted, n, is not positive.',
     +    ier, 1)
         return
      endif
c
      kk = abs(kflag)
      if (kk.ne.1 .and. kk.ne.2) then
         ier = 2
         call xermsg ('slatec', 'dpsort',
     +    'the sort control parameter, kflag, is not 2, 1, -1, or -2.',
     +    ier, 1)
         return
      endif
c
c     initialize permutation vector
c
      do 10 i=1,nn
         iperm(i) = i
   10 continue
c
c     return if only one value is to be sorted
c
      if (nn .eq. 1) return
c
c     alter array dx to get decreasing order if needed
c
      if (kflag .le. -1) then
         do 20 i=1,nn
            dx(i) = -dx(i)
   20    continue
      endif
c
c     sort dx only
c
      m = 1
      i = 1
      j = nn
      r = .375d0
c
   30 if (i .eq. j) go to 80
      if (r .le. 0.5898437d0) then
         r = r+3.90625d-2
      else
         r = r-0.21875d0
      endif
c
   40 k = i
c
c     select a central element of the array and save it in location l
c
      ij = i + int((j-i)*r)
      lm = iperm(ij)
c
c     if first element of array is greater than lm, interchange with lm
c
      if (dx(iperm(i)) .gt. dx(lm)) then
         iperm(ij) = iperm(i)
         iperm(i) = lm
         lm = iperm(ij)
      endif
      l = j
c
c     if last element of array is less than lm, interchange with lm
c
      if (dx(iperm(j)) .lt. dx(lm)) then
         iperm(ij) = iperm(j)
         iperm(j) = lm
         lm = iperm(ij)
c
c        if first element of array is greater than lm, interchange
c        with lm
c
         if (dx(iperm(i)) .gt. dx(lm)) then
            iperm(ij) = iperm(i)
            iperm(i) = lm
            lm = iperm(ij)
         endif
      endif
      go to 60
   50 lmt = iperm(l)
      iperm(l) = iperm(k)
      iperm(k) = lmt
c
c     find an element in the second half of the array which is smaller
c     than lm
c
   60 l = l-1
      if (dx(iperm(l)) .gt. dx(lm)) go to 60
c
c     find an element in the first half of the array which is greater
c     than lm
c
   70 k = k+1
      if (dx(iperm(k)) .lt. dx(lm)) go to 70
c
c     interchange these elements
c
      if (k .le. l) go to 50
c
c     save upper and lower subscripts of the array yet to be sorted
c
      if (l-i .gt. j-k) then
         il(m) = i
         iu(m) = l
         i = k
         m = m+1
      else
         il(m) = k
         iu(m) = j
         j = l
         m = m+1
      endif
      go to 90
c
c     begin again on another portion of the unsorted array
c
   80 m = m-1
      if (m .eq. 0) go to 120
      i = il(m)
      j = iu(m)
c
   90 if (j-i .ge. 1) go to 40
      if (i .eq. 1) go to 30
      i = i-1
c
  100 i = i+1
      if (i .eq. j) go to 80
      lm = iperm(i+1)
      if (dx(iperm(i)) .le. dx(lm)) go to 100
      k = i
c
  110 iperm(k+1) = iperm(k)
      k = k-1
      if (dx(lm) .lt. dx(iperm(k))) go to 110
      iperm(k+1) = lm
      go to 100
c
c     clean up
c
  120 if (kflag .le. -1) then
         do 130 i=1,nn
            dx(i) = -dx(i)
  130    continue
      endif
c
c     rearrange the values of dx if desired
c
      if (kk .eq. 2) then
c
c        use the iperm vector as a flag.
c        if iperm(i) < 0, then the i-th value is in correct location
c
         do 150 istrt=1,nn
            if (iperm(istrt) .ge. 0) then
               indx = istrt
               indx0 = indx
               temp = dx(istrt)
  140          if (iperm(indx) .gt. 0) then
                  dx(indx) = dx(iperm(indx))
                  indx0 = indx
                  iperm(indx) = -iperm(indx)
                  indx = abs(iperm(indx))
                  go to 140
               endif
               dx(indx0) = temp
            endif
  150    continue
c
c        revert the signs of the iperm values
c
         do 160 i=1,nn
            iperm(i) = -iperm(i)
  160    continue
c
      endif
c
      return
      end
