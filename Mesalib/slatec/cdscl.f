*deck cdscl
      subroutine cdscl (hmax, n, nq, rmax, h, rc, rh, yh)
c***begin prologue  cdscl
c***subsidiary
c***purpose  subroutine cdscl rescales the yh array whenever the step
c            size is changed.
c***library   slatec (sdrive)
c***type      complex (sdscl-s, ddscl-d, cdscl-c)
c***author  kahaner, d. k., (nist)
c             national institute of standards and technology
c             gaithersburg, md  20899
c           sutherland, c. d., (lanl)
c             mail stop d466
c             los alamos national laboratory
c             los alamos, nm  87545
c***routines called  (none)
c***revision history  (yymmdd)
c   790601  date written
c   900329  initial submission to slatec.
c***end prologue  cdscl
      integer i, j, n, nq
      complex yh(n,*)
      real h, hmax, rc, rh, rmax, r1
c***first executable statement  cdscl
      if (h .lt. 1.e0) then
        rh = min(abs(h)*rh, abs(h)*rmax, hmax)/abs(h)
      else
        rh = min(rh, rmax, hmax/abs(h))
      end if
      r1 = 1.e0
      do 10 j = 1,nq
        r1 = r1*rh
        do 10 i = 1,n
 10       yh(i,j+1) = yh(i,j+1)*r1
      h = h*rh
      rc = rc*rh
      return
      end