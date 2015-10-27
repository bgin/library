*deck psgf
      function psgf (x, iz, c, a, bh)
c***begin prologue  psgf
c***subsidiary
c***purpose  subsidiary to blktri
c***library   slatec
c***type      single precision (psgf-s)
c***author  (unknown)
c***see also  blktri
c***routines called  (none)
c***revision history  (yymmdd)
c   801001  date written
c   891214  prologue converted to version 4.0 format.  (bab)
c   900402  added type section.  (wrb)
c***end prologue  psgf
      dimension       a(*)       ,c(*)       ,bh(*)
c***first executable statement  psgf
      fsg = 1.
      hsg = 1.
      do 101 j=1,iz
         dd = 1./(x-bh(j))
         fsg = fsg*a(j)*dd
         hsg = hsg*c(j)*dd
  101 continue
      if (mod(iz,2)) 103,102,103
  102 psgf = 1.-fsg-hsg
      return
  103 psgf = 1.+fsg+hsg
      return
      end
