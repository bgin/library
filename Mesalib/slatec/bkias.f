*deck bkias
      subroutine bkias (x, n, ktrms, t, ans, ind, ms, gmrn, h, ierr)
c***begin prologue  bkias
c***subsidiary
c***purpose  subsidiary to bskin
c***library   slatec
c***type      single precision (bkias-s, dbkias-d)
c***author  amos, d. e., (snla)
c***description
c
c     bkias computes repeated integrals of the k0 bessel function
c     by the asymptotic expansion
c
c***see also  bskin
c***routines called  bdiff, gamrn, hkseq, r1mach
c***revision history  (yymmdd)
c   820601  date written
c   890531  changed all specific intrinsics to generic.  (wrb)
c   891214  prologue converted to version 4.0 format.  (bab)
c   900328  added type section.  (wrb)
c   910722  updated author section.  (als)
c***end prologue  bkias
      integer i, ii, ind, j, jmi, jn, k, kk, km, ktrms, mm, mp, ms, n,
     * ierr
      real ans, b, bnd, den1, den2, den3, er, err, fj, fk, fln, fm1,
     * gmrn, g1, gs, h, hn, hrtpi, rat, rg1, rxp, rz, rzx, s, ss, sumi,
     * sumj, t, tol, v, w, x, xp, z
      real gamrn, r1mach
      dimension b(120), xp(16), s(31), h(*), v(52), w(52), t(50),
     * bnd(15)
      save b, bnd, hrtpi
c-----------------------------------------------------------------------
c             coefficients of polynomial p(j-1,x), j=1,15
c-----------------------------------------------------------------------
      data b(1), b(2), b(3), b(4), b(5), b(6), b(7), b(8), b(9), b(10),
     * b(11), b(12), b(13), b(14), b(15), b(16), b(17), b(18), b(19),
     * b(20), b(21), b(22), b(23), b(24) /1.00000000000000000e+00,
     * 1.00000000000000000e+00,-2.00000000000000000e+00,
     * 1.00000000000000000e+00,-8.00000000000000000e+00,
     * 6.00000000000000000e+00,1.00000000000000000e+00,
     * -2.20000000000000000e+01,5.80000000000000000e+01,
     * -2.40000000000000000e+01,1.00000000000000000e+00,
     * -5.20000000000000000e+01,3.28000000000000000e+02,
     * -4.44000000000000000e+02,1.20000000000000000e+02,
     * 1.00000000000000000e+00,-1.14000000000000000e+02,
     * 1.45200000000000000e+03,-4.40000000000000000e+03,
     * 3.70800000000000000e+03,-7.20000000000000000e+02,
     * 1.00000000000000000e+00,-2.40000000000000000e+02,
     * 5.61000000000000000e+03/
      data b(25), b(26), b(27), b(28), b(29), b(30), b(31), b(32),
     * b(33), b(34), b(35), b(36), b(37), b(38), b(39), b(40), b(41),
     * b(42), b(43), b(44), b(45), b(46), b(47), b(48)
     * /-3.21200000000000000e+04,5.81400000000000000e+04,
     * -3.39840000000000000e+04,5.04000000000000000e+03,
     * 1.00000000000000000e+00,-4.94000000000000000e+02,
     * 1.99500000000000000e+04,-1.95800000000000000e+05,
     * 6.44020000000000000e+05,-7.85304000000000000e+05,
     * 3.41136000000000000e+05,-4.03200000000000000e+04,
     * 1.00000000000000000e+00,-1.00400000000000000e+03,
     * 6.72600000000000000e+04,-1.06250000000000000e+06,
     * 5.76550000000000000e+06,-1.24400640000000000e+07,
     * 1.10262960000000000e+07,-3.73392000000000000e+06,
     * 3.62880000000000000e+05,1.00000000000000000e+00,
     * -2.02600000000000000e+03,2.18848000000000000e+05/
      data b(49), b(50), b(51), b(52), b(53), b(54), b(55), b(56),
     * b(57), b(58), b(59), b(60), b(61), b(62), b(63), b(64), b(65),
     * b(66), b(67), b(68), b(69), b(70), b(71), b(72)
     * /-5.32616000000000000e+06,4.47650000000000000e+07,
     * -1.55357384000000000e+08,2.38904904000000000e+08,
     * -1.62186912000000000e+08,4.43390400000000000e+07,
     * -3.62880000000000000e+06,1.00000000000000000e+00,
     * -4.07200000000000000e+03,6.95038000000000000e+05,
     * -2.52439040000000000e+07,3.14369720000000000e+08,
     * -1.64838430400000000e+09,4.00269508800000000e+09,
     * -4.64216395200000000e+09,2.50748121600000000e+09,
     * -5.68356480000000000e+08,3.99168000000000000e+07,
     * 1.00000000000000000e+00,-8.16600000000000000e+03,
     * 2.17062600000000000e+06,-1.14876376000000000e+08,
     * 2.05148277600000000e+09,-1.55489607840000000e+10/
      data b(73), b(74), b(75), b(76), b(77), b(78), b(79), b(80),
     * b(81), b(82), b(83), b(84), b(85), b(86), b(87), b(88), b(89),
     * b(90), b(91), b(92), b(93), b(94), b(95), b(96)
     * /5.60413987840000000e+10,-1.01180433024000000e+11,
     * 9.21997902240000000e+10,-4.07883018240000000e+10,
     * 7.82771904000000000e+09,-4.79001600000000000e+08,
     * 1.00000000000000000e+00,-1.63560000000000000e+04,
     * 6.69969600000000000e+06,-5.07259276000000000e+08,
     * 1.26698177760000000e+10,-1.34323420224000000e+11,
     * 6.87720046384000000e+11,-1.81818864230400000e+12,
     * 2.54986547342400000e+12,-1.88307966182400000e+12,
     * 6.97929436800000000e+11,-1.15336085760000000e+11,
     * 6.22702080000000000e+09,1.00000000000000000e+00,
     * -3.27380000000000000e+04,2.05079880000000000e+07,
     * -2.18982980800000000e+09,7.50160522280000000e+10/
      data b(97), b(98), b(99), b(100), b(101), b(102), b(103), b(104),
     * b(105), b(106), b(107), b(108), b(109), b(110), b(111), b(112),
     * b(113), b(114), b(115), b(116), b(117), b(118)
     * /-1.08467651241600000e+12,7.63483214939200000e+12,
     * -2.82999100661120000e+13,5.74943734645920000e+13,
     * -6.47283751398720000e+13,3.96895780558080000e+13,
     * -1.25509040179200000e+13,1.81099255680000000e+12,
     * -8.71782912000000000e+10,1.00000000000000000e+00,
     * -6.55040000000000000e+04,6.24078900000000000e+07,
     * -9.29252692000000000e+09,4.29826006340000000e+11,
     * -8.30844432796800000e+12,7.83913848313120000e+13,
     * -3.94365587815520000e+14,1.11174747256968000e+15,
     * -1.79717122069056000e+15,1.66642448627145600e+15,
     * -8.65023253219584000e+14,2.36908271543040000e+14/
      data b(119), b(120) /-3.01963769856000000e+13,
     * 1.30767436800000000e+12/
c-----------------------------------------------------------------------
c             bounds b(m,k) , k=m-3
c-----------------------------------------------------------------------
      data bnd(1), bnd(2), bnd(3), bnd(4), bnd(5), bnd(6), bnd(7),
     * bnd(8), bnd(9), bnd(10), bnd(11), bnd(12), bnd(13), bnd(14),
     * bnd(15) /1.0e0,1.0e0,1.0e0,1.0e0,3.10e0,5.18e0,11.7e0,29.8e0,
     * 90.4e0,297.0e0,1070.0e0,4290.0e0,18100.0e0,84700.0e0,408000.0e0/
      data hrtpi /8.86226925452758014e-01/
c
c***first executable statement  bkias
      ierr=0
      tol = max(r1mach(4),1.0e-18)
      fln = n
      rz = 1.0e0/(x+fln)
      rzx = x*rz
      z = 0.5e0*(x+fln)
      if (ind.gt.1) go to 10
      gmrn = gamrn(z)
   10 continue
      gs = hrtpi*gmrn
      g1 = gs + gs
      rg1 = 1.0e0/g1
      gmrn = (rz+rz)/gmrn
      if (ind.gt.1) go to 70
c-----------------------------------------------------------------------
c     evaluate error for m=ms
c-----------------------------------------------------------------------
      hn = 0.5e0*fln
      den2 = ktrms + ktrms + n
      den3 = den2 - 2.0e0
      den1 = x + den2
      err = rg1*(x+x)/(den1-1.0e0)
      if (n.eq.0) go to 20
      rat = 1.0e0/(fln*fln)
   20 continue
      if (ktrms.eq.0) go to 30
      fj = ktrms
      rat = 0.25e0/(hrtpi*den3*sqrt(fj))
   30 continue
      err = err*rat
      fj = -3.0e0
      do 50 j=1,15
        if (j.le.5) err = err/den1
        fm1 = max(1.0e0,fj)
        fj = fj + 1.0e0
        er = bnd(j)*err
        if (ktrms.eq.0) go to 40
        er = er/fm1
        if (er.lt.tol) go to 60
        if (j.ge.5) err = err/den3
        go to 50
   40   continue
        er = er*(1.0e0+hn/fm1)
        if (er.lt.tol) go to 60
        if (j.ge.5) err = err/fln
   50 continue
      go to 200
   60 continue
      ms = j
   70 continue
      mm = ms + ms
      mp = mm + 1
c-----------------------------------------------------------------------
c     h(k)=(-z)**(k)*(psi(k-1,z)-psi(k-1,z+0.5))/gamma(k) , k=1,2,...,mm
c-----------------------------------------------------------------------
      if (ind.gt.1) go to 80
      call hkseq(z, mm, h, ierr)
      go to 100
   80 continue
      rat = z/(z-0.5e0)
      rxp = rat
      do 90 i=1,mm
        h(i) = rxp*(1.0e0-h(i))
        rxp = rxp*rat
   90 continue
  100 continue
c-----------------------------------------------------------------------
c     scaled s sequence
c-----------------------------------------------------------------------
      s(1) = 1.0e0
      fk = 1.0e0
      do 120 k=2,mp
        ss = 0.0e0
        km = k - 1
        i = km
        do 110 j=1,km
          ss = ss + s(j)*h(i)
          i = i - 1
  110   continue
        s(k) = ss/fk
        fk = fk + 1.0e0
  120 continue
c-----------------------------------------------------------------------
c     scaled s-tilda sequence
c-----------------------------------------------------------------------
      if (ktrms.eq.0) go to 160
      fk = 0.0e0
      ss = 0.0e0
      rg1 = rg1/z
      do 130 k=1,ktrms
        v(k) = z/(z+fk)
        w(k) = t(k)*v(k)
        ss = ss + w(k)
        fk = fk + 1.0e0
  130 continue
      s(1) = s(1) - ss*rg1
      do 150 i=2,mp
        ss = 0.0e0
        do 140 k=1,ktrms
          w(k) = w(k)*v(k)
          ss = ss + w(k)
  140   continue
        s(i) = s(i) - ss*rg1
  150 continue
  160 continue
c-----------------------------------------------------------------------
c     sum on j
c-----------------------------------------------------------------------
      sumj = 0.0e0
      jn = 1
      rxp = 1.0e0
      xp(1) = 1.0e0
      do 190 j=1,ms
        jn = jn + j - 1
        xp(j+1) = xp(j)*rzx
        rxp = rxp*rz
c-----------------------------------------------------------------------
c     sum on i
c-----------------------------------------------------------------------
        sumi = 0.0e0
        ii = jn
        do 180 i=1,j
          jmi = j - i + 1
          kk = j + i + 1
          do 170 k=1,jmi
            v(k) = s(kk)*xp(k)
            kk = kk + 1
  170     continue
          call bdiff(jmi, v)
          sumi = sumi + b(ii)*v(jmi)*xp(i+1)
          ii = ii + 1
  180   continue
        sumj = sumj + sumi*rxp
  190 continue
      ans = gs*(s(1)-sumj)
      return
  200 continue
      ierr=2
      return
      end
