 PROGRAM EXAMPLE 
  USE LA_PRECISION, ONLY: WP => DP
  USE f77_LAPACK, ONLY: F77GESV => LA_GESV
  USE f95_LAPACK, ONLY: F95GESV => LA_GESV

  USE f77_LAPACK, ONLY: F77GESDD => LA_GESDD
  USE f95_LAPACK, ONLY: F95GESDD => LA_GESDD

  IMPLICIT NONE 

  INTEGER :: INFO, J, LDA, LDB, N, NRHS, LWORK, S, I
  INTEGER, ALLOCATABLE :: IPIV(:)
  INTEGER, ALLOCATABLE :: IWORK(:)
  REAL :: T0, T1, T2
  DOUBLE PRECISION  ::  CPU_MFLP, RTC_MFLP, FLP, p,r
  DOUBLE PRECISION :: RTC, BEFORE, AFTER, RTC_TIME, CPU1_TIME
  COMPLEX(WP), ALLOCATABLE :: A(:,:), B(:,:), AA(:,:), BB(:,:)
  REAL(WP), ALLOCATABLE :: WR(:), RWORK(:)
  COMPLEX(WP), ALLOCATABLE :: VL(:,:), VR(:,:)
  COMPLEX(WP), ALLOCATABLE :: WORK(:)
  print *, 'N = '
  read (*,*) N

  NRHS = 1; S = 10
  print * ,'=========================================================='
  print * ,'      LAPACK95 TIMING routine, Double Complex Version     '
  print * ,'=========================================================='
  
  print *,' N , NRHS = ', N, NRHS
  ALLOCATE( A(N,N), B(N,NRHS), IPIV(N), AA(N,N), BB(N,NRHS) )
  ALLOCATE(WR(N), VL(N,N), VR(N,N), RWORK(5*N*N+6*N))
  ALLOCATE(IWORK(10*N))

  do i=1,n
    do j=1,n
      call random_number(p)
      call random_number(r)
      a(i,j) = cmplx( p, r)
    enddo
  enddo
!  CALL RANDOM_NUMBER(A)
  DO J = 1, NRHS
    B(:,J) = SUM( A, DIM=2 )*J
  ENDDO
  AA = A; BB = B
  LDA = N; LDB = N 

! DGESV
  CPU1_TIME = 0; RTC_TIME = 0; CPU_MFLP = 0; RTC_MFLP = 0;
  DO I = 1, S
    A = AA; B = BB
    CALL CPU_TIME(T0); CALL CPU_TIME(T1); T0 = T1-T0
    CALL F77GESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
    CALL CPU_TIME(T2)
    CPU1_TIME = CPU1_TIME + (T2-T1-T0)
    IF (INFO .NE. 0) THEN
      PRINT *,'INFO = ', INFO
      EXIT
    ENDIF
    A = AA; B = BB
    before = rtc()
    CALL F77GESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
    after = rtc()
    RTC_TIME = RTC_TIME + (after - before)
    IF (INFO .NE. 0) THEN
      PRINT *,'INFO = ', INFO
      EXIT
    ENDIF
  ENDDO
  CPU1_TIME = CPU1_TIME/S
  RTC_TIME = RTC_TIME/S
  FLP = 0.67*(DBLE(N)*(N*DBLE(N)))
  IF (CPU1_TIME .NE.0)THEN; CPU_MFLP = (FLP /CPU1_TIME)/1E6; ELSE; CPU_MFLP = 0
  ENDIF
  IF (RTC_TIME .NE.0)THEN; RTC_MFLP = (FLP /RTC_TIME)/1E6; ELSE; RTC_MFLP = 0
  ENDIF
  WRITE(*,*) 'CPUTIME and MFLOPS of L77GESV ', CPU1_TIME, CPU_MFLP
  WRITE(*,*) 'RTCTIME and MFLOPS of L77GESV ', RTC_TIME, RTC_MFLP

  CPU1_TIME = 0; RTC_TIME = 0; CPU_MFLP = 0; RTC_MFLP = 0;
  DO i = 1, s
    A = AA; B = BB
    CALL CPU_TIME(T1);
    CALL F95GESV( A, B ); CALL CPU_TIME(T2)
    CPU1_TIME = CPU1_TIME + (T2-T1-T0)
    
    A = AA; B = BB
    before = rtc()
    CALL F95GESV( A, B ); CALL CPU_TIME(T2)
    after = rtc()
    RTC_TIME = RTC_TIME+(after - before)
  ENDDO
  CPU1_TIME = CPU1_TIME/S
  RTC_TIME = RTC_TIME/S
  FLP = 0.67*(DBLE(N)*(N*DBLE(N)))
  IF (CPU1_TIME .NE.0)THEN; CPU_MFLP = (FLP /CPU1_TIME)/1E6; ELSE; CPU_MFLP = 0
  ENDIF
  IF (RTC_TIME .NE.0)THEN; RTC_MFLP = (FLP /RTC_TIME)/1E6; ELSE; RTC_MFLP = 0
  ENDIF
  WRITE(*,*) 'CPUTIME and MFLOPS of L95GESV ',  CPU1_TIME, CPU_MFLP
  WRITE(*,*) 'RTCTIME and MFLOPS of L95GESV ',  RTC_TIME, RTC_MFLP

! DGESDD, singular values & left and right singular vectors
   print *,'DGESDD, singular values & left and right singular vectors'
   CPU1_TIME = 0; RTC_TIME = 0; CPU_MFLP = 0; RTC_MFLP = 0;
  DO I = 1, S
    A = AA;  LWORK =10*N*N + 20*N
    ALLOCATE(WORK(LWORK))
    CALL CPU_TIME(T1)
    CALL F77GESDD( 'S', N, N, A, LDA, WR, VL, LDA, VR, LDA, WORK, &
&     LWORK, RWORK, IWORK, INFO )
    CALL CPU_TIME(T2)
    DEALLOCATE(WORK)
    CPU1_TIME = CPU1_TIME + ( T2-T1-T0 )
    IF (INFO .NE. 0) THEN
      PRINT *,'INFO = ', INFO
      EXIT
    ENDIF
    A = AA;  LWORK =10*N*N + 20*N
    ALLOCATE(WORK(LWORK))
    BEFORE = RTC()
    CALL F77GESDD( 'S', N, N, A, LDA, WR, VL, LDA, VR, LDA, WORK, &
&     LWORK, RWORK, IWORK, INFO )
    AFTER = RTC()
    DEALLOCATE(WORK)
    RTC_TIME=RTC_TIME + (AFTER - BEFORE)
    IF (INFO .NE. 0) THEN
      PRINT *,'INFO = ', INFO
      EXIT
    ENDIF
  ENDDO
  CPU1_TIME = CPU1_TIME/S
  RTC_TIME = RTC_TIME/S
  FLP = 6.67*(DBLE(N)*(N*DBLE(N)))
  IF (CPU1_TIME .NE.0)THEN; CPU_MFLP = (FLP /CPU1_TIME)/1E6; ELSE; CPU_MFLP = 0
  ENDIF
  IF (RTC_TIME .NE.0)THEN; RTC_MFLP = (FLP /RTC_TIME)/1E6; ELSE; RTC_MFLP = 0
  ENDIF
  WRITE(*,*) 'CPUTIME and MFLOPS of L77GESDD ', CPU1_TIME, CPU_MFLP
  WRITE(*,*) 'RTCTIME and MFLOPS of L77GESDD ', RTC_TIME, RTC_MFLP
  CPU1_TIME = 0; RTC_TIME = 0; CPU_MFLP = 0; RTC_MFLP = 0;
  DO I=1,S
    A = AA
    CALL CPU_TIME(T1)
    CALL F95GESDD( A, WR, VL, VR, JOB='N', INFO=INFO)
    CALL CPU_TIME(T2)
    CPU1_TIME = CPU1_TIME + ( T2-T1-T0 )
    IF (INFO .NE. 0) THEN
      PRINT *,'INFO = ', INFO
      EXIT
    ENDIF
    A = AA
    BEFORE = RTC()
    CALL F95GESDD( A, WR, VL, VR, JOB='N', INFO=INFO)
    AFTER = RTC()
    RTC_TIME=RTC_TIME + (AFTER - BEFORE)
    IF (INFO .NE. 0) THEN
      PRINT *,'INFO = ', INFO
      EXIT
    ENDIF
  ENDDO
  CPU1_TIME = CPU1_TIME/S
  RTC_TIME = RTC_TIME/S
  FLP = 6.67*(DBLE(N)*(N*DBLE(N)))
  IF (CPU1_TIME .NE.0)THEN; CPU_MFLP = (FLP /CPU1_TIME)/1E6; ELSE; CPU_MFLP = 0
  ENDIF
  IF (RTC_TIME .NE.0)THEN; RTC_MFLP = (FLP /RTC_TIME)/1E6; ELSE; RTC_MFLP = 0
  ENDIF
  WRITE(*,*) 'CPUTIME and MFLOPS of L95GESDD ', CPU1_TIME, CPU_MFLP
  WRITE(*,*) 'RTCTIME and MFLOPS of L95GESDD ', RTC_TIME, RTC_MFLP
  
  DEALLOCATE( A, B, IPIV, AA, BB )
  DEALLOCATE(WR, VL, VR, RWORK)
  DEALLOCATE(IWORK)
  
END PROGRAM EXAMPLE
  
