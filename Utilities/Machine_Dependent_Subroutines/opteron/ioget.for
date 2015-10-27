*deck %W%  %G%
      SUBROUTINE ioget(file,DATA,nbytes,fbyte,lbyte)

c
c***begin prologue     ioget
c***date written       870706   (yymmdd)
c***revision date      yymmdd   (yymmdd)
c
c***keywords           i/o READ
c***author             saxe, paul (lanl)
c***source             %W%   %G%
c
c***purpose            to transfer DATA from a disc file
c
c***description        ioget transfers 'nbytes' bytes of DATA from
c     the unit 'file', to 'data' starting at location 'fbyte' on that
c     unit. the first position in the file is considered to be byte 0.
c
c     n.b. this routine is machine dependent, and may restrict the byte
c     addresses and numbers of bytes to word or DOUBLE word boundaries and
c     amounts.
c
c on input:
c     file        INTEGER
c                 the unit number of the file to READ to.
c
c     DATA        INTEGER, REAL or LOGICAL. 'nbytes' long
c                 the DATA area to READ to.
c
c     nbytes      INTEGER
c                 the number of bytes to READ.
c
c     fbyte       INTEGER
c                 the byte location (starting at 0) to READ the DATA to
c                 the file.
c
c***references         
c
c***routines called    (NONE)
c
c***END prologue       ioget
c
c
      IMPLICIT INTEGER(a-z)
c
      USE Set_Mesa_Parameters,       ONLY : buflen
c      PARAMETER (buflen=#buflen)
c **       PARAMETER (itobyt=4)
c
      CHARACTER*4 itoc
      INTEGER DATA(*)
      INTEGER buffer
      LOGICAL PRINT
      INTEGER*4 ierr4, file4, record4
c
      COMMON /ioqprt/ PRINT
      COMMON /ioqbuf/ buffer(buflen)
      COMMON /io/     inp,iout
c
c     ----- PRINT IF requested DATA on tranfer -----
c
      IF (PRINT) THEN
         WRITE (iout,1) file,nbytes,fbyte
 1       FORMAT (' ioget:  file=',i3,' nbytes=',i10,' fbyte=',i10)
      END IF
c
c     ----- determine the first INTEGER word to READ -----
c
      fword=1+fbyte/itobyt(1)
c
c     ----- check for non-integral starting position -----
c
      IF ((fword-1)*itobyt(1).NE.fbyte) THEN
         CALL lnkerr('ioget: non-integral starting position for read')
      END IF
c
c     ----- determine the number of INTEGER words to READ -----
c
      nwords=nbytes/itobyt(1)
c
c     ----- check for non-integral number of words -----
c
      IF (nwords*itobyt(1).NE.nbytes) THEN
         CALL lnkerr('ioget: non-integral number of words to read')
      END IF
c
c     ---- determine which records need to be READ ----
c
      first=(fword-1)/buflen + 1
      lword=fword+nwords-1
      last=(lword-1)/buflen + 1
c
c     ----- loop over the records to be READ -----
c
      DO 1000 record=first,last
c
c        ----- READ the buffer ONTO record 'record' -----
c
         file4 = file
         record4 = record
         READ (unit=file4,rec=record4,iostat=ierr4) buffer
c
         ierr = ierr4

         IF(ierr.NE.0) THEN
            CALL lnkerr('fortran i/o error '//itoc(ierr)//
     $           ' reading in ioget')
         ENDIF
c
         IF (record.EQ.first) THEN
            offset=fword-(record-1)*buflen - 1
            count=buflen-offset
            IF(count.GT.nwords) count=nwords
            DO 20 i=1,count
               DATA(i)=buffer(i+offset)
 20         CONTINUE
         ELSE IF (record.EQ.last) THEN
            count=lword-(record-1)*buflen
            offset=nwords-count
            DO 40 i=1,count
               DATA(i+offset)=buffer(i)
 40         CONTINUE
         ELSE
            offset=buflen*(record-1)-fword+1
            DO 50 i=1,buflen
               DATA(i+offset)=buffer(i)
 50         CONTINUE
         END IF
 1000 CONTINUE
c
c     ----- and increment a sequential POINTER -----
c
      lbyte=fbyte+nbytes
c
c
      RETURN
      END
