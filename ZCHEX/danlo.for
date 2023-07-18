      PROGRAM DANLO
c
c This program based on DAN_LOAD_NEW
c Platform independent version
c written by Viktor Zerkin, June 2001
c		IAEA, Nuclear Data Section
c		Wagramer Strasse 5,
c		P.O.Box 100, A-1400,
c		Vienna, Austria
c
c Platform independent version:
c	- DICT_NEW.TOP	- sequential file
c	- DICT_NEW.###	- direct access files
c	- checked on Linux/g77 and VMS/DEC-FORTRAN
C**
C* Version 2000-1 (May 2000)
C*   Changed length of code to 30 characters.
C* Version 98-1 (December 1998)
C*   Updated for dictionaries with multiple keys
C* Version 94-1 (December 1994)
C*   Changed format for backup file
C* Version 91-2 (June 1991)
C*   Converted DANLOD to 3-digit dictionary #
C**
C* Written by Victoria McLane
C*            National Nuclear Data Center
C*            Brookhaven National Laboratory
C*            Upton, NY 11973
C**
C* Link using DANIEL.LOD
C**
C* Uses subroutine library DANIEL_NEW (DANERROR)
C*                         UTILY_SUB (INCRMT)
C**
C* Creates an indexed file for the EXFOR dictionaries from input file
C* in DAN_BACK format.
C**
C* Input file:
C*   dictionary index format: 
C*        Dictionary #, name, # of keys, format, # of records
C*           (1X,A3,A30,1X,I2,1X,A44,I4)
C*
C*   dictionary format: 
C*        Dictionary #, status, date, key(s), data
C*           (A3,1X,A3,1X,I6,1X,A30,1X,A80)
C**
C* Output files:
C*
C*   DICT_NEW.TOP format: (record length 80)
C*     Dictionary lock    (I1)  (initialized to 0)
C*     Dictionary #       (A3)
C*     Dictionary name    (A30)
C*     # of keys/record   (I1)
C*     # of fields/record (I2)
C*     Field formats      (A40) 10 fields max.
C*     Field formats      (A44) 11 fields max. !!!---zvv25.08.2005
C*     # of records       (I4)
C*
C*   DICT_NEW.nnn format: (record length 120)
C*     Status                       (A3)
C*     Date of entry or last update (I6) (yymmdd)
C*     Primary key                  (A30)
C*     Dictionary fields            (A80)
C*     Record tag (for updates)     (A1)
C**
      COMMON/UNITS/IDIC

      CHARACTER TAG/' '/
      CHARACTER*3 DNUM(1000),STATUS
      CHARACTER*8 KFMT(1000)
      CHARACTER*11 TODAY
      CHARACTER*30 KEY
      CHARACTER*30 NAME
      CHARACTER*50 FMAT
      CHARACTER*50 ISTR/'DANBACK.ALL'/
      CHARACTER*50 strTmp
      CHARACTER*80 REC

      CHARACTER*30 FNAME/'DICT_ZVV.    '/

      DIMENSION NKW(1000)

      dimension IKtop(1000,3,2)	! key location in char line
      dimension NFLDtop(1000)	! number of fields in # dict
      dimension NRECtop(1000)	! number of records in # dict
      dimension NUMtop(1000)	! flag: # dict in control list
      dimension FMATtop(1000)	! format of record in # dict
      dimension NAMEtop(1000)	! name of # dict
      character*50 FMATtop
      character*30 NAMEtop
      data NUMtop/1000*0/
      data IDICwtop/23/
      data IKtop/6000*0/

      DATA LOCK/0/
      DATA IN,IDIC/21,22/

 1000 FORMAT(/,' Welcome to DANLO ',/,' Load dictionaries for CHEX'/)
    5 FORMAT($,' Input file specs [',A11,']----> ')
    6 FORMAT(A50)
    9 FORMAT(/' DAN_LOAD_NEW (Version 2000-1) - RUN ON ',A11/1X,
     *       35(' ')/)
   10 FORMAT(I1,I3,A30,I1,I2,A40,I4)
   20 FORMAT(I3,1X,A30,1X,I2,1X,A44,1X,I4)
   25 FORMAT(A3,I6,A30,A80,A1)
   45 FORMAT(I3,1X,A3,1X,I6,1X,A30,1X,A80)
   90 FORMAT(' ** Error opening ',A50)

c?      CALL DATE_20(TODAY)
c?      TYPE 9,TODAY

C---  Open sequential input file
      write (*,1000)
      write (*,5) ISTR
      read (*,6) strTmp
      if (strTmp.ne.' ') ISTR=strTmp
      write (*,*) ' Process file: ',ISTR
      write (*,*)

      OPEN (UNIT=IN,FILE=ISTR,STATUS='OLD',ACCESS='SEQUENTIAL',ERR=800)

C---  Open control list (DICT_LIB.TOP)
      OPEN (UNIT=IDICwtop, FILE='DICT_ZVV.TOP',
     * STATUS='unknown', ACCESS='SEQUENTIAL', FORM='FORMATTED', ERR=805)

C*  Process dictionary control list

      write (*,*) ' Creating DICT_ZVV.TOP'

  110 READ(IN,20) NUM,NAME,NKEY,FMAT,NREC   ! Read record from input
      IF(NUM.EQ.0) GOTO 200                 ! Check for end of list
      WRITE(DNUM(NUM),FMT='(I3)') NUM       ! Set integer dictionary #
      NFLD = 0                              ! Initialize # of fields
c      write (*,*) 'FMAT=[',FMAT,']'
      DO 115 K = 1,44,4                     ! Calculate field length
        IF(FMAT(K:K).EQ.' ') GOTO 120
        NFLD = NFLD+1                       ! Increment # of fields
  115 CONTINUE

  120 NKW(NUM) = NKEY                       ! Store # of keys
c      write (*,*) 'FMAT=[',FMAT,'] NFLD=',NFLD
      KFMT(NUM) = FMAT(5:12)                ! Format for up to 3 keys
c?    WRITE(IDIC,10) LOCK,NUM,NAME,NKEY,NFLD,FMAT,NREC
      NUMtop(NUM)=1
      NFLDtop(NUM)=NFLD
      NRECtop(NUM)=NREC
      NAMEtop(NUM)=NAME
      FMATtop(NUM)=FMAT
      IKtop(NUM,1,1)=10
      IKtop(NUM,1,2)=39
      IKtop(NUM,2,1)=0
      IKtop(NUM,2,2)=0
      IKtop(NUM,3,1)=0
      IKtop(NUM,3,2)=0
      if (NKEY.GT.1) then
          READ(KFMT(NUM)(2:3),FMT='(I2)') LKEY ! Store length of second key
          K2 = 39 + LKEY
          IKtop(NUM,2,1)=40
          IKtop(NUM,2,2)=K2
      end if
      if (NKEY.GT.2) then
          READ(KFMT(NUM)(6:7),FMT='(I2)') LKEY ! Store length of second key
          K3 = K2 + LKEY
          IKtop(NUM,3,1)=K2+1
          IKtop(NUM,3,2)=K3
      end if
      GOTO 110


C*  Process dictionaries to be updated

  200 CLOSE(IDIC)                           ! Close control list
      LAST = 0                              ! Initialize last dictionary read

  310 READ (IN,45,END=900) NUM,STATUS,INDAY,KEY,REC ! Read next record
      IF(NUM.EQ.LAST) GOTO 400              ! Branch if number same as last

C---- New dictionary

      IF (LAST.NE.0) THEN
        CLOSE(IDIC)                         ! Close previous file
c?      CALL TOPUPD(LAST,DNUM(LAST),NREC,*900) ! Update control file
c?      READ(IDIC,10,KEY=DNUM,ERR=805) IFLAG,DNUM,NAME,KNUM,NFLD,FMAT,NREC
      WRITE(IDICwtop,1010,ERR=900) DNUM(LAST),NAMEtop(LAST),NKW(LAST)
     * , NFLDtop(LAST),FMATtop(LAST),NREC
     * , ((IKtop(LAST,ii,jj),jj=1,2),ii=1,3)
 1010 FORMAT(1x,A3,A30,I1,I2,A48,I4,6i3)
      END IF

  350 WRITE(FNAME(10:12),FMT='(I3.3)') NUM  ! Convert number to character string

      if (NUMtop(NUM).eq.0) then
          write (*,*) NUM,'dictionary is not in TOP'
          write (*,*) '---INTERRUPED---'
      end if
      write (*,*) ' Creating ',FNAME,' ',NAMEtop(NUM)
      OPEN (UNIT=IDIC, FILE=FNAME, STATUS='unknown',
     *      ACCESS='direct', FORM='FORMATTED',
     *      RECL=120, IOSTAT=IOS, ERR=805)
c?      IF (NKW(NUM).EQ.1) THEN
c?	write(*,*) 'open:',num,' keys:',nkw(num),10,39
c?        OPEN (UNIT=IDIC, FILE=FNAME, STATUS='NEW',
c?     *        ORGANIZATION='INDEXED', ACCESS='KEYED', RECL=120,
c?     *        FORM='FORMATTED', KEY=(10:39:CHARACTER),
c?     *        RECORDTYPE='FIXED', IOSTAT=IOS, ERR=805)
c?      ELSE IF (NKW(NUM).EQ.2) THEN
c?        READ(KFMT(NUM)(2:3),FMT='(I2)') LKEY ! Store length of second key
c?        K2 = 39 + LKEY
c?	write(*,*) 'open:',num,' keys:',nkw(num),10,39,k2
c?        OPEN (UNIT=IDIC, FILE=FNAME, STATUS='NEW',
c?     *        ORGANIZATION='INDEXED', ACCESS='KEYED', RECL=120,
c?     *        FORM='FORMATTED', 
c?     *        KEY=(10:39:CHARACTER,40:K2:CHARACTER),
c?     *        RECORDTYPE='FIXED', IOSTAT=IOS, ERR=805)
c?      ELSE IF (NKW(NUM).EQ.3) THEN
c?        READ(KFMT(NUM)(2:3),FMT='(I2)') LKEY ! Store length of second key
c?        K2 = 39 + LKEY
c?        READ(KFMT(NUM)(6:7),FMT='(I2)') LKEY ! Store length of second key
c?        K3 = K2 + LKEY
c?	write(*,*) 'open:',num,' keys:',nkw(num),10,39,k2,k3
c?        OPEN (UNIT=IDIC, FILE=FNAME, STATUS='NEW',
c?     *        ORGANIZATION='INDEXED', ACCESS='KEYED', RECL=120,
c?     *        FORM='FORMATTED', 
c?     *        KEY=(10:39:CHARACTER,40:K2:CHARACTER,K2+1:K3:CHARACTER),
c?     *        RECORDTYPE='FIXED', IOSTAT=IOS, ERR=805)
c?      END IF

      NREC = 0                         ! INITIALIZE # OF RECORDS IN DICTIONARY
      LAST = NUM                       ! STORE CURRENT DICTIONARY NUMBER

c?400 WRITE(IDIC,25) STATUS,INDAY,KEY,REC,TAG ! WRITE RECORD ON DICTIONARY
c-vms  400 WRITE(IDIC'NREC+1,25) STATUS,INDAY,KEY,REC,TAG ! WRITE RECORD ON DICTIONARY
  400 continue
      if (num.eq.227) then !+++2023-07-18:added by V.Zerkin for new dict227
c	write(*,*)'-0-',num,NREC,'[',KEY,']'
	iimeta=index(KEY,'-G ')
	if (iimeta.gt.0) then
c	  write(*,*)'-1-',num,NREC,'[',KEY,']',KEY(iimeta:iimeta+2),']'
	  KEY(iimeta:iimeta+2)=' '
	else
	  iimeta=index(KEY,'-M1 ')
	  if (iimeta.le.0) iimeta=index(KEY,'-M2 ')
	  if (iimeta.le.0) iimeta=index(KEY,'-M3 ')
	  if (iimeta.le.0) iimeta=index(KEY,'-M4 ')
	  if (iimeta.gt.0) then
c	    write(*,*)'-2-',num,NREC,'[',KEY,']',KEY(iimeta:iimeta+3),']'
	    KEY(iimeta+2:iimeta+2)=' '
	  endif
	endif
c	write(*,*)'-3-',num,NREC,'[',KEY,']'
      endif !---2023-07-18:added by V.Zerkin for new dict227
      WRITE(IDIC,rec=NREC+1,fmt=25) 
     * STATUS,INDAY,KEY,REC,TAG ! WRITE RECORD ON DICTIONARY
      NREC = NREC+1                    ! INCREMENT # OF RECORDS IN DICTIONARY
      
      GOTO 310

C---  ERROR 
  800 write (*,90) ISTR
      STOP
  805 write (*,*) ' ** Error opening DICTOP.DAT'
      STOP
c  850 write (*,*) ' ** Too many dictionaries, execution terminated'
c      CALL EXIT

  900 CLOSE(IDIC)
c?      CALL TOPUPD(NUM,DNUM(LAST),NREC,*900) ! Update control file
c?      READ(IDIC,10,KEY=DNUM,ERR=805) IFLAG,DNUM,NAME,KNUM,NFLD,FMAT,NREC
      WRITE(IDICwtop,1010,ERR=900) DNUM(LAST),NAMEtop(LAST),NKW(LAST)
     * , NFLDtop(LAST),FMATtop(LAST),NREC
     * , ((IKtop(LAST,ii,jj),jj=1,2),ii=1,3)
      STOP
      END
c
      SUBROUTINE DANERROR(NDIC,N)

C*  Prints error messages
C*    NDIC: dictionary number
C*    N: message #

      CHARACTER*35 MESS(20)/
     * 'dictionary control file error      ', ! #1
     * 'illegal dictionary number          ', ! #2
     * 'control index record locked        ', ! #3
     * 'dictionary record locked           ', ! #4
     * 'illegal field #                    ', ! #5
     * 'error opening dictionary           ', ! #6
     * 'error reading dictionary record    ', ! #7
     * 'error opening DICT_LIB.TOP         ', ! #8
     * 'error reading DICT_LIB.TOP         ', ! #9
     * 'error writing DICT_LIB.TOP         ', ! #10
     * 'error in dictionary key            ', ! #11
     * 'illegal dictionary update mode     ', ! #12
     * 'illegal key number                 ', ! #13
     * 'error writing dictionary record    ', ! #14
     * 'error deleting dictionary record   ', ! #15
     * 'illegal record retrieval option    ', ! #16
     * 'error rewriting dictionary record  ', ! #17
     * '                                   ', ! #18
     * '                                   ', ! #19
     * 'key not found                      '/ ! #20

      IF (NDIC.EQ.0) THEN
        write (*,2000) MESS(N),NDIC
      ELSE
        write (*,1000) MESS(N),NDIC
      END IF
      RETURN

 1000 FORMAT(' **** ',A35,' for dictionary ',I3)
 2000 FORMAT(' **** ',A35)
      END
