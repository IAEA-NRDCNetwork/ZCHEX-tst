      PROGRAM ORDER
!**
!* Written by Victoria McLane
!*            National Nuclear Data Center
!*            Brookhaven National Laboratory
!*            Upton, NY 11973-5000
!*
!* Version 2001-1 (April 2001)
!*   Updated to handle q heading.
!* Version 98-1 (February 1998)
!*   Updated to use 4-digit year.
!* Version 90-1 (June 1990)
!* Version 90-1a Corrected for Alpha word boundary alignment warnings
!*                 (TWB. 930824)
!*    Converted to VAX September 1987
!**
!* Adds record identification and bookkeeping information to a file
!*   in the EXFOR format.  If date is blank adds today's date; if 2-digit
!*   year is given, adds '19'.
!**
!* Link with CSISRS$PROG:UTILTY_SUB/LIBRARY (DATER,PUT1)
!**

      COMMON/CNTROL/INDEX,NUM1,NUM2

      COMMON/IDENTS/NSAN,ISEQ,NAN
        CHARACTER*5 NAN
        CHARACTER*5 NAN00

      COMMON/UNITS/IN,IOUT,ITMP,TNAM
        CHARACTER*400 TNAM

      CHARACTER*66 KARD,IBUF/' '/
      CHARACTER*11 TODAY
      CHARACTER*10 ENDLABEL/'END       '/
      CHARACTER IALT,IALTB/' '/
      CHARACTER*400 input_file

      COMMON/Records/iLineTot

      INTEGER DATE

      DATA NAN00/'10000'/
      DATA NAN,NSAN,ISEQ/'10000',0,0/         ! Initialize record ID
      DATA NENT/0/                            ! Initialize # entries

!   5 FORMAT(/' ORDER (Version 2010-04-08) run on ',A11/1X,41(1H-))
!   5 FORMAT(/' ORDER (Version 2019-07-19) run on ',A11/1X,41(1H-))
!   5 FORMAT(/' ORDER (Version 2023-05-18) run on ',A11/1X,41(1H-))
    5 FORMAT(/' ORDER (Version 2023-05-29) run on ',A11/1X,41(1H-))
    8 FORMAT(' Input file labeled'/2X,A33/)
    9 FORMAT(' Output file labeled'/2X,A33/)
   10 FORMAT(I8)
!  12 FORMAT(1X,'ORDER: ',A5,I6,'  Ln:',I8)
   12 FORMAT(1X,'ORDER:',I6,') ',A5,' ',A8,' ',I9)

c-zvv
c	write (*,*) 'iargc()=',iargc()
	input_file='a.x4'
	if (iargc().gt.0) call getarg(1,input_file)
	icmd=iargc() ! default taken from command line
	iEntry=0

!---- Store date, write terminal heading.
      CALL DATER(TODAY,DATE)
      WRITE (*,5) TODAY
      WRITE(IBUF(26:33),10) DATE

      CALL TAPES(input_file,icmd)             ! Open files
      CALL CONTIN(1,KARD,IALT,*9900)          ! Read file LABEL

      IF (INDEX.NE.1) THEN                    ! LABEL not found
        IBUF(1:10) = 'TRANS     '             ! Set LABEL for output
      ELSE
        IBUF(1:22) = KARD(1:22)               ! Store LABEL and File ID
        WRITE (*,8) KARD(1:33)                ! Type input file LABEL 
      END IF

      WRITE (*,9) IBUF(1:33)                  ! Type output file LABEL
c-zvv      CALL PUT1(IOUT,IBUF,NAN,NSAN,ISEQ,IALTB)! Write file LABEL on output

      ENDLABEL(4:10) = IBUF(1:7)              ! Set end label expected

      IF (INDEX.NE.1) GOTO 112

  105 CALL CONTIN(2,KARD,IALT,*2200)          ! Read ENTRY record
      IF(INDEX.EQ.7) GOTO 2200                ! Check for END LABEL

  112 NENT = NENT+1                           ! Increase entry count
      NSUB = 0                                ! Initialize # of subentries
      ISEQ = 1                                ! Initialize sequence #

      WRITE (*,12) NENT,NAN,KARD(26:33),iLineTot          ! List entry being processed

      IF (KARD(26:33).EQ.' ') THEN            ! Date is blank
        WRITE(KARD(26:33),10) DATE              ! Insert today's date
      ELSE IF (KARD(26:27).EQ.' ') THEN       ! 2-digit year
        KARD(26:27) = '19'
      END IF
      
!      WRITE (*,*) '[',KARD,']'
!      WRITE (*,12) NENT,NAN,KARD(26:33),iLineTot          ! List entry being processed
	if (iEntry.eq.0) then
	    NAN00(1:1)=NAN(1:1)
	    CALL PUT1(IOUT,IBUF,NAN00,0,0,' ')! Write file LABEL on output
	endif
	iEntry=iEntry+1
      CALL PUT1(IOUT,KARD,NAN,NSAN,ISEQ,IALT) ! Write record on output

      CALL CONTIN(3,KARD,IALT,*9900)          ! Read SUBENT record
 1000 NSUB = NSUB+1                           ! Increase subentry count
      ISEQ = 1                                ! Set sequence # to 1

      IF (KARD(26:33).EQ.' ') THEN            ! Date is blank
        WRITE(KARD(26:33),10) DATE              ! Insert today's date
      ELSE IF (KARD(26:27).EQ.' ') THEN       ! 2-digit year
        KARD(26:27) = '19'
      END IF
      
      CALL PUT1(IOUT,KARD,NAN,NSAN,ISEQ,IALT)  ! Write SUBENT record

      IF(INDEX.EQ.15) GOTO 2010                ! Branch of NOSUBENT

      CALL CONTIN(4,KARD,IALT,*9900)           ! Read BIB ID
      ISEQ = ISEQ+1                            ! Increment sequence #
      NUMBER = 0                               ! Initialize BIB record count
      IKEY = 0                                 ! Initialize # of keywords
      IF(INDEX.EQ.16) GOTO 1290                ! Branch if NOBIB
      CALL TMPFIL(1)
      CALL PUTTMP(KARD,IALT)                   ! Write BIB ID on temp file

 1210 CALL CONTIN(10,KARD,IALT,*9900)          ! Read BIB record
      IF(INDEX.EQ.10) GOTO 1240                ! Branch if ENDBIB 
      NUMBER = NUMBER+1                        ! Increase BIB record count
      CALL PUTTMP(KARD,IALT)                   ! Write record on temp file
      IF(KARD(1:10).NE.' ') IKEY = IKEY+1      ! Increment keyword count
      GOTO 1210

 1240 CALL TMPFIL(2)                        ! Close and reset temp file to input

!---- Copy BIB ID with correct counts
      CALL GETTMP(IBUF,IALTB)
      WRITE(IBUF(15:22),10) IKEY
      WRITE(IBUF(26:33),10) NUMBER
      CALL PUT1(IOUT,IBUF,NAN,NSAN,ISEQ,IALTB)

!---- Copy rest of BIB section onto output
      DO 1250 I = 1,NUMBER
        CALL GETTMP(IBUF,IALTB)
        ISEQ = ISEQ + 1
        CALL PUT1(IOUT,IBUF,NAN,NSAN,ISEQ,IALTB)
 1250 CONTINUE
      CALL TMPFIL(3)

!---- Write ENDBIB or NOBIB record on output.
 1290 ISEQ = ISEQ + 1
      WRITE(KARD(15:22),10) NUMBER
      KARD(28:33) = '     0'
      CALL PUT1(IOUT,KARD,NAN,NSAN,ISEQ,IALT)

      CALL CONTIN(5,KARD,IALT,*9900)          ! Read COMMON or NOCOMMON record
      CALL DOCOM(KARD,IALT)                   ! Process COMMON section

      IF(NSAN.EQ.1) GOTO 1700                 ! If subentry 1, skip DATA section

      CALL CONTIN(6,KARD,IALT,*9900)          ! Read DATA or NODATA record
      CALL DOCOM(KARD,IALT)                   ! Process DATA section

 1700 CALL CONTIN(9,KARD,IALT,*9900)          ! Read ENDSUBENT record
      NUM1 = ISEQ-1
      WRITE(KARD(15:22),10) NUM1
      KARD(28:33) = '     0'
      ISEQ = 99999
      CALL PUT1(IOUT,KARD,NAN,NSAN,ISEQ,IALT) ! Write ENDSUBENT record

 2010 CALL CONTIN(3,KARD,IALT,*9900)          ! Read SUBENT or ENDENTRY record

      IF (INDEX.EQ.8) THEN                    ! ENDENTRY
        WRITE(KARD(15:22),10) NSUB              ! Insert # of subentries
        KARD(28:33) = '     0'
        NSAN = 999
        ISEQ = 99999
        CALL PUT1(IOUT,KARD,NAN,NSAN,ISEQ,IALT) ! Write ENDENTRY record
        GOTO 105
      ELSE
        GOTO 1000
      END IF

!     END label

 2200 KARD(1:10) = ENDLABEL(1:10)
      WRITE(KARD(15:22),10) NENT              ! Insert # of entries 
      KARD(28:33) = '     0'
      KARD(34:66) = ' '
      NAN = 'Z9999'
      CALL PUT1(IOUT,KARD,NAN,NSAN,ISEQ,IALT) ! Write END label

 9900 CALL TMPFIL(3)                          ! Close files
c-zvv      CALL EXIT
	WRITE(*,*)
	WRITE(*,*) 'PROGRAM COMPLETED SUCCESSFULLY'
	stop
      END
      SUBROUTINE CONTIN(IKEY,RECORD,ALT,*)

!*  Reads control record
!*    IKEY: index of record expected (I)
!*    RECORD: record read (A66)
!*    ALT: alteration flag (A1)
!*    RETURN 1: end-of-file found

      COMMON/CNTROL/INDEX,NUM1,NUM2

      COMMON/IDENTS/ISAN,ISEQ,INAN
        CHARACTER*5 INAN

      COMMON/SYSID/NSYS,KS(23),KEYWD(23)
        CHARACTER*10 KEYWD

!---- I/O Unit numbers. (Assigned in SETUP).
      COMMON/UNITS/IN,IDUM(2),TNAM
        CHARACTER*400 TNAM

      CHARACTER ALT
      CHARACTER*66 RECORD
      CHARACTER*255 tmpRECORD
      character*255 tab2space


      COMMON/Records/iLineTot,Rec1,Rec2,Rec3,Rec4
      CHARACTER*66 Rec1/' '/,Rec2/' '/,Rec3/' '/,Rec4/' '/
      DATA iLineTot/0/


      DATA NSYS/23/

      DATA KEYWD/'TRANS     ','LIB       ','ARCHIVE   ',
     *           'REQUEST   ','ENTRY     ','SUBENT    ',
     *           'BIB       ','COMMON    ','DATA      ',
     *           'ENDTRANS  ','ENDLIB    ','ENDARCHIVE',
     *           'ENDREQUEST','ENDENTRY  ','ENDSUBENT ',
     *           'ENDBIB    ','ENDCOMMON ','ENDDATA   ',
     *           'NOENTRY   ','NOSUBENT  ','NOBIB     ',
     *           'NOCOMMON  ','NODATA    '/

      DATA KS/4*1,2,3,4,5,6,4*7,8,9,10,11,12,14,15,16,17,18/

      READ(IN,1000,END=800,ERR=901) RECORD,ALT ! Read control record
	Rec4=Rec3
	Rec3=Rec2
	Rec2=Rec1
	Rec1=RECORD
	iLineTot=iLineTot+1
	tmpRECORD=tab2space(RECORD)
!2023	READ(tmpRECORD,1000) RECORD,ALT        ! Read control record
	RECORD=tmpRECORD
!      WRITE (*,*) '[',RECORD,']'

!      if (RECORD.eq.' ') READ(IN,1000,END=800) RECORD,ALT  !...zv20190717
!      if (RECORD.eq.' ') READ(IN,1000,END=800) RECORD,ALT  !...zv20190717

      II = LOOKUP(RECORD(1:10),KEYWD,23)      ! Check if System ID

      IF (II.EQ.0) THEN                       ! Not System ID
        INDEX = 0
        GOTO 900
      END IF

      INDEX = KS(II)

      IF (INDEX.EQ.2 .OR. INDEX.EQ.14) THEN      ! ENTRY or NOENTRY
        INAN = RECORD(18:22)                       ! Form accession #
        ISAN = 0                                   ! Initialize subaccession #

      ELSE IF (INDEX.EQ.3 .OR. INDEX.EQ.15) THEN ! SUBENT or NOSUBENT
        READ(RECORD(20:22),2000) ISAN              ! form subaccession #

      ELSE IF (INDEX.GE.4 .AND. INDEX.LE.6) THEN ! BIB, COMMON, or DATA
        READ(RECORD(12:22),2000,ERR=900) NUM1
        READ(RECORD(23:33),2000,ERR=900) NUM2
      END IF

!---- Check validity of System ID found

      IF(IKEY.GE.7 .AND. IKEY.LE.12) GOTO 700

      IF (IKEY.EQ.1) THEN                        ! FILE LABEL EXPECTED
        IF(INDEX.EQ.1) RETURN
        IF(INDEX.NE.2 .AND. INDEX.NE.14) GOTO 900
        RETURN
      ELSE IF (IKEY.EQ.2) THEN                   ! ENTRY expected
        IF(INDEX.EQ.7) RETURN                      ! Check for END label
      ELSE IF (IKEY.EQ.3) THEN                   ! SUBENT expected
        IF(INDEX.EQ.8) RETURN                      ! Check for ENDENTRY
      END IF

!---- ENTRY, SUBENT, BIB, COMMON, or DATA expected. Check also for NO----.
      IF(INDEX.EQ.IKEY .OR. INDEX.EQ.IKEY+12) RETURN
      GOTO 900

!---- END----. check for ENDENTRY, ENDSUBENT.
  700 IF(IKEY.GE.10) RETURN
      IF(INDEX.NE.IKEY) GOTO 900
      RETURN

!---- End-of-file
  800 IF (IKEY.EQ.1) THEN                        ! TRANS expected; empty file.
        WRITE (*,8000)
      ELSE IF (IKEY.NE.2) THEN                   ! Not at beginning of new ENTRY
        GOTO 900
      END IF
      RETURN 1

!---- Input file format error, terminate.
  900 IF(IKEY.GE.10) RETURN
  901 WRITE (*,9000) IKEY
      WRITE (*,*)
      WRITE (*,9001) INAN,ISAN,ISEQ,iLineTot
      WRITE (*,*) 'Last lines:'
      WRITE (*,*) '-4:[',Rec4,']'
      WRITE (*,*) '-3:[',Rec3,']'
      WRITE (*,*) '-2:[',Rec2,']'
      WRITE (*,*) '-1:[',Rec1,']'
      if (Rec1.ne.RECORD) WRITE (*,*) ' 0:[',RECORD,']'
c      CALL EXIT
	stop

 1000 FORMAT(A66,13X,A1)
 2000 FORMAT(I11)
 8000 FORMAT(' **** Empty file'/)
 9000 FORMAT(' **** Input file format error - execution terminated ',I8)
 9001 FORMAT('Location: SUBENT ',A5,I3.3,I5,'   Line:',I8)
      END
      SUBROUTINE DOCOM(KARD,IALT)

!*  Reads COMMON and DATA sections
!*    KARD: Section header
!*    IALT: Alteration flag

      COMMON/CNTROL/INDEX,NUM1,NUM2

      COMMON/IDENTS/NSAN,ISEQ,NAN
        CHARACTER*5 NAN

      COMMON/UNITS/IN,IOUT,ITMP,TNAM
        CHARACTER*400 TNAM

      CHARACTER*66 KARD,IBUF
      CHARACTER IALT,IALTB, TESTLET

   10 FORMAT(I11)
   20 FORMAT(E11.6)
   31 FORMAT('  More than 18 data fields in subentry ',I3)

      NUMBER = 0
      NHEAD = 0
      NCOL = 0
      ID = INDEX

      IF(ID.EQ.17 .OR. ID.EQ.18) GOTO 5000
      IDX = ID+6
      NCRD = 0
      CALL TMPFIL(1)                      ! Open temporary output file

!---- COPY HEADINGS AND UNITS
 1000 CALL PUTTMP(KARD,IALT)              ! Write record on temporary file
      NCRD = NCRD+1                       ! Increment # of records in section.

      CALL CONTIN(IDX,KARD,IALT,*11111)          ! Read next input record
11111      IF(IDX.EQ.INDEX) GOTO 5000

      TESTLET = KARD(1:1)
      CALL UPSTR(TESTLET)
      IF (TESTLET.GE.'A'.AND. TESTLET.LE.'Z') GOTO 1050

      READ(KARD(1:11),20,ERR=1050) EFMT   ! Check for legal number
      GOTO 2100

!---- Count # of fields
 1050 NUMBER = NUMBER+1
      DO 1100 I = 1,66,11
        IF(KARD(I:I+10).EQ.' ') GOTO 1000
        NCOL = NCOL+1
 1100 CONTINUE
      GOTO 1000

!---- Calculate # of header records, # of data fields.
 2100 NCOL = NCOL/2
      NHEAD = ((NCOL-1)/6)+1
      IF(NCOL.GT.18) WRITE (*,31) NSAN

!---- Read and store records until ENDCOMMON or ENDDATA
 2200 NUMBER = NUMBER + 1
      NCRD = NCRD + 1
      CALL PUTTMP(KARD,IALT)
      CALL CONTIN(IDX,KARD,IALT,*11112)     ! Read next input record
11112      IF(IDX.EQ.INDEX) GOTO 2300     ! Test for End of Section ID
      GOTO 2200

 2300 CALL TMPFIL(2)                 ! Close temporary file
      CALL GETTMP(IBUF,IALTB)        ! Reopen as input file

      IF (ID.EQ.5) THEN              ! Set counts for COMMON ID
        NUM2 = NUMBER
      ELSE                           ! Set counts for DATA ID
        NUM2 = (NUMBER-(NHEAD*2))/NHEAD
      END IF

      WRITE(IBUF(12:22),10) NCOL
      WRITE(IBUF(23:33),10) NUM2
      ISEQ = ISEQ+1
      CALL PUT1(IOUT,IBUF,NAN,NSAN,ISEQ,IALTB) ! Write DATA or COMMON ID

!---- Copy rest of dummy file onto output.
      IF(NCRD.EQ.0) GOTO 5000
      DO 1680 I = 2,NCRD
        CALL GETTMP(IBUF,IALTB)
        ISEQ = ISEQ + 1
        CALL PUT1(IOUT,IBUF,NAN,NSAN,ISEQ,IALTB)
 1680 CONTINUE
      CALL TMPFIL(3)

!---- Write ENDDATA or ENDCOMMON ID
 5000 ISEQ = ISEQ + 1
      WRITE(KARD(12:22),10) NUMBER
      KARD(23:33) = '          0'
      CALL PUT1(IOUT,KARD,NAN,NSAN,ISEQ,IALT)
      RETURN
      END
      SUBROUTINE GETTMP(KARD,IALT)

!*  Reads record from temp file
!*    KARD: Input record
!*    IALT: Alteration flag

      COMMON/UNITS/IN,IOUT,ITMP,TNAM
        CHARACTER*400 TNAM

      CHARACTER*66 KARD
      CHARACTER IALT

   10 FORMAT(A66,A1)

      READ(ITMP,10) KARD,IALT            ! Write record
      RETURN
      END
      SUBROUTINE PUTTMP(KARD,IALT)

!*  Writes record on temporary file
!*    KARD: Input record

      COMMON/UNITS/IN,IOUT,ITMP,TNAM
        CHARACTER*400 TNAM

      CHARACTER*66 KARD
      CHARACTER IALT

   10 FORMAT(A66,A1)

      WRITE(ITMP,10) KARD,IALT           ! write record

      RETURN
      END



      SUBROUTINE TAPES(input_file,icmd)

!*  Set file specs; opens files.

      COMMON/UNITS/IN,IOUT,ITMP,TNAM
        CHARACTER*400 TNAM

      CHARACTER*400 input_file
      CHARACTER*400 output_file
      CHARACTER*400 ISTR 

C-zvv DATA TNAM/'TMP:______.TMP'/       ! Temporary data file name
      DATA TNAM/'TMP_______.TMP'/       ! Temporary data file name

      DATA IN,IOUT,ITMP/21,22,23/       ! Unit numbers

   10 FORMAT(/$,' Input file specs------> ')
   20 FORMAT(A400)
   30 FORMAT(/$,' Output file specs-----> ')
   90 FORMAT(/' *** Input file not found'/
     *       $,'     Enter new file or <cr> to exit--> ')

c-tst      write (*,*) 'input file LL=',len(input_file)
	output_file='a.ord'
	if (icmd.ne.0) then
	    lll=mylen(input_file)
c??	    lll=len(trim(input_file))
	    output_file=input_file(1:lll)
	    output_file(lll+1:lll+4)='.ord'
	endif

!---- Open input file 
      ISTR=' '
      write (*,*) 'Default input file: ',input_file(1:mylen(input_file))
c??      write (*,*) 'Default input file: ',trim(input_file)
      WRITE (*,10)
      if (icmd.eq.0)   READ (*,20) ISTR
      if (ISTR.ne.' ') input_file=ISTR
      write (*,*) 'Input file: ',input_file(1:mylen(input_file))
c??      write (*,*) 'Input file: ',trim(input_file)
      ISTR=input_file
  150 OPEN(UNIT=IN, FILE=ISTR, ACCESS='SEQUENTIAL',STATUS='OLD',
C-zvv     *     READONLY,ERR=900)
     *     ERR=900)

!---- Open output file 
      ISTR=' '
      write (*,*) 'Default output file: '
     1,output_file(1:mylen(output_file))
c??      write (*,*) 'Default output file: ',trim(output_file)
      WRITE (*,30)
      if (icmd.eq.0)   READ (*,20) ISTR
      if (ISTR.ne.' ') output_file=ISTR
      write (*,*) 'Output file: ',output_file(1:mylen(output_file))
c??      write (*,*) 'Output file: ',trim(output_file)
      ISTR=output_file
C-zvv      OPEN(UNIT=IOUT, FILE=ISTR, ACCESS='SEQUENTIAL',STATUS='NEW',
      OPEN(UNIT=IOUT, FILE=ISTR, ACCESS='SEQUENTIAL',STATUS='REPLACE'
C-zvv     *     ,CARRIAGECONTROL='LIST')
     *     )
c	write (*,*) '---OK:ISTR=',ISTR

	TNAM=ISTR
	lll=mylen(TNAM)
c??	lll=len(trim(TNAM))
	if (lll.GE.400) lll=400-1
	TNAM(lll+1:lll+1)='_'
	write (*,*) '---OK:TNAM=',TNAM(1:mylen(TNAM))
      RETURN

!---- Input file not found
c  900 WRITE (*,90)
  900 continue
	if (icmd.ne.0) then
	write (*,*) ' *** Input file not found ***'
	stop
	endif
      WRITE (*,90)
      READ (*,20) ISTR
      IF(ISTR.NE.' ') GOTO 150
      STOP
      END



      SUBROUTINE TMPFIL(IWAY)

!*  Opens and closes temporary file as specified.
!*    IWAY = 1: open temporary output file
!*           2: close and reopen temporary output file
!*           3: close and delete temporary input file

      COMMON/UNITS/INOUT(2),ITMP,TNAM
        CHARACTER*400 TNAM

      IF (IWAY.EQ.1) THEN                   ! Open new temporary file.
        OPEN(UNIT=ITMP, FILE=TNAM, ACCESS='SEQUENTIAL', 
     *       STATUS='REPLACE')
C-zvv     *       STATUS='NEW')

      ELSE IF (IWAY.EQ.2) THEN              ! Close and reopen temporary file.
        CLOSE(UNIT=ITMP)
        OPEN(UNIT=ITMP, FILE=TNAM, ACCESS='SEQUENTIAL',
     *       STATUS='OLD')

      ELSE IF (IWAY.EQ.3) THEN              ! Close and delete temporary file.
c-zvv        CLOSE(UNIT=ITMP, DISPOSE='DELETE')
        CLOSE(UNIT=ITMP, STATUS='DELETE')
      END IF
      RETURN
      END
