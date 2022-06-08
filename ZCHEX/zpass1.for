      SUBROUTINE PASS1
	save !zvv:2019-07-18
C**
C* Written by Victoria McLane
C*            National Nuclear Data Center
C*            Brookhaven National Laboratory
C*            Upton, NY 11973
C**
C* Version 2000-1 (June 2000)
C*   Made duplicate keyword in SAN non-fatal error 
C* Version 99-3 (November 1999)  
C*   Added legal character @
C* Version 99-2 (July 1999)  
C*   Corrected index error in LOCATE
C* Version 99-1 (January 1999)  
C*   Added 5 characters to list of legal characters.
C* Version 98-1 (May 1998)  NEW CODE
C*   CHEX rewritten as multipass code.
C**
C* Pass 1 checks
C*   sequence and validity of System Identifiers
C*   validity of BIB keywords 
C*   validity of data headings and units
C*   legal data formats
C*   legal characters 
C*   legal alteration codes
C*   transmission mode also checks:
C*     N1 and N2 field on System Id records.
C*     Record ID (Columns 67-79)
C**
C* Uses DANIEL data base on logical DANIEL$LIBRARY
C**
C* Load with:
C*   CSISRS$PROG:UTILTY_SUB (FILLIN,INTFORM,LOOKIN,LOOKUP)
C**
C* List of subroutines:
C*
C*   ALTER_FLAG: Checks alteration code.
C*   AN_CHECK: Sets accession number and checks. Initializes flags for ENTRY.
C*   BIB1: Processes BIB section.
C*   CHAR_CHECK: Checks for legal characters.
C*   DATA1: Processes the DATA section
C*   DATE_CHECK: Checks date in format YYMMDD.
C*   FLOAT_CHECK: Checks DATA fields for legal floating point numbers.
C*      Stores numbers.
C*   HEAD_CHECK: Checks for legal titles or units, and for embedded blanks.  
C*      Stores family codes for data headings.  Checks for required data 
C*      headings.
C*   LOCATE: Tries to relocate when expected System ID has not been read.
C*   READ_NEXT: Reads record from input file
C*        IWAY: 1  =  Checks for duplicate records and skips
C*              2  =  Does not check for duplicate records
C*   SEQ_CHECK: Checks AN, SAN, Sequence # in columns 67-79 against expected
C*       for transmission mode
C*   SYS_CHECK: Look up code in table of System Identifiers
C*   SYS_LOCATE: Check System ID code against code expected.
C*   SYSGET: Searches the input file until a System Identier found
C**

C---- Input record. (Set in READ_NEXT)
      COMMON/ALLCOM/KEY2,IPTR,KARD,ISEQ,IALT
        CHARACTER IPTR,IALT
        CHARACTER*13 ISEQ
        CHARACTER*10 KEY2
        CHARACTER*55 KARD

C---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

C---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ISANS/INTAN,ISECT

C---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC

C---- Record counts for ENTRY. (Set in READ_NEXT).
      COMMON/RECCNT/NCNTS(3),NSUB

      CHARACTER*10 MESS

      DATA ILAB,ISECND,IMSNG /3*0/
	data INTSAN/0/

C     File LABEL processing

      CALL READ_NEXT(1,ID,*998)                ! Read first record
      CALL SYS_LOCATE(ID,1,IMSNG,ISECND,*220,*290) ! Check for LABEL
!	write (*,*) '____ID=',ID
      IF(ID.GT.2.AND.ID.LT.18) GOTO 280

  220 ILAB = ID                                ! Store Label ID
      WRITE(IOUT,1100) KEY2,IPTR,KARD(1:22)    ! Write file label 
      GOTO 300

C---- Invalid LABEL, check if record is ENTRY ID.
  280 IF(ID.EQ.3) CALL ERR_REC(KEY2,IPTR,KARD,ISEQ,IALT,1)
      GOTO 305

C---- Illegal LABEL, print message.
  290 CALL ERR_REC(KEY2,IPTR,KARD,ISEQ,IALT,1)

C     ENTRY record processing

  300 LOCI = 3
      CALL READ_NEXT(1,ID,*990)                ! Read next record
  305 CALL SYS_LOCATE(ID,3,IMSNG,ISECND,*390,*350) 
                                               ! Check for ENTRY ID

C---- System ID - not ENTRY.
  310 INTAN = 0                                ! Set AN to 0
      CALL LOCATE(3,IDLST,ID,ISECND,*300,*490,*580,*490) ! Try to relocate
      GOTO 400

C---- Not legal System ID.
  350 CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,13) ! Error message
      GOTO 400

C---- ENTRY or NOSUBENT found
  390 IF (ID.EQ.4) THEN                        ! NOENTRY
        CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,13) ! Error message
        GOTO 300                                 ! Branch for NOENTRY
      END IF

C     SUBENT record processing

  400 LOCI = 6
      CALL READ_NEXT(1,ID,*990)                ! Read next record
      CALL SYS_LOCATE(ID,6,IMSNG,ISECND,*490,*480) 
                                               ! Check for SUBENT or NOSUBENT ID
C---- System ID, not SUBENT or NOSUBENT
      INTSAN = INTSAN+1                        ! Increment SAN
      CALL LOCATE(6,IDLST,ID,ISECND,*400,*580,*680,*980) ! Try to relocate
      GOTO 500

C---- Not System ID
  480 INTSAN = INTSAN+1                        ! Increment SAN
      CALL SAN_CHECK(ACNUM,INTAN,-INTSAN)      ! Initialize SAN (AN_CHECK entry)
      CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,1)
      GOTO 500

C---- SUBENT or NOSUBENT found
  490 NSUB = NSUB+1
      IF(ID.EQ.7) GOTO 900                     ! Branch for NOSUBENT

C     BIB Section processing

  500 LOCI = 9                                 ! Save section ID
  510 CALL READ_NEXT(1,ID,*990)                ! Read next record
      CALL SYS_LOCATE(ID,9,IMSNG,ISECND,*580,*570)
                                               ! Check for BIB or NOBIB ID

C---- System ID, not BIB or NOBIB
      CALL LOCATE(9,IDLST,ID,ISECND,*500,*680,*780,*980) ! Try to relocate
      GOTO 580

C---- Not System ID
  570 CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,1) ! Error message 
      GOTO 510

  580 IF (ID.EQ.10) GOTO 600                   ! Branch for NOBIB
      CALL BIB1(ISECND,IMSNG,ID,*600,*999)     ! Initialize & process BIB 

C---- No ENDBIB. Check for COMMON or NOCOMMON.
      CALL LOCATE(11,IDLST,ID,ISECND,*600,*680,*695,*980)

C     COMMON section processing

C---- Read next record, check for COMMON or NOCOMMON record.
  600 LOCI = 12                                ! Save section ID
  610 CALL READ_NEXT(1,ID,*990)                ! Read next record
      CALL SYS_LOCATE(ID,12,IMSNG,ISECND,*680,*650)
                                               ! Check for COMMON or NOCOMMON ID
C---- ID found, but not COMMON or NOCOMMON
      CALL LOCATE(12,IDLST,ID,ISECND,*600,*695,*830,*980) ! Try to relocate
      GOTO 681

C---- Record not System ID
  650 CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,1) ! Error message 
      GOTO 610

C---- COMMON or NOCOMMON
  680 IF(ID.EQ.13) GOTO 700                    ! Branch for NOCOMMON
  681 CALL DATA1(1,ID,IMSNG,ISECND,*700)       ! Process COMMON section

C---- ENDCOMMON not read. Check for DATA, NODATA, or ENDSUBENT.
      CALL LOCATE(14,IDLST,ID,ISECND,*700,*695,*830,*980)
      GOTO 700

C---- DATA, NODATA or ENDSUBENT read
  695 IF (INTSAN.EQ.1) THEN                    ! Subentry 1
        IF(ID.NE.8) GOTO 980                     ! ENDSUBENT found
      ELSE                                     ! Data subentry
        IF(ID.NE.8) GOTO 780                     ! DATA or NODATA found
        CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,3) 
                                                 ! ENDSUBENT found error message
      END IF
      GOTO 900

  700 IF(INTSAN.EQ.1) GOTO 800                 ! Subentry 1

C     DATA section processing

      LOCI = 15                                ! Store System ID
  710 CALL READ_NEXT(1,ID,*990)                ! Read next record
      CALL SYS_LOCATE(ID,15,IMSNG,ISECND,*780,*750)
                                               ! Check for DATA or NODATA ID
C---- Not DATA or NODATA
      CALL LOCATE(15,IDLST,ID,ISECND,*700,*900,*830,*980) ! Try to relocate
      GOTO 780

C---- Not System ID
  750 CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,1) ! Error message 
      GOTO 710

  780 IF(ID.EQ.16) GOTO 800                    ! Branch for NODATA
  781 CALL DATA1(2,ID,IMSNG,ISECND,*800)       ! Process DATA section

C---- ENDDATA not read. 
      CALL LOCATE(17,IDLST,ID,ISECND,*800,*900,*830,*980) ! Try to relocate

C     ENDSUBENT processing

  800 LOCI = 8
      CALL READ_NEXT(1,ID,*990)                ! Read next record
      CALL SYS_LOCATE(ID,8,IMSNG,ISECND,*900,*850)
                                               ! Check for ENDSUBENT
C---- Not ENDSUBENT
      CALL LOCATE(8,IDLST,ID,ISECND,*800,*830,*840,*986) ! Try to relocate
      GOTO 900                         

  830 IF(ID.EQ.5) GOTO 950                     ! Check if ENDENTRY
      GOTO 490

  840 IF(ID.EQ.3 .OR. ID.EQ.4) GOTO 390        ! ENTRY or NOENTRY
      GOTO 500                                 ! Should be BIB or NOBIB

C---- Not System ID.
  850 CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,1) ! Error message
      
  900 LOCI = 6                                 ! Set section ID to SUBENT
      CALL READ_NEXT(1,ID,*990)                ! Read next record
      CALL SYS_LOCATE(ID,6,IMSNG,ISECND,*490,*480)
                                            ! Check for SUBENT/NOSUBENT/ENDENTRY

      IF (ID.NE.5) THEN                        ! Not ENDENTRY
        INTSAN = INTSAN+1                      ! Increment Subaccession #
        CALL LOCATE(6,IDLST,ID,ISECND,*900,*580,*680,*390) ! Try to relocate
      END IF

C---- ENDENTRY found
  950 LOCI = 3                                 ! Set section ID to ENTRY
      CALL READ_NEXT(1,ID,*990)                ! Read next record
      CALL SYS_LOCATE(ID,3,IMSNG,ISECND,*390,*350) ! Check for ENTRY

C---- Not ENTRY record
      IF (ID.NE.2) THEN                        ! Not ENDTRANS
        DO955I = 19,27,2                       ! Check for other end labels
          IF(ID.EQ.I) GOTO 970
  955   CONTINUE
        GOTO 310                               ! Not end label
      END IF

C---- End label found
  970 IF(ID.NE.ILAB+1) CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,30)
                                              ! Label and end label do not match
  971 CALL READ_NEXT(1,ID,*999)                ! Read next record
      CALL SYS_LOCATE(ID,2,IMSNG,ISECND,*971,*978)
                                               ! Check for additional end labels
  978 CALL ERR_REC(KEY2,IPTR,KARD,ISEQ,IALT,2) ! Record after end label
      GOTO 999

C---- Lost in processing
  980 CALL ERR_MES1(2,2)                       ! Error message 
      IF(ID.EQ.0) CALL SYSGET(ID,IDLST,*999)   ! Skip to next ENTRY or SUBENT
      CALL ERR_REC(KEY2,IPTR,KARD,ISEQ,IALT,29)

  986 GOTO (980,970,390,390,950,490,490,900,580,580,600,680,680,700,
     *     780,780,800,980,970,980,970,980,970,980,970,980,970) ID

C---- End-of-file found while reading data
  990 CALL ERR_EOF(2,LOCI)                     ! Error message 
      GOTO 999

C---- File empty 
  998 CALL ERR_EOF(3,3)                        ! Error message

  999 RETURN

 1100 FORMAT(/' File labeled: ',A10,A1,A22)
      END
      SUBROUTINE ALTER_FLAG(IALT,ID,INTSAN,IERR)
	save !zvv:2019-07-18

C* Checks alteration code.
C*   IALT:   alteration code
C*   ID:     system identifier equivalent
C*   INTSAN: subaccession # (I3)
C*   IERR:   error flag   41 = illegal alter flag
C*                        42 = subentry missing alter code

C---- Total counts for records, entries, subentries. (Initialized in ALTER_FLAG)
      COMMON/COUNTS/KOUNT(4)
      COMMON/zCOUNTS/kountE,kountS

      CHARACTER ALTCOD(7)/'C','*','I','T','R','D',' '/  ! Legal ALTER flags
      CHARACTER IALT,ALTSUB/' '/

      DATA KOUNT/4*0/                   ! Initialize counts
c      data kountE,kountS/0,0/

      IERR = 0                          ! Turn off error flag

      IA = LOOKUP(IALT,ALTCOD,7)        ! Look up ALTER code  
      IF(IA.EQ.0) GOTO 500              ! Branch for illegal code

      IF (ID.EQ.6) THEN                 ! SUBENT
        ALTSUB = IALT                     ! Reset ALTER flag for SUBENT
        IF (IA.EQ.3 .OR. IA.EQ.7) THEN    ! New subentry
          IF(INTSAN.NE.1) KOUNT(4) = KOUNT(4)+1 ! Increment new data SAN count
        END IF
      ELSE IF (ID.EQ.3) THEN            ! ENTRY
        IF (IA.GT.3 .AND. IA.LT.7) GOTO 500
      ELSE IF (ID.EQ.7) THEN            ! NOSUBENT
        IF (IA.NE.2 .AND. IA.NE.7) GOTO 500
      ELSE 
        IF(IA.EQ.7) RETURN                ! Blank
        IF(IA.EQ.2) GOTO 500              ! '*' not legal
        IF (ALTSUB.NE.'C') THEN           ! No corrected code on SUBENT
          ALTSUB = 'C'                      ! Reset SUBENT ALTER code
          IF(ALTSUB.EQ.'I' .OR. ALTSUB.EQ.' ') KOUNT(4) = KOUNT(4)-1
                                            ! Decrement new subentry count
          IERR = 10                         ! Set error flag
          GOTO 510
        END IF
      END IF
      RETURN

C---- Error
  500 IERR = 9                        ! Set flag
  510 CALL ERR_MES(1,IERR)            ! Error message 
      RETURN
      END
      SUBROUTINE AN_CHECK(ACNUM,INTAN,INTSAN)
	save !zvv:2019-07-18

C* Sets accession number and checks.  Initializes flags for entry.
C*   ACNUM:  hollerith accession number (A5)
C*   INTAN:  integer accession number (I6)
C*   INTSAN: integer subaccession number (I3)

C---- Input record. (Set in READ_NEXT)
      COMMON/ALLCOM/KEY2,IPTR,KARD,ISEQ,IALT
        CHARACTER IPTR,IALT
        CHARACTER*13 ISEQ
        CHARACTER*10 KEY2
        CHARACTER*55 KARD

C---- Operating mode. (Set in OPENUP)
      COMMON/OPMODE/ITYPE,IWARNG
      COMMON/COUNTS/KOUNT(4)

      CHARACTER*3 SACNUM
      CHARACTER*5 ACNUM,ANSUB
      INTEGER HLDAC,HLDSAC

      DATA HLDAC,HLDSAC/2*0/

!	write(*,*)'___ACNUM=[',ACNUM(1:5),'] KARD(7:11)=[',KARD(7:11),']'
!	if (KARD(7:11).LE.ACNUM(1:5)) then
!	    write(*,*)'___KARD.LE.ACNUM___'
!	end if


      INTSAN = 0                       ! INITIALIZE SAN FOR ENTRY
      ACNUM(1:5) = KARD(7:11)          ! STORE AN
      IF(ITYPE.EQ.2) ISEQ(1:5) = ACNUM ! SET AN FOR COL.67-79 CHECK
      CALL PUTAN(ACNUM,KARD(15:22))    ! WRITE AN ON OUTPUT 
      KOUNT(2)=KOUNT(2)+1
      HLDSAC = 0                       ! INITIALIZE LAST SAN

      INTAN = INTFORM(ACNUM,5,IGOOF,2)     ! STORE ACCESSION # AS INTEGER

      IF(IGOOF.NE.0) THEN                  ! ERROR IN CONVERSION TO INTEGER
        CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,5) ! ERROR MESSAGE
        RETURN
      END IF

!	write(*,*)'___INTAN=[',INTAN,'] HLDAC=[',HLDAC,']'
!2020      IF (INTAN.LT.HLDAC) THEN             ! ACCESSION # LESS THEN LAST
!2020-06-25: LT ==> LE to avoid tye same ENTRY accession number, e.g. prelim.1470
      IF (INTAN.LE.HLDAC) THEN             ! ACCESSION # LESS THEN LAST
        CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,5) ! ERROR MESSAGE
      ELSE                                           
        HLDAC = INTAN                        ! STORE LAST AN
      END IF
      RETURN

      ENTRY SAN_CHECK(ACNUM,INTAN,INTSAN)

C*    SET SUBACCESSION NUMBER AND CHECK.
C*    RESETS FLAGS FOR SUBENTRY.
C*      ACNUM: HOLLERITH ACCESSION NUMBER (A5)
C*      INTAN: INTEGER ACCESSION NUMBER (I6)
C*      INTSAN: INTEGER SUBACCESSION NUMBER (I3)
C*              NEGATIVE IF SUBENT NOT READ (DO INITIALIZATION ONLY)

      IF(INTSAN.EQ.0) GOTO 550       ! LABEL OR ENTRY RECORD

      IF(INTSAN.LT.0) RETURN

C---- STORE ACCESSION # AND SUBACCESSION # FROM N1 FIELD.
  550 ANSUB(1:5) = KARD(4:8)
      SACNUM(1:3) = KARD(9:11)
      IF(ITYPE.NE.1) ISEQ(6:8) = SACNUM(1:3)

C---- FORM INTEGER ACCESSION # FOR COMPARISON AND CHECK.
      IF (INTAN.NE.0) THEN             ! ENTRY RECORD READ
        IF(ANSUB.NE.ACNUM) THEN          ! AN NOT EQUAL TO AN FROM ENTRY RECORD
          CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,6) ! ERROR MESSAGE
          GOTO 800
        END IF
      ELSE                             ! ENTRY RECORD NOT READ
        ACNUM = ANSUB                    ! SET AN FROM SUBENT
        INTAN = INTFORM(ANSUB,5,IGOOF,2) ! SET INTEGER AN
        HLDAC = INTAN                    ! SET AN FOR COMPARISON WITH NEXT
        CALL PUTAN(ACNUM,KARD(15:22))    ! WRITE AN ON OUTPUT 
      END IF

      INTSAN = INTFORM(SACNUM,3,IGOOF,1) ! FORM SUBENTRY # FOR COMPARISON
      IF(IGOOF.NE.0 .OR. INTSAN.LE.HLDSAC) THEN  
               ! ERROR IN CONVERSION TO INTEGER OR SUBACCESSION # LESS THAN LAST
        CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,6) ! ERROR MESSAGE
        GOTO 800
      END IF

      HLDSAC = INTSAN            ! STORE SUBACCESSION # FOR COMPARISON WITH NEXT
      RETURN

C---- ERROR IN SAN. IF LAST SAN WAS 0 OR 1, INCREMENT BY 1.
  800 IF(HLDSAC.GT.1) RETURN
      HLDSAC = HLDSAC+1          ! INCREMENT SUBACCESSION #
      INTSAN = HLDSAC            ! SET CURRENT ACCESSION # TO STORED
      RETURN
      END
      SUBROUTINE BIB1(ISECND,IMSNG,ID,*,*)
	save !zvv:2019-07-18

C* Processes BIB section.
C*   ISECND: sequence flag, 1 = last keyword out of sequence
C*   IMSNG:  missing keyword flag
C*   ID:     System ID numerical equivalent
C*            9 = BIB
C*           10 = NOBIB
C*           -2: BIB keyword
C*   RETURN 1: ENDBIB read
C*   RETURN 2: End of file read

C---- Input record. (Set in READ_NEXT)
      COMMON/ALLCOM/KEY2,IPTR,KARD,ISEQ,IALT
        CHARACTER IPTR,IALT
        CHARACTER*13 ISEQ
        CHARACTER*10 KEY2
        CHARACTER*55 KARD

C---- Fatal error flag. (Initialized in PASS1).
      COMMON/ENDITALL/ISTOP

C---- Error pointers. (Initialized in BIBSUB)
C-      IERR: ERROR FLAG, 0 = NO ERROR
C-      ARROW: ERROR COLUMN MARKERS (^)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

C---- Keyword table. (Set in DICT2)
      COMMON/KEYTAB/NKEY,KEYWD(100),KEYEQ(100),KSTAT(100),KREQ(100),
     *              KODED(100),KDICT(100)
        CHARACTER*10 KEYWD
        CHARACTER*3 KSTAT
        CHARACTER KREQ

C---- Operating mode. (Set in OPENUP)
      COMMON/OPMODE/ITYPE,IWARNG

C---- Record counts for ENTRY. Set in READ_NEXT.
      COMMON/RECCNT/N1CNT,N2CNT,NBIBR,NSUB

      INTEGER THISAN(100)

C---- Initialize data error flags 
c_g77      DATA ARROW/' '/

      IERR = 0                            ! Initialize error flag
      NBIBR = 0                           ! Initialize # of bib records
      NKEYR = 0                           ! Initialize # of keywords
      CALL FILLIN(THISAN,100,0)           ! Initialize keyword found 

      IF (IMSNG.NE.0) THEN                ! BIB ID missing
        IMSNG = 0
        GOTO 210
      END IF

      IF (ID.EQ.-2) GOTO 220              ! Branch for first record read.

!	write(*,*)'___KARD:[',KEY2,IPTR,KARD,']'
      CALL READ_NEXT(1,ID,*990)           ! Read first record
!	write(*,*)'___KARD:[',KEY2,IPTR,KARD,']'
  110 CALL SYS_LOCATE(ID,11,IMSNG,ISECND,*900,*210) 
                                          ! Check for ENDBIB
  210 IF (ID.GT.0) GOTO 950               ! Branch for Illegal System ID

  220 NBIBR = NBIBR+1                     ! Increment # of BIB records 
      IF(KEY2.EQ.' ') GOTO 400

      NKEYR = NKEYR+1                     ! Increment # of BIB keywords
      IDB = LOOKUP(KEY2,KEYWD,NKEY)       ! Look up keyword in table

      IF (IDB.EQ.0) THEN                  ! Keyword not in tables
        CALL ERR_MES1(1,12)                 ! Error message 
        CALL FLAGIT(1,ARROW,1,10,IERR)
        GOTO 700
      ELSE IF (THISAN(IDB).NE.0) THEN     ! Duplicate keyword
        CALL ERR_MES(1,13)                  ! Error message 
        CALL FLAGIT(1,ARROW,1,11,IERR)
      ELSE
        THISAN(IDB) = 1                     ! Set keyword found
      END IF

      IF(KSTAT(IDB).EQ.'OBS') THEN        ! Obsolete keyword
        CALL ERR_MES1(1,14)                 ! Error message 
        CALL FLAGIT(1,ARROW,1,10,IERR)
      END IF 
  400 CALL CHAR_CHECK(KARD)               ! Check free text

  700 IF(IERR.NE.0) CALL ERR_ARW(KEY2,IPTR,KARD,ISEQ,IALT,ARROW,IERR) 
                               ! Write record with error pointers 
      CALL READ_NEXT(1,ID,*990)           ! Read next record
      GOTO 110

C     End of BIB section

  900 IF(NBIBR.EQ.0) THEN                  ! Empty BIB 
        CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,10) ! Error message 
      END IF

      IF (ITYPE.EQ.2) THEN                 ! Transmission mode
        IF (N1CNT.NE.NKEYR) THEN
          CALL ERR_INSERT(1,4,10,'BIB       ')
        END IF
        IF (N2CNT.NE.NBIBR) THEN
          CALL ERR_INSERT(1,5,10,'BIB       ')
        END IF
      END IF
      RETURN 1

C---- ENDBIB not found.
  950 ISECND = 1
      RETURN

C---- End-of-file found. Print error message. Terminate.
  990 CALL ERR_EOF(1,9)                    ! Error message
      ISTOP=1
      RETURN 2
      END
      SUBROUTINE CHAR_CHECK(KARD)
	save !zvv:2019-07-18

C* Checks for legal characters.
C*   KARD: input array
C*   NOW:  starting position in array
C*   LST:  final position to be checked

C---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*55 KARD

      PARAMETER (NLEG=24)
      CHARACTER LEGCHR(NLEG)/
     *            '+', '-', '.', ')', '(', ',', '/', '=', '''', '*',
     *            '%', '<', '>', ':', ';', '&', '?', '!', '#',  '[',
     *            ']', '"', '~','@'/

      IFND = 0                        ! Initialize illegal character flag

      DO 300 I = 1,55
        IF(KARD(I:I).EQ.' ') GOTO 300
        IF(KARD(I:I).GE.'A' .AND. KARD(I:I).LE.'Z') GOTO 300

        IF(KARD(I:I).GE.'a' .AND. KARD(I:I).LE.'z') GOTO 300
        IF(KARD(I:I).GE.'0' .AND. KARD(I:I).LE.'9') GOTO 300
        IC=LOOKUP(KARD(I:I),LEGCHR,NLEG)

        IF(IC.EQ.0) CALL FLAGIT(1,ARROW,I+11,1,IFND) 
                                         ! Illegal character, set error markers
  300 CONTINUE

      IF (IFND.NE.0) THEN             ! Error found
        IERR=1                          ! Set error flag
        CALL ERR_MES(1,15)              ! Error message 
      END IF
      RETURN
      END
      SUBROUTINE DATA1(INDX,ID,IMSNG,ISECND,*)
	save !zvv:2019-07-18

C* Processes the DATA section
C* Input/output of routine
C*   ID: index of System Identifier
C*       -2: Data Heading
C*       12 = DATA
C*       13 = NODATA
C*   INDX: section index
C*         1 = COMMON, 2 = DATA
C*   IMSNG: record missing flag
C*   ISECND: last keyword out-of-sequence flag
C* RETURN 1: end section ID read

C---- Input record. (Set in READ_NEXT)
      COMMON/ALLCOM/KEYD,IPTR,KARD,ISEQ,IALT
        CHARACTER IPTR,IALT
        CHARACTER*13 ISEQ
        CHARACTER*10 KEYD
        CHARACTER*55 KARD

C---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

C---- Operating mode. (Set in OPENUP)
      COMMON/OPMODE/ITYPE,IWARNG

C---- Record counts for ENTRY. Set in READ_NEXT.
      COMMON/RECCNT/N1CNT,N2CNT,NDATR,NSUB

      CHARACTER*80 NOWREC
      CHARACTER*10 SETMES(2)/'COMMON    ','DATA      '/

      INTEGER FOUND

      EQUIVALENCE (NOWREC(1:1),KEYD(1:1))

      IERRD = 0          ! Turn off data error flag
      IHCNT = 0          ! heading count
      IUCNT = 0          ! unit count
      NLINE = 0          ! # of data lines
      NDATR = 0          ! # of data records

      IF (ID.EQ.-2)THEN                    ! Data Heading already read
        ID2 = (INDX*3) + 9                   ! Compute section ID from index
        GOTO 200            
      ELSE
        ID2 = ID                             ! Store section ID 
      END IF
 
  100 CALL READ_NEXT(2,ID,*930)            ! Read next record
      IF (ID.GT.0 .AND. ID.NE.15) GOTO 910 ! System ID read

  200 NDATR = NDATR+1                      ! Assumed to be DATA record

C     Check headings. 

      CALL HEAD_CHECK(IHCNT,24,FOUND,IERRD,*920) ! Branch for < 18 fields

      IF (FOUND.EQ.0) THEN                 ! Heading not found
        IF(IMSNG.NE.0) RETURN                ! Return if 2nd illegal record
        GOTO 400
      ELSE                                 ! Headings found
        IF (IUCNT.NE.0) THEN                 ! Units read
          IERRD = 1                            ! Set error flag
          CALL ERR_MES1(1,1)                   ! Error message 
        END IF
      END IF

  290 IF (IERR.NE.0 .OR. IERRD.NE.0) THEN  ! Error flag set
        CALL ERR_ARW(KEYD,IPTR,KARD,ISEQ,IALT,ARROW,IERR) ! 
        IERRD = 0
        GOTO 100
      END IF

      CALL READ_NEXT(2,ID,*930)            ! Read next record
      IF (ID.GT.0 .AND. ID.NE.15) GOTO 910 ! System ID read

  320 NDATR = NDATR+1                   ! DATA or COMMON section record found

C     Check units. 

  400 CALL HEAD_CHECK(IUCNT,25,FOUND,IERRD,*920) ! Branch for < 18 fields

      IF (FOUND.NE.0) THEN              ! Units record
        IF(IERR.NE.0 .OR. IERRD.NE.0) THEN
          CALL ERR_ARW(KEYD,IPTR,KARD,ISEQ,IALT,ARROW,IERR) ! Print markers 
          IERRD = 0
        END IF
        CALL READ_NEXT(2,ID,*930)         ! Read next record
        CALL SYS_LOCATE(ID,0,IMSNG,ISECND,*430,*430) ! Check for section end
        IF (ID.GT.0) RETURN               ! Other System ID read

  430   NDATR = NDATR+1                   ! DATA or COMMON section record found
        IF(IHCNT.GT.IUCNT) GOTO 400       ! Look for next units record
      ELSE                              ! Not units record
        IF (IHCNT.NE.0) THEN              ! Previous heading record found
          CALL HEAD_CHECK(IHCNT,24,FOUND,IERRD,*920) ! Check if heading
          IF(FOUND.NE.0) GOTO 290           ! Heading record
        END IF
      END IF

      IF (IERR.NE.0 .OR. IERRD.NE.0) THEN  ! Error flag set
        CALL ERR_ARW(KEYD,IPTR,KARD,ISEQ,IALT,ARROW,IERR) ! 
        IERRD = 0
        GOTO 100
      END IF

C     Check data

      FOUND = 0                         ! Turn off legal data flag
      IDCNT = 0                         ! Initialize # of data fields
      CALL FLOAT_CHECK(IDCNT,NOWREC,ARROW,IERRD,FOUND)
                                        ! Check for legal data fields
      IF (IERR.NE.0 .OR. IERRD.NE.0) THEN
        CALL ERR_ARW(KEYD,IPTR,KARD,ISEQ,IALT,ARROW,IERR) ! Error message       
        IERRD = 0
      END IF
      
      IF (FOUND.EQ.0) THEN              ! Legal data record not found
        IF(IHCNT.EQ.IUCNT .AND. IUCNT.NE.0) GOTO 605 ! Titles and units read
        CALL ERR_REC1(KEYD,IPTR,KARD,ISEQ,IALT,16) ! Error message 
        IF(IMSNG.NE.0) GOTO 890         ! Branch for 2nd illegal record
        IF(IHCNT.EQ.IUCNT) IMSNG = 1    ! Set illegal record flag
        GOTO 100
      END IF

      IF(IUCNT.NE.IHCNT) CALL ERR_INSERT(3,3,10,SETMES(INDX)) 
                                        ! Unequal # of titles and units 

  605 IF(IUCNT.GT.IHCNT) IHCNT = IUCNT  ! # of fields = max.of titles, units

      IF (IDCNT.GE.IHCNT) THEN          ! Full line read
        NLINE = NLINE+1                   ! Increment line count
        IF(IERRD.NE.0) CALL ERR_REC1(KEYD,IPTR,KARD,ISEQ,IALT,16)
                                          ! Error message
        IDCNT = 0                         ! Initialize # of data fields
      END IF

      IF (IERR.NE.0 .OR. IERRD.NE.0) THEN ! Error flag set
        CALL ERR_ARW(KEYD,IPTR,KARD,ISEQ,IALT,ARROW,IERR) ! 
        IERRD = 0
        GOTO 100
      END IF

  710 CALL READ_NEXT(2,ID,*930)           ! Read next record
      IF (ID.EQ.0) GOTO 720

      CALL SYS_LOCATE(ID,ID2+2,IMSNG,ISECND,*900,*720) ! Check for section end
      GOTO 910

C---- Record not System ID. 
  720 IF(INDX.EQ.1 .AND. IDCNT.EQ.0) GOTO 890 ! Error for COMMON section

      NDATR = NDATR+1                     ! Increment # of data records
      CALL FLOAT_CHECK(IDCNT,NOWREC,ARROW,IERRD,FOUND)
                                          ! Check data fields
      IF (IERR.NE.0 .OR. IERRD.NE.0) THEN
        CALL ERR_ARW(KEYD,IPTR,KARD,ISEQ,IALT,ARROW,IERR)       
                                          ! Write record and error markers
        IERRD = 0
      END IF
      GOTO 710

C---- End System ID not found
  890 RETURN  

C---- END System ID found.
  900 IF (ITYPE.EQ.2) THEN                ! Transmission mode
c-zvv        NRLIN = (IHCNT/6)+1
c	write (*,*) '-zvv- IHCNT=',IHCNT
        NRLIN = (IHCNT+5)/6	!-zvv-
c-zvv	write (*,*) '-zvv- IHCNT=',IHCNT,' NDATR=',NDATR,' NRLIN=',NRLIN
	if (NRLIN.le.0) NRLIN=1
        NDATR = NDATR/NRLIN
        IF (INDX.EQ.2) NDATR = NDATR - 2  ! DATA, reset line count
c-zvv	write (*,*) ' IHCNT=',IHCNT,' N1CNT=',N1CNT
        IF (N1CNT.NE.IHCNT) THEN
c-zvv	write (*,*) '-zvv-1- N1CNT=',N1CNT,' NDATR=',NDATR
c-zvv     *	,' IHCNT=',IHCNT,' NRLIN=',NRLIN
          CALL ERR_INSERT(1,4,10,SETMES(INDX))
        END IF
	if (INDX.eq.1) NDATR=NDATR*NRLIN
        IF (N2CNT.NE.NDATR) THEN
c-zvv	write (*,*) '-zvv-2- N2CNT=',N2CNT,' NDATR=',NDATR
c-zvv     *	,' IHCNT=',IHCNT,' NRLIN=',NRLIN,' INDX=',INDX
          CALL ERR_INSERT(1,5,10,SETMES(INDX))
        END IF
      END IF
      RETURN 1

C---- System ID found in section
  910 CALL SYS_LOCATE(ID,ID2+2,IMSNG,ISECND,*900,*900) ! Check for section end
      CALL ERR_REC1(KEYD,IPTR,KARD,ISEQ,IALT,1) ! Other System ID read, error
      RETURN                            

C---- Too many fields
  920 CALL ERR_REC1(KEYD,IPTR,KARD,ISEQ,IALT,23) ! Error message
      CALL SYSGET(ID,IDLST,*930)                 ! Skip section
      IF (ID.NE.ID2+2) RETURN                    ! Return for END ID not found
      RETURN 1

C---- End-of-file found
  930 CALL ERR_EOF(1,ID)                         ! Error message
      END
      SUBROUTINE DATE_CHECK(IDAT,NDIG,IERR)
	save !zvv:2019-07-18

C*  Checks date in format YYYYMMDD or YYMMDD (for year < 2000).
C*    IDAT: input date (6A1)
C*    NDIG: # of digits expected
C*    IERR: error flag

C---- Today's date. (Set in MAIN)
      COMMON/TODAY/IYMD

      CHARACTER*(*) IDAT
      INTEGER DAYS(12)

C---- # of days in each month
      DATA DAYS/31,29,31,30,31,30,31,31,30,31,30,31/

      IERR = 0         ! TURN OFF ERROR FLAG

C---- # of digits input must be 2, 4, 6 or 8.

      IF (IDAT(1:2).GT.'30') GOTO 900           ! must have 4-digit year.

      IF (NDIG.NE.4 .AND. NDIG.NE.6 .AND. NDIG.NE.8) GOTO 900                   
                                                ! Must have 2,4, or 6 digits.
      INTDAT = INTFORM(IDAT,NDIG,IERR,1)        ! Form integer date

C---- Check date range.
      IF (NDIG.LE.6) INTDAT = INTDAT*100
      IF (NDIG.EQ.4) INTDAT = INTDAT*100
      IF (INTDAT.LT.19320000 .OR. INTDAT.GT.IYMD) goto 800

      IF(NDIG.EQ.4) RETURN

C---- Check month
      IY = INTDAT/10000
      IM = (INTDAT-(IY*10000))/100
      IF (IM.LE.0 .OR. IM.GT.12) GOTO 800

      IF(NDIG.EQ. 6) RETURN

C---- Check day
      IYM = INTDAT/100
      ID = INTDAT-(IYM*100)
      IF (ID.LT.0 .OR. ID.GT.DAYS(IM)) GOTO 800
      RETURN

C---- Date out of range 
  800 CALL ERR_MES(1,6)     ! Error message 
      GOTO 990

C---- Illegal date structure
  900 CALL ERR_MES(1,7)     ! Error message 

  990 IERR = 1              ! Set error flag

      RETURN
      END
      SUBROUTINE FLOAT_CHECK(ICNT,STRING,IARW,IERR,IFDAT)
	save !zvv:2019-07-18

C* Checks data fields for legal floating point numbers.  Stores numbers.
C*   ICNT:   field count
C*   STRING: fields to be checked
C*   IARW:   error pointers
C*   IERR:   error flag
C*   IFDAT:  data found on record flag

      CHARACTER*80 IARW
      CHARACTER*66 STRING

      DO 690 I = 1,6                       ! Loop over data fields
        ICNT = ICNT+1                        ! Increment # of fields
        NOW = (I*11)-10                      ! 1st position in current field

        IF (STRING(NOW:NOW+10).NE.' ') THEN  ! Nonblank field

          DIGIT = FLOATIT(STRING(NOW:NOW+10),11,IERR1)
                                             ! Check floating point number
          IF (IERR1.EQ.0) THEN          
            IFDAT = 1                          ! Set data found
          ELSE                                 ! Error
            CALL ERR_MES1(1,20)
            CALL FLAGIT(1,IARW,NOW,11,IERR)      ! Set error markers
          END IF
        END IF
  690 CONTINUE
      RETURN
      END
      SUBROUTINE HEAD_CHECK(KNTFLD,KDIC,FOUND,IERRM,*)
      save !zvv:2019-03-06:really needed

C* Checks for legal titles or units, 
C*            blanks embedded in code,
C*            nonblank pointer in unit field.
C*   KNTFLD:  field count
C*   KDIC:   dictionary number
C*   FOUND:  keyword found on record flag
C*   IERRM:  error flag; message printed
C*   RETURN 1: more than 18 fields

C---- Input record. (Set in READ_NEXT)
      COMMON/ALLCOM/KEY,IPTR,KARD,ISEQ,IALT
        CHARACTER*55 KARD
        CHARACTER*13 ISEQ
        CHARACTER*10 KEY
        CHARACTER IPTR,IALT

C---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

C---- Fatal error flag. (Initialized in PASS1).
      COMMON/ENDITALL/ISTOP

C---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ISANS/INTAN,ISECT

C---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

C---- Last record. (Set in READ_NEXT, READ_CONT)
      COMMON/LASTRC/LKEY,LPTR,LFLD,LSEQ,LALT
        CHARACTER*55 LFLD
        CHARACTER*13 LSEQ
        CHARACTER*10 LKEY
        CHARACTER LPTR,LALT

      CHARACTER*80 DICLINE
      CHARACTER*11 KEYC(6)
      CHARACTER*10 KEYD/' '/
      CHARACTER*3 ISTAT
      CHARACTER KPTR

      INTEGER FOUND
!	save IDEP !zvv:2019-03-06

      EQUIVALENCE (KEYC(1)(1:10),KEY),(KEYC(1)(11:11),IPTR),
     *            (KEYC(2)(1:1),KARD(1:1))

      FOUND = 0                       ! Keyword found flag
      NFND = 0                        ! # of legal keywords found
      IFBLNK = 0                      ! Blank field flag
      LFLG = 0                        ! Blank field at end of last record flag

C---- Set up section index for stored pointers
      IF (INTSAN.EQ.1) THEN           ! Subentry 1 COMMON section
        LDX = 1
      ELSE IF (ISECT.EQ.15) THEN      ! DATA section
        LDX = 3
      ELSE                            ! DATA subentry COMMON section
        LDX = 2
      END IF

      DO 400 I = 1,6                         ! Loop over fields

        IF (KEYC(I)(1:10).EQ.' ') THEN       ! Blank field
          IFBLNK = 1
          IF(KEYC(I)(11:11).EQ.' ') GOTO 400   ! Pointer should be blank
          CALL ERR_MES1(1,23)                  ! Error message 
          CALL FLAGIT(1,ARROW,I*11,1,IERRM)
          GOTO 400
        END IF

        NFND = NFND+1                        ! Increment # of nonblank fields
        KEYD = KEYC(I)(1:10)                 ! Store code
        KPTR = KEYC(I)(11:11)                ! Store pointer
        CALL DANGET_STA_NEW(KDIC,KEYD,0,0,DICLINE,ISTAT,IERR1) ! Look up heading
        NOW = ((I-1)*11)+1                   ! Set current position
        IF(IERR1.NE.0) GOTO 380

        IF (ISTAT.EQ.'OBS') THEN             ! Obsolete code
!2022          CALL ERR_MES1(1,14)                ! Error message 
          CALL ERR_MES(1,31)                ! Error message 20220608
          CALL FLAGIT(1,ARROW,NOW,11,IERRM)
c	write(*,*)'_KARD:[',KEY2,'[',trim(KEYD)
        END IF

C----   LEGAL KEYWORD FOUND. 

        IF (FOUND.EQ.0) THEN                 ! First, any previous fields in error
          IF(NFND.GT.1) CALL FLAGIT(1,ARROW,1,(NOW-1),IERRM) ! Set markers
          FOUND = 1                            ! Set heading found on record

C----     Check if blanks at end of last record
  242     N = MOD(KNTFLD,6)    
          IF (N.NE.0) THEN                   ! Not first header record
            LFLG = 1                           ! Set blank on last field
            KNTFLD = KNTFLD+1                    ! Increment field #
            CALL ERR_FIELD(2,KNTFLD,1)          ! Error message 
            ISTOP=2
            GOTO 242
          END IF
          IF (LFLG.NE.0) THEN                ! Blank field found
            CALL ERR_REC(LKEY,LPTR,LFLD,LSEQ,LALT,18) ! Error message
            LFLG = 0                           ! Turn off blank field flag
          END IF
        END IF

!zvv	write(*,*)'_zv_HEAD_CHECK:KDIC=',KDIC,' LDX=',LDX,' IDEP=',IDEP
        IF (KDIC.EQ.24) THEN
          IF (DICLINE(3:3).EQ.'*') THEN
            IF (LDX.LT.3) THEN
              CALL FLAGIT(1,ARROW,11,NOW,IERRM)
              CALL ERR_MES1(1,17)
            ELSE 
              IDEP = 1
            END IF
          END IF
        ELSE
          IF (KEYC(I)(11:11).NE.' ') THEN
            CALL FLAGIT(1,ARROW,NOW+10,1,IERR)
            CALL ERR_MES1(1,15)
          END IF
          IF (LDX.EQ.3) THEN
            IF(IDEP.EQ.0) CALL ERR_MES1(2,16)
            IDEP = 1
          END IF
        END IF

        N = NFND+KNTFLD                       ! Store total count of fields
        IF(N.GT.18) GOTO 500                 ! Check for more than 18 fields

        IF(IFBLNK.EQ.0) GOTO 400             ! Legal field after blank field?

  380   IF(FOUND.NE.0) CALL FLAGIT(1,ARROW,NOW,11,IERRM) ! Set error markers

  400 CONTINUE

      IF (FOUND.NE.0) THEN                   ! If legal data heading found
        KNTFLD = KNTFLD+NFND                     ! Set count
        IF(IERRM.NE.0) CALL ERR_MES1(1,18)       ! Error message 
      END IF
      RETURN

C---- Too many fields.
  500 CALL ERR_MES1(1,24)                    ! Error message 
      IERR = 1
      RETURN 1
      END
      SUBROUTINE LOCATE(ID,IDLAST,IDNOW,ISECND,*,*,*,*)
	save !zvv:2019-07-18

C* Tries to relocate when expected System ID has not been read.
C* Input to routine
C*   ID:     expected System ID index
C*   IDLAST: index of last System ID read
C*   IDNOW:  index of System ID read
C*   ISECND: last record out-of-sequence flag
C* RETURN normal return if not able to process.
C*        1: duplicate of last System ID
C*        2: next valid System ID
C*        3: second next valid System ID
C*        4: second concutive illegal record.

C---- Input record. (Set in READ_NEXT)
      COMMON/ALLCOM/KEY2,IPTR,KARD,ISEQ,IALT
        CHARACTER IPTR,IALT
        CHARACTER*13 ISEQ
        CHARACTER*10 KEY2
        CHARACTER*55 KARD

C---- System identifier table. (Set in DICT1)
      COMMON/SYSTAB/NSYS,SYSID(30),INDX_SYS(30)
        CHARACTER*10 SYSID

      DIMENSION IDNEXT(17,3)

C---- Indicis for next valid System IDs.
      DATA IDNEXT/3, 0, 6, 3, 3, 9, 6, 6,-1,12,12,-2,15,15,-2, 8, 8,
     *            4, 0, 7, 2, 2,10, 5, 5, 0,13,13, 0,16,16, 8, 0, 0,
     *            0, 0, 0, 0, 0, 0, 7, 7, 0, 0, 0, 0, 8, 8, 0, 0, 0/

      IF (ID.NE.0 .AND. IDNOW.EQ.0) THEN   ! Current not valid ID
        CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,1)
        GOTO 410
      END IF

      IF (IDNOW.EQ.IDLAST) THEN       ! Duplicate of last System ID
        CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,4)
        RETURN 1
      END IF

      IF (ID.EQ.9 .OR. ID.EQ.12 .OR. ID.EQ.15) THEN
        IDNO = ID + 1
      ELSE
        IDNO = 0
      END IF

C---- Check if record is next valid System ID.
      DO 210 I = 1,3
        IF (IDNOW.EQ.IDNEXT(ID,I)) THEN      ! Match on next legal ID
          L = LOOKIN(ID,1,INDX_SYS,NSYS)       ! Look up index of ID
          CALL ERR_INSERT(3,1,10,SYSID(L))     ! Write error message 
          RETURN
        END IF
        IF (IDNO.NE.0) THEN                  ! Check on NO----
          IF (IDNOW.EQ.IDNEXT(IDNO,I)) THEN    ! Match on next legal ID
            L = LOOKIN(ID,1,INDX_SYS,NSYS)       ! Look up index of ID
            CALL ERR_INSERT(3,1,10,SYSID(L))     ! Write error message 
            RETURN 2
          END IF
        END IF
  210 CONTINUE
      GOTO 300


C---- Check if record is 2nd next valid system ID.
  300 DO 340 I = 1,3
        IF(IDNEXT(ID,I).LE.0) GOTO 400       ! No 2nd next valid ID
        ID2 = IDNEXT(ID,I)                   ! Set ID to next valid ID
        DO 310 II = 1,3
          IF (IDNOW.EQ.IDNEXT(ID2,II)) THEN    ! Match on 2nd next
            L = LOOKIN(ID,1,INDX_SYS,NSYS)       ! Look up index of ID
            CALL ERR_INSERT(3,1,10,SYSID(L))     ! Error message 
            RETURN
          END IF
  310   CONTINUE

        IF (IDNO.NE.0) THEN                  ! Check on NO----
          ID2 = IDNEXT(IDNO,I)                 ! Set to next valid ID
          DO 320 II = 1,3
            IF (IDNOW.EQ.IDNEXT(ID2,II)) THEN    ! Match on 2nd next
              L = LOOKIN(ID,1,INDX_SYS,NSYS)       ! Look up index of ID
              CALL ERR_INSERT(3,1,10,SYSID(L))     ! Error message 
              RETURN 3
            END IF
  320     CONTINUE
        END IF
  340 CONTINUE
      GOTO 400


C---- Not found. Write out of sequence message. 
  400 IF (IDNOW.EQ.0) THEN
        CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,1)
      ELSE
        CALL ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,3)
      END IF

  410 IF (ISECND.NE.0) RETURN 4
      RETURN
      END
      SUBROUTINE READ_NEXT(IWAY,ID,*)

	save !zvv:2019-03-07:really needed

C* Reads the input file
C* Input to routine
C*   IWAY: 1  =  check for duplicate records and skip
C*         2  =  do not check for duplicate records
C*   ID:   System Identifier equivalent
C* RETURN 1: end-of-file found.

C---- Input record. (Set in READ_NEXT)
C-      KEY:  columns 1-10
C-      IPTR: column 11
C-      KARD: columns 12-66
C-      ISEQ: sequence # (columns 75-79)
C----   IALT: alteration flag
      COMMON/ALLCOM/KEY,IPTR,KARD,ISEQ,IALT
        CHARACTER IPTR,IALT
        CHARACTER*13 ISEQ
        CHARACTER*10 KEY
        CHARACTER*55 KARD

C---- Current AN, SAN, section. (Set in READ_NEXT)
C-      ACNUM:  character accession #
C-      INTSAN: subaccession #
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

C---- Current AN, section. (Set in READ_NEXT)
C-      INTAN:  integer accession #
C-      ISECT:  section
C---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ISANS/INTAN,ISECT

C---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

C---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IODUM(3)

C---- Last record. (Set in READ_NEXT, READ_CONT)
C-      LKEY: columns 1-10
C-      LPTR: column 11
C-      LFLD: columns 12-66
C-      LSEQ: sequence #
C----   LALT: alteration flag
      COMMON/LASTRC/LKEY,LPTR,LFLD,LSEQ,LALT
        CHARACTER*10 LKEY
        CHARACTER LPTR,LALT
        CHARACTER*55 LFLD
        CHARACTER*13 LSEQ

C---- Operating mode. (Set in OPENUP)
      COMMON/OPMODE/ITYPE,IWARNG

C---- Record counts for entry. Set in READ_NEXT.
C-      N1CNT: N1 field of System ID
C-      N2CNT: N2 field of System ID
C-      N1REC: # of records read in section
C----   NSUB:  # of subentries read in entry
      COMMON/RECCNT/N1CNT,N2CNT,N1REC,NSUB

      COMMON/zCOUNTS/kountE,kountS

      CHARACTER*80 NOWREC,LSTREC

      EQUIVALENCE (NOWREC(1:10),KEY(1:10)),(NOWREC(11:11),IPTR),
     * (NOWREC(12:66),KARD(1:55)),(NOWREC(67:79),ISEQ(1:13)),
     * (NOWREC(80:80),IALT),
     * (LSTREC(1:10),LKEY(1:10)),(LSTREC(11:11),LPTR),
     * (LSTREC(12:66),LFLD(1:55)),(LSTREC(67:79),LSEQ(1:13)),
     * (LSTREC(80:80),LALT)

      DATA NENT/0/                     ! Initialize counts

      LSTREC = NOWREC                  ! Store last record for comparison

  110 READ(IN,8000,END=900) NOWREC     ! Read next record
      IERR = 0                         ! Turn off record error flag
      IERR1 = 0                        ! Turn off n1 error flag
      IERR2 = 0                        ! Turn off n2 error flag

      CALL SYS_CHECK(KEY,ID,IDLST)     ! Check for System Identifier

      IF (IWAY.EQ.1) THEN              ! Check for blank and duplicate records

        IF (NOWREC(1:66).EQ.' ') THEN    ! Blank record
          IF(ID.LE.0) N1REC = N1REC+1      ! Increment # of records in section
          IF(LSTBLK.EQ.0) THEN             ! 1st blank
            LSTBLK = 1                       ! Set blank record flag
            IF (IWARNG.NE.0) THEN
              CALL ERR_WARN(2,2)               ! Print warning message
              CALL ERR_REC(KEY,IPTR,KARD,ISEQ,IALT,0) ! Print record
            END IF
          END IF
          GOTO 110
        ELSE
          LSTBLK = 0                      ! Turn off blank record flag
        END IF

        IF (LSTREC(1:66).EQ.NOWREC(1:66)) THEN    ! Duplicate record
          IF (ID.NE.2) THEN                         ! Not ENDTRANS
            CALL ERR_REC(KEY,IPTR,KARD,ISEQ,IALT,32)  ! Print message
          END IF
          GOTO 110
        END IF
      END IF

      IF(ID.GT.0) ISECT = ID               ! For System ID, set section

      IF(ID.EQ.3) THEN                     ! ENTRY

c	ialt=NOWREC(11:11) !---zvv---2011.05.10
	if (NOWREC(11:11).eq.' ') kountE=kountE+1
c	if (ialt.eq.' ') write (*,*) '---new ENTRY: ',NOWREC(1:10),kountE
c	if (ialt.eq.'C') write (*,*) '---mod ENTRY: ',NOWREC(1:10),kountE
c	write (*,*) '---ENTRY: ',NOWREC(18:22)
c     +	,' IALT=[',NOWREC(11:11),'] new=',kountE

          CALL AN_CHECK(ACNUM,INTAN,INTSAN)  ! Check accession #
          NENT = NENT+1                      ! Increment ENTRY count

      ELSE IF(ID.EQ.6.OR.ID.EQ.7) THEN     ! SUBENT, NOSUBENT

	ialt=NOWREC(11:11) !---zvv---2011.05.10
c	if (ialt.eq.' ') write (*,*) '---new SUBENT: ',NOWREC(1:10)
c	if (ialt.eq.'C') write (*,*) '---modif SUBENT: ',NOWREC(1:10)

          CALL SAN_CHECK(ACNUM,INTAN,INTSAN) 
                                             ! Check subaccession # 
      END IF

      IF(ITYPE.EQ.1) GOTO 400              ! Branch for data processing mode

C*    TRANSMISSION MODE

      NSUBRC = NSUBRC+1                    ! Increment record count for subentry

c-zvv-	write (*,*) '---zvv---ID=',ID,'[',NOWREC(12:22),']'
      IF (ID.EQ.1) THEN                    ! TRANS ID
        INTSAN = 0                           ! Initialize SAN
        NSEQ = 0                             ! Initialize seq.#

C----   Set accession # for 1st record
        ACNUM = '00000'                      ! Initialize hollerith accession #
        IF (NOWREC(19:19).NE.' ') THEN       ! Transmission ID exists
          ACNUM(1:1) = NOWREC(19:19)           ! Store area code
c	write (*,*) '-zvv-1 INTSAN=',INTSAN,' ACNUM=',ACNUM
c-zvv-    INTSAN = INTFORM(ACNUM,5,IERR1,2)    ! Form integer equivalent
          INTAN = INTFORM(ACNUM,5,IERR1,2)    ! Form integer equivalent
c	write (*,*) '-zvv-2 INTSAN=',INTSAN,' IERR1=',IERR1
          IF (IERR1.NE.0) CALL FLAGIT(1,ARROW,12,11,IERR1) ! Error
        ELSE
c-zvv-    INTSAN = 0                           ! Set default accession #
          INTAN = 0                            ! Set default accession #
        END IF

        CALL DATE_CHECK(NOWREC(26:33),8,IERR2) ! Check date
        IF(IERR2.NE.0) CALL FLAGIT(1,ARROW,23,11,IERR) ! Set error markers

      ELSE IF (ID.EQ.2) THEN               ! ENDTRANS
        ACNUM(2:5) = '9999'                  ! Set minimum for col.67-71
        INTSAN = 999                         ! Set col.72-74
        NSEQ = 99999                         ! Set col.75-79
        N1 = INTFORM(NOWREC(12:22),11,IERR1,2) ! Check integer 1
        IF (IERR1.EQ.0) THEN
          IF (NENT.NE.N1) THEN
            CALL ERR_MES(1,4)
            IERR1 = 1
          END IF
        END IF
        IF(IERR1.NE.0) GOTO 390

      ELSE IF (ID.EQ.3 .OR. ID.EQ.4 .OR. ID.EQ.6 .OR. ID.EQ.7) THEN
                                           ! ENTRY,NOENTRY,SUBENT,NOSUBENT
        NSEQ = 1                             ! Initialize seq.#
        CALL DATE_CHECK(NOWREC(26:33),8,IERR2) ! Check date
        IF(IERR2.NE.0) CALL FLAGIT(1,ARROW,23,11,IERR) ! Set error markers
        NSUBRC = 0                         ! Initialize # of records in subentry
        IF ((ID.EQ.3).OR.(ID.EQ.4)) NSUB = 0

      ELSE IF (ID.EQ.5) THEN               ! ENDENTRY
        INTSAN = 999                         ! Set SAN
        NSEQ = 99999                         ! Set sequence #
        N1 = INTFORM(NOWREC(12:22),11,IERR1,2)
        IF (IERR1.EQ.0) THEN
          IF (NSUB.NE.N1) THEN
c	write (*,*) '-zvv- NSUB=',NSUB,' N1=',N1
            CALL ERR_MES(1,4)
            IERR1 = 1
          END IF
        END IF
        IF(IERR1.NE.0) GOTO 390

      ELSE IF (ID.EQ.8) THEN               ! ENDSUBENT
        NSEQ = 99999                         ! Set sequence #
        N1 = INTFORM(NOWREC(12:22),11,IERR1,2)
        IF (IERR1.EQ.0) THEN
          IF (NSUBRC-1.NE.N1) THEN
            CALL ERR_MES(1,4)
            IERR1 = 1
          END IF
        END IF
        IF(IERR1.NE.0) GOTO 390
       
      ELSE IF (ID.EQ.9 .OR. ID.EQ.12 .OR. ID.EQ.15) THEN ! BIB, COMMON, DATA
        NSEQ = NSEQ+1                        ! Increment sequence #
        N2CNT = INTFORM(NOWREC(23:33),11,IERR2,1)
        IF(IERR2.NE.0) CALL FLAGIT(1,ARROW,23,11,IERR2) ! Set error markers
        N1CNT = INTFORM(NOWREC(12:22),11,IERR1,2)
c-zvv-	write (*,*) '---zvv---N1CNT=',N1CNT,'[',NOWREC(12:22),']'
        IF (IERR1.NE.0) GOTO 390

      ELSE
        NSEQ = NSEQ+1                        ! Increment sequence #

      END IF
      GOTO 399

  390 CALL FLAGIT(1,ARROW,12,11,IERR1)     ! Set n1 error markers

  399 IF (IERR1.NE.0) THEN
c	write (*,*) '-zvv- 399'
        CALL ERR_MES(1,4)
        IERR = IERR1
      END IF
      IF (IERR2.NE.0) THEN
        CALL ERR_MES(1,5)
        IERR = IERR2
      END IF

C---- CHECK AN, SAN, SEQ IN COLS. 67-79
      CALL SEQ_CHECK(ISEQ,ACNUM,INTSAN,NSEQ)

C---- CHECK ALTERATION CODE
  400 CALL ALTER_FLAG(IALT,ID,INTSAN,IERR2)
      IF(IERR2.NE.0) CALL FLAGIT(1,ARROW,80,1,IERR) ! Set error markers

      IF(ID.NE.0 .AND. IERR.NE.0)
     *  CALL ERR_ARW(KEY,IPTR,KARD,ISEQ,IALT,ARROW,IERR) 

      RETURN

C---- End-of-file return.
  900 RETURN 1
 
 8000 FORMAT(A80)
      END
      SUBROUTINE SEQ_CHECK(ISEQ,ACNUM,NSAN,NSEQ)
	save !zvv:2019-07-18

C* Checks AN, SAN, sequence # in columns 67-79 against expected.
C*   ISEQ:  sequence #, columns 67-79 (A13)
C*   ACNUM: current AN (A5)
C*   NSAN:  current SAN (I3)
C*   NSEQ:  current sequence # (I5)

C---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*13 ISEQ
      CHARACTER*5 ACNUM

      IERRS = 0                       ! Initialize sequence error flag

C---- CHECK AN
c      write (*,*) '-zvv-4 ISEQ=',ISEQ(1:5),' ACNUM=',ACNUM
c      write (*,*) '___1___ISEQ=[',ISEQ,']'
c     *	,' ACNUM=',ACNUM,' NSAN=',NSAN,' NSEQ=',NSEQ
      IF(ISEQ(1:5).EQ.ACNUM) GOTO 300
      IF (ACNUM(2:5).EQ.'9999') THEN         ! END LABEL
        IF(ISEQ(2:5).EQ.'9999' .AND. ISEQ(1:1).GE.ACNUM(1:1)) GOTO 300
      ELSE IF (ACNUM(2:5).EQ.'0000') THEN    ! LABEL
        IF(ISEQ(2:5).EQ.'0000' .AND. ISEQ(1:1).GE.ACNUM(1:1)) GOTO 300
      END IF
      CALL FLAGIT(1,ARROW,67,5,IERRS)        ! SET ERROR MARKERS

C---- CHECK SAN
  300 NOWSAN = INTFORM(ISEQ(6:8),3,NONO,1)
c      write (*,*) '___3___ISEQ=',ISEQ(6:8),' SAN:',NOWSAN,' N=',NONO
      IF(NONO.NE.0) GOTO 390
      IF(NOWSAN.EQ.NSAN) GOTO 400
  390 CALL FLAGIT(1,ARROW,72,3,IERRS)        ! SET ERROR MARKERS

C---- CHECK SEQUENCE #
  400 NOWSEQ = INTFORM(ISEQ(9:13),5,NONO,1)
c      write (*,*) '___4___NOWSEQ:',NOWSEQ,' NSEQ=',NSEQ
      IF(NONO.NE.0) GOTO 490
      IF(NOWSEQ.EQ.NSEQ) GOTO 500
      NSEQ = NOWSEQ
  490 CALL FLAGIT(1,ARROW,75,5,IERRS)        ! Set error markers

  500 IF (IERRS.NE.0) THEN                   ! Error found
c      write (*,*) '___5___NOWSEQ=',NOWSEQ
c     *	,' NSEQ=',NSEQ,' NONO=',NONO,' IERRS=',IERRS
        CALL ERR_MES(1,3)                      ! Error message 
        IERR = IERRS
      END IF

      RETURN
      END
      SUBROUTINE SYS_CHECK(KEY2,ID,IDLST)
	save !zvv:2019-07-18

C* Looks up code in tables of System Identifiers, BIB Keywords, and 
C*   Data Headings.
C* Input to routine
C*   KEY2:  code to be checked
C* Output from routine
C*   ID:    -2: data heading
C*          -1: BIB keyword
C*           0: no ID found
C*          >0: index of System ID found
C*   IDLST: index of last record

C---- Keyword table. (Set in DICT2)
      COMMON/KEYTAB/NKEY,KEYWD(100),KEYEQ(100),KSTAT(100),KREQ(100),
     *              KD(200)
        CHARACTER*10 KEYWD
        CHARACTER*3 KSTAT
        CHARACTER KREQ

C---- System identifier table. (Set in DICT1)
      COMMON/SYSTAB/NSYS,SYSID(30),INDX_SYS(30)
        CHARACTER*10 SYSID

      CHARACTER*80 DLINE
      CHARACTER*10 KEY2

      IDLST = ID                     ! Save ID of last record

      I = LOOKUP(KEY2,SYSID,NSYS)    ! Look up code in System ID table

      IF (I.NE.0) THEN               ! System ID found
        ID = INDX_SYS(I)               ! Set ID#
        IF (ID.GT.17) THEN               ! File LABELS
          ID = MOD(ID,2)+1                 ! Reset ID #
        ELSE IF (ID.EQ.15) THEN          ! DATA
c-zvv-??          IF (IDLST.EQ.15) ID = -2         ! last was DATA, reset for heading.
          IF ((IDLST.EQ.15).or.(IDLST.EQ.-2)) ID = -2         ! last was DATA, reset for heading.
        END IF

      ELSE                           ! Not System ID
        I = LOOKUP(KEY2,KEYWD,NKEY)    ! Look up code in BIB Keyword table
        IF (I.NE.0) THEN               ! BIB Keyword found
          ID = -1
        ELSE
          CALL DANGET_NEW(24,KEY2,0,0,DLINE,IERR) 
                                           ! Look up code in Data Headings
          IF (IERR.EQ.0) THEN              ! Data Heading found
            ID = -2
          ELSE                             ! No match on keyword
            ID = 0
          END IF
        END IF

      END IF
      RETURN

      END
      SUBROUTINE SYS_LOCATE(ID,LOOK,IMSNG,ISECND,*,*)
	save !zvv:2019-07-18

C* Checks System ID code against code expected.
C* Input to routine
C*   ID:     index of code found; 0 = not found
C*   LOOK:   index of code expected
C* Output from routine
C*   IMSNG:  1 = missing  or out-of-sequence keyword 
C*   ISECND: 1 = second bad ID
C* RETURN normal return for other ID found
C*        1: correct System ID found
C*        2: no ID found

      IF(ID.EQ.LOOK) GOTO 600         ! Expected system ID found

      IF(ID.EQ.0) GOTO 700            ! Not legal keyword

C---- For SUBENT, BIB, COMMON, DATA check for NO----.
      IF(LOOK.NE.6 .AND. LOOK.NE.9 .AND. LOOK.NE.12 .AND. LOOK.NE.15)
     * GOTO 700
      IF (ID.NE.LOOK+1) GOTO 700

C---- Correct system ID found
  600 IMSNG = 0            ! Turn off missing flag
      ISECND = 0           ! Reset second try flag
      RETURN 1

C---- Correct System ID not found.
  700 IF (IMSNG.EQ.0 .AND. ISECND.EQ.0) THEN  ! First incorrect ID
        IMSNG = 1                               ! Set correct ID missing
        IF (ID.EQ.0) RETURN 2                     ! not System ID
        RETURN
      ELSE                                      ! Last ID incorrect
        ISECND = 1
        RETURN
      END IF
      END
      SUBROUTINE SYSGET(ID,IDLST,*)
	save !zvv:2019-07-18

C* Searches input file until a System Identier is found
C*   ID:    index of System ID found
C*   IDLST: index of last record read
C*   RETURN 1: end-of-file found

C---- READ NEXT RECORD. LOOK UP IN TABLE OF SYSTEM IDS.
  100 IDLST = ID
      CALL READ_NEXT(2,ID,*500)
      IF(ID.EQ.0)GOTO 100
      RETURN
C---- END-OF-FILE FOUND.
  500 RETURN 1
      END
