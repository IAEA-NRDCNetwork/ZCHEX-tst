      PROGRAM CHEX
!**
!* Written by Victoria McLane
!*            National Nuclear Data Center
!*            Brookhaven National Laboratory
!*            Upton, NY 11973
!**
!* Version 2001-2 (May 2001)
!*   PASS2 updates for lower case names.
!* Version 2000-2 (December 2000)
!*   PASS2 updates.
!* Version 2000-1 (June 2000)
!*   Updated to process 30 character dictionary keys.
!* Version 99-4 (November 1999) 
!*   Updated PASS1
!* Version 99-3 (July 1999) 
!*   Updated PASS1
!* Version 99-2 (June 1999) 
!*   Updated PASS2
!* Version 99-1 (January 1999) 
!*   Updated PASS2,ERR_SUB
!* Version 98-2 (July 1998) 
!*   Updated ERR_SUB
!* Version 98-1 (May 1998)  NEW CODE
!*   CHEX rewritten as multipass code.
!**
!* Uses DANIEL data base on logical DANIEL$LIBRARY
!**
!* Checks an input file in the EXFOR transmission format.
!* Makes two passes on file.  Fatal errors on first pass will cause program 
!* to terminate.  Fatal errors:
!* - missing, illegal or out-of-order System Identifier
!* - illegal or obsolete BIB Keyword
!* - illegal or obsolete Data Heading or Unit
!* - headings and units out of order 
!* - unequal number of Data Headings vs. Units vs. data fields
!* - more than 18 data fields
!* - illegal floating point number in data field
!* - empty section
!* - incomplete file
!**
!* Load with:
!*   SA0:[DICTION.PROGRAMS]DANIEL_NEW (DANGET,DANGET_STA,DANORD,DANVER,
!*                         DANVER_STA)
!*   CSISRS$PROG:UTILTY_SUB (DATER,INCRMT,FILHOL,FILLIN,INTFORM,INCRMT,
!*                           INCRMT_NEW,LOOKIN,LOOKUP,PARSCHR,PARS_CHR,RESTORE)
!**
!* List of subroutines:
!*
!*   CLOSUP: Closes files. Writes counts on error listing.
!*   DICT1: Reads System Identifier Dictionary. Stores System ID and internal
!*      numerical equivalent
!*   DICT2: Reads BIB Keyword Dictionary. Stores BIB Keywords and status
!*   OPENUP: Sets input and output files.
!**

!---- Fatal error flag. (Initialized in PASS1).
      COMMON/ENDITALL/ISTOP,IS

!---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC

!---- Today's date. (Set in MAIN)
!----   IYMD: Today's date as integer (YYMMDD)
      COMMON/TODAY/IYMD

      CHARACTER*50 OPENIN
      CHARACTER*11 DATE

      ISTOP = 0                        ! Initialize fatal error flag

      CALL DATER(DATE,IYMD)            ! Set dates
      write (*,5000) DATE                ! Write terminal heading
      CALL DICT1                         ! Read System Identifiers
      CALL DICT2                         ! Read BIB Keywords
      CALL OPENUP(OPENIN)                ! Open files, set operating mode
      WRITE(IOUT,5000) DATE              ! Write error list heading

!**** Pass 1

      write (*,5100)

      IPASS = 1

      CALL PASS1

      IF (ISTOP.EQ.0) THEN
        WRITE(IOUT,9100)
        write (*,9200)
        CLOSE(UNIT=IN)                       ! Close input file
        CALL ADD_LINES(57)                   ! Start new page
        WRITE(IOUT,9150)
      ELSE
        WRITE(IOUT,9000)
        GOTO 999
      END IF

!**** Pass 2      

      OPEN(UNIT=IN,ACCESS='SEQUENTIAL',FILE=OPENIN,STATUS='OLD')! Reopen input file
c_g77     *     READONLY)                         ! Reopen input file
      CALL PASS2

  999 CALL CLOSUP                            ! Close processing

 5000 FORMAT(/' CHEX (Version 2000-2) run on ',A11/1X,40('-'))
 5100 FORMAT(/' - First pass checking -')
 9000 FORMAT(/' *** MAJOR ERRORS FOUND *** must be corrected before ',
     *       'checking will continue')
 9100 FORMAT(/'   First pass completed with no fatal errors')
 9150 FORMAT(' - Second pass checking -'/)
 9200 FORMAT('   First pass completed with no fatal errors'//
     *         ' - Second pass checking -')
      END
      SUBROUTINE CLOSUP

!* Closes files. Writes counts on error listing.

!---- Total counts for records, entries, subentries. 
!---- (Initialized in PASS1: ALTER_FLAG)
      COMMON/COUNTS/KOUNT(4)

!---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC

!---- Line count for page
      COMMON/LCOUNT/LINES

!---- If no entries on vector common file delete.
      IF(IFVEC.EQ.0) THEN
c-absoft77        CLOSE(UNIT=IVEC,DISPOSE='DELETE')
        CLOSE(UNIT=IVEC)
      ELSE 
        write (*,9000)
      END IF

!---- WRITE OUT STATISTICS FOR TAPE.
      IF(LINES.GT.50) WRITE(IOUT,1100)   ! START NEW PAGE
      WRITE(IOUT,1000) KOUNT             ! WRITE COUNTS

c      CALL EXIT
      STOP

 1000 FORMAT(//' Tape statistics'//
     1       '   # of records-------------> ',I6/
     2       '   # of entries-------------> ',I6/
     3       '   # of subentries----------> ',I6/
     4       '   # of new data subentries-> ',I6/)
 1100 FORMAT(1H1)
 9000 FORMAT(//' **** Vector COMMON input file produced'/)
      END
      SUBROUTINE DICT1

!* Reads System Identifier Dictionary. Stores System ID and internal
!* numerical equivalent.

!---- System identifier table. (Set in DICT1)
!-      NSYS:     # of system IDs stored
!-      SYSID:    table of stored System IDs
!----   INDX_SYS: internal numerical equivalent for System ID
      COMMON/SYSTAB/NSYS,SYSID(30),INDX_SYS(30)
        CHARACTER*10 SYSID

      CHARACTER*30 KEY
      CHARACTER*80 LINE

!*    READ IN SYSTEM ID (UP TO MAX. OF 30)

      CALL DANORD_NEW(1,'F',0,LINE,KEY,IERR,*990) ! READ FIRST RECORD
      IF(IERR.NE.0) GOTO 990
      NSYS = 1                                  ! INITIALIZE STORED ARRAY INDEX

  200 SYSID(NSYS) = KEY(1:10)                   ! STORE SYSTEM ID
      READ(LINE(1:9),5000) INDX_SYS(NSYS)       ! STORE EQUIVALENT

      CALL DANORD_NEW(1,'N',0,LINE,KEY,IERR,*800) ! READ NEXT RECORD
      IF(IERR.NE.0) RETURN                      ! TERMINATE READ IF ERROR SET
      CALL INCRMT(NSYS,30,*900)                 ! INCREMENT STORED ARRAY INDEX
      GOTO 200

  800 RETURN                    

  900 write (*,*) ' **** More than 30 System Identifiers in dictionary'
c      CALL EXIT
      STOP

  990 write (*,*) ' **** DANORD error, execution terminated'
c      CALL EXIT
      STOP

 5000 FORMAT(I9)
      END
      SUBROUTINE DICT2

!* Reads BIB Keyword Dictionary. Stores Keyword and Status

!---- Keyword table. (Set in DICT2)
!-      NKEY:   # of keywords stored
!-      KEYWD:  stored BIB keywords
!-      KEYEQ:  numerical equivalent for keywords
!-      KSTAT:  status of BIB keywords
!-      KREQ:   required keyword flags
!-      KODED:  required code flags
!-      KDICT:  dictionary for code
      COMMON/KEYTAB/NKEY,KEYWD(100),KEYEQ(100),KSTAT(100),KREQ(100),
     *              KODED(100),KDICT(100)
        CHARACTER*10 KEYWD
        CHARACTER*3 KSTAT
        CHARACTER KREQ,KCOD

      CHARACTER*3 ISTAT
      CHARACTER*30 KEY
      CHARACTER*80 LINE

!*    READ IN SYSTEM ID (UP TO MAX. OF 30)

      CALL DANORD_STA_NEW(2,'F',0,LINE,KEY,ISTAT,IERR,*990) ! READ FIRST RECORD
      IF(IERR.NE.0) GOTO 990
      NKEY = 1                                  ! INITIALIZE STORED ARRAY INDEX

  200 KEYWD(NKEY) = KEY(1:10)                   ! Store System ID
      READ(LINE(27:28),1000) KEYEQ(NKEY)        ! Store numerical equivalent
      KREQ(NKEY) = LINE(26:26)                  ! Store keyword required flag
      KSTAT(NKEY) = ISTAT
      READ(LINE(30:32),2000) KDICT(NKEY)        ! Store numerical equivalent

      IF (LINE(29:29).EQ.'R') THEN              ! Code required
        KODED(NKEY) = 0
      ELSE                                      ! No code
        KODED(NKEY) = -1
      END IF

      CALL DANORD_STA_NEW(2,'N',0,LINE,KEY,ISTAT,IERR,*800)  ! READ NEXT RECORD
      IF(IERR.NE.0) RETURN                       ! TERMINATE READ IF ERROR SET
      CALL INCRMT(NKEY,100,*900)                 ! INCREMENT STORED ARRAY INDEX
      GOTO 200

  800 RETURN                    

  900 write (*,*) ' **** More than 100 BIB Keywords in dictionary'
c      CALL EXIT
      STOP

  990 write (*,*) ' **** DANORD error, execution terminated'
c      CALL EXIT
      STOP

 1000 FORMAT(I2)
 2000 FORMAT(I3)
      END
      SUBROUTINE OPENUP(OPENIN)

!*  Sets input and output files.
!*    OPENIN: Output file specs.

!---- I/O units, vector common flag. (Set in OPENUP)
!-      IN: input unit
!-      IOUT: output unit
!-      IVEC: vector common proceesing file unit
!----   IFVEC: vector common flag (1 = vector common found on input file)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC

!---- Operating mode. (Set in OPENUP)
!-      ITYPE: input file type
!-             0  =  data processing
!----          1  =  transmission
      COMMON/OPMODE/ITYPE,IWARNG

      CHARACTER*50 OPENIN,OPENOUT
      CHARACTER*50 strTmp
      CHARACTER TYP,NOYES
      CHARACTER TYPE(2)/'D','T'/

      DATA IN,IOUT,IVEC/28,29,27/        ! Data unit numbers
      DATA IFVEC,IWARNG/2*0/             ! Vector common found flag

!---- Open input file
      write (*,1000)
c_g77    read (*,2000) OPENIN
      OPENIN='a.x4'
      read (*,2000) strTmp
      if (strTmp.ne.' ') OPENIN=strTmp
  150 OPEN(UNIT=IN,ACCESS='SEQUENTIAL',FILE=OPENIN,STATUS='OLD',
     *     ERR=900)
c_g77       ,READONLY)

!---- Open output file
      write (*,3000)
c_g77    read (*,2000) OPENOUT
      OPENOUT='a.err'
      read (*,2000) strTmp
      if (strTmp.ne.' ') OPENOUT=strTmp
      OPEN(UNIT = IOUT,ACCESS = 'SEQUENTIAL',FILE = OPENOUT,
     * STATUS='unknown')

!---- Open vector common input file.
      write (*,3100)
      read (*,2000) OPENOUT
      IF(OPENOUT.NE.' ') OPEN(UNIT = IVEC,ACCESS = 'SEQUENTIAL',
     * FILE = OPENOUT,STATUS='unknown')

!---- Set format type.
  400 write (*,4000)
      read (*,4100) TYP
      CALL UPSTR(TYP)                    ! Convert to upper case
      if (TYP.eq.' ') TYP='D'
      ITYPE = LOOKUP(TYP,TYPE,2)         ! Check legal format type
      IF (ITYPE.EQ.0) THEN               ! Illegal
        write (*,4050)
        GOTO400
      ELSE IF (ITYPE.GT.2) THEN
        ITYPE = ITYPE-2
      END IF

!---- Warnings to be output?
      write (*,5000)
      read (*,4100) NOYES
      CALL UPSTR(NOYES)                  ! Convert to upper case
      IF (NOYES.NE.'N') IWARNG = 1
      RETURN

!---- Input file not found, reread
  900 write (*,9000)
      read (*,2000) OPENIN
      IF(OPENIN.NE.' ') GOTO 150
c      CALL EXIT
      STOP

 1000 FORMAT(/$,' Input file specs (a.x4)------------> ')
 2000 FORMAT(A50)
 3000 FORMAT(/$,' Output file specs (a.err)----------> ')
 3100 FORMAT(/$,' VECOM file specs-------------------> ')
 4000 FORMAT(/$,' File type: Data proc or Trans (D)--> ')
 4050 FORMAT('  **** Illegal code')
 4100 FORMAT(A1)
 5000 FORMAT(/$,' Print warning messages (Y/N)-------> ')
 9000 FORMAT(/' *** Input file not found'/
     *       $,'    Enter new file or <CR> to exit--> ')
      END
