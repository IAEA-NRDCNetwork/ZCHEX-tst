      PROGRAM ZCHEX
!**Title  : Program ZCHEX
!**Purpose: Checking EXFOR file: format and content
!**OS     : VAX-VMS, Windows, Linux, MacOS
!**Authors:
!*   1998-2001. DEC-Fortran
!*         Originally written on VMS by:
!*         Ms. Victoria McLane
!*         National Nuclear Data Center
!*         Brookhaven National Laboratory
!*         Upton, NY 11973
!*         USA
!*   2002-2023. MS-Windows, Linux, MacOS
!*         Further development by:
!*         Dr. Viktor Zerkin, v.zerkin@iaea.org
!*         Nuclear Data Section
!*         International Atomic Energy Agency
!*         A-1400 Vienna
!*         Austria
!______________________________________________________________________________
!
!**Updates since 2002
!*         2023-12-27   V.Zerkin
!*                      - process new STATUS with REFERENCE-code, e.g.:
!*                        (TABLE,,D.M.Chittenden+,P,ORO-367,1,196101)
!*         2023-07-18   V.Zerkin
!*                      - DANLO: adapted for using new Dict.227
!*         2023-05-29   V.Zerkin
!*                      - ZORDER debug: restore line ALT flag
!*         2023-05-18   V.Zerkin
!*                      - ZORDER: right column nsub with leading zero's
!*         2023-04-25   V.Zerkin
!*                      - improved indication of conflict Dict236.ResFlg=[.] with in SF4:must be empty
!*                      - indication of: Common|Data.ResonanceParam requires SF58.ReacType:RP=[.] in Dict-236
!*                      - Windows/Linux/MacOS: EXFOR Dictionary 9127
!*         2022-06-09   V.Zerkin
!*                      - improved checking with "Family flags" (Dict-24)
!*                      - debug: suppress output "^^^^^^^^^^" for blank lines if "no warnings"
!*                        (example:23327006)
!*         2021-06-18   V.Zerkin
!*                      - fixed a bug: crash on long code [,POL/DA/DA/DE,*/*/*,ANA]
!*         2021-05-24   V.Zerkin
!*                      - correctly checking (MONIT1..9) in DECAY-MON
!*         2021-05-21   V.Zerkin
!*                      - correctly checking muptiple code, e.g. ADD-RES:(DECAY,G-SPEC,STRUC)
!*         2021-05-14   V.Zerkin
!*                      - Dict236:[,SIG,,RES]: correctly accepting [EN-RES]
!*         2020-12-04   V.Zerkin
!*                      - DECAY-DATA: checking DG Abundance given without Energy,
!*                        Energy < 1.keV, Abundance > 2.(200%)
!*                      - debug: allowed multiple appearance of the same correlation
!*                        flags in the ERR-ANALYS codes, e.g. (ERR-4,,,F)
!*                      - INSTITUTE: allowed leading blank in the 2nd, 3rd, etc. lines
!*                      - statistics: output number of warnings and errors
!*                      - define flag “Print warnings” in the command line
!*         2020-06-29   V.Zerkin
!*                      - checking #Entry accession number: must be increasing
!*         2019-07-19   V.Zerkin
!*                      - checking Units in Subent-1 vs. expected Data:Units(Reaction)
!*         2019-03-07   V.Zerkin
!*                      - debug under gfortran: MacOS, Linux, Windows
!*         2019-01-10   V.Zerkin
!*                      - MacOS-10.14.2(Mojave), gfortran-8.2.0, EXFOR Dictionary 9119
!*         2018-04-20   V.Zerkin
!*                      - checking SF9 for obsolete codes (EXP)
!*         2017-11-16   V.Zerkin
!*                      - correct checking IAS-NUMB in the keyword LEVEL-PROP
!*                      - Windows/Linux: EXFOR Dictionary 9116
!*         2017-05-18   V.Zerkin
!*                      - checking correlation flag in the 4th field of ERR-ANALYS
!*                        ERR-ANALYS (ERR-4,1.,2.,U)
!*         2017-05-17   V.Zerkin
!*                      - debug checking: Reference long(L=11) Report-code, e.g.:
!*                        REFERENCE  (R,IAEA-TECDOC-1211,2001)
!*                        MONIT-REF  (,S.M.Qaim+,R,IAEA-TECDOC-1211,2001)
!*                        REL-REF    (N,,S.M.Qaim+,R,IAEA-TECDOC-1211,2001)
!*         2017-05-10   V.Zerkin
!*                      - debug checking of ERR-ANALYS code: limit by (...)
!*                        e.g. 33080002:
!*                        ERR-ANALYS (MONIT-ERR,2.,3.7) 56Fe(n,p)56Mn
!*         2015-05-14   V.Zerkin
!*                      - set required DATA Units "E" for Reaction-Ratio: E2/E
!*         2012-03-21   V.Zerkin
!*                      - resonance flag is defined only in the dict-236 (not dict-213)
!*         2011-04-21   V.Zerkin
!*                      - suppress error message for MONIT_REF referring to 
!*                        an evaluated library
!*         2011-01-31   V.Zerkin
!*                      - increasing maximum limit of DECAY-FLAG
!*                      - debug to avoid crash (ZPASS2:call PARS_CHR)
!*         2011-01-07   V.Zerkin
!*                      - checking obsolete flags for INC-SOURCE and DETECTOR
!*         2010-03-30   V.Zerkin
!*                      - debugging: completed checking new ERR-ANALYS format
!*                      - introduced definition of input file name in command line
!*                      - standard procedure to run from Windows-desktop
!*         2009-10-07   V.McLane
!*                      - recognize NPART in SF4
!*                      - check that author's name is longer than one character
!*                        (to find commas after initials)
!*                      - allow alphabetic characters in reference page number
!*                      - check coding for new ERR-ANALYS format (not completed)
!*         2007-10-02   V.Zerkin
!*                      - debugging: correct processing of N1,N2 in DATA and COMMON
!*         2007-06-08   V.Zerkin
!*                      - Windows/Linux: EXFOR Dictionary 9094
!*                      - Wildcards for SF7 during cheching of SF5-8:
!*                        improved algorithm - now uses all Dictionary-33 and 227;
!*                        (full test and Dict-236 having only wildcards were
!*                         prepared by O.Schwerer)
!*                      - debugging (according to S.Maev, 2007-06-06 e-mail)
!*                      - adopted to MacOS-X (using g77 fortran compiler)
!*         2006-12-12   V.Zerkin
!*                      - finds missing '(' in REACTION keyword
!*                      - allows isomer extensions "-L","-L1","-L2"
!*                        in REACTION and DECAY codes
!*                      - Wildcards for SF7 during cheching of SF5-8:
!*                        done using Dictionary-33 (works, but still under testing)
!*                      - indication of SAN in the right column of error-message
!*                      - input file name is indicated in the error file and 
!*                        on the user's terminal
!*                      - length of input file name is expanded from 50 to 65
!*         2005-12      V.Zerkin
!*                      - debugging: correct work in TRANS mode
!*         2005-09      V.Zerkin
!*                      - EXFOR Dictionaries were re-organized (9188):
!*                        7-->7+207, 13-->213, 27-->227+209, 36-->236
!*                      - debugging: correct checking N1, N2 in ENDENTRY, DATA
!*         2005-04      V.Zerkin
!*                      - debugging: usage of Status-field in Dictionaries
!*         2004-05      V.Zerkin: recompiled on Linux with Fortran:
!*                      - Lahey/Fujitsu Fortran 95 Compiler Release L6.20b
!*         2002-05      V.Zerkin: Ported from VMS to Windows, Linux:
!*                      - Dictionaries were re-organized to direct-access files;
!*                      - New program DANLO written to create new Dictionaries
!*                        from dictionaries-backup file (DAN_BACK_NEW.XXXX)
!*                      - Subroutines to access new Dictionaries
!*         2002-03      V.McLane: Provided source code on VMS (DEC-Fortran)
!**
!______________________________________________________________________________
!
!**History and Notes on Alpha-VMS (1998-2001):
!*
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
!* Makes two passes on file.
!* Fatal errors on first pass will cause program to terminate:
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
!______________________________________________________________________________
!

!---- Fatal error flag. (Initialized in PASS1).
      COMMON/ENDITALL/ISTOP,IS

!---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC

!---- Today's date. (Set in MAIN)
!----   IYMD: Today's date as integer (YYMMDD)
      COMMON/TODAY/IYMD

      COMMON/COUNTS/KOUNT(4)
      COMMON/zCOUNTS/kountE,kountS
      COMMON/errCOUNTS/kountErr,kountWarn
      data kountE,kountS/0,0/
      data kountErr,kountWarn/0,0/

      CHARACTER*65 OPENIN
      CHARACTER*11 DATE
      CHARACTER myTYP/'D'/
      CHARACTER myWARN/'N'/

      OPENIN='a.x4'
c-zvv	write (*,*) 'iargc()=',iargc()
      if (iargc().ge.1) call getarg(1,OPENIN)
      if (iargc().ge.2) call getarg(2,myTYP)
      if (iargc().ge.3) call getarg(3,myWARN)
	icmd=iargc() ! default taken from command line

      ISTOP=0                            ! Initialize fatal error flag

      CALL DATER(DATE,IYMD)              ! Set dates
      write (*,5000) DATE                ! Write terminal heading
      CALL DICT1                         ! Read System Identifiers
      CALL DICT2                         ! Read BIB Keywords
c-zvv2010      CALL OPENUP(OPENIN)       ! Open files, set operating mode
      CALL OPENUP(OPENIN,icmd,myTYP,myWARN)! Open files, set operating mode
      WRITE(IOUT,5000) DATE              ! Write error list heading
      WRITE(IOUT,*) ' '
      WRITE(IOUT,*) 'Input file: ',OPENIN! Write file name

!**** Pass 1

      write (*,5100)

      IPASS = 1

      CALL PASS1
c      WRITE(*,*) 'kountE=',kountE

      IF (ISTOP.EQ.0) THEN
        WRITE(IOUT,9100)
        write (*,9200)
        CLOSE(UNIT=IN)                       ! Close input file
        CALL ADD_LINES(57)                   ! Start new page
        WRITE(IOUT,9150)
      ELSE
        WRITE(IOUT,9000)
        WRITE(IOUT,*) '______ISTOP=',ISTOP
        WRITE(*,*)    '______ISTOP=',ISTOP
        GOTO 999
      END IF

!**** Pass 2      
      KOUNT(2)=0

      OPEN(UNIT=IN,ACCESS='SEQUENTIAL',FILE=OPENIN,STATUS='OLD')! Reopen input file
c_g77     *     READONLY)                         ! Reopen input file
      CALL PASS2

  999 CALL CLOSUP                            ! Close processing
c5000 FORMAT(/' ZCHEX (Version 2011-04-28) run on ',A11/1X,40('-'))
c5000 FORMAT(/' ZCHEX (Version 2012-03-21) run on ',A11/1X,40('-'))
c5000 FORMAT(/' ZCHEX (Ver-2015-05-14) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2017-05-18) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2017-11-16) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2018-04-20) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2019-01-10) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2019-03-07) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2019-07-19) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2020-06-29) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2020-12-01) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2020-12-04) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2020-12-14) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2021-05-14) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2021-05-21) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2021-05-24) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2021-06-18) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2021-07-04) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2021-12-29) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2022-06-09) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2023-04-21) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2023-04-24) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2023-04-25) run on ',A11/1X,41('-'))
c5000 FORMAT(/' ZCHEX (Ver-2023-07-18) run on ',A11/1X,41('-'))
 5000 FORMAT(/' ZCHEX (Ver-2023-12-27) run on ',A11/1X,41('-'))
 5100 FORMAT(/' - First pass checking -')
 9000 FORMAT(/' *** MAJOR ERRORS FOUND *** must be corrected before ',
     *       'checking will continue')
 9100 FORMAT(/'   First pass completed with no fatal errors')
 9150 FORMAT(' - Second pass checking -'/)
 9200 FORMAT('   First pass completed with no fatal errors'//
     *         ' - Second pass checking -')
      END




      SUBROUTINE CLOSUP
	save !zvv:2019-07-18

!* Closes files. Writes counts on error listing.

!---- Total counts for records, entries, subentries. 
!---- (Initialized in PASS1: ALTER_FLAG)
      COMMON/COUNTS/KOUNT(4)
      COMMON/zCOUNTS/kountE,kountS
      COMMON/errCOUNTS/kountErr,kountWarn

!---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC

!---- Line count for page
      COMMON/LCOUNT/LINES

      COMMON/ENDITALL/ISTOP,IS

!---- If no entries on vector common file delete.
      IF(IFVEC.EQ.0) THEN
c-absoft77        CLOSE(UNIT=IVEC,DISPOSE='DELETE')
        CLOSE(UNIT=IVEC)
      ELSE 
        write (*,9000)
      END IF

!---- WRITE OUT STATISTICS FOR TAPE.
      IF(LINES.GT.50) WRITE(IOUT,1100)   ! START NEW PAGE
      WRITE(IOUT,1000) KOUNT,kountE,kountWarn,kountErr ! WRITE COUNTS
      WRITE(*,1000) KOUNT,kountE,kountWarn,kountErr    ! TYPE COUNTS
!      WRITE(*,*) 'kountErr=',kountErr,' kountWarn=',kountWarn

c     CALL EXIT
!     STOP
      call EXIT(ISTOP)

 1000 FORMAT(//' Tape statistics'//
     1       '   # of records-------------> ',I8/
     2       '   # of entries-------------> ',I8/
     3       '   # of subentries----------> ',I8/
     4       '   # of new data subentries-> ',I8/
     5       '   # of new entries---------> ',I8/
     6       '   # of warnings------------> ',I8/
     7       '   # of errors--------------> ',I8/)
 1100 FORMAT(1H1)
 9000 FORMAT(//' **** Vector COMMON input file produced'/)
      END




      SUBROUTINE DICT1
	save !zvv:2019-07-18

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
	save !zvv:2019-07-18

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




c-zvv2010      SUBROUTINE OPENUP(OPENIN)
      SUBROUTINE OPENUP(OPENIN,icmd,myTYP,myWARN)
	save !zvv:2019-07-18

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
	COMMON/search227/iSearch227

      CHARACTER*65 OPENIN,OPENOUT
      CHARACTER*65 strTmp
      CHARACTER TYP,NOYES
      CHARACTER myTYP,myWARN
      CHARACTER TYPE(2)/'D','T'/

      DATA IN,IOUT,IVEC/28,29,27/        ! Data unit numbers
      DATA IFVEC,IWARNG/2*0/             ! Vector common found flag

      OPENOUT='a.err'
      strTmp=' '
	if (icmd.ne.0) then
c?	    lll=mylen(OPENIN)
	    lll=len(trim(OPENIN))
	    OPENOUT=OPENIN(1:lll)
	    OPENOUT(lll+1:lll+4)='.err'
c	    return
	endif
	iErrInput=0

!---- Open input file
  111 continue
c?      write (*,*) 'Default input file: ',OPENIN(1:mylen(OPENIN))
      write (*,*) 'Default input file: ',OPENIN(1:len(trim(OPENIN)))
c     write (*,*) 'Default input file: ',OPENIN
      write (*,1000) 
c_g77    read (*,2000) OPENIN
c-zvv      OPENIN='a.x4'
      if ((icmd.eq.0).or.(iErrInput.ne.0)) read (*,2000) strTmp
      if (strTmp.ne.' ') OPENIN=strTmp
      write (*,*) 'Input file: ',OPENIN
  150 OPEN(UNIT=IN,ACCESS='SEQUENTIAL',FILE=OPENIN,STATUS='OLD',
     *     ERR=900)
c_g77       ,READONLY)


!---- Open output file
c?	    lll=mylen(OPENIN)
	    lll=len(trim(OPENIN))
	    OPENOUT=OPENIN(1:lll)
	    OPENOUT(lll+1:lll+4)='.err'
	    strTmp=' '

c?      write (*,*) 'Default output file: ',OPENOUT(1:mylen(OPENOUT))
      write (*,*) 'Default output file: ',OPENOUT(1:len(trim(OPENOUT)))
      write (*,3000)
c_g77    read (*,2000) OPENOUT
      if (icmd.eq.0)      read (*,2000) strTmp
      if (strTmp.ne.' ') OPENOUT=strTmp
      write (*,*) 'Output file: ',OPENOUT
      OPEN(UNIT = IOUT,ACCESS = 'SEQUENTIAL',FILE=OPENOUT,
     * STATUS='unknown')

!---- Open vector common input file.
      write (*,3100)
	OPENOUT=' '
      if (icmd.eq.0)      read (*,2000) OPENOUT
      IF(OPENOUT.NE.' ') OPEN(UNIT = IVEC,ACCESS = 'SEQUENTIAL',
     * FILE = OPENOUT,STATUS='unknown')

!---- Set format type.
  400 write (*,4000)
	TYP='T'
	TYP='D'
	TYP=myTYP
      if (icmd.eq.0)      read (*,4100) TYP
      CALL UPSTR(TYP)                    ! Convert to upper case
      if (TYP.eq.' ') TYP='D'
      ITYPE = LOOKUP(TYP,TYPE,2)         ! Check legal format type
      IF (ITYPE.EQ.0) THEN               ! Illegal
        write (*,4050)
        GOTO400
      ELSE IF (ITYPE.GT.2) THEN
        ITYPE = ITYPE-2
      END IF
      write (*,*) 'Type=[',TYP,']'

!---- Warnings to be output?
      write (*,5000)
	NOYES='N'
	NOYES=myWARN
      if (icmd.eq.0)      read (*,4100) NOYES
      CALL UPSTR(NOYES)                  ! Convert to upper case
      IF (NOYES.NE.'N') IWARNG = 1
!?2020	IWARNG=1
      write (*,*) 'IWARNG=[',IWARNG,']'

!---- To check Dict227 for SF7?
c      write (*,6000)
c      read (*,4100) NOYES
c      CALL UPSTR(NOYES)                  ! Convert to upper case
c      IF (NOYES.EQ.'Y') iSearch227=1
      iSearch227=1 !always search in 227

c	if (iSearch227.ne.0)
c     +	write (*,*) 'Using Dict-227 for checking SF7 -> long time!'

      RETURN

!---- Input file not found, reread
c  900 continue
c      write (*,9000)
c      read (*,2000) OPENIN
c      IF(OPENIN.NE.' ') GOTO 150
  900 continue
      write (*,9001) OPENIN
	iErrInput=1
      if (icmd.ne.0) STOP
      GOTO 111
c      CALL EXIT
      STOP

c1000 FORMAT(/$,' Input file specs (a.x4)-------------> ')
c1000 FORMAT(/$,' Input file specs -------------------> ')
 1000 FORMAT($,' Input file specs -------------------> ')
 2000 FORMAT(A65)
c3000 FORMAT(/$,' Output file specs (a.err)-----------> ')
c3000 FORMAT(/$,' Output file specs-------------------> ')
 3000 FORMAT($,' Output file specs-------------------> ')
 3100 FORMAT(/$,' VECOM file specs--------------------> ')
 4000 FORMAT(/$,' File type: Data proc or Trans (D)---> ')
 4050 FORMAT('  **** Illegal code')
 4100 FORMAT(A1)
 5000 FORMAT(/$,' Print warning messages (Y/N)--------> ')
 6000 FORMAT(/$,' Use Dict-227 for checking SF7 (Y/N)-> ')
 9000 FORMAT(/' *** Input file not found: ',A65/
     *       $,'    Enter new file or <CR> to exit--> ')
 9001 FORMAT(/' *** Input file not found: ',A65)
      END
