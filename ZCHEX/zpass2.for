      SUBROUTINE PASS2
!**
!* Written by Victoria McLane
!*            National Nuclear Data Center
!*            Brookhaven National Laboratory
!*            Upton, NY 11973
!**
!* Version 2001-1 (May 2001)
!*   Updated for upper and lower case names.
!* Version 2000-2 (December 2000)
!*   Miscellaneous updates.
!* Version 2000-1 (May 2000)
!*   Added processing for multiple particles in EN-SEC.
!*   Corrections to LAB_CHECK, REACTION, LVL-PROP, STAT-CHECK, others
!* Version 99-2 (June 1999) 
!*   Added processing for LVL-PROP.
!*   Corrected end-of-file processing.
!*   Termimated DATA error list when # of errors for section is > 200.
!*   Corrected several error messages.
!* Version 99-1 (January 1999) 
!*   Corrected 
!*   . processing of resolution error units; 
!*   . error message for nuclide SF1 warning 
!* Version 98-1 (May 1998)  MAJOR REVISION
!*   CHEX rewritten as multipass code.
!**
!* Pass 2 checks
!*   validity of bib codes
!*   presence of required BIB keywords
!*   presemce of required data headings
!*   missing data headings or data fields
!*   validity of data flags
!*   validity of pointers
!*   monotonicity of data
!*   for transmission mode, also checks:
!*     Record ID (Columns 67-79)
!**
!* Uses DANIEL data base on logical DANIEL$LIBRARY
!**
!* Load with:
!*   SA0:[DICTION.PROGRAMS]DANIEL_NEW (DANGET,DANGET_STA,DANORD,DANVER,
!*                         DANVER_STA)
!*   CSISRS$PROG:UTILTY_SUB (DATER,INCRMT,FILHOL,FILLIN,INTFORM,INCRMT,
!*                           INCRMT_NEW,LOOKIN,LOOKUP,PARSCHR,PARS_CHR,RESTORE)
!*
!* List of subroutines used:
!*
!*   ADD_HEAD: Add heading and pointer to list of headings to be found in 
!*      COMMON or DATA, if not already in list.
!*   ADD_LIST: Adds word to array given.
!*   AN_GET: Sets accession number.  Initializes flags for entry.
!*      Entry SAN_GET sets subaccession number.
!*   AUTH_CHECK: Checks the BIB keyword AUTHOR
!*   BIBSUB: Processes BIB section.
!*   CHAR_NAME: Checks for legal characters in name.
!*   CHECK_HEAD: Checks if heading and pointer in list of stored headings
!*   CODE_CHECK: Checks BIB codes with standard coding format
!*   CROS_CHECK: Stores independent variable field families.
!*      Checks that required independent variables are found
!*        and that all independent variables found are required.
!*      Checks bib and data pointers.
!*      If REACTION pointers exist, checks that -
!*        there is more than one REACTION pointer
!*        there are DATA pointers for all REACTION pointers
!*        all BIB pointers match REACTION pointers
!*        all DATA pointers match REACTION pointers
!*      If REACTION pointers do not exist, checks that -
!*        there are no DATA pointers
!*        there is more than one BIB pointer
!*   DATASUB: Processes the DATA section
!*   DECAY: Checks the keywords DECAY-DATA, DECAY-MON and RAD-DET.
!*   DICT13: Reads Reaction Type Dictionary. Stores Reaction Type and family
!*      code equivalents
!*   EN_SEC: Checks the BIB keywords EN-SEC, HALF-LIFE, MISC, ERR-ANALYS,
!*      EMS-SEC, and MOM-SEC
!*   FACIL_CHECK: Checks the bib KEYWORD FACILITY
!*   FAMGET: Finds independent variables required for reaction type given.
!*      IWAY:  1 = look up code and store independent variables required
!*            >1 = look up code
!*   FIELD_CHECK: Checks data field headings.
!*   FIELD_COUNT: Checks that data exists for all headings and that all data
!*      fields have headings.
!*   FLAG_CHECK: Checks BIB keyword FLAG coding and the FLAG portion of the
!*      BIB keyword DECAY-DATA. Stores data heading in list of required 
!*      headings.
!*   GENQM: Removes general quantity modifier from reaction.
!*      Stores family codes for data headings. Checks for required data 
!*      headings.
!*   GET_EXT: Find heading extension.
!*   HEAD_PROC: Reads records, checks if Data Heading or Units. 
!*       For data headings, stores family codes, checks for required headings.
!*   HIST_CHECK: Checks code string for BIB keywords HISTORY and EXP-YEAR.
!*   LAB_CHECK: Checks the BIB keyword INSTITUTE
!*   LEAD_BLANK: Flags illegal leading blanks
!*   LVL_PROP: Checks code for keyword LEVEL-PROP
!*   MONOT: Checks that all independent variables types are present, and that
!*       at least one data field is present.  Checks for monotonicity in data.
!*       Checks for legal FLAG fields.
!*   NUCFLD: Checks nuclide field codes
!*   PARSER: Scans code string for REACTION, MONITOR, and ASSUMED RECORDS.
!*       Checks each subfield.
!*   PART_CHECK: Checks the BIB keyword PART-DET and the particle field of
!*       DECAY-DATA and DECAY-MON.
!*   REACTION: Checks the keywords REACTION, MONITOR and ASSUMED.
!*   READ_CONT: Reads record. Checks for continuation record. Writes error
!*        messages for previous record.
!*   REF_CHECK: Checks the keywords REFERENCE, REL-REF, and MONIT-REF
!*   REF_CODE: Stores report code and looks up in dictionary.
!*   REF_DATE: Stores reference date and checks.
!*   REF_PAGE: Checks reference page field.
!*   REF_PART: Checks reference field contained in parentheses
!*   REL_REF: Checks keyword REL-REF
!*      ENTRY MONIT_REF: Checks keyword MONIT-REF.
!*   QUANT_CHECK: Checks quantity part of REACTION string.
!*   STAT_CHECK: Checks coding for BIB keyword STATUS
!*   TEST_HEAD: Checks data heading code for BIB keywords MONITOR, ASSUMED,
!*       and REL-REF
!*   UNIT_CHECK: Checks units versus data headings
!**

!---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

      DATA ILAB/0/

c      CALL DICT13                        ! Read reaction type dictionary
      CALL DICT213                        ! Read reaction type dictionary

!     File LABEL processing

      CALL READ_REC(ID)                  ! Read label
      IF(ID.GT.2.AND.ID.LT.18) GOTO 400  ! No label, assume ENTRY

      ILAB = ID                          ! Store Label ID

!     ENTRY record processing

  300 CALL READ_REC(ID)                  ! Read next record

!     SUBENT record processing

  400 CALL READ_REC(ID)                  ! Read next record

      IF(ID.EQ.7) GOTO 900               ! Branch for NOSUBENT

!     BIB Section processing

  500 CALL READ_REC(ID)                  ! Read next record

      CALL BIBSUB(ID,*600,*1000)         ! Initialize & process BIB 

!     COMMON section processing

  600 CALL READ_REC(ID)                  ! Read next record

      IF (INTSAN.EQ.1) THEN
        LDX = 1
      ELSE
        LDX = 2
      END IF
      CALL DATASUB(LDX,ID,*700,*1000)    ! Process COMMON section

!     DATA section processing

  700 IF (INTSAN.NE.1) THEN              ! Subentry 1
        CALL READ_REC(ID)                  ! Read next record
        IF(ID.EQ.16) GOTO 800              ! Branch for NODATA

        CALL DATASUB(3,ID,*800,*1000)      ! Process DATA section
      END IF

!     End of subentry

  800 CALL READ_REC(ID)                  ! Read ENDSUBENT

  900 CALL READ_REC(ID)                  ! Read SUBENT or ENDENTRY
      IF (ID.EQ.6) THEN                  ! SUBENT
        GOTO 500 
      ELSE IF (ID.EQ.7) THEN             ! NOSUBENT
        GOTO 900
      ELSE                               ! ENDENTRY
        CALL READ_REC(ID)                  ! Read ENTRY or END label
        IF (ID.NE.ILAB+1) GOTO 400
      END IF

 1000 RETURN       
      END

      SUBROUTINE ADD_HEAD(NEW,IPTR,*)

!* Add heading and pointer if not already on list.
!*   NEW:  word to be added
!*   IPTR: current pointer
!*   RETURN 1: not in table

!---- Stored headings from BIB codes. (Initialized in BIB_SUB)
      COMMON/BIB_HEAD/NHT,NH1,BHEAD(36),NBPTR(36),NBPTR1(36),
     *                BHPTR(20,36),MATHED(20,36)
        CHARACTER BHPTR
        CHARACTER*10 BHEAD

      CHARACTER*10 NEW
      CHARACTER IPTR

      IF (NHT.NE.0) THEN              ! List already contains heading(s)
        L = LOOKUP(NEW,BHEAD,NHT)       ! Look up heading
        IF (L.NE.0) GOTO 200            ! Branch if already in list
      END IF

!---- Heading not in list, add.
      CALL ADD_LIST(BHEAD,36,NHT,NH1,NEW,'HEADINGS  ',*900) 
      L = NHT                                ! Initialize for 1st entry
      GOTO 600

!---- Heading in list
  200 LP = LOOKUP(IPTR,BHPTR(1,L),NBPTR(L))  ! Look for matching pointer
      IF(LP.NE.0) RETURN                     ! Heading/pointer in list

!---- Pointer not in list for heading
  600 CALL ADD_LIST(BHPTR(1,L),50,NBPTR(L),NBPTR1(L),IPTR,
     *   'HEADINGS  ',*900)                  ! Add pointer to list for heading

  900 RETURN 1
      END      
      SUBROUTINE ADD_LIST(KARD,MAX,NUM,NUM1,NEW,MARRAY,*)

!* Adds word to array given. If array is full, error message is written 
!* and array is not updated.
!*   KARD:   array to be updated
!*   MAX:    maximum size of array
!*   NUM:    current array index
!*   NUM1:   # in array found in subentry 1
!*   NEW:    word to be added
!*   MARRAY: array name
!*   RETURN 1: array full

!---- Current AN, SAN, section. (Set in READ_REC)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

      CHARACTER*10 MARRAY
      CHARACTER*(*) NEW,KARD(MAX)

      CALL INCRMT(NUM,MAX,*200)          ! Increment # of entries in list
      IF(INTSAN.EQ.1) NUM1 = NUM
                                         ! Increment # of entries in SAN 1 list
      KARD(NUM) = NEW                    ! Add string to list
      RETURN

!---- Array is full. 
  200 write (*,1000) MARRAY,MAX               ! Write message
      RETURN 1

 1000 FORMAT(/' ***** Unable to update array ',A10,' Max = ',I5/)
      END
      SUBROUTINE AN_GET(ACNUM,INTSAN)

!* Sets accession number.  Initializes flags for entry.
!*   ACNUM:  hollerith accession number (A5)
!*   INTSAN: integer subaccession number (I3)

!---- Input record. (Set in READ_NEXT)
      COMMON/ALLCOM/KEY2,IPTR,KARD,ISEQ
        CHARACTER*10 KEY2
        CHARACTER*1  IPTR
        CHARACTER*55 KARD
        CHARACTER*13 ISEQ

!---- Operating mode. (Set in OPENUP)
      COMMON/OPMODE/ITYPE,IWARNG

      CHARACTER*5 ACNUM,ANSUB

      INTSAN = 0                       ! Initialize SAN for entry
      ACNUM(1:5) = KARD(7:11)          ! Store AN
      CALL PUTAN(ACNUM)                ! Write AN on output (ERRSUB entry)

      RETURN

      ENTRY SAN_GET(ACNUM,INTSAN)

!*    SET SUBACCESSION NUMBER AND CHECK.
!*    RESETS FLAGS FOR SUBENTRY.
!*      ACNUM: HOLLERITH ACCESSION NUMBER (A5)
!*      INTSAN: INTEGER SUBACCESSION NUMBER (I3)
!*              NEGATIVE IF SUBENT NOT READ (DO INITIALIZATION ONLY)

!---- Store accession # and subaccession # from N1 field.
      ANSUB(1:5) = KARD(4:8)
      INTSAN = INTFORM(KARD(9:11),3,IGOOF,1) ! Form subentry # for comparison

      IF(ITYPE.NE.1) ISEQ(6:8) = KARD(9:11)
      RETURN
      END
      SUBROUTINE AUTH_CHECK(KARD,NOW,NBIBR,*)

!* Checks the BIB keyword AUTHOR:
!*   for legal characters
!*   that string on line ends with ',' or ')'
!*   that code string ends with ')'
!* Does not check for embedded blanks after initial blank.
!*   KARD:  input array (columns 12-66)
!*   NOW:   position in input array
!*   NBIBR: # of records read in BIB section
!*   RETURN 1: next record read

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*55 KARD
      CHARACTER DELIM(2)/ ',', ')'/

      IEND = 0                           ! Turn off end-of-code-string flag

  100 LASTC = 0                          ! Initialize last comma index
      MAX = 55-NOW+1                     ! Set max. # of characters in string

!---- Find next author string
  120 CALL PARSCHR(KARD,NOW,LST,N,MAX,DELIM,2,ICHR,*200)

      CALL CHAR_NAME(2,KARD,NOW,LST)     ! Check for legal characters
      NOW = LST+2                        ! Reset current position in string

      IF (ICHR.EQ.2) THEN                ! End of code string
        IEND = 1                           ! Set flag
        RETURN
      ELSE IF (ICHR.EQ.1) THEN           ! End of next author
        LASTC = LST+1                      ! Store position of last comma
        GOTO 120
      END IF

!---- END-OF-CODE STRING NOT FOUND
  200 IF (LASTC.EQ.0) THEN               ! No legal author found
        CALL ERR_MES(1,34)                 ! Error message 
        CALL FLAGIT(1,ARROW,13,54,IERR)
        GOTO 950
      ELSE IF (LASTC.NE.55) THEN         ! Not at end of line
        IF(KARD(LASTC+1:55).NE.' ') THEN   ! Characters after last comma
          NUM = LST-LASTC                 
          CALL ERR_MES(1,35)               ! Error message 
          CALL FLAGIT(1,ARROW,LASTC+12,NUM,IERR)
        END IF
      END IF

      CALL READ_CONT(1,NOW,*830)         ! Read next record

      IF (KARD(1:1).EQ.'(') THEN         ! Illegal left paren in col.12
        NOW = 2
        CALL ERR_MES(1,33)                 ! Error message 
        CALL FLAGIT(1,ARROW,12,1,IERR)
      END IF

      NBIBR = NBIBR+1                    ! Increment # of BIB records read
      GOTO 100

  830 RETURN 1                           ! Return for new keyword read

  950 RETURN
      END
      SUBROUTINE BIBSUB(ID,*)

!* Processes BIB section.
!*   ID:     System ID numerical equivalent
!*           9 = BIB
!*           10 = NOBIB
!*   RETURN 1: ENDBIB read

!---- Input record. (Set in READ_REC)
      COMMON/ALLCOM/KEY2,IPTR,KARD,ISEQ
        CHARACTER*10 KEY2
        CHARACTER*1  IPTR
        CHARACTER*55 KARD
        CHARACTER*13 ISEQ

!---- Current AN, SAN, section. (Set in READ_REC)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

!---- Stored flags from BIB for FLAG (1) and DECAY-DATA (2). (See FLAG_CHECK)
      COMMON/BIB_FLAG/NF1(3),NFT(3),FLAGS(200,3),FPTR(200,3),
     * MATFLG(200,3)
        CHARACTER FPTR

!---- Stored headings from BIB codes. (Initialized in BIB_SUB)
!-      NHT:    total 3 of stored headings                    (Set in ADD_HEAD)
!-      NH1:    # of heading stored from SAN 1                (Set in ADD_HEAD)
!-      BHEAD:  headings from BIB expected in COMMON or DATA  (Set in ADD_HEAD)
!-      BHPTR:  heading pointers from BIB                     (Set in ADD_HEAD)
!-      NBPTR:  total # of pointers for each stored heading   (Set in ADD_HEAD)
!-      NBPTR1: # of pointers for each heading from SAN 1     (Set in ADD_HEAD)
!-      MATHED: flags match found with Data heading         (Set in HEAD_PROC)
      COMMON/BIB_HEAD/NHT,NH1,BHEAD(36),NBPTR(36),NBPTR1(36),
     *                BHPTR(20,36),MATHED(20,36)
        CHARACTER BHPTR
        CHARACTER*10 BHEAD

!---- Stored BIB pointers (Initialized in BIB_SUB)
!-      NPB:    # of BIB pointers
!-      NPB1:   # of BIB pointers in SAN 1
!-      BIBPTR: stored BIB pointers
      COMMON/BIB_PTR/NPB,NPB1,BIBPTR(20)
        CHARACTER BIBPTR

!---- Independent variable special treatment flags
!-      SPECIN: Special treatment flags, set to SAN if found
!-              1: numerator/denominator flag (Set in REACTION)
!-              2: average resoance parameter flag (Set in QUANT_CHECK)
!-              3: kT (Set in UNIT_CHECK)
!----           4: PAR,NU
      COMMON/EN_PROC/SPECIN(4)
        INTEGER SPECIN

!---- Error pointers. (Initialized in BIBSUB)
!-      IERR: ERROR FLAG, 0 = NO ERROR
!-      ARROW: ERROR COLUMN MARKERS (^)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

!---- Independent variable families required. (Initialized in BIB_SUB)
!-      NEEDS: independent variables expected, set to SAN of reaction if
!-             expected, set to negative SAN if found.
!-             See FAMGET for contents. (set in CROS_CHECK,FAMGET,PARSER)
!-      NEEDFL: Monitor expected flag, set to SAN of monitor if expected,
!-              Set to negative SAN if found. (Set in PARSER,REACTION)
!-      NOTCK: Flag for independent variable check, do not check if
!-             set to 1 (Set in PARSER,REACTION)
      COMMON/FAMLY_REQ/NEEDS(7),NEEDFL,NOTCK

!---- Keyword table. (Set in DICT2)
      COMMON/KEYTAB/NKEY,KEYWD(100),KEYEQ(100),KSTAT(100),KREQ(100),
     *              KODED(100),KDICT(100)
        CHARACTER*10 KEYWD
        CHARACTER*3 KSTAT
        CHARACTER KREQ

!---- Number of monitors (Initialized in BIB_SUB).
!-      NMON1:  # of monitors in SAN 1                        (Set in REACTION)
!-      NMON:   total # of monitors                           (Set in REACTION)
      COMMON/MON_HEAD/NMON1,NMON

!---- Stored pointers for REACTION (Initialized in BIB_SUB)
!-      NPR:    # of REACTION pointers
!-      NPR1:   # of REACTION pointers in SAN 1
!-      RACPTR: stored REACTION pointers
!----   RUNIT:  code for REACTION units expected
      COMMON/REAC_PTR/NPR,NPR1,RACPTR(20),RUNIT(20)
        CHARACTER RACPTR
        CHARACTER*4 RUNIT

      CHARACTER LPTR
      INTEGER SW(100),SW4,THISAN(100),ISPSDD

!---- Initialize data error flags to blank.
      DATA ARROW/' '/
c_g77      DATA IERR,ISPSDD/2*0/

!---- Initialize
      KIND = 0                       ! Keyword type
      NBIBR = 0                      ! # of BIB records
      NKEYR = 0                      ! # of keywords
      NOW = 1                        ! Position in array KARD
      CALL FILLIN(THISAN,NKEY,0)     ! Keyword in SAN
      IERR  = 0                      ! Error flag
      IERRP = 0                      ! Pointer error flag
      IERRD = 0                      ! Duplicate keyword error flag

      IF (INTSAN.EQ.1) THEN          ! SAN 1
        CALL FILLIN(SW,NKEY,0)         ! required keyword flags
        SW4 = 0                        ! one of codes needed
        ISPSDD = 0                     ! superseded data flag
        CALL FILLIN(NEEDS,7,0)         ! independent variable flags
        NEEDFL = 0                     ! MONITOR needed flag
        CALL FILLIN(SPECIN,4,0)        ! special energy flags
        NPB = 0                        ! total # of BIB pointers
        NPB1 = 0                       ! # of SAN 1 BIB pointers

!----   Initialize # of stored data flags
        CALL FILLIN(NF1,2,0)
        CALL FILLIN(NFT,2,0)

!----   Initialize for stored headings
        IF (NHT.NE.0) THEN      
          CALL FILLIN(MATHED,720,0)             ! Headings
          IF (NH1.NE.0) CALL FILLIN(NBPTR1,NH1,0) ! Heading pointers
          IF (NHT.NE.0) CALL FILLIN(NBPTR,NHT,0)  ! Heading pointers
          NH1 = 0
          NHT = 0
          NMON1 = 0
          NMON = 0
        END IF

!----   Initialize stored REACTION pointers
        NPR = 0
        NPR1 = 0

      ELSE                               ! Data subentries
        DO 65 I = 1,NKEY                   ! Reset required keyword flags
          IF(SW(I).NE.1) SW(I) = 0
   65   CONTINUE
        IF(SW4.NE.1) SW4 = 0             ! One of codes needed flag
        IF(ISPSDD.NE.1) ISPSDD = 0         ! Reset superseded data flag

        DO 70 I = 1,7                    ! Reset independent variable flags
          IF(NEEDS(I).NE.1 .AND. NEEDS(I).NE.-1) NEEDS(I) = 0
   70   CONTINUE
        IF(NEEDFL.NE.1) NEEDFL = 0       ! Reset MONITOR needed flag
        DO 80 I = 1,4                    ! Reset special energy flags
          IF(SPECIN(I).NE.1) SPECIN(I) = 0   
   80   CONTINUE
        NPB = NPB1                       ! reset to # of BIB pointers in SAN 1

!----   Initialize # of stored data flags for subentry.
        DO 90 J = 1,2
          IF(NFT(J).EQ.0 .OR. NF1(J).EQ.NFT(J)) GOTO 90
          DO 85 I = 1,NFT(J)
            IF(MATFLG(I,J).NE.1) MATFLG(I,J) = 0
   85     CONTINUE
          NFT(J) = NF1(J)
   90   CONTINUE

!----   Initialize stored headings for subentry.
        IF (NHT.NE.0) THEN
          DO 100 I = 1,NHT
            DO 105 J = NBPTR1(I)+1,NBPTR(I)
              MATHED(J,I) = 0
  105       CONTINUE  
          NBPTR(I) = NBPTR1(I)
  100     CONTINUE
          NHT = NH1
          NMON = NMON1
        END IF

        NPR = NPR1                       ! Reset stored REACTION pointers 

      END IF

      IF (ID.EQ.10) RETURN 1

      CALL READ_REC(ID)                  ! Read first record

  210 NBIBR = NBIBR+1                    ! Increment # of BIB records found
      IK = LOOKUP(KEY2,KEYWD,NKEY)       ! Look up keyword in table
      IDB = KEYEQ(IK)
c	write (*,*) '-zvv-5 KEY2=',KEY2(1:10),' NKEY=',NKEY,' IK=',IK
c     *	,' IDB=',IDB
c     *	,' KEYWD=',KEYWD

      LPTR = IPTR                        ! Store pointer
      NKEYR = NKEYR+1                    ! Increment # of keywords found

      IF (THISAN(IK).NE.INTSAN) THEN     ! Keyword not already used in this SAN
        IF(SW(IK).EQ.0) SW(IK) = INTSAN    ! Set required keyword found
        THISAN(IK) = INTSAN                ! Set keyword found in current SAN
        GOTO 217
      END IF

!---- Set keyword type using lengths of tables as indicator
  217 CONTINUE

  240 IF (IDB.NE.30) THEN                ! Not REACTION, store pointers
        IF (IPTR.NE.' ') THEN              ! Nonblank pointer
          IF (NPB.NE.0) THEN                 ! Pointers already in list
            N = LOOKUP(IPTR,BIBPTR,NPB)        ! Look for duplicate 
            IF(N.NE.0) GOTO 245                ! Branch for duplicate
          END IF
          CALL ADD_LIST(BIBPTR,20,NPB,NPB1,IPTR,'BIBPTR    ',*245)
                                             ! Add pointer to list
        END IF
      END IF

  245 IF(KARD.EQ.' ') THEN               ! Blank columns 12 - 66
        CALL ERR_WARN(1,11)                ! Warning message 
        CALL FLAGIT(1,ARROW,12,55,IERR)
        GOTO 700
      END IF

!---- Check for coded information

  300 IF(KARD(1:1).NE.'(') GOTO 500
      IF(KODED(IK).EQ.0) KODED(IK) = 1   ! Set koded info found

      NOW = 2

C     Nonobligatory keywords with coded information

c	write (*,*) '-zvv-5 IDB=',IDB

      IF (IDB.EQ.32) THEN                  ! Process ASSUMED
        CALL REACTION(3,NOW,LPTR,*500,*805)
      ELSE IF (IDB.EQ.38) THEN             ! Process REL-REF
        CALL REL_REF(NOW)
      ELSE IF (IDB.EQ.37) THEN             ! Process MONITOR-REF
        CALL MONIT_REF(NOW)                  ! (ENTRY in REL_REF)
      ELSE IF (IDB.GE.33 .AND. IDB.LE.35) THEN  
                                          ! Process DECAY-DATA,DECAY-MON,RAD-DET
        CALL DECAY(IDB,KARD,NOW,LPTR,*805)
      ELSE IF (IDB.EQ.29) THEN             ! Process PART-DET
        CALL PART_CHECK(KARD,NOW)
      ELSE IF (IDB.EQ.18) THEN             ! Process EN-SEC
        CALL EN_SEC(1,7,KARD,NOW,LPTR,*500)
      ELSE IF (IDB.EQ.23) THEN             ! Process HALF-LIFE
        CALL EN_SEC(2,5,KARD,NOW,LPTR,*500)
      ELSE IF (IDB.EQ.14) THEN             ! Process INC-SOURCE
        CALL SOURCE_CHECK(KARD,NOW)
      ELSE IF (IDB.EQ.4) THEN              ! Process EXP-YEAR
        CALL HIST_CHECK(KARD,NOW)
      ELSE IF (IDB.EQ.24) THEN             ! Process MISC-COL
        CALL EN_SEC(3,0,KARD,NOW,LPTR,*500)
      ELSE IF (IDB.EQ.36) THEN             ! Process ADD-RES
        CALL CODE_CHECK(KARD,NOW,20,2)
      ELSE IF (IDB.EQ.42) THEN             ! Process FLAG
        CALL FLAG_CHECK(1,KARD,NOW,LPTR,*500) 
      ELSE IF (IDB.EQ.45) THEN             ! Process LEVEL-PROP
        CALL LVL_PROP(KARD,NOW,LPTR)
      ELSE IF (IDB.EQ.19) THEN             ! Process EMS-SEC
        CALL EN_SEC(5,7,KARD,NOW,LPTR,*500)
      ELSE IF (IDB.EQ.17) THEN             ! Process MOM-SEC
        CALL EN_SEC(6,7,KARD,NOW,LPTR,*500)
      ELSE IF (IDB.EQ.39) THEN             ! Process RESULT
        CALL CODE_CHECK(KARD,NOW,37,2)
      ELSE IF (IDB.EQ.22) THEN             ! Process COVARIANCE
        CALL CODE_CHECK(KARD,NOW,-1,1)
      
C      Obligatory keywords

      ELSE IF (IDB.EQ.1) THEN              ! Process TITLE 
        CALL ERR_MES(1,33)                   ! Error message 
        CALL FLAGIT(1,ARROW,12,1,IERR)
      ELSE IF (IDB.EQ.2) THEN              ! Process AUTHOR
        CALL AUTH_CHECK(KARD,NOW,NBIBR,*210)
      ELSE IF (IDB.EQ.3) THEN              ! Process INSTITUTE
        CALL LAB_CHECK(KARD,NOW,NBIBR,*210)
      ELSE IF (IDB.EQ.5) THEN              ! Process REFERENCE
        CALL REF_CHECK(KARD,NOW)
      ELSE IF (IDB.EQ.30) THEN             ! Process REACTION
        CALL REACTION(1,NOW,LPTR,*805)
      ELSE IF (IDB.EQ.31) THEN             ! Process MONITOR
        CALL REACTION(2,NOW,LPTR,*805)
      ELSE IF (IDB.EQ.21) THEN             ! Process ERR-ANALYS
        CALL EN_SEC(4,0,KARD,NOW,LPTR,*500)
      ELSE IF (IDB.EQ.28) THEN             ! Process HISTORY
        CALL HIST_CHECK(KARD,NOW)
      ELSE IF (IDB.EQ.27) THEN             ! Process STATUS
        CALL STAT_CHECK(KARD,NOW,INTSAN,ISPSDD)
      ELSE IF (IDB.EQ.11) THEN             ! Process FACILITY
        CALL FACIL_CHECK(KARD,NOW)
        SW4 = 1
      ELSE IF (IDB.GE.10 .AND. IDB.LE.13) THEN ! Check METHOD,DETCTOR,ANALYSIS
        CALL CODE_CHECK(KARD,NOW,KDICT(IK),2)
        SW4 = 1
      END IF

  500 IF(IERR.EQ.0 .AND. IERRP.EQ.0 .AND. IERRD.EQ.0) GOTO 800

  700 CALL ERR_ARW2(KEY2,IPTR,KARD,ISEQ,ARROW,IERR) 
                                          ! Write record with error pointers

  800 CALL READ_REC(ID)                   ! Read next record
      IF (ID.GT.0) GOTO 900
      IERRP = 0                           ! Pointer error flag
      IERRD = 0                           ! Duplicate keyword error flag
      NOW = 1                             ! Position in array kard

  805 IF (KEY2.EQ.' ') THEN               ! Continuation record
        NBIBR = NBIBR+1                     ! Increase # of BIB records read
        IF (IPTR.NE.' ') THEN               ! New pointer
          IF (IPTR.EQ.LPTR) THEN              ! Duplicate pointer
            CALL ERR_MES(1,59)                  ! Error message 
            CALL FLAGIT(1,ARROW,11,1,IERRP)
          END IF
          LPTR = IPTR                     ! Set current pointer
        END IF
        GOTO 240

      ELSE                                ! New key
        IF (KREQ(IK).EQ.'R') THEN ! REQUIRED CODES.
          IF (KODED(IK).EQ.0) THEN          ! Code missing on last
            CALL ERR_INSERT(1,19,10,KEYWD(IK))
          ELSE                             
            IF(KODED(IK).GT.0) KODED(IK) = 0  ! Reset code to not found
          END IF
        END IF
      END IF
      GOTO 210

!     End of BIB section

  900 IF (INTSAN.GT.1) THEN

!     BIB completeness check for DATA subentry

        IF(NEEDFL.NE.0) SW(31) = NEEDFL 
                               ! MONITOR not relevant, set MONITOR flag to found

!----   Skip for superseded data, nonstandard entries.
        IF(ISPSDD.NE.0) GOTO 930
        IF(ACNUM.GT.'11000' .AND. ACNUM.LT.'12700') GOTO 930
        IF(ACNUM.GE.'20000') GOTO 930

!----   Check if required keywords found
        DO 925 I = 1,NKEY
          IF (SW(I).EQ.0) THEN
            IF (KREQ(I).EQ.'R') CALL ERR_INSERT(1,33,10,KEYWD(I)) 
            IF (KREQ(I).EQ.'X') CALL ERR_INSERT(1,34,10,KEYWD(I)) 
          END IF
  925   CONTINUE

!----   One of keywords METHOD,DETECTOR,ANALYSIS,FACILITY must be present
        IF (SW4.EQ.0) CALL ERR_MES(2,51)      ! Missing 

      END IF

  930 RETURN 1
      END
      SUBROUTINE CHAR_NAME(IWAY,KARD,NOW,LST)

!* Checks free text.
!*   IWAY: 2 = author's name (AUTHOR keyword)
!*         3 = author's family name (REFERENCE keyword)
!*         4 = author's name (REL-REF and MONIT-REF keywords)
!*   KARD: input array
!*   NOW:  starting position in array
!*   LST:  final position to be checked

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*55 KARD

      CHARACTER LEGAUT(5)/'+', '''', '-', '.', ','/

      IF(NOW.GT.55) RETURN

      IF (NOW.EQ.1) THEN                ! First column of free text record
        IF (KARD(1:1).EQ.'(') THEN        ! Contains left paren
          CALL ERR_MES(1,33)                ! Error message 
          CALL FLAGIT(1,ARROW,12,1,IERR)
        END IF
      END IF

      IFND = 0                          ! Initialize illegal character flag
      DO 300 I = NOW,LST                ! Check for legal characters

        IF(KARD(I:I).EQ.' ') GOTO 300
        IF(KARD(I:I).GE.'A' .AND. KARD(I:I).LE.'Z') GOTO 300
        IF(KARD(I:I).GE.'a' .AND. KARD(I:I).LE.'z') GOTO 300

        IF (IWAY.EQ.2) THEN             ! Author's name (AUTHOR)
          IC=LOOKUP(KARD(I:I),LEGAUT(2),4)
        ELSE IF (IWAY.EQ.3) THEN        ! Author's family name
          IC=LOOKUP(KARD(I:I),LEGAUT(2),2)
        ELSE IF (IWAY.EQ.4) THEN        ! Author's name (REL-REF + MONIT-REF)
          IC=LOOKUP(KARD(I:I),LEGAUT,4)
        END IF

        IF(IC.EQ.0) CALL FLAGIT(1,ARROW,I+11,1,IFND) 
                                        ! Set error markers, illegal character
  300 CONTINUE

      IF (IFND.NE.0) THEN               ! Error found
        IERR=1                            ! Set error flag
        CALL ERR_MES(1,15)                ! Error message 
      END IF
      RETURN
      END
      SUBROUTINE CHECK_HEAD(NEW,IPTR,NOW,*)

!* Checks if heading and pointer are in list; if pointer to be matched
!* is blank, skips pointer check.
!*   NEW:   word to be added
!*   IPTR:  current pointer
!*   NOW:   starting position of heading
!*
!*   RETURN 1: match found

!---- Current AN, SAN, Section. (Set in READ_REC)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

!---- Stored headings from BIB codes. (Initialized in BIB_SUB)
      COMMON/BIB_HEAD/NHT,NH1,BHEAD(36),NBPTR(36),NBPTR1(36),
     *                BHPTR(20,36),MATHED(20,36)
        CHARACTER BHPTR
        CHARACTER*10 BHEAD

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*10 NEW,KEPT
      CHARACTER*3 NEXT(5)/'MAX', 'MIN', 'NM', 'DN', 'APR'/
      CHARACTER IPTR

      IF (NHT.EQ.0) RETURN                ! Empty list

      KEPT = NEW                          ! Store heading
      L = LOOKUP(KEPT,BHEAD,NHT)          ! Look up heading

      IF (L.EQ.0) THEN                    ! Heading not in list
        DO 150 I=10,2,-1                    ! Delete extension
          IF (KEPT(I:I).EQ.'-') THEN
            K = LOOKUP(KEPT(I+1:I+3),NEXT,5)
            IF (K.NE.0) KEPT(I:10) = ' '
            GOTO 160
          END IF
  150   CONTINUE
  160   IF (KEPT(1:1).EQ.'-' .OR. KEPT(1:1).EQ.'+') THEN
          KEPT(1:9) = KEPT(2:10)
          KEPT(10:10) = ' '
        END IF
        L = LOOKUP(KEPT,BHEAD,NHT)           ! Look up heading
        IF (L.EQ.0) RETURN                  
      END IF

!---- HEADING IN LIST

      IF (NBPTR(L).EQ.1) THEN               ! Only 1 BIB pointer for heading
        IF (BHPTR(1,L).EQ.' ') MATHED(1,L) = INTSAN ! Set match
      ELSE                                  ! More than 1 pointer
        LP = LOOKUP(IPTR,BHPTR(1,L),NBPTR(L)) ! Look for matching pointer
        IF (LP.NE.0) THEN                     ! Heading/pointer in list
          IF(MATHED(LP,L).EQ.1) GOTO 900        ! Pointer already found
          MATHED(LP,L) = INTSAN                 ! Set match
        ELSE                                  ! Pointer not in list
          IF (IPTR.EQ.' ') THEN                 ! Blank pointer
            DO 325 LP = 1,NBPTR(L)                ! Match on all stored pointers
              IF(MATHED(LP,L).EQ.1) GOTO 900        ! Pointer already found
              MATHED(LP,L) = INTSAN                 ! Set match
  325       CONTINUE
          ELSE                                  ! Non-blank pointer
            GOTO 900
          END IF
        END IF
      END IF
      RETURN 1

!---- More than one heading found for pointer
  900 CALL ERR_MES(1,39)                     ! Error message 
      CALL FLAGIT(1,ARROW,NOW,11,IERR)
      RETURN
      END      
      SUBROUTINE CODE_CHECK(KARD,NOW,KDIC,NCOD)

!* Checks BIB codes in standard coding format
!*   KARD: input array
!*   NOW:  starting position in array
!*   KDIC: dictionary number
!*         -1 = covariance (only code 'COVAR' legal)
!*         22 = detector
!*   NCOD: 1 = one code field allowed
!*         2 = more than one code field allowed

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*55 KARD
      CHARACTER*5 SPECOD(2),MULTI
      CHARACTER LIMTR(2)/',', ')'/

      INTEGER WHICH(2)

!---- Codes which require special processing for DETECTOR and INC-SOURCE
      DATA SPECOD/'COVAR','COIN '/
      
      DATA WHICH/-1,22/

      M = 0                            ! Turn off special code found flag
      NN = 0                           ! Initialize # of codes found
      MULTI = ' '                      ! Blank out current special code
      IERR1 = 0                        ! Turn off internal error flag

      I = LOOKIN(KDIC,1,WHICH,2)       ! See if dictionary has special code
      IF(I.NE.0) MULTI = SPECOD(I)     ! Store current special code

!---- Flag leading blanks
  200 IF(KARD(NOW:NOW).EQ.' ') CALL LEAD_BLANK(KARD,NOW,*910)

!---- Find next code
      CALL PARSCHR(KARD,NOW,KPT,NUM,20,LIMTR,2,ICHR,*800)
      IF (NUM.EQ.0) THEN                    ! Empty field
        CALL ERR_MES(1,34)                    ! Error message 
        CALL FLAGIT(1,ARROW,NOW+11,1,IERR)
         NOW = NOW+1
         GOTO 300
      END IF

      NN = NN+1

      IF (KDIC.LT.0) THEN                   ! Single code allowed for keyword
        IF(KARD(NOW:KPT).EQ.MULTI) RETURN     ! Check if required code
        CALL ERR_MES(1,35)                      ! Error message 
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)
        RETURN
      END IF                                 

      IF (KARD(NOW:KPT).EQ.MULTI) THEN      ! Special processing codes
        IF (NN.EQ.1) THEN                     ! In 1st position
          M = 1                                 ! Set special code found
        ELSE                                  ! Not in 1st position
          CALL ERR_MES(1,35)                    ! Message 
          CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)
        END IF      
      ELSE                                          
        CALL DANVER_NEW(KDIC,KARD(NOW:KPT),0,IERR1,*300) ! Look up code
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR1)
      END IF

  300 IF(ICHR.EQ.2) GOTO 900                ! Check for end of code string
      NOW = KPT+2                           ! Set to read after comma
      IF(NCOD.NE.1) GOTO 200                ! More than one code allowed

!---- End of code string not found
  800 CALL ERR_MES(1,36)
      CALL FLAGIT(1,ARROW,KPT+11,1,IERR1)
      GOTO 910

!---- End of code string found
  900 IF (M.NE.0 .AND. NN.EQ.1) THEN        ! Special code; no second code
        CALL ERR_WARN(1,6)                    ! Error message 
        CALL FLAGIT(0,ARROW,KPT+11,1,IERR)
        RETURN
      END IF

!---- If error flag set, print message.
  910 IF (IERR1.NE.0) THEN                  ! Errors found
        IERR = 1                              ! Set error flag
        CALL ERR_MES(1,35)                    ! Error message 
      END IF
      RETURN
      END
      SUBROUTINE CROS_CHECK(LDX,NFLD,NLINE)

!* Checks independent variable links to REACTION:
!*  - are required independent variables are found?
!*  - are all independent variables found required?
!* Checks if data heading defined in BIB are found in COMMON/DATA.
!* Checks BIB and DATA pointers.
!*   If non-blank reaction pointers exist, checks:
!*   - is there more than one REACTION pointer?
!*   - are there DATA pointers for all REACTION pointers?
!*   - do all BIB pointers match REACTION pointers?
!*   - do all DATA pointers match REACTION pointers?
!*   If REACTION pointers do not exist, checks:
!*   - are there are DATA pointers?
!*     if yes, checks for vector common.
!*   - is there more than one BIB pointer?
!**
!*   LDX:   Section index
!*          1 = Subentry 1 COMMON
!*          2 = Data subentry COMMON
!*          3 = DATA
!*   NFLD:  # of field headings
!*   NLINE: # of records/line

!---- Current AN, SAN, section. (Set in READ_REC)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

!---- Stored headings from BIB codes. (Initialized in BIB_SUB)
      COMMON/BIB_HEAD/NHT,NH1,BHEAD(36),NBPTR(36),NBPTR1(36),
     *                BHPTR(20,36),MATHED(20,36)
        CHARACTER BHPTR
        CHARACTER*10 BHEAD

!---- Stored BIB pointers (Initialized in BIB_SUB)
      COMMON/BIB_PTR/NPB,NPB1,BIBPTR(20)
        CHARACTER BIBPTR

!---- Stored headings for SAN 1 + current subentry. (Set in HEAD_PROC)
      COMMON/DATA_HEAD/NPD(3),HEADS(18,3),DATPTR(18,3)
        CHARACTER*11 HEADS
        CHARACTER DATPTR

!---- Independent variable special treatment flags
      COMMON/EN_PROC/SPECIN(4)
        INTEGER SPECIN

!---- Independent variable families required. (Initialized in BIB_SUB)
      COMMON/FAMLY_REQ/NEEDS(7),NEEDFL,NOTCK

!---- Family names for error messages. (Set in CROS_CHECK)
!-      MESFAM: independent variables for error messages
      COMMON/FAMMES/MESFAM(10)
        CHARACTER*10 MESFAM

!---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC

!---- Stored pointers for REACTION (Initialized in BIB_SUB)
      COMMON/REAC_PTR/NPR,NPR1,RACPTR(20),RUNIT(20)
        CHARACTER RACPTR
        CHARACTER*4 RUNIT

!---- Family codes for data fields.  (Set in UNIT_CHECK).
      COMMON/VAR_FIELD/INDFAM(18)

      CHARACTER*12 MESSKP/' '/
      INTEGER NCLINE

      DATA NCLINE/0/

      DATA MESFAM/'RES.ENERGY','ENERGY    ','SEC.ENERGY',
     *            'ANGLE     ','COEF.NUM. ','OTHER     ',
     *            'NUCLIDE   ','          ','          ',
     *            'DATA      '/

!*    Check for legal independent variables

      DO 180 I = 1,NFLD                         ! Loop over fields
        IF (INDFAM(I).LE.2 .OR. INDFAM(I).GT.10) GOTO 180             
                                                  ! Skip if not indep.var.
        N = INDFAM(I) - 2                         ! Set family index

        IF (INTSAN.NE.1) THEN                     ! Data subentry
          IF (NOTCK.EQ.0 .AND. NEEDS(N).EQ.0) THEN  ! Ind. var. not expected
            IF (N.EQ.2 .AND. SPECIN(2).NE.0) THEN     ! Avg.res.parameter
              IF (NEEDS(1).EQ.0) CALL ERR_INSERT(1,14,10,MESFAM(N))
                                                        ! Not energy
            ELSE IF (N.EQ.6) THEN
              CALL ERR_INSERT(1,28,10,HEADS(I,LDX))   ! Message to check use
            ELSE
              CALL ERR_INSERT(1,14,10,MESFAM(N))      ! Error message
            END IF                             
          END IF
          IF (NEEDS(N).EQ.-1) THEN
            CALL ERR_INSERT(1,16,10,MESFAM(N))      ! Error message
            GOTO 180
          END IF
        END IF
        NEEDS(N) = -INTSAN                      ! Set found in SAN
  180 CONTINUE

!     Current section COMMON 

      IF (LDX.LT.3) THEN
        IF(LDX.EQ.2) NCLINE = NLINE             ! # of recs/line 
        RETURN
      END IF

!     Current section DATA 

!*    Check independent variable links to REACTION found

      IF (NOTCK.NE.0) THEN           ! Do not check Indep var. link to REACTION 
        CALL ERR_MES(2,50)             ! Message 
      ELSE
        IF (NEEDS(1).GT.0) THEN        ! Resonance energy required
          IF(SPECIN(2).NE.0) THEN        ! Average res.par., check also res.en.
            IF(NEEDS(2).EQ.0) CALL ERR_INSERT(1,15,10,MESFAM(2)) ! Message
          END IF
        ELSE IF (NEEDS(2).GT.0) THEN   ! Energy required
          CALL ERR_INSERT(1,15,10,MESFAM(2)) ! message
        END IF
        DO 220 I = 3,7                 ! Check other required indep variables
          IF(NEEDS(I).GT.0) CALL ERR_INSERT(1,15,10,MESFAM(I))  ! message
  220   CONTINUE
      END IF

!*    Check for missing data headings

      IF (NHT.NE.0) THEN        
        DO 260 I = 1,NHT             ! Loop over # of stored headings
          DO 255 J = 1,NBPTR(I)        ! Loop over # stored pointers for heading
            IF (MATHED(J,I).EQ.0) THEN   ! No match
              MESSKP(1:10) = BHEAD(I)
              MESSKP(12:12) = BHPTR(J,I)
              CALL ERR_INSERT(1,17,12,MESSKP) ! Error message
            END IF
  255     CONTINUE
  260   CONTINUE
      END IF

!*    Cross check of REACTION and BIB pointers vs. DATA and COMMON

      IF (NPR.GT.1) THEN             ! More than 1 REACTION pointer

!----   If no pointers in DATA section, no link of REACTION to DATA
        IF (NPD(3).EQ.0) THEN            
          DO 270 I = 1,NPR             ! Loop over # of REACTION pointers
            CALL ERR_INSERT(1,21,1,RACPTR(I)) ! Error message 
  270     CONTINUE

        ELSE                           ! Data pointers exist

!----     All REACTION pointers should match DATA pointers
          DO 275 I = 1,NPR
            IP = LOOKUP(RACPTR(I),DATPTR(1,3),NPD(3))
                                           ! Check REACTION vs. DATA pointer
            IF (IP.EQ.0) CALL ERR_INSERT(1,24,1,RACPTR(I)) ! Error message 
  275     CONTINUE

          DO 280 L = 1,3                  
            IF (NPD(L).EQ.0) GOTO 280

!----       All DATA pointers should match REACTION pointers
            DO 278 I = 1,NPD(L)
              IF (DATPTR(I,L).NE.' ') THEN
                IP = LOOKUP(DATPTR(I,L),RACPTR,NPR)
                                               ! Check REACTION vs. DATA pointer
                IF (IP.EQ.0) CALL ERR_INSERT(1,23,1,DATPTR(I,L)) ! Error message
              END IF
  278       CONTINUE
  280     CONTINUE
        END IF

!----   All BIB pointers should match REACTION pointers
        IF (NPB.EQ.0) GOTO 300
        DO 285 I = 1,NPB
          IP = LOOKUP(BIBPTR(I),RACPTR,NPR)
                                              ! Check BIB vs. REACTION pointers
          IF(IP.EQ.0) CALL ERR_INSERT(1,22,1,BIBPTR(I)) 
                                              ! Error message 
  285   CONTINUE
      END IF

!*    Check COMMON and DATA pointers vs. BIB and REACTION

  300 NPDALL = NPD(1)+NPD(2)+NPD(3)       ! Total # of DATA pointers

!---- No COMMON or DATA pointers, BIB pointers in error
      IF (NPDALL.EQ.0) THEN 
        IF (NPB.EQ.1) CALL ERR_INSERT(1,26,1,BIBPTR(1))   ! Error message 
        RETURN
      END IF

!*    Check for Vector COMMON

      IF (NPR.GT.1) RETURN

      NVEC = 0                          ! Turn off Vector Common in SUBENT flag
      DO 520 I = 1,NPD(2)               ! Loop over # of section 2 pointers
        IF(DATPTR(I,2).EQ.' ') GOTO 520   ! Branch for blank pointer
        IP = LOOKUP(DATPTR(I,2),DATPTR(1,3),NPD(3)) 
                                      ! Check COMMON pointer vs. DATA pointers
        IF (IP.EQ.0) THEN                 ! No match
          CALL ERR_INSERT(1,25,1,DATPTR(I,2)) ! Error message 
          NVEC = -1
        END IF
  520 CONTINUE

      DO 600 I = 1,NPD(3)               ! Loop over # of section 3 pointers
        IF(DATPTR(I,3).EQ.' ') GOTO 600   ! Branch for blank pointer
        IF (NVEC.GE.0) NVEC = NVEC+1
        IP = LOOKUP(DATPTR(I,3),DATPTR(1,2),NPD(2)) 
                                        ! Check COMMON pointer vs. DATA pointers
        IF (IP.EQ.0) THEN                 ! No match
          CALL ERR_INSERT(1,25,1,DATPTR(I,3)) ! Error message 
          NVEC = -1
        END IF
  600 CONTINUE

      IF (NVEC.GT.0) THEN    ! Match on all pointers, write Vector Common file
        WRITE(IVEC,1000) NCLINE,NPD(2),ACNUM,INTSAN 
        WRITE(IVEC,1010) (DATPTR(I,2),I=1,NPD(2))
        WRITE(IVEC,1100) NLINE
        WRITE(IVEC,1010) (DATPTR(I,3),I=1,NPD(3))

        IFVEC = IFVEC+1                 ! Set Vector Common flag
        CALL ERR_MES(2,70)              ! Vector Common message
      END IF

      RETURN

 1000 FORMAT('COMMON',5X,2I11,33X,A5,I3)
 1010 FORMAT(6(10X,A1))
 1100 FORMAT('DATA',7X,I11)
      END
      SUBROUTINE DATASUB(LDX,ID,*)

!* Processes the DATA section
!*   LDX:    section index
!*           1 = subentry 1 COMMON
!*           2 = data subentry COMMON
!*           3 = DATA
!*   ID:     index of System Identifier
!*           12 = DATA
!*           13 = NODATA
!*   IMSNG:  record missing flag
!*   ISECND: last keyword out-of-sequence flag
!*   RETURN 1: end section ID found

!---- Input record. (Set in READ_REC)
      COMMON/ALLCOM/KEYD,IPTR,KARD,ISEQ
        CHARACTER*10 KEYD
        CHARACTER*1  IPTR
        CHARACTER*55 KARD
        CHARACTER*13 ISEQ

!---- Current AN, SAN, section. (Set in READ_REC)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

!---- Stored headings for SAN 1 + current subentry. (Set in HEAD_PROC)
      COMMON/DATA_HEAD/NPD(3),HEADS(18,3),DATPTR(18,3)
        CHARACTER*11 HEADS
        CHARACTER DATPTR

!---- Error pointers for data. (Initialized in DATASUB)
!----   DARROW: error column markers (^)
      COMMON/MARKER/DARROW(3),IERRD
        CHARACTER*80 DARROW

!---- Family codes for data fields.  (Set in UNIT_CHECK).
      COMMON/VAR_FIELD/INDFAM(18)

      CHARACTER*80 NOWREC, FLD(3)

      INTEGER IDATA(18)
      REAL STRDAT(18)

      EQUIVALENCE (NOWREC(1:1),KEYD(1:1))

      DATA DARROW/3*' '/          ! Initialize data error flags

      CALL FILLIN(INDFAM,18,0)    ! Initialize ind.var. fields

      IF (LDX.LT.3) THEN          ! COMMON section
        IF (INTSAN.EQ.1) THEN
          CALL FILLIN(NPD,3,0)      ! Initialize # stored SAN ! DATA pointers
          IWAY = 1
        ELSE
          CALL FILLIN(NPD(2),2,0)   ! Reset # stored DATA pointers for subentry
          IWAY = 2
        END IF
      ELSE
        IWAY = 3
      END IF

      IERR1 = 0                    ! Turn off error flag
      NERR  = 0                    ! Initialize # of errors in data section
      IHCNT = 0                    ! Initialize heading count
      NRLIN = 0                    ! Initialize # of data lines
      NDATR = 0                    ! Initialize # of data records
      IDEP  = 0                    ! Initialize dependent variable flag
      CALL FILLIN(IDATA,18,0)      ! Initialize data found in field

      IF (ID.EQ.13) RETURN         ! Return for NOCOMMON

C     Data Headings

      CALL READ_REC(ID)            ! Read first record
      NDATR = NDATR+1              ! Increment # of records in section

      CALL HEAD_PROC(LDX,IHCNT,24,NOTFND) ! Process 1st heading record

  300 NRLIN = NRLIN+1              ! Increment # of heading records
      IF (MOD(IHCNT,6).NE.0) THEN  ! End of headings
        IUCNT = 0                    ! Initialize unit count
        GOTO 400
      END IF
      CALL READ_REC(ID)            ! Read next record
      NDATR = NDATR+1              ! Increment # of records in section
      CALL HEAD_PROC(LDX,IHCNT,24,NOTFND)

      IF (NOTFND.NE.0) THEN        ! Not heading
        IUCNT = 0                    ! Initialize unit count
        GOTO 410
      ELSE
        GOTO 300
      END IF

!     Data Units

  400 CALL READ_REC(ID)              ! Read unit record
  410 CALL HEAD_PROC(LDX,IUCNT,25,NOTFND) ! Process unit record
      NDATR = NDATR+1                ! DATA or COMMON section record found
      IF (IUCNT.LT.IHCNT) GOTO 400

      CALL UNIT_CHECK(IWAY,IDEP)   ! Check heading vs. units and reaction

      IF (IWAY.EQ.3) THEN               ! DATA section
!----   DATA section must have dependent variable 
        IF(IDEP.EQ.0) CALL ERR_MES(2,51)  ! Error message
        CALL FIELD_CHECK(IWAY)            ! Check headings vs. BIB stored
      ELSE                              ! COMMON section
!----   Dependent variable illegal in COMMON section
        IF(IDEP.NE.0) CALL ERR_MES(2,52)  ! error message
      END IF

      CALL CROS_CHECK(LDX,IHCNT,NRLIN) 
                                   ! Check required variables and pointers
!     Data fields

      IF (IERRD.NE.0) THEN         
        CALL FILHOL(DARROW,3,' ')    ! Initialize error pointers
        IERRD = 0                    ! Turn off error flag
      END IF

      NREC = 0                       ! Initialize # of logical records

  510 DO 600 I = 1,NRLIN
        CALL READ_REC(ID)            ! Read record of data line
        IF (ID.GT.0) GOTO 800        ! Branch if END section read
        NDATR = NDATR+1              ! Increment # of records in section
        FLD(I) = NOWREC              ! Restore record
  600 CONTINUE
  
      CALL FLOAT_REC(IHCNT,FLD,IDATA,STRDAT) 
                                     ! Convert to floating point numbers
      IF (IWAY.EQ.3) THEN
        NREC = NREC+1                ! Increment # of logical records
        CALL MONOT(IHCNT,NREC,STRDAT,FLD,IERRD)
                                     ! Check data monotonicity and flags 
      END IF

      IF (IERRD.NE.0) THEN         ! Errors found on line
        IF (NERR.EQ.200) THEN        ! Maximum # of error messages for section
          CALL ERR_MES(1,55)
        ELSE IF (NERR.LT.200) THEN   ! Write error message for line
          DO 590 I = 1,NRLIN
            NOWREC = FLD(I)
            CALL ERR_ARW2(KEYD,IPTR,KARD,ISEQ,DARROW(I),IERRD) ! message
  590     CONTINUE
        END IF
        NERR = NERR+1                ! Increment # of errors found in section
      END IF
      GOTO 510

!     ENDDATA read

  800 CALL FIELD_COUNT(LDX,IDATA,IHCNT,IERRD) ! Check all fields contain data

      IF (IWAY.EQ.3) CALL FLAG_COUNT     ! For data, check all flags found

      RETURN
      END
      SUBROUTINE DECAY(IDB,KARD,NOW,LPTR,*)

!* Checks the keywords DECAY-DATA, DECAY-MON and RAD-DET.
!*   IDB:  keyword index 33 = DECAY-DATA
!*                       34 = DECAY-MON
!*                       35 = RAD-DET
!*   KARD: input character string
!*   NOW:  starting position in input string
!*   LPTR: pointer for current code
!*   RETURN 1: next record read

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*80 DICLINE
      CHARACTER*55 KARD
      CHARACTER LIMTR(4)/',', ')', ' ', '/' /

      IERRU = 0                           ! Turn off error flag

      IF (KARD(NOW:NOW).EQ.'(') THEN      ! FLAG field present
        IF (IDB.EQ.34) THEN                 ! DECAY-MON, Flag field illegal
           CALL FLAGIT(1,ARROW,NOW+11,1,IERRU) ! Set error markers
         ELSE                              
           NOW = NOW+1
           CALL FLAG_CHECK(2,KARD,NOW,LPTR,*200) ! Check flag field
        END IF
      END IF

!!!!  Nuclide field

  200 IFLD = 1

!---- FIND END OF NUCLIDE FIELD
      CALL PARSCHR(KARD,NOW,KPT,NUM,15,LIMTR,2,ICHR,*800)

      IF (NUM.EQ.0) THEN                  ! Nuclide not in field
        CALL ERR_FIELD(1,1,12)              ! Error message
        CALL FLAGIT(1,ARROW,NOW+11,1,IERR)
      ELSE
        CALL NUCFLD(5,KARD(NOW:KPT),NUM,IERR1,INE,MULT)   ! Check nuclide
        IF(IERR1.NE.0) CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR) ! Set error markers
      END IF

      IF(ICHR.EQ.2) GOTO 900              ! End of code string

      NOW = KPT+2

  300 IFLD = 2
      IF(IDB.EQ.35) GOTO 410              ! Branch for RAD-DET

!!!!  HALF-LIFE FIELD 

!---- Find end of HALF-LIFE field
      CALL PARSCHR(KARD,NOW,KPT,NUM,15,LIMTR,2,ICHR,*800)

      IF(NUM.EQ.0) GOTO(400,800) ICHR     ! Branch if field empty

!---- Separate units
      NDIG = NUM                          ! Store # of characters in field
      DO320I = KPT,NOW,-1                 ! Search for end of numerical subfield
        LST = I
        IF(KARD(I:I).GE.'0' .AND. KARD(I:I).LE.'9') GOTO 330
        IF(KARD(I:I).EQ.'.') GOTO 330
        NDIG = NDIG-1
  320 CONTINUE

!---- End of field not found
      CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRU) ! Set error markers
      GOTO 385

!---- Check for floating point number
  330 DIGIT = FLOATIT(KARD(NOW:LST),NDIG,IERR1)
      IF(IERR1.NE.0) GOTO 340

      IF(LST.NE.KPT) GOTO 350               ! Unit code found?
  340 CALL FLAGIT(1,ARROW,NOW+11,NDIG,IERRU)  ! Set error markers
      GOTO 385

!---- Check unit code in Dict. 25.
  350 CALL DANGET_NEW(25,KARD(LST+1:KPT),0,0,DICLINE,IERR1)

      IF (IERR1.EQ.0) THEN                  ! Unit found
        IF(DICLINE(36:39).EQ.'TIME') GOTO 385 ! Legal time unit
      END IF

      CALL ERR_MES(1,35)
      CALL FLAGIT(1,ARROW,LST+12,(NUM-NDIG),IERR) ! Set error markers

  385 NOW = KPT+1
      IF(ICHR.EQ.2) GOTO 900

!!!!  Type of radiation field

  400 IFLD = 3
  405 CALL INCRMT(NOW,55,*702)
      IF (KARD(NOW:NOW).EQ.' ') GOTO 405    ! Skip over leading blanks

!---- Find end of field
  410 CALL PARSCHR(KARD,NOW,KPT,NUM,15,LIMTR,4,ICHR,*800)

      IF (NUM.EQ.0) THEN                    ! Field empty
        IF (ICHR.LE.2) THEN                   ! Comma or ')' found
          CALL ERR_MES(1,38)                    ! Write error message 
          CALL FLAGIT(1,ARROW,NOW+11,1,IERR)
          GOTO (500,900) ICHR
        ELSE IF (ICHR.EQ.3) THEN              ! Blank at end of field
          GOTO 700
        ELSE IF (ICHR.EQ.4) THEN              ! '/' found
          CALL FLAGIT(1,ARROW,NOW+11,1,IERRU)   ! Set error markers
          NOW = KPT+2
          GOTO 410
        END IF
      END IF

      CALL DANVER_NEW(33,KARD(NOW:KPT),0,IERR1,*460) ! Check particle

!---- Illegal particle
      IF (KARD(NOW:NOW).GE.'0' .AND. KARD(NOW:NOW).LE.'9') THEN  ! Field missing
        CALL ERR_MES(1,38)                      ! error message
        CALL FLAGIT(1,ARROW,NOW+11,1,IERR)      ! set error marker
        GOTO 470   
      ELSE                                    ! illegal particle code
        CALL ERR_MES(1,35)                      ! error message
        NUM = KPT-NOW+1
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)    ! set error marker
      END IF
 
  460 NOW = KPT+1

  470 IF (IDB.EQ.35) GOTO (405,900,800,800) ICHR ! Branch for RAD-DET
      GOTO (500,900,690,400) ICHR

!!!!  ENERGY FIELD

  500 IFLD = 4
      LST = 0                                 ! initialize last separator
  505 CALL INCRMT(NOW,55,*702)

!---- Find end of next energy
  510 CALL PARSCHR(KARD,NOW,KPT,NUM,15,LIMTR,4,ICHR,*800)

      IF (NUM.EQ.0) THEN                      ! field empty
        IF (LST.NE.0) THEN                      ! last character '/'
          CALL ERR_MES(1,38)                      ! error message
          CALL FLAGIT(1,ARROW,NOW+11,1,IERR)
          GOTO(600,800,700,590) ICHR              ! branch for separator found
        ELSE
          GOTO(600,800,700,520) ICHR              ! branch for separator found
        END IF

  520   CALL FLAGIT(1,ARROW,NOW+11,1,IERRU)   ! '/' found, set error markers
        GOTO 590
      END IF

      DIGIT = FLOATIT(KARD(NOW:KPT),NUM,IERR1) ! check floating point number
      IF (IERR1.NE.0) CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRU) ! set markers

      NOW = KPT+1
      GOTO (600,900,690,590) ICHR

  590 LST = ICHR                              ! store / as last separator found
      GOTO 505
      
!     Abundance field

  600 IFLD = 5
      CALL INCRMT(NOW,55,*702)

!---- Find end of next abundance
  610 CALL PARSCHR(KARD,NOW,KPT,NUM,15,LIMTR,3,ICHR,*800)

      IF (NUM.EQ.0) GOTO(400,800,700) ICHR     ! Branch for empty field

      DIGIT = FLOATIT(KARD(NOW:KPT),NUM,IERR1) ! Check floating point number
      IF (IERR1.NE.0) THEN
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRU)  ! Set error markers
      ELSE
        IF (DIGIT.GT.1.0) THEN                   ! Abundance > 1.0
          CALL ERR_WARN(1,9)                       ! Warning message
          CALL FLAGIT(0,ARROW,NOW+11,NUM,IERR)
        END IF
      END IF        
      NOW = KPT+1
      GOTO (400,900,690) ICHR

  690 CALL FLAGIT(1,ARROW,NOW+11,1,IERRU) 
                                  ! Set error markers for blank at end of field

!---- End-of-field not found. Check for blanks on rest of record.
  700 IF(NOW.LT.55 .AND. KARD(NOW+1:55).NE.' ')
     *  CALL FLAGIT(1,ARROW,NOW+11,1,IERRU)    ! Set error markers

  702 IF (IERRU.NE.0) THEN
        IERR = 1
        CALL ERR_MES(1,38)                       ! Error message 
        IERRU = 0                                ! Reinitialize error flag
      END IF

!---- Read next record. Check for continuation of code.
      CALL READ_CONT(2,NOW,*990)

!---- Skip past blanks.
      DO 730 I = 1,55
        NOW = I
        IF(KARD(I:I).NE.' ') GOTO 750
  730 CONTINUE

  750 GOTO (200,300,410,510,610,410) IFLD      ! Branch to process next subfield

!---- Error found
  800 CALL FLAGIT(1,ARROW,NOW+11,1,IERRU)      ! Set error markers

  900 IF (IERRU.NE.0) THEN                     ! Internal error flag set
        IERR = 1                                 ! Set error flag
        CALL ERR_MES(1,38)                       ! Error message 
      END IF
      RETURN

  990 RETURN 1                                 ! Return for new keyword found
      END
      SUBROUTINE DICT13

!* Reads Reaction Type dictionary. Stores reaction types and fields expected.

!---- Reaction Type Table. (Set in DICT13)
!-      RECOD: Reaction type code
!-      ND:    # of codes stored
!----   FAMEQ: Flags to designate families expected.
      COMMON/FAMTAB/RECOD(200),ND,FAMEQ(200)
        CHARACTER*4 RECOD
        CHARACTER*10 FAMEQ

      CHARACTER*30 KEY
      CHARACTER*80 LINE

!*    Read in Reaction Types (Up to max. of 200)

      CALL DANORD_NEW(13,'F',0,LINE,KEY,IERR,*990)  ! Read first record
      IF(IERR.NE.0) GOTO 990
      ND = 1                                    ! Initialize stored array index

  200 RECOD(ND)(1:4) = KEY(1:4)                 ! Store Reaction Type
      FAMEQ(ND) = LINE(10:19)                   ! Store Family Indicis

      CALL DANORD_NEW(13,'N',0,LINE,KEY,IERR,*800)  ! Read next record
      IF(IERR.NE.0) RETURN                      ! Terminate read if error set
      CALL INCRMT(ND,200,*900)                  ! Increment stored array index
      GOTO 200

  800 RETURN                    

  900 write (*,*) ' **** More than 200 Reaction Types in dictionary'
c      CALL EXIT
      STOP

  990 write (*,*) ' **** DANORD error, execution terminated'
c      CALL EXIT
      STOP

      END
      SUBROUTINE DICT213

!* Reads Reaction Type dictionary. Stores reaction types and fields expected.

!---- Reaction Type Table. (Set in DICT213)
!-      RECOD: Reaction type code
!-      ND:    # of codes stored
!----   FAMEQ: Flags to designate families expected.
      COMMON/FAMTAB/RECOD(200),ND,FAMEQ(200)
        CHARACTER*4 RECOD
        CHARACTER*10 FAMEQ

      CHARACTER*30 KEY
      CHARACTER*80 LINE

!*    Read in Reaction Types (Up to max. of 200)

      CALL DANORD_NEW(213,'F',0,LINE,KEY,IERR,*990)  ! Read first record
      IF(IERR.NE.0) GOTO 990
      ND = 1                                    ! Initialize stored array index

  200 RECOD(ND)(1:4) = KEY(1:4)                 ! Store Reaction Type
      FAMEQ(ND) = LINE(10+3:19+3)                   ! Store Family Indicis
c	write (*,*) ND,' RECOD=',RECOD(ND)(1:4),' FAMEQ=',FAMEQ(ND)

      CALL DANORD_NEW(213,'N',0,LINE,KEY,IERR,*800)  ! Read next record
      IF(IERR.NE.0) RETURN                      ! Terminate read if error set
      CALL INCRMT(ND,200,*900)                  ! Increment stored array index
      GOTO 200

  800 RETURN                    

  900 write (*,*) ' **** More than 200 Reaction Types in dictionary'
c      CALL EXIT
      STOP

  990 write (*,*) ' **** DANORD error, execution terminated'
c      CALL EXIT
      STOP

      END
      SUBROUTINE EN_SEC(KIND,IFIELD,KARD,NOW,LPTR,*)

!* Checks the BIB keywords EN-SEC, HALF-LIFE, MISC, ERR-ANALYS, EMS-SEC
!* and MOM-SEC.
!*   KIND:   keyword processed  
!*           1 = EN-SEC
!*           2 = HALF-LIFE
!*           3 = MISC-COL
!*           4 = ERR-ANALYS
!*           5 = EMS-SEC
!*           6 = MOM-SEC
!*   IFIELD: nuclide field number
!*   KARD:   input array
!*   NOW:    position in array
!*   LPTR:   pointer for current code
!*   RETURN 1: do not check free text (all blank)

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*80 LINE
      CHARACTER*55 KARD
      CHARACTER*10 HEAD
      CHARACTER FAMCOD(6)/'E','6',' ',' ','S','L'/    ! Family codes
      CHARACTER LIMTR(3)/',', ')', '/' /
      CHARACTER FAMILY,LPTR

      IERRU = 0                            ! Turn off error flag for subroutine

!*    Heading Field

      CALL PARSCHR(KARD,NOW,KPT,NUM,10,LIMTR,2,ICHR,*700) ! find end of heading

      IF (NUM.EQ.0) THEN                   ! Empty field
        CALL ERR_MES(1,34)
        CALL FLAGIT(1,ARROW,NOW+11,1,IERR)
        GOTO 200
      END IF

      CALL DANGET_NEW(24,KARD(NOW:KPT),0,0,LINE,IERRU) ! look up heading
      IF(IERRU.NE.0) GOTO 140

!---- Check family codes
      IF (KIND.EQ.4) THEN                  ! ERR-ANALYS headings
        DO 130 I = NOW,KPT
          IF (KARD(I:I+2).EQ.'ERR') GOTO 150
  130   CONTINUE
      ELSE
        FAMILY(1:1) = LINE(3:3)
        IF(FAMILY.EQ.FAMCOD(KIND)) GOTO 150
      END IF

  140 CALL ERR_MES(1,35)
      CALL FLAGIT(1,ARROW,NOW+11,(KPT-NOW),IERR) ! Set error markers

  150 HEAD = KARD(NOW:KPT)                 ! Store heading
      CALL ADD_HEAD(HEAD,LPTR,*200)        ! Add to list of required headings

!---- Already in table
      CALL ERR_MES(1,39)                     ! Error message 
      CALL FLAGIT(1,ARROW,NOW+11,(KPT-NOW+1),IERR)

!*    Remaining fields

  200 NUM = 1                              ! initialize # of characters to flag

      IF (KIND.EQ.3) THEN          ! MISC-COL, only one code allowed
        IF(ICHR.EQ.2) GOTO 850       ! end of string
      ELSE IF (KIND.EQ.4) THEN     ! ERR-ANALYS, allow code + correlation factor
        IF(ICHR.EQ.2) GOTO 850       ! end of string
        NOW = KPT+2
        CALL PARSCHR(KARD,NOW,KPT,NUM,15,LIMTR(2),1,ICHR,*700)
                                                  ! find end of subfield
        VARBLE = FLOATIT(KARD(NOW:KPT),NUM,IERRU) ! check floating point number
        IF(IERRU.EQ.0) GOTO 800

      ELSE                         ! Additional subfields required
        IF (ICHR.EQ.2) THEN          ! end of string
          CALL ERR_MES(1,38)
          CALL FLAGIT(1,ARROW,KPT,1,IERR)   ! set error markers
          GOTO 800      
        END IF

!----   Check particle field.
  500   NOW = KPT+2
        CALL PARSCHR(KARD,NOW,KPT,NUM,15,LIMTR(2),2,ICHR,*700) ! find next field
        CALL NUCFLD(IFIELD,KARD(NOW:KPT),NUM,IERRU,INE,MULT)   ! check particle 
        IF (IERRU.EQ.0) THEN           ! No error
          IF (ICHR.EQ.2) GOTO 500
          GOTO 800
        END IF
      END IF

  700 CALL ERR_MES(1,36)
      CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)   ! set error markers

  800 NOW = KPT+2
      RETURN

  850 NOW = KPT+2
      IF (KARD(NOW:55).EQ.' ') THEN          ! blank rest of field
        NUM = 55-NOW+1
        CALL ERR_MES(1,45)
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)
        RETURN 1
      END IF

      RETURN
      END
      SUBROUTINE FACIL_CHECK(KARD,NOW)

!* Checks the BIB keyword FACILITY
!* Input to routine
!*   KARD: input character string
!*   NOW:  position in input array

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*55 KARD
      CHARACTER LIMTR(2)/',', ')'/

      N = 0
      IERRF = 0

!---- Flag illegal leading blanks.
  100 IF(KARD(NOW:NOW).EQ.' ') CALL LEAD_BLANK(KARD,NOW,*900)

      N = N+1                                  ! Increment # of codes
      CALL PARSCHR(KARD,NOW,KPT,NUM,7,LIMTR,2,ICHR,*500)

      IF (NUM.EQ.0) THEN                       ! Empty field
        CALL ERR_MES(1,34)                       ! Error message 
        CALL FLAGIT(1,ARROW,NOW+11,1,IERR)
        NOW = NOW+1
        GOTO 290
      END IF

      CALL DANVER_NEW(18,KARD(NOW:KPT),0,IERR1,*250) ! Look up code
      IF(N.EQ.2) GOTO 300                      ! No match, branch if second code
      CALL ERR_MES(1,35)                       ! Error message 
      CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRF)    ! Set error markers
  250 NOW = KPT+2

  290 GOTO (100,900) ICHR                      ! Branch for comma, rparen

  300 CALL DANVER_NEW(3,KARD(NOW:KPT),0,IERR1,*310) ! Look up INSTITUTE code
      GOTO 500

  310 IF(ICHR.EQ.2) GOTO 900
      NOW = KPT+2
      GOTO 800

!---- Error. Search for beginning of next field.
  500 CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRF)    ! Set error markers
      CALL ERR_MES(1,35)                       ! Error message 
      IF(ICHR.EQ.2 .OR. NOW.EQ.55) GOTO 900    ! Branch if end of string
      NOW = KPT+1
      DO 520 I = NOW,55
        IF (KARD(I:I).EQ.')') THEN               ! End of string
          GOTO 900                                     
        ELSE IF (KARD(I:I).EQ.',') THEN          ! End of code
          CALL INCRMT(NOW,55,*800)
          GOTO 100                                
        ELSE                                     ! Not end marker
          CALL FLAGIT(1,ARROW,I+11,1,IERRF)        ! Set error markers
          CALL ERR_MES(1,38)                       ! Error message 
        END IF
  520 CONTINUE
      GOTO 900

!---- End of code string not found
  800 NUM = 55-NOW+1
      CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRF)    ! Set error markers
      CALL ERR_MES(1,38)                       ! Error message 

  900 IF (IERRF.NE.0) IERR = 1
      RETURN

      END
      SUBROUTINE FAMGET(IWAY,RTYPE,NEEDS)

!*    Finds independent variables required for reaction type
!*    given.
!*      IWAY:  1 = Look up code and store independent variables required
!*            >1 = Look up code
!*      RTYPE: Reaction type from Dictionary 36.
!*      NEEDS: Independent variable types
!*             1: Resonance parameter
!*             2: Incident energy
!*             3: Secondary energy
!*             4: Angle
!*             5: Number
!*             6: Other (Temp)
!*             7: Element and/or mass

!---- Current AN, SAN, section. (Set in READ_REC)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

!---- Reaction Type Table. (Set in DICT13)
      COMMON/FAMTAB/RECOD(200),ND,FAMEQ(200)
        CHARACTER*4 RECOD
        CHARACTER*10 FAMEQ

      CHARACTER*4 RTYPE
      CHARACTER*4 RTYP2/' '/
      DIMENSION NEEDS(7)

      L = LOOKUP(RTYPE,RECOD,ND)             ! Look up reaction type
      IF (L.EQ.0) THEN                       ! No match found
        IF (RTYPE(4:4).NE.' ') THEN            
          RTYP2(1:3) = RTYPE(1:3)                ! Remove last character of type
          L = LOOKUP(RTYP2,RECOD,ND)             ! Look up reaction type again
          IF(L.NE.0) GOTO 200
        END IF
        CALL ERR_INSERT(1,27,4,RTYPE)         ! Write error message + code
        RETURN
      END IF

  200 IF (FAMEQ(L)(1:1).NE.' ') THEN
        NEEDS(1) = INTSAN  
        NEEDS(2) = 0
      END IF
      DO 150 I = 3,7                         ! Loop over # of families.
        IF(IWAY.EQ.1 .AND. NEEDS(I).EQ.0) THEN ! For Reaction
          IF(FAMEQ(L)(I:I).NE.' ') NEEDS(I) = INTSAN  ! Set indep. var. needed
        END IF
  150 CONTINUE

      RETURN
      END
      SUBROUTINE FIELD_CHECK(IWAY)

!*  Checks DATA and COMMON headings vs. those stored from BIB.
!*    IWAY 1 = SAN 1 COMMON section
!*         2 = Data subentry COMMON section
!*         3 = Data subentry DATA section

!---- Stored headings for SAN 1 + current subentry. (Set in HEAD_PROC)
      COMMON/DATA_HEAD/NHD(3),HEADS(18,3),HPTR(18,3)
        CHARACTER*11 HEADS
        CHARACTER HPTR

!---- Independent variable special treatment flags
      COMMON/EN_PROC/SPECIN(4)
        INTEGER SPECIN

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

!---- Number of monitors (Initialized in BIB_SUB).
      COMMON/MON_HEAD/NMON1,NMON

!---- Stored family, sub-family codes from Dictionary 24 for each field 
!-      (Set in UNIT_CHECK).
      COMMON/HEAD_FAMLY/IFAM(18),ISUBF(18)

      CHARACTER*10 KEYD
      CHARACTER*10 ROOT
      CHARACTER*4 REMOVE(4)/'MIN ','MAX ','APRX','APX '/
      CHARACTER*4 NUMDEN(2)/'NM  ','DN  '/
      CHARACTER*4 EXT

      DO 600 J = 1,3
        DO 500 I = 1,NHD(J)                     ! loop over stored headings 
          NOW = (MOD(I,6)*11)-10                  ! set index in array

          IF (IFAM(I).EQ.1) THEN                  ! Monitor
            IF (ISUBF(I).EQ.1 .AND. NMON.EQ.0) THEN ! Monitor not expected
              CALL FLAGIT(1,ARROW,NOW,11,IERR)        ! Set error markers
            END IF
          END IF

          CALL CHECK_HEAD(HEADS(I,J),HPTR(I,J),NOW,*500) 
                                                  ! Check heading vs stored
!----     No match, store heading root for comparison, and extension, if any
          ROOT = ' '
          KEYD = HEADS(I,J)
          CALL GET_EXT(KEYD,EXT,N2,*150)

!----     Extension found
          N = LOOKUP(EXT,REMOVE,4)             ! Look up codes to be removed
          IF (N.EQ.0) THEN                     ! If no match
            L = LOOKUP(EXT,NUMDEN,2)           ! Check -NM/-DN extensions
            IF (L.NE.0) THEN
              IF(SPECIN(1).EQ.0) THEN            ! Match, NM/DN not expected
                CALL ERR_INSERT(1,13,11,HEADS(I,J))
              END IF
              GOTO 500
            ELSE
              N2 = 10
            END IF
          END IF

  150     IF (KEYD(1:1).EQ.'-' .OR. KEYD(1:1).EQ.'+') THEN
            N1 = 2                               ! Remove leading + or -
          ELSE
            N1 = 1
          END IF

          IF(KEYD(N2:N2).GE.'1' .AND. KEYD(N2:N2).LE.'9') THEN
            N2 = N2-1
          END IF
        
          NN = N2-N1+1
          IF (NN.NE.10) THEN
            ROOT(1:NN) = KEYD(N1:N2)
            IF (ROOT(1:4).EQ.'ELEM') ROOT(5:7) = 'ENT'
            CALL CHECK_HEAD(ROOT,HPTR(I,J),NOW,*500) ! Check root vs stored
          END IF
  500   CONTINUE
  600 CONTINUE
      RETURN  

      END
      SUBROUTINE FIELD_COUNT(LDX,IDATA,IHCNT,IERR)

!* Checks that data exists for all headings and that all data fields have
!* headings.
!*   LDX:   Section index
!*          1 = Subentry 1 COMMON
!*          2 = Data subentry COMMON
!*          3 = DATA
!*   IDATA: data flag; 1= data found in field 
!*   IHCNT: # of data fields
!*   IERR:  error flag

      DIMENSION IDATA(18)

!---- Check that data exists for all headings.
      DO 200 I = 1,IHCNT
        II = I
        IF (IDATA(I).EQ.0) THEN
          IERR = 1
          MNUM = 4
          IF(LDX.LT.3) MNUM = 6
          CALL ERR_FIELD(2,II,MNUM)            ! Error message 
        END IF
  200 CONTINUE

!---- Check that all data fields have headings.
      IF(IHCNT.GE.18) RETURN
      DO 300 I = IHCNT+1,18
        II = I
        IF (IDATA(I).NE.0) THEN
          IERR = 1
          MNUM = 5
          IF(LDX.LT.3) MNUM = 7
          CALL ERR_FIELD(2,II,MNUM)            ! Error message 
        END IF
  300 CONTINUE
      RETURN
      END
      SUBROUTINE FLAG_CHECK(IWAY,KARD,NOW,LPTR,*)

!* Checks BIB keyword FLAG coding and the flag portion of BIB keyword
!* DECAY-DATA. Stores data heading in list of required headings.
!*   IWAY: 1 = FLAG
!*         2 = DECAY-DATA
!*         3 = LVL-PROP
!*   KARD: input array
!*   NOW:  starting position in array (reset at end of routine)
!*   LPTR: pointer for current code
!*   RETURN 1: do not check free text

!---- Current AN, SAN, section. (Set in READ_REC)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

!---- Stored flags from BIB for FLAG (1) and DECAY-DATA (2).
!-      NF1:    # of flags (Initialized in AN_GET)
!-      NFT:    total # of flags (Initialized in AN_GET)
!-      FLAGS:  numerical flags and pointers (Set in FLAG_CHECK)
!-      FPTR:   pointers for stored flags (Set in FLAG_CHECK)
!----   MATFLG: match flag, BIB vs. DATA (Set in MONOT, initialized in AN_GET)
      COMMON/BIB_FLAG/NF1(3),NFT(3),FLAGS(200,3),FPTR(200,3),
     * MATFLG(200,3)
        CHARACTER FPTR

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

!---- Flag family codes. (Set in FLAG_CHECK)
      COMMON/HOLFAM/HFLG(3)
        CHARACTER*10 HFLG

      CHARACTER*55 KARD
      CHARACTER*10 MESS
      CHARACTER LPTR

      DATA HFLG/'FLAG      ','DECAY-FLAG','LVL-FLAG  '/

c	write (*,*) '-zvv-5 FLAG_CHECK:IWAY=',IWAY,' [',KARD,'] '
c     *	,' NOW=',NOW,' LPTR=',LPTR

!---- Find end of heading field. 
      CALL PARSCHR(KARD,NOW,KPT,N,11,')',1,ICHR,*390)
      IF (N.EQ.0) THEN
        CALL ERR_MES(1,34)
        GOTO 400                                  ! BRANCH for empty field
      END IF

      DIG = FLOATIT(KARD(NOW:KPT),N,IERR)       ! Check floating point number
      IF(IERR.NE.0) THEN
        CALL ERR_MES(1,20)
        GOTO 400
      END IF

      INDX = NFT(IWAY)
      IF(INDX.EQ.0) GOTO 200                    ! No flags stored for keyword

!---- Check if FLAG already stored in table.
      DO 150 I = 1,INDX
        IF (DIG.EQ.FLAGS(I,IWAY) .AND. LPTR.EQ.FPTR(I,IWAY)) THEN 
                                                  ! FLAG already defined 
          IF(IWAY.GE.2) GOTO 500                    ! Branch for DECAY-DATA
          CALL ERR_MES(1,39)                        ! Error message 
          GOTO 400
        END IF
  150 CONTINUE

!---- Add FLAG to list of required headings if not already on.
  200 CALL ADD_HEAD(HFLG(IWAY),LPTR,*300)

!---- Store FLAGS for comparison with DATA.
  300 CALL INCRMT(INDX,200,*350)
      IF(INTSAN.EQ.1) NF1(IWAY) = NF1(IWAY)+1
      FLAGS(INDX,IWAY) = DIG
      FPTR(INDX,IWAY) = LPTR
      NFT(IWAY) = INDX
      GOTO 500

  350 write (*,1000) INDX                       ! Too many stored FLAGS
      GOTO 500

  390 CALL ERR_MES(1,36)

  400 CALL FLAGIT(1,ARROW,NOW+11,N+1,IERR) ! Set error markers

  500 NOW = NOW+N+1                        ! Reset current position
      IF(IWAY.GE.2) RETURN                   ! DECAY-DATA return

      IF (KARD(NOW:55).EQ.' ') THEN          ! No free text for FLAG
        N = 55-NOW+1
        CALL ERR_MES(1,36)                     ! Error message
        CALL FLAGIT(1,ARROW,NOW+11,N,IERR)
        RETURN 1
      END IF

      RETURN

      ENTRY FLAG_COUNT
!*    Checks that all flags are matched

      DO 800 J = 1,2
        NERR = 30+J
        DO 790 I = 1,NFT(J)
c	write (*,*) '-zvv-5 MATFLG(I,J)=',MATFLG(I,J),' I=',I,' J=',J
          IF (MATFLG(I,J).EQ.0) THEN
            WRITE(MESS,6000) FLAGS(I,J)
            CALL ERR_INSERT(1,NERR,10,MESS)
          END IF
  790   CONTINUE
  800 CONTINUE
      RETURN
 1000 FORMAT(/' **** Unable to update array FLAGS, max = ',I4/)
 6000 FORMAT(F10.2)
      END
      SUBROUTINE FLOAT_REC(ICNT,STRING,IDATA,STRDAT)

!* Checks data fields for legal floating point numbers.  Stores numbers.
!*   ICNT:   # of fields to be converted
!*   STRING: fields to be converted
!*   IDATA:  data found in field flags
!*   STRDAT: stored data for record

!---- Error pointers for data. (Initialized in DATASUB)
      COMMON/MARKER/DARROW(3),IERRD
        CHARACTER*80 DARROW

      CHARACTER*80 STRING(3),NOWREC
      INTEGER IDATA(18)
      REAL STRDAT(18)

      NOW = 0

      DO 700 J = 1,3
        NOWREC = STRING(J)
        DO 690 I = 1,6                      ! Loop over data fields
          CALL INCRMT(NOW,ICNT,*800)          ! Set position in current field
          LOW = (I*11)-10
          IF (NOWREC(LOW:LOW+10).EQ.' ') THEN ! Blank field
c?          STRDAT(NOW) = "2                      ! Set blank equivalent
            STRDAT(NOW) = 2                      ! Set blank equivalent
          ELSE 
            STRDAT(NOW) = FLOATIT(NOWREC(LOW:LOW+10),11,IERR1)
                                                  ! Convert to floating point
            IF (IERR1.NE.0) THEN                  ! Error
              CALL FLAGIT(1,DARROW(J),LOW,LOW+10,IERRD)
            ELSE                                  ! Data found in field
              IDATA(NOW) = 1                        
            END IF
          END IF
  690   CONTINUE
  700 CONTINUE

  800 RETURN
      END
      SUBROUTINE GENQM(KARD,LOW,LAST,MODAV,MODREL)

!* Removes General Quantity Modifier from REACTION.
!*   KARD:   SF 5-8 of REACTION keyword
!*   LOW:    index for start of SF8.
!*   LAST:   last position in array stored (reset after GQM removed)
!*   MODAV:  average modifier flag
!*   MODREL: rel modifier flag

      CHARACTER*40 KARD
      CHARACTER*80 MFLAG
      CHARACTER LIMTR/'/'/

!     Initialize
      MODREL = 0                               ! relative quantity flag
      MODAV = 0                                ! average quantity flag

!---- Check if modifier present
  150 IF(LOW.EQ.0) GOTO 900
      IF(KARD(LOW:LOW).EQ.' ') GOTO 900

      NUM = LAST-LOW+1                         ! Set # of chars in string
      CALL SEARCH_END(KARD(LOW:LAST),NUM,LIMTR,1,N,ICHR) ! Find next subfield
      KPT = LOW+N-1                            ! Set # of chars in code
      CALL DANGET_NEW(34,KARD(LOW:KPT),0,0,MFLAG,IERRM)      ! Look up modifier 
      IF(IERRM.NE.0) GOTO 900

      IF(MFLAG(11:14).EQ.'GENQ') GOTO 170
      LOW = KPT+2
      IF(LOW.GT.LAST) GOTO 900
      GOTO (150,900) ICHR

!---- General Quantity Modifier found
  170 READ(MFLAG(1:10),1000) M                 ! Store integer INE
      IF (M.EQ.1) THEN                              ! AV modifier
        MODAV = 1 
      ELSE IF (M.EQ.2 .OR. M.EQ.3 .OR. M.EQ.6) THEN ! MSC, RAW, or FCT
        MODREL = -M
      ELSE IF (M.EQ.4) THEN                         ! REL modifier
        MODREL = 1               
      END IF

!---- Delete modifier and restore rest of code
      IF (KPT.EQ.LAST) THEN
        KARD(LOW:KPT) = ' '
      ELSE
        NXT = KPT+1
        IF(ICHR.EQ.1) NXT = NXT+1           ! Set to remove trailing '/'
        LST = LOW+(LAST-NXT)
        KARD(LOW:LST) = KARD(NXT:LAST)
        KARD(LST+1:LAST) = ' '
        IF(ICHR.EQ.1) GOTO 150
      END IF
  900 L = LAST    

!---- Delete trailing commas and slashes
      DO 910 I = L,1,-1
        IF(KARD(I:I).EQ.' ') GOTO 910
        LAST = I
        IF(KARD(I:I).NE.',' .AND. KARD(I:I).NE.'/') RETURN
        KARD(I:I) = ' '
  910 CONTINUE
      RETURN

 1000 FORMAT(I10)
      END
      SUBROUTINE GET_EXT(KEYD,EXT,LST,*)

!* Find heading extension.
!* Input to routine
!*   KEYD: field heading
!* Output from routine
!*   EXT:  extension found
!*   LST:  last character of root
!* RETURN
!*   1 = no extension found

      CHARACTER*10 KEYD
      CHARACTER*4 EXT

      EXT = ' '

      DO 110 J = 10,2,-1
        IF (KEYD(J:J).NE.' ') THEN
          LEND=J                           ! Store last nonblank char. position
          GOTO 120
        END IF
  110 CONTINUE

  120 DO 130 J = LEND,2,-1     
        IF(KEYD(J:J).EQ.'-') THEN
          LST = J-1                          ! Store current index
          GOTO 200
        END IF
  130 CONTINUE

      LST = LEND
      RETURN 1                           ! No root to be stored

!*    Extension found

  200 LNTH = LEND-LST-1                  ! Check length
      IF (LNTH.GT.4) THEN                ! Extension too long
        LST = LEND
        RETURN
      END IF

      EXT(1:LNTH) = KEYD(LST+2:LEND)     ! Store extension for lookup
      RETURN
      END
      SUBROUTINE HEAD_PROC(LDX,KNT,KDIC,NOTFND)

!* Reads records, checks if Data Heading or Units. 
!* For data headings, stores family codes, checks for required headings.
!*   LDX:   Section index
!*          1 = Subentry 1 COMMON
!*          2 = Data subentry COMMON
!*          3 = DATA
!*   KNT:    field count
!*   KDIC:   dictionary number
!*   NOTFND: keyword not found

!---- Input record. (Set in READ_NEXT)
      COMMON/ALLCOM/KEY,IPTR,KARD,ISEQ
        CHARACTER*10 KEY
        CHARACTER*1  IPTR
        CHARACTER*55 KARD
        CHARACTER*13 ISEQ

!---- Stored headings for SAN 1 + current subentry. (Set in HEAD_PROC)
!-      NHD:   # of stored headings
!-      HEADS: heading keywords
!-      HPTR:  pointer for headings
      COMMON/DATA_HEAD/NHD(3),HEADS(18,3),HPTR(18,3)
        CHARACTER*11 HEADS
        CHARACTER HPTR

!---- Stored units for current section. (Set in HEAD_PROC)
!-      UHEAD: dict. 24 information for headings
!----   UTYP:  unit type for stored units 
      COMMON/DATA_UNITS/HEAD_CODE(18),UTYP(18)
        CHARACTER*15 HEAD_CODE
        CHARACTER*4 UTYP

      CHARACTER*80 DICLINE
      CHARACTER*11 KEYC(6)
      CHARACTER*10 KEYD/' '/

      EQUIVALENCE (KEYC(1)(1:10),KEY),(KEYC(1)(11:11),IPTR),
     *            (KEYC(2)(1:1),KARD(1:1))

      NOTFND = 0                             ! Keyword found flag

      DO 400 I = 1,6                         ! Loop over fields

        IF (KEYC(I)(1:10).EQ.' ') RETURN 

        KEYD = KEYC(I)(1:10)                 ! Store code
        CALL DANGET_NEW(KDIC,KEYD,0,0,DICLINE,IERR1) ! Look up heading

        IF (I.EQ.1 .AND. KDIC.EQ.24) THEN    ! 1st heading field
          IF(IERR1.NE.0) THEN
            NOTFND = 1
            RETURN                               ! return if no match
          END IF
        END IF

        KNT = KNT+1                          ! Keyword found, increment count

!*      Heading records

        IF (KDIC.EQ.24) THEN                 ! Heading record
          NHD(LDX) = NHD(LDX)+1                  ! Increment heading count
          HEADS(KNT,LDX) = KEYD                  ! Store heading
          HPTR(KNT,LDX) = KEYC(I)(11:11)         ! Store pointer
          HEAD_CODE(KNT) = DICLINE(1:15)         ! Store dict. 24 codes

!*      Unit records

        ELSE     
          UTYP(KNT) = DICLINE(36:39)             ! Store unit type
        END IF

  400 CONTINUE

      RETURN
      END
      SUBROUTINE HIST_CHECK(KARD,NOW)

!* Checks code string for BIB keywords HISTORY and EXP-YEAR.
!*   KARD: input character string
!*   NOW:  position in input string

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*55 KARD

!---- Check date.
      DO 200 I = 2,20
        LST = I-1
        IF (KARD(I:I).EQ.')') GOTO 250
  200 CONTINUE

  250 LP = LST+1
      IF (LST.EQ.8 .OR. LST.EQ.10) THEN      ! HISTORY code given 
        LST = LST-1
        CALL DANVER_NEW(15,KARD(LST+1:LST+1),0,IERR1,*300)
        CALL ERR_MES(1,35)                   ! Error message 
        CALL FLAGIT(1,ARROW,LST+12,1,IERR)
      ELSE IF (LST.GT.10) THEN               ! Code too long
        CALL ERR_MES(1,35)
        CALL FLAGIT(1,ARROW,LST+12,1,IERR)
        GOTO 920
      END IF        
      
  300 NOWDIG = LST-1
      CALL DATE_CHECK(KARD(2:LST),NOWDIG,IERR1)
      IF(IERR1.NE.0) CALL FLAGIT(1,ARROW,NOW+11,NOWDIG,IERR) ! Set markers

  920 NOW = LP+1

      RETURN
      END
      SUBROUTINE LAB_CHECK(KARD,NOW,NBIBR,*)

!* Checks the BIB keyword INSTITUTE for: legal codes, obsolete codes, 
!* illegal leading blanks. 
!* Codes must be separated by commas.  Code string must end with ')'.
!*   KARD:  Input character string.
!*   NOW:   Current position in input string.
!*   NBIBR: # of BIB records read.
!*   RETURN 1: Next BIB keyword read.

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*55 KARD
      CHARACTER*3 STATD
      CHARACTER LIMTR(2)/',', ')'/

!---- Flag leading blanks
  150 IF(KARD(NOW:NOW).EQ.' ') CALL LEAD_BLANK(KARD,NOW,*900)

      CALL PARSCHR(KARD,NOW,KPT,NUM,7,LIMTR,2,ICHR,*800) ! Find next code
      IF (NUM.EQ.0) THEN                          ! Empty field
        CALL ERR_MES(1,34)                          ! Error message 
        CALL FLAGIT(1,ARROW,NOW+11,1,IERR)
        NOW = NOW+1
        GOTO 870
      END IF

!---- Look up code
      CALL DANVER_STA_NEW(3,KARD(NOW:KPT),0,STATD,IERRL,*850) 

!---- Illegal field
  800 CALL ERR_MES(1,35)                          ! Error message 
      CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)
      GOTO 860

!---- Check for obsolete codes
  850 IF (STATD.EQ.'EXT') THEN
        CALL ERR_WARN(1,1)                        ! Error message 
        CALL FLAGIT(0,ARROW,NOW+11,NUM,IERR)
      ELSE IF (STATD.EQ.'OBS') THEN
        CALL ERR_MES(1,31)
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)
      END IF

  860 NOW = KPT+2

!---- If remainder of code field blank, read continuation record.
  870 IF (ICHR.NE.2) THEN                         ! Not end of code string
        IF (KARD(NOW:55).EQ.' ') THEN               ! Remainder of record blank.
          CALL READ_CONT(1,NOW,*930)
          IF(KARD(1:1).EQ.'(') THEN
            NOW = 2
            CALL ERR_MES(1,32)
            CALL FLAGIT(1,ARROW,NOW,12,IERR)
            NOW = 2
          END IF
          NBIBR = NBIBR+1
        END IF
        GOTO 150
      END IF

  900 RETURN

!---- New keyword found.
  930 RETURN 1
      END
      SUBROUTINE LEAD_BLANK(KARD,NOW,*)

!* Flags illegal leading blanks
!*   KARD: input array
!*   NOW: position in input array
!*   RETURN 1: array all blank

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*55 KARD

      LOW = NOW
  110 CALL INCRMT(NOW,55,*190)
      IF(KARD(NOW:NOW).NE.' ') GOTO 200
      GOTO 110

  190 NOW = 56                                  ! Set for all blank

  200 CALL ERR_MES(1,42)                        ! Message 
      CALL FLAGIT(1,ARROW,LOW+11,(NOW-LOW),IERR)

      IF(NOW.GT.55) RETURN 1                    ! Return for blank cols 12-55
      RETURN
      END
      SUBROUTINE LVL_PROP(KARD,NOW,LPTR)

!* Checks code for keywork LEVEL-PROP
!*   KARD: input record (columns 12-66)
!*   NOW:  current position in input string

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*80 DICLINE
      CHARACTER*55 KARD
      CHARACTER*1 LPTR
      CHARACTER LIMTR(3)/',', ')', '/' /

      IERRU = 0                                 ! Turn off error flag

c	write (*,*) '-zvv-5 LVL_PROP: ',KARD,NOW,LPTR
      IF (KARD(NOW:NOW).EQ.'(') THEN            ! FLAG field present
        NOW = NOW+1
        CALL FLAG_CHECK(3,KARD,NOW,LPTR,*200)   ! Check FLAG field
      END IF

!!!!  Nuclide field

  200 IFLD = 1

      CALL PARSCHR(KARD,NOW,KPT,NUM,15,LIMTR,2,ICHR,*800) ! Find end of nuclide
      IF (NUM.EQ.0) THEN                        ! Nuclide not in field
        CALL ERR_FIELD(1,1,12)                    ! Error message
        CALL FLAGIT(1,ARROW,NOW+11,1,IERR)
      ELSE
        CALL NUCFLD(7,KARD(NOW:KPT),NUM,IERRU,INE,MULT)   ! Check nuclide
        IF(IERRU.NE.0) CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR) ! Set error markers
      END IF

      IF(ICHR.EQ.2) GOTO 900                    ! End of code string

      NOW = KPT+2

!!!!  Level identification field

      IFLD = 2

      IF (KARD(NOW:NOW+5).EQ.'E-LVL=') THEN
        NOW = NOW+6
      ELSE IF (KARD(NOW:NOW+8).EQ.'LVL-NUMB=') THEN
        NOW = NOW+9
      ELSE
        CALL PARSCHR(KARD,NOW,KPT,NUM,15,LIMTR,2,ICHR,*800) 
        CALL ERR_FIELD(1,IFLD,18)
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)
        NOW = NOW+NUM+1
        GOTO 400
      END IF

!---- Check for legal floating point number
  380 CALL PARSCHR(KARD,NOW,KPT,NUM,15,LIMTR,2,ICHR,*800) 
      DIGIT = FLOATIT(KARD(NOW:KPT),NUM,IERRU)

      IF (IERRU.NE.0) THEN
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)
        CALL ERR_FIELD(1,IFLD,19)
      END IF

      NOW = KPT+2
      GOTO (400,900) ICHR    ! Branch for , )

!!!!  Level properties field

  400 IFLD = 3

  410 IF (KARD(NOW:NOW+4).EQ.'SPIN=') THEN
        NOW = NOW+5
      ELSE IF (KARD(NOW:NOW+6).EQ.'PARITY=') THEN
        NOW = NOW+7
      ELSE
        CALL PARSCHR(KARD,NOW,KPT,NUM,15,LIMTR,3,ICHR,*800) 
        IERR = 18
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)
        CALL ERR_FIELD(1,IFLD,18)
        GOTO 480
      END IF

!---- Check for legal floating point number
  470 CALL PARSCHR(KARD,NOW,KPT,NUM,15,LIMTR,3,ICHR,*800) 
      DIGIT = FLOATIT(KARD(NOW:KPT),NUM,IERRU)
  480 IF (IERRU.NE.0) THEN
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)
        CALL ERR_FIELD(1,IFLD,19)
      END IF

      NOW = KPT+2
      GOTO (410,900,470) ICHR    ! Branch for , ) /

!---- End of code string not found.
  800 CALL ERR_MES(1,36)
      CALL FLAGIT(1,ARROW,NOW+11,1,IERR)     ! Set error markers

  900 RETURN
      END
      SUBROUTINE MONOT(ICNT,NREC,STRDAT,KPTFLD,IERR)

!* Checks in data section:
!* - that all independent variables types are present,
!* - that at least one data field is present,
!* - for monotonicity in data,
!* - for legal FLAG fields.
!* ICNT:   # of data fields
!* NREC:   # of logical data records read
!* STRDAT: stored data for current record
!* KPTFLD: stored data record as read
!* IERR:   error flag

!---- Stored flags from BIB for FLAG (1) and DECAY-DATA (2). (See FLAG_CHECK)
      COMMON/BIB_FLAG/NF1(3),NFT(3),FLAGS(200,3),FPTR(200,3),
     * MATFLG(200,3)
        CHARACTER FPTR

!---- Stored headings for SAN 1 + current subentry. (Set in HEAD_PROC)
      COMMON/DATA_HEAD/NPD(3),HEADS(18,3),DATPTR(18,3)
        CHARACTER*11 HEADS
        CHARACTER DATPTR

!---- Family names for error messages. (Set in CROS_CHECK)
      COMMON/FAMMES/MESFAM(10)
        CHARACTER*10 MESFAM

!---- Flag family codes. (Set in FLAG_CHECK)
      COMMON/HOLFAM/HFLG(3)
        CHARACTER*10 HFLG

!---- Error pointers for data. (Initialized in DATASUB)
      COMMON/MARKER/DARROW(3),IERRD
        CHARACTER*80 DARROW

!---- Family codes for data fields.  (Set in UNIT_CHECK).
      COMMON/VAR_FIELD/INDFAM(18)

      CHARACTER*80 KPTFLD(3),NOWREC
      REAL STRDAT(18),LSTDAT(18)
      INTEGER MSORT(18)
      common/monot_g77/ MSORT

!!!!  Initialize
      NOW = 0
      LSTFLG = -1                          ! last indep. variable field
      LSTCNG = 0                           ! last variable changed
      NDFLD = 0                            ! # of data fields given
      IF (NREC.EQ.1) CALL FILLIN(MSORT,ICNT,0) ! Initialize sort flags

      DO 300 J = 1,3
        NOWREC = KPTFLD(J)
        DO 290 I = 1,6                     ! Loop over data fields
          CALL INCRMT(NOW,ICNT,*400)         ! Set position in current field
          LOW = (I*11)-10

          IF (INDFAM(NOW).EQ.2) THEN         ! Dependent variable
            IF(NOWREC(LOW:LOW+10).NE.' ') NDFLD = NDFLD+1 
                                              ! Increment # of data fields given

          ELSE IF (INDFAM(NOW).LT.0 .AND. INDFAM(NOW).GE.-3) THEN ! FLAG field
            IF (NOWREC(LOW:LOW+10).NE.' ') THEN  ! Flag present
              IND = IABS(INDFAM(NOW))            
c	write (*,*) '-zvv-5 IND=',IND,' NFT(IND)=',NFT(IND)
              IF(NFT(IND).LE.0) GOTO 150      ! No flags stored, error
              DO 130 L = 1,NFT(IND)           ! Loop over stored flags
!----           Check flag +- epsilon
                S = STRDAT(NOW)-FLAGS(L,IND)      
                S = ABS(S)
                IF(S.GE.1.0E-7) GOTO 130
                IF (FPTR(L,IND).EQ.DATPTR(NOW,3)) THEN
                  MATFLG(L,IND) = 1
                  GOTO 290
                END IF
  130         CONTINUE

!----         Undefined flag. 
  150         CALL ERR_INSERT(2,18,10,HFLG(IND))
              CALL FLAGIT(1,DARROW,LOW,11,IERR)
            END IF

          ELSE IF (INDFAM(NOW).GT.2 .AND. INDFAM(NOW).LT.10) THEN 
                                         ! Independent variable field
            IND = INDFAM(NOW)               ! Store independent variable field

            IF (LSTFLG.LT.0) THEN        ! 1st independent variable field
              LSTFLG = IND                 ! Initialize
              IGOT = 0                     ! Initialize indep. var. found flag

            ELSE IF (IND.NE.LSTFLG) THEN ! Variable type different from last
              IF(LSTCNG.NE.0) 
     *          CALL RESTORE(LSTDAT(NOW),ICNT-I+1,STRDAT(NOW))
                                             ! Store data if change
              IF (IGOT.EQ.0) THEN          ! Data not found for last variable 
                L = LSTFLG-2
                CALL ERR_INSERT(2,15,10,MESFAM(L))
                CALL FLAGIT(1,DARROW,LOW-11,11,IERR)
              END IF
              LSTFLG = IND                 ! Reset last indep. variable field
              IGOT = 0                     ! Turn off indep. variable found flag
            END IF
 
            IF(NOWREC(LOW:LOW+10).EQ.' ') GOTO 290 ! Is field blank?
            IGOT = 1                     ! Set indep. variable data found

            IF(LSTCNG.NE.0) GOTO 290     ! Last independent variable changed?
            LSTCNG = 1                   ! Set change flag
            IF (NREC.EQ.1) THEN          ! 1st data record
              LSTDAT(NOW) = STRDAT(NOW)
            ELSE IF (MSORT(NOW).EQ.0) THEN  ! Monotonicity flag not yet set
              IF (STRDAT(NOW).EQ.LSTDAT(NOW)) THEN  ! Same as last
                LSTCNG = 0                            ! Turn off change flag
              ELSE IF (STRDAT(NOW).LT.LSTDAT(NOW)) THEN ! < last
                MSORT(NOW) = -1                       ! Set sort to descending
              ELSE                                  ! > last
                MSORT(NOW) = 1                        ! Set sort to ascending
              END IF
            ELSE                              ! Monotonicity flag set, check
              IF (STRDAT(NOW).EQ.LSTDAT(NOW)) THEN  ! Same as last
                LSTCNG = 0                            ! Turn off change flag
                GOTO 290
              ELSE IF (STRDAT(NOW).LT.LSTDAT(NOW)) THEN ! Descending order
                IF (MSORT(NOW).LE.0) GOTO 290           ! Should be ascending
              ELSE IF (STRDAT(NOW).GT.LSTDAT(NOW)) THEN ! Ascending order
                IF (MSORT(NOW).GE.0) GOTO 290           ! Should be descending
              END IF
              CALL ERR_INSERT(2,20,10,HEADS(NOW,3)) 
              CALL FLAGIT(1,DARROW(J),LOW,11,IERR)
           END IF  
          END IF
  290   CONTINUE
  300 CONTINUE

  400 IF (NDFLD.EQ.0) THEN                ! Dependent data field not found
        IERR = 1
        CALL ERR_MES(1,54)                  ! Error message (ERRSUB entry)
      END IF

      CALL RESTORE(LSTDAT,ICNT,STRDAT)    ! Store record
      RETURN
      END


















      SUBROUTINE NUCFLD(IFIELD,STRING,NUM,IERRU,INE,MULT)

!* Checks Nuclide field codes
!*   IFIELD: field to be processeed
!*            1: REACTION SF1
!*            2: REACTION SF2
!*            3: REACTION SF3
!*            4: REACTION SF4
!*            5: HALF-LIFE, DECAY-DATA, DECAY-MON, RAD-DET
!*            6: PART-DET
!*            7: REACTION SF7, EN-SEC (Particle Designator)
!*   STRING: input array (contains only one code)
!*   NUM:    length of input string
!*   IERRU:  nuclide flag (message written)
!*            1 = illegal code
!*            2 = nuclide code not in dictionary
!*            3 = isomeric state code not legal
!*            6 = illegal use of particle
!*           12 = blank code
!*           14 = illegal nuclide
!*           20 = illegal leading blanks
!*   INE:    internal numerical equivalent for nuclide
!*   MULT:   multiplicity factor
!***

      CHARACTER*(*) STRING
      CHARACTER*80 DICLINE
      CHARACTER*20 ISOTPE
      CHARACTER*20 ISOTPECHK
      CHARACTER*10 ELEMS(3)/'ELEM      ','MASS      ','ELEM/MASS '/
      CHARACTER*2 MTYPE(7)/'G ','M ','M1','M2','M3','M4','T '/
      CHARACTER MNSTATE(4)/'1','2','3','4'/
      CHARACTER MLIM(4)/'-', ' ', '+', '/'/
      CHARACTER USES(4),STABL,NSTATE

      CHARACTER*2 HLUNITS
      CHARACTER USES227

      DIMENSION INEFY(3),NUSE(7)

      DATA NUSE/0,2,3,0,1,0,4/      ! Particle use for reaction fields
      DATA NMETA/7/                 ! # of metastable state codes

      DATA INEFY/-1000,-2000,-3000/ ! ELEM, MASS, ELEM/MASS equivalents.
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW


!!!!  Initialize
      IERRU = 0                     ! error flag off
      INE = 0                       ! int.num.equiv.
      MULT = 1                      ! multiplicity factor
      IF(IFIELD.EQ.1) INE3S = 0     ! scattering flag

!---- Set fields for error messages

      IF (IFIELD.EQ.5 .OR. IFIELD.EQ.6) THEN
        IFLD = 1
      ELSE
        IFLD = IFIELD
      END IF

      L = 1                            ! Initialize position in input string

      IF (STRING(1:1).EQ.' ') THEN     ! Illegal blank in first character
         IERRU = 20
         GOTO 810
      END IF

!---- Check for numeric 1st character

      IF (IFIELD.EQ.1 .OR. IFIELD.GE.4) THEN
        IF(STRING(1:1).GE.'0' .AND. STRING(1:1).LE.'9') GOTO 200
        IF(IFIELD.GT.4) GOTO 410
        GOTO 700
      ELSE IF (IFIELD.EQ.2 .OR. IFIELD.EQ.3) THEN
        IF(STRING(1:NUM).EQ.'0') GOTO 440
        IF (STRING(1:1).GE.'1' .AND. STRING(1:1).LE.'9') THEN ! NUM. 1ST CHAR.
          IF(NUM.LT.5) GOTO 400           ! Not Z-S-A, check multiplicity factor
        ELSE
          IF (IFIELD.EQ.2) GOTO 410       ! Check particle code
          GOTO 370                        ! Check process, particle code
        END IF
      END IF

!!!!  Z-S-A-M format checking


  200 CALL PARS_CHR(STRING(L:L+3),4,MLIM,2,KPT,ICHR,*390) 
                                          ! Read Z (up to 3 characters)
      LOW = 3-KPT+1                       ! Store starting posit. of Z in string

!---- Read symbol (up to 2 characters)
      CALL INCRMT_NEW(L,KPT+1,NUM,*800)   ! Reset low character
      CALL PARS_CHR(STRING(L:L+2),3,MLIM,2,KPT,ICHR,*800)

!---- READ A (UP TO 3 CHARACTERS)
      CALL INCRMT_NEW(L,KPT+1,NUM,*800)   ! Reset low character
      N2 = NUM-L+1                        ! Set max. size of string
      CALL PARS_CHR(STRING(L:NUM),N2,MLIM,2,KPT,ICHR,*262)
  262 IF(KPT.GT.3) GOTO 800               ! Check for max. of 3 chars in A

      LAST = L+KPT-1

      ISOTPE = ' '
      ISOTPE(LOW:LOW+LAST-1) = STRING(1:LAST) ! Readjust isotope for lookup

	MMAX=0
	ISOTPECHK=ISOTPE
	iISOTPECHK=1
	NSTATE=' '
	iBasic=1
	iGround=0
	ISOTPECHK(LOW+LAST:LOW+LAST)='-'
	ISOTPECHK(LOW+LAST+1:LOW+LAST+1)='M'
	if (LAST+1.lt.NUM) then
	    iBasic=0
	    if (STRING(LAST+2:NUM).eq.'G') iGround=1
	    do M=1,4
		iISOTPECHK=M
		CALL DANGETN_NEW(227,ISOTPECHK,iISOTPECHK
     *		,0,0,DICLINE,IERR) ! LOOK UP NUCLIDE
		if (IERR.eq.0) then
		    NSTATE=MNSTATE(M)
		    MMAX=M
		end if
	    end do
	end if

	if ((iBasic.ne.0).or.((iGround.ne.0))) then
	    CALL DANGET_NEW(227,ISOTPE,0,0,DICLINE,IERR)    ! LOOK UP NUCLIDE
	    if (IERR.ne.0)  then
		CALL DANGET_NEW(209,ISOTPE,0,0,DICLINE,IERR)! LOOK UP COMPOUND
	    end if
	    if (IERR.ne.0)  then
		CALL DANGET_NEW(227,ISOTPECHK,0,0,DICLINE,IERR) ! LOOK UP NUCLIDE-M
c		! -M exists, bat main isitope (Basic) does not exist
	    end if
	else
	    CALL DANGET_NEW(227,ISOTPECHK,0,0,DICLINE,IERR) ! LOOK UP NUCLIDE-M
	end if
      READ(DICLINE,1110) INE,USES227,HLUNITS ! Set INE and USE flags
 1110 FORMAT(6X,I6,1X,A1,18X,A2)
	STABL='S'
	if (HLUNITS.ne.'  ') STABL=' '
	USES(1)='1'
	USES(2)='2'
	USES(3)='3'
	USES(4)='4'
	if (USES227.eq.'X') USES(1)='X'
	if (USES227.eq.'Z') USES(3)='Z'
c	write(*,*) '227.INE=',INE,' USES=',USES,' STABL=',STABL
c     *  ,' U227=',USES227,' UNITS=',HLUNITS


c      CALL DANGET_NEW(27,ISOTPE,0,0,DICLINE,IERR) ! LOOK UP NUCLIDE
cc	write (*,*) ' 27:DICLINE=',DICLINE(1:64)

      IF (IERR.NE.0) THEN                     ! Illegal code
        GOTO 800
      END IF

c      READ(DICLINE,1100) INE,USES,STABL,NSTATE ! Set INE and USE flags
 1100 FORMAT(5X,I6,4A1,6X,A1,1X,A1)

!---- Check validity of nuclide in field.
      IERRNOW = 0
      IF (IFIELD.EQ.1) THEN                    ! SF1
        IF(USES(1).EQ.'1' .OR. USES(1).EQ.'X') GOTO 350 
        IF (USES(4).NE.'4') THEN
          IERRNOW = 1
        ELSE                                   ! Valid only for nuclear quantity
          CALL ERR_WARN(1,15)                    ! Error message
          CALL FLAGIT(0,ARROW,0,0,IERRU)
          GOTO 350
        END IF
      ELSE IF (IFIELD.EQ.2) THEN               ! SF2
        IF(USES(2).NE.'2') IERRNOW = 1
      ELSE IF (IFIELD.EQ.3) THEN               ! SF3, EN-SEC
        IF(USES(3).NE.'3') IERRNOW = 1
      ELSE IF (IFIELD.EQ.4) THEN               ! SF4
        IF(STRING(L:LAST).EQ.'0' .AND. INE.NE.0) THEN ! Natural element,not GAM
          IF(INE3S.EQ.0) IERRNOW = 1
        ELSE
          IF(USES(3).NE.'3' .AND. USES(3).NE.'Z') IERRNOW = 1
        END IF
      ELSE IF (IFIELD.EQ.6) THEN               ! PART-DET
        IF(USES(3).NE.'3') IERRNOW = 1
      ELSE                                     ! SF7, DECAY-DATA
        IF(USES(3).NE.'3' .AND. USES(3).NE.'Z') IERRNOW = 1
      END IF

      IF (IERRNOW.NE.0) THEN
        CALL ERR_WARN(1,13)                      ! Print warning
        CALL FLAGIT(0,ARROW,0,0,IERRU)
      END IF

  350 IF(ICHR.NE.1) GOTO 365

!---- Read isomeric state code

  360 CALL INCRMT_NEW(L,KPT+1,NUM,*800)        ! Reset low character

      N = NUM-L+1
      CALL PARS_CHR(STRING(L:NUM),N,MLIM(2),3,KPT,ICHR,*362)

  362 IF (KPT.EQ.0) THEN                       ! Blank code
        CALL ERR_FIELD(1,IFLD,12)
        IERRU = 12
        RETURN
      END IF

      M = LOOKUP(STRING(L:L+KPT-1),MTYPE,NMETA) ! Look up code
      IF(M.EQ.0) GOTO 800

      IF (M.EQ.1) THEN                         ! Ground state code
        IF(ICHR.EQ.1) GOTO 360                   ! Complex isomeric state code
        IF (NSTATE.EQ.' ') THEN                  ! 'G' Illegal if no metastate
          CALL ERR_WARN(1,12)                      ! Print warning
          CALL FLAGIT(0,ARROW,0,0,IERRU)
        END IF
      ELSE IF (M.LE.6) THEN                    ! Metastable state code
        IF(M.GT.2) M = M-1                       ! Reset code equiv. for M1,etc.

!----   Set # of metastable states allowed
        IF (NSTATE.EQ.'A') THEN               
          MS = 1
        ELSE
          READ(NSTATE,3100) MS
        END IF

        IF (MS.LT.(M-1)) THEN                  ! More than allowed #
          CALL ERR_WARN(1,12)                    !Print warning
          CALL FLAGIT(0,ARROW,0,0,IERRU)
        END IF
      END IF

  363 IF(ICHR.NE.0) GOTO 360                   ! Not end of string
      RETURN

  365 IF (IFIELD.EQ.5) THEN                    ! Decay particle
        IF (STABL.EQ.'S') THEN                   ! Stable isotope
          CALL ERR_MES(1,49)
          CALL FLAGIT(0,ARROW,0,0,IERRU)
        ELSE IF (NSTATE.NE.' ' .AND. NSTATE.NE.'A') THEN ! Isomers exist
          CALL ERR_WARN(1,10)                              ! Print warning
          CALL FLAGIT(0,ARROW,0,0,IERRU)
        END IF
      END IF
      RETURN

!!!!  Process code checking (FIELD 3)

!---- Check for process code
  370 CALL DANGET_NEW(30,STRING(1:NUM),0,0,DICLINE,IERR)
      IF(IERR.NE.0) GOTO 410                     ! Not process, branch 

      READ(DICLINE,3000) INE                     ! Get int.num.equiv.
      INE = -INE
      IF(INE.GE.-70 .AND. INE.LE.-40) INE3S = 1  ! Set scattering flag
      RETURN

!!!!  Particle code checking (FIELDS 2,3,5)

  390 IF(IFIELD.NE.3) GOTO 800

!---- Check multiplicity factor.
  400 L = L+1
      IF(STRING(L:L).GE.'0' .AND. STRING(L:L).LE.'9') GOTO 400
      READ(STRING(1:L-1),4000,ERR=800) MULT
      IF (MULT.GT.99) THEN
        IERRU = 18
        CALL ERR_FIELD(1,IFIELD,13)              ! Error message
      END IF

!---- Check particle code
  410 CALL DANGET_NEW(33,STRING(L:NUM),0,0,DICLINE,IERR)
      IF(IERR.NE.0) GOTO 800                     ! Illegal code

      READ(DICLINE,4000) INE,INE7,USES           ! Get INE, USE flags
      IF(IFIELD.EQ.7) INE = INE7
      N = NUSE(IFIELD)
      IF (USES(N).EQ.' ') THEN
        IERRU = 6
        GOTO 860
      END IF
      RETURN

  440 MULT = -1                                  ! Zero in SF, no multiplicity.
      RETURN

!!!!  ELEM,MASS checking (Fields 1, 4)

!---- Check for ELEM, MASS codes.
  700 IF(NUM.GT.10) GOTO 800
      I = LOOKUP(STRING(1:NUM),ELEMS,3)
      IF(I.EQ.0) GOTO 800
      INE = INEFY(I)
      RETURN

  800 IERRU = 14
  810 INE = -9999
  860 CALL ERR_FIELD(1,IFLD,IERRU)               ! Error message 

      RETURN

 3000 FORMAT(I10)
 3100 FORMAT(I1)
 4000 FORMAT(I6,I5,4A1)
      END







      SUBROUTINE NUCFLDold(IFIELD,STRING,NUM,IERRU,INE,MULT)

!* Checks Nuclide field codes
!*   IFIELD: field to be processeed
!*            1: REACTION SF1
!*            2: REACTION SF2
!*            3: REACTION SF3
!*            4: REACTION SF4
!*            5: HALF-LIFE, DECAY-DATA, DECAY-MON, RAD-DET
!*            6: PART-DET
!*            7: REACTION SF7, EN-SEC (Particle Designator)
!*   STRING: input array (contains only one code)
!*   NUM:    length of input string
!*   IERRU:  nuclide flag (message written)
!*            1 = illegal code
!*            2 = nuclide code not in dictionary
!*            3 = isomeric state code not legal
!*            6 = illegal use of particle
!*           12 = blank code
!*           14 = illegal nuclide
!*           20 = illegal leading blanks
!*   INE:    internal numerical equivalent for nuclide
!*   MULT:   multiplicity factor
!***

      CHARACTER*(*) STRING
      CHARACTER*80 DICLINE
      CHARACTER*20 ISOTPE
      CHARACTER*10 ELEMS(3)/'ELEM      ','MASS      ','ELEM/MASS '/
      CHARACTER*2 MTYPE(7)/'G ','M ','M1','M2','M3','M4','T '/
      CHARACTER MLIM(4)/'-', ' ', '+', '/'/
      CHARACTER USES(4),STABL,NSTATE

      DIMENSION INEFY(3),NUSE(7)

      DATA NUSE/0,2,3,0,1,0,4/      ! Particle use for reaction fields
      DATA NMETA/7/                 ! # of metastable state codes

      DATA INEFY/-1000,-2000,-3000/ ! ELEM, MASS, ELEM/MASS equivalents.
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

!!!!  Initialize
      IERRU = 0                     ! error flag off
      INE = 0                       ! int.num.equiv.
      MULT = 1                      ! multiplicity factor
      IF(IFIELD.EQ.1) INE3S = 0     ! scattering flag

!---- Set fields for error messages

      IF (IFIELD.EQ.5 .OR. IFIELD.EQ.6) THEN
        IFLD = 1
      ELSE
        IFLD = IFIELD
      END IF

      L = 1                            ! Initialize position in input string

      IF (STRING(1:1).EQ.' ') THEN     ! Illegal blank in first character
         IERRU = 20
         GOTO 810
      END IF

!---- Check for numeric 1st character

      IF (IFIELD.EQ.1 .OR. IFIELD.GE.4) THEN
        IF(STRING(1:1).GE.'0' .AND. STRING(1:1).LE.'9') GOTO 200
        IF(IFIELD.GT.4) GOTO 410
        GOTO 700
      ELSE IF (IFIELD.EQ.2 .OR. IFIELD.EQ.3) THEN
        IF(STRING(1:NUM).EQ.'0') GOTO 440
        IF (STRING(1:1).GE.'1' .AND. STRING(1:1).LE.'9') THEN ! NUM. 1ST CHAR.
          IF(NUM.LT.5) GOTO 400           ! Not Z-S-A, check multiplicity factor
        ELSE
          IF (IFIELD.EQ.2) GOTO 410       ! Check particle code
          GOTO 370                        ! Check process, particle code
        END IF
      END IF

!!!!  Z-S-A-M format checking


  200 CALL PARS_CHR(STRING(L:L+3),4,MLIM,2,KPT,ICHR,*390) 
                                          ! Read Z (up to 3 characters)
      LOW = 3-KPT+1                       ! Store starting posit. of Z in string

!---- Read symbol (up to 2 characters)
      CALL INCRMT_NEW(L,KPT+1,NUM,*800)   ! Reset low character
      CALL PARS_CHR(STRING(L:L+2),3,MLIM,2,KPT,ICHR,*800)

!---- READ A (UP TO 3 CHARACTERS)
      CALL INCRMT_NEW(L,KPT+1,NUM,*800)   ! Reset low character
      N2 = NUM-L+1                        ! Set max. size of string
      CALL PARS_CHR(STRING(L:NUM),N2,MLIM,2,KPT,ICHR,*262)
  262 IF(KPT.GT.3) GOTO 800               ! Check for max. of 3 chars in A

      LAST = L+KPT-1

      ISOTPE = ' '
      ISOTPE(LOW:LOW+LAST-1) = STRING(1:LAST) ! Readjust isotope for lookup
	write (*,*) 'x27:ISOTPE=',ISOTPE
      CALL DANGET_NEW(27,ISOTPE,0,0,DICLINE,IERR) ! LOOK UP NUCLIDE
      IF (IERR.NE.0) THEN                     ! Illegal code
        GOTO 800
      END IF

c	write (*,*) 'x27:DICLINE=',DICLINE
      READ(DICLINE,1100) INE,USES,STABL,NSTATE ! Set INE and USE flags
	write(*,*) 'INE=',INE,' USES=',USES,' STABL=',STABL
     *  ,' NSTATE=[',NSTATE,']'

!---- Check validity of nuclide in field.
      IERRNOW = 0
      IF (IFIELD.EQ.1) THEN                    ! SF1
        IF(USES(1).EQ.'1' .OR. USES(1).EQ.'X') GOTO 350 
        IF (USES(4).NE.'4') THEN
          IERRNOW = 1
        ELSE                                   ! Valid only for nuclear quantity
          CALL ERR_WARN(1,15)                    ! Error message
          CALL FLAGIT(0,ARROW,0,0,IERRU)
          GOTO 350
        END IF
      ELSE IF (IFIELD.EQ.2) THEN               ! SF2
        IF(USES(2).NE.'2') IERRNOW = 1
      ELSE IF (IFIELD.EQ.3) THEN               ! SF3, EN-SEC
        IF(USES(3).NE.'3') IERRNOW = 1
      ELSE IF (IFIELD.EQ.4) THEN               ! SF4
        IF(STRING(L:LAST).EQ.'0' .AND. INE.NE.0) THEN ! Natural element,not GAM
          IF(INE3S.EQ.0) IERRNOW = 1
        ELSE
          IF(USES(3).NE.'3' .AND. USES(3).NE.'Z') IERRNOW = 1
        END IF
      ELSE IF (IFIELD.EQ.6) THEN               ! PART-DET
        IF(USES(3).NE.'3') IERRNOW = 1
      ELSE                                     ! SF7, DECAY-DATA
        IF(USES(3).NE.'3' .AND. USES(3).NE.'Z') IERRNOW = 1
      END IF

      IF (IERRNOW.NE.0) THEN
        CALL ERR_WARN(1,13)                      ! Print warning
        CALL FLAGIT(0,ARROW,0,0,IERRU)
      END IF

  350 IF(ICHR.NE.1) GOTO 365

!---- Read isomeric state code

  360 CALL INCRMT_NEW(L,KPT+1,NUM,*800)        ! Reset low character

      N = NUM-L+1
      CALL PARS_CHR(STRING(L:NUM),N,MLIM(2),3,KPT,ICHR,*362)

  362 IF (KPT.EQ.0) THEN                       ! Blank code
        CALL ERR_FIELD(1,IFLD,12)
        IERRU = 12
        RETURN
      END IF

      M = LOOKUP(STRING(L:L+KPT-1),MTYPE,NMETA) ! Look up code
      IF(M.EQ.0) GOTO 800

      IF (M.EQ.1) THEN                         ! Ground state code
        IF(ICHR.EQ.1) GOTO 360                   ! Complex isomeric state code
        IF (NSTATE.EQ.' ') THEN                  ! 'G' Illegal if no metastate
          CALL ERR_WARN(1,12)                      ! Print warning
          CALL FLAGIT(0,ARROW,0,0,IERRU)
        END IF
      ELSE IF (M.LE.6) THEN                    ! Metastable state code
        IF(M.GT.2) M = M-1                       ! Reset code equiv. for M1,etc.

!----   Set # of metastable states allowed
        IF (NSTATE.EQ.'A') THEN               
          MS = 1
        ELSE
          READ(NSTATE,3100) MS
        END IF

        IF (MS.LT.(M-1)) THEN                  ! More than allowed #
          CALL ERR_WARN(1,12)                    ! Print warning
          CALL FLAGIT(0,ARROW,0,0,IERRU)
        END IF
      END IF

  363 IF(ICHR.NE.0) GOTO 360                   ! Not end of string
      RETURN

  365 IF (IFIELD.EQ.5) THEN                    ! Decay particle
        IF (STABL.EQ.'S') THEN                   ! Stable isotope
          CALL ERR_MES(1,49)
          CALL FLAGIT(0,ARROW,0,0,IERRU)
        ELSE IF (NSTATE.NE.' ' .AND. NSTATE.NE.'A') THEN ! Isomers exist
          CALL ERR_WARN(1,10)                              ! Print warning
          CALL FLAGIT(0,ARROW,0,0,IERRU)
        END IF
      END IF
      RETURN

!!!!  Process code checking (FIELD 3)

!---- Check for process code
  370 CALL DANGET_NEW(30,STRING(1:NUM),0,0,DICLINE,IERR)
      IF(IERR.NE.0) GOTO 410                     ! Not process, branch 

      READ(DICLINE,3000) INE                     ! Get int.num.equiv.
      INE = -INE
      IF(INE.GE.-70 .AND. INE.LE.-40) INE3S = 1  ! Set scattering flag
      RETURN

!!!!  Particle code checking (FIELDS 2,3,5)

  390 IF(IFIELD.NE.3) GOTO 800

!---- Check multiplicity factor.
  400 L = L+1
      IF(STRING(L:L).GE.'0' .AND. STRING(L:L).LE.'9') GOTO 400
      READ(STRING(1:L-1),4000,ERR=800) MULT
      IF (MULT.GT.99) THEN
        IERRU = 18
        CALL ERR_FIELD(1,IFIELD,13)              ! Error message
      END IF

!---- Check particle code
  410 CALL DANGET_NEW(33,STRING(L:NUM),0,0,DICLINE,IERR)
      IF(IERR.NE.0) GOTO 800                     ! Illegal code

      READ(DICLINE,4000) INE,INE7,USES           ! Get INE, USE flags
      IF(IFIELD.EQ.7) INE = INE7
      N = NUSE(IFIELD)
      IF (USES(N).EQ.' ') THEN
        IERRU = 6
        GOTO 860
      END IF
      RETURN

  440 MULT = -1                                  ! Zero in SF, no multiplicity.
      RETURN

!!!!  ELEM,MASS checking (Fields 1, 4)

!---- Check for ELEM, MASS codes.
  700 IF(NUM.GT.10) GOTO 800
      I = LOOKUP(STRING(1:NUM),ELEMS,3)
      IF(I.EQ.0) GOTO 800
      INE = INEFY(I)
      RETURN

  800 IERRU = 14
  810 INE = -9999
  860 CALL ERR_FIELD(1,IFLD,IERRU)               ! Error message 

      RETURN

 1100 FORMAT(5X,I6,4A1,6X,A1,1X,A1)
 3000 FORMAT(I10)
 3100 FORMAT(I1)
 4000 FORMAT(I6,I5,4A1)
      END









      SUBROUTINE PARSER(IWAY,LOW,LST,RUNIT)

!* Scans REACTION string and checks each subfield.
!*   IWAY:  1  =  REACTION
!*          2  =  MONITOR
!*          3  =  ASSUMED
!*          4  =  INC-SOURCE
!*   LOW:   Starting position in array for scan.
!*   LST:   Last position in array to be scanned
!*   RUNIT: Data units expected from REACTION

!---- Input record. (Set in READ_REC)
      COMMON/ALLCOM/KEY2,IPTR,KARD,ISEQ
        CHARACTER*10 KEY2
        CHARACTER*1  IPTR
        CHARACTER*55 KARD
        CHARACTER*13 ISEQ

!---- Current AN, SAN, section. (Set in READ_REC)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

!---- Independent variable families required. (Initialized in BIB_SUB)
      COMMON/FAMLY_REQ/NEEDS(7),NEEDFL,NOTCK

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

!---- Flags which link reaction to quantity (Set in PARSER)
!-    IHEAVY: Heaviest particle error flag; 
!-             0 = no error
!-            >0 = error, set to position in string 
!-    INEOUT: Sum of output particles (1000*Z + A)
!-    SUBF4:  Subfield 4 checking flag
!-            -1 = not expected for nuclear quantity
!-            -2 = error in SF1, 2, 3 and/or 4 (can't check SF4).
!-             0 = may or may not be present
!-             1 = should be present
!-            >1 = error, set to position in string
!-    NUCQ:   Nuclear quantity flag
      COMMON/REACT_FLAGS/IHEAVY,INEOUT,SUBF4,NUCQ
        INTEGER SUBF4

      CHARACTER DELIM(4)/'(', ',', ')', '+'/
      CHARACTER DELIM2(3)/',', '/', ')'/
      CHARACTER*4 RUNIT

!!!!  Initialize
      RUNIT = ' '        ! units expected
      NOW = LOW          ! current position in array KARD
      NUCQ = 0           ! nuclear quantity flag off
      IERRU = 0          ! nuclide use error flag off
      IERRF = 0          ! other error flag off
      SUBF4 = 1          ! to SF4 expected
      NOTCK = 0          ! independent variables check on

!*    Scan SF1.

      IFIELD = 1                             ! Set field to 1
      CALL PARSCHR(KARD,NOW,KPT,NUM1,30,DELIM,3,ICHR,*900)
      IF (ICHR.NE.1) THEN                    ! If delimiter '(' not found
        CALL ERR_MES(1,36)                     ! Set error markers
        CALL FLAGIT(1,ARROW,NOW+11,NUM1,IERR)
        RETURN
      END IF
        
      IF (NUM1.EQ.0) THEN                    ! If SF1 missing
        CALL ERR_FIELD(1,IFIELD,15)              ! Error message
        CALL FLAGIT(1,ARROW,NOW+11,1,IERRU)      ! Set error markers
        GOTO 10
      END IF

      CALL NUCFLD(1,KARD(NOW:KPT),NUM1,IERR1,INZA,MULT) ! Check nuclide

      IF (IERR1.GT.0) THEN                   ! If error
        CALL FLAGIT(1,ARROW,NOW+11,NUM1,IERRU)   ! Set error markers
        IF (IERR1.LE.2) THEN                   ! Illegal code, numeq. not found
          SUBF4 = -2                             ! Skip i/o particle check
          GOTO 10
        END IF
      END IF

      INA = MOD(INZA,1000)                   ! Store A for isotope
      IF(INA.EQ.0) SUBF4 = -1                ! Set SF4 blank expected
      IF(IWAY.NE.1) GOTO 10

!---- For variable target, store required headings and pointers.
      IF (INZA.EQ.-1000 .OR. INZA.EQ.-3000) THEN  ! If ELEM or ELEM/MASS
        CALL ADD_HEAD('MASS      ',IPTR,*5)
      END IF
    5 IF (INZA.EQ.-2000 .OR. INZA.EQ.-3000) THEN  ! If MASS or ELEM/MASS
        CALL ADD_HEAD('ELEMENT   ',IPTR,*10)
      END IF

!!!!  Scan SF2

   10 IFIELD = 2                              
      NOW = KPT+2
      CALL PARSCHR(KARD,NOW,KPT,NUM2,15,DELIM(2),2,ICHR,*900)

      IF (ICHR.NE.1) THEN                    ! If delimiter ',' not found
        CALL ERR_MES(1,36)                     ! Set error markers
        CALL FLAGIT(1,ARROW,NOW+11,NUM2,IERR)
        GOTO 990
      END IF

      IF (NUM2.EQ.0) THEN                    ! If SF2 missing
        CALL ERR_FIELD(1,IFIELD,15)                ! Error message
        CALL FLAGIT(1,ARROW,NOW+11,1,IERRU)      ! Set error markers
        GOTO 30
      END IF

      IF(KARD(NOW:KPT).EQ.'0') THEN          ! If nuclear quantity
        NEEDFL = INTSAN                        ! Monitor not needed
        SUBF4 = -1                             ! SF4 blank expected
        NUCQ = 1                               ! Nuclear quantity flag
      END IF

      CALL NUCFLD(2,KARD(NOW:KPT),NUM2,IERR1,INE2,MULT) ! Check code
      IF (IERR1.GT.0) THEN                   ! If error
        CALL FLAGIT(1,ARROW,NOW+11,NUM2,IERRU)   ! Set error markers 
        IF (IERR1.LE.2) THEN                   ! Illegal code, no int.num.eq.
          SUBF4 = -2                             ! Skip i/o particle seq.check
          GOTO 30
        END IF
      END IF
      IF (INE2.GE.0) INZA = INZA+INE2        ! Add INE to input Z,A

!!!!  Scan SF3, up to 4 codes.

   30 IFIELD = 3          
      MAXZA = 0                              ! Initialize max. Z,A for SF3
      IHEAVY = 0                             ! Initialize heaviest part. index
      NZAOUT = 0                             ! Initialize sum of output Z,A

      DO 50 I = 1,4
        NOW = KPT+2
        CALL PARSCHR(KARD,NOW,KPT,NUM,15,DELIM(3),2,ICHR,*900)

        IF(NUM.EQ.0) THEN                    ! If blank field
          CALL ERR_FIELD(1,IFIELD,15)            ! Error message 
          CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR1)  ! Set error markers 
          GOTO 45
        END IF

!----   Check code. Determine if outgoing particle or processes (negative INE).
        II = I
        CALL NUCFLD(3,KARD(NOW:KPT),NUM,IERR1,INE3,MULT)

        IF (IERR1.GT.0) THEN                 ! If error
          CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRU) ! Set error markers
          IF (IERR1.LE.2) THEN                ! Illegal code
            SUBF4 = -2                        ! Skip i/o particle equiv.check
            GOTO 45
          END IF
        END IF
    
        IF (INE3.GE.-70 .AND. INE3.LE.-40) THEN ! Scattering
          IF(SUBF4.EQ.-1) SUBF4 = 1           ! SF4 should be present
          IF (INE2.NE.0) THEN                 ! Input particle found
            IF(NZAOUT.EQ.INE2) THEN             ! Scattering already found
              CALL ERR_FIELD(1,IFIELD,15)         ! Error message
              CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRU) ! Set error markers
              GOTO 45        
            ELSE                                ! Scattering code ok
              INE3 = INE2                         ! Set field 3 = field 2 
            END IF
          END IF

        ELSE IF (INE3.EQ.-90) THEN        ! Emission, set SF4 required
          SUBF4 = 1                 
          NZAOUT = -1
        ELSE IF (INE3.LT.0) THEN          ! Other processes
          SUBF4 = -1                         ! Set blank SF4 expected
          IF(INE3.EQ.-10) NEEDFL = INTSAN    ! 'TOT', set monitor not needed

        ELSE                              ! Particle or element code
          IF (INE3.LT.MAXZA) THEN           ! Particles not in order
            IHEAVY = NOW+11                   ! Store position
          ELSE                              ! Particles in order
            MAXZA = INE3                      ! Store heaviest particle
          END IF
          IF (MULT.LT.0) THEN               ! No output particles
            SUBF4 = -1
          ELSE                              ! Output particles found
            INE3 = INE3*MULT                  ! Multiply by multiplicity factor
          END IF
        END IF

        IF(NZAOUT.GE.0) NZAOUT = NZAOUT+INE3 ! Add INE to output Z,A

   45   IF(ICHR.EQ.1) GOTO 60
   50 CONTINUE
      GOTO 900
      
!!!!  Scan SF4

   60 IFIELD = 4                        
      INEOUT = 0                            ! Reset output particle equiv. to 0
      NOW = KPT+2
      CALL PARSCHR(KARD,NOW,KPT,NUM,20,DELIM(2),2,ICHR,*900)

      IF (NUM.EQ.0) THEN                    ! No reaction product given
        IF(INA.GE.300) GOTO 300                ! Compound
        IF (SUBF4.GT.0) SUBF4 = NOW+11        ! SF4 expected
        GOTO 300
      ELSE
        IF (INA.EQ.0) SUBF4 = 0             ! SF4 may or may not be present
      END IF

      CALL NUCFLD(4,KARD(NOW:KPT),NUM,IERR1,INEOUT,MULT) ! Check code
      IF (IERR1.GT.0) THEN                  ! Error
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRU)  ! Set error markers
        IF (IERR1.LE.2) THEN                  ! Illegal code, no int.num.eq.
          SUBF4 = -2                            ! Skip i/o particle equiv. check
          GOTO 300
        ELSE IF (IERR1.EQ.6) THEN             ! Illegal use of nuclide
          GOTO 300
        END IF
      END IF

      IF(NZAOUT.GE.0) NZAOUT = NZAOUT+INEOUT ! Add INE to output Z,A

      IF (INEOUT.GT.-1000) THEN            ! Not ELEM or MASS
        IF (SUBF4.EQ.-2) THEN                ! SF4 not to be checked
          SUBF4 = 1                            ! Reset SF4 check code
        ELSE IF (NZAOUT.GE.0) THEN           ! Not process code in SF3
          IF(MAXZA.GT.INEOUT) THEN             ! SF4 should be heaviest
            IF(IHEAVY.EQ.0) IHEAVY = NOW+11      ! Store position in array
          END IF
          IF (NUM1.NE.0 .AND. NUM2.NE.0) THEN  ! SF1 and SF2 present
            IF (SUBF4.EQ.-1) THEN                ! SF4 not expected
              CALL ERR_FIELD(1,4,14)                ! Error message
              CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRU) ! Set error markers
            ELSE IF (SUBF4.GT.0) THEN            ! SF4 expected (INA>0)
              IF (INZA.NE.NZAOUT) THEN
                 CALL ERR_MES(1,64)                 ! Error message
                 CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRU)
              END IF
            END IF
          END IF
        END IF

      ELSE                                    ! ELEM or MASS
        IF(IWAY.NE.1) GOTO 300                   ! Branch if not REACTION
        NEEDS(7) = INTSAN                       ! Set indep. variable required

!----   Set required headings
        IF (INEOUT.EQ.-1000 .OR. INEOUT.EQ.-3000) THEN  ! Variable element
          CALL ADD_HEAD('ELEMENT   ',IPTR,*70)
        END IF
   70   IF (INEOUT.EQ.-2000 .OR. INEOUT.EQ.-3000) THEN  ! Variable mass
          CALL ADD_HEAD('MASS      ',IPTR,*300)
        END IF
      END IF

C     Scan SF 5-8

  300 IF (IWAY.EQ.4) GOTO 990                  ! INC-SPECT (no quantity fields)
      IFIELD = 5                         
      NOW = KPT+2
      IF(ICHR.EQ.2) GOTO 980                   ! End of code string

      CALL QUANT_CHECK(IWAY,IFIELD,NOW,LST,NUM,RUNIT,IERRU,*350)

      IF (NEEDS(7).NE.0) THEN                  ! Product required.
        IF(INEOUT.GT.-1000) NEEDS(7) = -INTSAN   ! Set found, if in SF4.
      END IF

      IF(NOW.GE.LST) GOTO 990
      GOTO 800

C     Quantity not found. Separate fields and check codes.

  350 IERR2 = 0
      LOWM = NOW                             ! Store starting position of SF5

  360 IF (KARD(NOW:NOW).EQ.'(') THEN             ! Check for left paren in code.
        ND = 2
      ELSE
        ND = 3
      END IF

      CALL PARSCHR(KARD,NOW,KPT,NUM1,5,DELIM2,ND,ICHR,*900)
      IF (NUM1.NE.0) THEN                    ! Field contains code
        CALL DANVER_NEW(31,KARD(NOW:KPT),0,IERR1,*380)
        CALL FLAGIT(1,ARROW,NOW+11,NUM1,IERR2)   ! Set error markers
        I = IFIELD
        CALL ERR_FIELD(1,I,14)
      END IF
  380 IF(ICHR.EQ.3) GOTO 500                 ! End of REACTION string
      NOW = KPT+2
      IF(ICHR.EQ.2) GOTO 360                 ! Multiple codes in subfield

      DO 480 IFIELD = 6,8
  420   CALL PARSCHR(KARD,NOW,KPT,NUM1,5,DELIM2,3,ICHR,*900)
        IF (NUM1.NE.0) THEN                    ! Field contains code
          KDIC = 26+IFIELD
          CALL DANVER_NEW(KDIC,KARD(NOW:KPT),0,IERR1,*450)
          CALL FLAGIT(1,ARROW,NOW+11,NUM1,IERR2)   ! Set error markers
          I = IFIELD
          CALL ERR_FIELD(1,I,14)
        END IF
  450   IF(ICHR.EQ.3) GOTO 500                 ! End of REACTION string
        NOW = KPT+2
        IF(ICHR.EQ.2) GOTO 420                 ! Multiple codes in subfield
  480 CONTINUE
      NOW = NOW-1

!---- REACTION not in dictionary
  500 IF (IERR2.EQ.0) THEN                     ! Fields ok
        CALL ERR_MES(1,66)                        ! Error message
        CALL FLAGIT(1,ARROW,LOWM+11,NUM,IERRU)
      ELSE                                     ! Field not ok
        IERRU = 1                                ! Set error flag
      END IF

      IF (ICHR.EQ.3) THEN                      ! End of string
        DO 610 I = KPT,1,-1                      ! Flag trailing commas
          IF(KARD(I:I).NE.',') GOTO 990
          CALL FLAGIT(1,ARROW,I+11,1,IERR)         ! Set error markers
  610   CONTINUE
        GOTO 990
      END IF
      NOW = KPT+2

!!!!  Scan SF9

  800 CALL PARSCHR(KARD,NOW,KPT,NUM,6,')',1,ICHR,*900)
      CALL DANVER_NEW(35,KARD(NOW:KPT),0,IERR1,*990)

!---- Errors
  900 CALL ERR_MES(1,36)
      CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)
      GOTO 990

  980 CALL FLAGIT(1,ARROW,NOW+11,1,IERRU)         ! Set error markers
      CALL ERR_FIELD(1,IFIELD,14)                   ! Error message

  990 IF(IERRF.NE.0) CALL ERR_MES(1,36)             ! Error message
      IF(IERRU.NE.0 .OR. IERRF.NE.0) IERR = 1

      RETURN
      END
      SUBROUTINE PART_CHECK(KARD,NOW)

!*  Checks BIB keyword PART-DET 
!*    KARD: input array
!*    NOW:  position in input array

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*55 KARD
      CHARACTER LIMTR(2)/',', ')' /

      IERRM = 0                                 ! Initialize internal error flag

  100 CALL PARSCHR(KARD,NOW,KPT,NUM,15,LIMTR,2,ICHR,*700) ! Find code field

      IF (NUM.EQ.0) THEN                        ! Empty field
        CALL ERR_MES(1,34)
        CALL FLAGIT(1,ARROW,NOW+11,1,IERR)
        GOTO 400
      END IF

      CALL DANVER_NEW(33,KARD(NOW:KPT),0,IERRU,*400)    ! Look up particle code

!---- CODE NOT IN DICTIONARY
      CALL NUCFLD(6,KARD(NOW:KPT),NUM,IERRU,INE,MULT)   ! Check for Z-S-A
      IF(IERRU.NE.0) CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR) 
                                                        ! Set error markers
  400 NOW = KPT+2
      IF(ICHR.EQ.1) GOTO  100                           ! Branch for next code
      GOTO 800

!---- END OF CODE STRING NOT FOUND.
  700 CALL ERR_MES(1,36)
      CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)
      NOW = KPT+2

  800 IF (IERRM.NE.0) THEN                      ! Internal error flag set
        CALL ERR_MES(1,35)                        ! Error message 
        IERR = 1                                  ! Set error flag
      END IF
      RETURN
      END
      SUBROUTINE QUANT_CHECK(IWAY,IFIELD,NOW,LST,NUM,RUNIT,IERRU,*)

!* Checks quantity part of REACTION string. 
!*   IWAY:   1  =  REACTION
!*           2  =  MONITOR
!*           3  =  ASSUMED
!*           4  =  INC-SOURCE
!*   IFIELD: Current field
!*   NOW:    Starting position in array for scan.
!*   LST:    Last position in array to be scanned
!*   NUM:    Number of characters in mdified quantity
!*   RUNIT:  Data units expected from REACTION
!*   IERRU:  Error flag
!*   RETURN 1: Reaction not found in dictionary 36

!---- Input record. (Set in READ_REC)
      COMMON/ALLCOM/KEY2,IPTR,KARD,ISEQ
        CHARACTER*10 KEY2
        CHARACTER*1  IPTR
        CHARACTER*55 KARD
        CHARACTER*13 ISEQ

!---- Current AN, SAN, section. (Set in READ_REC)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

!---- Independent variable special treatment flags
      COMMON/EN_PROC/SPECIN(4)
        INTEGER SPECIN

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

!---- Independent variable families required. (Initialized in BIB_SUB)
      COMMON/FAMLY_REQ/NEEDS(7),NEEDFL,NOTCK

!---- Flags which link reaction to quantity (Set in PARSER)
      COMMON/REACT_FLAGS/IHEAVY,INEOUT,SUBF4,NUCQ
        INTEGER SUBF4

      CHARACTER*80 DICLINE
      CHARACTER*30 KEY
      CHARACTER*4 RTYPE,RUNIT
      CHARACTER*3 ISTAT
      CHARACTER RESFLG

!!!!  Initialize
      LOWM = 0                               ! start of modifier field
      NR = 0                                 ! # of right parens
      RESFLG = ' '                           ! resonance quantity flag

!---- Scan to end of SF8 or REACTION string.

  120 DO 150 I = NOW,LST
        KPT=I
        IF (KARD(I:I).EQ.',') THEN           ! If end of subfield
          ICHR = 1
          CALL INCRMT(IFIELD,8,*170)           ! Increment field #
          IF (IFIELD.EQ.8) THEN                ! Modifier field
            IF (KARD(I+1:I+1).EQ.')') THEN       ! Training commas
              CALL ERR_MES(1,67)
              CALL FLAGIT(1,ARROW,I+11,1,IERRU)    
              GOTO 200
            END IF
            IF(KARD(I+1:I+1).EQ.',') GOTO 180    ! Check for code in SF8
            LOWM = KPT-NOW+2                     ! Store starting position
          END IF
        ELSE IF (KARD(I:I).EQ.'(') THEN      ! If left parenthesis
          NR = NR-1                            ! Decrement # of right parens
        ELSE IF (KARD(I:I).EQ.')') THEN      ! If right parenthesis
          NR = NR+1                            ! Increment # of right parens
          ICHR = 2                             ! Set right paren flag
          IF(NR.GT.0) GOTO 170                 ! Test for end of string
        END IF
  150 CONTINUE
      GOTO 180

  170 KPT = KPT-1
  180 IF(KPT.EQ.NOW) THEN
        IF(IWAY.EQ.1) NOTCK = 1            ! For REACTION, set to not check data
        CALL ERR_MES(1,50)
        CALL FLAGIT(1,ARROW,NOW,1,IERRU)
        RETURN
      END IF

!---- Delete any general quantity modifiers in SF8. Check for AV & REL.

  200 NUM = KPT-NOW+1

      IF (NUM.GT.40) THEN                      ! Max. number of characters = 40
        CALL ERR_MES(1,47)
        CALL FLAGIT(1,ARROW,NOW+31,(NUM-40),IERRU)
        RETURN 1
      END IF

      KEY = ' '
      KEY(1:NUM) = KARD(NOW:KPT)
      CALL GENQM(KEY,LOWM,NUM,MODAV,MODREL)

      IF (NUM.GT.30) THEN                    ! Max. number of characters = 30
        CALL ERR_MES(1,47)
        CALL FLAGIT(1,ARROW,NOW+31,(NUM-30),IERRU)
        RETURN 1
      END IF

c-zvv-      CALL DANGET_STA_NEW(36,KEY(1:30),0,0,DICLINE,ISTAT,IERR1) 
      CALL DANGET_STA_NEW(236,KEY(1:30),0,0,DICLINE,ISTAT,IERR1) 
                                             ! Look up subfields 5-8

      IF (IERR1.NE.0) THEN                   ! If illegal code
        RUNIT = 'MIS '                         ! Expected data units missing
        NOTCK = 1                              ! Indep. var. not to be checked
        IF (SUBF4.GT.11) THEN                  ! If SF4 missing
          CALL ERR_FIELD(1,4,15)                 ! Error message
          CALL FLAGIT(1,ARROW,SUBF4,1,IERRU)    ! Set error markers
        END IF
        RETURN 1 
      END IF

      IF (ISTAT.EQ.'OBS') THEN               ! If obsolete quantity code
        CALL ERR_MES(1,31)
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRU)
      END IF

      IF (MODREL.GT.0) THEN                 ! If relative data
        RUNIT = 'NO  '                        ! Store expected unit family
        NEEDFL = INTSAN                       ! No MONITOR needed
      ELSE IF (MODREL.LT.0) THEN            ! If nonspecific REACTION
        RUNIT = 'MIS '                        ! Skip unit check
        IF(MODREL.EQ.-3) NEEDFL = INTSAN      ! RAW data, no MONITOR needed 
        CALL ERR_WARN(1,14)                   ! Warning message 
        IERRU = 1                             ! Set error flag
      ELSE                                  ! Otherwise
c-zvv-        RUNIT(1:4) = DICLINE(29:32)           ! Set units from DICT.36
c-zvv-        RESFLG = DICLINE(33:33)
        RUNIT(1:4) = DICLINE(29-24:32-24)           ! Set units from DICT.36
        RESFLG = DICLINE(33-24:33-24)
      END IF

c-zvv-      RTYPE(1:4) = DICLINE(25:28)           ! Store quantity type
      RTYPE(1:4) = DICLINE(25-24:28-24)           ! Store quantity type
      IF(RTYPE.EQ.' ') GOTO 330

      IF (IWAY.EQ.1) THEN                   ! REACTION
        CALL FAMGET(IWAY,RTYPE,NEEDS)         ! Store families expected
        IF (NEEDS(1).NE.0) RESFLG = '.'       ! Resonance parameter
      END IF

      IF (RESFLG.EQ.'.') THEN               ! Resonance parameter
        IF (INEOUT.NE.0) THEN                 ! If SF4 not blank
          CALL ERR_FIELD(1,4,14)                ! Error message 
          CALL FLAGIT(1,ARROW,NOW+10,1,IERRU)   ! Set error markers
        END IF
        IF (IWAY.EQ.1) THEN                   ! If REACTION being processed
          NEEDFL = INTSAN                       ! Set MONITOR not needed
          IF (RTYPE.EQ.'RE  ') THEN             ! Resonance energy
            NEEDS(1) = -INTSAN                    ! Store -ANSAN
          ELSE IF (RTYPE.EQ.'RP A' .OR. MODAV.NE.0) THEN ! Avg.res.par.
            SPECIN(2) = INTSAN                    ! Set flag to ANSAN
          ELSE IF (RTYPE.EQ.'RPP ') THEN        ! Partial res.par.
            NEEDS(3) = INTSAN                     ! Sec. en. needed
          END IF
        END IF

      ELSE                                   ! If not resonance parameter
        IF (RESFLG.EQ.'.') THEN                ! Quantity at resonance
          NEEDS(1) = NEEDS(2)                   ! Set res. en. needed
          NEEDS(2) = 0                          ! Cancel energy needed
        END IF
        IF (RTYPE.EQ.'P * ') THEN              ! Partial yield for spec.quantity
          SPECIN(4) = INTSAN                     ! Set flag to ANSAN
        END IF
        IF (SUBF4.GT.11) THEN                  ! If SF4 missing
          CALL ERR_FIELD(1,4,15)                 ! Error message 
          CALL FLAGIT(1,ARROW,SUBF4,1,IERRU)     ! Set error markers
        END IF
        IF (IWAY.EQ.1) THEN                    ! If REACTION being processed
          IF(NEEDS(2).EQ.0 .AND. NUCQ.EQ.0) NEEDS(2) = INTSAN ! Energy required
        END IF
      END IF

  330 IF (IHEAVY.GT.0) THEN                  ! If heaviest particle not in SF4
        DO 340 I = NOW,53                      ! Check for SEQ in SF5
          IF(KARD(I:I).EQ.',' .OR. KARD(I:I).EQ.')') GOTO 380
          IF(KARD(I:I+2).EQ.'SEQ') GOTO 390
  340   CONTINUE

!----   'SEQ' NOT FOUND, ERROR.
  380   NUM1 = (NOW+11)-IHEAVY                   ! # of characters to mark
        CALL ERR_MES(1,63)
        CALL FLAGIT(1,ARROW,IHEAVY,NUM1,IERRU)
      END IF

  390 NOW = KPT+2
      RETURN 
      END
      SUBROUTINE REACTION(IWAY,NOW,PTR,*)

!* Checks code for the keywords REACTION, MONITOR and ASSUMED.
!*   IWAY: keyword to be checked
!*         1  =  REACTION
!*         2  =  MONITOR
!*         3  =  ASSUMED
!*   NOW:  position in input array
!*   PTR:  pointer for current code
!*   RETURN 1: next record read

!---- Input record. (Set in READ_REC)
      COMMON/ALLCOM/KEY2,IPTR,KARD,ISEQ
        CHARACTER*10 KEY2
        CHARACTER*1  IPTR
        CHARACTER*55 KARD
        CHARACTER*13 ISEQ

!---- Current AN, SAN, section. (Set in READ_REC)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

!---- Independent variable special treatment flags
      COMMON/EN_PROC/SPECIN(4)
        INTEGER SPECIN

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

!---- Independent variable families required. (Initialized in BIB_SUB)
      COMMON/FAMLY_REQ/NEEDS(7),NEEDFL,NOTCK

!---- Last record. (Set in READ_REC, READ_CONT)
      COMMON/LASTRC/LKEY,LPTR,LFLD,LSEQ
        CHARACTER*55 LFLD
        CHARACTER*13 LSEQ
        CHARACTER*10 LKEY
        CHARACTER LPTR

!---- Number of monitors (Initialized in BIB_SUB).
      COMMON/MON_HEAD/NMON1,NMON

!---- Stored pointers for REACTION (Initialized in BIB_SUB)
      COMMON/REAC_PTR/NPR,NPR1,RACPTR(20),RUNIT(20)
        CHARACTER RACPTR
        CHARACTER*4 RUNIT

      CHARACTER*10 PMESS/'POINTER:  '/
      CHARACTER*20 MESS2/'   left,    right  '/
      CHARACTER*10 LEGHED
      CHARACTER*4 RUNIT1/'    '/
      CHARACTER PTR
      CHARACTER SEPART(6)/'+', '-', '*', '/', '=', ','/

!!!!  Initialize
      JSEP = 0                           ! separator to not ratio
      IRIGHT = 0                         ! # of right parens
      ILEFT = 1                          ! # of left parens
      NREACT = 1                         ! # of REACTION strings

      IF(KARD(NOW:NOW).EQ.' ') CALL LEAD_BLANK(KARD,NOW,*910) 
                                               ! Flag leading blanks
!!!!  REACTION

      IF (IWAY.EQ.1) THEN        
        IF (NPR.NE.0) THEN                     ! More than 1 pointer
          IF (PTR.EQ.' ') THEN                   ! Blank pointer, error
            CALL ERR_MES(1,57)
            CALL FLAGIT(1,ARROW,11,1,IERR)
            GOTO 100
          ELSE                                 ! Nonblank, check for duplicates
            J = LOOKUP(PTR,RACPTR,NPR)
            IF (J.NE.0) THEN                     ! Duplicate
              CALL ERR_MES(1,59)
              CALL FLAGIT(1,ARROW,10,1,IERR)
              GOTO 100
            END IF
          END IF
        END IF
        CALL ADD_LIST(RACPTR,20,NPR,NPR1,PTR,'RACPTR    ',*100)
                                               ! Add pointer to stored pointers
        RUNIT(NPR) = '   '                     ! Initialize units expected

!!!!  MONITOR

      ELSE IF (IWAY.EQ.2) THEN         
        NMON = NMON+1                      
        IF(INTSAN.EQ.1) NMON1 = NMON1+1

        IF(KARD(NOW:NOW).EQ.'(') THEN          ! Paren found
          NOW = NOW+1
          IF (KARD(NOW:NOW).GE.'0' .AND. KARD(NOW:NOW).LE.'9') THEN ! Target
            ILEFT = ILEFT+1
          ELSE                                 ! Heading
            LEGHED ='MONIT'
            CALL TEST_HEAD(2,KARD,NOW,LEGHED,')',PTR,*70)
            IF (KARD(NOW:NOW).EQ.',') THEN       ! illegal following comma
              CALL ERR_MES(1,43)                   ! write error message   
              CALL FLAGIT(1,ARROW,NOW+11,1,IERR)   ! flag error
              NOW = NOW+1
            END IF
            GOTO 100

!----       Can't identify code
   70       CALL ERR_MES(1,35)                   ! write error message   
            N = NOW-1-LOW
            CALL FLAGIT(1,ARROW,LOW,N,IERR)      ! flag error
          END IF

        ELSE IF(KARD(NOW:NOW).GE.'0' .AND. KARD(NOW:NOW).LE.'9') THEN
          GOTO 100                             ! Assume no heading field

        ELSE                                   ! Assume missing parens.
          CALL FLAGIT(1,ARROW,NOW+11,1,IERR)   
          LOW = NOW
          LEGHED ='MONIT'
          CALL TEST_HEAD(2,KARD,NOW,LEGHED,',',PTR,*100) ! CHECK HEADING FIELD
          CALL ERR_MES(1,47)                   ! ERROR MESSAGE 
          CALL FLAGIT(1,ARROW,NOW+10,1,IERR)   ! ERROR MESSAGE 
        END IF
    
!!!!  ASSUMED

      ELSE                            
        IF (KARD(NOW:NOW).GE.'0' .AND. KARD(NOW:NOW).LE.'9') THEN ! No heading
          CALL ERR_MES(1,38)
          CALL FLAGIT(1,ARROW,NOW+11,1,IERR)
          GOTO 110
        END IF
        LEGHED ='ASSUM'
        IF (KARD(NOW:NOW) .EQ.'(') THEN        ! Illegal () around eading
          CALL ERR_MES(1,44)
          CALL FLAGIT(1,ARROW,NOW+11,1,IERR)
          NOW = NOW+1
          CALL TEST_HEAD(3,KARD,NOW,LEGHED,')',PTR,*100) ! Check legal heading
          CALL FLAGIT(1,ARROW,NOW+10,1,IERR)
        ELSE                                   ! Legal heading field
          CALL TEST_HEAD(3,KARD,NOW,LEGHED,',',PTR,*100) ! Check legal heading
        END IF       
      END IF

  100 LOW = NOW
      DO WHILE (KARD(NOW:NOW).EQ.'(')          ! Read past left parenthesis
        ILEFT = ILEFT+1
        CALL INCRMT(NOW,55,*830)
      END DO

      IF(KARD(NOW:NOW).EQ.' ') CALL LEAD_BLANK(KARD,NOW,*910) 
                                               ! Flag leading blanks
  110 LOW = NOW
      IR2 = 0
      DO 200 I = LOW,55                        ! Store next reaction string
        NOW = I
        IF(KARD(NOW:NOW).EQ.'(') IR2 = IR2-1   ! Count left parens
        IF(KARD(NOW:NOW).EQ.')') IR2 = IR2+1   ! Count right parens
        IF(IR2.GT.0) GOTO  250                 ! Check for end of string
  200 CONTINUE
      GOTO 830
  250 CALL INCRMT(IRIGHT,ILEFT,*810)

      CALL PARSER(IWAY,LOW,NOW,RUNIT1)         ! Check REACTION string

!---- For REACTION, checked expected units
      IF (IWAY.EQ.1) THEN                      ! REACTION
        IF (NREACT.EQ.1) THEN                    ! 1st string
          RUNIT(NPR) = RUNIT1                      ! Set to current
        ELSE IF (JSEP.NE.4) THEN                 ! Not 1st string or ratio
          IF (RUNIT(NPR).NE.'MIS ' .AND. RUNIT1.NE.RUNIT(NPR)) THEN
            CALL ERR_WARN(2,8)                       ! Warning message
            CALL FLAGIT(0,ARROW,0,0,IERR)
          END IF
        END IF
      END IF

!---- Read past right parens
  290 IF(IRIGHT.EQ.ILEFT) GOTO 950             ! Branch for end of string
      CALL INCRMT(NOW,55,*810)
      IF(KARD(NOW:NOW).NE.')') GOTO 300
      IRIGHT = IRIGHT+1
      GOTO 290

!---- Test for valid separator on next character.
  300 J = LOOKUP(KARD(NOW:NOW),SEPART,6)

      IF (J.EQ.0) THEN                         ! Not valid separator
        IF (KARD(NOW:NOW).EQ.' ') THEN
          GOTO 810
        ELSE
          GOTO 850
        END IF
      ELSE IF (J.EQ.4) THEN                    ! Ratio
        IF (IWAY.EQ.1) THEN                      ! REACTION
          IF (JSEP.EQ.0) THEN                      ! 1st separator
            RUNIT(NPR) = 'NO  '                      ! Reset data units expected
            NEEDFL = INTSAN                          ! Set MONITOR not needed
          ELSE IF (JSEP.NE.4) THEN                 ! Complex reaction
            IF (RUNIT(NPR).NE.'MIS ') THEN           ! Message not written
              RUNIT(NPR) = 'MIS '                      ! Set to skip unit check
              PMESS(9:9) = PTR
              CALL ERR_INSERT(1,29,10,PMESS)           ! Error message
            END IF
          END IF
        END IF
        IF (KARD(NOW+1:NOW+1).EQ.'/') THEN       ! Special ratios '//'
          NOW = NOW+1                              ! SKIP PAST
          SPECIN(1) = INTSAN                       ! Set numerator/denom flag
        END IF
      END IF
      JSEP = J                                 ! Store separator

      CALL INCRMT(NOW,55,*500)
      IF(KARD(NOW:NOW).EQ.'(') GOTO 580        ! Branch if new code string

      IF(KARD(NOW:55).NE.' ') GOTO 850   ! Remainder of record not blank, branch

!---- Read new record, test for continuation.
  500 CALL READ_CONT(1,NOW,*780)         
      IF (IPTR.NE.' ') THEN                    ! New REACTION pointer
        LOW = 0        
        GOTO 781                
      END IF
      IF(KARD(1:1).NE.'(') GOTO 780            ! No code string on record.

  580 CALL INCRMT(NREACT,50,*600)              ! Increment # of reaction strings
      GOTO 100

!---- Too many REACTION strings
  600 CALL ERR_MES(2,60)                       ! Error message 
      GOTO 100

!---- Continuation of REACTION string not found.
  780 LOW = NOW
  781 WRITE(MESS2(1:2),1000) ILEFT
      WRITE(MESS2(10:11),1000) IRIGHT
      CALL ERR_INSERT(1,12,20,MESS2)           ! Error message 
      CALL FLAGIT(1,ARROW,LOW+11,1,IERR)       ! Set error marker
      NOTCK = 1                                ! Set to skip ind.variable check
      RETURN 1

!---- No left parenthesis. Continuation of REACTION string not found.
  810 WRITE(MESS2(1:2),1000) ILEFT
      WRITE(MESS2(10:11),1000) IRIGHT
      CALL ERR_INSERT(1,12,20,MESS2)           ! Error message 
      CALL FLAGIT(1,ARROW,NOW+11,1,IERR)       ! Set error markers
      RETURN

!---- Illegal REACTION string
  830 CALL ERR_MES(1,35)
      CALL FLAGIT(1,ARROW,LOW+11,(55-LOW),IERR)
      RETURN

  850 CALL ERR_MES(1,36)
      CALL FLAGIT(1,ARROW,NOW+11,(55-NOW),IERR)
  910 RETURN 

!---- End of string
  950 CALL INCRMT(NOW,55,*910)
      J = LOOKUP(KARD(NOW:NOW),SEPART,6)         ! Look up next character
      IF (J.NE.0 .OR. KARD(NOW:NOW).EQ.')') THEN ! Next character is separator
        CALL ERR_WARN(1,7)                         ! Warning message 
        CALL FLAGIT(0,ARROW,NOW+11,1,IERR)
      END IF
      RETURN
 1000 FORMAT(I2)
      END
      SUBROUTINE READ_CONT(IWAY,NOW,*)

!* Reads record. Checks for continuation record. Writes error messages for 
!* previous record.
!*   IWAY: 1 = do not check for parenthesis in column 12 of new record
!*         2 = check for parenthesis in column 12 of new record
!*   NOW:  current location in record array.
!*   RETURN 1: continuation not found

!---- Input record. (Set in READ_REC)
      COMMON/ALLCOM/KEY2,IPTR,KARD,ISEQ
        CHARACTER*10 KEY2
        CHARACTER*1  IPTR
        CHARACTER*55 KARD
        CHARACTER*13 ISEQ

!---- Last record. (Set in READ_REC, READ_CONT)
      COMMON/LASTRC/KEYSAV,IPTRS,KARDS,ISEQS
        CHARACTER IPTRS
        CHARACTER*13 ISEQS
        CHARACTER*10 KEYSAV
        CHARACTER*55 KARDS

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*69 INREC,STRREC

      EQUIVALENCE (INREC(1:1),IPTR),(INREC(2:56),KARD(1:55)),
     * (INREC(57:69),ISEQ),
     * (STRREC(1:1),IPTRS),(STRREC(2:56),KARDS(1:55)),
     * (STRREC(57:69),ISEQS)

!---- Store current record.
      KEYSAV = KEY2
      STRREC = INREC 
      IERRL = IERR

      CALL READ_REC(ID)          ! Read next record
      IF(KEY2.NE.' ') GOTO 500   ! Record continuation (blank KEYWORD) not found
      IF(IWAY.EQ.2 .AND. KARD(1:1).EQ.'(') GOTO 500 
                                 ! Code continuation not found

!---- Continuation record found. 
      IF(IERRL.NE.0) CALL ERR_ARW2(KEYSAV,IPTRS,KARDS,ISEQS,ARROW,IERRL)
                                 ! Write error flags for last record
      IERR = 0                   ! Turn off error flag
      NOW = 1                    ! Initialize position in string
      RETURN

!---- Continuation not found.
  500 CALL ERR_MES(1,13)
      CALL FLAGIT(1,ARROW,NOW+11,1,IERR)                 ! Set error flags
      CALL ERR_ARW2(KEYSAV,IPTRS,KARDS,ISEQS,ARROW,IERR) ! Write error message
      RETURN 1

      END
      SUBROUTINE READ_REC(ID)      
!* Reads the input file
!*   ID:   System Identifier equivalent

!---- Input record. (Set in READ_NEXT)
!-      KEY:  columns 1-10
!-      IPTR: column 11
!-      KARD: columns 12-66
!----   ISEQ: sequence # (columns 75-79)
      COMMON/ALLCOM/KEY,IPTR,KARD,ISEQ
        CHARACTER*10 KEY
        CHARACTER*1  IPTR
        CHARACTER*55 KARD
        CHARACTER*13 ISEQ

!---- Current AN, SAN, section. (Set in READ_NEXT)
!-      ACNUM:  character accession #
!----   INTSAN: subaccession #
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

!---- Total counts for records, entries, subentries. 
!-    (Initialized in PASS!:ALTER_FLAG)
!-      NTOTRC: total # of records (Set in READ_REC)
!-      NENT:   # of entries (Set in AN_GET)
!-      NTOTSB: total # of subentries (Set in AN_GET)
!----   NEWSUB: total # of new data subentries (Set in PASS1:ALTER_FLAG)
      COMMON/COUNTS/NTOTRC,NENT,NTOTSB,NEWSUB

!---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IODUM(2)

      CHARACTER*79 NOWREC

      EQUIVALENCE (NOWREC(1:10),KEY(1:10)),(NOWREC(11:11),IPTR),
     * (NOWREC(12:66),KARD(1:55)),(NOWREC(67:79),ISEQ(1:13))

      READ(IN,8000,END=900) NOWREC         ! Read next record
      NTOTRC = NTOTRC+1                    ! Increment count

      CALL SYS_CHECK(KEY,ID,IDLST)         ! Check if SYSTEM IDENTIFIER

!---- FOR DATA, IF LAST WAS DATA, RESET ID TO 0.
      IF (ID.EQ.15 .AND. IDLST.EQ.15) ID = 0

      IF (ID.GT.17) ID = MOD(ID,2)+1       ! For file labels, reset ID #

      IF(ID.EQ.3) THEN                     ! ENTRY
        CALL AN_GET(ACNUM,INTSAN)            ! Check accession #
        NENT = NENT+1                        ! Increment ENTRY count

      ELSE IF(ID.EQ.6.OR.ID.EQ.7) THEN     ! SUBENT, NOSUBENT
          CALL SAN_GET(ACNUM,INTSAN)       ! Check subaccession # (ENTRY AN_GET)
          NTOTSB = NTOTSB+1                  ! Increment SUBENT count
      END IF
      RETURN

c 900 CALL EXIT
  900 STOP

 8000 FORMAT(A79)
 9000 FORMAT(' **** End-of-file read out of order ***'/
     *       '      Execution terminated')
      END
      SUBROUTINE REF_CHECK(KARD,NOW)

!* Checks reference code for keywords REFERENCE, REL-REF, and MONIT-REF
!*   KARD: input character string
!*   NOW:  current position in input string

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*55 KARD
      CHARACTER REFTYP(8)/'B','C','J','P','R','S','T','W'/
      CHARACTER LIMTR(3)/ '(', ',', ')'/

!!!!  Initialize
      IERRP = 0       ! error flag off
      NLEFT = 1       ! # of left parens

!---- Check for equivalent references (extra paren).
      IF (KARD(NOW:NOW).EQ.'(') THEN
        NOW  = NOW+1
        NLEFT = NLEFT+1
      END IF

  150 KREF = LOOKUP(KARD(NOW:NOW),REFTYP,8) ! Look up reference type

      IF (KREF.EQ.0 .OR. KARD(NOW+1:NOW+1).NE.',') THEN ! No or illegal code
        CALL ERR_MES(1,35)
        CALL FLAGIT(1,ARROW,NOW+11,1,IERR)
        RETURN                              ! Abort checking remainder of string
      END IF

      NOW = NOW+2

      CALL REF_DATE(KARD,NOW,NFLD,LST,IEND) ! Check date

      IF (NOW.GE.LST) THEN                  ! No other fields present
        CALL ERR_MES(1,38)
        CALL FLAGIT(1,ARROW,NOW+11,1,IERR)  
        GOTO 805
      END IF

      GOTO (200,200,300,400,400,400,500,500) KREF  ! Branch to process

!!!!  Reference type B or C  (required fields - code, date)

  200 continue
      if (KREF.EQ.1) CALL REF_CODE(KARD,NOW,207,10)   ! Check code: B
      if (KREF.NE.1) CALL REF_CODE(KARD,NOW,7,10)     ! Check code: C

!---- Check volume
      IF(NOW.EQ.LST) GOTO 805
      CALL PARSCHR(KARD,NOW,KPT,NUM,20,LIMTR(2),1,ICHR,*700)
      NOW = KPT+2
      GOTO (800,250,240) NFLD-1             ! Branch according to # of fields

  240 IF(KARD(NOW:NOW).EQ.'(') CALL REF_PART(KARD,NOW,IERRP,*245) ! Check part
  245 NOW  = NOW+1

  250 IF(NOW.GE.LST) GOTO 805
      CALL REF_PAGE(KARD,NOW,IERRP)         ! Check page
      GOTO 800

!!!!  Reference type J (required fields - code, volume, page, date)

  300 IF (KARD(NOW:NOW).EQ.',') THEN        ! Journal code missing
        CALL ERR_MES(1,38)
        CALL FLAGIT(1,ARROW,NOW+11,1,IERR)    ! Set error markers
        NOW = NOW+1
      ELSE                                  ! Check journal code
        CALL REF_CODE(KARD,NOW,5,6)
      END IF
      IF(NOW.GE.LST) GOTO 700

!---- Check volume
      CALL PARSCHR(KARD,NOW,KPT,NUM,20,LIMTR,3,ICHR,*700)
      IF (ICHR.NE.2) THEN                   ! Illegal parenthesis in field
        NOW1 = KPT
        CALL PARSCHR(KARD,NOW1,KPT,NUM,20,LIMTR(2),1,ICHR,*700)
                                            ! Look for comma
        IF(NUM.EQ.0) NUM = 1                ! Flag next character if field empty
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRP) ! Set error markers
      END IF

      NOW = KPT+2
      IF (KARD(NOW:NOW).EQ.'(') THEN        ! Left paren in issue field
        CALL REF_PART(KARD,NOW,IERRP,*335)    ! Check issue #
  335   NOW = NOW+1
      ELSE IF (NFLD.GE.4) THEN              ! Issue field missing parens.
        CALL PARSCHR(KARD,NOW,KPT,NUM,20,LIMTR(2),2,ICHR,*700)
        IF(NUM.EQ.0) NUM = 1                  ! Flag next char. if field empty
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRP) ! Set error markers
        NOW = KPT+2
      END IF

      IF(NOW.GE.LST) GOTO 700
      CALL REF_PAGE(KARD,NOW,IERRP)         ! Check page
      GOTO 800

!!!!  Reference type P, R or S (required fields - code, date)

  400 CALL REF_CODE(KARD,NOW,6,20)          ! Check report code and report #.
      IF(NOW.GE.LST) GOTO 805
      IF(NFLD.LT.3) GOTO 440

!---- Check for presence of left paren in volume
      IF (KARD(NOW:NOW).EQ.'(') THEN        ! Legal volume field
        CALL REF_PART(KARD,NOW,IERRP,*700)    ! Check volume field
        NOW = NOW+1
      ELSE                                  ! Volume field missing parens.
        CALL PARSCHR(KARD,NOW,KPT,NUM,20,LIMTR(2),2,ICHR,*700)
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRP) ! Set error markers
        NOW = KPT+2
      END IF
      IF(NOW.GE.LST) GOTO 805

  440 CALL REF_PAGE(KARD,NOW,IERRP)         ! Check page
      GOTO 800

!!!!  Reference type T or W (required fields - code, date)

  500 CALL REF_CODE(KARD,NOW,0,30)          ! Check author
      IF(NOW.GE.LST) GOTO 805

      CALL REF_PAGE(KARD,NOW,IERRP)         ! Check page
      GOTO 800

!!!!  End of reference.

!---- Set error flags.
  700 CALL FLAGIT(1,ARROW,NOW+11,1,IERRP)   ! Set error markers
      IF(IEND.EQ.0) GOTO 900

!---- Check if all fields checked
  800 IF (NOW.LT.LST) THEN
        NUM = LST-NOW
        CALL FLAGIT(1,ARROW,NOW+12,NUM,IERRP) ! Set error markers
      END IF

!---- If equivalent reference expected, check for  = .
  805 IF(NLEFT.LE.1) GOTO 995
      NOW = IEND+1

      IF (KARD(NOW:NOW).EQ.'=') THEN        ! Equivalent reference
        CALL INCRMT(NOW,55,*830)
        IF(KARD(NOW:NOW).NE.'(') GOTO 820     ! Next code not on line
        NOW = NOW+1
        GOTO 150
      ELSE IF(KARD(NOW:NOW).NE.')') THEN
        CALL FLAGIT(1,ARROW,NOW+11,1,IERRP)   ! Set error markers
      END IF
      GOTO 995

!---- Next code not on line.
  820 IF (KARD(NOW:55).NE.' ') THEN         ! Remainder of field not blank.
        NUM = 55-NOW+1
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRP) ! Set error markers
      END IF

!---- End of string not found on record
  830 IF (IERRP.NE.0) THEN                  ! Error set
        CALL ERR_MES(1,36)                    ! Error message 
        IERR = 1
      END IF
      CALL READ_CONT(1,NOW,*998)            ! Look for continuation record

!---- Continuation found.
      IF (KARD(1:1).NE.'(') THEN            ! Code not on continuation
        CALL FLAGIT(1,ARROW,NOW+11,1,IERRP)   ! Set error markers
      ELSE                                  ! Code on line
        NOW = NOW+1
        GOTO 150
      END IF

!---- End of code string not found
  900 CALL ERR_MES(1,36)                    ! Error message 

  995 IF (IERRP.NE.0) THEN                  ! Error set
        CALL ERR_MES(1,43)                    ! Error message 
        IERR = 1
      END IF

  998 RETURN  
      END
      SUBROUTINE REF_CODE(KARD,NOW,KDIC,MAX)

!* Stores report code and looks up in dictionary
!*   KARD: input array
!*   NOW:  starting position of code in array
!*   KDIC: dictionary number of code
!*   MAX:  maximum # of characters in code

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*55 KARD
      CHARACTER*3 STATD
      CHARACTER LIMTR(2)/',', ')'/

      IF(KARD(NOW:NOW).EQ.' ') CALL LEAD_BLANK(KARD,NOW,*900)
                                            ! Flag leading blanks

      IF(KDIC.NE.6) GOTO 200                ! Not report

!!!!  REPORTS, Initialize

      N = 1                                 ! # of illegal characters
      NUM = 0                               ! # of characters read
      NL = 0                                ! # of left parens
      NR = 0                                ! # of right parens
      IFIELD = 1                            ! Field to report code

!---- Read until dash, comma or right paren found.
      DO 180 I = NOW,55
        IF(KARD(I:I).EQ.',') THEN             ! End of report code    
          LST = I-1
          GOTO (185,190) IFIELD
        ELSE IF(KARD(I:I).EQ.'(') THEN        ! left paren
          NL = NL+1
        ELSE IF(KARD(I:I).EQ.')') THEN        ! right paren
          IF(NL.EQ.NR) GOTO 185
          NR = NR+1
        END IF 
        IF(IFIELD.EQ.2) GOTO 180      

        CALL INCRMT(NUM,55,*185)              ! increment position
        KPT = I

!----   Check for end of field - first hyphen followed by a digit or
!         opening paren
        IF(KARD(I:I).NE.'-') GOTO 180
        N = NUM                               ! Store # chars up to hyphen
        IF(KARD(I+1:I+1).GE.'0' .AND. KARD(I+1:I+1).LE.'9'
     *      .OR. KARD(I+1:I+1).EQ.'(') THEN
          IF(N.GT.11) N = 11
          IFIELD = 2                            ! Set field to report #
        END IF
  180 CONTINUE

!---- End of code not found
  185 CALL ERR_MES(1,36)
      CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)
      GOTO 195

!---- End of code found
  190 IF (NUM.GT.MAX) THEN                  ! code too long
        CALL ERR_MES(1,37)
        NUM = MAX-NUM+1
        CALL FLAGIT(1,ARROW,NOW+MAX+11,NUM,IERR)
      ELSE
        GOTO 300
      END IF

  195 NOW = LST+2
      RETURN

!!!!  BOOKS, CONFERENCES, JOURNALS, PRIVATE COMM., THESES

!---- Store unitl a comma or rparen is found.
  200 CALL PARSCHR(KARD,NOW,KPT,N,MAX,LIMTR,2,ICHR,*500)
      LST = KPT
      IF(KDIC.EQ.0) GOTO 400

!!!!  BOOKS, CONFERENCES, JOURNALS, REPORTS

  300 CALL DANVER_STA_NEW(KDIC,KARD(NOW:KPT),0,STATD,NONO,*390) ! Look up code
      GOTO 500

  390 IF (STATD.EQ.'OBS') THEN
        CALL ERR_MES(1,31)
        CALL FLAGIT(1,ARROW,NOW+11,N,IERR)
      ELSE IF (STATD.EQ.'EXT') THEN
        CALL ERR_WARN(1,1)                  ! Error message 
        CALL FLAGIT(0,ARROW,NOW+11,N,IERR)
      END IF
      NOW = LST+2
      RETURN

!!!!  PRIVATE COMMUNICATIONS, THESES

  400 CALL CHAR_NAME(3,KARD,NOW,KPT-1)      ! Check author's family name
      NOW = KPT+2
      RETURN

  500 CALL ERR_MES(1,35)
      CALL FLAGIT(1,ARROW,NOW+11,N,IERR)
      NOW = LST+2
  900 RETURN
      END
      SUBROUTINE REF_DATE(KARD,NOW,NFLD,LST,IEND)

!* Stores reference date and checks.
!*   KARD:  input array
!*   NOW:   starting position of code in array
!*   NFLD:  # of fields skipped
!*   LST:   position of last comma
!*   IEND:  end of code string flag

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*55 KARD
      CHARACTER*8 KPTDAT

!!!!  Initialize
      IEND = 0                              ! end of code string flag off
      IL = 0                                ! # of left parens
      IR = 0                                ! # of right parens
      NFLD = 0                              ! # of fields in reference

      DO 150 I = NOW,55                     ! Look for end of code string
        IF (KARD(I:I).EQ.'(') THEN            ! Increment # left parens
          IL = IL+1        ! Increment left parens
        ELSE IF (KARD(I:I).EQ.',') THEN
          LC = I                                ! Store last comma position
        ELSE IF (KARD(I:I).EQ.')') THEN
          IF(IL.EQ.IR) GOTO 200                 ! Branch if end of string
          IR = IR+1                             ! Increment # right parens
        END IF
        KPT = I
        IF(KARD(I:I).EQ.',') NFLD = NFLD+1 ! Increment # of fields
  150 CONTINUE

!---- End of REFERENCE not found
      CALL ERR_MES(1,36)
      CALL FLAGIT(1,ARROW,KPT+11,1,IERR)
      LST = LC
      RETURN

  200 IEND = KPT+1                          ! Store position of final paren
      DO 250 I = KPT,NOW,-1                 ! Backspace to beginning of date
        LST = I
        IF(KARD(I:I).EQ.' ') THEN             ! Blank in date field
          CALL FLAGIT(1,ARROW,I+11,1,IERR)      ! Set error marker
          CALL ERR_MES(1,34)
        ELSE IF (KARD(I:I).EQ.',') THEN
          GOTO 290
        END IF
  250 CONTINUE
      LST = LST-1

  290 NDAT = KPT-LST                        ! Compute # of characters in date
      IF (NDAT.GT.8) THEN                   ! Too many characters
        CALL FLAGIT(1,ARROW,LST+12,NDAT,IERR) ! Set error markers
        CALL ERR_MES(1,37)
        RETURN
      END IF

      KPTDAT = ' '
      KPTDAT(1:NDAT) = KARD(LST+1:KPT)
      CALL DATE_CHECK(KPTDAT,NDAT,IERR1)   ! Check date
      IF (IERR1.NE.0) CALL FLAGIT(1,ARROW,LST+12,NDAT,IERR)
      RETURN
      END
      SUBROUTINE REF_PAGE(KARD,NOW,IERRP)

!* Checks reference page field.
!*   KARD:  input array
!*   NOW:   starting position in array
!*   IERRP: error for reference code (Initialized in REF_CHECK)

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*55 KARD
      CHARACTER LIMTR(3)/',', ')', '('/

!---- Flag leading blanks
      IF(KARD(NOW:NOW).EQ.' ') CALL LEAD_BLANK(KARD,NOW,*990)

!---- Find end of page field (comma).
      CALL PARSCHR(KARD,NOW,KPT,NUM,15,LIMTR,3,ICHR,*900)
      IF(ICHR.EQ.2) GOTO 900

!---- Check page # 
      IF (NUM.NE.0) THEN
        IFIX = INTFORM(KARD(NOW:KPT),NUM,IGOOF,1)
        IF (IGOOF.NE.0) CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRP) ! Set markers
      END IF
      NOW = KPT+1

      IF(ICHR.EQ.3) THEN                  ! Part field present
        CALL REF_PART(KARD,NOW,IERRP,*990)
      END IF
      GOTO 990

!---- SET ERROR FLAG.
  900 CALL FLAGIT(1,ARROW,NOW+11,1,IERRP) ! End of reference not found

  990 RETURN
      END
      SUBROUTINE REF_PART(KARD,NOW,IERRP,*)

!* Checks reference field contained in parentheses
!*   KARD:  input array
!*   NOW:   starting address of field
!*   IERRP: error for reference code (Initialized in REF_CHECK)
!*   RETURN 1: error in field

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*55 KARD

      N1 = NOW+1
      DO 100 I = N1,55                  ! Look for end of subfield (right paren)
        NOW = I
        IF(KARD(I:I).EQ.',') GOTO 150       ! End of field before right paren
        IF(KARD(I:I).EQ.')') GOTO  300
        IF(KARD(I:I).EQ.'(' .OR. KARD(I:I).EQ.' ')
     *    CALL FLAGIT(1,ARROW,I+11,1,IERRP) ! Illegal character, set markers
  100 CONTINUE

!---- ERROR IN FIELD
  150 CALL FLAGIT(1,ARROW,N1+11,1,IERRP)  ! Set error markers
      GOTO 990

  300 IF(NOW.LE.1) CALL FLAGIT(1,ARROW,NOW+11,1,IERRP)
                                          ! Field blank, set error markers

      NOW  = NOW+1
  410 IF(KARD(NOW:NOW).EQ.',') GOTO 900   ! Look for end of field (comma)
      CALL FLAGIT(1,ARROW,NOW+11,1,IERRP) ! Set error markers
      CALL INCRMT(NOW,55,*990)
      GOTO 410

  900 IF(IERRP.EQ.0) RETURN               ! Normal RETURN
  990 RETURN 1                            ! Error RETURN
      END
      SUBROUTINE REL_REF(NOW)

!* Checks keyword REL-REF and MONIT-REF (Separate ENTRY point for MONIT-REF)
!*   NOW: starting point in input array

!---- Input record. (Set in READ_REC)
      COMMON/ALLCOM/KEY2,IPTR,KARD,ISEQ
        CHARACTER*10 KEY2
        CHARACTER*1  IPTR
        CHARACTER*55 KARD
        CHARACTER*13 ISEQ

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*10 LEGHED/'MONIT'/
      CHARACTER LIMTR(2)/',', ')'/

      common /rel_ref_g77/ ierrm
      data ierrm/0/

      IERRM = 0

!!!!  CODE FIELD

      CALL PARSCHR(KARD,NOW,KPT,N,8,LIMTR,2,ICHR,*800)

      IF (N.EQ.1) THEN                        ! Code
          CALL DANVER_NEW(17,KARD(NOW:NOW),0,NONO,*150)
          CALL FLAGIT(1,ARROW,NOW+11,1,IERRM)   ! Set error markers
  150     NOW = KPT+2
      ELSE IF (N.EQ.0) THEN                   ! Empty field
          CALL ERR_MES(1,34)
          CALL FLAGIT(1,ARROW,NOW+11,1,IERR)
          NOW = NOW+1
      ELSE                                    ! Illegal field
          CALL FLAGIT(1,ARROW,NOW+11,N,IERRM)   ! Set error markers
      END IF

      IF(ICHR.EQ.2) GOTO 800                  ! Check for end of code string
      GOTO 250

!!!!  Entry point for MONIT-REF

      ENTRY MONIT_REF(NOW)

!!!!  HEADING field

      IF(KARD(NOW:NOW).EQ.'(') THEN           ! Check heading field
        NOW = NOW+1
        CALL TEST_HEAD(4,KARD,NOW,LEGHED,')',IPTR,*250)
      END IF

!!!!  Subaccession number field

!---- CHECK ACCESSION NUMBER
  250 CALL PARSCHR(KARD,NOW,KPT,N,8,LIMTR,2,ICHR,*800)

      IF (N.NE.0) THEN                        ! Field not empty
        IF (N.EQ.8) THEN
          IFIX = INTFORM(KARD(NOW:KPT),N,NONO,2)
          IF(NONO.NE.0) CALL FLAGIT(1,ARROW,NOW+11,N,IERRM) ! Set markers
          IF(ICHR.NE.1) GOTO 800
        ELSE
          CALL FLAGIT(1,ARROW,NOW+11,N,IERRM)   ! Set error markers
        END IF
      END IF

!!!!  Author field

      NOW = KPT+2
      IF (KARD(NOW:NOW).EQ.',') THEN          ! Author field not present
        NOW = NOW+1
      ELSE                                    ! Author field
        CALL PARSCHR(KARD,NOW,KPT,N,30,LIMTR,2,ICHR,*800)
        CALL CHAR_NAME(4,KARD,NOW,KPT)
        NOW = KPT+2
        IF(NOW.EQ.55) GOTO 800
      END IF

!!!!  REFERENCE FIELD

      CALL REF_CHECK(KARD,NOW)                ! Check field

      GOTO 910

  800 CALL FLAGIT(1,ARROW,NOW+11,1,IERRM)     ! Set error markers

  910 IF (IERRM.NE.0) THEN
        IERR = 1
        CALL ERR_MES(1,11)                    ! Error message 
      END IF
      RETURN
      END
      SUBROUTINE SOURCE_CHECK(KARD,NOW)

!* Checks coded information for BIB keyword INC-SPECT
!*   KARD: input character string
!*   NOW:  position in input string

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*80 DICLINE
      CHARACTER*55 KARD
      CHARACTER*4 UNIT
      CHARACTER LIM,LIMTR(3)/',', ')', '='/

      IERR1 = 0                    ! internal error flag off

!---- Flag leading blanks
      IF(KARD(NOW:NOW).EQ.' ') CALL LEAD_BLANK(KARD,NOW,*990)

!---- Find next code
  250 CALL PARSCHR(KARD,NOW,KPT,NUM,20,LIMTR,3,ICHR,*900)
      IF (NUM.EQ.0) THEN                        ! Empty field
        CALL ERR_MES(1,34)
        CALL FLAGIT(1,ARROW,NOW+11,1,IERR)      
        NOW = NOW+1
        GOTO 300
      ELSE
        CALL DANGET_NEW(19,KARD(NOW:KPT),0,0,DICLINE,IERR1) ! Look up code
        IF (IERR1.NE.0) THEN
          CALL ERR_MES(1,35)
          CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)      
          GOTO 300
        END IF
        LIM = DICLINE(58:58)                        ! Store delimiter
      END IF

  300 NOW = KPT+2
      IF (ICHR.EQ.3) THEN                   
        IF (LIM.NE.'=') THEN                      ! REACTION not expected
          CALL ERR_MES(1,43)
          CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)      
        ELSE
          IF (KARD(NOW:NOW).NE.'(') THEN
            CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)
          ELSE
            NOW = NOW+1
          END IF
          CALL PARSCHR(KARD,NOW,KPT,NUM,40,LIMTR(2),1,ICHR,*900)
          CALL PARSER(4,NOW,KPT,UNIT)
        END IF
      ELSE IF(ICHR.EQ.1) THEN                  ! New code expected, next field
        GOTO 250
      ELSE
        IF (LIM.NE.' ') THEN                   ! Possible missing field
          CALL ERR_WARN(1,6)                      ! Print warning
          CALL FLAGIT(0,ARROW,KPT+11,1,IERR)
        END IF
      END IF
      RETURN

!---- No delimiter found.
  900 CALL ERR_MES(1,36)
      CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)
      
  990 RETURN
      END
      SUBROUTINE STAT_CHECK(KARD,NOW,INTSAN,ISPSDD)

!* Checks coded information for BIB keyword STATUS
!*   KARD:   input character string
!*   NUM:    position in input string
!*   INTSAN: current subaccession #
!*   ISPSDD: superseded data flag

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER*80 LINE
      CHARACTER*55 KARD
      CHARACTER LIMTR(3)/',', ')', ' '/

!!!!  Initialize
      IERRS = 0                             ! internal error flag off
      IFIELD = 1                            ! set to 1st field

  110 CALL PARSCHR(KARD,NOW,KPT,NUM,5,LIMTR,3,ICHR,*800) ! Find code

      IF (NUM.EQ.0) THEN                    ! Empty field
        CALL ERR_MES(1,63)
        CALL FLAGIT(1,ARROW,NOW+11,1,IERR)      
        NOW = NOW+1
        GOTO 290
      END IF

  200 CALL DANGET_NEW(16,KARD(NOW:KPT),0,0,LINE,NONO)  ! Look up code in Dict
      IF(NONO.NE.0) CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRS)
                                            ! Code not found, set error markers
      NOW  = KPT+2
      
      IF (IFIELD.EQ.1) THEN                 ! One code read
        IFIELD = IFIELD+1                     ! Increment field
        IF(LINE(61:61).NE.' ') GOTO 300       ! Branch if SAN expected
      END IF

  290 IF (ICHR.EQ.3) THEN                   ! Blank at end of code
        CALL PARSCHR(KARD,NOW,KPT,NUM,5,LIMTR,2,ICHR,*800) ! Find seperator
      END IF
      IF (ICHR.EQ.2) GOTO 900               ! End of code string
      GOTO 110

!!!!  Codes which allow subaccession # in 2nd field

!---- For superseded, out-of-date or unobtainable data, store SAN
  300 READ(LINE(1:5),1000) INE
      IF(INE.GE.80) ISPSDD = INTSAN

      IF (ICHR.EQ.2) THEN                   ! No 2nd field
        IF (LINE(61:61).EQ.'R') THEN          ! Required 
          CALL ERR_MES(1,38)
          CALL FLAGIT(1,ARROW,NOW+10,1,IERR)      
        ELSE
          CALL ERR_WARN(1,6)                  ! Error message 
          CALL FLAGIT(0,ARROW,NOW+10,1,IERR)      
        END IF
        GOTO 990
      END IF

!---- Check for subaccession # in field
      CALL PARSCHR(KARD,NOW,KPT,NUM,9,LIMTR,2,ICHR,*800)
      IFIX = INTFORM(KARD(NOW:KPT),NUM,NONO,2)

      IF (NONO.NE.0) THEN                   ! Code not SAN
        IERR = 1
        CALL ERR_MES(1,37)                    ! Error message 
        GOTO 200
      ELSE                                  ! SAN found
        IF(NUM.NE.8) GOTO 800                 ! Not 8 digits
        IF(ICHR.EQ.2) GOTO 900                ! Should be end of string
      END IF

  800 CALL FLAGIT(1,ARROW,NOW+11,NUM,IERRS) ! Set error markers

  900 IF (IERRS.NE.0) THEN
        CALL ERR_MES(1,11)                    ! Error message 
        IERR = 1
      END IF
      NOW = KPT                             ! Set position to free text field

  990 RETURN

 1000 FORMAT(I5)
      END
      SUBROUTINE TEST_HEAD(IWAY,KARD,NOW,LEGHED,LSEP,PTR,*)

!* Checks data heading code for BIB keywords MONITOR, ASSUMED, and REL-REF
!*   IWAY:   index for keyword
!*           2 = MONITOR
!*           3 = ASSUMED
!*           4 = MON-REF
!*   KARD:   input array
!*   NOW:    current position in array
!*   LEGHED: legal heading
!*   LSEP:   terminator for string
!*   PTR:    pointer
!*   RETURN 1: legal heading not found

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

      CHARACTER LSEP,PTR
      CHARACTER*55 KARD
      CHARACTER*10 LEGHED,KASS

!---- Find code
      CALL PARSCHR(KARD,NOW,KPT,NUM,11,LSEP,1,ISEP,*50)
      KASS = ' '
      KASS(1:NUM) = KARD(NOW:KPT)

!---- Check for legal heading followed by blank, '-', or 1-digit integer
      IF (KASS(1:5).NE.LEGHED(1:5)) GOTO 50   ! Illegal heading
      IF (KASS(6:6).EQ.'-' .OR. KASS(6:10).EQ.' ') GOTO 90
      IF (KASS(6:6).GE.'1' .AND. KASS(6:6).LE.'9') GOTO 90

   50 CALL ERR_FIELD(1,1,17)                  ! Error message 
      CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)    ! Set error markers
      RETURN 1

   90 IF (IWAY.EQ.2 .OR. IWAY.EQ.3) THEN      ! MONITOR and ASSUMED
        CALL ADD_HEAD(KASS,PTR,*200)            ! Add heading to list

!----   Heading already on list
        CALL ERR_MES(1,39)
        CALL FLAGIT(1,ARROW,NOW+11,NUM,IERR)      
      END IF

  200 NOW = KPT+2
      RETURN
      END
      SUBROUTINE UNIT_CHECK(IWAY,IDEP)

!* Checks units versus data headings
!*   IWAY: 1 = SAN 1 COMMON section
!*         2 = Data subentry COMMON section
!*         3 = Data subentry DATA section
!*   IDEP: dependent variable found flag

!---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

!---- Stored headings for SAN 1 + current subentry. (Set in HEAD_PROC)
      COMMON/DATA_HEAD/NHD(3),HEADS(18,3),HPTR(18,3)
        CHARACTER*11 HEADS
        CHARACTER HPTR

!---- Stored units for current section. (Set in HEAD_PROC)
      COMMON/DATA_UNITS/HEAD_CODE(18),UTYP(18)
        CHARACTER*15 HEAD_CODE
        CHARACTER*4 UTYP

!---- Error pointers. (Initialized in BIBSUB)
      COMMON/ERRPTR/IERR,ARROW
        CHARACTER*80 ARROW

!---- Stored family, sub-family codes from Dictionary 24 for each field 
!-    (Set in UNIT_CHECK).
!-      IFAM: family
!-            0 = flags
!-                subfamily 1=FLAG
!-                          2=DECAY-FLAG
!-                          3=LVL-FLAG
!-                          4=MISC
!-            1 = monitor, assumed values
!-                subfamily 1=monitor 
!-                          2=assumed 
!-            2 = data
!-            3 = resonance parameter
!-                subfamily 1=quantum number
!-                          2=resonance energy 
!-            4 = incident energy
!-                subfamily 1=energy
!-                          2=momentum
!-                          3=spectrum energy
!-            5 = secondary energy
!-                subfamily 1=particle energy
!-                          2=level energy
!-                          3=excitation energy
!-                          4=Q value
!-                          5=energy degredation
!-                          6=energy gain
!-                          7=level number
!-                          8=linear momentum
!-                          9=polarity
!-            6 = angle
!-                subfamily 1=angle
!-                          2=cosine
!-                          7=momentum transfer
!-                          8=wave number
!-            7 = number
!-                subfamily 1=coefficient number
!-                          2=kq
!-            8 = other variable 
!-                subfamily 1=half-life, 
!-                          2=sample temperature, 
!-                          3=sample thickness,
!-                          4=polarization
!-            9 = isotope/particle identification
!-                subfamily 1=element
!-                          2=mass
!-                          3=isomer
!-                          4=monitor element
!-                          5=monitor mass
!-                          8=effective mass
!-                          9=emitted nucleons
!----   ISUB: subfamily (value dependent on family, see above)
      COMMON/HEAD_FAMLY/IFAM(18),ISUB(18)
        INTEGER IFAM,ISUB

!---- Independent variable families required. (Initialized in BIB_SUB)
      COMMON/FAMLY_REQ/NEEDS(7),NEEDFL,NOTCK

!---- Stored pointers for REACTION (Initialized in BIB_SUB)
      COMMON/REAC_PTR/NPR,NPR1,RACPTR(20),RUNIT(20)
        CHARACTER RACPTR
        CHARACTER*4 RUNIT

!---- Family codes for data fields.  (Set in UNIT_CHECK).
!-      INDFAM: variable field equivalent for each field.
!-              Set to family code for dependent and independent variables.
!-              Set to 10 + family code for errors.
!-              Set to -subfamily for flags
!----           Set to 0 for all other headings.
      COMMON/VAR_FIELD/INDFAM(18)
        INTEGER INDFAM

      CHARACTER*20 MESS/'     in field       '/
      CHARACTER*4 UTYPE
      CHARACTER FNOW
      INTEGER IDEP

      DO 500 I = 1,NHD(IWAY)               ! Loop over stored headings

        READ(HEAD_CODE(I)(1:1),1000) IFAM(I) ! Store family number
        READ(HEAD_CODE(I)(2:2),1000) ISUB(I) ! Store family number
        UTYPE = HEAD_CODE(I)(11:14)          ! Store unit type for heading
!----   Store variable type from plotting flags.
        IF (IFAM(I).EQ.2) THEN               
          FNOW = HEAD_CODE(I)(7:7)
        ELSE
          FNOW = HEAD_CODE(I)(4:4)           
        END IF

!---- Store variable flags
        IF (IFAM(I).EQ.0) THEN               ! FLAG
          INDFAM(I) = -ISUB(I)
        ELSE IF (IFAM(I).EQ.1) THEN          ! MONIT, ASSUM
          INDFAM(I) = 0
        ELSE                                 ! Other field
          IF (FNOW.EQ.'9') THEN                ! Error
            INDFAM(I) = 10 + IFAM(I)
          ELSE IF (FNOW.NE.'0') THEN           ! Variable
            INDFAM(I) = IFAM(I)
            IF(IFAM(I).EQ.2) IDEP = 1            ! Dependent variable found
          END IF
        END IF

!!!   Dependent variables 

        IF (IFAM(I).EQ.2) THEN               
          IF(NOTCK.NE.0 .OR. NPR.EQ.0) GOTO 500 
          N = LOOKUP(HPTR(I,IWAY),RACPTR,NPR)     ! Find pointer index
          IF(N.EQ.0) N = 1                        ! Default for blank pointer
          IF(RUNIT(N).EQ.'MIS ') GOTO 500         ! Unit check to be skipped

          IF (RUNIT(N).NE.UTYP(I)) THEN           ! Not expected units

            IF (INDFAM(I).GT.10) THEN             ! error field
              IF (UTYP(I).EQ.'PC  ') GOTO 500       ! % allowed
              IF (UTYP(I).EQ.'RSL ' .AND. IFAM(I).EQ.4) GOTO 500
                                                    ! energy resolution
            END IF

            MESS(1:4) = RUNIT(N)                    ! Store unit for listing
            WRITE(MESS(15:16),2000) I
            CALL ERR_INSERT(1,11,20,MESS)
          END IF

!!!   Independent variables        

        ELSE IF (IFAM(I).GT.2) THEN
          IF(UTYPE.NE.UTYP(I)) THEN             ! Unit codes not equal

            IF (INDFAM(I).EQ.4) THEN              ! Energy field
              IF(ISUB(I).EQ.4) THEN                 ! Spectrum temperature
                IF(UTYP(I).EQ.'TEM ') GOTO 500 
              ELSE IF (ISUB(I).NE.2) THEN           ! Other than momentum
                IF(UTYP(I).EQ.'L   ') GOTO 500          ! Wave length
              END IF

            ELSE IF (INDFAM(I).GT.10) THEN        ! Error field
              IF (UTYP(I).EQ.'PC  ') GOTO 500       ! % allowed for error
              IF (IFAM(I).EQ.4) THEN                ! energy
                IF (ISUB(I).LE.2 .AND. UTYP(I).EQ.'RSL ') GOTO 500
                                                      ! resolution allowed
              END IF
            END IF
            MESS(1:4) = UTYPE
            WRITE(MESS(15:15),1000) I
            CALL ERR_INSERT(1,11,20,MESS)
          END IF 
        END IF

  500 CONTINUE
      RETURN

 1000 FORMAT(I1)
 2000 FORMAT(I2)
      END     
