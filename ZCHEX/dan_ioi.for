      SUBROUTINE OPEN_NEW(NUM,IERR,*)

C*  Opens dictionary file; assigns unit #.
C*    NUM: dictionary #
C*    IERR: error #
C*    RETURN 1: error 

C---- Units for open dictionaries.  Set in OPEN_NEW.
      COMMON/DANUNITS/NOPEN(200)

C---- DAN_TOP variables
C-      NKEY: # of keys for dictionary
C-      NFLD: # of fields in code
C----   LENFLD: length of fields in code string
      COMMON/TOPREC/NKEY(200),NFLD(200)
      COMMON/TOPSTR/LENFLD(10,200)

      CHARACTER*30 NAME/'DANIEL$LIB:DICT_NEW.   '/

      DATA NOPEN,INIT/201*0/
      DATA IDIC/80/                         ! INITIALIZE OUTPUT UNIT

   10 FORMAT(I3.3)

      IF (NOPEN(NUM).EQ.0) THEN             ! Dictionary not opened

        IF (INIT.EQ.0) THEN                   ! 1st open
          CALL DAN_TOP_new(IERR,*805)               ! Open dictionary control list
          INIT = 1
        END IF

        IDIC = IDIC+1
        NOPEN(NUM) = IDIC
        WRITE(NAME(21:23),10) NUM 

        IF (NKEY(NUM).EQ.1) THEN
          OPEN (UNIT=IDIC, FILE=NAME, STATUS='OLD',
     *        ORGANIZATION='INDEXED', ACCESS='KEYED', RECL=120,
     *        FORM='FORMATTED', KEY=(10:39:CHARACTER),
     *        RECORDTYPE='FIXED', READONLY, IOSTAT=IOS, ERR=805)
        ELSE IF (NKEY(NUM).EQ.2) THEN
          K2 = 39 + LENFLD(1,NUM)
          OPEN (UNIT=IDIC, FILE=NAME, STATUS='OLD',
     *        ORGANIZATION='INDEXED', ACCESS='KEYED', RECL=120,
     *        FORM='FORMATTED', 
     *        KEY=(10:39:CHARACTER,40:K2:CHARACTER),
     *        RECORDTYPE='FIXED', READONLY, IOSTAT=IOS, ERR=805)
        ELSE IF (NKEY(NUM).EQ.3) THEN
          K2 = 39 + LENFLD(1,NUM)
          K3 = K2 + LENFLD(2,NUM)
          OPEN (UNIT=IDIC, FILE=NAME, STATUS='OLD',
     *        ORGANIZATION='INDEXED', ACCESS='KEYED', RECL=120,
     *        FORM='FORMATTED', 
     *        KEY=(10:39:CHARACTER,40:K2:CHARACTER,K2+1:K3:CHARACTER),
     *        RECORDTYPE='FIXED', READONLY, IOSTAT=IOS, ERR=805)
        END IF
      ELSE                                  ! Open dictionary for update
        IF (NKEY(NUM).EQ.1) THEN
          OPEN (UNIT=IDIC, FILE=NAME, STATUS='OLD',
     *        ORGANIZATION='INDEXED', ACCESS='KEYED', RECL=120,
     *        FORM='FORMATTED', KEY=(10:39:CHARACTER),
     *        RECORDTYPE='FIXED', IOSTAT=IOS, ERR=805)
        ELSE IF (NKEY(NUM).EQ.2) THEN
          K2 = 39 + LENFLD(1,NUM)
          OPEN (UNIT=IDIC, FILE=NAME, STATUS='OLD',
     *        ORGANIZATION='INDEXED', ACCESS='KEYED', RECL=120,
     *        FORM='FORMATTED', 
     *        KEY=(10:39:CHARACTER,40:K2:CHARACTER),
     *        RECORDTYPE='FIXED', IOSTAT=IOS, ERR=805)
        ELSE IF (NKEY(NUM).EQ.3) THEN
          K2 = 39 + LENFLD(1,NUM)
          K3 = K2 + LENFLD(2,NUM)
          OPEN (UNIT=IDIC, FILE=NAME, STATUS='OLD',
     *        ORGANIZATION='INDEXED', ACCESS='KEYED', RECL=120,
     *        FORM='FORMATTED', 
     *        KEY=(10:39:CHARACTER,40:K2:CHARACTER,K2+1:K3:CHARACTER),
     *        RECORDTYPE='FIXED', IOSTAT=IOS, ERR=805)
        END IF
      END IF

      RETURN

C---- ERROR OPENING DICTIONARY
  805 IERR = 6
      CALL DANERROR(NUM,IERR)
      RETURN 1

      ENTRY CLOSE_DIC(NUM)

      CLOSE(NOPEN(NUM))
      NOPEN(NUM) = 0
      RETURN

      END
      SUBROUTINE DAN_TOP_NEW(IERR,*)

C*  Reads dictionary control index (DICT_NEW.TOP).
C*    IERR: error # 
C*    RETURN 1: read error

C---- DAN_TOP variables
C-      NKEY: # of keys for dictionary
C-      NFLD: # of fields in code
C----   LENFLD: length of fields in code string
      COMMON/TOPREC/NKEY(200),NFLD(200)
      COMMON/TOPSTR/LENFLD(10,200)

      CHARACTER*40 FORM
      CHARACTER*3 CNUM

      DATA IDIC/80/

C*    Format of dictionary control list records
C*      CNUM: dictionary number (A3)
C*      NAME: dictionary name (A30)
C*      NKEY: number of dictionary keys (I1)
C*      NFLD: number of dictionary fields (I2)
C*      FORM: format of dictionary fields - up to 10 (A40)
C*      NREC: # of records in dictionary (I4)

   11 FORMAT(1X,A3,30X,I1,I2,A40)
   20 FORMAT(I3)

C---  Open control list
      OPEN (UNIT=IDIC, FILE='DANIEL$LIB:DICT_NEW.TOP',
     *    STATUS='OLD', ORGANIZATION='INDEXED', ACCESS='SEQUENTIAL',
     *    RECORDTYPE='FIXED', FORM='FORMATTED', READONLY, ERR=800)

      J = 0                                 ! Initialize read tries

  110 READ(IDIC,11,END=200,IOSTAT=IOS,ERR=805) CNUM,NKEYI,NFLDI,FORM

      UNLOCK (IDIC)

      READ(CNUM,20) NUM                     ! Store dictionary number
      NKEY(NUM) = NKEYI                     ! Store # OF KEYS
      NFLD(NUM) =NFLDI                      ! Store # of code fields
      II = 2

      DO 150 I=1,NFLDI-1                    ! Store format of code fields
        II = II+4
        READ(FORM(II:II+1),20) LENFLD(I,NUM)
  150 CONTINUE

      GOTO 110

  200 CLOSE (UNIT=IDIC)
      RETURN

C---  Error land
  800 IERR = 8
      GOTO 845

  805 IF (IOS.EQ.52) THEN                   ! Locked record
        J = J+1
        IF (J.LE.20) GOTO 110               ! Try to read up to 20 times
        IERR = 3
      ELSE                                  ! Error in reading record
        IERR = 9
      END IF

  845 CALL DANERROR(0,IERR)
      RETURN 1
      END
c_g77 SUBROUTINE READ_KEY(IDIC,KEY,LNTH,NKEY,KEY1,REC,STAT,IERR,*)
      SUBROUTINE READZ_KEY(NUMdic,KEY,LNTH,NKEY,KEY1,REC,STAT,IERR,*)

C*  Reads dictionary record specified by SKEY and NKEY
C*    IDIC: dictionary unit number
C*    KEY: key for record to be retrieved
C*    LNTH: key length
C*    NKEY: keyid #
C*    KEY1: primary key
C*    REC: dictionary record (returned)
C*    STAT: record status
C*    IERR: error flag
C*    RETURN 1: error or key not in dictionary

      CHARACTER*(*) KEY
      CHARACTER*3 STAT
      CHARACTER*30 KEY1
      CHARACTER*80 REC

      COMMON/DANUNITS/NOPEN(200)
      IDIC = NOPEN(NUMdic)

    2 FORMAT(A3,I6,A30,A80)

      IERR = 0                              ! TURN OFF ERROR FLAG
      J = 0                                 ! INITIALIZE # OF READ TRIES
c-zvv      write(*,*) '===find: 1..',LNTH,' [',KEY(1:LNTH),']'
C---- READ RECORD FOR KEY SPECIFIED
   20 READ (IDIC,2,KEY=KEY(1:LNTH),KEYID=NKEY,IOSTAT=IOS,ERR=820)
     *      STAT,IDATE,KEY1,REC
      UNLOCK (IDIC)
      RETURN

  820 IF (IOS.EQ.52) THEN                   ! LOCKED RECORD
        J = J+1
        IF (J.LE.20) GOTO 20                  ! TRY TO READ UP TO 20 TIMES
        IERR = 4 
      ELSE IF (IOS.EQ.36) THEN              ! KEY NOT FOUND
        IERR = 20
      ELSE                                  ! ERROR IN READING RECORD
        IERR = 7
      END IF

      RETURN 1
      END
c_g77 SUBROUTINE READ_SEQ(IDIC,REC,STATUS,IERR,*,*)
      SUBROUTINE READZ_SEQ(NUMdic,REC,STATUS,IERR,*,*)

C*  Reads dictionary record sequentially
C*    IDIC: dictionary unit number
C*    REC: character string returned
C*    STATUS: status code for line (A3)
C*    IERR: error # (0 = NO ERROR)
C*    RETURN 1: error
C*    RETURN 2: end-of-file

      CHARACTER*3 STATUS
      CHARACTER*110 REC

      COMMON/DANUNITS/NOPEN(200)
      IDIC = NOPEN(NUMdic)

    2 FORMAT(A3,I6,A110)

      J = 0                                 ! INITIALIZE # OF READ TRIES

C---- READ RECORD SEQUENTIALLY
   20 READ (IDIC,2,IOSTAT=IERR,ERR=820,END=500) STATUS,IDATE,REC
      UNLOCK (IDIC)
      RETURN

  500 RETURN 2                              ! END-OF-FILE

  820 IF (IERR.EQ.52) THEN                  ! LOCKED RECORD
        J = J+1
        IF (J.LE.20) GOTO 20                  ! TRY READ UP TO 20 TIMES
        IERR = 4
      ELSE                                  ! ERROR IN READING RECORD
        IERR = 7
      END IF

      RETURN 1
      END
      SUBROUTINE READ_VER_NEW(IDIC,STATUS,IDATE,KEY,REC,FLAG,IERR,*)

C*  Reads dictionary record sequentially
C*    IDIC: dictionary unit number
C*    REC: character string returned
C*    STATUS: status code for line (A3)
C*    FLAG: verification flag (A1)
C*    IERR: error # 
C*    RETURN 1: end-of-file

      CHARACTER FLAG
      CHARACTER*3 STATUS
      CHARACTER*30 KEY
      CHARACTER*80 REC

    2 FORMAT(A3,I6,A30,A80,A1)

      J = 0                                 ! INITIALIZE # OF READ TRYS
      IERR = 0                              ! INITIALIZE ERROR FLAG

C---- READ RECORD SEQUENTIALLY
   20 READ (IDIC,2,IOSTAT=IERR,ERR=820,END=500) STATUS,IDATE,KEY,REC,
     *  FLAG
      UNLOCK (IDIC)
      RETURN

  500 RETURN 1                              ! END-OF-FILE

  820 IF (IERR.EQ.52) THEN                  ! LOCKED RECORD
        J = J+1
        IF (J.LE.20) GOTO 20                  ! TRY READ UP TO 20 TIMES
        IERR = 4
      ELSE                                  ! ERROR IN READING RECORD
        IERR = 7
      END IF

      RETURN
      END
