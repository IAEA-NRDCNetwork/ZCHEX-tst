c main
C* DANIEL_NEW:  Daniel Dictionary Database User Subroutines
C**
C* Version 2000-1 (May 2000)
C*   Added routines to read files with keys of 30 characters.
C* Version 98-1 (December 1998)
C*   Updated to direct all control list reads to OPEN_NEW.
C*   Updated DAN_TOP to close file after reading.
C* Version 94-1 (November 1994)
C*   Added routine GET_PRIME_KEY
C**
C* Written by Victoria McLane
C*            National Nuclear Data Center
C*            Brookhaven National Laboratory
C*            Upton, NY 11973
C**
C* Retrieval subroutines:
C*   DANGET - retrieves a record or field using dictionary number and key;
C*            returns status
C*   DANGET_NEW - retrieves a record or field using dictionary number and key
C*            for files with 30 character keys; returns status
C*   DANGET_STA - retrieves a record or field using dictionary number and
C*                key; returns status
C*   DANGET_STA_NEW - retrieves a record or field using dictionary number and
C*                key for files with 30 character keys; returns status
C*   DANORD - retrieves a record or field using sequential read from dictionary
C*   DANORD_NEW - retrieves a record or field using sequential read from 
C*                dictionary for files with 30 character keys
C*   DANORD_STA - retrieves a record or field using sequential read from
C*                dictionary; returns status
C*   DANORD_STA_NEW - retrieves a record or field using sequential read from
C*                dictionary for files with 30 character keys; returns status
C*   DANVER - verifies if a key exists in the specified dictionary
C*   DANVER_NEW - verifies if a key exists in the specified dictionary for 
C*                files with 30 character keys
C*   DANVER_CIN - verifies if a key exists in the specified dictionary;
C*                key length given as input parameter (for CINDA use of 
C*                dictionaries 6 and 7).
C*   DANVER_CIN_NEW - verifies if a key exists in the specified dictionary for
C*                files with 30 character keys; key length given as input 
C*                parameter (for CINDA use of dictionaries 6 and 7).
C*   DANVER_STA - verifies if a key exists in the specified dictionary;
C*                returns status
C*   DANVER_STA_NEW - verifies if a key exists in the specified dictionary;
C*                returns status for files with 30 character keys
C*   GET_PRIME_KEY - returns primary key for secondary key specified
C*   GET_PRIME_KEY_NEW - returns primary key for secondary key specified
C*                for files with 30 character keys
C**
C* General subroutines:
C*   OPEN_NEW - opens specified dictionary, if not already open; reads
C*              control list for 1st open for files with 30 character keys
C*   DAN_TOP - reads control list record for all dictionaries
C*   DAN_TOP_NEW - reads control list record for all dictionaries for files
C*              with 30 character keys
C*   READ_KEY - reads dictionary record by key specified for files with 
C*              30 character keys
C*   READ_KEY1 - reads dictionary record by primary key
C*   READ_KEY1_NEW - reads dictionary record by primary key for files
C*              with 30 character keys
C*   READ_SEQ - reads dictionary record sequentially for files with 30
C*              character keys
C*   READ_VER - reads dictionary record sequentially for verification
C*   READ_VER_NEW - reads dictionary record sequentially for verification for 
C*              files with 30 character keys
C*   DANERROR - types error messages
C**
















      SUBROUTINE DANGET_new(NUM,KEY,IKEY,IFLD,LINE,IERR)
C**
C*  Gets a record from the specified dictionary using the
C*      specified key and returns the desired field.
C*    NUM: dictionary number (I2)
C*    KEY: key for record being retrieved (A*)
C*    IKEY: key ID (I); 0 = 1st key
C*    IFLD: field number requested (I): 0 = all fields
C*                                     -1 = primary key
C*    LINE: information returned to the caller (A80)
C**   IERR: error flag (return) (I); 0 = no error   

C---- Units for open dictionaries.  Set in OPEN_NEW.
      COMMON/DANUNITS/NOPEN(1000)

C---- DAN_TOP variables
C-      NKEY: # of keys for dictionary
C-      NFLD: # of fields in code
C----   LENFLD: length of fields in code string
      COMMON/TOPREC/NKEY(1000),NFLD(1000)
      COMMON/TOPSTR/LENFLD(20,1000)

      CHARACTER*(*) KEY
      CHARACTER*3 ISTAT
      CHARACTER*30 SKEY,KEY1
      CHARACTER*80 REC,LINE

      SKEY = KEY                            ! Restore key as 20 chars.
      LINE = ' '                            ! Initialize return string

      CALL OPEN_NEW(NUM,IERR,*950)          ! Open dictionary

C---- Check field number (max. 10)
      IF (IFLD.LT.-1 .OR. IFLD.GT.NFLD(NUM)) THEN
        IERR = 5
        GOTO 800
      END IF

C---- Check key number.
      IF (IKEY.LT.0 .OR. IKEY.GT.NKEY(NUM)-1) THEN
        IERR = 13
        GOTO 800
      END IF

C---- Set key length
      IF (IKEY.EQ.0) THEN
        LNTH = 20
      ELSE
        LNTH = LENFLD(IKEY,NUM)
      END IF

c?    CALL READ_KEY(NOPEN(NUM),SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*800)
      CALL READZ_KEY(NUM,SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*800)

      IF (IFLD.EQ.0) THEN                     ! Return all fields
        LINE = REC

      ELSE IF (IFLD.EQ.-1) THEN               ! Return primary key
        LINE(1:30) = KEY1

      ELSE                                    ! Return requested field
        I1 = 1
        IF (IFLD.GT.1) THEN                     ! Fields > 1 
          DO 50 I = 1,IFLD-1                      ! Set beginning of field
            I1 = I1+LENFLD(I,NUM) 
   50     CONTINUE
        END IF
        LINE = ' '                              ! Initialize retrieved record
        I2 = I1+LENFLD(IFLD,NUM)-1              ! Set end of field
        LNTH = LENFLD(IFLD,NUM)                 ! Set field length
        LINE(1:LNTH) = REC(I1:I2)
      END IF

      RETURN

C---- Write error message, except for key not found.
  800 IF(IERR.NE.20) CALL DANERROR(NUM,IERR)

  950 RETURN
      END








      SUBROUTINE DANGETN_new(NUM,KEY,i0KEY,IKEY,IFLD,LINE,IERR)
C**
C*  Gets a record from the specified dictionary using the
C*      specified key and returns the desired field.
C*    NUM: dictionary number (I2)
C*    KEY: key for record being retrieved (A*)
C*    IKEY: key ID (I); 0 = 1st key
C*    IFLD: field number requested (I): 0 = all fields
C*                                     -1 = primary key
C*    LINE: information returned to the caller (A80)
C**   IERR: error flag (return) (I); 0 = no error   

C---- Units for open dictionaries.  Set in OPEN_NEW.
      COMMON/DANUNITS/NOPEN(1000)

C---- DAN_TOP variables
C-      NKEY: # of keys for dictionary
C-      NFLD: # of fields in code
C----   LENFLD: length of fields in code string
      COMMON/TOPREC/NKEY(1000),NFLD(1000)
      COMMON/TOPSTR/LENFLD(20,1000)

      CHARACTER*(*) KEY
      CHARACTER*3 ISTAT
      CHARACTER*30 SKEY,KEY1
      CHARACTER*80 REC,LINE

      SKEY = KEY                            ! Restore key as 20 chars.
      LINE = ' '                            ! Initialize return string

      CALL OPEN_NEW(NUM,IERR,*950)          ! Open dictionary

C---- Check field number (max. 10)
      IF (IFLD.LT.-1 .OR. IFLD.GT.NFLD(NUM)) THEN
        IERR = 5
        GOTO 800
      END IF

C---- Check key number.
      IF (IKEY.LT.0 .OR. IKEY.GT.NKEY(NUM)-1) THEN
        IERR = 13
        GOTO 800
      END IF

C---- Set key length
      IF (IKEY.EQ.0) THEN
        LNTH = 20
      ELSE
        LNTH = LENFLD(IKEY,NUM)
      END IF

c?    CALL READ_KEY(NOPEN(NUM),SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*800)
c+    CALL READZ_KEY(NUM,SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*800)
      CALL READZ_KEYN(NUM,SKEY,i0KEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*800)

      IF (IFLD.EQ.0) THEN                     ! Return all fields
        LINE = REC

      ELSE IF (IFLD.EQ.-1) THEN               ! Return primary key
        LINE(1:30) = KEY1

      ELSE                                    ! Return requested field
        I1 = 1
        IF (IFLD.GT.1) THEN                     ! Fields > 1 
          DO 50 I = 1,IFLD-1                      ! Set beginning of field
            I1 = I1+LENFLD(I,NUM) 
   50     CONTINUE
        END IF
        LINE = ' '                              ! Initialize retrieved record
        I2 = I1+LENFLD(IFLD,NUM)-1              ! Set end of field
        LNTH = LENFLD(IFLD,NUM)                 ! Set field length
        LINE(1:LNTH) = REC(I1:I2)
      END IF

      RETURN

C---- Write error message, except for key not found.
  800 IF(IERR.NE.20) CALL DANERROR(NUM,IERR)

  950 RETURN
      END
























      SUBROUTINE DANGET_STA_NEW(NUM,KEY,IKEY,IFLD,LINE,STATUS,IERR)

C*  Gets a record from the specified dictionary using the specified key
C*    and returns the desired field; returns status.
C*  Errors are returned to the caller
C*    NUM: dictionary number (I2)
C*    KEY: key for record being retrieved (A*)
C*    IKEY: key ID (I); 0 = 1st key
C*    IFLD: field number requested (I); 0 = all fields
C*                                     -1 = primary key
C*    LINE: information returned to the caller (A80)
C*    STATUS: status code for line (A3)
C*    IERR: error flag (return) (I); 0 = no error   

C---- Units for open dictionaries.  Set in OPEN_NEW.
      COMMON/DANUNITS/NOPEN(1000)

C---- DAN_TOP variables
C-      NKEY: # of keys for dictionary
C-      NFLD: # of fields in code
C----   LENFLD: length of fields in code string
      COMMON/TOPREC/NKEY(1000),NFLD(1000)
      COMMON/TOPSTR/LENFLD(20,1000)

      CHARACTER*(*) KEY
      CHARACTER*3 STATUS,ISTAT
      CHARACTER*30 SKEY,KEY1
      CHARACTER*80 REC,LINE

      SKEY = KEY                            ! Restore key as 20 chars.
      LINE = ' '                            ! Initialize return string

      CALL OPEN_NEW(NUM,IERR,*950)          ! Open dictionary

C---  Check field number (max. 10)
      IF (IFLD.LT.-1 .OR. IFLD.GT.NFLD(NUM)) THEN
        IERR = 5
        GOTO 800
      END IF

C---  Check key number (max. 3)
      IF (IKEY.LT.0 .OR. IKEY.GT.NKEY(NUM)-1) THEN
        IERR = 13
        GOTO 800
      END IF

C---- Set key length
      IF (IKEY.EQ.0) THEN
        LNTH = 30
      ELSE
        LNTH = LENFLD(IKEY,NUM)
      END IF

c?    CALL READ_KEY(NOPEN(NUM),SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*800)
      CALL READZ_KEY(NUM,SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*800)

      IF (IFLD.EQ.0) THEN                   ! Return all fields
        LINE = REC

      ELSE IF (IFLD.EQ.-1) THEN             ! Return primary key
        LINE(1:30) = KEY1

      ELSE                                  ! Return requested field
        I1 = 1 
        IF (IFLD.GT.1) THEN                   ! Fields > 1 
          DO 50 I = 1,IFLD-1                    ! Set beginning of field
            I1 = I1+LENFLD(I,NUM) 
   50     CONTINUE
        END IF
        LINE = ' '                            ! Initialize retrieved record
        I2 = I1+LENFLD(IFLD,NUM)-1            ! Set end of field
        LNTH = LENFLD(IFLD,NUM)               ! Set field length
        LINE(1:LNTH) = REC(I1:I2)
      END IF

      STATUS = ISTAT
      RETURN

  800 IF(IERR.NE.20) CALL DANERROR(NUM,IERR)

  950 RETURN
      END
      SUBROUTINE DANORD_NEW(NUM,IOPT,IFLD,LINE,SKEY,IERR,*)

C*  Reads dictionary sequentially.
C*    NUM: number of dictionary (I2)
C*    IOPT: record retrieval option (A1); F = first record
C*                                        N = next record
C*    IFLD: field desired (I); 0 = all fields
C*    LINE: field retrieved (A80)
C*    SKEY: primary key for record being retrieved (A20)
C*    IERR: errors (I); 0 = no error
C*    RETURN 1: END-OF-FILE

C---- Units for open dictionaries.  Set in OPEN_NEW.
      COMMON/DANUNITS/NOPEN(1000)

C---- DAN_TOP variables
C-      NKEY: # of keys for dictionary
C-      NFLD: # of fields in code
C----   LENFLD: length of fields in code string
      COMMON/TOPREC/NKEY(1000),NFLD(1000)
      COMMON/TOPSTR/LENFLD(20,1000)

      CHARACTER*110 REC
      CHARACTER*80 LINE
      CHARACTER*30 SKEY
      CHARACTER*3 ISTAT
      CHARACTER IOPT,LEGOPT(2)/'F','N'/

C---- TEST FOR LEGAL OPTION
      DO 110 I=1,2
        NOPT=I
        IF (IOPT.EQ.LEGOPT(I)) GOTO120
  110 CONTINUE
      GOTO 820

  120 IF (NOPT.EQ.1) THEN                    ! FIRST RECORD
        CALL OPEN_NEW(NUM,IERR,*950)         ! OPEN DICTIONARY
      END IF

C---- CHECK FIELD NUMBER
      IF (IFLD.LT.0 .OR. IFLD.GT.NFLD(NUM)) THEN
        IERR = 5
        GOTO 900
      END IF

C---- READ DATA INTO STRING/INCLUDING STATUS
c?    CALL READ_SEQ(NOPEN(NUM),REC,ISTAT,IERR,*900,*500)
      CALL READZ_SEQ(NUM,REC,ISTAT,IERR,*900,*500)

      SKEY = REC(1:30)

      IF (IFLD.EQ.0) THEN                   ! All fields
        LINE = REC(31:110)                    ! Transfer all fields
      ELSE
        I1 = 31                               ! Initialize start of field
        IF (IFLD.GT.1) THEN                   ! Field > 1
          DO 350 I = 1,IFLD-1                   ! Reset start of field
            I1 = I1+LENFLD(I,NUM)
  350     CONTINUE
        END IF
        LINE = ' '                            ! Initialize fields returned
        I2 = I1+LENFLD(IFLD,NUM)-1            ! Set end of field 
        LNTH = LENFLD(IFLD,NUM)               ! Set field length
        LINE(1:LNTH) = REC(I1:I2)
      END IF

      RETURN

C---- END-OF-FILE, CLOSE DICTIONARY
  500 CALL CLOSE_DIC(NUM)
      RETURN 1

  820 CALL DANERROR(NUM,16)                 ! Illegal record retrieval option
      RETURN

  900 CALL DANERROR(NUM,IERR)
      CALL CLOSE_DIC(NUM)

  950 RETURN
      END
      SUBROUTINE DANORD_STA_NEW(NUM,IOPT,IFLD,LINE,SKEY,STATUS,IERR,*)

C*  Reads the specified dictionary. Returns key and associated information.
C*  Errors are returned to the caller.
C*    NUM: number of dictionary (I2)
C*    IOPT: record retrieval option (A1); F = first record
C*                                        N = next record
C*    IFLD: field desired (I); 0 = all fields
C*    LINE: field retrieved (A80)
C*    SKEY: primary key for record being retrieved (A20)
C*    STATUS: status code (A3)
C*    IERR: errors (I); 0 = no error
C*    RETURN 1: END-OF-FILE

C---- Units for open dictionaries.  Set in OPEN_NEW.
      COMMON/DANUNITS/NOPEN(1000)

C---- DAN_TOP variables
C-      NKEY: # of keys for dictionary
C-      NFLD: # of fields in code
C----   LENFLD: length of fields in code string
      COMMON/TOPREC/NKEY(1000),NFLD(1000)
      COMMON/TOPSTR/LENFLD(20,1000)

      CHARACTER*110 REC
      CHARACTER*80 LINE
      CHARACTER*30 SKEY
      CHARACTER*3 STATUS,ISTAT
      CHARACTER IOPT,LEGOPT(2)/'F','N'/

C---- TEST FOR LEGAL OPTION
      DO 110 I=1,2
        NOPT=I
        IF (IOPT.EQ.LEGOPT(I)) GOTO120
  110 CONTINUE
      GOTO 820

  120 IF (NOPT.EQ.1) THEN                    ! FIRST RECORD
        CALL OPEN_NEW(NUM,IERR,*950)           ! OPEN DICTIONARY
      END IF

C---- CHECK FIELD NUMBER
      IF (IFLD.LT.0 .OR. IFLD.GT.NFLD(NUM)) THEN
        IERR = 5
        GOTO 900
      END IF

C---- READ DATA INTO STRING/INCLUDING STATUS
c?    CALL READ_SEQ(NOPEN(NUM),REC,ISTAT,IERR,*900,*500)
      CALL READZ_SEQ(NUM,REC,ISTAT,IERR,*900,*500)

      SKEY = REC(1:30)

      IF (IFLD.EQ.0) THEN                    ! ALL FIELDS
        LINE = REC(31:110)                     ! TRANSFER ALL FIELDS
      ELSE
        I1 = 31                                ! INITIALIZE START OF FIELD
        IF (IFLD.GT.1) THEN                    ! FIELD > 1
          DO 350 I = 1,IFLD-1                    ! RESET START OF FIELD
            I1 = I1+LENFLD(I,NUM)
  350     CONTINUE
        END IF
        LINE = ' '                             ! INITIALIZE FIELDS RETURNED
        I2 = I1+LENFLD(IFLD,NUM)-1             ! SET END OF FIELD 
        LNTH = LENFLD(IFLD,NUM)                ! SET FIELD LENGTH
        LINE(1:LNTH) = REC(I1:I2)
      END IF

      STATUS = ISTAT
      RETURN

C---- END-OF-FILE, CLOSE DICTIONARY
  500 CALL CLOSE_DIC(NUM)
      RETURN 1

  820 CALL DANERROR(NUM,16)                  ! ILLEGAL RECORD RETRIEVAL OPTION
      RETURN

  900 CALL DANERROR(NUM,IERR)
      CALL CLOSE_DIC(NUM)

  950 RETURN
      END
      SUBROUTINE DANVER_NEW(NUM,KEY,IKEY,IERR,*)

C*  Verifies that key exists in dictionary; does not return status code
C*    NUM: number of dictionary (I2)
C*    KEY: key for record being retrieved (A*)
C*    IKEY: index of key (I)
C*    IERR: error # (I); 0 = no error   
C*    RETURN 1: MATCH FOUND       

C---- Units for open dictionaries.  Set in OPEN_NEW.
      COMMON/DANUNITS/NOPEN(1000)

C---- DAN_TOP variables
C----   LENFLD: length of fields in code string
      COMMON/TOPSTR/LENFLD(20,1000)

      CHARACTER*(*) KEY
      CHARACTER*3 ISTAT
      CHARACTER*30 SKEY,KEY1
      CHARACTER*80 REC

      SKEY = KEY                            ! RESTORE KEY AS 20 CHARACTERS

      CALL OPEN_NEW(NUM,IERR,*950)  

C---- SET KEY LENGTH
      IF (IKEY.NE.0) THEN
        LNTH = LENFLD(IKEY,NUM)
      ELSE
        LNTH = 30
      END IF

c?    CALL READ_KEY(NOPEN(NUM),SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*950)
      CALL READZ_KEY(NUM,SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*950)

      RETURN 1

C---  CLOSE UP
  950 IF (IERR.NE.20) CALL DANERROR(NUM,IERR)
      RETURN
      END
      SUBROUTINE DANVER_STA(NUM,KEY,IKEY,STATUS,IERR,*)

C*  Verifies that key exists in dictionary; returns status code.
C*  Errors are returned to the caller.
C*    NUM: number of dictionary (I2)
C*    KEY: key for record being retrieved (A*)
C*    IKEY: index of key (I)
C*    STATUS: status code for line (A3)
C*    IERR: error # (I); 0 = no error   
C*    RETURN 1: MATCH FOUND       

C---- Units for open dictionaries.  Set in OPEN_NEW.
      COMMON/DANUNITS/NOPEN(1000)

C---- DAN_TOP variables
C----   LENFLD: length of fields in code string
      COMMON/TOPSTR/LENFLD(20,1000)

      CHARACTER*(*) KEY
      CHARACTER*3 STATUS,ISTAT
      CHARACTER*30 SKEY,KEY1
      CHARACTER*80 REC

      SKEY = KEY                            ! RESTORE KEY AS 20 CHARACTERS

      CALL OPEN_NEW(NUM,IERR,*950)  

C---- SET KEY LENGTH
      IF (IKEY.NE.0) THEN
        LNTH = LENFLD(IKEY,NUM)
      ELSE
        LNTH = 30
      END IF

c?    CALL READ_KEY(NOPEN(NUM),SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*950)
      CALL READZ_KEY(NUM,SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*950)

      STATUS = ISTAT
      RETURN 1

C---  CLOSE UP
  950 IF (IERR.NE.20) CALL DANERROR(NUM,IERR)
      RETURN
      END
      SUBROUTINE DANVER_STA_NEW(NUM,KEY,IKEY,STATUS,IERR,*)

C*  Verifies that key exists in dictionary; returns status code.
C*  Errors are returned to the caller.
C*    NUM: number of dictionary (I2)
C*    KEY: key for record being retrieved (A*)
C*    IKEY: index of key (I)
C*    STATUS: status code for line (A3)
C*    IERR: error # (I); 0 = no error   
C*    RETURN 1: MATCH FOUND       

C---- Units for open dictionaries.  Set in OPEN_NEW.
      COMMON/DANUNITS/NOPEN(1000)

C---- DAN_TOP variables
C----   LENFLD: length of fields in code string
      COMMON/TOPSTR/LENFLD(20,1000)

      CHARACTER*(*) KEY
      CHARACTER*3 STATUS,ISTAT
      CHARACTER*30 SKEY,KEY1
      CHARACTER*80 REC

      SKEY = KEY                            ! RESTORE KEY AS 20 CHARACTERS

      CALL OPEN_NEW(NUM,IERR,*950)  

C---- SET KEY LENGTH
      IF (IKEY.NE.0) THEN
        LNTH = LENFLD(IKEY,NUM)
      ELSE
        LNTH = 30
      END IF

c?    CALL READ_KEY(NOPEN(NUM),SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*950)
      CALL READZ_KEY(NUM,SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*950)

      STATUS = ISTAT
      RETURN 1

C---  CLOSE UP
  950 IF (IERR.NE.20) CALL DANERROR(NUM,IERR)
      RETURN
      END
      SUBROUTINE DANVER_CIN(NUM,KEY,LNTH,IKEY,STATUS,IERR,*)

C*  Reads the specified dictionary using the specified key. 
C*  For use with CINDA retrievals in dictionaries 6 and 7.
C*  Errors are returned to the caller.
C*    NUM: number of dictionary (I2)
C*    KEY: key for record being retrieved (A*)
C*    LNTH: length of key
C*    IKEY: index of key (I)
C*    STATUS: status code for line (A3)
C*    IERR: error # (I); 0 = no error   
C*    RETURN 1: MATCH FOUND       

C---- Units for open dictionaries.  Set in OPEN_NEW.
      COMMON/DANUNITS/NOPEN(1000)

C---- DAN_TOP variables
C----   LENFLD: length of fields in code string
      COMMON/TOPSTR/LENFLD(20,1000)

      CHARACTER*(*) KEY
      CHARACTER*3 STATUS
      CHARACTER*30 KEY1
      CHARACTER*80 REC

      CALL OPEN_NEW(NUM,IERR,*950)  

c?    CALL READ_KEY(NOPEN(NUM),KEY,LNTH,IKEY,KEY1,REC,STATUS,IERR,*950)
      CALL READZ_KEY(NUM,KEY,LNTH,IKEY,KEY1,REC,STATUS,IERR,*950)
      RETURN 1

  950 IF (IERR.NE.20) CALL DANERROR(NUM,IERR)
      RETURN
      END
      SUBROUTINE DANVER_CIN_NEW(NUM,KEY,LNTH,IKEY,STATUS,IERR,*)

C*  Reads the specified dictionary using the specified key. 
C*  For use with CINDA retrievals in dictionaries 6 and 7.
C*  Errors are returned to the caller.
C*    NUM: number of dictionary (I2)
C*    KEY: key for record being retrieved (A*)
C*    LNTH: length of key
C*    IKEY: index of key (I)
C*    STATUS: status code for line (A3)
C*    IERR: error # (I); 0 = no error   
C*    RETURN 1: MATCH FOUND       

C---- Units for open dictionaries.  Set in OPEN_NEW.
      COMMON/DANUNITS/NOPEN(1000)

C---- DAN_TOP variables
C----   LENFLD: length of fields in code string
      COMMON/TOPSTR/LENFLD(20,1000)

      CHARACTER*(*) KEY
      CHARACTER*3 STATUS
      CHARACTER*30 KEY1
      CHARACTER*80 REC

      CALL OPEN_NEW(NUM,IERR,*950)  

c?    CALL READ_KEY(NOPEN(NUM),KEY,LNTH,IKEY,KEY1,REC,STATUS,IERR,*950)
      CALL READZ_KEY(NUM,KEY,LNTH,IKEY,KEY1,REC,STATUS,IERR,*950)
      RETURN 1

  950 IF (IERR.NE.20) CALL DANERROR(NUM,IERR)
      RETURN
      END
      SUBROUTINE GET_PRIME_KEY(NUM,KEY,IKEY,KEY1,IERR)

C*  Returns primary key for secondary key specified
C*    NUM: dictionary number (I2)
C*    KEY: key for record being retrieved (A*)
C*    IKEY: key ID (I); 0 = 1st key
C*    KEY1: primary key for record being retrieved (A*)
C*    IERR: error flag (return) (I); 0 = no error   

C---- Units for open dictionaries.  Set in OPEN_NEW.
      COMMON/DANUNITS/NOPEN(1000)

C---- DAN_TOP variables
C-      NKEY: # of keys for dictionary
C-      NFLD: # of fields in code
C----   LENFLD: length of fields in code string
      COMMON/TOPREC/NKEY(1000),NFLD(1000)
      COMMON/TOPSTR/LENFLD(20,1000)

      CHARACTER*(*) KEY
      CHARACTER*3 ISTAT
      CHARACTER*30 SKEY,KEY1
      CHARACTER*80 REC

      SKEY = KEY                            ! Restore key as 20 chars.

      CALL OPEN_NEW(NUM,IERR,*950)          ! Open dictionary

C---  CHECK KEY NUMBER (MAX. 3)
      IF (IKEY.LT.1 .OR. IKEY.GT.NKEY(NUM)-1) THEN
        IERR = 13
        GOTO 800
      END IF

C---- SET KEY LENGTH
      LNTH = LENFLD(IKEY,NUM)

C---- READ DICTIONARY RECORD
c?    CALL READ_KEY(NOPEN(NUM),SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*800)
      CALL READZ_KEY(NUM,SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*800)
      RETURN

C---- WRITE ERROR MESSAGE, EXCEPT FOR KEY NOT FOUND
  800 IF(IERR.NE.20) CALL DANERROR(NUM,IERR)

  950 RETURN
      END
      SUBROUTINE GET_PRIME_KEY_NEW(NUM,KEY,IKEY,KEY1,IERR)

C*  Returns primary key for secondary key specified
C*    NUM: dictionary number (I2)
C*    KEY: key for record being retrieved (A*)
C*    IKEY: key ID (I); 0 = 1st key
C*    KEY1: primary key for record being retrieved (A*)
C*    IERR: error flag (return) (I); 0 = no error   

C---- Units for open dictionaries.  Set in OPEN_NEW.
      COMMON/DANUNITS/NOPEN(1000)

C---- DAN_TOP variables
C-      NKEY: # of keys for dictionary
C-      NFLD: # of fields in code
C----   LENFLD: length of fields in code string
      COMMON/TOPREC/NKEY(1000),NFLD(1000)
      COMMON/TOPSTR/LENFLD(20,1000)

      CHARACTER*(*) KEY
      CHARACTER*3 ISTAT
      CHARACTER*30 SKEY,KEY1
      CHARACTER*80 REC

      SKEY = KEY                            ! Restore key as 20 chars.

      CALL OPEN_NEW(NUM,IERR,*950)          ! Open dictionary

C---  CHECK KEY NUMBER (MAX. 3)
      IF (IKEY.LT.1 .OR. IKEY.GT.NKEY(NUM)-1) THEN
        IERR = 13
        GOTO 800
      END IF

C---- SET KEY LENGTH
      LNTH = LENFLD(IKEY,NUM)

C---- READ DICTIONARY RECORD
c?    CALL READ_KEY(NOPEN(NUM),SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*800)
      CALL READZ_KEY(NUM,SKEY,LNTH,IKEY,KEY1,REC,ISTAT,IERR,*800)
      RETURN

C---- WRITE ERROR MESSAGE, EXCEPT FOR KEY NOT FOUND
  800 IF(IERR.NE.20) CALL DANERROR(NUM,IERR)

  950 RETURN
      END
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

!     IF (NDIC.EQ.0) THEN
      IF ((NDIC.EQ.0).OR.(NDIC.GT.20)) THEN
        write (*,2000) MESS(N),NDIC
      ELSE
        write (*,1000) MESS(N),NDIC
      END IF
      RETURN

 1000 FORMAT(' **** ',A35,' for dictionary ',I3)
 2000 FORMAT(' **** ',A35)
      END
