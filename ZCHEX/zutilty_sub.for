C*    UTILTY_SUB: Package of subroutines for processing CSISRS data.
C**
C* Version 98-1 (November 1998)
C*   Added subroutine PRETTY_DATE
C* Version 97-1 (July 1997)
C*   Updated DATER to use Year 2000 date subroutines
C* Version 95-1 (January 1995)
C*   Added subroutine BACK_ANSAN
C* Version 90-1 (March 1990)
C**
C* Written by Victoria McLane
C*            National Nuclear Data Center
C*            Brookhaven National Laboratory
C*            Upton, NY 11973-5000
C**
C* The following subroutines are included:
C*    ABUND_SUB: Finds natural abundance for specified isotope
C*    AWEIGHT: Finds atomic weight for natural element specified
C*    BACK_ANSAN: Converts integer subaccession number to hollerith 
C*                equivalent.
C*    BAKOUT: Converts integer accession number to hollerith 
C*            equivalent.
C*    DATER:  Returns character and integer dates
C*    FILHOL: Fills hollerith array with hollerith string specified
C*    FILLIN: Fills given array with integer specified
C*    GET_NAME: Finds file name from input specification string.
C*    GET1:   Reads record in EXFOR format.  Branch return for end-of-file.
C*    INCRMT: Increments integer specified up to maximum value specified
C*            Branch return if maximum exceeded.
C*    INCRMT_NEW: Increments integer specified up to maximum value specified
C*            Branch return if maximum exceeded.
C*    PARSCHR: Searches character string for delimiters specified.
C*            Returns index of delimiter found and position in string.
C*            Branch return for delimiter not found.
C*    PARS_CHR: Searches character string for delimiters specified.
C*            Returns index of delimiter found and # of characters in string. 
C*            Branch return for delimiter not found.
C*    PRETTY_DATE: Converts integer date to character: dd-mon-yyyy.
C*            If date input to routine has 2-digit year, insets '19' at 
C*            beginning of input string.
C*    PUT1: Writes record in EXFOR format.
C*    RESTORE: Copies array specified into second array.
C*    S_FROM_Z: finds Z equivalent for element symbol
C*    SEARCH_END: Searches character string for delimiters specified.
C*            Returns index of delimiter found and # of characters in string.
C*    SPIN_SUB: Finds spin for isotope specified.
C**
C* The following functions are included:
C*    INT_FORM: Returns integer equivalent of character string specified
C*            For CSISRS AN/SAN returns character equivalent for 1st digit.
C*            # of digits returned.
C*    INTFORM: Returns integer equivalent of character string specified
C*            For CSISRS AN/SAN returns character equivalent for 1st 
C*            digit. # of digits not returned.
C*    LOOKIN: Looks up integer specified in array given.
C*            Returns index of match in array.
C*    LOOKUP: Looks up character string specified in array given.
C*            Returns index of match in array.
C*    MAX_SIZE: Returns maximum value of: integer incremented by value given,
C*            or maximum value allowed.
C*    Z_FROM_S: finds element symbol from Z-number   

      SUBROUTINE ABUND_SUB(ISOTPE,ABUND,*)

C*  Finds natural abundance for a given isotope
C*    ISOTPE: Natural isotope (Input: ZZZ-SS-AAA)
C*    ABUND: Abundance (Return: E)
C*    RETURN 1: Abundance not found

      CHARACTER*(*) ISOTPE
      CHARACTER*80 LINE

   10 FORMAT(E11.4)
   20 FORMAT(' ** ABUNDANCE NOT FOUND FOR: ',A)
      CALL DANGET_new(27,ISOTPE,1,5,LINE,IERR)

      IF (IERR.NE.0) THEN
        write (*,20) ISOTPE
        ABUND = 0.
        RETURN 1
      END IF

      READ(LINE(1:11),10) ABUND

      RETURN
      END
      SUBROUTINE AWEIGHT(ELEMNT,AWGHT,*)

C*  Finds atomic weight for natural element
C*    ELEMNT: Natural element (Input: ZZZ-SS-0)
C*    AWGHT: Atomic weight (Return: E)
C*    RETURN 1: Atomic weight not found

      CHARACTER*(*) ELEMNT
      CHARACTER*80 LINE

   10 FORMAT(E11.4)
   20 FORMAT(' ** ATOMIC WEIGHT NOT FOUND FOR: ',A)

      CALL DANGET_new(27,ELEMNT,1,5,LINE,IERR)

      IF (IERR.NE.0) THEN
        write (*,20) ELEMNT
        AWGHT = 0.
        RETURN 1
      END IF

      READ(LINE(1:11),10) AWGHT

      RETURN
      END
      SUBROUTINE BACK_ANSAN(IFIX,ANSAN)

C*  Converts integer subaccession # to hollerith equivalent.
C*    IFIX: Integer subaccession # (Input)
C*    ANSAN: Hollerith subaccession # (Returned)

      CHARACTER IHOL(36)
      CHARACTER*8 ANSAN
      CHARACTER*9 SETAN

      DATA IHOL/'0','1','2','3','4','5','6','7','8','9',
     *          'A','B','C','D','E','F','G','H','I','J',
     *          'K','L','M','N','O','P','Q','R','S','T',
     *          'U','V','W','X','Y','Z'/

      ANSAN = '00000000'           ! INITIALIZE HOLLERITH EQUIVALENT

      IREM = IFIX/10000000         ! GET FIRST DIGIT
      ANSAN(1:1) = IHOL(IREM+1)    ! STORE HOLLERITH EQUIVALENT

      WRITE(SETAN,1000) IFIX
      ANSAN(2:8) = SETAN(3:9)

 1000 FORMAT(I9)
      END
      SUBROUTINE BAKOUT(IFIX,OUTAN)

C*  Converts integer accession # to hollerith equivalent.
C*    IFIX: Integer (Input: I)
C*    OUTAN: Accession # (Returned: A5)

      CHARACTER IHOL(36)
      CHARACTER*5 OUTAN
      CHARACTER*6 SETAN

      DATA IHOL/'0','1','2','3','4','5','6','7','8','9',
     *          'A','B','C','D','E','F','G','H','I','J',
     *          'K','L','M','N','O','P','Q','R','S','T',
     *          'U','V','W','X','Y','Z'/

      OUTAN = '00000'              ! CLEAR HOLLERITH EQUIVALENT

      IREM = IFIX/10000            ! GET FIRST DIGIT
      OUTAN(1:1) = IHOL(IREM+1)    ! STORE HOLLERITH EQUIVALENT

      WRITE(SETAN,1000) IFIX
      OUTAN(2:5) = SETAN(3:6)

C---- BLANK-FILL TO FIRST NON-ZERO
      DO 60 K = 1,4
        IF(OUTAN(K:K).NE.'0') GOTO 80
        OUTAN(K:K) = ' '
   60 CONTINUE
   80 RETURN

 1000 FORMAT(I6)
      END
      SUBROUTINE DATER(TODAY,IYMD)

C*  Stores date
C*    TODAY: Hollerith date (Returned: DD-Mon-YYYY)
C*    IYMD: Integer date (Returned: YYYYMMDD)

      CHARACTER*11 TODAY
      INTEGER IYMD

C---- READ HOLLERITH DATE.
      CALL DATE_20(TODAY)
      CALL LOSTR(TODAY(5:6))

C---- READ INTEGER DATE. STORE AS CHARACTERS.
      CALL IDATE_20(IM,ID,IY,IYMD)
      RETURN
      END
      SUBROUTINE FILHOL(HKARD,N,HFILL)

C*  Fills array specified with character string given
C*    HKARD: Character array (Input/Return)
C*    N: # of words in array (Input)
C*    HFILL: Character string to be indserted (Input)

      CHARACTER*(*) HKARD(N), HFILL

      DO 210 I = 1,N
        HKARD(I) = HFILL
  210 CONTINUE
      RETURN
      END
      SUBROUTINE FILLIN(KARD,N,IFILL)

C*  Fills integer array specified with integer given
C*    KARD: Integer array (Input/Return)
C*    N: # of words in array (Input)
C*    IFILL: Integer to be stored (Input)

      INTEGER KARD(N),IFILL

      DO 110 I = 1,N
        KARD(I) = IFILL
  110 CONTINUE
      RETURN
      END
      FUNCTION FLOATIT(STRING,N,IGOOF)

C*    Converts character string to floating point number
C*      STRING: Character string (Input)
C*      N: Length of string (Input)
C*      IGOOF: Error indicator (Returned)
C*             1 = Trailing blanks after exponent
C*             2 = Illegal character within string
C*             3 = No decimal point

      CHARACTER*(*) STRING

      IGOOF = 0                ! INITIALIZE ERROR FLAG
      NDEC = 0                 ! INITIALIZE DECIMAL POINT NOT FOUND
      NEXP = 0                 ! INITIALIZE EXPONENT NOT FOUND

      READ(STRING,1000,ERR=900) REALNUM

      DO 120 I = 1,N           ! FIND START OF FIELD
        LOW = I
        IF(STRING(I:I).NE.' ') GOTO 130
  120 CONTINUE
      GOTO 990

  130 DO 140 I = LOW,N         ! FIND END OF FIELD
        LST = I
        IF (STRING(I:I).EQ.'.') THEN
          NDEC = NDEC+1
        ELSE IF (STRING(I:I).EQ.'E') THEN
          NEXP = NEXP+1
        ELSE IF (STRING(I:I).EQ.' ') THEN
          GOTO 150
        END IF
  140 CONTINUE
      GOTO 700

  150 IF(NEXP.NE.0) GOTO 850

      DO 170 I = LST,N           ! CHECK FOR ALL BLANKS OR EXPONENT
        NOW = I
        IF(STRING(I:I).EQ.'E') GOTO 200
        IF(STRING(I:I).EQ.'-' .OR. STRING(I:I).EQ.'+') GOTO 200
        IF(STRING(I:I).NE.' ') GOTO 900
  170 CONTINUE
      GOTO 700

  200 DO 220 I = NOW+1,N       ! CHECK FOR TRAILING BLANKS
        IF(STRING(I:I).EQ.' ') GOTO 850
  220 CONTINUE

  700 IF (NDEC.NE.1) THEN
        IGOOF = 3
        GOTO 990
      END IF

      FLOATIT = REALNUM
      RETURN

  850 IGOOF = 1
      GOTO 990

  900 IGOOF = 2

  990 FLOATIT = 0.0
      RETURN

 1000 FORMAT(E13.7)
      END
      INTEGER FUNCTION FORM_INT(IFIELD,N,IDIG,IGOOF,IUPTO)

C*    Converts character string to integer value.
C*      Input to routine:
C*          IFIELD: Character string
C*          N: # of characters
C*      Output from routine
C*          IDIG: # of digits in integer
C*          IGOOF: error indicator
C*                 1 = Character count in error
C*                 2 = Embedded/trailing blanks
C*                 3 = Illegal character within string
C*          IUPTO: # of characters to be searched
C*                 10 = Numeric characters only
C*                 36 = Alphanumeric characters
C*                      for 1st digit of integer in EXFOR AN, SAN

      CHARACTER*(*) IFIELD
      CHARACTER*1 IHOL(36)

      DATA IHOL/'0','1','2','3','4','5','6','7','8','9',
     *          'A','B','C','D','E','F','G','H','I','J',
     *          'K','L','M','N','O','P','Q','R','S','T',
     *          'U','V','W','X','Y','Z'/

C---- CLEAR INDICATORS AND FIELDS
      IGOOF = 0
      ISIGN = 1
      IDIG = 0
      IFIX = 0
      NOBL = 0
      IF (N.LE.0 .OR. N.GT.11) THEN
        IGOOF = 1                      ! SET ERROR
        GOTO 150
      END IF

      DO 30 K=1,N
        IF (IFIELD(K:K).EQ.' ') THEN     ! BLANK CHARACTER
          IF (IDIG.NE.0) THEN              ! TRAILING BLANKS  
            IGOOF = 2                        ! MUST BE RIGHT-ADJUSTED
            GOTO 150
          END IF
          NOBL = NOBL+1                        ! INCREMENT # OF BLANKS
        ELSE IF (IFIELD(K:K).EQ.'-') THEN    ! '-' CHARACTER
          IF (ISIGN.EQ.-1) THEN                ! '-' ALREADY FOUND
            IGOOF = 3                            ! SET ERROR
            GOTO 150
          ELSE
            ISIGN = -1                         ! SET FLAG TO FOUND
          END IF
        ELSE
          J = LOOKUP(IFIELD(K:K),IHOL,IUPTO)     ! LOOK UP HOLLERITH
          IF (J.GT.0) THEN                     ! MATCH; FORM NUMBER
            IFIX = IFIX*10+(J-1)
            IDIG = IDIG+1                          ! INCREMENT DIGIT COUNT
          END IF
        END IF
   30 CONTINUE

      IFIX = IFIX*ISIGN
      INCD = 0
      IF(ISIGN.NE.1) INCD = 1
      IF ((NOBL+IDIG+INCD).NE.N) THEN
        IGOOF = 4
        GOTO 150
      END IF

      FORM_INT = IFIX                  ! SET INTEGER
      RETURN

C---- ERROR
  150 FORM_INT = 0                     ! CLEAR RESULT
      IDIG = 0
      RETURN
      END
      SUBROUTINE GET_NAME(ISTR,N,FNAME)

C*  Gets file name from file specification string
C*    ISTR: file specification string (Input)
C*    N: length of string (Input)
C*    FNAME: file name (Output: A6)

      CHARACTER*(*) ISTR
      CHARACTER*6 FNAME

      FNAME = ' '
      LOW = 1

      DO 200 I = 1,N
        IF(ISTR(I:I).EQ.':' .OR. ISTR(I:I).EQ.']') LOW = I+1
        IF(ISTR(I:I).EQ.'.') GOTO 250
        LAST = I
  200 CONTINUE

  250 NUM = LAST-LOW
      IF (NUM.GT.5) THEN    ! MAX. SIZE = 6 CHARACTERS
        NUM = 5
        LAST = LOW+5
      END IF

      FNAME(1:1+NUM) = ISTR(LOW:LAST)

      RETURN
      END
      SUBROUTINE GET1(IN,CARD,AN,ISAN,ISEQ,ALT,IEND,*)

C*  Reads a record from an EXFOR file and checks for ENDTRANS.
C*    IN: input unit # (Input)
C*    CARD: Input record (Returned)
C*    AN : Accession # (Returned)
C*    ISAN: Subaccession # (Returned)
C*    ISEQ: Sequence # (Returned)
C*    ALT: Alteration flag (Returned)
C*    IEND: Logical end-of-file for input file
C*          1 = ENDTRANS (or equivalent) found
C*          2 = end-of-file found
C*    RETURN 1: end-of-file 

      COMMON/GET_COUNT/ICOUNT

      CHARACTER*66 CARD
      CHARACTER*5 AN
      CHARACTER ALT

      DATA ICOUNT/0/

      READ(IN,1000,END=200) CARD,AN,ISAN,ISEQ,ALT ! READ RECORD FROM SOURCE
      ICOUNT = ICOUNT+1
      IF (AN(2:5).EQ.'9999') IEND = 1
      RETURN

C---- END-OF-FILE FOUND. SET LOGICAL END OF FILE ON.
  200 IEND = 2
      RETURN 1

 1000 FORMAT(A66,A5,I3,I5,A1)
      END
      SUBROUTINE INCRMT(N,MAX,*)

C*  Increments variable, tests against maximum value allowed. If maximum
C*  value exceeded, original value is returned.
C*    N: Variable (Input/Return)
C*    MAX: Maximum value allowed (Input)
C*    RETURN 1: Maximum value exceeded

      IF(N.GE.MAX) RETURN 1
      N = N+1
      RETURN
      END
      SUBROUTINE INCRMT_NEW(NUM,IADD,MAX,*)

C*  Returns integer incremented by value given. Tests against maximum
C*  value allowed. If maximum value exceeded, original value is
C*  returned.
C*    NUM: Integer (Input/Return)
C*    IADD: Integer to be added (Input)
C*    MAX: Maximum value allowed (Input)
C*    RETURN 1: Maximum value exceeded

      IF ((NUM+IADD).GT.MAX) RETURN 1
      NUM = NUM+IADD

      RETURN
      END
      INTEGER FUNCTION INT_FORM(IFIELD,N,IDIG,IGOOF,LIMIT)

C*    Converts character string to integer value.
C*      Input to routine:
C*          IFIELD: Character string
C*          N: # of characters
C*      Output from routine
C*          IDIG: # of digits in integer
C*          IGOOF: error indicator
C*                 1 = Character count negative
C*                 2 = Embedded/trailing blanks
C*                 3 = Illegal character within string
C*          LIMIT: Index to characters to be searched
C*                 1 = Numeric characters only
C*                 2 = Alphanumeric character on 1st digit of integer (AN,SAN)

      DIMENSION LIMHOL(2)
      CHARACTER*(*) IFIELD
      CHARACTER*1 IHOL(36)

      DATA LIMHOL/10,36/
      DATA IHOL/'0','1','2','3','4','5','6','7','8','9',
     *          'A','B','C','D','E','F','G','H','I','J',
     *          'K','L','M','N','O','P','Q','R','S','T',
     *          'U','V','W','X','Y','Z'/

C---- CLEAR INDICATORS AND FIELDS
      IGOOF = 0
      ISIGN = 1
      IDIG = 0
      IFIX = 0
      IUPTO = LIMHOL(LIMIT)
      NOBL = 0
      IF (N.LE.0) THEN
        IGOOF = 1                      ! SET ERROR
        GOTO 150
      END IF

      DO 30 K=1,N
        IF (IFIELD(K:K).EQ.' ') THEN     ! BLANK CHARACTER
          IF (IDIG.NE.0) THEN              ! TRAILING BLANKS  
            IGOOF = 2                        ! MUST BE RIGHT-ADJUSTED
            GOTO 150
          END IF
          NOBL = NOBL+1                        ! INCREMENT # OF BLANKS
        ELSE IF (IFIELD(K:K).EQ.'-') THEN    ! '-' CHARACTER
          IF (ISIGN.EQ.-1) THEN                ! '-' ALREADY FOUND
            IGOOF = 3                            ! SET ERROR
            GOTO 150
          ELSE
            ISIGN = -1                         ! SET FLAG TO FOUND
          END IF
        ELSE
          J = LOOKUP(IFIELD(K:K),IHOL,IUPTO)     ! LOOK UP HOLLERITH
          IF (J.GT.0) THEN                     ! MATCH; FORM NUMBER
            IFIX = IFIX*10+(J-1)
            IDIG = IDIG+1                          ! INCREMENT DIGIT COUNT
          END IF
        END IF
   30 CONTINUE

      IFIX = IFIX*ISIGN
      INCD = 0
      IF(ISIGN.NE.1) INCD = 1
      IF ((NOBL+IDIG+INCD).NE.N) THEN
        IGOOF = 4
        GOTO 150
      END IF

      INT_FORM = IFIX                  ! SET INTEGER
      RETURN

C---- ERROR
  150 INT_FORM = 0                     ! CLEAR RESULT
      IDIG = 0
      RETURN
      END
      INTEGER FUNCTION INTFORM(IFIELD,NCHR,IGOOF,LIMIT)

C*    CONVERTS CHARACTER STRING TO AN INTEGER
C*      INPUT TO ROUTINE:
C*          IFIELD: INPUT CHARACTER STRING
C*          N: NUMBER OF CHARACTERS
C*      OUTPUT OF ROUTINE
C*          IGOOF: ERROR INDICATOR
C*                 1 = CHARACTER COUNT NEGATIVE
C*                 2 = EMBEDDED BLANKS
C*                 3 = ILLEGAL CHARACTER WITHIN STRING
C*                 4 = TRAILING BLANKS
C*          LIMIT: INDEX TO # OF INTEGERS TO SEARCH THROUGH
C*                 1 = NUMERIC CHARACTERS ONLY
C*                 2 = ALPHANUMERIC CHAR. ON FIRST DIGIT OF INTEGER (AN, SAN)

      DIMENSION LIMHOL(2)
      CHARACTER*(*) IFIELD
      CHARACTER*1 IHOL(36)

      DATA LIMHOL/10,36/
      DATA IHOL/'0','1','2','3','4','5','6','7','8','9',
     *          'A','B','C','D','E','F','G','H','I','J',
     *          'K','L','M','N','O','P','Q','R','S','T',
     *          'U','V','W','X','Y','Z'/

C---- CLEAR INDICATORS AND FIELDS
      N = NCHR
      IGOOF = 0
      ISIGN = 1
      IDIG = 0
      IFIX = 0
      IUPTO = LIMHOL(LIMIT)
      NOBL = 0
      IF (N.LE.0) THEN
        IGOOF = 1                      ! SET ERROR
        GOTO 150
      END IF

C---- CUT OFF TRAILING BLANKS
      DO 10 K=NCHR,1,-1
        IF(IFIELD(K:K).NE.' ') GOTO 20
        N = N-1
        IGOOF = 4                        ! SET FLAG, TRAILING BLANKS
   10 CONTINUE
      IGOOF = 1                          ! BLANK FIELD
      GOTO 150

   20 DO 30 K=1,N
        IF (IFIELD(K:K).EQ.' ') THEN     ! BLANK CHARACTER
          IF (IDIG.NE.0) THEN              ! TRAILING BLANKS  
            IGOOF = 2                        ! MUST BE RIGHT-ADJUSTED
            GOTO 150
          END IF
          NOBL = NOBL+1                        ! INCREMENT # OF BLANKS
        ELSE IF (IFIELD(K:K).EQ.'-') THEN    ! '-' CHARACTER
          IF (ISIGN.EQ.-1) THEN                ! '-' ALREADY FOUND
            IGOOF = 3                            ! SET ERROR
            GOTO 150
          ELSE
            ISIGN = -1                         ! SET FLAG TO FOUND
          END IF
        ELSE
          J = LOOKUP(IFIELD(K:K),IHOL,IUPTO)     ! LOOK UP HOLLERITH
          IF (J.GT.0) THEN                     ! MATCH; FORM NUMBER
            IFIX = IFIX*10+(J-1)
            IDIG = IDIG+1                          ! INCREMENT DIGIT COUNT
          END IF
        END IF
   30 CONTINUE

      IFIX = IFIX*ISIGN
      INCD = 0
      IF(ISIGN.NE.1) INCD = 1
      IF ((NOBL+IDIG+INCD).NE.N) THEN
        IGOOF = 4
        GOTO 150
      END IF

      INTFORM = IFIX                  ! SET INTEGER
      RETURN

C---- ERROR
  150 INTFORM = 0                     ! CLEAR RESULT
      RETURN
      END
      FUNCTION LOOKIN(KARD,N,TABLE,NTAB)

C*    Looks up code given in integer table given.
C*    Returns index of match in table; if no match, index is set to 0.
C*      KARD: Integer array (Input)
C*      N: # of words in array (Input)
C*      TABLE: Integer array table (Input)
C*      NTAB: # of entries in table

      INTEGER KARD(N),TABLE(N,NTAB)

      DO 200 K = 1,NTAB                ! LOOP OVER # OF ENTRIES IN TABLE

        DO 150 J = 1,N                   ! LOOP OVER # OF WORDS IN ARRAY
          IF(KARD(J).NE.TABLE(J,K))GOTO 200
  150   CONTINUE

C----   MATCH.
        LOOKIN = K
        RETURN

  200 CONTINUE

C---- NO MATCH.
      LOOKIN = 0
      RETURN
      END
      INTEGER FUNCTION LOOKUP(KARD,TABLE,NTAB)

C*    Looks up character code in data table.
C*    Returns index of match in table; if not match, index is set to 0.
C*      KARD: Character code (Input)
C*      TABLE: Data talbe (Input)
C*      NTAB: # of entries in data table (Input)

      CHARACTER*(*) KARD,TABLE(NTAB)

      DO 200 K = 1,NTAB              ! LOOP OVER # OF ENTRIES IN TABLE
        IF(KARD.NE.TABLE(K)) GOTO 200
        LOOKUP = K                     ! MATCH FOUND
        RETURN
  200 CONTINUE

      LOOKUP = 0                     ! NO MATCH
      RETURN
      END
      INTEGER FUNCTION MAX_SIZE(NUM,IADD,MAX)

C*    Returns maximum value of:
C*        Integer incremented by value given
C*        or maximum value allowed.
C*      NUM: Integer (Input)
C*      IADD: Integer to be added (Input)
C*      MAX: Maximum value allowed (Input)

      N = NUM+IADD

      IF (N.GT.MAX) THEN
        MAX_SIZE = MAX

      ELSE
        MAX_SIZE = N
      END IF
      RETURN
      END
      SUBROUTINE PARSCHR(KARD,LOW,LAST,N,MAX,LIMTR,NLIM,ICHR,*)

C*  Scans field for terminator
C*    KARD: Array to be scanned (Input)
C*    LOW: 1st character in string to be scanned (Input)
C*    LAST: Last character before delimiter (Returned)
C*    N: # of characters in substring (Returned)
C*    MAX: Maximum # of characters in substring (Input)
C*    LIMTR: Substring delimiters (Input)
C*    NLIM: # of substring delimiters (Input)
C*    ICHR: Index of delimiter found (Returned): initialized to 0.
C*    RETURN 1: Delimiter not found

      CHARACTER*(*) KARD
      CHARACTER LIMTR(NLIM)

      N = 0
      LAST = LOW-1            ! INITIALIZE LAST POSITION

C---- SEARCH FOR DELIMITERS
      DO 200 I = LOW,55
        DO 180 K = 1,NLIM
          IF(KARD(I:I).NE.LIMTR(K)) GOTO 180
          ICHR = K
          RETURN
  180   CONTINUE

        N = N+1
        IF(N.GT.MAX) RETURN 1
        LAST = I
  200 CONTINUE
      RETURN 1
      END
      SUBROUTINE PARS_CHR(KARD,MAX,LIMTR,NLIM,N,ICHR,*)

C*  Scans field for terminator
C*    KARD: Array to be scanned (Input)
C*    MAX: Maximum # of characters in substring (Input)
C*    LIMTR: Field delimiters (Input)
C*    NLIM: # of delimiters (Input)  
C*    N: # of characters in substring (Returned)
C*    ICHR: index of delimiter found (Returned): initialized to 0
C*    RETURN 1: Delimiter not found

      CHARACTER*(*) KARD
      CHARACTER LIMTR(NLIM)

      N = 0
      ICHR = 0

      DO 200 I = 1,MAX               ! SEARCH TO END OF STRING

        DO 180 K = 1,NLIM              ! SEARCH THROUGH DELIMITERS
          IF(KARD(I:I).EQ.LIMTR(K)) THEN ! DELIMITER FOUND
            ICHR = K                       ! SET INDEX
            RETURN
          END IF
  180   CONTINUE

        N = I                          ! STORE LAST POSITION
  200 CONTINUE

      RETURN 1
      END
      SUBROUTINE PRETTY_DATE(IYMD,DAY,LOWCHR)

C*  Converts integer date to character: dd-mon-yyyy.  If date input to
C*  routine has 2-digit year, insets '19' at beginning of input string.
C*    IYMD: date in integer form (Input: A8)
C*    DAY: character date (Returned: A11)
C*    LOWCHR: first position of character stored in output date (Returned)

      CHARACTER*11 DAY
      CHARACTER*3 MONTH(12)/'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *                      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
      CHARACTER*8 IYMD

   10 FORMAT(I2)

      DAY = ' '                             ! Initialize date.

C---- Construct 4-digit year, if necessary.

      IF (IYMD(1:1).GE.'3') THEN            
        IYMD(3:8) = IYMD(1:6)
        IYMD(1:2) = '19'
      END IF

C---- Store year.
      DAY(8:11) = IYMD(1:4)                 ! store year
      LOWCHR = 8

      LOW = 5

      IF (IYMD(LOW:LOW+1).EQ.'00' .OR. IYMD(LOW:LOW+1).EQ.' ') THEN 
        LOW = LOW+4                    
        RETURN                             ! No month stored
      END IF

C---- Store character month.
      DAY(7:7) = ' '
      READ(IYMD(LOW:LOW+1),10) MN
      DAY(4:6) = MONTH(MN)
      LOWCHR = 4

      LOW = LOW+2

      IF (IYMD(LOW:LOW+1).EQ.'00' .OR. IYMD(LOW:LOW+1).EQ.' ') THEN
        LOW = LOW+2 
        RETURN                             ! No day stored
      END IF

C---- Store day without leading blanks.
      DAY(3:3) = ' '
      IF (IYMD(LOW:LOW).EQ.'0') THEN
        DAY(2:2) = IYMD(LOW+1:LOW+1)
        LOWCHR = 2
      ELSE
        DAY(1:2) = IYMD(LOW:LOW+1)
        LOWCHR = 1
      END IF

      RETURN
      END
      SUBROUTINE PUT1(IOUT,CARD,AN,ISAN,ISEQ,ALT)

C*    Writes record onto output file in EXFOR format
C*      IOUT: Output unit # (Input)
C*      CARD: output record (Input)
C*      AN : Accession # (Input)
C*      ISAN: Subaccession # (Input)
C*      ISEQ: Sequence # (Input)
C*      ALT: Alteration flag (Input)

      COMMON/PUT_COUNT/ICOUNT    ! COUNT OF RECORD WRITTEN

      CHARACTER*66 CARD
      CHARACTER*5 AN
      CHARACTER ALT

      DATA ICOUNT/0/

      WRITE(IOUT,1000) CARD,AN,ISAN,ISEQ,ALT ! WRITE RECORD ON OUTPUT
      ICOUNT = ICOUNT+1

      RETURN

 1000 FORMAT(A66,A5,I3,I5,A1)
      END
      SUBROUTINE RESTORE(CARD1,N,CARD2)

C*    Moves contents of one array into a second array
C*      KARD1: Ouput array (Returned)
C*      N: Array dimension (Input)
C*      KARD2: Input array (Input)

      DIMENSION CARD1(N),CARD2(N)

      DO 200 I = 1,N
        CARD1(I) = CARD2(I)
  200 CONTINUE
      RETURN
      END
      SUBROUTINE S_FROM_Z(IZ,SYM,IERR)

C*    Gets element symbol for given z number
C*      IZ: Z-Number (Input)
C*      SYM: Element symbol (Returned)
C*      IERR: Error flag (Returned): 0 = no error

      CHARACTER*80 LINE
      CHARACTER*20 ZNUM/' '/
      CHARACTER*2 SYM

   10 FORMAT(I3)
   20 FORMAT(' ** SYMBOL NOT FOUND FOR Z = ',A20)

      WRITE(ZNUM(1:3),10) IZ
      CALL DANGET_new(8,ZNUM,0,1,LINE,IERR)
      SYM = LINE(1:2)

      IF(IERR.NE.0) write (*,20) ZNUM
      RETURN
      END
      SUBROUTINE SEARCH_END(KARD,MAX,LIMTR,NLIM,N,ICHR)

C*    Scans field for terminator
C*      KARD: Array to be scanned (Input)
C*      MAX: Maximum # of characters in substring (Input)
C*      LIMTR: Field delimiters (Input)
C*      NLIM: # of delimiters (Input)
C*      N: # of characters in substring (Output): Initialized to 0
C*      ICHR: Character index found (Output): Initialized to 0

      CHARACTER*(*) KARD
      CHARACTER LIMTR(NLIM)

      N = 0
      ICHR = 0

      DO 200 I = 1,MAX               ! SEARCH TO END OF STRING
        DO 180 K = 1,NLIM              ! SEARCH THROUGH DELIMITERS
          IF(KARD(I:I).EQ.LIMTR(K)) THEN ! DELIMITER FOUND
            ICHR = K                       ! SET INDEX
            RETURN
          END IF
  180   CONTINUE
        N = I                          ! STORE LAST POSITION
  200 CONTINUE

      RETURN
      END
      SUBROUTINE SPIN_SUB(ISOTPE,SPIN,*)

C*    Finds spin for a given isotope
C*      ISOTPE: Natural isotope (Input: ZZZ-SS-AAA)
C*      SPIN: Spin (Returned: E)
C*      RETURN 1: Spin not found

      CHARACTER*(*) ISOTPE
      CHARACTER*80 LINE

   10 FORMAT(F5.1)
   20 FORMAT(' ** SPIN NOT FOUND FOR ',A)

      CALL DANGET_new(27,ISOTPE,1,4,LINE,IERR)

      IF (IERR.NE.0) THEN
        write (*,20) ISOTPE
        RETURN 1
      END IF

      READ(LINE(1:11),10) SPIN

      RETURN
      END
      INTEGER FUNCTION Z_FROM_S(SYM,IERR)

C*    Returns Z-number for a given element symbol
C*      SYM: Element symbol
C*      IERR: Error flag

      CHARACTER*80 LINE
      CHARACTER*2 SYM

   10 FORMAT(I3)

      CALL DANGET_new(8,SYM,1,-1,LINE,IERR)
      READ(LINE(1:3),10) IZ

      IF (IERR.NE.0) THEN
        Z_FROM_S = 0
      ELSE
        Z_FROM_S = IZ
      END IF
      RETURN
      END
