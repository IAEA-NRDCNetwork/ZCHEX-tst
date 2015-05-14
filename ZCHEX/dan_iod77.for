      SUBROUTINE OPEN_NEW(NUM,IERR,*)

C*  Opens dictionary file; assigns unit #.
C*    NUM: dictionary #
C*    IERR: error #
C*    RETURN 1: error 

C---- Units for open dictionaries.  Set in OPEN_NEW.
      COMMON/DANUNITS/NOPEN(1000)

C---- DAN_TOP variables
C-      NKEY: # of keys for dictionary
C-      NFLD: # of fields in code
C----   LENFLD: length of fields in code string
      COMMON/TOPREC/NKEY(1000),NFLD(1000)
      COMMON/TOPSTR/LENFLD(20,1000)
      COMMON/TOPNAM/NAMEtop(1000)
      character*30 NAMEtop

      dimension IK(3,2)  ! key location in char line for current #dict
      dimension IKtop(1000,3,2)  ! key location in char line
      dimension NRECtop(1000)    ! number of records in # dict
      dimension IRECtop(1000)    ! current record # in # dict
      COMMON/topZVV/IKtop,NRECtop,IRECtop

      CHARACTER*30 NAME/'DICT_ZVV.   '/

      DATA NOPEN/1000*0/
      DATA INIT/0/
c77      DATA IDIC/80/                         ! INITIALIZE OUTPUT UNIT
      DATA IDIC/40/                         ! INITIALIZE OUTPUT UNIT

   10 FORMAT(I3.3)

      ierr=0

      IF (NOPEN(NUM).EQ.0) THEN             ! Dictionary not opened

        IF (INIT.EQ.0) THEN                   ! 1st open
          CALL DAN_TOP_new(IERR,*805)               ! Open dictionary control list
          INIT = 1
        END IF

        IDIC = IDIC+1
        NOPEN(NUM) = IDIC
        WRITE(NAME(10:12),10) NUM 

c-debug      write (*,*) '=== openDic:',num,nkey(num),' ',name

      OPEN (UNIT=IDIC, FILE=NAME, STATUS='old',
     *      ACCESS='direct', FORM='FORMATTED',
     *      RECL=120, IOSTAT=IOS, ERR=805)
c77     *      RECL=120, READONLY, IOSTAT=IOS, ERR=805)
c-debug
	call DICT2X(num,maxIDATE)
c      write (*,*) '...maxIDATE=',maxIDATE
c      write (*,17) num,IDIC,NRECtop(num),NAMEtop(num)
c   17 FORMAT(' 	OK: Open Dictionary',I4,' UNIT=',I3,' L=',I5,': ',A30)
      write (*,17) num,maxIDATE,IDIC,NRECtop(num),NAMEtop(num)
   17 FORMAT('  OK: Open Dict.',I4,':',I6,' UNIT=',I3,' L=',I4,': ',A30)
      IRECtop(NUM)=1
      ELSE                                  ! Open dictionary for update
      OPEN (UNIT=IDIC, FILE=NAME, STATUS='old',
     *      ACCESS='direct', FORM='FORMATTED',
     *      RECL=120, IOSTAT=IOS, ERR=805)
      END IF

      RETURN

C---- ERROR Opening DICTIONARY
  805 IERR = 6
      CALL DANERROR(NUM,IERR)
      RETURN 1

      ENTRY CLOSE_DIC(NUM)

c-debug      write (*,*) '===closeDic:',num
      CLOSE(NOPEN(NUM))
      NOPEN(NUM) = 0
      RETURN

      END




      SUBROUTINE DICT2X(NDICT,maxIDATE)

      CHARACTER*3 ISTAT
      CHARACTER*30 KEY
      CHARACTER*80 LINE
      COMMON/dateZVV/myIDATE
	maxIDATE=-1

      CALL DANORD_STA_NEW(NDICT,'F',0,LINE,KEY,ISTAT,IERR,*990) ! READ FIRST RECORD
      IF(IERR.NE.0) GOTO 990

200	continue
	CALL DANORD_STA_NEW(NDICT,'N',0,LINE,KEY,ISTAT,IERR,*800)  ! READ NEXT RECORD
c	write (*,*) '...ISTAT=',ISTAT,' [',trim(LINE),']'
	IF(IERR.NE.0) RETURN                       ! TERMINATE READ IF ERROR SET
	if (myIDATE.gt.maxIDATE) maxIDATE=myIDATE
      GOTO 200

  800 RETURN                    
  990 return
      END







      SUBROUTINE DAN_TOP_NEW(IERR,*)

C*  Reads dictionary control index (DICT_NEW.TOP).
C*    IERR: error # 
C*    RETURN 1: read error

C---- DAN_TOP variables
C-      NKEY: # of keys for dictionary
C-      NFLD: # of fields in code
C----   LENFLD: length of fields in code string
      COMMON/TOPREC/NKEY(1000),NFLD(1000)
      COMMON/TOPSTR/LENFLD(20,1000)
      COMMON/TOPNAM/NAMEtop(1000)
      character*30 NAMEtop,NAMEtop1

      CHARACTER*48 FORM
      CHARACTER*3 CNUM

      dimension IK(3,2)  ! key location in char line for current #dict
      dimension IKtop(1000,3,2)  ! key location in char line
      dimension NRECtop(1000)    ! number of records in # dict
      dimension IRECtop(1000)    ! current record # in # dict
      COMMON/topZVV/IKtop,NRECtop,IRECtop
      data IKtop/6000*0/
      data NRECtop/1000*0/

      DATA IDIC/80/

C*    Format of dictionary control list records
C*      CNUM: dictionary number (A3)
C*      NAME: dictionary name (A30)
C*      NKEY: number of dictionary keys (I1)
C*      NFLD: number of dictionary fields (I2)
C*      FORM: format of dictionary fields - up to 12 (A48)
C*      NREC: # of records in dictionary (I4)

   20 FORMAT(I3)
      ierr=0

C---  Open control list
c?      OPEN (UNIT=IDIC, FILE='DANIEL$LIB:DICT_NEW.TOP',
c?     *    STATUS='OLD', ORGANIZATION='INDEXED', ACCESS='SEQUENTIAL',
c?     *    RECORDTYPE='FIXED', FORM='FORMATTED', READONLY, ERR=800)
      OPEN (UNIT=IDIC, FILE='DICT_ZVV.TOP',
     *    STATUS='OLD', ACCESS='SEQUENTIAL',
     *    FORM='FORMATTED', ERR=800)
c77     *    FORM='FORMATTED', READONLY, ERR=800)

      J = 0                                 ! Initialize read tries

c?110 READ(IDIC,11,END=200,IOSTAT=IOS,ERR=805) CNUM,NKEYI,NFLDI,FORM
c? 11 FORMAT(1X,A3,30X,I1,I2,A40)
  110 read(IDIC,11,END=200,IOSTAT=IOS,ERR=805) CNUM
     * ,NAMEtop1,NKEYI,NFLDI,FORM
     * ,NRECI, ((IK(ii,jj),jj=1,2),ii=1,3)
c-debug      write(*,11) CNUM,NKEYI,NFLDI,FORM
c-debug     * ,NRECI,((IK(ii,jj),jj=1,2),ii=1,3)
	if (1.eq.0)
     * write(*,11) CNUM
     * ,NAMEtop1,NKEYI,NFLDI,FORM
     * ,NRECI, ((IK(ii,jj),jj=1,2),ii=1,3)
   11 FORMAT(1X,A3,A30,I1,I2,A48,i4,6i3)

c77      UNLOCK (IDIC)

      READ(CNUM,20) NUM             ! Store dictionary number
      NKEY(NUM) = NKEYI             ! Store # OF KEYS
      NFLD(NUM) = NFLDI             ! Store # of code fields
      NRECtop(NUM)=NRECI
      NAMEtop(NUM)=NAMEtop1
      IRECtop(NUM)=1
      do ii=1,3
        do jj=1,2
          IKtop(NUM,ii,jj)=IK(ii,jj)
        end do
      end do

      II = 2
      DO 150 I=1,NFLDI-1                    ! Store format of code fields
        II = II+4
        READ(FORM(II:II+1),20) LENFLD(I,NUM)
  150 CONTINUE

      GOTO 110

  200 CLOSE (UNIT=IDIC)
      IERR=0
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






c?    SUBROUTINE READZ_KEY(IDIC,KEY,LNTH,NKEY,KEY1,REC,STAT,IERR,*)
      SUBROUTINE READZ_KEYN(NUMdic,KEY,i0KEY
     *	,LNTH,NKEY,KEY1,REC,STAT,IERR,*)

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

C---- DAN_TOP variables
      dimension IK(3,2)  ! key location in char line for current #dict
      dimension IKtop(1000,3,2)  ! key location in char line
      dimension NRECtop(1000)    ! number of records in # dict
      dimension IRECtop(1000)    ! current record # in # dict
      COMMON/topZVV/IKtop,NRECtop,IRECtop
      COMMON/DANUNITS/NOPEN(1000)

      character*120 dicline
      character*40 dickey
      character*40 findkey

      IDIC = NOPEN(NUMdic)

      IERR = 0                              ! TURN OFF ERROR FLAG
      J = 0                                 ! INITIALIZE # OF READ TRIES
      iKEY = 0                              ! INITIALIZE # OF READ TRIES

C---- READ RECORD FOR KEY SPECIFIED
    2 FORMAT(A3,I6,A30,A80)
c?   20 READ (IDIC,2,KEY=KEY(1:LNTH),KEYID=NKEY,IOSTAT=IOS,ERR=820)
c?     *      STAT,IDATE,KEY1,REC
c      write(*,*) NUMdic,NKEY,'===find: ',LNTH,' [',KEY(1:LNTH),']'
      ik1=IKtop(NUMdic,NKEY+1,1)
c     ik2=IKtop(NUMdic,NKEY+1,2)
      ik2=ik1+LNTH-1
      findkey=KEY(1:LNTH)
      idebug=0
c      if (KEY(1:LNTH).eq.'XR') idebug=1
      do irec=1,NRECtop(NUMdic)
        read (IDIC,rec=irec,fmt=77) dicline
   77   format(a120)
c        write(*,*) 'line: ',dicline
c       dickey = dicline(IKtop(NUMdic,NKEY+1,1):IKtop(NUMdic,NKEY+1,2))
        dickey = dicline(ik1:ik2)
c       if (idebug.ne.0) write(*,*) irec,' ???check: '
c     *,dickey(1:ik2-ik1+1),nkey,ik1,ik2
c        if (dickey.eq.KEY(1:LNTH)) goto 20
        if (dickey(1:LNTH).eq.findkey(1:LNTH)) then
	    iKEY=iKEY+1
	    if (iKEY.eq.i0KEY) goto 20
	end if
      if (idebug.ne.0) write(*,*) irec,' ---check: ',findkey
      end do
      IOS=36
      IRECtop(NUMdic)=1
      goto 820
   20 read (IDIC,rec=irec,fmt=2,IOSTAT=IOS,ERR=820)
c????     * STATUS,INDAY,KEY,REC,TAG ! WRITE RECORD ON DICTIONARY
c2005.04.20     * STATUS,INDAY,KEY,REC ! WRITE RECORD ON DICTIONARY
     * STAT,INDAY,KEY,REC ! WRITE RECORD ON DICTIONARY
      irec = irec+1                    ! INCREMENT # OF RECORDS IN DICTIONARY
      IRECtop(NUMdic)=irec+1
c      if (idebug.ne.0)
c     * write(*,*) NUMdic,NKEY,'!!!===found: ',LNTH,' [',KEY(1:LNTH),']'
c      if (NUMdic.eq.3)
c     * write(*,*) NUMdic,NKEY,' stat=[',STAT,']'
c     * ,'!!!===found: ',LNTH,' [',KEY(1:LNTH),']'

c77      UNLOCK (IDIC)
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


c?    SUBROUTINE READZ_KEY(IDIC,KEY,LNTH,NKEY,KEY1,REC,STAT,IERR,*)
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

C---- DAN_TOP variables
      dimension IK(3,2)  ! key location in char line for current #dict
      dimension IKtop(1000,3,2)  ! key location in char line
      dimension NRECtop(1000)    ! number of records in # dict
      dimension IRECtop(1000)    ! current record # in # dict
      COMMON/topZVV/IKtop,NRECtop,IRECtop
      COMMON/DANUNITS/NOPEN(1000)

      character*120 dicline
      character*40 dickey
      character*40 findkey

      IDIC = NOPEN(NUMdic)
c	write (*,*) '...NUMdic=',NUMdic,' IDIC=',IDIC

      IERR = 0                              ! TURN OFF ERROR FLAG
      J = 0                                 ! INITIALIZE # OF READ TRIES

C---- READ RECORD FOR KEY SPECIFIED
    2 FORMAT(A3,I6,A30,A80)
c?   20 READ (IDIC,2,KEY=KEY(1:LNTH),KEYID=NKEY,IOSTAT=IOS,ERR=820)
c?     *      STAT,IDATE,KEY1,REC
c      write(*,*) NUMdic,NKEY,'===find: ',LNTH,' [',KEY(1:LNTH),']'
      ik1=IKtop(NUMdic,NKEY+1,1)
c     ik2=IKtop(NUMdic,NKEY+1,2)
      ik2=ik1+LNTH-1
      findkey=KEY(1:LNTH)
      idebug=0
c      if (KEY(1:LNTH).eq.'XR') idebug=1
      do irec=1,NRECtop(NUMdic)
        read (IDIC,rec=irec,fmt=77) dicline
   77   format(a120)
c        write(*,*) 'line: ',dicline
c       dickey = dicline(IKtop(NUMdic,NKEY+1,1):IKtop(NUMdic,NKEY+1,2))
        dickey = dicline(ik1:ik2)
c       if (idebug.ne.0) write(*,*) irec,' ???check: '
c     *,dickey(1:ik2-ik1+1),nkey,ik1,ik2
c        if (dickey.eq.KEY(1:LNTH)) goto 20
        if (dickey(1:LNTH).eq.findkey(1:LNTH)) goto 20
      if (idebug.ne.0) write(*,*) irec,' ---check: ',findkey
      end do
      IOS=36
      IRECtop(NUMdic)=1
      goto 820
   20 read (IDIC,rec=irec,fmt=2,IOSTAT=IOS,ERR=820)
c????     * STATUS,INDAY,KEY,REC,TAG ! WRITE RECORD ON DICTIONARY
c2005.04.20     * STATUS,INDAY,KEY,REC ! WRITE RECORD ON DICTIONARY
     * STAT,INDAY,KEY,REC ! WRITE RECORD ON DICTIONARY
c	write (*,*) '...READZ_KEY...stat=[',STAT,']'
      irec = irec+1                    ! INCREMENT # OF RECORDS IN DICTIONARY
      IRECtop(NUMdic)=irec+1
c      if (idebug.ne.0)
c     * write(*,*) NUMdic,NKEY,'!!!===found: ',LNTH,' [',KEY(1:LNTH),']'
c      if (NUMdic.eq.3)
c     * write(*,*) NUMdic,NKEY,' stat=[',STAT,']'
c     * ,'!!!===found: ',LNTH,' [',KEY(1:LNTH),']'

c77      UNLOCK (IDIC)
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





c?    SUBROUTINE READ_SEQ(IDIC,REC,STATUS,IERR,*,*)
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

      COMMON/DANUNITS/NOPEN(1000)
      COMMON/TOPREC/NKEY(1000),NFLD(1000)
      COMMON/TOPSTR/LENFLD(20,1000)
      dimension IK(3,2)  ! key location in char line for current #dict
      dimension IKtop(1000,3,2)  ! key location in char line
      dimension NRECtop(1000)    ! number of records in # dict
      dimension IRECtop(1000)    ! current record # in # dict
      COMMON/topZVV/IKtop,NRECtop,IRECtop
      COMMON/dateZVV/myIDATE

    2 FORMAT(A3,I6,A110)

      IDIC = NOPEN(NUMdic)
      J = 0                                 ! INITIALIZE # OF READ TRIES

      IERR=0
C---- READ RECORD SEQUENTIALLY
c? 20 READ (IDIC,2,IOSTAT=IERR,ERR=820,END=500) STATUS,IDATE,REC
      irec=IRECtop(NUMdic)
      if (irec.gt.NRECtop(NUMdic)) goto 500
   20 read (IDIC,rec=IREC,fmt=2,IOSTAT=IOS,ERR=820)
     * STATUS,IDATE,REC
      IRECtop(NUMdic) = IRECtop(NUMdic)+1
c	write (*,*) '...IDATE=',IDATE
      myIDATE=IDATE
c77      UNLOCK (IDIC)
      RETURN

c?  500 RETURN 2                              ! END-OF-FILE
  500 IRECtop(NUMdic) = 1
      RETURN 2                              ! END-OF-FILE

  820 IF (IERR.EQ.52) THEN                  ! LOCKED RECORD
        J = J+1
        IF (J.LE.20) GOTO 20                  ! TRY READ UP TO 20 TIMES
        IERR = 4
      ELSE                                  ! ERROR IN READING RECORD
        IERR = 7
      END IF

      RETURN 1
      END

      SUBROUTINE READZ_NUM(NUMdic,REC,IREC,STATUS,IERR,*,*)

C*  Reads dictionary record sequentially
C*    IDIC: dictionary unit number
C*    REC: character string returned
C*    STATUS: status code for line (A3)
C*    IERR: error # (0 = NO ERROR)
C*    RETURN 1: error
C*    RETURN 2: end-of-file

      CHARACTER*3 STATUS
      CHARACTER*110 REC

      COMMON/DANUNITS/NOPEN(1000)
      COMMON/TOPREC/NKEY(1000),NFLD(1000)
      COMMON/TOPSTR/LENFLD(20,1000)
      dimension IK(3,2)  ! key location in char line for current #dict
      dimension IKtop(1000,3,2)  ! key location in char line
      dimension NRECtop(1000)    ! number of records in # dict
      dimension IRECtop(1000)    ! current record # in # dict
      COMMON/topZVV/IKtop,NRECtop,IRECtop

    2 FORMAT(A3,I6,A110)

      IDIC = NOPEN(NUMdic)
      J = 0                                 ! INITIALIZE # OF READ TRIES

      IERR=0
C---- READ RECORD DIRECTLY
      if ((IREC.le.0).or.(IREC.gt.NRECtop(NUMdic))) goto 500
   20 read (IDIC,rec=IREC,fmt=2,IOSTAT=IOS,ERR=820)
     * STATUS,IDATE,REC
      IRECtop(NUMdic) = IREC+1
      RETURN

  500 IRECtop(NUMdic) = 1
      RETURN 2                              ! END-OF-FILE

  820 IF (IERR.EQ.52) THEN                  ! LOCKED RECORD
        J = J+1
        IF (J.LE.20) GOTO 20                  ! TRY READ UP TO 20 TIMES
        IERR = 4
      ELSE                                  ! ERROR IN READING RECORD
        IERR = 7
      END IF

      RETURN 1
      END

      function MY_LDIC(NUMdic)
C*  Reads zize of dictionary
      COMMON/DANUNITS/NOPEN(1000)
      COMMON/TOPREC/NKEY(1000),NFLD(1000)
      COMMON/TOPSTR/LENFLD(20,1000)
      dimension IK(3,2)  ! key location in char line for current #dict
      dimension IKtop(1000,3,2)  ! key location in char line
      dimension NRECtop(1000)    ! number of records in # dict
      dimension IRECtop(1000)    ! current record # in # dict
      COMMON/topZVV/IKtop,NRECtop,IRECtop

      IDIC = NOPEN(NUMdic)
      MY_LDIC=NRECtop(NUMdic)
      RETURN
      END
