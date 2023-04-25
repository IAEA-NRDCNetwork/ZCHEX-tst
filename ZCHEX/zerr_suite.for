C*    SUBROUTINE LIBRARY ERR_SUITE
C**
C* Written by Victoria McLane
C*            National Nuclear Data Center
C*            Brookhaven National Laboratory
C*            Upton, NY 11973
C**
C* Version 99-2 (June 1999)
C*   Error message update
C* Version 98-2 (July 1998)
C*   Error message update
C* Version 98-1 (May 1998)  NEW CODE
C*   CHEX rewritten as multipass code.
C**
C* List of subroutines:
C*
C* ADD_LINES: Increments # of lines on error listing
C* ERR_ARW: Prints line and pointer record. Resets pointers and error flag.
C* ERR_ARW2: Prints line and pointer record for PASS2. Resets pointers and 
C*           error flag.
C* ERR_EOF: End-of-file while processing record.
C* ERR_FIELD: Print message with field number.
C* ERR_INSERT: Writes error message with code supplied.
C* ERR_MES: Write error message only.
C* ERR_MES1: Write error message only; sets fatal error flag.
C* ERR_REC: Prints error messages with record.
C* ERR_REC1: Prints error messages with record; sets fatal error flag
C* ERR_WARN: Write warning message
C* PUTAN: Writes accession # on output
C* FLAGIT: Puts error tags under illegal columns
C**
      SUBROUTINE ADD_LINES(NADD)

C*  Increments # of lines on error listing
C*    NADD:   # of lines to add

C---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC
      COMMON/errCOUNTS/kountErr,kountWarn

      DATA MAXLIN,NLINES/57,4/    ! Initialize maximum # of lines, line count
      
      CALL INCRMT_NEW(NLINES,NADD,MAXLIN,*900) ! Increment # of lines written
      RETURN

C---- Page full.
  900 WRITE(IOUT,1000)                  ! Start new page
      NLINES = 0                        ! Reinitialize # of lines
      RETURN

 1000 FORMAT('1')
      END
      SUBROUTINE ERR_ARW(KEY2,IPTR,KARD,ISEQ,IALT,ARROW,IERR)

C*    Print line and pointer record. Resets pointers and error flag.
C*      KEY2, IPTR, KARD, ISEQ, IALT: Input record
C*      ARROW: Error column markers
C*      IERR:  Error flag

C---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

C---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC
      COMMON/errCOUNTS/kountErr,kountWarn

      CHARACTER*80 ARROW
      CHARACTER*55 KARD
      CHARACTER*13 ISEQ
      CHARACTER*10 KEY2
      CHARACTER IPTR,IALT

c	write (*,*) '-zvv-ERR_ARW: INTSAN=',INTSAN

      WRITE(IOUT,8000) KEY2,IPTR,KARD,ACNUM,INTSAN,ISEQ(9:13),IALT
      WRITE(IOUT,6000) ARROW

      ARROW=' '                 ! Blank out pointer array
      IERR = 0                  ! Turn off error flag

      CALL ADD_LINES(2)
      RETURN

 6000 FORMAT(7X,A80)
 8000 FORMAT(7X,A10,A1,A55,A5,I3.3,A5,A1)
      END
      SUBROUTINE ERR_ARW2(KEY2,IPTR,KARD,ISEQ,ARROW,IERR)

C*    Print line and pointer record for PASS2. Resets pointers and error flag.
C*      KEY2, IPTR, KARD, ISEQ: Input record
C*      ARROW: Error column markers
C*      IERR:  Error flag

C---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

C---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC
      COMMON/errCOUNTS/kountErr,kountWarn

      CHARACTER*80 ARROW
      CHARACTER*55 KARD
      CHARACTER*13 ISEQ
      CHARACTER*10 KEY2
      CHARACTER IPTR

      WRITE(IOUT,8000) KEY2,IPTR,KARD,ACNUM,INTSAN,ISEQ(9:13)
      WRITE(IOUT,6000) ARROW

      ARROW=' '                 ! Blank out pointer array
      IERR = 0                  ! Turn off error flag

      CALL ADD_LINES(2)
      RETURN

 6000 FORMAT(7X,A80)
 8000 FORMAT(7X,A10,A1,A55,A5,I3.3,A5)
      END
      SUBROUTINE ERR_EOF(IWAY,N)

C*  End-of-file while processing record.
C*    IWAY: 1 = EOF in BIB, COMMON or DATA section
C*          2 = EOF while looking for System ID.
C*          3 = Empty file
C*    N: Index of BIB keyword to be printed

C---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC
      COMMON/errCOUNTS/kountErr,kountWarn

C---- System identifier table. (Set in DICT1)
      COMMON/SYSTAB/NSYS,SYSID(30),INDX_SYS(30)
        CHARACTER*10 SYSID

C---- Error messages.
      CHARACTER*40 MESS(3)/
     *  ' End-of-file found in section:          ',
     *  ' End-of-file found while looking for:   ',
     *  ' File empty - execution terminated      '/

      kountErr=kountErr+1
      IF (IWAY.EQ.3) THEN
        WRITE(IOUT,3100) MESS(IWAY)
      ELSE
        L = LOOKIN(N,1,INDX_SYS,NSYS)
        WRITE(IOUT,3100) MESS(IWAY),SYSID(L)
      END IF 
      RETURN

 3100 FORMAT('  ** ',A40,1X,A10,17X,A5,I3/)
      END
      SUBROUTINE ERR_FIELD(IWAY,IFLD,N)

C*  Print message with field number.
C*    IWAY: 1 = write message
C*          2 = write message with AN/SAN
C*    IFLD: field number
C*    N: index of error message

C---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

C---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC
      COMMON/errCOUNTS/kountErr,kountWarn

      CHARACTER*40 MESS(22)/
     1  ' Blank in HEADING or UNIT               ',  ! Pass 1
     2  '                                        ',
     3  '                                        ',
     4  ' No entries in DATA section for         ',
     5  ' No HEADING in DATA section for         ',
     6  ' No entries in COMMON Section for       ',
     7  ' No HEADING in COMMON Section for       ',
     8  '                                        ',
     9  '                                        ',
     A  '                                        ',
     1  ' Illegal use of nuclide                 ',  ! Pass 2
     2  ' Missing code for                       ',
     3  ' Multiplicity > 99 in                   ',
     4  ' Illegal code in                        ',
     5  ' Missing code in                        ',
     6  ' No data given in                       ',
     7  ' Heading not defined for                ',
     8  ' Illegal field identification for       ',
     9  ' Illegal floating point number in       ',
     A  ' Illegal leading blanks                 ',
     1  ' Dict236.ResFlg conflicts with code in  ',
     2  ' Com1Dat.ResPrm conflicts with code in  '/


      kountErr=kountErr+1
      IF (IWAY.EQ.1) THEN
        WRITE(IOUT,4000) MESS(N),IFLD
        CALL ADD_LINES(1)
      ELSE                    ! WRITE MESSAGE WITH AN,SAN
        WRITE(IOUT,4000) MESS(N),IFLD,ACNUM,INTSAN
        CALL ADD_LINES(2)
      END IF
      RETURN
 4000 FORMAT('  ** ',A40,' field ',I2,19X,A5,I3.3/)
      END
      SUBROUTINE ERR_INSERT(IWAY,N,NCHR,KODIN)

C*  Writes error message with code supplied.
C*    IWAY  1 = write message with AN/SAN
C*          2 = write message
C*          3 = write message with AN/SAN; set fatal error
C*    NCHR: # of characters in message insert
C*    N:    index of message
C*    KODIN: message insert

C---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

C---- Fatal error flag. (Initialized in PASS1).
      COMMON/ENDITALL/ISTOP

C---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC
      COMMON/errCOUNTS/kountErr,kountWarn

      COMMON/lastKODE/lastKODE1
      CHARACTER*200 lastKODE1

      CHARACTER*(*) KODIN
      CHARACTER*20 KODE

      CHARACTER*40 MESS(36)/
     1 ' Missing System Identitier              ',  ! Pass 1 messages
     2 '                                        ',
     3 ' Unequal # of titles and units in       ',
     4 ' Incorrect count in N1 on preceeding    ',
     5 ' Incorrect count in N2 on preceeding    ',
     6 '                                        ',
     7 '                                        ',
     8 '                                        ',
     9 '                                        ',
     A '                                        ',
     1 ' Incorrect units, should be dimension   ',  ! Pass 2 messages
     2 ' Unmatched parenthesis:                 ',
     3 ' Heading not expected from REACTION     ',
     4 ' Illegal independent variable           ',
     5 ' Missing independent variable           ',
     6 ' Variable duplicated in subentry 1      ',
     7 ' Missing data heading                   ',
     8 ' Field value undefined                  ',
     9 ' Code missing for                       ',
     A ' Nonmonatonic data field                ',  ! Message 20
     1 ' No link for REACTION pointer           ',
     2 ' BIB pointer not linked to REACTION     ',
     3 ' DATA pointer not linked to REACTION    ',
     4 ' REACTION pointer not linked to DATA    ',
     5 ' Pointer not linked to REACTION or VCom ',
     6 ' Only one pointer for BIB               ',
     7 ' No match for REACTION type             ',
     8 ' Check use of heading                   ',
     9 ' Complex REACTION; units not checked    ',
     A ' Only one pointer for REACTION          ',  ! Message 30
     1 ' No match for FLAG                      ',
     2 ' No match for DECAY FLAG                ',
     3 ' Missing required keyword               ',
     4 ' Possible missing required keyword      ',
     5 ' Expected Family Flag /vs.Dict-24/      ',
     6 ' Com1Dat.ResPrm requires SF58.Reac-Type '/

      kountErr=kountErr+1
      KODE = ' '
      KODE(1:NCHR) = KODIN
	if (N.EQ.11) then
	    if (lastKODE1.eq.KODE(1:NCHR)) return
	    lastKODE1=KODE(1:NCHR)
	endif

c	if (N.EQ.27) then
c	WRITE(*,1000) MESS(N),KODE,ACNUM,INTSAN
c	WRITE(*,*) 'N=',N
c	pause
c	endif
      IF (IWAY.EQ.2) THEN               ! Print message
        WRITE(IOUT,1000) MESS(N),KODE
      ELSE                              ! Print message with AN/SAN
	if (N.EQ.35) then
        WRITE(IOUT,1001) MESS(N),KODE,ACNUM,INTSAN
	else
        WRITE(IOUT,1000) MESS(N),KODE,ACNUM,INTSAN
	endif
        IF(IWAY.EQ.3) ISTOP=3           ! Set fatal error flag
      END IF
      CALL ADD_LINES(2)
      RETURN

 1000 FORMAT('  ** ',A40,1X,A20,7X,A5,I3.3/)
 1001 FORMAT('  ** ',A40,1X,A27,A5,I3.3)
      END
      SUBROUTINE ERR_MES(IWAY,N)

C*  Write error message only
C*    IWAY: 1 = write error message
C*          2 = write error message with AN/SAN
C*    N: index of error message

C---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

C---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC
      COMMON/errCOUNTS/kountErr,kountWarn

      CHARACTER*45 MESS(71)

C---- STAND ALONE ERROR MESSAGES
      DATA (MESS(I),I=1,30)/
     1  '                                             ',  ! Pass 1 messages
     2  '                                             ',
     3  ' Invalid record ID                           ',
     4  ' Incorrect count in N1 field                 ',
     5  ' Incorrect count on N2 field                 ',
     6  ' Date out of legal range                     ',
     7  ' Illegal date                                ',
     8  '                                             ',
     9  ' Illegal ALTERATION CODE                     ',
     A  ' SUBENT record missing ALTER CODE            ',  ! Message #10
     1  '                                             ',
     2  '                                             ',
     3  ' Duplicate BIB Keyword in SAN                ',
     4  '                                             ',
     5  ' Illegal character                           ',
     6  '                                             ',
     7  '                                             ',
     8  '                                             ',
     9  '                                             ',
     A  '                                             ',  ! Message #20
     1  ' DATA in 1st Subentry                        ',
     2  ' DATA Section missing                        ',
     3  ' Illegal pointer                             ',
     4  '                                             ',
     5  '                                             ',
     6  '                                             ',
     7  '                                             ',
     8  '                                             ',
     9  '                                             ',
     A  '                                             '/  ! Message #30
      DATA (MESS(I),I=31,71)/     
     1  ' Obsolete code                               ',  ! Pass 2 messages
     2  ' Missing parenthesis in column 12            ',
     3  ' Illegal left parenthesis in column 12       ',
     4  ' Illegal empty code field                    ',
     5  ' Illegal code field                          ',
     6  ' End of code string not found                ',
     7  ' Code too long                               ',
     8  ' Missing code field(s)                       ',
     9  ' Multiply defined HEADING or FLAG            ',
     A  '                                             ',  ! Message #40
     1  '                                             ',
     2  ' Illegal blank in code                       ',
     3  ' Illegal code separator                      ',
     4  ' Illegal parenthesis for heading field       ',
     5  ' Blank free text field                       ',
     6  ' Missing free text                           ',
     7  ' Missing parenthesis for heading field       ',
     8  '                                             ',
     9  ' Nucleus is stable                           ',
     A  ' Ind. var. link to REACTION not checked      ',  ! Message #50
     1  ' METHOD,DETECTOR,ANALYSIS or FACILITY needed ',
     2  '                                             ',
     3  ' Illegal data field                          ',
     4  ' Dependent variable missing on record        ',
     5  ' 200 DATA errors found - listing terminated  ',
     6  '                                             ',
     7  ' Illegal or missing pointer                  ',
     8  ' More than one heading found for pointer     ',
     9  ' Duplicate pointer                           ',
     A  ' Too many REACTION strings to process        ',  ! Message #60
     1  '                                             ',
     2  '                                             ',
     3  ' Outgoing particle sequence incorrect        ',
     4  ' Input/output particles do not add up        ',
     5  '                                             ',
     6  ' REACTION fields 5-8 not in dictionary       ',
     7  ' Illegal trailing commas                     ',
     8  '                                             ',
     9  ' or long REACTION string: not fully checked  ',
     A  ' Vector COMMON found                         ',  ! Message #70
     1  ' DG: Intensity without Energy                '/

!---- Input record. (Set in READ_NEXT)
      COMMON/ALLCOM/KEY2,IPTR,KARD,ISEQ
        CHARACTER*10 KEY2
        CHARACTER*1  IPTR
        CHARACTER*55 KARD
        CHARACTER*13 ISEQ

      kountErr=kountErr+1
      IF (IWAY.EQ.1) THEN                    ! Write message
        WRITE(IOUT,9001) MESS(N),ACNUM,INTSAN
c	write(*,*) '...[',KEY2,IPTR,KARD,']...debug'
c	write(IOUT,*) '...[',KEY2,IPTR,KARD,']...debug'
        CALL ADD_LINES(1)
      ELSE                                   ! Write message with AN/SAN
        WRITE(IOUT,9000) MESS(N),ACNUM,INTSAN
        CALL ADD_LINES(2)
      END IF

      RETURN
 9000 FORMAT('  ** ',A45,23X,A5,I3.3/)
 9001 FORMAT('  ** ',A45,23X,A5,I3.3)
      END
      SUBROUTINE ERR_MES1(IWAY,N)

C*  Write error message only. Sets fatal error flag.
C*    IWAY: 1 = write error message
C*          2 = write error message with AN/SAN
C*    N: index of error message

C---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

C---- Fatal error flag. (Initialized in PASS1).
      COMMON/ENDITALL/ISTOP

C---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC
      COMMON/errCOUNTS/kountErr,kountWarn

C---- STAND ALONE ERROR MESSAGES
      CHARACTER*40 MESS(30)/
     1  ' Record out of sequence                 ',  ! Pass 1 messages
     2  ' Lost in processing                     ',
     3  '                                        ',
     4  '                                        ',
     5  '                                        ',             
     6  '                                        ',
     7  '                                        ',
     8  '                                        ',
     9  '                                        ',
     A  '                                        ',  ! Message #10
     1  '                                        ',
     2  ' Illegal BIB Keyword                    ',
     3  '                                        ',
     4  ' Obsolete BIB Keyword                   ',
     5  ' Nonblank pointer in UNIT field         ',
     6  ' Dependent variable missing in DATA     ',
     7  ' Illegal dependent variable in COMMON   ',
     8  ' Illegal DATA HEADING or UNIT           ',
     9  ' Unmatched HEADINGS, UNITS, DATA        ',
     A  ' Illegal floating point number          ',  ! Message #20
     1  ' DATA in 1st Subentry                   ',
     2  ' DATA Section missing                   ',
     3  ' Illegal pointer                        ',
     4  ' More than 18 fields                    ',
     5  ' Allowed: U F P C                       ',
     6  '                                        ',
     7  '                                        ',
     8  '                                        ',
     9  '                                        ',
     A  '                                        '/  ! Message #30

      kountErr=kountErr+1
      IF (IWAY.EQ.1) THEN                    ! Write message
!        WRITE(IOUT,9002) IWAY,N
        WRITE(IOUT,9000) MESS(N)
        CALL ADD_LINES(1)
      ELSE                                   ! Write message with AN/SAN
!        WRITE(IOUT,9002) IWAY,N
        WRITE(IOUT,9000) MESS(N),ACNUM,INTSAN
        CALL ADD_LINES(2)
      END IF

!!!!!!!!!!test>>remove next 3 line for regular checking
!20220608
      IF (N.EQ.14) RETURN  ! Obsolete BIB Keyword
      IF (N.EQ.18) RETURN  ! Illegal DATA HEADING or UNIT
      IF (N.EQ.20) RETURN  ! Illegal floating point number

!!!!!!!!!!test>>remove next line for full EXFOR testing
      ISTOP=4

      RETURN
 9002 FORMAT('  ** ___ERR_MES1: IWAY=',I8,'  N=',I8)
 9000 FORMAT('  ** ',A40,28X,A5,I3.3/)
      END
      SUBROUTINE ERR_REC(KEY2,IPTR,KARD,ISEQ,IALT,N)

C*  Prints error messages with record
C*    KEY2, IPTR, KARD, ISEQ, IALT: input record
C*    N:  index of error message

C---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

C---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC
      COMMON/errCOUNTS/kountErr,kountWarn

C---- Operating mode. (Set in OPENUP)
      COMMON/OPMODE/ITYPE,IWARNG

      CHARACTER*80 ARROW
      CHARACTER*55 KARD
      CHARACTER*13 ISEQ
      CHARACTER*10 KEY2
      CHARACTER*5 ANLIST
      CHARACTER IPTR,IALT

C---- STAND ALONE ERROR MESSAGES
      CHARACTER*40 MESS(40)/
     1  ' Label invalid or missing: TRANS        ',
     2  ' Record after end label                 ',
     3  ' Record out of sequence                 ',
     4  ' Duplicate System Identifier            ',
     5  ' Invalid accession number               ',  
     6  ' Invalid subaccession number            ',
     7  ' Incorrect count in N1 field            ',
     8  ' Illegal BIB Keyword                    ',
     9  ' Duplicate BIB Keyword in SAN           ',
     A  ' Empty BIB Section                      ',  ! Message #10
     1  ' Illegal character                      ',
     2  ' Incorrect count on N2 field            ',
     3  ' Illegal record                         ',
     4  ' Illegal floating point number          ',
     5  ' Incomplete DATA Section                ',
     6  ' Illegal DATA field                     ',
     7  ' Illegal DATA Heading                   ',
     8  ' UNIT field after blank field           ',
     9  ' Illegal DATA UNIT                      ',
     A  ' DATA in 1st Subentry                   ',  ! Message #20
     1  ' Unmatched HEADINGS, UNITS, DATA        ',
     2  ' DATA after blank                       ',
     3  ' More than 18 fields - Section skipped  ',
     4  '                                        ',
     5  ' Over 3 records in COMMON               ',
     6  '                                        ',
     7  ' DATA Section missing                   ',
     8  ' Lost in processing                     ',
     9  ' Resume checking at                     ',
     A  ' END LABEL does not match LABEL         ',  ! Message #30
     1  ' Invalid BIB code                       ',
     2  ' Duplicate record                       ',
     3  ' Illegal ALTERATION CODE                ',
     4  ' SUBENT record missing ALTER CODE       ',
     5  ' Obsolete code found                    ',
     6  ' Date out of legal range                ',
     7  ' Illegal date                           ',
     8  ' Invalid RECORD ID (columns 67-79)      ',
     9  ' Illegal floating point number          ',
     A  '                                        '/  ! Message #40

      kountErr=kountErr+1
C---- Write error message.
      IF(N.NE.0) WRITE(IOUT,9000) MESS(N)
      CALL ADD_LINES(1)

C---- Write record. Increment # of lines on page.
      WRITE(IOUT,9100) KEY2,IPTR,KARD,ACNUM,INTSAN,ISEQ(9:13),IALT
      CALL ADD_LINES(2)
      ISTOP=5
      RETURN

 9000 FORMAT('  ** ',A40)
 9100 FORMAT(7X,A10,A1,A55,A5,I3.3,A5,A1/)
      END
      SUBROUTINE ERR_REC1(KEY2,IPTR,KARD,ISEQ,IALT,N)

C*  Prints error messages with record; sets fatal error flag
C*    KEY2, IPTR, KARD, ISEQ, IALT: input record
C*    N:  index of error message

C---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

C---- Fatal error flag. (Initialized in PASS1).
      COMMON/ENDITALL/ISTOP

C---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC
      COMMON/errCOUNTS/kountErr,kountWarn

C---- Operating mode. (Set in OPENUP)
      COMMON/OPMODE/ITYPE,IWARNG

      CHARACTER*80 ARROW
      CHARACTER*55 KARD
      CHARACTER*13 ISEQ
      CHARACTER*10 KEY2
      CHARACTER*5 ANLIST
      CHARACTER IPTR,IALT

C---- STAND ALONE ERROR MESSAGES
      CHARACTER*40 MESS(30)/
     1  ' Invalid System Identifier              ',
     2  '                                        ',
     3  ' Record out of sequence                 ',
     4  ' Duplicate System Identifier            ',
     5  ' Invalid #Entry num.: must be increasing',  
     6  ' Invalid subaccession number            ',
     7  '                                        ',
     8  '                                        ',
     9  '                                        ',
     A  ' Empty BIB Section                      ',  ! Message #10
     1  '                                        ',
     2  '                                        ',
     3  ' Illegal record                         ',
     4  '                                        ',
     5  '                                        ',
     6  ' Illegal DATA field                     ',
     7  '                                        ',
     8  ' UNIT field after blank field           ',
     9  '                                        ',
     A  '                                        ',  ! Message #20
     1  '                                        ',
     2  '                                        ',
     3  ' More than 18 fields - Section skipped  ',
     4  '                                        ',
     5  '                                        ',
     6  '                                        ',
     7  '                                        ',
     8  '                                        ',
     9  '                                        ',
     A  ' END LABEL does not match LABEL         '/  ! Message #30

C---- Write error message.
      IF(N.NE.0) WRITE(IOUT,9000) MESS(N)
      CALL ADD_LINES(1)

C---- Write record. Increment # of lines on page.
      WRITE(IOUT,9100) KEY2,IPTR,KARD,ACNUM,INTSAN,ISEQ(9:13),IALT
      CALL ADD_LINES(2)
      ISTOP=6
      RETURN

 9000 FORMAT('  ** ',A40,26X,A5,I3/)
 9100 FORMAT(7X,A10,A1,A55,A5,I3.3,A5,A1/)
      END
      SUBROUTINE ERR_WARN(IWAY,N)

C*  Write warning message
C*    IWAY: 1 = write error message
C*          2 = write error message with AN/SAN
C*    N: index of error message
C*    ARROW: Error markers
C*    LOW:   Initial position of field in error
C*    NUM:   Width of field in error
C*    IERR:  Error flag

C---- Current AN, SAN, Section. (Set in READ_NEXT)
      COMMON/ANSAN/INTSAN,ACNUM
        CHARACTER*5 ACNUM 

C---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC
      COMMON/errCOUNTS/kountErr,kountWarn

C---- Operating mode. (Set in OPENUP)
      COMMON/OPMODE/ITYPE,IWARNG

C---- Warning messages 
      CHARACTER*40 MESSW(17)/
     1  ' Extinct code found                     ',  ! Pass 1
     2  ' Blank record(s) following:             ',
     3  '                                        ',
     4  '                                        ',
     5  '                                        ',
     6  ' Possible missing code field            ',  ! Pass 2
     7  ' Separator after end of REACTION string ',
     8  ' REACTION strings not same dimension    ',
     9  ' Abundance > 2.(200%)                   ',
     A  ' Possible missing isomeric state code   ',
     1  ' Blank free text field                  ',
     2  ' Isomeric state not defined in Dict.227 ',
     3  ' Use of nuclide in field not in Dict.227',
     4  ' Nonspecific REACTION; units not checked',
     5  ' SF1 for use with SF2=0 only            ',
     6  ' DG: Intensity without Energy           ',
     7  ' Energy < 1.keV                         '/

      kountWarn=kountWarn+1
      IF(IWARNG.EQ.0) RETURN                     ! Warning flag off.

      IF (IWAY.EQ.1) THEN                        ! Write message
        WRITE(IOUT,6000) MESSW(N)
        CALL ADD_LINES(1)
      ELSE                                       ! Write message with AN/SAN
        WRITE(IOUT,5000) MESSW(N),ACNUM,INTSAN
        CALL ADD_LINES(2)
      END IF
      RETURN

 5000 FORMAT('  ??  WARNING ',A40,19X,A5,I3.3)
 6000 FORMAT('  ??  WARNING ',A40)
      END
      SUBROUTINE PUTAN(ANLIST,N2ASC)

C*  Writes accession # on output
C*    ANLIST: accession #

C---- I/O units, vector common flag. (Set in OPENUP)
      COMMON/IOUNIT/IN,IOUT,IVEC,IFVEC
      COMMON/COUNTS/KOUNT(4)

      CHARACTER*5 ANLIST
      CHARACTER*8 N2ASC

c-zvv      write (*,9500) ANLIST
      write (*,9501) ANLIST,N2ASC,KOUNT(2)+1
	call flush(6) !---zvv2010
      WRITE(IOUT,9400) ANLIST,N2ASC
      CALL ADD_LINES(2)
      RETURN

 9400 FORMAT(/' ENTRY ',A5,' ',A8)
 9500 FORMAT(1X,A5)
 9501 FORMAT(1X,'+++ENTRY: ',A5,' ',A8,I6)
      END
      SUBROUTINE FLAGIT(IWAY,ARROW,LOW,NUM,IERR)

C*  Puts error tags under illegal columns
C*  Input to routine
C*    IWAY: 0 = warning message
C*    ARROW: error tag array
C*    LOW: first column to be flagged
C*    NUM: number of columns to be flagged
C*  Ouput from routine
C*    IERR: error flag

C---- Operating mode. (Set in OPENUP)
      COMMON/OPMODE/ITYPE,IWARNG

      CHARACTER*80 ARROW

      IF(IWAY.EQ.0 .AND. IWARNG.EQ.0) RETURN          ! Warning flag off.

!	write (*,*) '...FLAGIT:',IWAY,LOW,NUM,IERR
C---- Set error pointers
      IF ((NUM.GT.0).AND.(LOW.GT.0)) THEN
        DO 820 I = LOW,LOW+NUM-1
          ARROW(I:I) = '^'
  820   CONTINUE
      END IF

      IERR = 1                    ! SET FLAG
      RETURN
      END
