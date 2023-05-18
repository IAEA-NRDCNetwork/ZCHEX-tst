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

 1000 FORMAT(A66,A5,I3.3,I5,A1)
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


      function mylen(str)
	CHARACTER*(*) str
	mylen=len(str)
	do i=mylen,1,-1
c	call outCharArray(str(i),1)
	if (str(i:i).ne.' ') return
	mylen=mylen-1
	end do
	return
      end

      character*255 function tab2space(str)
	CHARACTER*(*) str
	CHARACTER*255 strOut
	mylen=len(str)
	strOut=' '
	iout=1;
	do i=1,mylen
	    if (str(i:i).eq.'	') then
		ll=8-mod(i-1,8)
		do ii=1,ll
		    strOut(iout:iout)=' '
		    iout=iout+1
		enddo
	    else
		strOut(iout:iout)=str(i:i)
		iout=iout+1
	    endif
	end do
	tab2space=strOut
	return
      end
