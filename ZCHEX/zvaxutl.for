C***********************************************************************
C
C     VAXUTL.FOR     C.L. DUNFORD            7/9/1997
C
C     THIS LIBRARY OF ROUTINES CONTAINS ROUTINES OF GENERAL UTILITY FOR
C       THE VAX
C
C     USER ACCESSIBLE ROUTINES
C         UPSTR                  LOSTR
C         NOLBLANK               NOLBLANKL
C         SUPBLANK               KSEARCH  
C         SCOUNT                 LSTRING    
C         TOKEN                  TOKENL
C         TSTRING
C         NO_SCREEN_SET
C         NNDC_MENU              NNDC_VMENU
C         NNDC_INPVAL            NNDC_GET (OBSOLETE)
C         NNDC_MULTISELECT       NNDC_COLSEL
C         SET_FORM_TITLE         SET_FORM_NAME
C         SET_FORM_VALUE         GET_FORM_VALUE
C         INPFORM                
C         NNDC_GETHELP(OBSOLETE) NNDC_PUTHELP
C         USER_HELP
C         CHECKPAR               TRANVAL
C         PUT_CHARS              ACTION
C         OUTPUT_SCROLL
C         SET_SCROLL_HEADER      SET_SCROLL_MENU
C         SET_SCROLL_OVERLAP
C         DEFINE_VM              REDEFINE_VM
C         SIZE_VM                CLEAR_VM
C         PUT_VM
C         NNDC_HELP              SET_HELP_FILE
C         SET_HELP               HELP_INPUT  
C         HELP_OUTPUT
C         SORT                   EDTS_FILE
C         VSLEEP                 ASLEEP
C         DATE_20                IDATE_20
C         APPEND_FILE            GET_SYMBOL
C         TEXT_CON               FIX_FP_NUMBER
C         TRANSNUC               NUCTXT
C         REACTXT
C         GET_CENTER
C         NNDC$GETJPIW           NNDC$SPAWN
C         NNDC$TRNLIO            NNDC$GETNODE
C
C***********************************************************************
C
      SUBROUTINE UPSTR(STRING)
C
C     ROUTINE TO CONVERT A STRING TO ALL UPPER CASE
C
C         STRING  -  CHARACTER STRING TO BE CONVERTED
C
      CHARACTER STRING*(*)
C
      L = LEN(STRING)
      DO 50 I=1,L
      IC = ICHAR(STRING(I:I))
      IF(IC.GT.96.AND.IC.LT.123)   STRING(I:I) = CHAR(IC-32)
   50 CONTINUE
C
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE LOSTR(STRING)
C
C     ROUTINE TO CONVERT A STRING TO ALL LOWER CASE
C
C         STRING  -  CHARACTER STRING TO BE CONVERTED
C
      CHARACTER STRING*(*)
C
      L = LEN(STRING)
      DO 50 I=1,L
      IC = ICHAR(STRING(I:I))
      IF(IC.GT.64.AND.IC.LT.91)   STRING(I:I) = CHAR(IC+32)
   50 CONTINUE
C
      RETURN
      END
C
C
C***********************************************************************
C
      SUBROUTINE DATE_20(DATE)
C
C     RETURNS DATE AS A CHARACTER STRING OF 11 CHARACTERS IN THE
C          FORM  DD-MMM-YYYY
C
      CHARACTER DATE*(*)
      CHARACTER DAY_TIME*23
c      CHARACTER MONTHS*36/'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'/
      CHARACTER*3 MONTHS(12)/'JAN','FEB','MAR','APR','MAY'
     *,'JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
C
C     GET THE DATE AND TIME AS A CHARACTER STRING
C
c?    CALL LIB$DATE_TIME(DAY_TIME)
      CALL DATE_and_TIME(DAY_TIME)
      READ(DAY_TIME(1:8),'(I4,I2,I2)')  IYEAR,IMONTH,IDAY
C
C     EXTRACT THE DATE ONLY
C
c?      DATE = DAY_TIME(1:11)
c      write(date,fmt='(i2.2,1h-,a3,1h-,i4.4)') iday
c     * ,months(((imonth-1)*3+1):((imonth-1)*3+3)),iyear
      write(date,fmt='(i2.2,1h-,a3,1h-,i4.4)')iday,MONTHS(imonth),iyear
C
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE IDATE_20(IMONTH,IDAY,IYEAR,IDATE)
C
C     ROUTINE TO RETURN DATE AS COMPONENTS AND IN THE FORM YYYYMMDD
C
      CHARACTER DAY_TIME*23
C
      CALL DATE_AND_TIME(DAY_TIME)
      READ(DAY_TIME(1:8),'(I4,I2,I2)')  IYEAR,IMONTH,IDAY
C
C     COMBINE TO SINGLE INTEGER FORMAT
C
      IDATE = 10000*IYEAR + 100*IMONTH + IDAY
C
      RETURN
      END
