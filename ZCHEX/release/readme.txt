Title     : Program ZCHEX
Version   : 2023-12-27, IAEA-NDS
Author    : V.McLane, BNL, USA, 1998-2001
          : V.Zerkin, IAEA-NDS, 2001-2023
Purpose   : Checking EXFOR entries
Usage     : see file 'howto.txt'

Updated:
         2002-03      V.McLane: Provided source code on VMS (DEC-Fortran)
         2002-05      V.Zerkin: Ported from VMS to Windows, Linux:
                      - Dictionaries were re-organized to direct-access files;
                      - New program DANLO written to create new Dictionaries
                        from dictionaries-backup file (DAN_BACK_NEW.XXXX)
                      - Subroutines to access new Dictionaries
         2004-05      V.Zerkin: recompiled on Linux with Fortran:
                      - Lahey/Fujitsu Fortran 95 Compiler Release L6.20b
         2005-04      V.Zerkin
                      - Windows/Linux: EXFOR Dictionary 9086
                      - debugging: usage of Status-field in Dictionaries
         2005-09      V.Zerkin
                      - EXFOR Dictionaries were re-organized (9188):
                        7-->7+207, 13-->213, 27-->227+209, 36-->236
                      - debugging: correct checking N1, N2 in ENDENTRY, DATA
         2005-12      V.Zerkin
                      - debugging: correct work in TRANS mode
         2006-01      V.Zerkin
                      - Windows/Linux: EXFOR Dictionary 9090
         2006-12-12   V.Zerkin
                      - Windows/Linux: EXFOR Dictionary 9093
                      - finds missing '(' in REACTION keyword
                      - allows isomer extensions "-L","-L1","-L2"
                        in REACTION and DECAY codes
                      - Wildcards for SF7 during cheching of SF5-8:
                        done using Dictionary-33 (works, but still under testing)
                      - indication of SAN in the right column of error-message
                      - input file name is indicated in the error file and 
                        on the user's terminal
                      - length of input file name is expanded from 50 to 65
         2007-06-08   V.Zerkin
                      - Windows/Linux: EXFOR Dictionary 9094
                      - Wildcards for SF7 during cheching of SF5-8:
                        improved algorithm - now uses all Dictionary-33 and 227;
			(full test and Dict-236 having only wildcards were
			 prepared by O.Schwerer)
                      - debugging (according to S.Maev, 2007-06-06 e-mail)
                      - adopted to MacOS-X (using g77 fortran compiler)
         2007-10-02   V.Zerkin
                      - Windows/Linux/Mac: EXFOR Dictionary 9095 (wildcards only)
                      - debugging: correct processing of N1,N2 in DATA and COMMON
         2009-10-07   V.McLane
                      - recognize NPART in SF4
                      - check that author's name is longer than one character
			(to find commas after initials)
		      - allow alphabetic characters in reference page number
		      - check coding for new ERR-ANALYS format (not completed)
         2010-03-30   V.Zerkin
                      - debugging: completed checking new ERR-ANALYS format
                      - introduced definition of input file name in command line
		      - standard procedure to run from Windows-desktop
                      - Windows/Linux/Mac: EXFOR Dictionary 9100
         2011-01-07   V.Zerkin
                      - checking obsolete flags for INC-SOURCE and DETECTOR
                      - Windows/Linux: EXFOR Dictionary 9101
         2011-01-31   V.Zerkin
                      - increasing maximum limit of DECAY-FLAG
                      - debug to avoid crash (ZPASS2:call PARS_CHR)
         2011-04-21   V.Zerkin
                      - suppress error message for MONIT_REF referring to 
                        an evaluated library
         2012-03-21   V.Zerkin
                      - resonance flag is defined only in the dict-236 (not dict-213)
         2013-02-08   V.Zerkin
                      - Windows/Linux: EXFOR Dictionary 9104
         2015-05-14   V.Zerkin
                      - Windows/Linux: EXFOR Dictionary 9111
                      - set required DATA Units "E" for Reaction-Ratio: E2/E
         2017-05-10   V.Zerkin
                      - Windows/Linux: EXFOR Dictionary 9115
                      - debug checking of ERR-ANALYS code: limit by (...)
                        e.g. 33080002:
                        ERR-ANALYS (MONIT-ERR,2.,3.7) 56Fe(n,p)56Mn
         2017-05-17   V.Zerkin
                      - debug checking: Reference long(L=11) Report-code, e.g.:
                        REFERENCE  (R,IAEA-TECDOC-1211,2001)
                        MONIT-REF  (,S.M.Qaim+,R,IAEA-TECDOC-1211,2001)
                        REL-REF    (N,,S.M.Qaim+,R,IAEA-TECDOC-1211,2001)
         2017-05-18   V.Zerkin
                      - checking correlation flag in the 4th field of ERR-ANALYS
                        ERR-ANALYS (ERR-4,1.,2.,U)
         2017-11-16   V.Zerkin
                      - correct checking IAS-NUMB in the keyword LEVEL-PROP
                      - Windows/Linux: EXFOR Dictionary 9116
         2018-01-15   V.Zerkin
                      - Windows/Linux/MacOS: EXFOR Dictionary 9117
         2018-04-20   V.Zerkin
                      - checking SF9 for obsolete codes (EXP)
         2019-01-10   V.Zerkin
                      - MacOS-10.14.2(Mojave), gfortran-8.2.0, EXFOR Dictionary 9119
         2019-03-07   V.Zerkin
                      - debug under gfortran: MacOS, Linux, Windows
         2019-07-19   V.Zerkin
                      - checking Units in Subent-1 vs. expected Data:Units(Reaction)
         2020-06-29   V.Zerkin
                      - checking #Entry accession number: must be increasing
         2020-12-04   V.Zerkin
                      - DECAY-DATA: checking DG Abundance given without Energy,
                        Energy < 1.keV, Abundance > 2.(200%)
                      - debug: allowed multiple appearance of the same correlation
                        flags in the ERR-ANALYS codes, e.g. (ERR-4,,,F)
                      - INSTITUTE: allowed leading blank in the 2nd, 3rd, etc. lines
                      - statistics: output number of warnings and errors
                      - define flag “Print warnings” in the command line
         2021-05-14   V.Zerkin
                      - Dict236:[,SIG,,RES]: correctly accepting [EN-RES]
         2021-05-21   V.Zerkin
                      - correctly checking muptiple code, e.g. ADD-RES:(DECAY,G-SPEC,STRUC)
         2021-05-24   V.Zerkin
                      - correctly checking (MONIT1..9) in DECAY-MON
         2021-06-18   V.Zerkin
                      - fixed a bug: crash on long code [,POL/DA/DA/DE,*/*/*,ANA]
         2021-07-04   V.Zerkin
                      - Windows/Linux/MacOS: EXFOR Dictionary 9124
         2021-12-29   V.Zerkin
                      - Windows/Linux/MacOS: EXFOR Dictionary 9125
         2022-06-09   V.Zerkin
                      - improved checking with "Family flags" (Dict-24)
                      - debug: suppress output "^^^^^^^^^^" for blank lines if "no warnings"
                        (example:23327006)
         2023-04-25   V.Zerkin
                      - improved indication of conflict Dict236.ResFlg=[.] with in SF4:must be empty
                      - indication of: Common|Data.ResonanceParam requires SF58.ReacType:RP=[.] in Dict-236
                      - Windows/Linux/MacOS: EXFOR Dictionary 9127
         2023-05-18   V.Zerkin
                      - ZORDER: right column nsub with leading zero's
         2023-05-29   V.Zerkin
                      - ZORDER debug: restore line ALT flag
         2023-07-18   V.Zerkin
                      - Windows/Linux/MacOS: EXFOR Dictionary 9128
                      - DANLO: adapted for using new Dict.227
         2023-12-27   V.Zerkin
                      - Windows/Linux/MacOS: EXFOR Dictionary 9129
                      - process new STATUS with REFERENCE-code, e.g.:
                        (TABLE,,D.M.Chittenden+,P,ORO-367,1,196101)

History:
         1998-2001
         Originally written on VMS by:
         -----------------------------
         Ms. Victoria McLane
         National Nuclear Data Center
         Bldg. 197D
         Brookhaven National Laboratory
         P.O. Box 5000
         Upton, NY 11973-5000
         U.S.A.

         2001-2023
         Adopted to Windows(2001), Linux(2001) and MacOS(2007).
         Development, debugging by:
         --------------------------------------------
         Mr. Viktor Zerkin
         Room: A-2321
         Nuclear Data Section
         International Atomic Energy Agency
         P.O. Box 100
         A-1400 Vienna
         Austria, Europe
         PHONE    :+43 1-2600-21714
         FAX      :+43 1-26007
         E-MAIL   :v.zerkin@iaea.org, v.zerkin@gmail.com
