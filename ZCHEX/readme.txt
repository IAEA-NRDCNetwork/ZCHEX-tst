Title  : Program ZCHEX
Author : V.McLane, BNL, USA, 1998-2001
       : V.Zerkin, IAEA-NDS, 2001-2024

Purpose: Checking EXFOR file

Contents:
    1) Dictionaries: backup, direct-access
    2) source codes (Fortran)
       -- ZCHEX:  checks an input file in the EXFOR transmission format
       -- DANLO:  prepares dictionaries for ZCHEX from "Daniel-backup file" DAN_BACK_NEW.VER
       -- ZORDER: adds record identification and bookkeeping information to an EXFOR file producing TRANS file
    3) sctipts* to compile source codes, prepare exacutables, run tests on the platforms:
       -- MS-Windows : zchex-win-gfort.sh
       -- Linux-32   : zchex-lin-gfort32.sh
       -- Linux-64   : zchex-lin-gfort64.sh
       -- MacOS      : zchex-mac-gfort.sh
    4) tests: scripts and EXFOR samples for testing ZCHEX-code
*Note. The scripts prepared to use gfortran as Fortran compiler

System environment:
    1) please, check you gfortran:
       $ gfortran --version
    2) if fails, install gfortran:
       -- Linux:
          $ sudo apt-get install gfortran
       -- MacOS:
          $ brew install gfortran
       -- Windows: follow instructions on 
          https://gcc.gnu.org/install/binaries.html
          or https://osdn.net/projects/mingw/

Installation:
    1) make sure that gfortran is installed:
       $ gfortran --version
    2) select your platform and run one of the scripts above, for example:
       $ bash zchex-lin-gfort64.sh
       $ bash zchex-win-gfort.sh
         ... wait ...
         ... new directory created with new executables and tests' results
         ... please, check final message and compare number of files and lines, for eaxmple:
         ___You must have [39] files *.err and [3440] lines in the file ___allerr.txt
         39
         3440 zchex-win-2024-06-10/___allerr.txt

Run:
    - follow instruction in the new directory howto.txt, for example:
      zchex-win-2024-06-10/howto.txt

All the best,
Viktor Zerkin
