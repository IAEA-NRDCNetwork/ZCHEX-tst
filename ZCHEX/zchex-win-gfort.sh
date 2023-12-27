set -x
gfortran --version
#echo LD_LIBRARY_PATH=$LD_LIBRARY_PATH
#echo PATH=$PATH
#ls -la /c/projects/util/MinGW-4.8.1/bin
#export 
#LD_LIBRARY_PATH=/c/projects/util/MinGW-4.8.1/bin:$LD_LIBRARY_PATH
#LIBPATH=/c/projects/util/MinGW-4.8.1/bin:$LIBPATH
#LIB=/c/projects/util/MinGW-4.8.1/bin:$LIB
#PATH=/c/projects/util/MinGW-4.8.1/bin:$PATH

#PATH=/c/projects/util/MinGW-4.8.1/bin
#PATH=/c/projects/util/MinGW-4.8.1/msys/1.0/bin:$PATH

gfortran -m32 -std=legacy -finit-real=zero -finit-integer=0 -fbounds-check -static -w -o zchex-win-gfort32.exe zchex.for zpass1.for zpass2.for zutilty_sub.for zdaniel_new.for zvaxutl.for zerr_suite.for dan_iod77.for
gfortran -m32 -std=legacy -finit-real=zero -finit-integer=0 -fbounds-check -static -w -o danlo-win-gfort32.exe danlo.for
gfortran -m32 -std=legacy -finit-real=zero -finit-integer=0 -fbounds-check -static -w -o zorder-win-gfort32.exe zorder.for zvaxutl.for zutilty_order.for

#cp -p zchex-win-gfort32.exe zchex-win-`date +%F`/zchexw.exe; exit

ls -la *.exe
rm -rf zchex-win-`date +%F`
mkdir  zchex-win-`date +%F`
cd     zchex-win-`date +%F`
cp -p  ../release/* .
mv     ../*.exe .
cp -p danlo-win-gfort32.exe  danlow.exe
cp -p zchex-win-gfort32.exe  zchexw.exe
cp -p zorder-win-gfort32.exe zorderw.exe

./danlow.exe <tt0.inp
./test-all-win.sh
set +x
echo ---Finished. You must have [39] files.err in this directory---
ls -la *.err
ls -la *.err | wc -l

source ../os-ver.sh >___os-ver.txt

cd ..
du -d 1 zchex-win-`date +%F`
#zip -r zchex-win-`date +%F`.zip zchex-win-`date +%F`
#tar cvzf zchex-win-`date +%F`.tar.gz zchex-win-`date +%F`
jar -cvfM zchex-win-`date +%F`.zip zchex-win-`date +%F`

echo "___File zchex.tto2 must be empty___";ls -la zchex-win-`date +%F`/zchex.tto2;
echo "___You must have [39] files *.err and [3440] lines in the file ___allerr.txt"
ls -la zchex-win-`date +%F`/*.err | wc -l
wc -l  zchex-win-`date +%F`/___allerr.txt
exit

#du -h -d 1 zchex-mac-`date +%F`

#zip -r zchex-src-`date +%F`.zip  *.for *.mak *.sh *.bat release
#cd ../zchex_mac-2017-05-18/
#test-all-mac.sh
#ls -la *.err
#./test-all-mac64.sh 
#  564  tar cvzf zchex_mac-2017-05-18.tar.gz zchex_mac-2017-05-18
#  565  cp -p zchex_mac-2017-05-18.tar.gz /Volumes/My\ Passport/zerkin/zchex/
#  566  cp -p zchex_mac-2017-05-18.tar.gz /Volumes/NDS-50Y/
#  571  cp -p zchex_mac-2017-05-18.zip /Volumes/NDS-50Y/
#! /bin/sh
