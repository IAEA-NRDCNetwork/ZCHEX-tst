set -x
lf95 -m32 -static -o zchex-lin-lf95.exe  zchex.for zpass1.for zpass2.for zutilty_sub.for zdaniel_new.for zvaxutl.for zerr_suite.for dan_iod77.for
lf95 -m32 -static -o danlo-lin-lf95.exe  danlo.for
lf95 -m32 -static -o zorder-lin-lf95.exe zorder.for zvaxutl.for zutilty_order.for

#cp -p zchex-lin-lf95.exe zchex-lin-2019-03-06/zchexl.exe; exit
ls -la *.exe
rm -rf zchex-lin-`date +%F`
mkdir  zchex-lin-`date +%F`
cd     zchex-lin-`date +%F`
cp -p  ../release/* .
mv     ../*.exe .
cp -p danlo-lin-lf95.exe  danlol.exe
cp -p zchex-lin-lf95.exe  zchexl.exe
cp -p zorder-lin-lf95.exe zorderl.exe

./danlol.exe <tt0.inp
./test-all-lin.sh
set +x
echo ---Finished. You must have [38] files.err in this directory---
ls -la *.err
ls -la *.err | wc -l

source ../os-ver.sh >___os-ver.txt
echo "--------"    >>___os-ver.txt
lf95 --version     >>___os-ver.txt

cd ..
du -d 1 zchex-lin-`date +%F`
zip -r zchex-lin32lf95-`date +%F`.zip zchex-lin-`date +%F`

echo "___File zchex.tto2 must be empty___";ls -la zchex-lin-`date +%F`/zchex.tto2
echo "___You must have [38] files *.err and [3412] lines in the file ___allerr.txt"
ls -la zchex-lin-`date +%F`/*.err | wc -l
wc -l  zchex-lin-`date +%F`/___allerr.txt
exit

#du -h -d 1 zchex-lin-`date +%F`

#zip -r zchex-src-`date +%F`.zip  *.for *.mak *.sh *.bat release

gfortran -m64 --chk --trace -w -static -o zchex-lin-gfort64.exe zchex.for zpass1.for zpass2.for zutilty_sub.for zdaniel_new.for zvaxutl.for zerr_suite.for dan_iod77.for
gfortran -m64 --trace -fbacktrace -w -static -o from_shin/zchexl.exe zchex.for zpass1.for zpass2.for zutilty_sub.for zdaniel_new.for zvaxutl.for zerr_suite.for dan_iod77.for
gfortran -m64 -fbounds-check -w -static -o from_shin/zchexl.exe zchex.for zpass1.for zpass2.for zutilty_sub.for zdaniel_new.for zvaxutl.for zerr_suite.for dan_iod77.for

#test:2019-03-06
gfortran -m64 -finit-real=zero -finit-integer=0 -fbounds-check -w -static -o zchex-lin-gfort64.exe zchex.for zpass1.for zpass2.for zutilty_sub.for zdaniel_new.for zvaxutl.for zerr_suite.for dan_iod77.for
gfortran -m64 -finit-real=zero -finit-integer=0 -fbounds-check -w -static -o danlo-lin-gfort64.exe danlo.for
gfortran -m64 -std=legacy -fbounds-check -w -static -o zchex-lin-gfort64.exe zchex.for zpass1.for zpass2.for zutilty_sub.for zdaniel_new.for zvaxutl.for zerr_suite.for dan_iod77.for


make -f zchexl_lf95.mak 
make -f danlol_lf95.mak

lf95 -m32 -static -o zchex-lin-2019-07-18/zchexl.exe zchex.for zpass1.for zpass2.for zutilty_sub.for zdaniel_new.for zvaxutl.for zerr_suite.for dan_iod77.for
