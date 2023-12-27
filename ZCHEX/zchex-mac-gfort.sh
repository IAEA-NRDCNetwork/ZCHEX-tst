set -x
gfortran -std=legacy -finit-real=zero -finit-integer=0 -fbounds-check -all_load -w -o zchex-mac-gfort.exe zchex.for zpass1.for zpass2.for zutilty_sub.for zdaniel_new.for zvaxutl.for zerr_suite.for dan_iod77.for
gfortran -std=legacy -finit-real=zero -finit-integer=0 -fbounds-check -all_load -w -o danlo-mac-gfort.exe danlo.for
gfortran -std=legacy -finit-real=zero -finit-integer=0 -fbounds-check -all_load -w -o zorder-mac-gfort.exe zorder.for zvaxutl.for zutilty_order.for

ls -la *.exe
rm -rf zchex-mac-`date +%F`
mkdir  zchex-mac-`date +%F`
cd     zchex-mac-`date +%F`
cp -p  ../release/* .
mv     ../*.exe .
cp -p danlo-mac-gfort.exe  danlom.exe
cp -p zchex-mac-gfort.exe  zchexm.exe
cp -p zorder-mac-gfort.exe zorderm.exe
./danlom.exe <tt0.inp
./test-all-mac.sh
set +x
echo ---Finished. You must have [39] files.err in this directory---
ls -la *.err
ls -la *.err | wc -l

source ../os-ver.sh >___os-ver.txt

cd ..
du -d 1 zchex-mac-`date +%F`
zip -r zchex-mac-`date +%F`.zip zchex-mac-`date +%F`

echo "___File zchex.tto2 must be empty___";ls -la zchex-mac-`date +%F`/zchex.tto2
echo "___You must have [39] files *.err and [3440] lines in the file ___allerr.txt"
ls -la zchex-mac-`date +%F`/*.err | wc -l
wc -l  zchex-mac-`date +%F`/___allerr.txt
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
