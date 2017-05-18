set -x
gfortran -m64 -all_load -w -o zchex-mac64.exe zchex.for zpass1.for zpass2.for zutilty_sub.for zdaniel_new.for zvaxutl.for zerr_suite.for dan_iod77.for
gfortran -m32 -all_load -w -o zchex-mac.exe zchex.for zpass1.for zpass2.for zutilty_sub.for zdaniel_new.for zvaxutl.for zerr_suite.for dan_iod77.for
gfortran -m32 -all_load -w -o danlo-mac.exe danlo.for
gfortran -m64 -all_load -w -o danlo-mac64.exe danlo.for
ls -la *.exe
#cp -p *.exe ../zchex_mac-2017-05-18/
#cd ../zchex_mac-2017-05-18/
#test-all-mac.sh
#ls -la *.err
#./test-all-mac64.sh 
#  564  tar cvzf zchex_mac-2017-05-18.tar.gz zchex_mac-2017-05-18
#  565  cp -p zchex_mac-2017-05-18.tar.gz /Volumes/My\ Passport/zerkin/zchex/
#  566  cp -p zchex_mac-2017-05-18.tar.gz /Volumes/NDS-50Y/
#  571  cp -p zchex_mac-2017-05-18.zip /Volumes/NDS-50Y/
