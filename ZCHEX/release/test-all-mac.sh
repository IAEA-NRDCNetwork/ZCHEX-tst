rm zchex.tto2
rm *.err
./zchexm.exe x.x4                2>>zchex.tto2
./zchexm.exe o2.x4               2>>zchex.tto2
./zchexm.exe o1.x4               2>>zchex.tto2
./zchexm.exe e1943.x4            2>>zchex.tto2
./zchexm.exe aa.x4               2>>zchex.tto2
./zchexm.exe g18.x4              2>>zchex.tto2
./zchexm.exe a.x4                2>>zchex.tto2
./zchexm.exe na2l.x4             2>>zchex.tto2
./zchexm.exe o1728.txt           2>>zchex.tto2
./zchexm.exe mm1.x4              2>>zchex.tto2
./zchexm.exe 41110001-x4.txt     2>>zchex.tto2
./zchexm.exe 32694001-x4.txt     2>>zchex.tto2
./zchexm.exe 00001001-x4.txt     2>>zchex.tto2
./zchexm.exe nao4.x4             2>>zchex.tto2
./zchexm.exe nao5.x4             2>>zchex.tto2
./zchexm.exe nao6.x4             2>>zchex.tto2
./zchexm.exe 30448002-x4.txt     2>>zchex.tto2
./zchexm.exe 33080002.x4         2>>zchex.tto2
./zchexm.exe t0294001x4.txt      2>>zchex.tto2
./zchexm.exe d4048002.x4         2>>zchex.tto2
./zchexm.exe 31788.txt           2>>zchex.tto2
./zchexm.exe f1378.txt t         2>>zchex.tto2
./zchexm.exe O0226015.x4         2>>zchex.tto2
./zchexm.exe prelim.1470         2>>zchex.tto2
./zchexm.exe 33143.x4            2>>zchex.tto2
./zchexm.exe 11748020.x4 T  y    2>>zchex.tto2
./zchexm.exe E1877002.x4 "" y    2>>zchex.tto2
./zchexm.exe 31796001.x4         2>>zchex.tto2
./zchexm.exe O0790.x4            2>>zchex.tto2
./zchexm.exe 41576008.x4         2>>zchex.tto2
./zchexm.exe 14332.x4            2>>zchex.tto2
./zchexm.exe 32699.x4            2>>zchex.tto2
./zchexm.exe 22073.x4            2>>zchex.tto2
./zchexm.exe 23327006.x4         2>>zchex.tto2
./zchexm.exe 21564002.x4         2>>zchex.tto2
./zchexm.exe 10053003.x4         2>>zchex.tto2
./zchexm.exe G4012002.x4         2>>zchex.tto2
./zchexm.exe 14672003.x4         2>>zchex.tto2
cat *.err >___allerr.txt
echo "___Program errors file: zchex.tto2" >>___allerr.txt
cat zchex.tto2 >>___allerr.txt
echo "___Program errors file: end" >>___allerr.txt
