echo "Now                  `date +%F,%T`"
echo "Nodename             `uname -n`"
echo "Machtype             $MACHTYPE "
echo "Kernel-name          `uname -s`"
echo "Kernel-release       `uname -r`"
echo "Processor            `uname -p`"

myos=`uname -s`
if test "$myos" = "Darwin" ; then
    echo ""
    echo "_______$ sw_vers"
    sw_vers
elif test "$myos" = "Linux" ; then
    echo "Operating-system     `uname -o`"
    echo ""
    echo "_______$ lsb_release -a"
    lsb_release -a              
    echo ""
    echo "_______$ lscpu"         
    lscpu                       
else
    echo "bash                 `uname -o`"
    echo "Operating-system     $OS"
fi

echo ""
echo "_______$ gfortran --version"
gfortran --version
