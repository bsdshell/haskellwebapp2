#!/bin/bash

#================================================================================ 
# Last Upate: Fri Oct  7 12:39:43 PDT 2016 
# Tue Nov 15 00:08:03 PST 2016  - add rsync, home dotfile to GoogleDrive/homedotfile
# 
# Script to manage all the small tasks such as editing and backup
#================================================================================ 
# all the colors in color.sh file
# [gf] open it 
# how to use in /Users/cat/myfile/script/jav.sh 
# e.g. printf "${FG_BR_RED}Hello World${RESET_ALL}\n"
# -------------------------------------------------------------------------------- 
# Tue May  7 16:46:25 2019 
# Add full path to ghc, Emacs can't find ghc from M-:
#================================================================================ 
# Sun Jul 28 18:21:09 2019 
# update to more generic file path
#================================================================================ 

# $(basename file.cpp) => file
#if [ "$#" -eq 0 ]; then
#else
#fi

#if [ "$?" -eq 0 ]; then
#else
#fi

#for var in $(ls) 
#do
#    $echo $var
#done 

# Mon Aug  5 14:27:58 2019 
# remove the code to read config.txt => osconfig.txt file
# change to read $b/publicfile/osconfig.txt

# copy binary into $HOME/myfile/mybin/xxxBin/xxx
# create symbol link in $sym/xxx ->  $HOME/myfile/mybin/xxxBin/xxx

function help(){
    printc 196 "help message"
}

source "$HOME/myfile/bitbucket/script/color.sh"  

getpwd


# getName -> $ff/mybin/getName  is Haskell code
mybin=$HOME/myfile/mybin
fname=$(getName "$PWD")
dir=${fname}Bin
bindir=$mybin/$dir

printc 200 "[fname=$fname]"
printc 200 "[dir=$dir]"
printc 200 "[bindir=$bindir]"

echo "install in => install"
echo "install un => uninstall"

if [[ "$1" == "in" ]]; then
    mkdir "$bindir"

    # cp -av * $bindir   
 
    # Thu 28 May 01:08:15 2020 
    # KEY: rsync exclude directory
    # ignore .git dir
    cd $b
    # -L copy symbolic link as a actual file
    rsync -Lav --exclude '.git' --exclude 'pdf' haskellwebapp2/ $bindir
    rsync -av haskellwebapp2/pdf $bindir

#    cp -av  copy symbolink too
#
#    cp -rf * $bindir
#    stack install --local-bin-path $bindir 
#    cp ./config.txt $bindir 
#    ls -lah $bindir
#    cp *.html $bindir
#    cp -rfP src $bindir
#    cd $bindir
#    ln -s /Library/WebServer/Documents/xfido/pdf pdf
#
    cd $sym
    rm $sym/haskellwebapp2.sh
#
    ln -s $scr/haskellwebapp2.sh haskellwebapp2.sh 
#    ls -lah $mybin
    ls -lah $sym | grep $fname
elif [[ "$1" == "un" ]]; then
    rm -rf $bindir
    cd $sym
    rm $sym/$fname

    printc 200 "remove $bindir"
    printc 200 "remove $sym/$fname"
else 
    printc 300 "Unsupported option"
fi
