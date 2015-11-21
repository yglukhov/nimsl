#!/bin/sh

echo "BUILD.sh"
env

set -ev

installNim()
{
    git clone "https://github.com/nim-lang/Nim" ~/nim
    cd ~/nim
    sh bootstrap.sh
    export PATH=$PWD/bin:$PATH
    cd -
}

installNimble()
{
    git clone https://github.com/nim-lang/nimble.git ~/nimble
    cd ~/nimble
    nim c -r src/nimble install
    cd -
    PATH=$HOME/.nimble/bin:$PATH
}

installDependencies()
{
    nimble install -y
}

buildTest()
{
    nim c -r test.nim
}

if [ "$(uname)" = "Linux" ]
then
    # Install deps on Linux
else
    if [ "$(uname)" = "Darwin" ]
    then
        # Install deps on MacOS
    fi
fi
#sudo -n apt-get update
#sudo -n apt-get install -y libsdl2-dev


echo Install Nim
installNim

echo Install Nimble
installNimble

echo Install dependencies
installDependencies

echo Build test
buildTest
