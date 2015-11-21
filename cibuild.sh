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
    export PATH=$HOME/.nimble/bin:$PATH
}

installLinuxDependencies()
{

}

installMacOSDependencies()
{

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
    installLinuxDependencies
else
    if [ "$(uname)" = "Darwin" ]
    then
        installMacOSDependencies
    fi
fi

installNim

installNimble

installDependencies

buildTest
