#! /bin/bash
INSTALL_DIR=./electron-app/build/bin
STATIC_RESOURCES=./frontend/static

echo "Clean install directory $INSTALL_DIR"
rm -rf $INSTALL_DIR
mkdir $INSTALL_DIR

echo "build backend library"
cabal build backend

echo "install frontend into $INSTALL_DIR"
cabal install frontend --install-method=copy --installdir $INSTALL_DIR

echo "copy frontend resources to $INSTALL_DIR"
cp -r $STATIC_RESOURCES $INSTALL_DIR