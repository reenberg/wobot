#!/bin/sh

for DOTFILE in `ls -d dotfiles/*`
do 
    dot -Tpdf -oimages/`basename $DOTFILE .dot`.pdf $DOTFILE
done


