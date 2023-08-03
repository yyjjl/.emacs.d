#!/bin/bash

base=${HOME}/.emacs.d/.cache
output=${base}/LanguageTool
url=https://languagetool.org/download/LanguageTool-stable.zip

tmp=${output}.$$

trap "rm -f ${tmp}" EXIT

mkdir -p ${output}
curl -SL --progress-bar $url -o ${tmp}
unzip ${tmp} -d ${output}

target=(${output}/*)
if [ ${#target[@]} == 1 ] && [ -d ${target[0]} ]; then
    echo "Link ${target[0]} => ${base}/LT"
    ln -sf ${target[0]} ${base}/LT
fi
