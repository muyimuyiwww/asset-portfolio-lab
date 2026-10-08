#!/bin/zsh
set -e
cd "${0:A:h}"
command -v Rscript >/dev/null || { echo '请先安装 R'; exit 1; }
mkdir -p work
Rscript start.R
