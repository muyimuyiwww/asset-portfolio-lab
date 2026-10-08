#!/bin/zsh
set -e
cd "${0:A:h}"
command -v python3 >/dev/null || { echo '请先安装 Python 3'; exit 1; }
command -v Rscript >/dev/null || { echo '请先安装 R'; exit 1; }
mkdir -p work .Rlib
python3 -m venv .venv
.venv/bin/python -m pip install -r requirements.lock
RENV_PATHS_ROOT="$PWD/work/renv" Rscript -e 'options(timeout=300);install.packages("renv",lib=".Rlib",repos="https://cloud.r-project.org");.libPaths(c(".Rlib",.libPaths()));renv::restore(library=".Rlib",prompt=FALSE)'
