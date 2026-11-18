#!/bin/bash
# Build the three environments next to the Snakefile (run once, on a login node: needs internet).
#   bash envs/setup.sh
# Needs uv (https://docs.astral.sh/uv/):  curl -LsSf https://astral.sh/uv/install.sh | sh
set -euo pipefail
cd "$(dirname "$0")/.."
command -v uv >/dev/null || { echo "install uv first: curl -LsSf https://astral.sh/uv/install.sh | sh"; exit 1; }
PY=3.12
# keep the (multi-GB) package cache off the small home quota
export UV_CACHE_DIR="${UV_CACHE_DIR:-$PWD/.cache/uv}"   # repo is on scratch
export TORCH_HOME="${TORCH_HOME:-$HOME/.cache/torch}"

uv venv -q -p $PY .venv-sleap
uv pip install -q -p .venv-sleap/bin/python --index-url https://download.pytorch.org/whl/cu128 \
    torch==2.11.0 torchvision==0.26.0
uv pip install -q -p .venv-sleap/bin/python -r envs/sleap.txt

uv venv -q -p $PY .venv-naps
uv pip install -q -p .venv-naps/bin/python -r envs/naps.txt

uv venv -q -p $PY .venv-smk
uv pip install -q -p .venv-smk/bin/python -r envs/snakemake.txt

# ImageNet weights for the ConvNeXt models (compute nodes have no internet)
.venv-sleap/bin/python -c "import torchvision as tv; tv.models.convnext_tiny(weights='DEFAULT')" >/dev/null

.venv-sleap/bin/sleap-nn --help >/dev/null && echo "sleap-nn ok"
.venv-naps/bin/naps-track --help >/dev/null && .venv-naps/bin/naps-color-decodes --help >/dev/null && echo "naps ok"
.venv-smk/bin/snakemake --version
