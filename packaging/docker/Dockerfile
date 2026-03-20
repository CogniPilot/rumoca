# syntax=docker/dockerfile:1.7

ARG FOUNDATION_IMAGE=ubuntu:24.04@sha256:d1e2e92c075e5ca139d51a140fff46f84315c0fdce203eab2807c7e495eff4f9

FROM ${FOUNDATION_IMAGE} AS foundation

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    TZ=Etc/UTC

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        bash \
        ca-certificates \
        curl \
        git \
        procps \
        tini \
        tzdata \
    && ln -snf "/usr/share/zoneinfo/${TZ}" /etc/localtime \
    && echo "${TZ}" > /etc/timezone \
    && update-ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY packaging/docker/entrypoint.sh /usr/local/bin/rumoca-entrypoint
RUN chmod 0755 /usr/local/bin/rumoca-entrypoint

WORKDIR /workspace

ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/rumoca-entrypoint"]
CMD ["bash", "-l"]

FROM foundation AS core

ARG PYTHON_VENV=/opt/rumoca/python
ARG JULIA_VERSION=1.11.7
ARG JULIA_URL=https://julialang-s3.julialang.org/bin/linux/x64/1.11/julia-1.11.7-linux-x86_64.tar.gz
ARG JULIA_SHA256=aa5924114ecb89fd341e59aa898cd1882b3cb622ca4972582c1518eff5f68c05

ENV PATH=${PYTHON_VENV}/bin:/opt/julia/bin:${PATH} \
    JULIA_DEPOT_PATH=/opt/rumoca/julia-depot \
    JULIA_PROJECT=/opt/rumoca/julia-env \
    PIP_DISABLE_PIP_VERSION_CHECK=1 \
    PYTHONUNBUFFERED=1

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libgomp1 \
        liblapack3 \
        libopenblas0-pthread \
        python3 \
        python3-venv \
    && rm -rf /var/lib/apt/lists/* \
    && python3 -m venv "${PYTHON_VENV}" \
    && "${PYTHON_VENV}/bin/pip" install --upgrade pip setuptools wheel

COPY packaging/docker/python/core-requirements.txt /tmp/core-requirements.txt
RUN "${PYTHON_VENV}/bin/pip" install --no-cache-dir -r /tmp/core-requirements.txt \
    && rm -f /tmp/core-requirements.txt

RUN mkdir -p /opt/rumoca /opt/rumoca/julia-depot /opt/rumoca/julia-env \
    && curl -fsSL "${JULIA_URL}" -o /tmp/julia.tar.gz \
    && echo "${JULIA_SHA256}  /tmp/julia.tar.gz" | sha256sum -c - \
    && tar -xzf /tmp/julia.tar.gz -C /opt/rumoca \
    && ln -s "/opt/rumoca/julia-${JULIA_VERSION}" /opt/julia \
    && rm -f /tmp/julia.tar.gz

COPY packaging/docker/julia/Project.toml /opt/rumoca/julia-env/Project.toml
COPY packaging/docker/julia/Manifest.toml /opt/rumoca/julia-env/Manifest.toml
RUN JULIA_PKG_PRECOMPILE_AUTO=0 julia -e 'using Dates, Pkg; Pkg.instantiate(); using ModelingToolkit, SciMLBase, Sundials; Pkg.gc(; collect_delay=Day(0))' \
    && rm -rf /root/.julia/logs

FROM core AS ci

ARG OMC_APT_CODENAME=noble

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        gnupg \
    && curl -fsSL http://build.openmodelica.org/apt/openmodelica.asc \
        | gpg --dearmor -o /usr/share/keyrings/openmodelica-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/openmodelica-keyring.gpg] https://build.openmodelica.org/apt ${OMC_APT_CODENAME} stable" \
        > /etc/apt/sources.list.d/openmodelica.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        omc \
    && omc --version \
    && rm -rf /var/lib/apt/lists/*

FROM ci AS dev

ARG PYTHON_VENV=/opt/rumoca/python
ARG CARGO_HOME=/opt/rumoca/cargo
ARG RUSTUP_HOME=/opt/rumoca/rustup
ARG NODE_VERSION=22.22.1
ARG NODE_URL=https://nodejs.org/dist/v22.22.1/node-v22.22.1-linux-x64.tar.xz
ARG NODE_SHA256=9a6bc82f9b491279147219f6a18add1e18424dce90d41d2a5fcd69d4924ba3aa
ARG WASM_PACK_VERSION=0.13.1

ENV CARGO_HOME=${CARGO_HOME} \
    PATH=/opt/node/bin:${CARGO_HOME}/bin:${PYTHON_VENV}/bin:/opt/julia/bin:${PATH} \
    PYO3_PYTHON=${PYTHON_VENV}/bin/python \
    RUSTUP_HOME=${RUSTUP_HOME}

# VS Code desktop extension smoke runs under Electron, so the dev image needs a
# virtual X server plus the usual Linux desktop runtime libraries even in
# otherwise headless CI/devcontainer environments.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential \
        clang \
        libclang-dev \
        libasound2t64 \
        libatk-bridge2.0-0 \
        libdrm2 \
        libgbm1 \
        libgtk-3-0 \
        libnss3 \
        libx11-xcb1 \
        libxcb-dri3-0 \
        libxss1 \
        libxshmfence1 \
        libxtst6 \
        xauth \
        xvfb \
        lld \
        pkg-config \
        python3-dev \
        zip \
        xz-utils \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /opt/rumoca "${CARGO_HOME}" "${RUSTUP_HOME}" \
    && curl -fsSL "${NODE_URL}" -o /tmp/node.tar.xz \
    && echo "${NODE_SHA256}  /tmp/node.tar.xz" | sha256sum -c - \
    && tar -xJf /tmp/node.tar.xz -C /opt/rumoca \
    && ln -s "/opt/rumoca/node-v${NODE_VERSION}-linux-x64" /opt/node \
    && rm -f /tmp/node.tar.xz

COPY rust-toolchain.toml /tmp/rust-toolchain.toml
COPY Cargo.lock /tmp/Cargo.lock
COPY packaging/docker/python/dev-requirements.txt /tmp/dev-requirements.txt

RUN set -euo pipefail \
    && toolchain_channel="$(awk -F'\"' '/^channel/ { print $2 }' /tmp/rust-toolchain.toml)" \
    && curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs -o /tmp/rustup-init.sh \
    && sh /tmp/rustup-init.sh -y --default-toolchain "${toolchain_channel}" --profile minimal \
    && rustup target add wasm32-unknown-unknown \
    && rustup component add rust-src rustfmt clippy \
    && rm -f /tmp/rustup-init.sh

RUN python3 - <<'PY' > /tmp/wasm-bindgen-version
import pathlib
import tomllib

lock = tomllib.loads(pathlib.Path("/tmp/Cargo.lock").read_text())
versions = sorted(
    {
        pkg["version"]
        for pkg in lock.get("package", [])
        if pkg.get("name") == "wasm-bindgen"
    }
)
if not versions:
    raise SystemExit("failed to discover wasm-bindgen version in Cargo.lock")
print(versions[-1])
PY

RUN set -euo pipefail \
    && wasm_bindgen_version="$(cat /tmp/wasm-bindgen-version)" \
    && cargo install wasm-bindgen-cli --version "${wasm_bindgen_version}" --locked \
    && cargo install wasm-pack --version "${WASM_PACK_VERSION}" --locked \
    && "${PYTHON_VENV}/bin/pip" install --no-cache-dir -r /tmp/dev-requirements.txt \
    && rm -rf "${CARGO_HOME}/registry" "${CARGO_HOME}/git" /root/.cargo /root/.rustup \
    && rm -f /tmp/Cargo.lock /tmp/dev-requirements.txt /tmp/rust-toolchain.toml /tmp/wasm-bindgen-version
