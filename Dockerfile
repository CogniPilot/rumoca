# Stage 1: Build with nightly Rust
FROM rustlang/rust:nightly AS builder

WORKDIR /app

COPY Cargo.toml Cargo.lock ./
COPY src ./src
COPY build.rs ./
COPY modelica.par ./

# Debug output
RUN ls -R /app
RUN cat Cargo.toml

# Build with verbose logging
RUN cargo build --release --verbose

# Stage 2: Runtime image
FROM python:3.12-slim

    
# Install dependencies (for Rust build)
RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    libssl-dev \
    pkg-config \
    bash \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /tests

COPY tests/notebooks/examples notebooks/examples
COPY tests/notebooks/dependencies notebooks/dependencies
COPY tests/models models
COPY tests/templates templates
COPY tests/instructions.md instructions.md

RUN pip install --no-cache-dir -r notebooks/dependencies/requirements.txt
RUN pip install notebooks/dependencies/casadi-3.7.0.dev+main-cp312-none-manylinux2014_x86_64.whl

COPY --from=builder /app/target/release/rumoca /usr/local/bin/rumoca
COPY --from=builder /app/modelica.par /usr/local/bin/modelica.par

EXPOSE 8888

ENTRYPOINT ["/bin/bash"]

