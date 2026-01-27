#!/bin/bash
# test-linux.sh - Run Klar tests in a Linux Docker container
#
# Usage:
#   ./scripts/test-linux.sh           # Run all tests
#   ./scripts/test-linux.sh ffi       # Run only FFI tests
#   ./scripts/test-linux.sh --shell   # Drop into interactive shell
#
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
IMAGE_NAME="klar-linux-test"
CONTAINER_NAME="klar-test-$$"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

info() { echo -e "${BLUE}[INFO]${NC} $1"; }
success() { echo -e "${GREEN}[OK]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; exit 1; }

# Check Docker is available
check_docker() {
    if ! command -v docker &> /dev/null; then
        error "Docker not found. Please install Docker Desktop."
    fi

    if ! docker info &> /dev/null; then
        error "Docker is not running. Please start Docker Desktop."
    fi

    success "Docker is running"
}

# Build the test image
build_image() {
    info "Building Linux test image..."

    docker build -t "$IMAGE_NAME" -f - "$PROJECT_DIR" << 'DOCKERFILE'
FROM ubuntu:22.04

# Avoid interactive prompts
ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies and add LLVM 18 repository
RUN apt-get update && apt-get install -y \
    curl \
    xz-utils \
    build-essential \
    libc6-dev \
    gnupg \
    software-properties-common \
    wget \
    && wget -qO- https://apt.llvm.org/llvm-snapshot.gpg.key | tee /etc/apt/trusted.gpg.d/apt.llvm.org.asc \
    && add-apt-repository -y "deb http://apt.llvm.org/jammy/ llvm-toolchain-jammy-18 main" \
    && apt-get update \
    && apt-get install -y llvm-18 llvm-18-dev clang-18 lld-18 \
    && rm -rf /var/lib/apt/lists/*

# Detect architecture and install Zig 0.14.0
RUN ARCH=$(uname -m) && \
    if [ "$ARCH" = "aarch64" ]; then \
        ZIG_ARCH="aarch64"; \
    else \
        ZIG_ARCH="x86_64"; \
    fi && \
    curl -L "https://ziglang.org/download/0.14.0/zig-linux-${ZIG_ARCH}-0.14.0.tar.xz" | tar -xJ -C /opt && \
    ln -s /opt/zig-linux-${ZIG_ARCH}-0.14.0/zig /usr/local/bin/zig

# Set up LLVM alternatives
RUN update-alternatives --install /usr/bin/llvm-config llvm-config /usr/bin/llvm-config-18 100 \
    && update-alternatives --install /usr/bin/clang clang /usr/bin/clang-18 100 \
    && update-alternatives --install /usr/bin/lld lld /usr/bin/lld-18 100

# Create symlinks for LLVM headers and library (Ubuntu uses versioned paths)
RUN ln -sf /usr/include/llvm-c-18 /usr/include/llvm-c && \
    ln -sf /usr/include/llvm-18 /usr/include/llvm && \
    ln -sf /usr/lib/llvm-18/lib/libLLVM-18.so /usr/lib/llvm-18/lib/libLLVM.so

# Set LLVM_PREFIX for build.zig detection
ENV LLVM_PREFIX=/usr/lib/llvm-18

# Verify installations
RUN zig version && llvm-config --version && ls /usr/include/llvm-c/

WORKDIR /workspace
DOCKERFILE

    success "Image built: $IMAGE_NAME"
}

# Run tests in container
run_tests() {
    local test_type="${1:-all}"

    info "Running tests in Linux container (test type: $test_type)..."

    docker run --rm \
        --name "$CONTAINER_NAME" \
        -v "$PROJECT_DIR:/workspace:delegated" \
        -w /workspace \
        "$IMAGE_NAME" \
        bash -c "
            set -e
            echo '=== Building Klar compiler ==='
            ./build.sh

            echo ''
            echo '=== Running tests ==='
            case '$test_type' in
                ffi)
                    echo 'Running FFI tests...'
                    PASSED=0
                    FAILED=0
                    CRASHED=0
                    for f in test/native/ffi/*.kl; do
                        name=\$(basename \"\$f\" .kl)
                        # Compile the test
                        if ./zig-out/bin/klar build \"\$f\" -o /tmp/test_\$name 2>&1 | grep -q '^Built\\|^Compiling'; then
                            # Run with timeout to catch hangs, capture exit code
                            timeout 5 /tmp/test_\$name 2>&1
                            result=\$?
                            if [ \$result -eq 139 ] || [ \$result -eq 134 ] || [ \$result -eq 136 ]; then
                                echo \"💥 \$name (crashed: signal \$((result - 128)))\"
                                CRASHED=\$((CRASHED + 1))
                            elif [ \$result -eq 124 ]; then
                                echo \"⏰ \$name (timeout)\"
                                FAILED=\$((FAILED + 1))
                            else
                                echo \"✓ \$name (exit: \$result)\"
                                PASSED=\$((PASSED + 1))
                            fi
                        else
                            echo \"✗ \$name (build failed)\"
                            FAILED=\$((FAILED + 1))
                        fi
                        rm -f /tmp/test_\$name
                    done
                    echo \"\"
                    echo \"FFI tests: \$PASSED passed, \$FAILED failed, \$CRASHED crashed\"
                    ;;
                native)
                    ./scripts/run-native-tests.sh
                    ;;
                unit)
                    ./scripts/run-unit-tests.sh
                    ;;
                all)
                    ./run-tests.sh
                    ;;
                *)
                    echo 'Unknown test type: $test_type'
                    echo 'Valid types: all, ffi, native, unit'
                    exit 1
                    ;;
            esac

            echo ''
            echo '=== Linux tests completed ==='
        "
}

# Interactive shell for debugging
run_shell() {
    info "Starting interactive shell in Linux container..."

    docker run --rm -it \
        --name "$CONTAINER_NAME" \
        -v "$PROJECT_DIR:/workspace:delegated" \
        -w /workspace \
        "$IMAGE_NAME" \
        bash
}

# Clean up image
clean() {
    info "Removing test image..."
    docker rmi "$IMAGE_NAME" 2>/dev/null || true
    success "Cleaned up"
}

# Main
main() {
    cd "$PROJECT_DIR"

    case "${1:-}" in
        --help|-h)
            echo "Usage: $0 [command]"
            echo ""
            echo "Commands:"
            echo "  (none)     Run all tests"
            echo "  ffi        Run only FFI tests"
            echo "  native     Run native compilation tests"
            echo "  unit       Run unit tests"
            echo "  --shell    Start interactive shell"
            echo "  --build    Only build the Docker image"
            echo "  --clean    Remove the Docker image"
            echo "  --help     Show this help"
            exit 0
            ;;
        --shell)
            check_docker
            build_image
            run_shell
            ;;
        --build)
            check_docker
            build_image
            ;;
        --clean)
            clean
            ;;
        *)
            check_docker
            build_image
            run_tests "${1:-all}"
            ;;
    esac
}

main "$@"
