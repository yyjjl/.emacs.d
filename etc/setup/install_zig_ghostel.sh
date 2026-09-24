#!/bin/bash
#
# Install Zig 0.16.0 and (re)build the ghostel native module against the
# *local* glibc, so the produced ghostel-module.so is loadable on hosts
# whose glibc is older than the one used for the pre-built GitHub release.
#
# Background: the release binary is linked against a very new glibc (its
# pthread_*/shm_* symbols carry GLIBC_2.32/2.34 version tags because glibc
# 2.34 folded libpthread/librt into libc).  On an older system (e.g. Debian
# buster, glibc 2.28) `module-load' fails with a version-mismatch error.
# Building here with `-Dtarget=<arch>-linux-gnu.<ver>' pins the symbol
# versions to the host glibc and fixes the load failure.
#
# Usage:
#   etc/setup/install_zig_ghostel.sh                 # auto-detect glibc + pkg
#   GLIBC_TARGET=2.28 etc/setup/install_zig_ghostel.sh
#   GHOSTEL_DIR=/path/to/ghostel-XXXX etc/setup/install_zig_ghostel.sh
#
# Honoured env vars:
#   ZIG_VERSION    Zig toolchain version to install (default 0.16.0; ghostel
#                  pins exactly this in build.zig, do not bump casually).
#   GLIBC_TARGET   glibc version to link against (default: detected via ldd).
#   GHOSTEL_DIR    ghostel package dir (default: newest under elpa/).
#   HTTP_PROXY_URL Proxy used to fetch GitHub deps that Zig's HTTP client
#                  cannot reach directly (default: byted relay). GitHub
#                  tarballs go through it; deps.files.ghostty.org stays direct.

set -euo pipefail

CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null && pwd)"

# shellcheck source=./utils.sh
. "${CURRENT_DIR}/utils.sh"

ZIG_VERSION="${ZIG_VERSION:-0.16.0}"
HTTP_PROXY_URL="${HTTP_PROXY_URL:-http://sys-proxy-rd-relay.byted.org:8118}"
# deps.files.ghostty.org is reachable directly and rejects this proxy, so keep
# it (and the loopback) out of any proxying we do for GitHub-hosted deps.
GHOSTTY_DIRECT_HOST="deps.files.ghostty.org"

ZIG_PREFIX="${HOME}/.local/lib/zig-${ZIG_VERSION}"
ZIG_BIN="${HOME}/.local/bin/zig"

arch="$(uname -m)"       # x86_64 / aarch64
os="$(uname -s)"

# --------------------------------------------------------------------------
# 1. Locate the ghostel package directory.
# --------------------------------------------------------------------------
find_ghostel_dir() {
    if [ -n "${GHOSTEL_DIR:-}" ]; then
        echo "${GHOSTEL_DIR}"
        return 0
    fi
    # Newest elpa/ghostel-* checkout that actually carries build.zig.
    local elpa d
    for elpa in "${HOME}/.emacs.d/elpa" "${CURRENT_DIR}/../../elpa"; do
        [ -d "${elpa}" ] || continue
        d="$(ls -d "${elpa}"/ghostel-* 2>/dev/null | sort -V | tail -1 || true)"
        if [ -n "${d}" ] && [ -f "${d}/build.zig" ]; then
            echo "${d}"
            return 0
        fi
    done
    return 1
}

# --------------------------------------------------------------------------
# 2. Install Zig 0.16.0 (exact version; build.zig enforces it).
#    Prefer the `ziglang' PyPI wheel (a static binary, mirrored fast on the
#    byted index); fall back to the official ziglang.org tarball.
# --------------------------------------------------------------------------
install_zig() {
    if [ -x "${ZIG_BIN}" ] && [ "$("${ZIG_BIN}" version 2>/dev/null)" = "${ZIG_VERSION}" ]; then
        info "Zig ${ZIG_VERSION} already installed at ${ZIG_BIN}"
        return 0
    fi
    if command -v zig >/dev/null 2>&1 && [ "$(zig version 2>/dev/null)" = "${ZIG_VERSION}" ]; then
        info "Zig ${ZIG_VERSION} already on PATH ($(command -v zig))"
        ZIG_BIN="$(command -v zig)"
        return 0
    fi

    log "Installing Zig ${ZIG_VERSION} ..."
    local tmp
    tmp="$(mktemp -d)"
    trap 'rm -rf "${tmp}"' RETURN

    if command -v pip >/dev/null 2>&1 && \
       pip download "ziglang==${ZIG_VERSION}" --no-deps -d "${tmp}/wheel" \
           --only-binary=:all: >/dev/null 2>&1; then
        local whl
        whl="$(ls "${tmp}/wheel"/ziglang-*.whl | head -1)"
        info "Fetched Zig wheel: $(basename "${whl}")"
        python3 -m zipfile -e "${whl}" "${tmp}/extract"
        rm -rf "${ZIG_PREFIX}"
        mkdir -p "${ZIG_PREFIX}"
        cp -a "${tmp}/extract/ziglang/zig" "${ZIG_PREFIX}/zig"
        cp -a "${tmp}/extract/ziglang/lib" "${ZIG_PREFIX}/lib"
        chmod +x "${ZIG_PREFIX}/zig"
    else
        warn "PyPI wheel unavailable; falling back to ziglang.org tarball"
        local ztriple url
        case "${os}-${arch}" in
            Linux-x86_64)  ztriple="x86_64-linux" ;;
            Linux-aarch64) ztriple="aarch64-linux" ;;
            Darwin-arm64)  ztriple="aarch64-macos" ;;
            Darwin-x86_64) ztriple="x86_64-macos" ;;
            *) critical "Unsupported platform ${os}-${arch}"; return 1 ;;
        esac
        url="https://ziglang.org/download/${ZIG_VERSION}/zig-${ztriple}-${ZIG_VERSION}.tar.xz"
        info "Downloading ${url}"
        curl -fL --retry 3 -o "${tmp}/zig.tar.xz" "${url}"
        mkdir -p "${tmp}/extract"
        tar -C "${tmp}/extract" --strip-components=1 -xf "${tmp}/zig.tar.xz"
        rm -rf "${ZIG_PREFIX}"
        mkdir -p "${ZIG_PREFIX}"
        cp -a "${tmp}/extract"/. "${ZIG_PREFIX}/"
        chmod +x "${ZIG_PREFIX}/zig"
    fi

    mkdir -p "$(dirname "${ZIG_BIN}")"
    ln -sf "${ZIG_PREFIX}/zig" "${ZIG_BIN}"
    info "Zig ${ZIG_VERSION} installed -> ${ZIG_BIN}"
    "${ZIG_BIN}" version
}

# --------------------------------------------------------------------------
# 3. Detect the glibc version to target.
# --------------------------------------------------------------------------
detect_glibc_target() {
    if [ -n "${GLIBC_TARGET:-}" ]; then
        echo "${GLIBC_TARGET}"
        return 0
    fi
    # `ldd --version' prints e.g. "ldd (Debian GLIBC 2.28-10) 2.28".
    # Capture first, then parse: piping straight into head under
    # `set -o pipefail' races on SIGPIPE and can abort the script.
    local ver_line
    ver_line="$(ldd --version 2>/dev/null)"
    ver_line="${ver_line%%$'\n'*}"
    echo "${ver_line}" | grep -oE '[0-9]+\.[0-9]+' | tail -1
}

# --------------------------------------------------------------------------
# 4. Build the module, prefetching GitHub-hosted deps through the proxy.
#    Zig's HTTP client cannot traverse the byted relay, so on each
#    "github.com/... .tar.gz" failure we curl the tarball via the proxy into
#    the global cache and retry.  deps.files.ghostty.org downloads directly.
# --------------------------------------------------------------------------
build_module() {
    local pkg="$1" target="$2"
    export ZIG_GLOBAL_CACHE_DIR="${ZIG_GLOBAL_CACHE_DIR:-${HOME}/.cache/zig}"
    export ZIG_LOCAL_CACHE_DIR="${pkg}/.zig-cache"

    local run; run="$(mktemp)"
    local attempt url d out h
    for attempt in $(seq 1 40); do
        log "zig build attempt ${attempt} (target ${target}) ..."
        # No proxy in the build env: direct hosts work; GitHub fails and is
        # handled below by an out-of-band curl fetch.
        if ( cd "${pkg}" && env -u http_proxy -u https_proxy -u HTTP_PROXY -u HTTPS_PROXY \
                "${ZIG_BIN}" build --prefix . -Doptimize=ReleaseFast -Dcpu=baseline \
                    -Dtarget="${arch}-linux-gnu.${target}" ) >"${run}" 2>&1; then
            info "Module built successfully"
            rm -f "${run}"
            return 0
        fi

        url="$(grep -oE 'https://(github\.com|codeload\.github\.com)/[^"]+\.tar\.gz' "${run}" | head -1 || true)"
        if [ -z "${url}" ]; then
            critical "Build failed and no GitHub dependency to prefetch:"
            cat "${run}"
            rm -f "${run}"
            return 1
        fi

        info "Prefetching GitHub dep via proxy: ${url}"
        d="$(mktemp -d)"
        out="${d}/${url##*/}"
        if curl -fsSL --retry 3 --max-time 180 --proxy "${HTTP_PROXY_URL}" -o "${out}" "${url}"; then
            # `zig fetch <tarball>' copies it into the global cache under its
            # content hash, so the next build resolves it offline.
            h="$( "${ZIG_BIN}" fetch "${out}" )"
            info "  cached as ${h}"
        else
            critical "Failed to download ${url} via proxy"
            rm -rf "${d}" "${run}"
            return 1
        fi
        rm -rf "${d}"
    done
    critical "Exhausted build attempts"
    rm -f "${run}"
    return 1
}

# --------------------------------------------------------------------------
# 5. Verify the produced module: no glibc symbol newer than the target and
#    it actually loads in Emacs.
# --------------------------------------------------------------------------
verify_module() {
    local pkg="$1" target="$2"
    local so="${pkg}/ghostel-module.so"
    [ -f "${so}" ] || { critical "Module not found: ${so}"; return 1; }

    local tmaj tmin bad
    tmaj="${target%%.*}"; tmin="${target#*.}"
    bad="$(objdump -T "${so}" 2>/dev/null | grep -oE 'GLIBC_[0-9]+\.[0-9]+' | sort -uV | \
        awk -F'[_.]' -v tj="${tmaj}" -v tn="${tmin}" \
            '($2>tj)||($2==tj&&$3>tn){print}')"
    if [ -n "${bad}" ]; then
        critical "Module still requires glibc symbols newer than ${target}:"
        echo "${bad}"
        return 1
    fi
    info "glibc requirement OK (nothing above ${target})"

    local emacs
    emacs="$(command -v emacs || true)"
    if [ -n "${emacs}" ]; then
        if "${emacs}" -Q --batch --eval \
             "(progn (module-load \"${so}\") (princ \"MODULE-LOADED-OK\"))" 2>/dev/null \
             | grep -q MODULE-LOADED-OK; then
            info "Emacs module-load smoke test passed"
        else
            warn "Emacs could not load the module; inspect manually"
        fi
    fi
    info "Done. Restart Emacs to pick up ${so}"
}

main() {
    local pkg target
    if ! pkg="$(find_ghostel_dir)"; then
        critical "Could not locate a ghostel package dir with build.zig (set GHOSTEL_DIR)"
        exit 1
    fi
    info "ghostel package: ${pkg}"

    install_zig

    target="$(detect_glibc_target)"
    if [ -z "${target}" ]; then
        critical "Could not detect system glibc version (set GLIBC_TARGET)"
        exit 1
    fi
    info "Targeting glibc ${target} (${arch}-linux-gnu.${target})"

    build_module "${pkg}" "${target}"
    verify_module "${pkg}" "${target}"
}

main "$@"
