#!/usr/bin/env bash

core_apt_packages="sdcv xsel aspell git"

for pkg in $core_apt_packages; do
    if apt-package-installed "$pkg"; then
        info "Package $pkg installed"
    else
        info "Installing $pkg ..."
        sudo apt install -y "$pkg"
    fi
done
