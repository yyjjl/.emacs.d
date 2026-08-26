#!/usr/bin/env python3
"""Build Emacs (and its dependencies) into a private prefix.

Design goals (fixing the old ``progress.json`` based installer):

1. Never rebuild software that is already installed.  Whether a package is
   installed is decided by probing the *actual artifact on disk* (e.g. the
   library or binary it produces) plus a version stamp -- not by a recorded
   "how far did we get last time" state.
2. No fragile per-step progress file.  A package is either fully installed
   (a stamp exists) or it is rebuilt from scratch.  Stamps are written only
   after a successful ``make install``.
3. Changing a version is a one-liner.  Bump the constant (or pass
   ``--emacs-version``); a mismatching stamp automatically triggers a rebuild.

Usage examples::

    ./install_emacs_lite.py                      # install emacs + missing deps
    ./install_emacs_lite.py --emacs-version 31.1 # pick the emacs version
    ./install_emacs_lite.py --force emacs        # force rebuild one package
    ./install_emacs_lite.py --force all          # force rebuild everything
    ./install_emacs_lite.py --only emacs gnutls  # restrict to some packages
    ./install_emacs_lite.py --list               # show status and exit
"""

import argparse
import os
import shutil
import subprocess
import sys
from dataclasses import dataclass, field
from typing import Callable, List, Optional

# --------------------------------------------------------------------------- #
# Version configuration -- edit here to change what gets built.
# --------------------------------------------------------------------------- #

EMACS_VERSION = '31.1'
# The gcc/libgccjit toolchain Emacs native-compilation links against.  Keep
# this in sync with what is actually installed under the prefix -- bumping it
# triggers a (very long) gcc rebuild on the next run.
GCC_VERSION = '11.1.0'
GNUTLS_VERSION = '3.6.16'

# Base URL mirroring https://ftp.gnu.org/gnu.  The official host is often
# painfully slow from CN; a nearby mirror makes downloads fast.  Override with
# --gnu-mirror (e.g. https://ftp.gnu.org/gnu to force the canonical host).
GNU_MIRROR = 'https://mirrors.tuna.tsinghua.edu.cn/gnu'

GNU_SOFTWARES = [
    # name,          version,    archive ext
    ('ncurses',      '6.5',      'tar.gz'),
    ('texinfo',      '6.8',      'tar.gz'),
    ('readline',     '6.3',      'tar.gz'),
    ('gmp',          '6.2.0',    'tar.xz'),
    ('nettle',       '3.4.1',    'tar.gz'),
    ('libunistring', '0.9.10',   'tar.gz'),
    ('libtool',      '2.4',      'tar.xz'),
    ('mpfr',         '4.1.0',    'tar.xz'),
    ('mpc',          '1.2.1',    'tar.gz'),
]

EMACS_CONFIGURE_ARGS = [
    '--with-libgmp=no',
    '--with-native-compilation=yes',
    '--with-x-toolkit=no',
    '--with-xpm=ifavailable',
    '--with-jpeg=ifavailable',
    '--with-png=ifavailable',
    '--with-gif=ifavailable',
    '--with-tiff=ifavailable',
    '--with-tree-sitter=ifavailable',
]


# --------------------------------------------------------------------------- #
# Package model
# --------------------------------------------------------------------------- #

@dataclass
class Package:
    """A buildable component.

    ``marker`` is a path (relative to the install prefix) of a real artifact
    the package produces.  It is used both to skip work when the file is
    already there and to "adopt" pre-existing installs that predate the stamp
    mechanism.  ``{version}`` in ``marker`` is substituted with ``version``.
    """

    name: str
    version: str
    url: str
    marker: str
    configure_args: List[str] = field(default_factory=list)
    # Custom build hook; when set it fully replaces the default
    # configure/make/make-install flow.  Receives (installer,).
    build: Optional[Callable[['Installer'], None]] = None

    @property
    def archive_ext(self) -> str:
        for ext in ('tar.gz', 'tar.xz', 'tar.bz2'):
            if self.url.endswith(ext):
                return ext
        return self.url.rsplit('.', 1)[-1]

    @property
    def archive_name(self) -> str:
        return '{}-{}.{}'.format(self.name, self.version, self.archive_ext)

    @property
    def src_name(self) -> str:
        return '{}-{}'.format(self.name, self.version)

    def marker_path(self, prefix: str) -> str:
        return os.path.join(prefix, self.marker.format(version=self.version))


# --------------------------------------------------------------------------- #
# Installer
# --------------------------------------------------------------------------- #

class Installer:
    def __init__(self, prefix, src_dir, cache_dir, jobs, force):
        self.prefix = os.path.abspath(os.path.expanduser(prefix))
        self.src_dir = os.path.abspath(os.path.expanduser(src_dir))
        self.cache_dir = os.path.abspath(os.path.expanduser(cache_dir))
        self.stamp_dir = os.path.join(self.prefix, '.install-stamps')
        self.jobs = jobs
        self.force = set(force or [])

        for path in (self.prefix, self.src_dir, self.cache_dir, self.stamp_dir):
            os.makedirs(path, exist_ok=True)

        self._setup_environment()

    # -- environment -------------------------------------------------------- #

    def _setup_environment(self):
        prefix = self.prefix

        def prepend(var, value):
            old = os.environ.get(var, '')
            os.environ[var] = value + (os.pathsep + old if old else '')

        prepend('PATH', '{}/bin'.format(prefix))
        os.environ['LDFLAGS'] = '-L{0}/lib -L{0}/lib64'.format(prefix)
        os.environ['CPATH'] = '{}/include'.format(prefix)
        os.environ['LD_LIBRARY_PATH'] = '{0}/lib:{0}/lib64'.format(prefix)
        prepend('PKG_CONFIG_PATH',
                '{0}/lib/pkgconfig:{0}/lib64/pkgconfig'.format(prefix))
        os.environ['CFLAGS'] = '-gdwarf-4'

    # -- stamps / install detection ---------------------------------------- #

    def _stamp_file(self, name):
        return os.path.join(self.stamp_dir, name)

    def _read_stamp(self, name):
        try:
            with open(self._stamp_file(name)) as fp:
                return fp.read().strip()
        except OSError:
            return None

    def _write_stamp(self, pkg):
        with open(self._stamp_file(pkg.name), 'w') as fp:
            fp.write(str(pkg.version))

    def is_installed(self, pkg):
        """Return True when ``pkg`` at its configured version is present."""
        if pkg.name in self.force or 'all' in self.force:
            return False

        marker_ok = os.path.exists(pkg.marker_path(self.prefix))
        stamp = self._read_stamp(pkg.name)

        if stamp is not None:
            # Stamp is authoritative, but only trust it while the artifact it
            # promised is still on disk.
            return stamp == str(pkg.version) and marker_ok

        # No stamp: this may be a legacy install predating the stamp scheme.
        # Adopt it when the real artifact is on disk so we don't rebuild.
        if marker_ok:
            self._write_stamp(pkg)
            return True

        return False

    # -- shell helpers ------------------------------------------------------ #

    @staticmethod
    def _run(cmd, **kwargs):
        printable = cmd if isinstance(cmd, str) else ' '.join(cmd)
        print('  $ {}'.format(printable), flush=True)
        subprocess.run(cmd, check=True, **kwargs)

    def download(self, pkg):
        archive = os.path.join(self.cache_dir, pkg.archive_name)
        if not os.path.exists(archive):
            tmp = archive + '.part'
            try:
                self._run(['wget', '--no-check-certificate',
                           '-c', '-O', tmp, pkg.url])
            except Exception:
                if os.path.exists(tmp):
                    os.unlink(tmp)
                raise
            os.replace(tmp, archive)
        return archive

    def extract(self, pkg, archive):
        src_path = os.path.join(self.src_dir, pkg.src_name)
        if os.path.isdir(src_path):
            return src_path
        opt = {'tar.gz': '-zxf', 'tar.xz': '-Jxf', 'tar.bz2': '-jxf'}.get(
            pkg.archive_ext, '-xf')
        self._run(['tar', opt, archive, '-C', self.src_dir])
        return src_path

    def configure(self, extra_args):
        if not os.path.exists('configure') and os.path.exists('autogen.sh'):
            self._run(['./autogen.sh'])
        self._run(['./configure', '--prefix={}'.format(self.prefix), *extra_args])

    def make_install(self):
        self._run(['make', '-j{}'.format(self.jobs)])
        self._run(['make', 'install'])

    # -- top level ---------------------------------------------------------- #

    def install(self, pkg):
        if self.is_installed(pkg):
            print('[skip] {}-{} already installed'.format(pkg.name, pkg.version))
            return False

        print('[build] {}-{}'.format(pkg.name, pkg.version))
        archive = self.download(pkg)
        src_path = self.extract(pkg, archive)

        cwd = os.getcwd()
        try:
            os.chdir(src_path)
            if pkg.build is not None:
                pkg.build(self)
            else:
                self.configure(pkg.configure_args)
                self.make_install()
        finally:
            os.chdir(cwd)

        if not os.path.exists(pkg.marker_path(self.prefix)):
            raise RuntimeError(
                '{}: build finished but marker {!r} is missing'.format(
                    pkg.name, pkg.marker_path(self.prefix)))

        self._write_stamp(pkg)
        print('[done] {}-{}'.format(pkg.name, pkg.version))
        return True


# --------------------------------------------------------------------------- #
# Package catalogue
# --------------------------------------------------------------------------- #

def gnu_url(mirror, name, version, ext):
    return '{0}/{1}/{1}-{2}.{3}'.format(mirror.rstrip('/'), name, version, ext)


def _build_gcc(installer):
    installer.configure([
        '--disable-bootstrap',
        '--enable-host-shared',
        '--enable-languages=jit,c,c++',
        '--disable-multilib',
        '--with-gmp=' + installer.prefix,
        '--with-mpfr=' + installer.prefix,
        '--with-mpc=' + installer.prefix,
    ])
    installer.make_install()


def build_packages(emacs_version, mirror):
    packages = []

    # Markers are the real artifacts each dep produces in the prefix.
    markers = {
        'ncurses': 'lib/libncurses.a',
        'texinfo': 'bin/makeinfo',
        'readline': 'lib/libreadline.so',
        'gmp': 'lib/libgmp.so',
        'nettle': 'lib64/libnettle.so',
        'libunistring': 'lib/libunistring.so',
        'libtool': 'bin/libtool',
        'mpfr': 'lib/libmpfr.so',
        'mpc': 'lib/libmpc.so',
    }
    for name, version, ext in GNU_SOFTWARES:
        packages.append(Package(
            name=name,
            version=version,
            url=gnu_url(mirror, name, version, ext),
            marker=markers[name],
        ))

    # GnuTLS lives on gnupg's mirror, path keyed by the major.minor series.
    gnutls_series = '.'.join(GNUTLS_VERSION.split('.')[:2])
    packages.append(Package(
        name='gnutls',
        version=GNUTLS_VERSION,
        url='https://www.gnupg.org/ftp/gcrypt/gnutls/v{0}/gnutls-{1}.tar.xz'.format(
            gnutls_series, GNUTLS_VERSION),
        marker='lib/libgnutls.so',
        configure_args=['--with-included-libtasn1', '--without-p11-kit'],
    ))

    packages.append(Package(
        name='gcc',
        version=GCC_VERSION,
        url='{0}/gcc/gcc-{1}/gcc-{1}.tar.xz'.format(mirror.rstrip('/'), GCC_VERSION),
        marker='bin/gcc',
        build=_build_gcc,
    ))

    packages.append(Package(
        name='emacs',
        version=emacs_version,
        url=gnu_url(mirror, 'emacs', emacs_version, 'tar.gz'),
        marker='bin/emacs-{version}',
        configure_args=EMACS_CONFIGURE_ARGS,
    ))

    return packages


# --------------------------------------------------------------------------- #
# CLI
# --------------------------------------------------------------------------- #

def parse_args(argv):
    parser = argparse.ArgumentParser(
        description='Build Emacs and its dependencies into a private prefix.')
    parser.add_argument('--prefix', default='~/.local',
                        help='install prefix (default: %(default)s)')
    parser.add_argument('--src-dir', default='~/program',
                        help='where sources are extracted (default: %(default)s)')
    parser.add_argument('--cache-dir', default='~/program/cache',
                        help='where archives are cached (default: %(default)s)')
    parser.add_argument('--emacs-version', default=EMACS_VERSION,
                        help='emacs version to build (default: %(default)s)')
    parser.add_argument('--gnu-mirror', default=GNU_MIRROR,
                        help='base URL mirroring ftp.gnu.org/gnu (default: %(default)s)')
    parser.add_argument('--jobs', type=int, default=min((os.cpu_count() or 4), 8),
                        help='parallel make jobs (default: %(default)s)')
    parser.add_argument('--only', nargs='+', metavar='PKG',
                        help='only act on these packages')
    parser.add_argument('--force', nargs='+', metavar='PKG', default=[],
                        help="rebuild these packages ('all' for everything)")
    parser.add_argument('--list', action='store_true',
                        help='print install status and exit')
    return parser.parse_args(argv)


def main(argv=None):
    args = parse_args(argv if argv is not None else sys.argv[1:])

    if not shutil.which('wget'):
        sys.exit('error: wget is required but was not found on PATH')

    packages = build_packages(args.emacs_version, args.gnu_mirror)
    if args.only:
        wanted = set(args.only)
        packages = [p for p in packages if p.name in wanted]
        missing = wanted - {p.name for p in packages}
        if missing:
            sys.exit('error: unknown package(s): {}'.format(', '.join(sorted(missing))))

    installer = Installer(
        prefix=args.prefix,
        src_dir=args.src_dir,
        cache_dir=args.cache_dir,
        jobs=args.jobs,
        force=args.force,
    )

    if args.list:
        print('prefix: {}'.format(installer.prefix))
        for pkg in packages:
            state = 'installed' if installer.is_installed(pkg) else 'missing'
            print('  {:<14} {:<8} {}'.format(pkg.name, pkg.version, state))
        return

    built = []
    for pkg in packages:
        if installer.install(pkg):
            built.append('{}-{}'.format(pkg.name, pkg.version))

    print('\nSummary: {} built, {} already present.'.format(
        len(built), len(packages) - len(built)))
    if built:
        print('  built: {}'.format(', '.join(built)))


if __name__ == '__main__':
    main()
