#!/usr/bin/python3

import json
import os
import subprocess
from glob import glob


def run_cmd(cmd, **kwargs):
    print('run_cmd', cmd)
    return subprocess.run(cmd, **kwargs).returncode == 0


def download_and_uncompress(url, save_path, opt=None):
    if not os.path.exists(save_path):
        if not run_cmd(['wget', '--no-check-certificate', '-O', save_path, url]):
            try:
                os.unlink(save_path)
            except Exception:
                pass
            return False

    if opt is None:
        return True

    return run_cmd(['tar', opt, save_path])


class Installer(object):
    def __init__(self, prefix=None, progress_path='progress.json', cache_path='cache'):
        if prefix is None:
            prefix = os.path.expanduser('~/.local')

        self._prefix = os.path.abspath(prefix)
        self._progress = {}
        self._progress_path = progress_path
        self._cache_path = cache_path

        os.makedirs(self._cache_path, exist_ok=True)

    @property
    def prefix(self):
        return self._prefix

    @property
    def cache_path(self):
        return self._cache_path

    def progress(self, tag, value=None):
        if value is None:
            return self._progress.get(tag)
        self._progress[tag] = value

    def _load(self):
        prefix = self._prefix
        os.environ['PATH'] = '{}/bin:{}'.format(
            prefix, os.environ.get('PATH', '')
        )
        os.environ['LDFLAGS'] = '-L{0}/lib -L{0}/lib64'.format(prefix)
        os.environ['CPATH'] = '{}/include'.format(prefix)
        os.environ['LD_LIBRARY_PATH'] = '{0}/lib:{0}/lib64'.format(prefix)
        os.environ['PKG_CONFIG_PATH'] = '{0}/lib/pkgconfig:{0}/lib64/pkgconfig:{1}'.format(
            prefix,
            os.environ.get('PKG_CONFIG_PATH', '')
        )
        os.environ['CFLAGS'] = '-gdwarf-4'

        if os.path.exists(self._progress_path):
            with open(self._progress_path) as fp:
                self._progress = json.load(fp)

    def _save(self):
        with open(self._progress_path, 'w') as fp:
            json.dump(self._progress, fp, indent=2)

    def __enter__(self, *args, **kwargs):
        self._load()
        return self

    def __exit__(self, *args, **kwargs):
        self._save()

    def run(self, tag, *processes):
        last_progress = None

        current_dir = os.path.abspath(os.curdir)
        try:
            for process in processes:
                cmd = process[0]
                args = process[1:]
                name = cmd.__qualname__

                print(name, args)
                if not name.startswith('Installer.'):
                    cmd(*args)
                    continue

                if self.progress(tag) == last_progress:
                    if not cmd(*args):
                        raise Exception('{}: {} failed'.format(tag, name))
                    print('{}: {} succeeded'.format(tag, name))
                    self.progress(tag, name)

                last_progress = name
        finally:
            os.chdir(current_dir)

    def extract_and_link(self, pattern, exe, bin_dir):
        archive = glob(os.path.join(self._cache_path, pattern))[0]
        directory = os.path.basename(archive[:-7])
        exe_path = os.path.join(bin_dir, exe)
        run_cmd(['tar', 'zxf', archive])
        if os.path.exists(exe_path):
            os.unlink(exe_path)
        os.symlink(os.path.abspath(os.path.join(directory, exe)), exe_path)

    def make(self, target_dir=None):
        args = []
        if target_dir is not None:
            args = ['-C', target_dir]

        if not run_cmd(['make', '-j4', *args]):
            return False

        return run_cmd(['make', *args, 'install'])

    def configure(self, *args):
        if not os.path.exists('configure'):
            assert run_cmd(['./autogen.sh'])
        return run_cmd(['./configure', '--prefix={}'.format(self._prefix), *args])

    def cmake(self, *args, build_dir='build'):
        os.makedirs(build_dir, exist_ok=True)

        src_dir = os.path.abspath(os.curdir)
        try:
            os.chdir(build_dir)

            return run_cmd([
                'cmake', src_dir,
                '-DCMAKE_INSTALL_PREFIX={}'.format(self._prefix),
                *args
            ])
        finally:
            os.chdir(src_dir)

    def download_software(self, name, version, ext, fmt):
        directory = '{}-{}'.format(name, version)
        save_path = os.path.join(self._cache_path, '{}.{}'.format(directory, ext))

        url = fmt.format(name=name, version=version, ext=ext)

        if ext == 'tar.gz':
            opt = '-zxf'
        elif ext == 'tar.xz':
            opt = '-Jxf'
        else:
            opt = '-xf'

        return download_and_uncompress(url, save_path, opt)

    def download_gnu_software(self, name, version, ext):
        fmt = 'http://ftp.gnu.org/gnu/{name}/{name}-{version}.{ext}'
        return self.download_software(name, version, ext, fmt)

    def download_gnutls(self, version):
        save_path = os.path.join(
            self._cache_path, 'gnutls-{}.{}.tar.xz'.format(*version)
        )
        url = 'http://www.gnupg.org/ftp/gcrypt/gnutls/v{0}/gnutls-{0}.{1}.tar.xz'.format(
            *version
        )
        return download_and_uncompress(url, save_path,  '-Jxf')

    def download_gcc(self, version):
        save_path = os.path.join(
            self._cache_path, 'gcc-{}.tar.xz'.format(version)
        )
        url = 'http://ftp.gnu.org/gnu/gcc/gcc-{0}/gcc-{0}.tar.xz'.format(
            version
        )
        return download_and_uncompress(url, save_path,  '-Jxf')

    def download_and_install_cmake(self, version):
        save_path = os.path.join(
            self._cache_path, 'cmake-{0}-linux-x86_64.sh'.format(version)
        )
        url = 'https://github.com/Kitware/CMake/releases/download/v{0}/cmake-{0}-linux-x86_64.sh'.format(
            version
        )

        if not download_and_uncompress(url, save_path):
            return False

        return run_cmd(
            ['bash', save_path, '--prefix={}'.format(self._prefix)],
            input=b'y\nn\n'
        )


def download_from_github(repo, directory):
    url = 'https://github.com/' + repo + '.git'

    current_dir = os.path.abspath(os.curdir)
    if os.path.exists(directory):
        try:
            os.chdir(directory)
            return run_cmd(['git', 'pull'])
        finally:
            os.chdir(current_dir)
    else:
        return 0 == os.system('git clone {} {}'.format(url, directory))


GNU_SOFTWARES = [
    ('ncurses', '6.2', 'tar.gz'),
    ('texinfo', '6.8', 'tar.gz'),
    ('readline', '6.3', 'tar.gz'),
    ('gmp', '6.2.0', 'tar.xz'),
    ('nettle', '3.4.1', 'tar.gz'),
    ('libunistring', '0.9.10', 'tar.gz'),
    ('libtool', '2.4', 'tar.xz'),
    ('mpfr', '4.1.0', 'tar.xz'),
    ('mpc', '1.2.1', 'tar.gz')
]
GNUTLS = ('3.6', '16')
GCC = '11.1.0'
CMAKE = '3.21.0'
ZLIB = '1.2.13'

PYTHON_URL = 'https://www.python.org/ftp/python/3.9.7/Python-3.9.7.tgz'

SRC_DIR = os.path.expanduser('~/program/')
ROOT_DIR = os.path.expanduser('~/.local/')
BIN_DIR = os.path.join(ROOT_DIR, 'bin')

os.makedirs(SRC_DIR, exist_ok=True)
os.makedirs(ROOT_DIR, exist_ok=True)
os.makedirs(BIN_DIR, exist_ok=True)

os.chdir(SRC_DIR)
with Installer(prefix=ROOT_DIR) as installer:
    # installer.extract_and_link('fd-*', 'fd', BIN_DIR)
    # installer.extract_and_link('ripgrep-*', 'rg', BIN_DIR)
    # run_cmd(['tar', 'zxf', glob(os.path.join(installer.cache_path, 'fzf-*'))[0], '-C', BIN_DIR])

    installer.download_and_install_cmake(CMAKE)

    for name, version, ext, *cmd_args in GNU_SOFTWARES:
        installer.run(
            name,
            [installer.download_gnu_software, name, version, ext],
            [os.chdir, '{}-{}'.format(name, version)],
            [installer.configure, *cmd_args],
            [installer.make],
        )

    installer.run(
        'zlib',
        [installer.download_software, 'zlib', ZLIB, 'tar.gz', 'http://www.zlib.net/{name}-{version}.{ext}'],
        [os.chdir, 'zlib-{}'.format(ZLIB)],
        [installer.configure],
        [installer.make],
    )

    installer.run(
        'gnutls',
        [installer.download_gnutls, GNUTLS],
        [os.chdir, 'gnutls-{}.{}'.format(*GNUTLS)],
        [
            installer.configure,
            '--with-included-libtasn1',
            '--without-p11-kit'
        ],
        [installer.make],
    )

    installer.run(
        'gcc',
        [installer.download_gcc, GCC],
        [os.chdir, 'gcc-{}'.format(GCC)],
        [
            installer.configure,
            '--disable-bootstrap',
            '--enable-host-shared',
            '--enable-languages=jit,c,c++',
            '--disable-multilib',
            '--with-gmp=' + installer.prefix,
            '--with-mpfr=' + installer.prefix,
            '--with-mpc=' + installer.prefix
        ],
        [installer.make],
    )

    installer.run(
        'libvterm',
        [download_from_github, 'neovim/libvterm', 'libvterm'],
        [os.chdir, 'libvterm'],
        [subprocess.run, ['make']],
        [subprocess.run,
         ['make', 'install', 'PREFIX={}'.format(installer.prefix)]]
    )

    installer.run(
        'jansson',
        [download_from_github, 'akheron/jansson', 'jansson'],
        [os.chdir, 'jansson'],
        [installer.cmake, '-DJANSSON_BUILD_DOCS=OFF'],
        [os.chdir, 'build'],
        [installer.make]
    )

    installer.run(
        'tree-sitter',
        [download_from_github, 'tree-sitter/tree-sitter', 'tree-sitter'],
        [os.chdir, 'tree-sitter'],
        [subprocess.run, ['make']],
        [subprocess.run,
         ['make', 'install', 'PREFIX={}'.format(installer.prefix)]]
    )

    installer.run(
        'emacs',
        [download_from_github, 'emacs-mirror/emacs', 'emacs'],
        [os.chdir, 'emacs'],
        [
            installer.configure,
            '--with-json',
            '--with-libgmp=no',
            '--with-native-compilation=yes',
            '--with-x-toolkit=no',
            '--with-xpm=ifavailable',
            '--with-jpeg=ifavailable',
            '--with-gif=ifavailable',
            '--with-tiff=ifavailable',
            '--with-tree-sitter'
        ],
        [installer.make]
    )

    installer.run(
        'ctags',
        [download_from_github, 'universal-ctags/ctags', 'universal-ctags'],
        [os.chdir, 'universal-ctags'],
        [installer.configure],
        [installer.make]
    )
