#!/usr/bin/python3

import json
import os
import subprocess

class Installer(object):
    def __init__(self,
                 prefix=None,
                 progress_path='progress.json'):
        if prefix is None:
            prefix = os.path.expanduser('~/.local')

        self._prefix = prefix
        self._progress = {}
        self._progress_path = progress_path

    def progress(self, tag, value=None):
        if value is None:
            return self._progress.get(tag)
        self._progress[tag] = value

    def _load(self):
        prefix = self._prefix
        os.environ['PATH'] = '{}/bin:{}'.format(
            prefix,
            os.environ.get('PATH', ''))
        os.environ['LDFLAGS'] = '-L{0}/lib -L{0}/lib64'.format(prefix)
        os.environ['CPATH'] = '{}/include'.format(prefix)
        os.environ['LD_LIBRARY_PATH'] = '{}/lib'.format(prefix)
        os.environ['PKG_CONFIG_PATH'] = '{}/lib/pkg_config_path'.format(
            prefix,
            os.environ.get('PKG_CONFIG_PATH', '')
        )

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
            for index, process in enumerate(processes, 1):
                name = process[0]
                cmd = process[1]
                args = process[2:]
                if hasattr(cmd, '__name__'):
                    print(index, cmd.__name__, args)
                else:
                    print(index, cmd, args)

                if name is None:
                    cmd(*args)
                    continue

                if self.progress(tag) == last_progress:
                    if not cmd(*args):
                        raise Exception('{} can not be {}'.format(tag, name))
                    print('{} is {}'.format(tag, name))
                    self.progress(tag, name)

                last_progress = name
        finally:
            os.chdir(current_dir)

    def make(self, target_dir=None):
        args = []
        if target_dir is not None:
            args = ['-C', target_dir]

        ret = subprocess.run(['make', '-j', *args])

        if ret.returncode != 0:
            return False

        ret = subprocess.run(['make', *args, 'install'])

        return ret.returncode == 0

    def configure(self, *args):
        if os.path.exists('autogen.sh'):
            ret = subprocess.run(['./autogen.sh'])
            if ret.returncode != 0:
                return False

        ret = subprocess.run(
            ['./configure', '--prefix={}'.format(self._prefix), *args]
        )

        return ret.returncode == 0

    def cmake(self, *args, build_dir='build'):
        os.makedirs(build_dir, exist_ok=True)

        src_dir = os.path.abspath(os.curdir)
        try:
            os.chdir(build_dir)

            ret = subprocess.run(
                ['cmake', src_dir,
                 '-DCMAKE_INSTALL_PREFIX={}'.format(self._prefix),
                 *args]
            )

            return ret.returncode == 0
        finally:
            os.chdir(src_dir)

def wget_and_uncompress(url, opt):
    wget = subprocess.Popen(['wget', '-O-', url], stdout=subprocess.PIPE)
    ret = subprocess.run(['tar', opt], stdin=wget.stdout)
    return ret.returncode == 0

def download_gnu_software(name, version, ext):
    directory = '{}-{}'.format(name, version)
    url = 'https://ftp.gnu.org/gnu/{}/{}.{}'.format(name, directory, ext)
    if ext == 'tar.gz':
        opt = '-zx'
    elif ext == 'tar.xz':
        opt = '-Jx'
    else:
        opt = '-x'

    return wget_and_uncompress(url, opt)

def download_gnutls(version):
    directory = 'gnutls-{}.{}'.format(*version)
    url = 'https://www.gnupg.org/ftp/gcrypt/gnutls/v{0}/gnutls-{0}.{1}.tar.xz'.format(*version)
    return wget_and_uncompress(url, '-Jx')


def download_from_github(repo, directory):
    url = 'https://github.com/' + repo
    if os.path.exists(directory):
        pass
        


GNU_SOFTWARES = [
    ('ncurses', '6.2', 'tar.gz'),
    ('texinfo', '6.8', 'tar.gz'),
    ('readline', '6.3', 'tar.gz'),
    ('gmp', '6.2.0', 'tar.xz'),
    ('nettle', '3.4.1', 'tar.gz'),
    ('libunistring', '0.9.10', 'tar.gz'),
]
GNUTLS = ('3.6', '16')


os.chdir('/home/yyj/Downloads/src')
with Installer(prefix='/home/yyj/Downloads/root') as installer:
    for name, version, ext in GNU_SOFTWARES:
        installer.run(
            name,
            ['downloaded', download_gnu_software, name, version, ext],
            [None, os.chdir, '{}-{}'.format(name, version)],
            ['configured', installer.configure],
            ['built', installer.make],
        )

    installer.run(
        'gnutls',
        ['downloaded', download_gnutls, GNUTLS],
        [None, os.chdir, 'gnutls-{}.{}'.format(*GNUTLS)],
        ['configured', installer.configure],
        ['built', installer.make],
    )
    
