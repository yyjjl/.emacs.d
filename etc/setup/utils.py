# -*- coding: utf-8 -*-

import logging
import os
import platform
import shutil

import requests

logging.basicConfig(level=logging.INFO)

logger = logging.getLogger(os.path.basename(__file__))

def get_system_type():
    system_type = platform.platform().lower()
    if system_type.startswith('linux'):
        return 'linux'
    if system_type.startswith('macos'):
        return 'macos'
    return system_type

def get_github_release_info(owner, repo):
    url = 'https://api.github.com/repos/{owner}/{repo}/releases/latest'.format(
        owner=owner,
        repo=repo
    )
    return requests.get(url).json()

def download_github_assets(owner, repo, output_dir, filter_fn):
    release_info = get_github_release_info(owner, repo)

    output_files = []
    for asset_info in release_info['assets']:
        if not filter_fn(asset_info):
            continue

        name = asset_info['name']
        url = asset_info['browser_download_url']

        path = os.path.join(output_dir, name)
        if os.path.exists(path):
            output_files.append(path)
            print(f'INFO {path} exists')
            continue

        try:
            rsp = requests.get(url, stream=True)
            with open(path, 'wb') as fp:
                for data in rsp.iter_content(1024):
                    fp.write(data)

            output_files.append(path)
        except:
            try:
                os.unlink(path)
            except:
                pass
            raise

    return output_files


def run_command(command, strict=True, directory=None):
    if directory is not None:
        prev_directory = os.path.abspath(os.curdir)

        logger.info('CD: %s', directory)
        os.chdir(directory)

    logger.info('Run: %s', command)
    code = os.system(command)

    if directory is not None:
        os.chdir(prev_directory)

    if code != 0:
        if strict:
            raise RuntimeError(f'failed to run: {command}')
        logger.warning('failed to run: %s', command)

    return code


def install_from_git(url, build_commands, output_dir, project=None):
    if project is None:
        project = url.rsplit('/', 1)[1]
        if project.endswith('.git'):
            project = project[:-4]

    target_dir = os.path.join(os.path.abspath(output_dir), project)

    if os.path.exists(target_dir):
        prompt = 'Pull'
        command = 'git pull && git submodule update'
        directory = target_dir
    else:
        prompt = 'Clone'
        command = f'git clone --depth=1 --recursive {url} {target_dir}'
        directory = None

    logger.info('%s from %s', prompt, url)
    run_command(command, directory=directory)

    os.chdir(target_dir)
    for command in build_commands:
        run_command(command)


def try_link_file(source_file, target_file, copy=False):
    if os.path.exists(target_file):
        backup_index = 1
        while True:
            backup_file = target_file + f'.backup.{backup_index}'
            if not os.path.exists(backup_file):
                break
            backup_index += 1

        logger.info('move %s to %s', target_file, backup_file)
        shutil.move(target_file, backup_file)
    else:
        os.makedirs(os.path.dirname(target_file), exist_ok=True)

    if not copy:
        os.symlink(source_file, target_file)
    else:
        shutil.copy(source_file, target_file)
