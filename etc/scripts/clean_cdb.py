# -*- coding: utf-8 -*-

import argparse
import json
import logging
import os

IGNORE_PREFIXES = ['.blade_tools']
ALLOW_PREFIXES = ['-W', '-I', '-D']

parser = argparse.ArgumentParser()
parser.add_argument(
    '--mode', '-m',
    default='release',
    choices=['debug', 'release'],
    help='The blade build profile.'
)
parser.add_argument(
    'workspace',
    help='The workspace path'
)


def main(argv=None):
    args = parser.parse_args(argv)

    compdb_path = os.path.join(
        args.workspace, 'build64_{}'.format(args.mode),
        'compile_commands.json'
    )
    compdb = json.load(open(compdb_path))

    new_compdb = []
    for item in compdb:
        if not item['output'].endswith('.o'):
            # Skip linkage commands.
            continue

        tokens = item['command'].split(' ')
        if any(tokens[0].startswith(prefix) for prefix in IGNORE_PREFIXES):
            tokens = tokens[1:]

        if not tokens[0].startswith('/opt/tiger'):
            logging.fatal('strange file: %s %s', item['file'], tokens[0])
            continue

        command = [tokens[0], '-fsyntax-only', '--std=c++14']

        for token in tokens[1:]:
            if any(token.startswith(prefix) for prefix in ALLOW_PREFIXES):
                command.append(token)

        command.append(item['file'])

        item['command'] = ' '.join(command)
        new_compdb.append(item)

    output_path = os.path.join(args.workspace, 'compile_commands.json')
    if os.path.exists(output_path):
        os.unlink(output_path)

    with open(output_path, 'w') as fp:
        json.dump(new_compdb, fp, indent=2)


if __name__ == '__main__':
    main()
