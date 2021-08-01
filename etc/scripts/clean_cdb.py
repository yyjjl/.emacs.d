# -*- coding: utf-8 -*-

import argparse
import json
import logging
import os

IGNORE_PREFIXES = ['.blade_tools']
ALLOW_PREFIXES = ['-W', '-I', '-D']

parser = argparse.ArgumentParser()
parser.add_argument(
    '--driver-mapping-path', default='../cpp_drivers.json'
)
parser.add_argument(
    'workspace',
    help='The workspace path'
)


def main(argv=None):
    args = parser.parse_args(argv)

    driver_mapping = {}
    if os.path.exists(args.driver_mapping_path):
        try:
            driver_mapping = json.load(open(args.driver_mapping_path))
            logging.info('%s is loaded', args.driver_mapping_path)
        except Exception:
            pass

    ignore_prefixes = driver_mapping.get('ignore', [])
    ignore_prefixes.extend(IGNORE_PREFIXES)

    allow_prefixes = driver_mapping.get('allow', [])
    allow_prefixes.extend(ALLOW_PREFIXES)

    compdb_path = os.path.join(
        args.workspace, 'build64_{}'.format(driver_mapping.get('mode', 'release')),
        'compile_commands.json'
    )
    compdb = json.load(open(compdb_path))

    new_compdb = []
    for item in compdb:
        if not item['output'].endswith('.o'):
            # Skip linkage commands.
            continue

        tokens = item['command'].split(' ')
        if any(tokens[0].startswith(prefix) for prefix in ignore_prefixes):
            tokens = tokens[1:]

        if not tokens[0].startswith('/opt/tiger'):
            logging.fatal('strange file: %s %s', item['file'], tokens[0])
            continue

        tokens[0] = driver_mapping.get(tokens[0], tokens[0])

        command = [tokens[0], '-fsyntax-only']

        for token in tokens[1:]:
            if any(token.startswith(prefix) for prefix in allow_prefixes):
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
    logging.basicConfig(level=logging.INFO)

    main()
