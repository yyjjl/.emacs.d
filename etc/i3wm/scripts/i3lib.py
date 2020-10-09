# -*- coding: utf-8 -*-

import json
import subprocess


def command_output(cmd):
    output = []
    if cmd:
        p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
        for line in p.stdout.readlines():
            output.append(line.rstrip().decode())
    return json.loads(''.join(output))


def find_windows(tree, window_list=None):
    if window_list is None:
        window_list = []
    if "nodes" in tree and tree["nodes"]:
        for node in tree["nodes"]:
            find_windows(node, window_list)
    elif "floating_nodes" in tree and tree["floating_nodes"]:
        for node in tree["floating_nodes"]:
            find_windows(node, window_list)
    else:
        if tree.get("layout", "dockarea") != "dockarea" and \
           not tree.get("name", "").startswith("i3bar for output") and \
           tree.get("window"):
            window_list.append(tree)

    return window_list


def focus_window(app):
    subprocess.Popen(['i3-msg', '[id="%d"] focus' % (app['window'])],
                     stdout=subprocess.PIPE)


def get_workspaces(key='focused'):
    w_list = command_output(['i3-msg', '-t', 'get_workspaces'])
    if key:
        w_list = list(filter(lambda x: x[key], w_list))
    return w_list


def find_workspaces_with_name(tree, names, window_list=None):
    if window_list is None:
        window_list = []
    if tree.get('name') in names:
        window_list.append(tree)
    else:
        for node in tree.get("nodes", []):
            find_workspaces_with_name(node, names, window_list)
    return window_list


def get_windows(key='focused'):
    root = command_output(['i3-msg', '-t', 'get_tree'])
    if key:
        names = list(map(lambda x: x['name'], get_workspaces(key)))
        nodes = find_workspaces_with_name(root, names)
    else:
        nodes = [root]
    return sum([find_windows(node) for node in nodes], [])
