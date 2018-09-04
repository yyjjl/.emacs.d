#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys

missing = []
for m in sys.argv[1:]:
    try:
        __import__(m)
    except ImportError as e:
        missing.append(m)
if len(missing) > 0:
    print(*missing, end='')
