#!/usr/bin/env bash
seq 1 ${1} | tr ' ' '\n' | sed -E 's/^/chapter /g' | python chapters.py
