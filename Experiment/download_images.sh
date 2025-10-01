#!/usr/bin/env bash
cut -d, -f2 image_paths.csv | tail -n +2 | while read path; do curl -o "$path" --create-dirs "https://gershmanlab.com/experiments/yang/toc/Experiment/$path"; done