#!/bin/sh

status=0
touched_python_files=$(git diff --cached --name-only | grep -E '\.py$')
if [ -n "$touched_python_files" ]; then
    options="\
      --ignore=E26 \
      --max-line-length=150 \
    "

    output=$(autopep8 -d $options "${touched_python_files[@]}")
    status=$?

    if [ -n "$output" ]; then
        autopep8 -i -j 0 $options "${touched_python_files[@]}"
        echo ">>> autopep8 edited some files <<<"
        exit 1
    fi
fi

exit $status
