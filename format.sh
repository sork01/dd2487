#!/bin/bash

# usage: ./format.sh [target-path] [target-path] ...
# 
# if target-path(s) is a file, format the single file, otherwise
# format all clojure source in directory tree (recursive) pointed
# to by target-path(s)
# 
# if target-path is omitted the default "src test" is used

if [ ! -f "./zprintl-0.4.12" ] || [ ! -x "./zprintl-0.4.12" ]; then
    echo "Please make sure you have the file ./zprintl-0.4.12 and that it is executable"
    exit
fi

targets="src test"
gitcheck="yes"

usertargets=""

if [ "$#" -gt 0 ]; then
    while [ "$#" -gt 0 ]; do
        case "$1" in
        "-h" | "--help")
            echo "usage: ./format.sh [target-path]"
            echo ""
            echo "if target-path is a file, format the single file, otherwise"
            echo "format all clojure source in directory tree (recursive) pointed"
            echo "to by target-path"
            echo ""
            echo "if target-path is omitted the default \"src\" is used"
            exit
            ;;
        "-y" | "--yolo")
            gitcheck="no"
            ;;
        *)
            usertargets="$usertargets $1"
        esac
        
        shift
    done
fi

if [ ! "$usertargets" == "" ]; then
    targets="$usertargets"
fi

OLDIFS=$IFS
IFS=" "

targets=$(echo "$targets")

IFS=$OLDIFS

for target in $targets; do
    if [ "$gitcheck" == "yes" ]; then
        gitstatus=$(git status --porcelain "$target" 2>&1)
        
        if [ ! "$gitstatus" == "" ]; then
            echo "Working directory '$target' is not clean (use [-y, --yolo] to disable this check), aborting!"
            exit
        fi
    fi
    
    for f in $(find "$target" | grep -P '\.clj(.+)?$'); do
        code=$(cat "$f")
        formatted=$(echo "$code" | ./zprintl-0.4.12)
        status="$?"
        
        if [ ! "$status" == "0" ]; then
            echo "$f: formatter exited with nonzero status code $status, aborting!"
            exit
        fi
        
        if [ "$formatted" == "" ]; then
            echo "$f: formatter produced no output, aborting!"
            exit
        fi
        
        if [ ! "$code" == "$formatted" ]; then
            echo Formatted: "$f"
            echo "$formatted" > "$f"
        fi
    done
done
