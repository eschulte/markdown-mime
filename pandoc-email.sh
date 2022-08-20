#!/bin/bash
#
# Script to run pandoc with premailer to convert markdown to HTML
# appropriate for an email body.  Expects input form STDIN and writes
# output to STDOUT.  Set `markdown-command' to the path to this
# script.
#
pandoc --embed-resources --standalone \
       --css $(dirname $0)/pandoc-email.css \
       -V title:"" --metadata title="email" \
       2>/dev/null \
    |python3 -m premailer 2>/dev/null
