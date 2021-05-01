#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "usage $0 <output-file>"
    exit 1
fi

if [ -d ".git" ]; then
    FILES=($(git ls-files  --full-name --))
else
    FILES=($(find . -type f))
fi

{
    for src in ${FILES[@]}; do
        case "${src}" in
            *.ad[absm]|*.[CFHMSacfhlmpsty]|*.def|*.in[cs]|*.s[as]|*.src|*.cc|*.hh|*.[chy]++|*.[ch]pp|*.[chy]xx|*.pdb|*.[ch]s|*.[Cc][Oo][Bb]|*.[eh]rl|*.f90|*.for|*.java|*.[cem]l|*.clisp|*.lisp|*.[Ll][Ss][Pp]|[Mm]akefile*|*.pas|*.[Pp][LlMm]|*.psw|*.lm|*.pc|*.prolog|*.oak|*.p[sy]|*.sch|*.scheme|*.[Ss][Cc][Mm]|*.[Ss][Mm]|*.bib|*.cl[os]|*.ltx|*.sty|*.TeX|*.tex|*.texi|*.texinfo|*.txi|*.x[bp]m|*.yy| *.[Ss][Qq][Ll])
                etags -o- "${src}";
                ;;
            *)
                FTYPE=$(file "${src}");
                case "${FTYPE}" in
                    *script*text*)
                        etags -o- "${src}";
                        ;;
                    *text*)
                        if head -n1 "${src}" | grep '^#!' >/dev/null 2>&1;
                        then
                            etags -o- "${src}";
                        fi;
                        ;;
                esac;
                ;;
        esac;
    done;
} > "$1"
