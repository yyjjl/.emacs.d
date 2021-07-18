#!/usr/bin/env bash

KEYBINDING_DIR=~/Library/KeyBindings

echo "Checking for $KEYBINDING_DIR"
mkdir -p $KEYBINDING_DIR

FILE=~/Library/KeyBindings/DefaultKeybinding.dict
BACKUP_FILE="${FILE}.back.$(date +%Y%m%d_%H%M%S)"

if [ -f $FILE ]; then
    echo "Found existing $FILE"
    echo "copying to $BACKUP_FILE"
    cp $FILE $BACKUP_FILE
fi

cat <<-EOF > $FILE
/* ~/Library/KeyBindings/DefaultKeyBinding.dict */
{
    "~f"      = "moveWordForward:";              /* M-f */
    "~b"      = "moveWordBackward:";             /* M-b */
    "~<"      = "moveToBeginningOfDocument:";    /* M-< */
    "~>"      = "moveToEndOfDocument:";          /* M-> */
    "~v"      = "pageUp:";                       /* M-v */
    "^v"      = "pageDown:";                     /* C-v */
    "~d"      = "deleteWordForward:";            /* M-d */
    "~^h"     = "deleteWordBackward:";           /* M-C-h */
    "~\010"   = "deleteWordBackward:";           /* M-backspace */
    "~\177"   = "deleteWordBackward:";           /* M-delete */
    "~\UF728" = "deleteWordForward:";            /* delete */
    "\UF729"  = "moveToBeginningOfDocument:";    /* home */
    "\UF72B"  = "moveToEndOfDocument:";          /* end */
    "@\UF729" = "moveToBeginningOfParagraph:";   /* A-home */
    "@\UF72B" = "moveToEndOfParagraph:";         /* A-end */
    "@\UF700" = "moveToBeginningOfDocument:";    /* A-up */
    "@\UF701" = "moveToEndOfDocument:";          /* A-down */
    "^\UF700" = "pageUp:";                       /* C-up */
    "^\UF701" = "pageDown:";                     /* C-down */
    "^/"      = "undo:";                         /* C-/ */
    "~c"      = "capitalizeWord:";               /* M-c */
    "~u"      = "uppercaseWord:";                /* M-u */
    "~l"      = "lowercaseWord:";                /* M-l */
    "^t"      = "transpose:";                    /* C-t */
    "~t"      = "transposeWords:";               /* M-t */
    "~/"      = "complete:";                     /* M-/ */
    "^g"      = "_cancelKey:";                   /* C-g */
    "^a"      = "moveToBeginningOfLine:";        /* C-a */
    "^e"      = "moveToEndOfLine:";              /* C-e */   
"~w" = "copy:";
"^w" = "cut:";
"^y" = "paste:";
}
EOF
