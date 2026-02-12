#!/usr/bin/env bash
# fzf previewer: symlinks, dirs, images (kitty/chafa/imgcat), text, binaries

f="$1"
mime=$(file -b --dereference --mime-type "$f")
category=${mime%%/*}
kind=${mime##*/}

if [ "$kind" = symlink ]; then
    eza -l --no-user --no-permissions --no-time --no-filesize "$f"
elif [ "$category" = inode ]; then
    eza --icons "$f"
elif [ "$category" = image ]; then
    dim=${FZF_PREVIEW_COLUMNS}x${FZF_PREVIEW_LINES}
    if [ "$dim" = x ]; then
        dim=$(stty size < /dev/tty | awk '{print $2 "x" $1}')
    elif ! [ "$KITTY_WINDOW_ID" ] && \
         [ $((FZF_PREVIEW_TOP + FZF_PREVIEW_LINES)) -eq "$(stty size < /dev/tty | awk '{print $1}')" ]; then
        # Avoid scrolling issue when Sixel image touches the bottom of the screen
        dim=${FZF_PREVIEW_COLUMNS}x$((FZF_PREVIEW_LINES - 1))
    fi
    identify -format '%wx%h\n' "$f"
    # 1. kitten icat (Kitty / Ghostty)
    if { [ "$KITTY_WINDOW_ID" ] || [ "$GHOSTTY_RESOURCES_DIR" ]; } && command -v kitten > /dev/null; then
        kitten icat --clear --transfer-mode=memory --unicode-placeholder --stdin=no --place="$dim@0x0" "$f" | sed '$d' | sed $'$s/$/\e[m/'
    # 2. chafa (auto-detects sixel/kitty/iterm/symbols)
    elif command -v chafa > /dev/null; then
        chafa -s "$dim" "$f"
        echo
    # 3. imgcat (iTerm2)
    elif command -v imgcat > /dev/null; then
        imgcat -W "${dim%%x*}" -H "${dim##*x}" "$f"
    else
        file "$f"
    fi
elif [ "$category" = text ]; then
    bat -p --color=always "$f" 2>/dev/null
else
    name=$(basename "$f")
    size=$(stat -c %s "$f" | numfmt --to=iec-i --suffix=B --format="%.2f")
    echo "$name is an $size binary file"
fi
