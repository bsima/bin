#!/usr/bin/env bash
cmd_list="play stop next prev toggle add insert volume seek repeat single consume random clear crop shop-start shop-stop"

# TODO: dmenu integration. idk how to detect I'm in dmenu and switch to that...
menu() {
    prompt=$1
    shift
    fzf \
        --bind "ctrl-a:select-all" \
        --preview 'mpc status && echo queue: && mpc playlist' \
        --prompt "$prompt> " \
        $@
}

cmd=$(echo $cmd_list | sed 's/ /\n/g' | menu "mpd")

[[ -z $cmd ]] && exit 1

needs_selection=(add insert)
if [[ $cmd == insert  ]]; then
    mpc listall | menu "insert" | mpc insert
    mpc play
elif [[ $cmd == add ]]; then
    mpc listall | menu "add" --multi | mpc add
    mpc play
elif [[ $cmd == shop-stop ]]; then
    systemctl --user stop shop-music.service
elif [[ $cmd == shop-start ]]; then
    systemctl --user start shop-music.service
else
    mpc $cmd
fi
