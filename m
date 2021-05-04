#!/usr/bin/env bash
cmd_list="play stop next prev toggle add insert volume seek repeat single consume random clear crop shop-start shop-stop"

# TODO: dmenu integration. idk how to detect I'm in dmenu and switch to that...
menu() {
    prompt=$1
    shift
    fzf \
        --preview 'mpc status && echo queue: && mpc queued' \
        --prompt "$prompt> " \
        $@
}

select_song() {
    file=$(mpc listall | menu "enqueue")
    [[ -z $file ]] && exit 1
    mpc "$1" "$file"
    mpc play
}

cmd=$(echo $cmd_list | sed 's/ /\n/g' | menu "mpd")

[[ -z $cmd ]] && exit 1

needs_selection=(add insert)
if [[ " ${needs_selection[@]} " =~ $cmd  ]]; then
    select_song $cmd
elif [[ $cmd == shop-stop ]]; then
    systemctl --user stop shop-music.service
elif [[ $cmd == shop-start ]]; then
    systemctl --user start shop-music.service
else
    mpc $cmd
fi
