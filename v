if [[ -d $1 ]]
then
    rg --files $1 | fzf | xargs edit
elif [[ -f $1 ]]
then
    edit $1
elif [[ ! -z $1 ]]
then
    edit $1
else
    rg --files . | fzf | xargs edit
fi
