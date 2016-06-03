alias ls='ls -G'
alias lt='ls -ltrh'
alias lk='ls -ltrho'
alias la='ls -a'
alias lta='ls -ltrha'
alias lc='cl'

alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
alias emsvr='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon'
alias em='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t'
alias enw='emacs -nw -q --load ~/.shemacs'

alias dui='du -hc -d 1'
alias grep='grep --color=auto'
alias cpi='cp -ri'

alias pdflatex='/Library/TeX/Distributions/Programs/texbin/pdflatex'
alias py='python3.5'

alias gst='git st'
alias gad='git add'
alias gcm='git cm'
alias gca='git ca'
alias gam='git cam'
alias gdf='git diff'
alias gco='git co'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias ..3='cd ../../..'
alias ..4='cd ../../../..'
alias ..5='cd ../../../../..'
alias ..6='cd ../../../../../..'
alias ..7='cd ../../../../../../..'
alias c..='cl ..'
alias c...='cl ../..'

#Functional alias
ei() {
    if [ -f "$1" ]; then
	enw "$1" --eval '(setq buffer-read-only t)' --eval '(god-local-mode 1)'
    else
	echo "File $1 not found."
    fi
}

cl() {
    if [[ -z $1 ]]; then
        ls -ltrho
    elif [[ -d $1 ]]; then
        cd $1 && ls -ltrho
    elif [[ -f $1 ]]; then
        cd $(dirname $1) && ls -ltrho
    else
        echo "$1: No such directory or file"; return 1
    fi
}

mkcd() {
    mkdir -p "$1" && cd "$1"
}

cpcd() {
    if [ $# -lt 2 ]; then
        echo "Must have at least 2 arguments!"; return 1
    fi
    if [ -d ${!#} ]; then
        cp -r $@ && cd -- ${!#}
    else
        cp -r $@ && $(dirname ${!#})
    fi
}

mkpdf(){
    local FILENAME="$1" #| cut -d'.' --complement -f2-
    FILENAME=$(echo $FILENAME | cut -d'.' --complement -f2-)
    local LATEXFILENAME="$FILENAME.tex"
    local PDFFILENAME="$FILENAME.pdf"

    pdflatex $LATEXFILENAME && open $PDFFILENAME
}

# Enable C-s for isearch-forward
stty -ixon
