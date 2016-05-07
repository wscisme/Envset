alias ls='ls -G'
alias lt='ls -ltrh'
alias la='ls -a'
alias lta='ls -ltrha'

alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
alias emsvr='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon'
alias em='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t'
alias enw='emacs -nw -q --load ~/.shemacs'

alias dui='du -hc -d 1'

alias pdflatex='/Library/TeX/Distributions/Programs/texbin/pdflatex'
alias py='python3.5'

alias gst='git st'
alias gad='git add'
alias gcm='git cm'
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
cl() {
    cd $1 && lt
}

ei() {
    if [ -f "$1" ]; then
	enw "$1" --eval '(setq buffer-read-only t)' --eval '(god-local-mode 1)'
    else
	echo "File $1 not found."
    fi
}

mkcd() {
  mkdir "$1" && cd "$1"
}

mkpdf(){
    local FILENAME="$1" #| cut -d'.' --complement -f2-
    FILENAME=$(echo $FILENAME | cut -d'.' --complement -f2-)
    local LATEXFILENAME="$FILENAME.tex"
    local PDFFILENAME="$FILENAME.pdf"

    pdflatex $LATEXFILENAME && open $PDFFILENAME 
}
