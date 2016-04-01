alias ls='ls -G'
alias lt='ls -ltrh'
alias la='ls -a'
alias lta='ls -ltrha'

alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
alias em='/Applications/Emacs.app/Contents/MacOS/Emacs -nw -q --load ~/.shemacs'

alias dui='du -hc -d 1'

alias pdflatex='/Library/TeX/Distributions/Programs/texbin/pdflatex'
alias py='python3.5'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias ..3='cd ../../..'
alias ..4='cd ../../../..'
alias ..5='cd ../../../../..'
alias ..6='cd ../../../../../..'
alias ..7='cd ../../../../../../..'

#Functional alias
ei() {
    if [ -f "$1" ]
    then
	emacs -q "$1" --eval '(setq buffer-read-only t)'
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
