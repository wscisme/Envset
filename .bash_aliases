alias ls='ls -G'
alias lt='ls -ltrho'
alias le='ls -ltrho'
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
alias g14='g++ -std=c++14'
alias g17='g++ -std=c++1z'

alias pdflatex='/Library/TeX/Distributions/Programs/texbin/pdflatex'
alias py='python3.5'

alias gst='git st'
alias gad='git add'
alias gcm='git cm'
alias gca='git ca'
alias gam='git cam'
alias gdf='git diff'
alias gco='git co'
alias gsh='git sh'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias ..4='cd ../../../..'
alias ..5='cd ../../../../..'
alias ..6='cd ../../../../../..'
alias c..='cl ..'
alias c...='cl ../..'

#Functional alias and new functions
rnd() {
    local nthf=1
    if [[ -z $2 ]]; then
        local rndf=`ls -ltr | awk '{if ($5 != 0) print $9}' | tail -n $nthf`
    elif [[ $2 == "*/*" ]]; then
        local rndf=`ls -ltr *${2}* | awk '{if ($5 != 0) print $9}' | tail -n $nthf`
    else
        local rndf=`ls -ltr | grep ${2} | awk '{if ($5 != 0) print $9}' | tail -n $nthf`
    fi
    if [[ $rndf != "" ]]; then
        echo "$1 $rndf"
        $1 $rndf
        history -s "$1 $rndf"
    else
        echo "Fail to find any random file."
    fi
}

cl() {
    if [[ -z $1 ]]; then
        ls -ltrho
    elif [[ -d $1 ]] || [[ $1 == '-' ]]; then
        cd $1 && ls -ltrho
    elif [[ -f $1 ]]; then
        cd $(dirname $1) && ls -ltrho
    elif [[ -f "$*" ]]; then
        cd $(dirname "$*") && ls -ltrho
    else
        local des=$(echo $1 | cut -d'/' --complement -f2-)
        if [[ -d $des ]]; then
            ls -ltrho $des
            echo "\"$1\" is not a directory or file, do list of \"$des\" instead"
        else
            echo "$1: No such directory or file"; return 1
        fi
    fi
}

lg() {
    ls -l | grep "$*" | col 9 | xargs echo
}

mkcd() {
    mkdir -p "$1" && cd "$1"
}

mkcp() {
    mkdir -p ${!#} && cpcd $@
}

mkmv() {
    mkdir -p ${!#} && mv $@
}

mdcp() {
    mkdir -p ${!#} && cpcd $@
}

mdmv() {
    mkdir -p ${!#} && mvcd $@
}

cpcd() {
    if [ $# -lt 2 ]; then
        echo "Must have at least 2 arguments!"; return 1
    fi
    if [ -d ${!#} ]; then
        cp -r $@ && cd -- ${!#}
    else
        cp -r $@ && cd $(dirname ${!#})
    fi
}

cpcl() {
    cpcd $@ && ls -ltrhG
}

mvcd() {
    if [ $# -lt 2 ]; then
        echo "Must have at least 2 arguments!"; return 1
    fi
    if [ -d ${!#} ]; then
        mv $@ && cd -- ${!#}
    else
        mv $@ && cd $(dirname ${!#})
    fi
}

gcl() {
    if [ ! -z $2 ]; then
        git clone $@ && cl $2
    else
        git clone "$1" && cl $(basename "$1" ".git")
    fi
}

kajobs() {
    local pid=$(jobs -p)
    if [ -n "${pid}" ]; then
        kill -9 $pid
    fi
}

ucp() {
    scp -r uaf:$1 .
}

web() {
    local fname=$(basename $1)
    local des=${2:-"slides"}
    local machine=${3:-"uaf"}
    if [[ $machine == "uaf" ]]; then
        scp -r $1 uaf:~/public_html/$des
        local addr="http://uaf-8.t2.ucsd.edu/~${SSH_USER}/$des/$fname"
    elif [[ $machine == "lxplus" ]]; then
        scp -r $1 lxplus:~/www/share/$des
        local addr="http://${SSH_USER}.cern.ch/${SSH_USER}/share/$des/$fname"
    fi
    echo "Posted online at $addr"
}

mkc() {
    if [ -f Makefile ]; then
        make clean
    else
        echo rm *.so *.pcm *.d
        rm *.so *.pcm *.d
    fi
}

function col {
    if [ $# -lt 1 ]; then
        echo "usage: col <col #>"; return 1
    fi
    if [[ $1 -lt 0 ]]; then
        awk "{print \$(NF+$(($1+1)))}"
    else
        awk -v x=$1 '{print $x}'
    fi
}

# Enable C-s for isearch-forward
stty -ixon
