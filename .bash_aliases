# Aliases and functions
alias ls='ls -G'
alias lt='ls -ltrho'
alias le='ls -ltrho'
alias la='ls -a'
alias lta='ls -ltrha'

alias Emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
alias emsvr='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon'
alias em='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t'
alias enw='emacs -nw -q --load ~/.shemacs'

alias dui='du -hc -d 1'
alias grep='grep --color=auto'
alias cpi='cp -ri'

alias pdflatex='/Library/TeX/Distributions/Programs/texbin/pdflatex'
alias py='python3'
alias g11='g++ -std=c++11'
alias g14='g++ -std=c++14'
alias g17='g++ -std=c++17'
alias g20='g++ -std=c++2a'

alias gst='git st'
alias gad='git add'
alias gcm='git cm'
alias gca='git ca'
alias gam='git cam'
alias gmd='git amd'
alias gdf='git diff'
alias gdc='git diff --cached'
alias gco='git co'
alias gsh='git sh'
alias epg='pg'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias ..4='cd ../../../..'
alias ..5='cd ../../../../..'
alias ..6='cd ../../../../../..'
alias c..='cl ..'
alias c...='cl ../..'

alias vrnd='rnd vi'
alias clnd='rnd cl'
alias pgnd='rnd pg'
alias icnd='rnd ic .pdf'
alias prnd='rnd python .py'

# Functional alias
function pg { printf `greadlink -f $1` | clip; }
function lg { ls -l | grep "$*" | col 9 | xargs echo; }
function ucp { scp -r uaf:$1 . ; }
function bcp { scp -r t2b:$1 . ; }

function mkcd { mkdir -p "$1" && cd "$1"; }
function mkcp { mkdir -p ${!#} && cpcd $@; }
function mkmv { mkdir -p ${!#} && mv $@; }
function mdcp { mkdir -p ${!#} && cpcd $@; }
function mdmv { mkdir -p ${!#} && mvcd $@; }

# Bash functions
function rnd {
    local nthf=${3:-1}
    if [[ -z $2 ]]; then
        local rndf=`ls -ltr | awk '{if ($5 != 0) print $9}' | tail -n $nthf | head -n 1`
    elif [[ -f $2 ]] || [[ -d $2 ]]; then
        local rndf=$2
    elif [[ $2 == "*/*" ]]; then
        local rndf=`ls -ltr *${2}* | awk '{if ($5 != 0) print $9}' | tail -n $nthf | head -n 1`
    else
        local rndf=`ls -ltr | grep ${2} | awk '{if ($5 != 0) print $9}' | tail -n $nthf | head -n 1`
    fi
    if [[ $rndf != "" ]]; then
        echo "$1 $rndf"
        eval $1 $rndf
        history -s "$1 $rndf"
    else
        echo "Fail to find any random file."
    fi
}

function cl {
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

function cpcd {
    if [ $# -lt 2 ]; then
        echo "Must have at least 2 arguments!"; return 1
    fi
    if [ -d ${!#} ]; then
        cp -r $@ && cd -- ${!#}
    else
        cp -r $@ && cd $(dirname ${!#})
    fi
}

function mvcd {
    if [ $# -lt 2 ]; then
        echo "Must have at least 2 arguments!"; return 1
    fi
    if [ -d ${!#} ]; then
        mv $@ && cd -- ${!#}
    else
        mv $@ && cd $(dirname ${!#})
    fi
}

function gcl {
    if [ ! -z $2 ]; then
        git clone $@ && cl $2
    else
        git clone --recurse-submodules "$1" && cl $(basename "$1" ".git")
    fi
}

function gcp {
    local url=$1
    if [[ $url == "https://github.com/"*  ]]; then
        url=${url/"https://github.com/"/"https://raw.githubusercontent.com/"}
    fi
    echo curl -O $url
    curl -O $url
}

function kajobs {
    local pid=$(jobs -p)
    if [ -n "${pid}" ]; then
        kill -9 $pid
    fi
}

function mkc {
    if [ -f Makefile ]; then
        make clean
    else
        echo rm *.so *.pcm *.d
        rm *.so *.pcm *.d
    fi
}

function dscp {
    # Double scp
    local srcf=$1
    local desf=$2
    local srch=${3:-"hig"}
    local desh=${4:-"hcx1"}
    scp -r $srch:$srcf ~/play/temp/ || return
    local fn=$(basename $srcf)
    scp -r ~/play/temp/$fn $desh:$desf || return
}

function web {
    local fname=$(basename $1)
    local des=${2:-"slides"}
    local machine=${3:-"uaf"}
    if [[ $machine == "uaf" ]]; then
        scp -r $1 uaf:~/public_html/$des
        local addr="http://uaf-8.t2.ucsd.edu/~${SSH_USER}/$des/$fname"
    elif [[ $machine == "ucsb" ]]; then
        scp -r $1 tau:~/htdoc/share/$des
        local addr="http://hep.ucsb.edu/people/${SSH_USER}/share/$des/$fname"
    elif [[ $machine == "lxplus" ]] || [[ $machine == "cern" ]]; then
        scp -r $1 lxplus:~/www/share/$des
        local addr="http://${SSH_USER}.web.cern.ch/${SSH_USER}/share/$des/$fname"
    fi
    echo "Posted online at $addr"
    echo $addr | clip
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

function clip {
    # Escape sequence for iTerm
    read foo
    echo -e "\033]1337;CopyToClipboard=;\a$foo\033]1337;EndCopy\a"
}

