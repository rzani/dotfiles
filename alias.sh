#!/bin/bash
#
# This file provide aliases to simplify my life ;)
#
# @author rzani <rodrigo.zhs@gmail.com>
#

# APT
alias api="sudo apt-get install"
alias apu="sudo apt-get update"
alias apug="sudo apt-get upgrade"

# GIT
alias gpu="git push --set-upstream origin $(git symbolic-ref -q HEAD | cut -d"/" -f 3)"

# DOCKER
alias dps="docker ps"
alias dpsa="docker ps -a"
alias dimg="docker images"
alias dexec="docker exec -it"

# DOCKER MACHINE
alias dm-ccook-legado='docker-machine start cybercook-legado && eval "$(docker-machine env cybercook-legado)"'
alias dm-ccook='docker-machine start cybercook && eval "$(docker-machine env cybercook)"'
alias dm-web='docker-machine start web && eval "$(docker-machine env web)"'

# NODE
alias node="docker run -it --rm --name nodejs -v "$PWD":/usr/src/app -w /usr/src/app node:slim node"
alias npm="docker run -it --rm --name nodejs -v "$PWD":/usr/src/app -w /usr/src/app node:slim npm"
alias bower="docker run -it --rm --name bower -v "$PWD":/srv marmelab/bower bash -c bower --allow-root --config.interactive=false"

# FILES
alias hosts="sudo vim /etc/hosts"
alias zshconfig="vim ~/.zshrc"

## WEB CONTAINER
alias php7="docker exec -it web_php7_1"
alias php5="docker exec -it web_php5_1"

# EMACS
alias emacs="XLIB_SKIP_ARGB_VISUALS=1 /usr/local/bin/emacs"

#famosos
alias famoso1="ssh famoso1 -t byobu"
alias famoso2="ssh famoso2 -t byobu"
alias famoso3="ssh famoso3 -t byobu"
alias famoso4="ssh famoso4 -t byobu"
alias famoso5="ssh famoso5 -t byobu"

alias deploy-ccook="ssh famoso5 'cd /var/www/html/cybercook && git pull'";
alias deploy-vm="ssh famoso4 'cd /var/www/html/vilamulher && git pull'";
alias deploy-me="ssh famoso3 'cd /var/www/html/maisequilibrio && git pull'";

#bailes
alias baile1="ssh baile1 -t byobu"
alias baile2="ssh baile2 -t byobu"
alias baile3="ssh baile3 -t byobu"
alias baile4="ssh baile4 -t byobu"
