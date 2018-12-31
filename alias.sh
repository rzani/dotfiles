#!/bin/bash
#
# This file provide aliases to simplify my life ;)
#
# @author rzani <rodrigo.zhs@gmail.com>
#

# APT
alias api="sudo apt install"
alias apu="sudo apt update"
alias apug="sudo apt upgrade"

# GIT
alias gpu='git push --set-upstream origin $(git symbolic-ref -q HEAD | cut -d"/" -f 3)'

# DOCKER
alias dps="docker ps --format 'table{{.ID}}\t{{.Names}}\t{{.Ports}}'"
alias dpsa="docker ps -a"
alias dimg="docker images"
alias dexec="docker exec -it"
alias dn="docker network"
alias dc="docker container"
alias dl="docker logs"

# GO
alias go-coverage="go test -coverprofile=coverage.out && go tool cover -html=coverage.out && rm -rf coverage.out"

# Python
alias p3="python3"

# LARADOCK - DEV
alias dev-up="cd ~/Workspace/webserver/dev && docker-compose up -d nginx mariadb redis && cd -"
alias dev-start="cd ~/Workspace/webserver/dev && docker-compose up -d nginx mariadb redis && cd -"
alias dev-stop="cd ~/Workspace/webserver/dev && docker-compose stop && cd -"
alias dev-restart="cd ~/Workspace/webserver/dev && docker-compose restart && cd -"
alias dev-bash="cd ~/Workspace/webserver/dev && docker-compose exec --user=laradock workspace bash && cd -"

# FILES
alias hosts="sudo vim /etc/hosts"
alias zshconfig="vim ~/.zshrc"

# TMUX
alias t="tmux"
alias ta="tmux a"
alias tls="tmux ls"
alias tn="tmux new -s "

# SERVERS
alias ns2="ssh ns2 -t byobu"
alias ns1="ssh ns1 -t byobu"

