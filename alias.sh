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

# LARADOCK
alias work-up="cd ~/Workspace/webserver/laradock && docker-compose up -d nginx mysql redis"
alias work-start="cd ~/Workspace/webserver/laradock && docker-compose start nginx mysql redis"
alias work-stop="cd ~/Workspace/webserver/laradock && docker-compose stop nginx mysql redis"
alias work-restart="cd ~/Workspace/webserver/laradock && docker-compose restart nginx mysql redis"
alias work-bash="cd ~/Workspace/webserver/laradock && docker-compose exec --user=laradock workspace bash"

# FILES
alias hosts="sudo vim /etc/hosts"
alias zshconfig="vim ~/.zshrc"

# TMUX
alias t="tmux"
alias ta="tmux a"
alias tls="tmux ls"
alias tn="tmux new -s "
