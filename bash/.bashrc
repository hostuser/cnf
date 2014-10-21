. /etc/profile

# completion
#source /usr/share/git/git-prompt.sh
#complete -cf sudo
#complete -cf man

#complete -o default -o nospace -F _git_branch gb
#complete -o default -o nospace -F _git_checkout gco

# functions


# aliases 

alias gs='git status '
alias gc='git commit -a -m '
alias gca='git commit -a '
alias ga='git add '
alias gco='git checkout '
alias gb='git branch '
alias pullm='git pull origin master'
alias pull='git pull origin develop'
alias pushm='git push origin master'
alias push='git push origin develop'
alias pushr='git push origin release'
alias pullr='git pull origin release'
alias m='mvn -DskipTests=true clean install'
alias h='history|grep '

f_ikill() {
		ps aux | percol | awk '{ print $2 }' | xargs kill 
}
alias ikill=f_ikill



alias alert='notify-send --urgency=low -i "$([ $? = 0 ] \
  && echo terminal || echo error)" "$(history|tail -n1| \
  sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

#alias e='emacsclient -a "" -t '
alias zile='emacsclient -a "" -t '
#alias emacs='emacsclient -a "" -c --no-wait'

alias bashrc='e $HOME/.bashrc'

alias fgr='find .|grep -v .svn|grep -v .git|grep '

# exports
export PATH=~/local/bin:/opt/gradle/bin/:/opt/maven/bin:$PATH
#export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64
export JAVA_HOME=/usr/lib/jvm/java-8-oracle/

#export EDITOR='/usr/bin/emacs -nw '
export EDITOR='/usr/bin/emacsclient -a "" -t '

export IDEA_JDK=/usr/lib/jvm/java-8-oracle
export STUDIO_JDK="$IDEA_JDK"
export WEBIDE_JDK="$IDEA_JDK"

echo;fortune;echo

# history
export HISTCONTROL=ignoreboth
export HISTSIZE=10000
#export PROMPT_COMMAND=history -a
#export PROMPT_COMMAND="$PROMPT_COMMAND; history -a; history -n;"

shopt -s histappend
source /usr/share/autojump/autojump.bash

