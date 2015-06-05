LOG="$HOME/profile-invocations"
echo "-----" >>$LOG
echo "Caller: $0" >>$LOG
echo "DESKTOP_SESSION: $DESKTOP_SESSION" >>$LOG
echo "GDMSESSION: $GDMSESSION" >>$LOG


#if [ "$0" = "/etc/gdm/Xsession" ]; then
#    eval $(gnome-keyring-daemon --start)
#    export SSH_AUTH_SOCK GPG_AGENT_INFO GNOME_KEYRING_CONTROL GNOME_KEYRING_PID
#    export SSH_AGENT_PID=$GNOME_KEYRING_PID
#fi

export PATH=/home/markus/.local/bin:/home/markus/local/bin:$PATH
