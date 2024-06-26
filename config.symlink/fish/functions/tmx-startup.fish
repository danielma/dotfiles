function tmx-startup --description "My tmux startup script"
    tmux new-session -d -s inbox \; \
	send-keys -t inbox:0 'c inbox && yarn start' Enter \; \
	new-session -d -s work \; \
	rename-window -t work:0 'tests' \; \
	new-window -t work:1 -n 'console' -d \; \
	attach -t work:0
end
