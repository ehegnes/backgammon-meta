all: backgammon-server backgammon-client

backgammon-server:
	make -C server

backgammon-client:
	make -C client
