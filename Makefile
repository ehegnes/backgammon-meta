.PHONY: server client

all: server client

server:
	make -C server run

client:
	make -C client
