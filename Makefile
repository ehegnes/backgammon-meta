all: backgammon-logic backgammon-server

backgammon-logic:
	cargo build --manifest-path=backgammon-logic/Cargo.toml

backgammon-server:
	make -C server
