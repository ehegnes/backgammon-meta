all: build run

run:
	stack exec backgammon-server-exe

build: backgammon-logic
	stack build

backgammon-logic:
	cargo build --manifest-path=backgammon-logic-wrapper/Cargo.toml

clean:
	stack clean
