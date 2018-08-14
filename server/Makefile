.PHONY: all test clean

all: build test run

run:
	stack exec backgammon-server-exe

build: backgammon-logic-wrapper
	stack build

backgammon-logic-wrapper:
	cargo build --manifest-path=backgammon-logic-wrapper/Cargo.toml

test:
	stack test

clean:
	stack clean
