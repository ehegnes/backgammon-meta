.PHONY: all test clean backgammon-logic backgammon-logic-wrapper

all: build test

run: build
	stack exec backgammon-server-exe

build: backgammon-logic-wrapper
	stack build

generate-types:
	stack exec generate-types

backgammon-logic-wrapper: backgammon-logic
	cargo build --manifest-path=backgammon-logic-wrapper/Cargo.toml

backgammon-logic:
	cargo build --manifest-path=backgammon-logic/Cargo.toml

test:
	stack test

clean:
	stack clean
	(cd backgammon-logic; cargo clean)
	(cd backgammon-logic-wrapper; cargo clean)

todo:
	grep -nEIr --color=always --exclude-dir='.*' --exclude=Makefile 'XXX|TODO|BUG|FIXME'
