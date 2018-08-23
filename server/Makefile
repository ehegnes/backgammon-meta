.PHONY: all test clean

all: build test

run: build
	stack exec backgammon-server-exe

build: backgammon-logic-wrapper
	stack build

generate-types:
	stack exec generate-types

backgammon-logic-wrapper:
	cargo build --manifest-path=backgammon-logic-wrapper/Cargo.toml

test:
	stack test

clean:
	stack clean

todo:
	grep -nEIr --color=always --exclude-dir='.*' --exclude=Makefile 'XXX|TODO|BUG|FIXME'
