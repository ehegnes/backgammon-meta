.PHONY: all test clean backgammon-logic backgammon-logic-wrapper

all: build test

run: build
	stack exec backgammon-server-exe

build: backgammon-logic-wrapper
	stack build

generate-types: build
	stack exec generate-types

backgammon-logic-wrapper: backgammon-logic
	cargo build --manifest-path=backgammon-logic-wrapper/Cargo.toml

backgammon-logic:
	cargo build --manifest-path=backgammon-logic/Cargo.toml

test: backgammon-logic-wrapper
	stack test

clean:
	stack clean
	(cd backgammon-logic; cargo clean)
	(cd backgammon-logic-wrapper; cargo clean)

todo:
	grep -nEIr --color=always --exclude-dir='.*' --exclude=Makefile 'XXX|TODO|BUG|FIXME'

gdb-test: backgammon-logic-wrapper
	-RUST_BACKTRACE=1 stack test --no-strip
	gdb --args .stack-work/dist/*/*/build/backgammon-server-test/backgammon-server-test +RTS -V0

gdb-exe: backgammon-logic-wrapper
	stack build --no-strip
	gdb --args .stack-work/dist/*/*/build/backgammon-server-exe/backgammon-server-exe +RTS -V0
