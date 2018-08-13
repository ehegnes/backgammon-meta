#![allow(unused_imports)]
extern crate backgammon_logic;
extern crate arrayvec;

use backgammon_logic::player::Player;
use backgammon_logic::game::{Dice};
use backgammon_logic::board::{Point, MaybePoint};
use backgammon_logic::board::{Board, InternalBoard, INITIAL_BOARD};

pub use backgammon_logic::game::Die;
pub use backgammon_logic::constants::*;

use arrayvec::ArrayVec;

use std::convert::From;

pub const PLAYER_BLACK: u8 = 0;
pub const PLAYER_WHITE: u8 = 1;

#[repr(C)]
#[derive(Debug, Clone, PartialEq)]
pub enum RustPlayer {
    Black,
    White,
}

impl From<Player> for RustPlayer {
    fn from(player: Player) -> Self {
        match player {
            Player::Black => RustPlayer::Black,
            Player::White => RustPlayer::White,
        }
    }
}

#[repr(C)]
pub struct RustDice {
    d1: Die,
    d2: Die,
}

impl From<Dice> for RustDice {
    fn from(dice: Dice) -> Self {
        RustDice {
            d1: dice.0,
            d2: dice.1,
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct RustPoint {
    owner: RustPlayer,
    count: u8,
}

impl From<Point> for RustPoint {
    fn from(point: Point) -> Self {
        RustPoint {
            owner: RustPlayer::from(point.owner),
            count: point.count,
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct RustMaybePoint(Option<Box<RustPoint>>);

impl From<MaybePoint> for RustMaybePoint {
    fn from(maybe_point: MaybePoint) -> Self {
        RustMaybePoint(maybe_point.map(|x| Box::new(RustPoint::from(x))))
    }
}

#[repr(C)]
#[derive(Clone, Debug)]
pub struct RustInternalBoard([RustMaybePoint; BOARD_SIZE]);

impl From<InternalBoard> for RustInternalBoard {
    fn from(internal_board: InternalBoard) -> Self {
        let internal_board = internal_board
            .into_iter()
            .map(|x| RustMaybePoint::from(*x))
            .collect::<Vec<RustMaybePoint>>();
        let a: ArrayVec<_> = internal_board.into_iter().collect();
        let a: [RustMaybePoint; BOARD_SIZE] = a.into_inner().unwrap();
        RustInternalBoard(a)
    }
}

#[no_mangle]
pub extern fn init_player() -> Box<RustPlayer> {
    Box::new(RustPlayer::Black)
}

#[no_mangle]
pub extern fn init_point() -> Box<RustPoint> {
    Box::new(RustPoint { owner: RustPlayer::White, count: 5 })
}

#[no_mangle]
pub extern fn init_some_point() -> RustMaybePoint {
    let point = Some(Point { owner: Player::Black, count: 2 });
    RustMaybePoint::from(point)
}

#[no_mangle]
pub extern fn init_none_point() -> RustMaybePoint {
    let point = None;
    RustMaybePoint::from(point)
}

#[no_mangle]
pub extern fn init_internal_board() -> Box<RustInternalBoard> {
    let player = Player::Black;
    Box::new(RustInternalBoard::from(Board::init().board(player)))
}
