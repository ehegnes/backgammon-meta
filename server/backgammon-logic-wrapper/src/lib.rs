#![allow(unused_imports)]
extern crate backgammon_logic;

use backgammon_logic::player::Player;
use backgammon_logic::game::{Dice};
use backgammon_logic::board::{Point, MaybePoint};
use backgammon_logic::board::{InternalBoard};

pub use backgammon_logic::game::Die;

use std::convert::From;

pub const PLAYER_BLACK: u8 = 0;
pub const PLAYER_WHITE: u8 = 1;

#[repr(C)]
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
pub struct RustMaybePoint(Option<Box<RustPoint>>);

impl From<MaybePoint> for RustMaybePoint {
    fn from(maybe_point: MaybePoint) -> Self {
        RustMaybePoint(maybe_point.map(|x| Box::new(RustPoint::from(x))))
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
