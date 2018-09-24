#![allow(unused_imports)]
extern crate backgammon_logic;
extern crate arrayvec;

use backgammon_logic::player::Player;
use backgammon_logic::game::{Dice};
use backgammon_logic::board::{Board, INITIAL_BOARD, Point, MaybePoint};
use backgammon_logic::moves::{Move, Submove};

pub use backgammon_logic::game::Die;
pub use backgammon_logic::constants::*;
pub use backgammon_logic::board::{Position};

use arrayvec::ArrayVec;

use std::convert::From;
use std::str::FromStr;
use std::os::raw::c_void;

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

mod sm {
    use super::Position;

    #[repr(C)]
    #[derive(Debug, Clone)]
    pub struct BearOff {
        pub from: Position,
    }

    #[repr(C)]
    #[derive(Debug, Clone)]
    pub struct Enter {
        pub to: Position,
    }

    #[repr(C)]
    #[derive(Debug, Clone)]
    pub struct Move {
        pub from: Position,
        pub to: Position,
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
/// `Box<T>` is required here because we can't pass structs by-value to Haskell
pub enum RustSubmove {
    BearOff(Box<sm::BearOff>),
    Enter(Box<sm::Enter>),
    Move(Box<sm::Move>),
}

impl From<Submove> for RustSubmove {
    fn from(submove: Submove) -> Self {
        match submove {
            Submove::BearOff { from } =>
                RustSubmove::BearOff(Box::new(sm::BearOff { from })),
            Submove::Enter { to } =>
                RustSubmove::Enter(Box::new(sm::Enter { to })),
            Submove::Move { from, to } =>
                RustSubmove::Move(Box::new(sm::Move { from, to })),
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
#[derive(Clone)]
pub struct RustBoard {
    board: [RustMaybePoint; BOARD_SIZE],
    bar_black: u8,
    bar_white: u8,
}

impl From<Board> for RustBoard {
    fn from(board: Board) -> Self {
        RustBoard {
            board: {
                let internal_board = board.board
                    .into_iter()
                    .map(|x| RustMaybePoint::from(*x))
                    .collect::<Vec<RustMaybePoint>>();
                let a: ArrayVec<_> = internal_board.into_iter().collect();
                let a: [RustMaybePoint; BOARD_SIZE] = a.into_inner().unwrap();
                a
            },
            bar_black: board.bar_black,
            bar_white: board.bar_white,
        }
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct RustMove {
    submoves: [*const c_void ; 4],
}

/// We can be sure that a validated move has 4 or fewer submoves
impl From<Move> for RustMove {
    fn from(m: Move) -> Self {
        let length = m.submoves.len();
        RustMove {
            submoves: {
                let m = m.submoves
                    .clone()
                    .into_iter()
                    .map(|x| Box::new(RustSubmove::from(x)))
                    .collect::<Vec<Box<RustSubmove>>>();
                let mut m = m.into_iter().map(|x| Box::into_raw(x) as *const c_void).collect::<ArrayVec<_>>();
                // Fill remaining entries with a null pointer
                for _ in 0..(4 - length) {
                    m.push(std::ptr::null())
                }
                let m = m.into_inner().unwrap();
                m
            },
        }
    }
}

#[no_mangle]
pub extern fn test_dice() -> Box<RustDice> {
    Box::new(RustDice::from((2, 6)))
}

#[no_mangle]
pub extern fn test_player() -> Box<RustPlayer> {
    Box::new(RustPlayer::Black)
}

#[no_mangle]
pub extern fn test_point() -> Box<RustPoint> {
    Box::new(RustPoint { owner: RustPlayer::White, count: 5 })
}

#[no_mangle]
pub extern fn test_some_point() -> RustMaybePoint {
    let point = Some(Point { owner: Player::Black, count: 2 });
    RustMaybePoint::from(point)
}

#[no_mangle]
pub extern fn test_none_point() -> RustMaybePoint {
    let point = None;
    RustMaybePoint::from(point)
}

#[no_mangle]
pub extern fn test_board() -> Box<RustBoard> {
    Box::new(RustBoard::from(Board::init()))
}

#[no_mangle]
pub extern fn test_submove_bear_off() -> Box<RustSubmove> {
    Box::new(RustSubmove::from(Submove::BearOff { from: 1 }))
}

#[no_mangle]
pub extern fn test_submove_enter() -> Box<RustSubmove> {
    Box::new(RustSubmove::from(Submove::Enter { to: 1 }))
}

#[no_mangle]
pub extern fn test_submove_move() -> Box<RustSubmove> {
    Box::new(RustSubmove::from(Submove::Move { from: 1, to: 2 }))
}

#[no_mangle]
pub extern fn test_move() -> Box<RustMove> {
    Box::new(RustMove::from(Move { submoves: vec![
        Submove::Move { from: 1, to: 2 },
        Submove::BearOff { from: 1 },
        Submove::Enter { to: 1 },
    ]}))
}
