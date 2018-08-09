extern crate backgammon_logic;

use backgammon_logic::player::Player;
use backgammon_logic::game::{Dice};

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
