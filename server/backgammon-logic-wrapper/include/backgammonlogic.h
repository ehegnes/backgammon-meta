#ifndef BACKGAMMON_LOGIC_H
#define BACKGAMMON_LOGIC_H

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#define PLAYER_BLACK 0
#define PLAYER_WHITE 1

#define SUBMOVE_BEAR_OFF 0
#define SUBMOVE_ENTER 1
#define SUBMOVE_MOVE 2

#define BOARD_SIZE 24

typedef enum {
    Black,
    White,
} RustPlayer;

typedef uint8_t Die;

typedef struct {
    Die d1;
    Die d2;
} RustDice;

typedef enum {
    BearOff,
    Enter,
    Move,
} RustSubmoveTag;

typedef struct {
    size_t to;
} RustSubmoveEnter;

typedef struct {
    size_t from;
    size_t to;
} RustSubmoveMove;

typedef struct {
    size_t from;
} RustSubmoveBearOff;

typedef union {
    RustSubmoveBearOff BearOff;
    RustSubmoveEnter Enter;
    RustSubmoveMove Move;
} RustSubmovePayload;

typedef struct {
    RustSubmoveTag tag;
    RustSubmovePayload *payload;
} RustSubmove;

typedef struct {
    RustPlayer owner;
    uint8_t count;
} RustPoint;

typedef RustPoint *RustMaybePoint;

typedef RustMaybePoint **RustInternalBoard;

typedef struct {
    RustInternalBoard board;
    uint8_t bar_black;
    uint8_t bar_white;
} RustBoard;

typedef struct {
    RustSubmove **submoves;
} RustMove;

RustMaybePoint test_some_point(void);
RustMaybePoint test_none_point(void);
RustPlayer *test_player(void);
RustPoint *test_point(void);
RustBoard *test_board(void);
RustSubmove *test_submove_bear_off(void);
RustSubmove *test_submove_enter(void);
RustSubmove *test_submove_move(void);
RustMove *test_move(void);

#endif /* BACKGAMMON_LOGIC_H */
