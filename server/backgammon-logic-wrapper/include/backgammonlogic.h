#ifndef BACKGAMMON_LOGIC_H
#define BACKGAMMON_LOGIC_H

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#define PLAYER_BLACK 0
#define PLAYER_WHITE 1
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

RustMaybePoint test_some_point(void);
RustMaybePoint test_none_point(void);
RustPlayer *test_player(void);
RustPoint *test_point(void);
RustBoard *test_board(void);

#endif /* BACKGAMMON_LOGIC_H */
