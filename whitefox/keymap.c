// -*- eval: (electric-indent-mode 0)

#include "whitefox.h"

#define BASE 0 // default layer
#define SYMB 1 // symbols

// Macros

#define M_COLN     M(KC_C_COLN)
#define CMD_TAB    M(KC_CMD_TAB)

const uint16_t keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  /* Layer 0: Default Layer
   * ,---------------------------------------------------------------.
   * |Esc|  1|  2|  3|  4|  5|  6|  7|  8|  9|  0|  -|  =|  \|  `|Ins|
   * |---------------------------------------------------------------|
   * |Tab  |  Q|  W|  E|  R|  T|  Y|  U|  I|  O|  P|  [|  ]|Backs|Del|
   * |---------------------------------------------------------------|
   * |ES/F18|  A|  S|  D|  F|  G|  H|  J|  K|  L|  ;|  '|Enter   |PgU
   * |---------------------------------------------------------------|
   * |Shift   |  Z|  X|  C|  V|  B|  N|  M|  ,|  .|  /|Shift |Up |PgD|
   * |---------------------------------------------------------------|
   * |Ctrl|Alt |Gui |         Space         |Alt |SYMB   |Lef|Dow|Rig|
   * `---------------------------------------------------------------'
   */
  [BASE] = KEYMAP_TRUEFOX( \
    KC_ESC,       KC_1,   KC_2,        KC_3,   KC_4,   KC_5,   KC_6,   KC_7,   KC_8,   KC_9,   KC_0,   KC_MINS,KC_EQL, KC_GRV,KC_BSLS,OSM(MOD_HYPR), \
    KC_TAB,       KC_Q,   KC_W,        KC_E,   KC_R,   KC_T,   KC_Y,   KC_U,   KC_I,   KC_O,   KC_P,   KC_LBRC,KC_RBRC,KC_BSPC,       KC_DEL, \
    MEH_T(KC_ESC),KC_A,   KC_S,        KC_D,   KC_F,   KC_G,   KC_H,   KC_J,   KC_K,   KC_L,   KC_SCLN,KC_QUOT,        KC_ENT,        KC_PGUP,\
    KC_LSFT,              CTL_T(KC_Z), KC_X,   KC_C,   KC_V,   KC_B,   KC_N,   KC_M,   KC_COMM,KC_DOT, KC_SLSH,KC_RSFT,       KC_UP,  KC_PGDN,\
    KC_LCTL,      KC_LALT,KC_LGUI,                                     KC_SPC,           KC_RALT,MO(SYMB),            KC_LEFT,KC_DOWN,KC_RGHT \
  ),
  /* Layer 1: Symbols Layer
   * ,---------------------------------------------------------------.
   * |Esc|  1|  2|  3|  4|  5|  6|  7|  8|  9|  0|  -|  =|  \|  `|Ins|
   * |---------------------------------------------------------------|
   * |Tab  |  Q|  W|  E|  R|  T|  Y|  U|  I|  O|  P|  [|  ]|Backs|Del|
   * |---------------------------------------------------------------|
   * |ES/F18|  A|  S|  D|  F|  G|  H|  J|  K|  L|  ;|  '|Enter   |PgU
   * |---------------------------------------------------------------|
   * |Shift   |  Z|  X|  C|  V|  B|  N|  M|  ,|  .|  /|Shift |Up |PgD|
   * |---------------------------------------------------------------|
   * |Ctrl|Alt |Gui |         Space         |Alt |SYMB   |Lef|Dow|Rig|
   * `---------------------------------------------------------------'
   */
  [SYMB] = KEYMAP_TRUEFOX( \
    RESET  ,KC_F1,  KC_F2,  KC_F3,  KC_F4,  KC_F5,  KC_F6,  KC_F7,  KC_F8,  KC_F9,  KC_F10, KC_F11, KC_F12, KC_TRNS,KC_TRNS,KC_MUTE,\
    KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,        KC_TRNS,\
    KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,        KC_TRNS,        KC_VOLU,\
    KC_TRNS,        KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,     KC_TRNS,   KC_VOLD,\
    KC_TRNS,KC_TRNS,KC_TRNS,               KC_TRNS,          KC_TRNS,KC_TRNS,             KC_TRNS,KC_TRNS,KC_TRNS \
  ),
/* Keymap 0: Basic layer
 *
 * ,--------------------------------------------------.           ,--------------------------------------------------.
 * | CMD-Tab|   1  |   2  |   3  |   4  |   5  |NUMPAD|           | No   |   6  |   7  |   8  |   9  |   _  |   =    |
 * |--------+------+------+------+------+-------------|           |------+------+------+------+------+------+--------|
 * |Tab/SYMB|   Q  |   W  |   E  |   R  |   T  | cmd  |           | Hyper|   Y  |   U  |   I  |   O  |   P  |   -    |
 * |--------+------+------+------+------+------| spc  |           | (OS) |------+------+------+------+------+--------|
 * |Esc     |A/MDIA|   S  |   D  |   F  |   G  |------|           |------|   H  |   J  |   K  |   L  |:/SYMB| '      |
 * |--------+------+------+------+------+------| CAPS |           | Meh  |------+------+------+------+------+--------|
 * | SHIFT  |Z/Ctrl|   X  |   C  |   V  |   B  |      |           |      |   N  |   M  |   ,  |   .  |/LGUI | SHIFT  |
 * `--------+------+------+------+------+-------------'           `-------------+------+------+------+------+--------'
 *   |Grv/L1| Ctrl | Symb | Lalt | Gui  |                                       | SYMB |</LGui|  v   |  ^   |   >  |
 *   `----------------------------------'                                       `----------------------------------'
 *                                        ,-------------.       ,-------------.
 *                                        | Vol- | Vol+ |       | Pause| Next   |
 *                                 ,------|------|------|       |------+--------+------.
 *                                 |      |      | Mute |       | Prev |        |      |
 *                                 |BSPC  | Del  |------|       |------|  Enter |Space |
 *                                 |      |      | Flash|       | Flash|        |      |
 *                                 `--------------------'       `----------------------'
 */
// If it accepts an argument (i.e, is a function), it doesn't need KC_.
// Otherwise, it needs KC_*
/* [BASE] = LAYOUT_ergodox(  // layer 0 : default */
/*         // left hand */
/* 	KC_GRV,         KC_1,          KC_2,             KC_3,           KC_4,         KC_5,            TG(NUMP), */
/* 	LT(SYMB,KC_TAB),KC_Q,          KC_W,             KC_E,           KC_R,         KC_T,            LGUI(KC_SPC), */
/*         MEH_T(KC_ESC),  LT(MDIA, KC_A),KC_S,             KC_D,           KC_F,         KC_G, */
/*         KC_LSFT,        CTL_T(KC_Z),   KC_X,             KC_C,           KC_V,         KC_B,            KC_CAPS,        */
/*         LT(SYMB,KC_GRV),KC_LCTL,       MO(NUMP),         KC_LALT,        KC_LGUI, */
/*                                                                                        KC_VOLD,         KC_VOLU, */
/*                                                                                                         KC_MUTE, */
/* 	                                                                 KC_BSPC,      LT(SYMB, KC_DEL),RESET, */
/*         // right hand */
/*              KC_NO,       KC_6,   KC_7,    KC_8,          KC_9,   KC_UNDS,          KC_EQL, */
/*              KC_FN2,      KC_Y,   KC_U,    KC_I,          KC_O,   KC_P,             KC_MINUS, */
/*                           KC_H,   KC_J,    KC_K,          KC_L,   M_COLN,           KC_QUOT, */
/*              MEH_T(KC_NO),KC_N,   KC_M,    KC_COMM,       KC_DOT, GUI_T(KC_SLSH),   KC_LSFT, */
/*                                   MO(SYMB),GUI_T(KC_LEFT),KC_DOWN,KC_UP,            KC_RIGHT, */
/*              KC_MPLY,        KC_MNXT,       */
/*              KC_MPRV, */
/*              RESET,  KC_ENT, KC_SPC */
/*     ), */
/* Keymap 1: Symbol Layer
 *
 * ,--------------------------------------------------.           ,--------------------------------------------------.
 * |        |  F1  |  F2  |  F3  |  F4  |  F5  |  F6  |           |  F7  |  F8  |  F9  |  F10 |  F11 |  F12 |        |
 * |--------+------+------+------+------+-------------|           |------+------+------+------+------+------+--------|
 * |        |   !  |   @  |   (  |   )  |   %  |      |           |      |   ^  |   <  |   >  |   |  |      |    +   |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |   ~    |   `  |   #  |   {  |   }  |      |------|           |------|   &  |   [  |   ]  |   ;  |      |    "   |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |        |      |   $  |   [  |   ]  |      |      |           |      |   *  |   <  |   >  |   \  |  ?   |        |
 * `--------+------+------+------+------+-------------'           `-------------+------+------+------+------+--------'
 *   |      |      |      |      |      |                                       |      |   0  |   .  |   ,  |      |
 *   `----------------------------------'                                       `----------------------------------'
 *                                        ,-------------.       ,-------------.
 *                                        |      |      |       |      |      |
 *                                 ,------|------|------|       |------+------+------.
 *                                 |      |      |      |       |      |      |      |
 *                                 |      |      |------|       |------|      |      |
 *                                 |      |      |      |       |      |      |      |
 *                                 `--------------------'       `--------------------'
 */
// SYMBOLS
/* [SYMB] = LAYOUT_ergodox( */
/*        // left hand */
/*        KC_TRNS,KC_F1,  KC_F2,  KC_F3,  KC_F4,  KC_F5,        KC_F6, */
/*        KC_TRNS,KC_EXLM,KC_AT,  KC_LPRN,KC_RPRN,KC_PERC,      KC_TRNS, */
/*        KC_TILD,KC_GRV, KC_HASH,KC_LCBR,KC_RCBR,KC_TRNS, */
/*        KC_TRNS,KC_TRNS,KC_DLR, KC_LBRC,KC_RBRC,KC_TRNS,      KC_TRNS, */
/*        KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS, */
/*                                        KC_TRNS,KC_TRNS, */
/*                                                KC_TRNS, */
/*                                KC_TRNS,KC_TRNS,KC_TRNS, */
/*        // right hand */
/*        KC_F7,   KC_F8,   KC_F9,        KC_F10,         KC_F11,  KC_F12,  KC_TRNS, */
/*        KC_TRNS, KC_CIRC, LSFT(KC_COMM),LSFT(KC_DOT),   KC_PIPE, KC_TRNS, KC_PLUS, */
/*                 KC_AMPR, KC_LBRC,      KC_RBRC,        KC_SCLN, KC_TRNS, LSFT(KC_QUOT), */
/*        KC_TRNS, KC_ASTR, KC_1,         KC_2,           KC_BSLS, KC_QUES, KC_TRNS, */
/*                          KC_TRNS,      KC_0,           KC_DOT,  KC_COMM, KC_TRNS, */
/*        KC_TRNS, KC_TRNS, */
/*        KC_TRNS, */
/*        KC_TRNS, KC_TRNS, KC_TRNS */
/* ), */
};

const uint16_t PROGMEM fn_actions[] = {
    /* [1] = ACTION_MODS_TAP_TOGGLE(MOD_LSFT),                               // FN1 - shift/caps */
    /* [2] = ACTION_MODS_ONESHOT(MOD_LSFT | MOD_LALT | MOD_LCTL | MOD_LGUI), // FN2 - hyper oneshot */
};

/* static uint16_t start; */
/* static bool keypressed; */
/* static bool shiftdownbefore; */

const macro_t *action_get_macro(keyrecord_t *record, uint8_t id, uint8_t opt)
{
  /* switch(id) { */
  /* case KC_C_COLN: */
  /*   if (record->event.pressed) { */
  /*     start = timer_read(); */
  /*     shiftdownbefore = get_mods()&MOD_BIT(KC_LSHIFT); */
  /*     keypressed = false; */
  /*     layer_on(SYMB); */
  /*   } else { */
  /*     layer_off(SYMB); */
  /*     if (timer_elapsed(start) < 150 && !keypressed) { */
  /*       if (get_mods()&MOD_BIT(KC_LSHIFT) || shiftdownbefore) { */
  /*         unregister_code(KC_LSHIFT); */
  /*         register_code(KC_SCLN); */
  /*         unregister_code(KC_SCLN); */
  /*         register_code(KC_LSHIFT); */
  /*       } else { */
  /*         SEND_STRING(":"); */
  /*       } */
  /*     } */
  /*   } */
  /*   break; */
  /* case KC_CMD_TAB: */
  /*   return false; // (record->event.pressed ? MACRO( D(LGUI), T(TAB), END ) : MACRO( T(LGUI), END )); */
  /* } */

  return MACRO_NONE;
};

/* bool process_record_user(uint16_t keycode, keyrecord_t *record) { */
/*   if (record->event.pressed) { */
/*     keypressed = true; */
/*   } */
/*   return true; */
/* } */

// Runs just one time when the keyboard initializes.
/* void matrix_init_user(void) { */

/* }; */

// Runs constantly in the background, in a loop.
/* void matrix_scan_user(void) { */

/*     uint8_t layer = biton32(layer_state); */

/*     ergodox_board_led_off(); */
/*     ergodox_right_led_1_off(); */
/*     ergodox_right_led_2_off(); */
/*     ergodox_right_led_3_off(); */
/*     switch (layer) { */
/*       // TODO: Make this relevant to the ErgoDox EZ. */
/*         case 1: */
/*             ergodox_right_led_1_on(); */
/*             break; */
/*         case 2: */
/*             ergodox_right_led_2_on(); */
/*             break; */
/*         default: */
/*             // none */
/*             break; */
/*     } */
/* }; */

/* Keymap Template
  * ,---------------------------------------------------------------.
  * |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |
  * |---------------------------------------------------------------|
  * |     |   |   |   |   |   |   |   |   |   |   |   |   |     |   |
  * |---------------------------------------------------------------|
  * |      |   |   |   |   |   |   |   |   |   |   |   |        |   |
  * |---------------------------------------------------------------|
  * |        |   |   |   |   |   |   |   |   |   |   |      |   |   |
  * |---------------------------------------------------------------|
  * |    |    |    |                       |    |       |   |   |   |
  * `---------------------------------------------------------------'
  
[SYMB] = KEYMAP_TRUEFOX( \
  KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,\
  KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,        KC_TRNS,\
  KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,        KC_TRNS,        KC_TRNS,\
  KC_TRNS,        KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,     KC_TRNS,   KC_TRNS,\
  KC_TRNS,KC_TRNS,KC_TRNS,               KC_TRNS,          KC_TRNS,KC_TRNS,             KC_TRNS,KC_TRNS,KC_TRNS,\
),
*/
