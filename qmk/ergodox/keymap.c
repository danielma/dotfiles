// -*- eval: (progn (whitespace-mode 0) (apheleia-mode 0) (electric-indent-mode 0)) -*-

#include QMK_KEYBOARD_H
#include "debug.h"
#include "action_layer.h"

#define CLMK 0 // colemak layer
#define QWRT 1 // qwerty layer
#define SYMB 2 // symbols
#define NUMP 3 // numpad
#define MOTI 4 // motion

#define OS_HYPR OSM(MOD_HYPR)

enum custom_keycodes {
  MCR_0 = SAFE_RANGE,
  CLN_FLP,
};

// Macros

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
// If it accepts an argument (i.e, is a function), it doesn't need KC_.
// Otherwise, it needs KC_*
/* Keymap 0: Colemak-DH(M) layer
 *
 * ╭────────┬──────┬──────┬──────┬──────┬──────┬──────╮ ╭──────┬──────┬──────┬──────┬──────┬──────┬────────╮
 * │ `      │   1  │   2  │   3  │   4  │   5  │ QWRT │ │ NUMP │   6  │   7  │   8  │   9  │   _  │   =    │
 * ├────────┼──────┼──────┼──────┼──────┼──────┼──────┤ ├──────┼──────┼──────┼──────┼──────┼──────┼────────┤
 * │ Tab    │   Q  │   W  │   F  │   P  │   B  │ cmd  │ │ Hyper│   J  │   L  │   U  │   Y  │   :  │   -    │
 * ├────────┼──────┼──────┼──────┼──────┼──────┤ spc  │ │ (OS) ├──────┼──────┼──────┼──────┼──────┼────────┤
 * │ Esc    │   A  │   R  │   S  │   T  │   G  ├──────┤ ├──────┤   M  │   N  │   E  │   I  │   O  │ '/SYMB │
 * ├────────┼──────┼──────┼──────┼──────┼──────┤ CAPS │ │ Meh  ├──────┼──────┼──────┼──────┼──────┼────────┤
 * │ SHIFT  │Z/Ctrl│   X  │   C  │   D  │   V  │      │ │      │   K  │   H  │   ,  │   .  │/SYMB │ SHIFT  │
 * ╰─┬──────┼──────┼──────┼──────┼──────┼──────┴──────╯ ╰──────┴──────┼──────┼──────┼──────┼──────┼──────┬─╯
 *   │Grv/L1│ Ctrl │ Nump │ Lalt │ Gui  │                             │ SYMB │←/LGui│   ↓  │  ↑   │   →  │
 *   ╰──────┴──────┴──────┴──────┴──────╯                             ╰──────┴──────┴──────┴──────┴──────╯
 *                                      ╭──────┬──────╮ ╭──────┬────────╮
 *                                      │ Vol─ │ Vol+ │ │ Pause│ Next   │
 *                               ╭──────┼──────┼──────┤ ├──────┼────────┼──────╮
 *                               │      │      │ Mute │ │ Prev │        │      │
 *                               │BSPC  │ Del  ├──────┤ ├──────┤ Enter  │Space │
 *                               │      │      │ Flash│ │ Flash│        │      │
 *                               ╰──────┴──────┴──────╯ ╰──────┴────────┴──────╯
 */
[CLMK] = LAYOUT_ergodox_pretty(
  KC_GRV,  KC_1,        KC_2,          KC_3,    KC_4,    KC_5,    DF(CLMK),      /**/ TG(NUMP),     KC_6,    KC_7,    KC_8,          KC_9,    KC_UNDS, KC_EQL,
  KC_TAB,  KC_Q,        KC_W,          KC_F,    KC_P,    KC_B,    LGUI(KC_SPC),  /**/ OS_HYPR,      KC_J,    KC_L,    KC_U,          KC_Y,    CLN_FLP, KC_MINUS,
  KC_ESC,  KC_A,        LT(MOTI,KC_R), KC_S,    KC_T,    KC_G,                   /**/               KC_M,    KC_N,    KC_E,          KC_I,    KC_O,    KC_QUOT,
  KC_LSFT, CTL_T(KC_Z), KC_X,          KC_C,    KC_D,    KC_V,    KC_CAPS,       /**/ MEH_T(KC_NO), KC_K,    KC_H,    KC_COMM,       KC_DOT,  LT(SYMB,KC_SLSH), KC_LSFT,
  KC_GRV,  KC_LCTL,     MO(NUMP),      KC_LALT, KC_LGUI,                         /**/                        MO(SYMB),GUI_T(KC_LEFT),KC_DOWN, KC_UP,   KC_RIGHT,
                                                               KC_VOLD, KC_VOLU,       /**/ KC_MPLY,      KC_MNXT,
                                                                        KC_MUTE,       /**/ KC_MPRV,
                                                      KC_BSPC ,KC_DEL,  RESET,         /**/ RESET,        KC_ENT, KC_SPC
),
/* Keymap 1: Basic layer
 *
 * ╭────────┬──────┬──────┬──────┬──────┬──────┬──────╮ ╭──────┬──────┬──────┬──────┬──────┬──────┬────────╮
 * │ `      │   1  │   2  │   3  │   4  │   5  │ CLMK │ │ NUMP │   6  │   7  │   8  │   9  │   _  │   =    │
 * ├────────┼──────┼──────┼──────┼──────┼──────┼──────┤ ├──────┼──────┼──────┼──────┼──────┼──────┼────────┤
 * │Tab     │   Q  │   W  │   E  │   R  │   T  │ cmd  │ │ Hyper│   Y  │   U  │   I  │   O  │   P  │   -    │
 * ├────────┼──────┼──────┼──────┼──────┼──────┤ spc  │ │ (OS) ├──────┼──────┼──────┼──────┼──────┼────────┤
 * │ Esc    │   A  │   S  │   D  │   F  │   G  ├──────┤ ├──────┤   H  │   J  │   K  │   L  │   :  │ '/SYMB │
 * ├────────┼──────┼──────┼──────┼──────┼──────┤ CAPS │ │ Meh  ├──────┼──────┼──────┼──────┼──────┼────────┤
 * │ SHIFT  │Z/Ctrl│   X  │   C  │   V  │   B  │      │ │      │   N  │   M  │   ,  │   .  │/LGUI │ SHIFT  │
 * ╰─┬──────┼──────┼──────┼──────┼──────┼──────┴──────╯ ╰──────┴──────┼──────┼──────┼──────┼──────┼──────┬─╯
 *   │Grv/L1│ Ctrl │ Nump │ Lalt │ Gui  │                             │ SYMB │←/LGui│   ↓  │  ↑   │   →  │
 *   ╰──────┴──────┴──────┴──────┴──────╯                             ╰──────┴──────┴──────┴──────┴──────╯
 *                                      ╭──────┬──────╮ ╭──────┬────────╮
 *                                      │ Vol─ │ Vol+ │ │ Pause│ Next   │
 *                               ╭──────┼──────┼──────┤ ├──────┼────────┼──────╮
 *                               │      │      │ Mute │ │ Prev │        │      │
 *                               │BSPC  │ Del  ├──────┤ ├──────┤ Enter  │Space │
 *                               │      │      │ Flash│ │ Flash│        │      │
 *                               ╰──────┴──────┴──────╯ ╰──────┴────────┴──────╯
 */
[QWRT] = LAYOUT_ergodox_pretty(  // layer 0 : default
  _______, KC_1,        KC_2,     KC_3,    KC_4,    KC_5,    DF(CLMK),    /**/ TG(NUMP),    KC_6,   KC_7,    KC_8,          KC_9,   KC_UNDS,   KC_EQL,
  KC_TAB,  KC_Q,        KC_W,     KC_E,    KC_R,    KC_T,    LGUI(KC_SPC),/**/ OS_HYPR,     KC_Y,   KC_U,    KC_I,          KC_O,   KC_P,      KC_MINUS,
  KC_ESC,  KC_A,        KC_S,     KC_D,    KC_F,    KC_G,                 /**/              KC_H,   KC_J,    KC_K,          KC_L,   CLN_FLP,   _______,
  KC_LSFT, CTL_T(KC_Z), KC_X,     KC_C,    KC_V,    KC_B,    KC_CAPS,     /**/ MEH_T(KC_NO),KC_N,   KC_M,    KC_COMM,       KC_DOT, _______,   KC_LSFT,
  KC_GRV,  KC_LCTL,     MO(NUMP), KC_LALT, KC_LGUI,                       /**/                      MO(SYMB),GUI_T(KC_LEFT),KC_DOWN,KC_UP,     KC_RIGHT,
                                                    KC_VOLD, KC_VOLU,     /**/ KC_MPLY,     KC_MNXT,
                                                             KC_MUTE,     /**/ KC_MPRV,
                                  KC_BSPC,  LT(SYMB, KC_DEL),RESET,       /**/ RESET,       KC_ENT, KC_SPC
  ),
/* Keymap 2: Symbol Layer
 *
 * ,--------------------------------------------------.           ,--------------------------------------------------.
 * |        |  F1  |  F2  |  F3  |  F4  |  F5  |  F6  |           |  F7  |  F8  |  F9  |  F10 |  F11 |  F12 |        |
 * |--------+------+------+------+------+-------------|           |------+------+------+------+------+------+--------|
 * |        |   !  |   @  |   {  |   }  |   %  |      |           |      |   ^  |   <  |   >  |   |  |      |    +   |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |   ~    |   `  |   #  |   (  |   )  |      |------|           |------|   &  |      |      |   ;  |      |    "   |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |        |      |   $  |   [  |   ]  |      |      |           |      |   *  |      |      |   \  |  ?   |        |
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
[SYMB] = LAYOUT_ergodox_pretty(
       _______,KC_F1,  KC_F2,  KC_F3,  KC_F4,  KC_F5,        KC_F6,   /**/ KC_F7,   KC_F8,   KC_F9,   KC_F10,  KC_F11,  KC_F12,  _______,
       _______,KC_EXLM,KC_AT,  KC_LCBR,KC_RCBR,KC_PERC,      _______, /**/ _______, KC_CIRC, KC_LABK, KC_RABK, KC_PIPE, _______, KC_PLUS,
       KC_TILD,KC_GRV, KC_HASH,KC_LPRN,KC_RPRN,_______,               /**/          KC_AMPR, _______, _______, KC_SCLN, _______, LSFT(KC_QUOT),
       _______,_______,KC_DLR, KC_LBRC,KC_RBRC,_______,      _______, /**/ _______, KC_ASTR, _______, _______, KC_BSLS, KC_QUES, _______,
       _______,_______,_______,_______,_______,                       /**/                   _______, _______, KC_DOT,  KC_COMM, _______,
                                       _______,_______,               /**/ _______, _______,
                                               _______,               /**/ _______,
                               _______,_______,_______,               /**/ _______, _______, _______
),
/* Numpad
 *
 * ,--------------------------------------------------.           ,--------------------------------------------------.
 * |        | Brt+ | Brt- |      |      |      |      |           |      |      |      |      |      |      |        |
 * |--------+------+------+------+------+-------------|           |------+------+------+------+------+------+--------|
 * |        |      |      |      |      |      |      |           |      |      |  7   |  8   |  9   |      |        |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |        |      |      |      |      |      |------|           |------|      |  4   |  5   |  6   |      |        |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |        |      |      |      |      |      |      |           |      |      |  1   |  2   |  3   |      |        |
 * `--------+------+------+------+------+-------------'           `-------------+------+------+------+------+--------'
 *   |      |      |      |      |      |                                       |  ,   |  0   |  .   |      |      |
 *   `----------------------------------'                                       `----------------------------------'
 *                                        ,-------------.       ,-------------.
 *                                        |      |      |       |      |      |
 *                                 ,------|------|------|       |------+------+------.
 *                                 |      |      |      |       |      |      |      |
 *                                 |      |      |------|       |------|      |      |
 *                                 |      |      |      |       |      |      |      |
 *                                 `--------------------'       `--------------------'
 */
// Numpad
[NUMP] = LAYOUT_ergodox_pretty(
       _______, KC_SLCK, KC_PAUS, _______, _______, _______, _______, /**/ _______, _______, _______, _______, _______, _______, KC_SLEP,
       _______, _______, _______, _______, _______, _______, _______, /**/ _______, _______, KC_7,    KC_8,    KC_9,    _______, _______,
       _______, _______, _______, _______, _______, _______,          /**/          _______, KC_4,    KC_5,    KC_6,    _______, _______,
       _______, _______, _______, _______, _______, _______, _______, /**/ _______, _______, KC_1,    KC_2,    KC_3,    _______, _______,
       _______, _______, _______, _______, _______,                   /**/                   KC_COMM, KC_0,    KC_DOT,  _______, _______,
                                                    _______, _______, /**/ _______, _______,
                                                             _______, /**/ _______,
                                           _______, _______, _______, /**/ _______, _______, _______
),
/* Motion
 *
 * ,--------------------------------------------------.           ,--------------------------------------------------.
 * |        |      |      |      |      |      |      |           |      |      |      |      |      |      |        |
 * |--------+------+------+------+------+-------------|           |------+------+------+------+------+------+--------|
 * |        |      |      |      |      |      |      |           |      |      |      |      |      |      |        |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |        |      |      |      |      |      |------|           |------|  ←   |  ↓   |  ↑   |  →   |      |        |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |        |      |      |      |      |      |      |           |      |      |      |      |      |      |        |
 * `--------+------+------+------+------+-------------'           `-------------+------+------+------+------+--------'
 *   |      |      |      |      |      |                                       |      |      |      |      |      |
 *   `----------------------------------'                                       `----------------------------------'
 *                                        ,-------------.       ,-------------.
 *                                        |      |      |       |      |      |
 *                                 ,------|------|------|       |------+------+------.
 *                                 |      |      |      |       |      |      |      |
 *                                 |      |      |------|       |------|      |      |
 *                                 |      |      |      |       |      |      |      |
 *                                 `--------------------'       `--------------------'
 */
[MOTI] = LAYOUT_ergodox_pretty(
  _______, _______, _______, _______, _______, _______, _______, /**/ _______, _______, _______, _______, _______, _______, _______,
  _______, _______, _______, _______, _______, _______, _______, /**/ _______, _______, _______, _______, _______, _______, _______,
  _______, _______, _______, _______, _______, _______,          /**/          KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT, _______, _______,
  _______, _______, _______, _______, _______, _______, _______, /**/ _______, _______, _______, _______, _______, _______, _______,
  _______, _______, _______, _______, _______,                   /**/                   _______, _______, _______, _______, _______,
                                               _______, _______, /**/ _______, _______,
                                                        _______, /**/ _______,
                                      _______, _______, _______, /**/ _______, _______, _______
)
};

static bool keypressed;

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  if (record->event.pressed) {
    keypressed = true;
  }
  
  switch (keycode) {
    case MCR_0:
        if (record->event.pressed) {
            keypressed = false;
            layer_on(SYMB);
        } else {
            layer_off(SYMB);

            if (!keypressed) {
                if (get_mods()&MOD_BIT(KC_LSHIFT)) {
                    unregister_code(KC_LSHIFT);
                    register_code(KC_SCLN);
                    unregister_code(KC_SCLN);
                    register_code(KC_LSHIFT);
                } else {
                    SEND_STRING(":");
                }
            }
        }
        break;
    case CLN_FLP:
        if (record->event.pressed) {
            if (get_mods()&MOD_BIT(KC_LSHIFT)) {
                unregister_code(KC_LSHIFT);
                register_code(KC_SCLN);
                unregister_code(KC_SCLN);
                register_code(KC_LSHIFT);
            } else {
                SEND_STRING(":");
            }
        }
      break;
  }

  return true;
}

// Runs just one time when the keyboard initializes.
// void matrix_init_user(void) {
// 
// };

layer_state_t default_layer_state_set_user(layer_state_t state) {
  if (IS_LAYER_ON_STATE(state, QWRT)) {
    ergodox_right_led_1_on();
  } else {
    ergodox_right_led_1_off();
  }
  
  return state;
}

// Runs constantly in the background, in a loop.
// Runs whenever there is a layer state change.
layer_state_t layer_state_set_user(layer_state_t state) {
    ergodox_board_led_off();
    ergodox_right_led_1_off();
    ergodox_right_led_2_off();
    ergodox_right_led_3_off();

    uint8_t layer = get_highest_layer(state);
    switch (layer) {
        case 0:
#ifdef RGBLIGHT_COLOR_LAYER_0
            rgblight_setrgb(RGBLIGHT_COLOR_LAYER_0);
#endif
            break;
        case 1:
            ergodox_right_led_1_on();
#ifdef RGBLIGHT_COLOR_LAYER_1
            rgblight_setrgb(RGBLIGHT_COLOR_LAYER_1);
#endif
            break;
        case 2:
            ergodox_right_led_2_on();
#ifdef RGBLIGHT_COLOR_LAYER_2
            rgblight_setrgb(RGBLIGHT_COLOR_LAYER_2);
#endif
            break;
        case 3:
            ergodox_right_led_3_on();
#ifdef RGBLIGHT_COLOR_LAYER_3
            rgblight_setrgb(RGBLIGHT_COLOR_LAYER_3);
#endif
            break;
        case 4:
            ergodox_right_led_1_on();
            ergodox_right_led_2_on();
#ifdef RGBLIGHT_COLOR_LAYER_4
            rgblight_setrgb(RGBLIGHT_COLOR_LAYER_4);
#endif
            break;
        case 5:
            ergodox_right_led_1_on();
            ergodox_right_led_3_on();
#ifdef RGBLIGHT_COLOR_LAYER_5
            rgblight_setrgb(RGBLIGHT_COLOR_LAYER_5);
#endif
            break;
        case 6:
            ergodox_right_led_2_on();
            ergodox_right_led_3_on();
#ifdef RGBLIGHT_COLOR_LAYER_6
            rgblight_setrgb(RGBLIGHT_COLOR_LAYER_6);
#endif
            break;
        case 7:
            ergodox_right_led_1_on();
            ergodox_right_led_2_on();
            ergodox_right_led_3_on();
#ifdef RGBLIGHT_COLOR_LAYER_7
            rgblight_setrgb(RGBLIGHT_COLOR_LAYER_7);
#endif
            break;
        default:
            break;
    }

    return state;
};
