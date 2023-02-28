#include QMK_KEYBOARD_H
#include "version.h"
#include "keymap_german.h"
#include "keymap_nordic.h"
#include "keymap_french.h"
#include "keymap_spanish.h"
#include "keymap_hungarian.h"
#include "keymap_swedish.h"
#include "keymap_br_abnt2.h"
#include "keymap_canadian_multilingual.h"
#include "keymap_german_ch.h"
#include "keymap_jp.h"
#include "keymap_korean.h"
#include "keymap_bepo.h"
#include "keymap_italian.h"
#include "keymap_slovenian.h"
#include "keymap_lithuanian_azerty.h"
#include "keymap_danish.h"
#include "keymap_norwegian.h"
#include "keymap_portuguese.h"
#include "keymap_contributions.h"
#include "keymap_czech.h"
#include "keymap_romanian.h"
#include "keymap_russian.h"
#include "keymap_uk.h"
#include "keymap_estonian.h"
#include "keymap_belgian.h"
#include "keymap_us_international.h"
#include "keymap_croatian.h"
#include "keymap_turkish_q.h"
#include "keymap_slovak.h"

#define KC_MAC_UNDO LGUI(KC_Z)
#define KC_MAC_CUT LGUI(KC_X)
#define KC_MAC_COPY LGUI(KC_C)
#define KC_MAC_PASTE LGUI(KC_V)
#define KC_PC_UNDO LCTL(KC_Z)
#define KC_PC_CUT LCTL(KC_X)
#define KC_PC_COPY LCTL(KC_C)
#define KC_PC_PASTE LCTL(KC_V)
#define ES_LESS_MAC KC_GRAVE
#define ES_GRTR_MAC LSFT(KC_GRAVE)
#define ES_BSLS_MAC ALGR(KC_6)
#define NO_PIPE_ALT KC_GRAVE
#define NO_BSLS_ALT KC_EQUAL
#define LSA_T(kc) MT(MOD_LSFT | MOD_LALT, kc)
#define BP_NDSH_MAC ALGR(KC_8)
#define SE_SECT_MAC ALGR(KC_6)
#define MOON_LED_LEVEL LED_LEVEL
#define MT_CTLZ MT(MOD_LCTL, KC_Z)
#define OS_HYPR OSM(MOD_HYPR)

#define BASE 0
#define CLMK 1
#define SYMB 2
#define MDIA 3
#define NMPD 4

enum custom_keycodes {
  RGB_SLD = ML_SAFE_RANGE,
  HSV_86_255_128,
  ST_MCR0,
  MC_COLN,
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  [BASE] = LAYOUT_moonlander(
    KC_GRAVE,          KC_1,     KC_2,    KC_3,    KC_4,    KC_5,    DF(CLMK),       /**/ KC_RIGHT, KC_6,     KC_7,    KC_8,    KC_9,    KC_UNDS,   KC_EQUAL,
    KC_TAB,            KC_Q,     KC_W,    KC_E,    KC_R,    KC_T,    LGUI(KC_SPACE), /**/ OS_HYPR,  KC_Y,     KC_U,    KC_I,    KC_O,    KC_P,      KC_MINUS,
    KC_ESCAPE,         KC_A,     KC_S,    KC_D,    KC_F,    KC_G,    KC_HYPR,        /**/ KC_MEH,   KC_H,     KC_J,    KC_K,    KC_L,    ST_MCR0,   KC_QUOTE,
    KC_LSHIFT,         MT_CTLZ,  KC_X,    KC_C,    KC_V,    KC_B,                    /**/           KC_N,     KC_M,    KC_COMMA,KC_DOT,  LT(SYMB,KC_SLASH), KC_RSHIFT,  
    LT(SYMB,KC_GRAVE), KC_LCTRL, TT(NMPD),KC_LALT, KC_LGUI, KC_TRNS,                 /**/           OSL(MDIA),OSL(SYMB),  MT(MOD_RGUI, KC_LEFT),KC_DOWN,KC_UP,KC_RIGHT,       
    KC_BSPACE,         KC_F24,   KC_LGUI,                                                                                     KC_MPLY, KC_ENTER,  KC_SPACE
  ),
  [CLMK] = LAYOUT_moonlander(
    KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, DF(BASE),/**/ KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,      KC_TRNS, 
    KC_TRNS, KC_Q,    KC_W,    KC_F,    KC_P,    KC_B,    KC_TRNS, /**/ KC_TRNS, KC_J,    KC_L,    KC_U,    KC_Y,    MC_COLN,      KC_TRNS, 
    KC_TRNS, KC_A,    KC_R,    KC_S,    KC_T,    KC_G,    KC_TRNS, /**/ KC_TRNS, KC_M,    KC_N,    KC_E,    KC_I,    LT(SYMB,KC_O),KC_TRNS, 
    KC_TRNS, KC_Z,    KC_X,    KC_C,    KC_D,    KC_V,             /**/          KC_K,    KC_H,    KC_TRNS, KC_TRNS, KC_TRNS,      KC_TRNS,
    KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,          /**/          KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,      KC_TRNS, 
    KC_TRNS, KC_TRNS, KC_TRNS,                                     /**/                                     KC_TRNS, KC_TRNS,      KC_TRNS
  ),
  [SYMB] = LAYOUT_moonlander(
    KC_TRANSPARENT, KC_F1,          KC_F2,          KC_F3,          KC_F4,          KC_F5,          KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_F6,          KC_F7,          KC_F8,          KC_F9,          KC_F10,         KC_F11,         
    KC_TRANSPARENT, KC_EXLM,        KC_AT,          KC_LCBR,        KC_RCBR,        KC_PERC,        KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_CIRC,        KC_LABK,        KC_RABK,        KC_PIPE,        KC_ASTR,        KC_PLUS,         
    KC_TILD,        KC_GRAVE,       KC_HASH,        KC_LPRN,        KC_RPRN,        KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_AMPR,        KC_LBRACKET,    KC_RBRACKET,    KC_SCLN,        KC_TRANSPARENT, LSFT(KC_QUOT), 
    KC_TRANSPARENT, KC_TRANSPARENT, KC_DLR,         KC_LBRACKET,    KC_RBRACKET,    KC_TRANSPARENT, KC_ASTR,        KC_1,           KC_2,           KC_BSLASH,      KC_TRANSPARENT, KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_COMMA,       KC_TRANSPARENT, HSV_86_255_128, KC_TRANSPARENT, RGB_MOD,                                                                                                        RGB_TOG,        KC_TRANSPARENT, KC_0,           KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, 
    KC_TRANSPARENT, RGB_VAI,        TOGGLE_LAYER_COLOR,                RGB_SLD,        RGB_HUD,        KC_TRANSPARENT
  ),
  [MDIA] = LAYOUT_moonlander(
    AU_TOG,         KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, RESET,          
    MU_TOG,         KC_TRANSPARENT, KC_TRANSPARENT, KC_MS_UP,       KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, 
    MU_MOD,         KC_TRANSPARENT, KC_MS_LEFT,     KC_MS_DOWN,     KC_MS_RIGHT,    KC_TRANSPARENT, KC_TRANSPARENT,                                                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_MEDIA_PLAY_PAUSE,
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_MEDIA_PREV_TRACK,KC_MEDIA_NEXT_TRACK,KC_TRANSPARENT, KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_MS_BTN1,     KC_MS_BTN2,     AU_TOG,                                                                                                         TO(0),          KC_AUDIO_VOL_DOWN,KC_AUDIO_VOL_UP,KC_AUDIO_MUTE,  KC_TRANSPARENT, KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_TRANSPARENT, RESET,                          KC_TRANSPARENT, KC_TRANSPARENT, KC_WWW_BACK
  ),
  [NMPD] = LAYOUT_moonlander(
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                 KC_TRANSPARENT, KC_TRANSPARENT, KC_7,           KC_8,           KC_9,           KC_TRANSPARENT, KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                 KC_TRANSPARENT, KC_TRANSPARENT, KC_4,           KC_5,           KC_6,           KC_TRANSPARENT, KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_1,           KC_2,           KC_3,           KC_TRANSPARENT, KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_COMMA,       KC_0,           KC_DOT,         KC_TRANSPARENT, KC_TRANSPARENT, 
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT
  ),
};

extern rgb_config_t rgb_matrix_config;

void keyboard_post_init_user(void) {
  rgb_matrix_enable();
}

const uint8_t PROGMEM ledmap[][DRIVER_LED_TOTAL][3] = {
    [SYMB] = { {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255}, {41,255,255} },

};

void set_layer_color(int layer) {
  for (int i = 0; i < DRIVER_LED_TOTAL; i++) {
    HSV hsv = {
      .h = pgm_read_byte(&ledmap[layer][i][0]),
      .s = pgm_read_byte(&ledmap[layer][i][1]),
      .v = pgm_read_byte(&ledmap[layer][i][2]),
    };
    if (!hsv.h && !hsv.s && !hsv.v) {
        rgb_matrix_set_color( i, 0, 0, 0 );
    } else {
        RGB rgb = hsv_to_rgb( hsv );
        float f = (float)rgb_matrix_config.hsv.v / UINT8_MAX;
        rgb_matrix_set_color( i, f * rgb.r, f * rgb.g, f * rgb.b );
    }
  }
}

void rgb_matrix_indicators_user(void) {
  if (keyboard_config.disable_layer_led) { return; }
  switch (biton32(layer_state)) {
    case 2:
      set_layer_color(2);
      break;
   default:
    if (rgb_matrix_get_flags() == LED_FLAG_NONE)
      rgb_matrix_set_color_all(0, 0, 0);
    break;
  }
}

static bool keypressed;

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    if (record->event.pressed) {
        keypressed = true;
    }

    switch (keycode) {
    case ST_MCR0:
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
    case MC_COLN:
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
    case RGB_SLD:
        if (record->event.pressed) {
            rgblight_mode(1);
        }
        return false;
    case HSV_86_255_128:
        if (record->event.pressed) {
            rgblight_mode(1);
            rgblight_sethsv(86,255,128);
        }
        return false;
    }
    return true;
}

