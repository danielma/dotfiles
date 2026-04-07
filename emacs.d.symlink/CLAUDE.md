# Emacs Directory Notes

## Config ownership

- Put UI defaults in `config/dm-ui.el`, even when they only apply in terminal Emacs.
- Treat `config/dm-term.el` as terminal tooling and emulator integration config, such as `vterm`, `eat`, and project terminal commands.
- `xterm-mouse-mode` belongs in `config/dm-ui.el` because it is a display/input default for TTY frames, not terminal emulator plumbing.
