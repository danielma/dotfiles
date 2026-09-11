# Bedrock Extras Migration Handoff

## Goal

Integrate Emacs Bedrock's `extras/base.el` first, then `extras/dev.el`, while
preserving the useful parts of the existing configuration and removing true
duplicates. Treat understanding and implementation as separate steps.

Review only one extra per context:

1. Finish and activate `extras/base.el`.
2. Clear context if desired.
3. Finish and activate `extras/dev.el`.

The Bedrock source currently lives at:

```text
/var/folders/29/7zc9hh416bz9zc77n3l5d5mc0000gn/T/__codeberg_org_ashton314_emacs_bedrockY2ERtE/
```

Neither extra has been copied into this repository yet.

## How to work with Daniel

- Ask one concrete keep/remove/merge question at a time.
- Keep each explanation short and include a candid recommendation.
- Show a small code sample when the implementation is material or requested.
- When Daniel accepts a change, implement it and make a small Conventional
  Commit immediately.
- A decision to retain unchanged code does not require an empty commit.
- Before staging, inspect the exact diff. Daniel may be editing the same files
  in a live Emacs session; never sweep concurrent edits into an unrelated
  commit.
- Stage only explicit files. Leave unrelated untracked files and Emacs
  auto-save files alone.
- Put behavior in the existing module that owns that concern, then remove the
  duplicate from the Bedrock extra. Do not create a second source of truth.
- Do not activate an extra until every active form in it has been reviewed.

Use standard shell syntax in documentation, even though Daniel uses fish
interactively.

## Current decisions and ownership

- Use built-in `package.el`, not `straight.el`, for the Bedrock migration.
- Keep `which-key-mode`.
- Try Emacs's built-in minibuffer completion before restoring Vertico and
  Orderless.
- Try `completion-preview-mode` before restoring Corfu.
- Keep Meow; do not enable CUA or Evil.
- UI defaults belong in `config/dm-ui.el`.
- General keybindings and transients belong in `config/dm-bindings.el`.
- Tab presentation belongs in `config/dm-tabs.el`.
- Theme and font behavior belongs in `config/dm-colors.el`; retain the adaptive
  Doom themes.
- Text wrapping and line-number behavior have been centralized in `dm-ui.el`.
- Bedrock's built-in matching-expression highlighting replaces Rainbow
  Delimiters configuration.
- Keep the tab bar visible, with fixed-width names, hidden buttons, and new
  tabs opening to `*scratch*`.
- There is an intentional TODO in `dm-bindings.el` to reconcile the custom tab
  transient with the built-in `C-x t` prefix.

Useful history begins with the baseline commit:

```text
8b5e458 refactor(emacs): adopt Bedrock init baseline
```

Use `git log --oneline 8b5e458^..HEAD` for the full decision log. Do not assume
the hash named `HEAD` in this document remains current.

## Phase 1: `extras/base.el`

Copy only `extras/base.el` into the repository and commit that inactive source
as a baseline. Do not load it from `init.el` yet. Then review these active
sections in file order.

### Avy

- New capability relative to the currently loaded configuration.
- Review `C-c j` and `s-j`, including the `s-j` isearch binding, against Meow
  and existing global bindings.
- Review the later Avy-to-Embark action as part of the same feature.

### Consult

- `config/dm-ui.el` already has a larger Consult binding set and Consult-based
  Xref display.
- Compare every Bedrock binding rather than loading both declarations.
- Preserve accepted bindings in `dm-ui.el`; remove duplicates from the extra.
- Pay particular attention to `M-s s`, isearch integration, narrowing with
  `<`, and any command whose current binding differs.

### Embark

- `config/dm-completion.el` previously bound `embark-act` to `C-.` in the
  minibuffer; Bedrock proposes global `C-c a` plus Avy integration.
- Bedrock disables `which-key` and replaces prefix help with Embark. That
  conflicts with an explicit decision to keep `which-key`.
- Review Embark actions independently from the prefix-help replacement. Do not
  disable `which-key` without a new explicit decision.

### Vertico and Orderless

- The existing `dm-completion.el` contains Vertico, Vertico Posframe, and a
  more detailed Orderless setup.
- Daniel explicitly chose to try Emacs's built-in minibuffer completion first.
- Start with a recommendation to omit Vertico and Orderless. Restore a focused
  piece only if Daniel identifies a missing capability.

### Marginalia

- Already configured in `dm-ui.el`.
- Decide whether its annotations are worth retaining with built-in completion.
- Keep one declaration in the owning module.

### Corfu, Popupinfo, Cape, and Kind Icon

- These compete with the current trial of built-in
  `completion-preview-mode`.
- The old `dm-completion.el` also contains Corfu, terminal support, and Kind
  Icon configuration.
- Review the in-buffer completion strategy as a whole before enabling any of
  these packages. Avoid running Corfu and completion previews together by
  accident.

### Eshell and Eat

- `config/dm-term.el` already configures Eat and other terminal tooling.
- Move accepted Eat behavior there and compare its terminal name and Eshell
  integration line by line.
- The Consult history binding for Eshell depends on the earlier Consult
  decision.

### Activating base

After every active form is resolved:

- Prefer loading reviewed behavior through the relevant `dm-*` modules rather
  than loading a mostly empty duplicate extra.
- If useful cohesive behavior remains in `extras/base.el`, load that file only
  after a clean batch syntax/load check.
- Confirm interactively that minibuffer completion, completion-at-point,
  isearch, Meow, Consult, and terminal behavior still work.
- Commit activation separately from the individual configuration decisions.

## Phase 2: `extras/dev.el`

Start this phase only after base is complete and stable. Copy `extras/dev.el`
as an inactive baseline and review these sections in order.

### Tree-sitter and pairing

- Compare Bedrock's built-in tree-sitter settings with the cache,
  `treesit-auto`, and `treesit-fold` behavior in `config/dm-prog.el`.
- Check whether the old tree-sitter availability cache is still justified on
  Emacs 31 before retaining it.
- Bedrock enables `electric-pair-mode`; the old setup enables Smartparens.
  Choose one global owner rather than enabling both indiscriminately.

### Project mode-line display

- Bedrock adds the project name to the mode line.
- Daniel wants a simpler mode line, and `config/dm-projects.el` owns project
  behavior. Review the value and placement before enabling it.

### Magit

- Magit is already retained, and `config/dm-magit.el` contains substantial
  workflow configuration.
- Merge only the `C-x g` binding if desired; do not create a second package
  declaration merely because it appears in the extra.

### File modes

- Markdown behavior exists in `config/dm-prose.el`; wrapping now belongs in
  `dm-ui.el`.
- YAML and other language modes belong in `config/dm-langs.el`.
- Determine whether Emacs 31's built-in JSON/tree-sitter support makes the
  external `json-mode` package unnecessary.

### Eglot

- Compare every Bedrock Eglot setting with `config/dm-lsp.el`, the Eglot hooks
  in `config/dm-prog.el`, and language-specific setup.
- Review `eglot-send-changes-idle-time`, `eglot-extend-to-xref`, and the code
  action indicator independently.
- Treat `(fset #'jsonrpc--log-event #'ignore)` skeptically: it replaces a
  private function globally. Keep it only with evidence that logging remains a
  real Emacs 31 performance problem.
- Bedrock's sample Haskell/Elixir workspace configuration should be removed
  unless those languages are actually in scope.
- Restore old Eglot customization, such as autoreconnect, only after confirming
  it remains relevant.

### Tempel versus Yasnippet

- `config/dm-yasnippet.el` contains the current snippet workflow.
- Bedrock proposes Tempel with overlapping expansion and navigation behavior.
- Compare workflows before installing Tempel. Do not enable both globally
  without explicit reasons and non-conflicting keys.

### Activating dev

After every active form is resolved, run syntax/load checks and manually test
at least one representative project for formatting, tree-sitter, Eglot,
completion, Magit, and snippets. Commit activation separately.

## Suggested fresh-context prompts

For the first session:

> Read `BEDROCK-EXTRAS-HANDOFF.md` and follow it. Work only on
> `extras/base.el`. Walk me through one active change at a time, give a short
> recommendation, and make a small Conventional Commit whenever I accept a
> change.

For the second session:

> Read `BEDROCK-EXTRAS-HANDOFF.md` and the decision commits made during the
> base migration. Work only on `extras/dev.el` in the same one-question-at-a-
> time style. Do not revisit completed base decisions unless dev exposes a real
> conflict.
