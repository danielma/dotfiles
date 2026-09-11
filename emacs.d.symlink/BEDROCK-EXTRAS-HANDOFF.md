# Bedrock Extras Migration Handoff

## Goal

Integrate Emacs Bedrock's extras while preserving the useful parts of the
existing configuration and removing true duplicates. Treat understanding and
implementation as separate steps.

`extras/base.el` is complete. Review only `extras/dev.el` in the next context.

Before starting dev, confirm the base behavior interactively: Vertico and
Orderless minibuffer completion, Corfu completion-at-point, isearch-to-Consult
handoff, Meow, Embark prefix help, and Ghostel.

The Bedrock source currently lives at:

```text
/var/folders/29/7zc9hh416bz9zc77n3l5d5mc0000gn/T/__codeberg_org_ashton314_emacs_bedrockY2ERtE/
```

`extras/dev.el` has not been copied into this repository yet. The base extra
was committed as an inactive upstream baseline, reviewed completely, and then
deleted after all retained behavior moved into its owning modules.

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
- Try Embark's automatic prefix help instead of Which Key. A commented Which
  Key fallback lives beside the Embark configuration in `dm-bindings.el`.
- Use Vertico and Orderless for minibuffer completion.
- Use Corfu, Popupinfo, Cape, and Kind Icon for completion-at-point; keep
  `completion-preview-mode` disabled.
- Keep Meow; do not enable CUA or Evil.
- Keep Ghostel and omit Bedrock's Eshell and Eat configuration.
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

## Phase 1: `extras/base.el` — complete

Do not recreate or load `extras/base.el`. Git history contains the upstream
baseline and each decision commit. The notes below record the outcomes.

### Avy

- Retained as a new capability in `dm-bindings.el`.
- `C-c j` runs `avy-goto-char-timer` globally and `avy-isearch` during isearch.
- Pressing `.` during Avy selection invokes Embark at the chosen target.

### Consult

- The larger binding set and Consult-based Xref display remain in `dm-ui.el`.
- `<` narrows Consult results. Isearch can hand off to Consult with `M-s l` or
  `M-s L`, and `M-s e` opens Consult's isearch history.
- `M-e` still edits the isearch string, `M-s o` still runs built-in Occur, and
  the redundant `M-s s` alias was omitted.

### Embark

- `C-c a` invokes `embark-act` globally; the dormant minibuffer-only `C-.`
  binding was not restored.
- Embark automatic prefix help is enabled with a one-second delay.
- Which Key is disabled, with a commented fallback beside the Embark setup.
- `embark-consult` is retained with previews in Embark collect buffers.

### Vertico and Orderless

- Bedrock's plain Vertico, Vertico Directory, and Orderless behavior is active
  in `dm-ui.el`.
- The richer dormant Posframe and Orderless customizations were not restored.

### Marginalia

- Retained in `dm-ui.el` as the single declaration.

### Corfu, Popupinfo, Cape, and Kind Icon

- The complete Bedrock stack is active in `dm-ui.el`.
- Built-in `completion-preview-mode` is explicitly disabled so the two
  completion frontends do not compete.

### Eshell and Eat

- Omitted entirely. Ghostel remains the chosen terminal workflow.

### Activating base — complete

- Reviewed behavior lives in `dm-ui.el` and `dm-bindings.el`.
- `dm-ui.el` is loaded from `init.el`; `extras/base.el` is not needed.
- Batch load and behavior assertions pass. Interactive smoke testing is the
  remaining gate before dev begins.

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

## Suggested fresh-context prompt

> Read `BEDROCK-EXTRAS-HANDOFF.md` and the decision commits made during the
> base migration. First confirm the base smoke test is complete, then work only
> on `extras/dev.el` in the same one-question-at-a-time style. Do not revisit
> completed base decisions unless dev exposes a real conflict.
