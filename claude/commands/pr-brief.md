Give me a high-level brief on this PR. I'm an engineering manager — I don't need line-by-line review or critique. I want to understand what the author did, how they approached it, and whether the change is coherent.

PR: $ARGUMENTS

## Steps

1. Parse $ARGUMENTS. It may be:
   - A full GitHub URL: `https://github.com/planningcenter/home-mobile/pull/123`
   - A repo shorthand: `home-mobile#123` or `ChurchCenterApp#123`
   - If only a number is given, ask which repo

2. Fetch PR data using `gh`:
   ```
   gh pr view <number> --repo planningcenter/<repo> --json title,author,body,additions,deletions,changedFiles,commits,labels,milestone
   gh pr diff <number> --repo planningcenter/<repo>
   ```

3. Analyze the diff structurally — not line by line. Look for:
   - Which directories/modules are touched
   - Whether changes cluster around one concern or spread across many
   - What kind of change this is (new feature, fix, refactor, config, infra, etc.)
   - Whether the PR description explains intent and approach, or is thin
   - Whether the commits tell a coherent story

4. Write the brief using this format:

---

**PR #[number] — [title]**
https://github.com/planningcenter/[repo]/pull/[number]
[repo] · [author] · +[additions] / -[deletions] across [N] files

**What**
One sentence. What does this change do for the product or system?

**Approach**
How did the author solve it? Did they introduce a new abstraction, patch a specific call site, refactor a shared component, add a new layer? Be concrete about the technique.

**Coherence**
Is this one focused change, or are multiple concerns bundled together? Does the scope match the description? Flag if the PR is doing more than it says (or less).

**Blast radius**
Which parts of the codebase are affected? Is the change well-localized or spread across many modules/layers? Name the directories or domains touched.

**Signal**
Anything worth knowing as a manager — not criticism. Examples: unusually large for the described scope, heavy on config changes with no tests, commit history suggests the approach changed mid-way, description is thin relative to the complexity of the diff.

---

Keep each section to 2-4 sentences max. Be direct. No praise, no hedging.
