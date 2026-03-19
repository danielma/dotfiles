Show me all PRs opened across my two teams' repos in the last time window.

Arguments: $ARGUMENTS (optional — a duration like `24h`, `48h`, `7d`. Default: `24h`)

## Steps

1. Parse $ARGUMENTS for a duration. Default to `24h` if empty. Supported formats:
   - `24h`, `48h` — hours
   - `7d`, `3d` — days

2. Calculate the cutoff timestamp using bash. On macOS:
   - For hours: `date -u -v-Nh +%Y-%m-%dT%H:%M:%SZ` (replace N with hours)
   - For days: `date -u -v-Nd +%Y-%m-%dT%H:%M:%SZ` (replace N with days)

3. Fetch PRs from both repos created since the cutoff:
   ```
   gh pr list --repo planningcenter/home-mobile --state all --search "created:>=<cutoff>" --json number,title,author,createdAt,state --limit 100
   gh pr list --repo planningcenter/ChurchCenterApp --state all --search "created:>=<cutoff>" --json number,title,author,createdAt,state --limit 100
   ```

4. Format the output like this:

---

**PR Digest — last [duration]** (since [human-readable date])

**home-mobile** · [N PRs]
- #751 `joeltjames` — feat: visual rebrand to Planning Center ✓ merged
- #750 `joeltjames` — feat: extract dashboard Home tab · open
- #749 `someuser` — fix: crash on launch · open

**ChurchCenterApp** · [N PRs]
- #1234 `anotheruser` — feat: new feature · open

*[total] PRs opened across both repos. Run `/brief-pr home-mobile#751` to dive into one.*

---

If no PRs are found in the window, say so clearly and suggest widening the range.
Keep output scannable — no extra commentary.
