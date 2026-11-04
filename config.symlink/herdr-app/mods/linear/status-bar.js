// The status bar item: how many open issues are assigned to you, and how many are in progress. Toasts
// when a new issue is assigned to you; a click opens the panel as a popup. Shown by a layout block with
// props { view: "status-bar" }.
import { div } from "gpui-kit";
import { h_flex } from "gpui-base";
import { theme, notify, openOverlay, nextCommand } from "herdr";
import { loadConfig, fetchIssues, NO_KEY } from "./api.js";

const REFRESH_MS = 120000;
export const PANEL_WIDTH = 460, PANEL_HEIGHT = 620;

export class StatusBar {
  init(cx) {
    this.issues = null; // null until the first answer
    this.error = null;
    this.seen = null;   // issue ids from the previous check
    this.refresh(cx);
    cx.timer.every(REFRESH_MS, (cx) => this.refresh(cx));
    cx.spawn(async (cx) => {
      for (;;) {
        const command = await nextCommand();
        if (command === "refresh") this.refresh(cx);
      }
    });
  }

  refresh(cx) {
    if (this.busy) return;
    this.busy = true;
    cx.spawn(async (cx) => {
      try {
        const config = await loadConfig();
        if (config.error) throw new Error(config.error);
        if (!config.apiKey) throw new Error(NO_KEY);
        const issues = await fetchIssues(config.apiKey);
        this.announce(issues);
        this.issues = issues; this.error = null;
      } catch (e) { this.error = String(e.message ?? e); }
      finally { this.busy = false; cx.notify(); }
    });
  }

  // Toast for every issue that wasn't assigned to you at the last check.
  announce(issues) {
    const before = this.seen;
    this.seen = new Set(issues.map((i) => i.id));
    if (!before) return; // first check: nothing to announce
    for (const i of issues) {
      if (before.has(i.id)) continue;
      notify({
        title: `Assigned to you: ${i.identifier}`,
        body: i.title.slice(0, 140),
        level: "info",
        timeout: 10000,
        action: { name: "mod.open", args: { name: "linear" } },
      });
    }
  }

  open(cx) {
    openOverlay({ mod: "linear", kind: "popup", width: PANEL_WIDTH, height: PANEL_HEIGHT, props: { compact: true } });
    this.refresh(cx);
  }

  label() {
    if (this.error) return this.error === NO_KEY ? "Linear · no key" : "Linear ?";
    if (!this.issues) return "Linear …";
    const doing = this.issues.filter((i) => i.stateType === "started").length;
    return doing ? `Linear · ${this.issues.length} (${doing} in progress)` : `Linear · ${this.issues.length}`;
  }

  render() {
    const t = theme();
    const urgent = (this.issues ?? []).some((i) => i.priority === 1);
    const dot = this.error || !this.issues?.length ? t.dim : urgent ? t.ansi1 : t.accent;
    return h_flex().id("linear-status-bar").size_full().px(14).gap(6).items_center().justify_end().cursor_pointer()
      .bg(t.chrome).border_t(1).border_color(t.border).text_size(11.8).text_color(t.muted)
      .hover((s) => s.text_color(t.foreground))
      .when(Boolean(this.error), (e) => e.tooltip(this.error ?? ""))
      .on_click((_e, cx) => this.open(cx))
      .child(div().size(6).rounded(3).bg(dot))
      .child(div().truncate().when(urgent && !this.error, (d) => d.text_color(t.ansi1).font_semibold()).child(this.label()));
  }
}
