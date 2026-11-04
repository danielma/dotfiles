// The status chip's popup: the issue's team workflow states; a click moves the issue there.
// Opened by the panel as an overlay of this same mod with props { dialog: "status", issueId, current, states }.
import { div } from "gpui-kit";
import { v_flex, h_flex } from "gpui-base";
import { run, props, closeOverlay, theme, notify } from "herdr";
import { loadConfig, gql, NO_KEY } from "./api.js";

const ROW = 28, PADDING = 5;
const UPDATE = `mutation($id: String!, $stateId: String!) { issueUpdate(id: $id, input: { stateId: $stateId }) { success } }`;

export const STATUS_MENU_WIDTH = 190;
export const statusMenuHeight = (states) => states.length * ROW + PADDING * 2;

export class StatusMenu {
  init() { this.busy = null; }

  pick(state, cx) {
    const { issueId, current, overlay } = props();
    if (this.busy) return;
    if (state.name === current) return closeOverlay(overlay);
    this.busy = state.id; cx.notify();
    cx.spawn(async (cx) => {
      try {
        const config = await loadConfig();
        if (!config.apiKey) throw new Error(config.error ?? NO_KEY);
        const data = await gql(config.apiKey, UPDATE, { id: issueId, stateId: state.id });
        if (!data.issueUpdate.success) throw new Error("Linear did not change the status");
        run("linear.refresh");
      } catch (e) {
        notify({ title: "Linear status change failed", body: String(e.message ?? e), level: "error" });
      }
      closeOverlay(overlay);
      cx.notify();
    });
  }

  render() {
    const t = theme(), { states = [], current } = props();
    return v_flex().id("linear-status-menu").size_full().p(PADDING).bg(t.popover).text_color(t.foreground).text_size(12.5)
      .children(states.map((s) => h_flex().id(`status-${s.id}`).h(ROW).px(9).gap(9).flex_none().items_center()
        .rounded(6).cursor_pointer().hover((st) => st.bg(t.selected)).on_click((_e, cx) => this.pick(s, cx))
        .child(div().size(8).flex_none().rounded_full().bg(s.color ?? t.dim))
        .child(div().flex_1().child(s.name))
        .child(div().flex_none().text_color(t.muted).child(this.busy === s.id ? "…" : s.name === current ? "✓" : ""))));
  }
}
