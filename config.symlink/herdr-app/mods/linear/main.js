// Your open Linear issues. A row opens the issue in the browser; `Space` shows its Herdr space, or asks what
// to start a new one with (space-menu.js); `+` opens the new-issue dialog (new.js). Both are this same mod as
// an overlay, with props { dialog: "space" } or { dialog: "new" }. Clicking an issue's status opens
// status-menu.js the same way, with props { dialog: "status" }. A layout block with props
// { view: "status-bar" } is the status bar item instead (status-bar.js), which opens this panel as a popup.
import { View, div, svg } from "gpui-kit";
import { v_flex, h_flex, InputState, Input } from "gpui-base";
import { spaces, run, shared, setShared, theme, nextCommand, props, openOverlay } from "herdr";
import { ISSUE_QUERY, linkedIdentifiers, isOpen, SEARCH_QUERY, NOTIFICATIONS_QUERY, unreadInbox, markReadMutation, parseNode, STATUSES, TEAM_STATES_QUERY, sortStates, filterIssues, byState, ago } from "./linear.js";
import { loadConfig, gql, fetchIssues, NO_KEY } from "./api.js";
import { NewIssueForm } from "./new.js";
import { SpaceMenu, MENU_WIDTH, menuHeight } from "./space-menu.js";
import { StatusMenu, STATUS_MENU_WIDTH, statusMenuHeight } from "./status-menu.js";
import { openUrl } from "./open.js";
import { StatusBar, PANEL_HEIGHT } from "./status-bar.js";

const REFRESH_MS = 60000;

// Closed issues still linked to an open space, so their row (and Space button) stays until the space goes.
// Fetched one by one: a deleted issue fails alone.
async function fetchLinked(apiKey, known) {
  const have = new Set(known.map((i) => i.identifier));
  const missing = linkedIdentifiers(shared(SPACES_KEY), spaces()).filter((ident) => !have.has(ident));
  const found = await Promise.all(missing.map((id) => gql(apiKey, ISSUE_QUERY, { id }).then((d) => d.issue && parseNode(d.issue)).catch(() => null)));
  return found.filter(Boolean);
}

// Issue -> Herdr space links, by issue identifier: { "TEAM-123": "<space id>" }. A space named
// "<identifier>" or "<identifier> …" counts too, so links survive a lost id and can be made by hand.
const SPACES_KEY = "linear.spaces";

function spaceFor(issue) {
  const map = shared(SPACES_KEY) ?? {}, all = spaces(), ident = issue.identifier;
  const byId = all.find((s) => s.id === map[ident]);
  if (byId) return byId;
  const byName = all.find((s) => s.named && (s.name === ident || s.name.startsWith(`${ident} `)));
  if (byName) setShared(SPACES_KEY, { ...map, [ident]: byName.id });
  return byName ?? null;
}

// A space's avatar color, as the spaces mod draws it (mods/spaces/main.js keeps the same table).
const MARK = { ansi6: "#22a99a", ansi3: "#e3814f", ansi4: "#5b8def", ansi5: "#8b7cf6", ansi2: "#3fae6a", ansi1: "#d9649a" };

function spaceColor(space, t) {
  const color = (shared("space.colors") ?? {})[space.id] ?? "ansi6";
  return color.startsWith("#") ? color : (MARK[color] ?? t[color]);
}

const PRIORITY_COLORS = { 1: "ansi1", 2: "working" };
const INBOX_SHOWN = 8;
const SEARCH_MIN = 2, SEARCH_DELAY_MS = 350, SEARCH_LIMIT = 25;
const ASSIGN = `mutation($id: String!, $me: String!) { issueUpdate(id: $id, input: { assigneeId: $me }) { success } }`;

export default class Linear extends View {
  init(_props, cx) {
    if (props()?.dialog === "new") { this.form = new NewIssueForm(); return this.form.init(cx); }
    if (props()?.dialog === "space") { this.form = new SpaceMenu(); return; }
    if (props()?.dialog === "status") { this.form = new StatusMenu(); return this.form.init(cx); }
    if (props()?.view === "status-bar") { this.form = new StatusBar(); return this.form.init(cx); }
    this.inboxOpen = !props()?.compact; // the status bar's popup starts with the inbox folded, so issues show first
    this.issues = []; this.status = "all"; this.text = ""; this.generation = 0; this.loading = false;
    this.results = []; this.searchGen = 0; this.searching = false; this.assigning = new Set();
    this.teamStates = new Map(); // team id -> promise of its sorted workflow states
    this.search = InputState.new({ placeholder: "Filter or search Linear" });
    this.search.on("change", (_e, cx) => { this.text = this.search.value(); this.scheduleSearch(cx); cx.notify(); });
    this.refresh(cx);
    cx.timer.every(REFRESH_MS, (cx) => this.refresh(cx));
    cx.spawn(async (cx) => {
      for (;;) {
        const command = await nextCommand();
        if (command === "refresh") this.refresh(cx);
        cx.notify();
      }
    });
  }

  // Background refreshes stay quiet: no redraw until results arrive, and the icon only spins for a
  // click. "Loading…" only shows before the first load finishes.
  refresh(cx, manual = false) {
    const generation = ++this.generation;
    this.loading = true; this.manual = manual;
    cx.spawn(async (cx) => {
      try {
        const config = await loadConfig();
        this.config = config;
        if (config.error) throw new Error(config.error);
        if (!config.apiKey) throw new Error(NO_KEY);
        if (!this.viewerId) this.viewerId = (await gql(config.apiKey, "{ viewer { id } }")).viewer.id;
        const [issues, inbox] = await Promise.all([fetchIssues(config.apiKey),
          gql(config.apiKey, NOTIFICATIONS_QUERY).then((d) => unreadInbox(d.notifications.nodes)).catch(() => null)]);
        issues.push(...await fetchLinked(config.apiKey, issues));
        if (generation === this.generation) { this.issues = issues; if (inbox) this.inbox = inbox; this.error = null; }
        for (const issue of issues) this.statesFor(issue.teamId).catch(() => {}); // so the status menu opens at once
      } catch (e) { if (generation === this.generation) this.error = String(e.message ?? e); }
      finally { if (generation === this.generation) { this.loading = false; this.loaded = true; } cx.notify(); }
    });
    if (manual) cx.notify();
  }

  // Linear-wide search, a moment after typing stops; the newest search wins.
  scheduleSearch(cx) {
    const gen = ++this.searchGen, term = this.text.trim();
    if (term.length < SEARCH_MIN) { this.results = []; this.searching = false; return; }
    this.searching = true;
    cx.timer.after(SEARCH_DELAY_MS, (cx) => {
      if (gen !== this.searchGen) return;
      cx.spawn(async (cx) => {
        try {
          if (!this.config?.apiKey) throw new Error(NO_KEY);
          const data = await gql(this.config.apiKey, SEARCH_QUERY, { term, first: SEARCH_LIMIT });
          if (gen === this.searchGen) { const found = data.searchIssues.nodes.map(parseNode);
            const team = this.config.team; // HERDR_LINEAR_TEAM's results come first; Linear's relevance order is kept within each group.
            this.results = [...byState(found.filter((i) => i.team === team)), ...byState(found.filter((i) => i.team !== team))]; this.searchError = null; }
        } catch (e) { if (gen === this.searchGen) this.searchError = String(e.message ?? e); }
        finally { if (gen === this.searchGen) this.searching = false; cx.notify(); }
      });
    });
  }

  clearSearch(cx) {
    this.search.set_value("");
    this.text = ""; this.results = []; this.searchError = null; this.searching = false; this.searchGen++;
    cx.notify();
  }

  assignToMe(issue, cx) {
    if (!this.viewerId || this.assigning.has(issue.id)) return;
    this.assigning.add(issue.id);
    cx.spawn(async (cx) => {
      try {
        const data = await gql(this.config.apiKey, ASSIGN, { id: issue.id, me: this.viewerId });
        if (!data.issueUpdate.success) throw new Error("Linear did not assign the issue");
        issue.assigneeId = this.viewerId; issue.assigneeName = "you";
        this.refresh(cx);
      } catch (e) { this.searchError = String(e.message ?? e); }
      finally { this.assigning.delete(issue.id); cx.notify(); }
    });
    cx.notify();
  }

  // A team's workflow states, fetched once per team; a failed fetch is retried on the next ask.
  statesFor(teamId) {
    if (!teamId) return Promise.reject(new Error("Issue has no team"));
    let p = this.teamStates.get(teamId);
    if (!p) {
      p = gql(this.config.apiKey, TEAM_STATES_QUERY, { id: teamId }).then((d) => sortStates(d.team.states.nodes));
      p.catch(() => this.teamStates.delete(teamId));
      this.teamStates.set(teamId, p);
    }
    return p;
  }

  openStatusMenu(issue, cx) {
    cx.spawn(async (cx) => {
      try {
        const states = await this.statesFor(issue.teamId);
        openOverlay({ mod: "linear", kind: "popup", at: "pointer", width: STATUS_MENU_WIDTH, height: statusMenuHeight(states),
          props: { dialog: "status", issueId: issue.id, current: issue.state, states } });
      } catch (e) { this.searchError = String(e.message ?? e); cx.notify(); }
    });
  }

  assignButton(issue, t) {
    const busy = this.assigning.has(issue.id);
    return div().id(`assign-${issue.id}`).flex_none().px(7).h(20).flex().items_center().rounded(6)
      .text_size(11).text_color(t.accent).border_1().border_color(t.accent).hover((s) => s.bg(t.raised).text_color(t.foreground))
      .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
      .on_click((_e, cx) => { cx.stop_propagation(); this.assignToMe(issue, cx); }).child(busy ? "Assigning…" : "Assign to me");
  }

  // Show the issue's space, or ask what to start one with. It's named after the issue, in the team's
  // mapped repo if any.
  openSpace(issue) {
    const space = spaceFor(issue);
    if (space) return run("space.select", { id: space.id });
    const folder = this.config?.repos?.[issue.team], name = `${issue.identifier} ${issue.title}`.slice(0, 60);
    const agents = this.config?.agents ?? [];
    openOverlay({ mod: "linear", kind: "popup", width: MENU_WIDTH, height: menuHeight(agents), props: { dialog: "space", name, folder, agents } });
  }

  chip(status, t) {
    const active = this.status === status.id;
    return h_flex().id(`status-${status.id}`).h(24).px(8).flex_none().items_center().rounded(6).cursor_pointer().text_size(11)
      .bg(active ? t.selected : t.raised).text_color(active ? t.foreground : t.muted)
      .hover((s) => s.bg(t.hover)).on_click((_e, cx) => { this.status = status.id; cx.notify(); }).child(status.label);
  }

  // Filled in the linked space's avatar color, if there is one.
  spaceButton(issue, t, color) {
    const base = div().id(`space-${issue.id}`).flex_none().px(7).h(20).flex().items_center().rounded(6).text_size(11).border_1();
    return (color
      ? base.bg(color).border_color(color).text_color("#ffffff").hover((s) => s.opacity(0.85))
      : base.text_color(t.muted).border_color(t.border).hover((s) => s.bg(t.raised).text_color(t.foreground)))
      .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
      .on_click((_e, cx) => { cx.stop_propagation(); this.openSpace(issue); }).child("Space");
  }

  meta(issue, t) {
    const priority = issue.priority > 0 ? issue.priorityLabel : null;
    return h_flex().flex_1().min_w(0).gap(8).items_center().overflow_hidden()
      .child(h_flex().id(`state-${issue.id}`).flex_none().gap(5).items_center().px(4).ml(-4).rounded(4).cursor_pointer()
        .hover((s) => s.bg(t.raised)).tooltip("Change status")
        .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
        .on_click((_e, cx) => { cx.stop_propagation(); this.openStatusMenu(issue, cx); })
        .child(div().size(7).rounded_full().bg(issue.stateColor ?? t.dim))
        .child(div().text_color(t.muted).child(issue.state)))
      .when(Boolean(priority), (e) => e.child(div().flex_none().text_color(t[PRIORITY_COLORS[issue.priority]] ?? t.dim).child(priority)))
      .children(issue.labels.slice(0, 2).map((l) => div().flex_none().px(5).rounded(4).border_1()
        .border_color(`${l.color}88`).text_color(t.muted).child(l.name)));
  }

  row(issue, t, search = false) {
    const space = spaceFor(issue), mine = issue.assigneeId === this.viewerId;
    return v_flex().id(`issue-${issue.id}`).flex_none().px(10).py(7).gap(3).rounded(7).cursor_pointer()
      .when(Boolean(space?.active), (e) => e.bg(t.selected))
      .hover((s) => s.bg(t.hover)).on_click(() => openUrl(issue.url))
      .child(h_flex().gap(8).items_start()
        .child(div().flex_none().pt(1).text_size(11).text_color(t.accent).child(issue.identifier))
        .child(v_flex().flex_1().min_w(0).gap(3)
          .child(h_flex().gap(8).items_start()
            .child(div().flex_1().min_w(0).whitespace_normal().child(issue.title))
            .when(search && !mine, (e) => e.child(this.assignButton(issue, t)))
            .child(this.spaceButton(issue, t, space && spaceColor(space, t))))
          .child(h_flex().gap(8).items_center().text_size(11).text_color(t.dim)
            .child(this.meta(issue, t))
            .when(search, (e) => e.child(div().flex_none().text_color(t.muted).child(mine ? "You" : issue.assigneeName ?? "Unassigned")))
            .child(div().flex_none().child(`${issue.team} · ${ago(issue.updatedAt)}`)))));
  }

  // Marks these inbox rows read in Linear and drops them from the panel; a failure puts them back.
  markRead(groups, cx) {
    const ids = groups.flatMap((g) => g.ids), keys = new Set(groups.map((g) => g.id));
    if (!ids.length || !this.config?.apiKey) return;
    const before = this.inbox;
    this.inbox = (this.inbox ?? []).filter((g) => !keys.has(g.id)); this.inboxError = null;
    cx.spawn(async (cx) => {
      try { await gql(this.config.apiKey, markReadMutation(ids)); }
      catch (e) { this.inbox = before; this.inboxError = String(e.message ?? e); }
      cx.notify();
    });
    cx.notify();
  }

  inboxSection(t) {
    const inbox = this.inbox ?? [], shown = inbox.slice(0, INBOX_SHOWN), more = inbox.length - shown.length;
    const link = (id, label, url) => div().id(id).flex_none().px(6).rounded(4).cursor_pointer().text_size(11).text_color(t.accent)
      .hover((s) => s.bg(t.hover)).on_click(() => openUrl(url)).child(label);
    return v_flex().flex_none().gap(1).pb(6).mb(4).border_b(1).border_color(t.border)
      .child(h_flex().px(10).pt(4).pb(2).gap(6).items_center().text_size(11)
        .child(div().text_color(t.muted).font_semibold().child("Inbox"))
        .child(div().px(5).rounded(4).bg(t.accent).text_color(t.accent_foreground).child(String(inbox.length)))
        .child(div().flex_1())
        .child(div().id("inbox-read-all").flex_none().px(6).rounded(4).cursor_pointer().text_size(11).text_color(t.muted)
          .hover((s) => s.bg(t.hover).text_color(t.foreground)).on_click((_e, cx) => this.markRead(inbox, cx)).child("Mark all read"))
        .child(link("inbox-all", "Open inbox", this.config?.inboxUrl))
        .child(div().id("inbox-toggle").flex_none().px(6).rounded(4).cursor_pointer().text_size(11).text_color(t.muted)
          .hover((s) => s.bg(t.hover).text_color(t.foreground)).on_click((_e, cx) => { this.inboxOpen = !this.inboxOpen; cx.notify(); })
          .child(this.inboxOpen ? "Hide" : "Show")))
      .when(Boolean(this.inboxError), (e) => e.child(div().px(10).py(2).text_size(11).text_color(t.ansi1).child(this.inboxError ?? "")))
      .children((this.inboxOpen ? shown : []).map((n) => v_flex().id(`note-${n.id}`).flex_none().px(10).py(5).gap(2).rounded(7).cursor_pointer()
        .hover((s) => s.bg(t.hover)).on_click(() => openUrl(n.url))
        .child(h_flex().gap(8).items_start()
          .child(div().flex_1().min_w(0).whitespace_normal().text_size(12.5).child(n.title))
          .child(div().flex_none().pt(1).text_size(11).text_color(t.dim).child(ago(n.createdAt)))
          .child(div().id(`note-read-${n.id}`).flex_none().size(20).flex().items_center().justify_center().rounded(5)
            .cursor_pointer().text_size(12).text_color(t.muted).border_1().border_color(t.border).tooltip("Mark read")
            .hover((s) => s.bg(t.raised).text_color(t.foreground))
            .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
            .on_click((_e, cx) => { cx.stop_propagation(); this.markRead([n], cx); }).child("✓")))
        .child(h_flex().gap(6).items_center().text_size(11).text_color(t.muted)
          .child(div().flex_1().min_w(0).truncate().child(n.subtitle))
          .when(n.count > 1, (e) => e.child(div().flex_none().text_color(t.dim).child(`+${n.count - 1}`))))))
      .when(this.inboxOpen && more > 0, (e) => e.child(div().px(10).py(3).child(link("inbox-more", `${more} more in Linear`, this.config?.inboxUrl))));
  }

  emptyText() {
    if (this.error) return this.error;
    if (!this.loaded) return "Loading…";
    return this.issues.length ? "No issues match these filters" : "No open issues assigned to you";
  }

  render() {
    if (this.form) return this.form.render();
    const t = theme(), list = byState(filterIssues(this.issues, { status: this.status, text: this.text }));
    const searching = this.text.trim().length >= SEARCH_MIN, shownIds = new Set(list.map((i) => i.id));
    const results = searching ? filterIssues(this.results, { status: this.status, text: "" }).filter((i) => !shownIds.has(i.id)) : [];
    // A popup sizes to its content, so the status bar's popup gets a fixed height to give the list room.
    return v_flex().id("linear").size_full().min_h(0).when(Boolean(props()?.compact), (e) => e.h(PANEL_HEIGHT)).rounded(10).overflow_hidden().bg(t.background).border_1().border_color(t.border)
      .text_color(t.foreground).text_size(13)
      .child(h_flex().h(42).flex_none().px(14).gap(8).items_center().border_b(1).border_color(t.border)
        .child(div().font_semibold().child("Linear"))
        .child(div().text_size(11).text_color(t.dim).child(`${this.issues.filter(isOpen).length} open`))
        .child(div().flex_1())
        .child(div().id("new-issue").px(8).h(26).flex().items_center().rounded(7).cursor_pointer().text_size(13).text_color(t.muted)
          .hover((s) => s.bg(t.hover).text_color(t.foreground))
          .on_click(() => openOverlay({ mod: "linear", kind: "dialog", width: 520, height: 740, props: { dialog: "new" } })).child("+"))
        .child(div().id("refresh").size(26).flex().items_center().justify_center().rounded(7).cursor_pointer().text_color(t.muted)
          .tooltip("Refresh").hover((s) => s.bg(t.hover).text_color(t.foreground)).on_click((_e, cx) => this.refresh(cx, true))
          .child(svg("mods/linear/refresh.svg").size(13).when(this.loading && this.manual, (e) => e.spin(900)))))
      .child(v_flex().flex_none().px(8).py(7).gap(6).border_b(1).border_color(t.border)
        .child(h_flex().h(28).pl(8).pr(4).gap(4).items_center().rounded(6).border_1()
          .border_color(this.text.trim() ? t.accent : t.border).bg(t.background)
          .child(div().flex_1().min_w(0).h_full().child(Input.new(this.search).h_full().w_full().text_size(12).text_color(t.foreground)))
          .when(this.text.length > 0, (e) => e.child(div().id("search-clear").flex_none().size(20).flex().items_center().justify_center()
            .rounded(4).cursor_pointer().text_size(14).text_color(t.dim).hover((s) => s.bg(t.hover).text_color(t.foreground))
            .on_click((_e, cx) => this.clearSearch(cx)).child("×"))))
        .child(h_flex().gap(6).items_center().children(STATUSES.map((s) => this.chip(s, t)))))
      .child(v_flex().id("linear-list").flex_1().min_h(0).overflow_y_scrollbar().p(6).gap(2)
        .when(!searching && (this.inbox?.length ?? 0) > 0, (e) => e.child(this.inboxSection(t)))
        .children(list.map((issue) => this.row(issue, t)))
        .when(!list.length && !searching, (e) => e.child(div().p(8).text_color(this.error ? t.ansi1 : t.dim).child(this.emptyText())))
        .when(searching, (e) => e
          .child(h_flex().px(10).pt(8).pb(2).gap(6).items_center().text_size(11)
            .child(div().text_color(t.muted).font_semibold().child("Search results"))
            .child(div().text_color(t.dim).child(this.searching ? "searching…" : String(results.length))))
          .when(Boolean(this.searchError), (e) => e.child(div().px(10).py(4).text_size(11).text_color(t.ansi1).child(this.searchError ?? "")))
          .children(results.map((issue) => this.row(issue, t, true)))
          .when(!this.searching && !results.length && !this.searchError, (e) => e.child(div().px(10).py(4).text_size(11).text_color(t.dim).child("No other matches")))));
  }
}
