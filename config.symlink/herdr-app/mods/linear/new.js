// The "New issue" dialog: title, description, team, status, priority, Investment Category, other labels, project.
// Opened by the panel's + button as an overlay of this same mod with props { dialog: "new" }.
import { div } from "gpui-kit";
import { v_flex, h_flex, InputState, Input, TextareaState, Textarea } from "gpui-base";
import { run, props, closeOverlay, theme, shared, setShared } from "herdr";
import { loadConfig, gql, NO_KEY } from "./api.js";

const LAST = "linear.new.last";
const CATEGORY_GROUP = "Investment Category";
const MAX_LABEL_MATCHES = 12;
// Linear's workflow order; states of the same type sort by position.
const STATE_ORDER = ["triage", "backlog", "unstarted", "started", "completed", "canceled"];
// Linear's priority values: 0 none, 1 urgent … 4 low.
const PRIORITIES = [[0, "No priority"], [1, "Urgent"], [2, "High"], [3, "Medium"], [4, "Low"]];

const VIEWER = `{ viewer { id teams { nodes { id key name } } } }`;
const TEAM = `query($id: String!) { team(id: $id) {
  states(first: 50) { nodes { id name type color position } }
  labels(first: 250) { nodes { id name color isGroup parent { id name } } }
  projects(first: 50, filter: { state: { nin: ["completed", "canceled"] } }) { nodes { id name } }
} }`;
const CREATE = `mutation($input: IssueCreateInput!) { issueCreate(input: $input) {
  success issue { identifier title url team { key } }
} }`;

export class NewIssueForm {
  init(cx) {
    const last = shared(LAST) ?? {};
    this.last = last;
    this.teams = []; this.labels = []; this.projects = []; this.states = []; this.selected = new Set();
    this.stateId = null; this.stateName = last.stateName ?? null; this.priority = last.priority ?? 0;
    this.teamId = null; this.projectId = last.projectId ?? null; this.assignMe = false;
    this.openSpace = last.openSpace ?? false; this.labelFilter = ""; this.busy = false; this.error = null;
    this.title = InputState.new({ placeholder: "Issue title" });
    this.description = TextareaState.new({ placeholder: "Description (optional, Markdown)" });
    this.description.set_auto_grow(4, 10);
    this.filter = InputState.new({ placeholder: "Filter labels" });
    this.filter.on("change", (_e, cx) => { this.labelFilter = this.filter.value().trim().toLowerCase(); cx.notify(); });
    this.title.on("submit", (_e, cx) => this.create(cx));
    cx.spawn(async (cx) => {
      try {
        this.config = await loadConfig();
        if (!this.config.apiKey) throw new Error(this.config.error ?? NO_KEY);
        const data = await gql(this.config.apiKey, VIEWER);
        this.viewerId = data.viewer.id;
        this.teams = data.viewer.teams.nodes;
        const team = this.teams.find((t) => t.id === last.teamId) ?? this.teams.find((t) => t.key === this.config.team) ?? this.teams[0];
        if (team) await this.loadTeam(team.id, cx, true);
      } catch (e) { this.error = String(e.message ?? e); }
      cx.notify();
    });
  }

  async loadTeam(teamId, cx, restoring = false) {
    this.teamId = teamId; this.labels = []; this.projects = []; this.states = []; this.loadingTeam = true; cx.notify();
    try {
      const data = await gql(this.config.apiKey, TEAM, { id: teamId });
      if (this.teamId !== teamId) return;
      this.labels = data.team.labels.nodes.filter((l) => !l.isGroup);
      this.projects = data.team.projects.nodes;
      const rank = (s) => STATE_ORDER.indexOf(s.type);
      this.states = data.team.states.nodes.filter((s) => STATE_ORDER.includes(s.type)).sort((a, b) => rank(a) - rank(b) || a.position - b.position);
      // Keep the status across teams by name; otherwise leave it to the team's default.
      this.stateId = this.states.find((s) => s.name === this.stateName)?.id ?? null;
      const ids = new Set(this.labels.map((l) => l.id));
      const keep = restoring ? (this.last.categoryId ? [this.last.categoryId] : []) : [...this.selected];
      this.selected = new Set(keep.filter((id) => ids.has(id)));
      if (!this.projects.some((p) => p.id === this.projectId)) this.projectId = null;
    } catch (e) { this.error = String(e.message ?? e); }
    finally { this.loadingTeam = false; cx.notify(); }
  }

  // Linear allows one label per group: picking one drops its siblings.
  toggleLabel(label) {
    if (this.selected.has(label.id)) { this.selected.delete(label.id); return; }
    if (label.parent) for (const l of this.labels) if (l.parent?.id === label.parent.id) this.selected.delete(l.id);
    this.selected.add(label.id);
  }

  create(cx) {
    const title = this.title.value().trim();
    if (this.busy || !title || !this.teamId) return;
    this.busy = true; this.error = null;
    const category = this.labels.find((l) => this.selected.has(l.id) && l.parent?.name === CATEGORY_GROUP);
    const input = {
      teamId: this.teamId, title,
      ...(this.description.value().trim() ? { description: this.description.value() } : {}),
      ...(this.assignMe && this.viewerId ? { assigneeId: this.viewerId } : {}),
      ...(this.selected.size ? { labelIds: [...this.selected] } : {}),
      ...(this.projectId ? { projectId: this.projectId } : {}),
      ...(this.stateId ? { stateId: this.stateId } : {}),
      ...(this.priority ? { priority: this.priority } : {}),
    };
    cx.spawn(async (cx) => {
      try {
        const data = await gql(this.config.apiKey, CREATE, { input });
        if (!data.issueCreate.success) throw new Error("Linear did not create the issue");
        const issue = data.issueCreate.issue;
        setShared(LAST, { teamId: this.teamId, categoryId: category?.id ?? null, projectId: this.projectId,
          stateName: this.stateName, priority: this.priority, openSpace: this.openSpace });
        if (this.openSpace) {
          const folder = this.config.repos?.[issue.team.key];
          run("space.new", { name: `${issue.identifier} ${issue.title}`.slice(0, 60), ...(folder ? { folder } : {}) });
        }
        run("linear.refresh");
        closeOverlay(props().overlay);
      } catch (e) { this.error = String(e.message ?? e); }
      finally { this.busy = false; cx.notify(); }
    });
    cx.notify();
  }

  chip(id, label, on, onClick, t, color = null) {
    return h_flex().id(id).h(24).px(8).gap(5).flex_none().items_center().rounded(6).cursor_pointer().text_size(11.5)
      .bg(on ? t.selected : t.raised).text_color(on ? t.foreground : t.muted)
      .hover((s) => s.bg(t.hover)).on_click((_e, cx) => { onClick(cx); cx.notify(); })
      .when(Boolean(color), (e) => e.child(div().size(7).rounded_full().bg(color ?? t.dim)))
      .child(label);
  }

  section(title, child, t) {
    return v_flex().gap(6).child(div().text_size(11).text_color(t.dim).child(title)).child(child);
  }

  field(state, t) {
    return div().h(30).px(10).flex().items_center().rounded(8).border_1().border_color(t.border).bg(t.background)
      .child(Input.new(state).h_full().text_size(12.5).text_color(t.foreground));
  }

  render() {
    const t = theme();
    const categories = this.labels.filter((l) => l.parent?.name === CATEGORY_GROUP);
    const others = this.labels.filter((l) => l.parent?.name !== CATEGORY_GROUP);
    const chosen = others.filter((l) => this.selected.has(l.id));
    const matches = this.labelFilter
      ? others.filter((l) => !this.selected.has(l.id) && `${l.parent?.name ?? ""} ${l.name}`.toLowerCase().includes(this.labelFilter)).slice(0, MAX_LABEL_MATCHES)
      : [];
    const labelName = (l) => (l.parent ? `${l.parent.name}: ${l.name}` : l.name);
    const ready = Boolean(this.teamId) && !this.busy;
    return v_flex().id("linear-new").size_full().p(16).gap(14).bg(t.popover).text_color(t.foreground).text_size(12.5)
      .child(div().flex_none().text_size(14).font_semibold().child("New Linear issue"))
      .child(v_flex().id("linear-new-body").flex_1().min_h(0).overflow_y_scrollbar().gap(14)
        .child(this.field(this.title, t))
        .child(div().px(10).py(6).rounded(8).border_1().border_color(t.border).bg(t.background)
          .child(Textarea.new(this.description).w_full().text_size(12.5).text_color(t.foreground)))
        .child(this.section("Team", h_flex().flex_wrap().gap(6).children(this.teams.map((team) =>
          this.chip(`team-${team.key}`, team.name, this.teamId === team.id, (cx) => { if (this.teamId !== team.id) cx.spawn(async (cx) => this.loadTeam(team.id, cx)); }, t))), t))
        .child(this.section("Status", this.loadingTeam ? div().text_color(t.dim).child("Loading…")
          : h_flex().flex_wrap().gap(6)
            .child(this.chip("state-default", "Team default", !this.stateId, () => { this.stateId = null; this.stateName = null; }, t))
            .children(this.states.map((s) => this.chip(`state-${s.id}`, s.name, this.stateId === s.id,
              () => { this.stateId = s.id; this.stateName = s.name; }, t, s.color))), t))
        .child(this.section("Priority", h_flex().flex_wrap().gap(6).children(PRIORITIES.map(([value, name]) =>
          this.chip(`prio-${value}`, name, this.priority === value, () => { this.priority = value; }, t))), t))
        .child(this.section("Investment Category", this.loadingTeam ? div().text_color(t.dim).child("Loading…")
          : h_flex().flex_wrap().gap(6).children(categories.map((l) =>
            this.chip(`cat-${l.id}`, l.name, this.selected.has(l.id), () => this.toggleLabel(l), t, l.color))), t))
        .child(this.section("Labels", v_flex().gap(6)
          .when(chosen.length > 0, (e) => e.child(h_flex().flex_wrap().gap(6).children(chosen.map((l) =>
            this.chip(`lab-${l.id}`, `${labelName(l)} ×`, true, () => this.toggleLabel(l), t, l.color)))))
          .child(this.field(this.filter, t))
          .when(matches.length > 0, (e) => e.child(h_flex().flex_wrap().gap(6).children(matches.map((l) =>
            this.chip(`lab-${l.id}`, labelName(l), false, () => this.toggleLabel(l), t, l.color))))), t))
        .child(this.section("Project", h_flex().flex_wrap().gap(6)
          .child(this.chip("proj-none", "None", !this.projectId, () => { this.projectId = null; }, t))
          .children(this.projects.map((p) => this.chip(`proj-${p.id}`, p.name, this.projectId === p.id, () => { this.projectId = p.id; }, t))), t))
        .child(this.section("Options", h_flex().flex_wrap().gap(6)
          .child(this.chip("opt-me", this.assignMe ? "✓ Assign to me" : "Assign to me", this.assignMe, () => { this.assignMe = !this.assignMe; }, t))
          .child(this.chip("opt-space", this.openSpace ? "✓ Open a space for it" : "Open a space for it", this.openSpace, () => { this.openSpace = !this.openSpace; }, t)), t)))
      .child(h_flex().flex_none().gap(8).items_center()
        .child(div().flex_1().min_w(0).text_size(11).text_color(t.ansi1).child(this.error ?? ""))
        .child(h_flex().id("new-cancel").h(30).px(12).items_center().rounded(8).cursor_pointer().text_color(t.muted)
          .hover((s) => s.bg(t.hover)).on_click(() => closeOverlay(props().overlay)).child("Cancel"))
        .child(h_flex().id("new-create").h(30).px(14).items_center().rounded(8).cursor_pointer()
          .bg(ready ? t.solid : t.raised).text_color(ready ? t.solid_foreground : t.dim)
          .on_click((_e, cx) => this.create(cx)).child(this.busy ? "Creating…" : "Create issue")));
  }
}
