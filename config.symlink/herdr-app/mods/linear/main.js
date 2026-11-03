// Linear issues assigned to me that are started or unstarted, most urgent first.
// A click opens the issue in Linear. The API key comes from 1Password, like bin/linear-work.

import { View, div } from "gpui-kit";
import { v_flex, h_flex } from "gpui-base";
import { exec, theme } from "herdr";

const REFRESH_MS = 5 * 60 * 1000;
const API_KEY_REFERENCE = "op://Employee/Linear PAT/credential";

const QUERY = `
  query {
    viewer {
      assignedIssues(first: 100, filter: { state: { type: { in: ["started", "unstarted"] } } }) {
        nodes {
          id
          identifier
          title
          url
          branchName
          priority
          updatedAt
          team { key }
          state { name type color }
        }
      }
    }
  }
`;

const STATE_RANK = { started: 0, unstarted: 1 };
// Linear's priority 0 means "no priority", which belongs after Low (4).
const priorityRank = (priority) => (priority === 0 ? 5 : priority);
const byUrgency = (a, b) =>
  STATE_RANK[a.state.type] - STATE_RANK[b.state.type] ||
  priorityRank(a.priority) - priorityRank(b.priority) ||
  b.updatedAt.localeCompare(a.updatedAt);

async function readApiKey() {
  const result = await exec("op", ["read", API_KEY_REFERENCE]);
  if (result.code === 0) {
    return result.stdout.trim();
  } else {
    throw new Error(`Couldn't read the Linear API key from 1Password: ${result.stderr.trim()}`);
  }
}

async function fetchIssues(apiKey) {
  const response = await fetch("https://api.linear.app/graphql", {
    method: "POST",
    headers: { "Content-Type": "application/json", Authorization: apiKey },
    body: JSON.stringify({ query: QUERY }),
  });
  const body = await response.json();
  if (response.ok && !body.errors) {
    return body.data.viewer.assignedIssues.nodes.sort(byUrgency);
  } else {
    throw new Error(body.errors?.[0]?.message ?? `Linear responded ${response.status}.`);
  }
}

export default class Linear extends View {
  init(_props, cx) {
    this.apiKey = null;
    this.issues = null;
    this.error = null;
    this.refresh(cx);
    cx.timer.every(REFRESH_MS, (cx) => this.refresh(cx));
  }

  // The key is kept between refreshes so 1Password isn't asked every few minutes,
  // and dropped after a failure in case it was rotated.
  refresh(cx) {
    cx.spawn(async (cx) => {
      try {
        this.apiKey = this.apiKey ?? (await readApiKey());
        this.issues = await fetchIssues(this.apiKey);
        this.error = null;
      } catch (error) {
        this.apiKey = null;
        this.error = error.message;
      }
      cx.notify();
    });
  }

  row(t, issue) {
    return h_flex()
      .id(`linear-${issue.id}`)
      .px(8)
      .h(30)
      .flex_none()
      .rounded(8)
      .items_center()
      .gap(8)
      .cursor_pointer()
      .hover((s) => s.bg(t.hover))
      .tooltip(`${issue.state.name} · ${issue.title}`)
      .on_click(() => exec("open", [issue.url]))
      .child(div().size(8).flex_none().rounded(4).bg(issue.state.color))
      .child(div().flex_none().text_size(11).text_color(t.dim).child(issue.identifier))
      .child(div().flex_1().min_w(0).truncate().text_color(t.foreground).child(issue.title));
  }

  message(t, text) {
    return div().px(8).text_size(12).text_color(t.dim).child(text);
  }

  body(t) {
    if (this.error) {
      return [this.message(t, this.error)];
    } else if (this.issues === null) {
      return [this.message(t, "Loading…")];
    } else if (this.issues.length === 0) {
      return [this.message(t, "Nothing assigned to you.")];
    } else {
      return this.issues.map((issue) => this.row(t, issue));
    }
  }

  render(_cx) {
    const t = theme();
    return v_flex()
      .size_full()
      .min_h(0)
      .bg(t.chrome)
      .border_t(1)
      .border_color(t.border)
      .text_size(13)
      .child(
        h_flex()
          .px(16)
          .h(41)
          .pt(14)
          .pb(5)
          .flex_none()
          .items_center()
          .justify_between()
          .child(div().text_size(11.5).font_medium().text_color(t.dim).child("Linear"))
          .child(
            div()
              .id("linear-refresh")
              .text_size(12)
              .text_color(t.dim)
              .cursor_pointer()
              .hover((s) => s.text_color(t.foreground))
              .tooltip("Refresh")
              .on_click((_e, cx) => this.refresh(cx))
              .child("↻"),
          ),
      )
      .child(v_flex().id("linear-list").flex_1().min_h(0).px(8).overflow_y_scrollbar().children(this.body(t)));
  }
}
