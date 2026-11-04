// Pure helpers for the Linear panel: the GraphQL query, config and response parsing, filters.
// No Herdr imports, so `node --test` can load this file.

export const API_URL = "https://api.linear.app/graphql";

// Linear's largest page; a load follows `endCursor` until the last page, up to MAX_PAGES.
export const PAGE_SIZE = 250;
export const MAX_PAGES = 40;

// The viewer's open issues: anything not completed or canceled, most recently updated first.
export const ISSUES_QUERY = `query AssignedIssues($first: Int!, $after: String) {
  viewer {
    assignedIssues(first: $first, after: $after, orderBy: updatedAt, filter: { state: { type: { nin: ["completed", "canceled", "duplicate"] } } }) {
      pageInfo { hasNextPage endCursor }
      nodes {
        id identifier title url branchName priority priorityLabel updatedAt description
        state { name type color }
        team { id key name }
        labels { nodes { name color } }
      }
    }
  }
}`;

// A team's workflow states, for the status menu: Linear's workflow order, then position within a type.
export const STATE_ORDER = ["triage", "backlog", "unstarted", "started", "completed", "canceled"];
export const TEAM_STATES_QUERY = `query($id: String!) { team(id: $id) { states(first: 50) { nodes { id name type color position } } } }`;
export function sortStates(nodes) {
  const rank = (s) => STATE_ORDER.indexOf(s.type);
  return (nodes ?? []).filter((s) => STATE_ORDER.includes(s.type)).sort((a, b) => rank(a) - rank(b) || a.position - b.position);
}

// Status chips, by Linear's workflow state types.
export const STATUSES = [
  { id: "all", label: "All", types: null },
  { id: "todo", label: "Todo", types: ["unstarted"] },
  { id: "doing", label: "In progress", types: ["started"] },
  { id: "backlog", label: "Backlog", types: ["backlog", "triage"] },
];

const expand = (path, home) => (path.startsWith("~/") ? `${home}${path.slice(1)}` : path);

// `~/.config/herdr-app/linear.json`: { "apiKey": "lin_api_…", "repos": { "<TEAM KEY>": "~/dev/repo" },
// "repoChoices": ["~/dev/other", …] } (more folders on this computer the agent dialog offers).
export function parseConfig(text, home) {
  if (text == null) return { apiKey: null, repos: {}, repoChoices: [] };
  let raw;
  try { raw = JSON.parse(text); } catch { return { apiKey: null, repos: {}, repoChoices: [], error: "linear.json is not valid JSON" }; }
  const apiKey = typeof raw?.apiKey === "string" && raw.apiKey.trim() ? raw.apiKey.trim() : null;
  const repos = {};
  for (const [team, folder] of Object.entries(raw?.repos ?? {})) {
    if (typeof folder === "string" && folder.trim()) repos[team.toUpperCase()] = expand(folder.trim(), home);
  }
  const choices = Array.isArray(raw?.repoChoices) ? raw.repoChoices : [];
  const repoChoices = choices.filter((f) => typeof f === "string" && f.trim()).map((f) => expand(f.trim(), home));
  return { apiKey, repos, repoChoices };
}

// The dialog's repositories: the default one first, then each choice on this computer, once each.
export function repoOptions(start, choices) {
  const options = start.cwd ? [start] : [];
  for (const cwd of choices) {
    if (!options.some((o) => o.cwd === cwd && !o.on)) options.push({ cwd, on: null });
  }
  return options;
}

const isAuthError = (message) => /authenticat/i.test(message ?? "");

// A short reason for a failed request, from its status and body.
export function linearError(status, body) {
  if (status === 401) return "Invalid Linear API key";
  try {
    const message = JSON.parse(body)?.errors?.[0]?.message;
    if (message) return isAuthError(message) ? "Invalid Linear API key" : message;
  } catch { /* not JSON */ }
  return `Linear answered ${status}`;
}

// Issues matching a search term, any assignee or state; same fields as ISSUES_QUERY plus the assignee.
export const SEARCH_QUERY = `query Search($term: String!, $first: Int!) {
  searchIssues(term: $term, first: $first) {
    nodes {
      id identifier title url branchName priority priorityLabel updatedAt description
      state { name type color }
      team { id key name }
      labels { nodes { name color } }
      assignee { id name }
    }
  }
}`;

// One issue by its identifier ("TEAM-123"), in any state; same fields as ISSUES_QUERY.
export const ISSUE_QUERY = `query Issue($id: String!) {
  issue(id: $id) {
    id identifier title url branchName priority priorityLabel updatedAt description
    state { name type color }
    team { id key name }
    labels { nodes { name color } }
  }
}`;

const IDENT = /^([A-Z][A-Z0-9]*-\d+)(?:\s|$)/;
// Identifiers of issues linked to a space that still exists: by the link table ({ ident: space id })
// or by a space named "<identifier>" or "<identifier> …".
export function linkedIdentifiers(map, spaces) {
  const ids = new Set(spaces.map((s) => s.id)), idents = new Set();
  for (const [ident, id] of Object.entries(map ?? {})) if (ids.has(id)) idents.add(ident);
  for (const s of spaces) { const m = s.named && s.name?.match(IDENT); if (m) idents.add(m[1]); }
  return [...idents];
}

export const isOpen = (issue) => !["completed", "canceled", "duplicate"].includes(issue.stateType);

export function parseIssues(json) {
  const message = json?.errors?.[0]?.message;
  if (message) throw new Error(isAuthError(message) ? "Invalid Linear API key" : message);
  return (json?.data?.viewer?.assignedIssues?.nodes ?? []).map(parseNode);
}

export function parseNode(n) {
  return {
    id: n.id, identifier: n.identifier, title: n.title, url: n.url, branchName: n.branchName ?? null,
    priority: n.priority ?? 0, priorityLabel: n.priorityLabel ?? "", updatedAt: n.updatedAt,
    description: n.description ?? null,
    state: n.state?.name ?? "", stateType: n.state?.type ?? "", stateColor: n.state?.color ?? null,
    teamId: n.team?.id ?? null, team: n.team?.key ?? "", teamName: n.team?.name ?? "",
    labels: (n.labels?.nodes ?? []).map((l) => ({ name: l.name, color: l.color })),
    assigneeId: n.assignee?.id ?? null, assigneeName: n.assignee?.name ?? null,
  };
}

// The cursor of the next page, or null on the last one.
export function nextCursor(json) {
  const info = json?.data?.viewer?.assignedIssues?.pageInfo;
  return info?.hasNextPage && info.endCursor ? info.endCursor : null;
}

// In progress, then to do, then backlog/triage, then the rest; the existing order is kept within each.
const STATE_RANK = { started: 0, unstarted: 1, backlog: 2, triage: 2 };
export function byState(issues) {
  const rank = (i) => STATE_RANK[i.stateType] ?? 3;
  return issues.map((issue, n) => ({ issue, n })).sort((a, b) => rank(a.issue) - rank(b.issue) || a.n - b.n).map((x) => x.issue);
}

// Inbox notifications (newest first). The API returns each one separately; the panel groups them
// by what they're about, like Linear's own inbox.
export const NOTIFICATIONS_QUERY = `query Inbox { notifications(first: 100) {
  nodes { id type readAt archivedAt snoozedUntilAt createdAt title subtitle url inboxUrl }
} }`;

export function unreadInbox(nodes, now = Date.now()) {
  const groups = new Map();
  for (const n of nodes ?? []) {
    if (n.readAt || n.archivedAt || (n.snoozedUntilAt && Date.parse(n.snoozedUntilAt) > now) || !n.url) continue;
    const key = n.url.split(/[?#]/)[0];
    const g = groups.get(key);
    if (g) { g.count += 1; g.ids.push(n.id); }
    else groups.set(key, { id: n.id, ids: [n.id], title: n.title ?? "", subtitle: n.subtitle ?? "", url: n.inboxUrl || n.url, createdAt: n.createdAt, count: 1 });
  }
  return [...groups.values()];
}

// One request that marks every given notification read.
export function markReadMutation(ids, now = new Date().toISOString()) {
  const parts = ids.map((id, i) => `n${i}: notificationUpdate(id: ${JSON.stringify(id)}, input: { readAt: ${JSON.stringify(now)} }) { success }`);
  return `mutation MarkRead { ${parts.join(" ")} }`;
}

export function filterIssues(issues, { status, text }) {
  const types = STATUSES.find((s) => s.id === status)?.types ?? null;
  const needle = (text ?? "").trim().toLowerCase();
  return issues.filter((i) => (!types || types.includes(i.stateType))
    && (!needle || `${i.identifier} ${i.title}`.toLowerCase().includes(needle)));
}

export function ago(iso, now = Date.now()) {
  const minutes = Math.max(0, Math.round((now - Date.parse(iso)) / 60000));
  if (minutes < 60) return `${minutes}m`;
  if (minutes < 1440) return `${Math.round(minutes / 60)}h`;
  return `${Math.round(minutes / 1440)}d`;
}
