// Linear API access shared by the panel and the new-issue dialog.
import { home, appHome, readTextFile, exec } from "herdr";
import { API_URL, ISSUES_QUERY, PAGE_SIZE, MAX_PAGES, parseConfig, linearError, parseIssues, nextCursor } from "./linear.js";
import { shellEnv } from "./env.js";

export const NO_KEY = "No Linear API key. Add it to ~/.config/herdr-app/linear.json or 1Password.";

const OP_REFERENCE = "op://Employee/Linear PAT/credential";

export async function loadConfig() {
  const text = await readTextFile(`${appHome()}/linear.json`).catch(() => null);
  const config = parseConfig(text, home());
  if (!config.apiKey && !config.error) {
    const fromOp = await (opKey ??= readOpKey());
    if (fromOp.apiKey) {
      config.apiKey = fromOp.apiKey;
    } else {
      opKey = null; // so the next load asks 1Password again
      config.error = fromOp.error;
    }
  }
  return { ...config, ...(await (personal ??= loadPersonal())) };
}

// Read once: the panel and status bar reload their config every minute or two, and `op` is slow
// (and may ask to unlock 1Password).
let opKey = null;
async function readOpKey() {
  const result = await exec("op", ["read", OP_REFERENCE]).catch((e) => ({ code: 1, stderr: String(e.message ?? e) }));
  if (result.code === 0) {
    return { apiKey: result.stdout.trim() };
  } else {
    return { error: `Couldn't read the Linear API key from 1Password: ${result.stderr.trim()}` };
  }
}

// Read once: the panel reloads its config every minute, and a login shell is slow to start.
let personal = null;
async function loadPersonal() {
  const [workspace, team, agents] = await Promise.all(["HERDR_LINEAR_WORKSPACE", "HERDR_LINEAR_TEAM", "HERDR_LINEAR_AGENTS"].map(shellEnv));
  return {
    inboxUrl: workspace ? `https://linear.app/${encodeURIComponent(workspace)}/inbox` : "https://linear.app/",
    team,
    agents: (agents ?? "").split(",").map((a) => a.trim()).filter(Boolean),
  };
}

// One GraphQL request; throws a short message on HTTP or GraphQL errors.
export async function gql(apiKey, query, variables = {}) {
  const r = await fetch(API_URL, {
    method: "POST",
    headers: { "Content-Type": "application/json", Authorization: apiKey },
    body: JSON.stringify({ query, variables }),
  });
  const body = await r.text();
  if (!r.ok) throw new Error(linearError(r.status, body));
  const json = JSON.parse(body);
  const message = json?.errors?.[0]?.extensions?.userPresentableMessage ?? json?.errors?.[0]?.message;
  if (message) throw new Error(message);
  return json.data;
}

// Every page of your open issues, following the cursor; MAX_PAGES stops a cursor that never ends.
export async function fetchIssues(apiKey) {
  const issues = [];
  let after = null;
  for (let page = 0; page < MAX_PAGES; page++) {
    const json = { data: await gql(apiKey, ISSUES_QUERY, { first: PAGE_SIZE, after }) };
    issues.push(...parseIssues(json));
    after = nextCursor(json);
    if (!after) break;
  }
  return issues;
}
