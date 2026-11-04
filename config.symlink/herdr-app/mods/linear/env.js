// A variable from your shell. Apps launched from the Dock don't get those, so fall back to a login shell.
import { exec } from "herdr";

export async function shellEnv(name) {
  const direct = await exec("sh", ["-c", `printf %s "$${name}"`]).catch(() => null);
  if (direct?.code === 0 && direct.stdout.trim()) return direct.stdout.trim();
  const script = `"\${SHELL:-sh}" -lic 'printf "\\nHERDR_ENV_VALUE=%s\\n" "$${name}"' 2>/dev/null`;
  const login = await exec("sh", ["-c", script]).catch(() => null);
  return login?.stdout?.match(/^HERDR_ENV_VALUE=(.+)$/m)?.[1]?.trim() ?? null;
}
