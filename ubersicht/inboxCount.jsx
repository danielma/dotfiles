export const command =
  '/usr/local/bin/tmux list-windows -t inbox -F "#W" 2>&1 | grep inbox-cli | sed "s/^inbox-cli (\\([0-9]*\\))/\\1/"';

export const refreshFrequency = 60000;

export const initialState = { output: 0 };

export function updateState(event, previousState) {
  return { output: parseInt(event.output, 10) };
}

export function render({ output }) {
  const el = document.querySelector("#uebersicht .inboxCount");

  if (output > 0) {
    el.innerHTML = output;
    el.classList.remove("bg-base0B-FF-important");
  } else {
    el.innerHTML = "✔";
    el.classList.add("bg-base0B-FF-important");
  }

  return null;
}
