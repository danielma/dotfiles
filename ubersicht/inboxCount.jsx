export const command =
  '/usr/local/bin/tmux list-windows -t inbox -F "#W" 2>&1 | grep inbox-cli | sed "s/^inbox-cli (\\([0-9]*\\))/\\1/"';

export const refreshFrequency = 10000;

export const initialState = { output: 0 };

export function updateState(event, previousState) {
  const output = parseInt(event.output, 10);

  doImperativeWork(output);

  return { output };
}

function doImperativeWork(count) {
  const el = document.querySelector("#uebersicht .inboxCount");

  if (count > 0) {
    el.innerHTML = count;
    el.classList.remove("bg-base0B-FF-important");
  } else {
    el.innerHTML = "✔";
    el.classList.add("bg-base0B-FF-important");
  }
}

export function render() {
  return null;
}
