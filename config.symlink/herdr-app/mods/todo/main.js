// A todo list per space, kept as a Markdown checklist that agents can read
// and edit too: todos/<space id>.md (`herdr-todo path` prints it).
import { View, div, svg } from "gpui-kit";
import { v_flex, h_flex, InputState, Input } from "gpui-base";
import { spaces, exec, readTextFile, writeTextFile, watchFolder, theme, appHome } from "herdr";

const ITEM = /^(\s*)([-*] \[)([ xX])(\] )(.*)$/;
const folder = () => `${appHome()}/todos`;
const fileFor = (space) => `${folder()}/${space.id.replaceAll("/", "__")}.md`;
const header = (space) =>
  `<!-- Herdr todo list for the space "${space.name}" (${space.id}). One "- [ ] item" per line; "- [x]" is done; indent two spaces to nest. -->\n`;

const indentOf = (line) => /^\s*/.exec(line)[0].length;

// An item owns the lines below it that are indented past it (its children,
// and theirs), so it ends at the next line that isn't. Blank lines between
// children don't end it, but trailing ones aren't its own.
function endOf(lines, index, indent) {
  let end = index + 1;
  while (end < lines.length && (!lines[end].trim() || indentOf(lines[end]) > indent)) {
    end++;
  }
  while (end > index + 1 && !lines[end - 1].trim()) {
    end--;
  }
  return end;
}

// An item's depth counts the items above it that it's indented under, so
// any consistent indent nests (two spaces, four, or a tab).
function itemsIn(text) {
  const lines = (text ?? "").split("\n"), ancestors = [];
  return lines.flatMap((line, index) => {
    const match = ITEM.exec(line), indent = indentOf(line);
    if (line.trim()) {
      while (ancestors.length && ancestors.at(-1) >= indent) {
        ancestors.pop();
      }
    }
    if (match) {
      const depth = ancestors.length;
      ancestors.push(indent);
      return [{ index, end: endOf(lines, index, indent), indent, depth, done: match[3] !== " ", text: match[5] }];
    } else {
      return [];
    }
  });
}

const within = (item, index) => index >= item.index && index < item.end;
const siblingBefore = (items, item) => items.findLast((other) => other.index < item.index && other.depth <= item.depth);
const parentOf = (items, item) => items.findLast((other) => other.index < item.index && other.depth < item.depth);
const canIndent = (items, item) => siblingBefore(items, item)?.depth === item.depth;
const canOutdent = (items, item) => Boolean(parentOf(items, item));

export default class Todo extends View {
  init(_props, cx) {
    this.text = null;
    this.typing = false;
    this.pressed = null;
    this.over = null;
    this.hovered = null;
    this.input = InputState.new({ placeholder: "Add a todo" });
    this.input.on("submit", (_e, cx) => this.add(cx));
    this.input.on("focus", (_e, cx) => { this.typing = true; cx.notify(); });
    this.input.on("blur", (_e, cx) => { this.typing = false; cx.notify(); });
    cx.spawn(async (cx) => {
      await exec("mkdir", ["-p", folder()]);
      for (;;) {
        await watchFolder(folder());
        this.load(cx);
      }
    });
  }

  load(cx) {
    const space = this.space;
    if (space) {
      cx.spawn(async (cx) => {
        const text = await readTextFile(fileFor(space));
        if (this.space?.id === space.id) {
          this.text = text;
          cx.notify();
        }
      });
    }
  }

  // Changes the lines we last read; a stale snapshot (an agent wrote in the
  // meantime) rejects, so reload and let the person try again on fresh text.
  write(change, cx) {
    const space = this.space, before = this.text;
    const lines = (before ?? header(space)).replace(/\n$/, "").split("\n");
    const after = `${change(lines).join("\n")}\n`;
    this.text = after;
    cx.notify();
    cx.spawn(async (cx) => {
      try {
        await writeTextFile(fileFor(space), after, before);
      } catch {
        this.load(cx);
      }
    });
  }

  add(cx) {
    const text = this.input.value().trim();
    if (text && this.space) {
      this.input.set_value("");
      this.write((lines) => [...lines, `- [ ] ${text}`], cx);
    }
  }

  toggle(item, cx) {
    this.write((lines) => lines.map((line, i) =>
      i === item.index ? line.replace(ITEM, (_m, indent, a, mark, b, rest) => `${indent}${a}${mark === " " ? "x" : " "}${b}${rest}`) : line), cx);
  }

  remove(item, cx) {
    this.write((lines) => lines.filter((_line, i) => !within(item, i)), cx);
  }

  // Nests an item, with its children, under the sibling above it.
  indent(item, items, cx) {
    if (canIndent(items, item)) {
      this.write((lines) => lines.map((line, i) => within(item, i) && line.trim() ? `  ${line}` : line), cx);
    }
  }

  // Lifts an item, with its children, to its parent's level, landing just
  // after the parent's children so the siblings below it stay where they are.
  outdent(item, items, cx) {
    const parent = parentOf(items, item);
    if (parent) {
      const shift = item.indent - parent.indent;
      this.write((lines) => {
        const block = lines.slice(item.index, item.end).map((line) => line.slice(Math.min(shift, indentOf(line))));
        const rest = lines.filter((_line, i) => !within(item, i));
        rest.splice(parent.end - block.length, 0, ...block);
        return rest;
      }, cx);
    }
  }

  hover(item, hovered, cx) {
    if (hovered) {
      this.hovered = item.index;
    } else if (this.hovered === item.index) {
      this.hovered = null;
    }
    cx.notify();
  }

  // Moves an item with its children: below the target and its children
  // when dragging down, above the target when dragging up. Indents stay as
  // they were, so a moved item may nest under a new parent.
  move(from, to, cx) {
    this.write((lines) => {
      const block = lines.slice(from.index, from.end), rest = lines.filter((_line, i) => !within(from, i));
      const at = to.index > from.index ? to.end - block.length : to.index;
      rest.splice(at, 0, ...block);
      return rest;
    }, cx);
  }

  cancelDrag(cx) {
    if (this.pressed !== null) {
      this.pressed = null;
      this.over = null;
      cx.notify();
    }
  }

  drop(item, cx) {
    const from = this.pressed, over = this.over;
    this.pressed = null;
    this.over = null;
    if (from !== null && over === item.index && !within(from, item.index)) {
      this.move(from, item, cx);
    } else {
      cx.notify();
    }
  }

  render(cx) {
    const t = theme(), space = spaces().find((s) => s.active) ?? null;
    if (space?.id !== this.space?.id) {
      this.space = space;
      this.text = null;
      this.load(cx);
    } else {
      this.space = space;
    }
    const items = itemsIn(this.text), open = items.filter((item) => !item.done).length;
    const dragging = this.pressed !== null;
    const target = dragging && !within(this.pressed, this.over) ? items.find((item) => item.index === this.over) : null;
    const down = Boolean(target) && target.index > this.pressed.index;
    const landing = down ? items.findLast((item) => item.index < target.end) : target;
    const nestButton = (id, icon, enabled, click) => div().id(id).size(16).flex_none().flex().items_center().justify_center().rounded(4)
      .text_color(t.dim).when(!enabled, (el) => el.opacity(0.3))
      .when(enabled, (el) => el.cursor_pointer().hover((s) => s.text_color(t.foreground).bg(t.hover)).on_click((_e, cx) => click(cx)))
      .child(svg(icon).size(12));
    const row = (item) => h_flex().id(`todo-${item.index}`).relative().min_h(28).flex_none().pl(8 + item.depth * 20).pr(8).py(4).gap(8).items_start().rounded(7)
      .hover((s) => s.bg(t.hover))
      .when(Boolean(target) && within(this.pressed, item.index), (el) => el.opacity(0.5))
      .when(landing === item, (el) => el.child(
        div().absolute().left(0).right(0).h(2).bg(t.accent).when(down, (line) => line.bottom(0)).when(!down, (line) => line.top(0))))
      .on_mouse_down("left", (_e, cx) => {
        this.pressed = item;
        this.over = item.index;
        cx.notify();
      })
      .on_mouse_move((_e, cx) => {
        if (this.pressed !== null && this.over !== item.index) {
          this.over = item.index;
          cx.notify();
        }
      })
      .on_mouse_up("left", (_e, cx) => this.drop(item, cx))
      .on_hover((hovered, cx) => this.hover(item, hovered, cx))
      .child(div().id(`check-${item.index}`).mt(2).size(14).flex_none().rounded(4).border_1().cursor_pointer()
        .border_color(item.done ? t.accent : t.input).bg(item.done ? t.accent : t.background)
        .flex().items_center().justify_center().text_size(10).text_color(t.accent_foreground)
        .on_click((_e, cx) => this.toggle(item, cx)).child(item.done ? "✓" : ""))
      .child(div().flex_1().min_w(0).text_color(item.done ? t.dim : t.foreground).when(item.done, (el) => el.line_through()).child(item.text))
      .child(h_flex().flex_none().gap(2).when(this.hovered !== item.index || Boolean(target), (el) => el.invisible())
        .child(nestButton(`outdent-${item.index}`, "mods/todo/outdent.svg", canOutdent(items, item), (cx) => this.outdent(item, items, cx)))
        .child(nestButton(`indent-${item.index}`, "mods/todo/indent.svg", canIndent(items, item), (cx) => this.indent(item, items, cx))))
      .child(div().id(`remove-${item.index}`).flex_none().text_color(t.dim).cursor_pointer().hover((s) => s.text_color(t.foreground))
        .on_click((_e, cx) => this.remove(item, cx)).child(svg("mods/pane/close.svg").size(12)));
    return v_flex().id("todo").size_full().min_h(0).rounded(10).overflow_hidden().bg(t.background).border_1().border_color(t.border).text_color(t.foreground).text_size(13)
      .on_mouse_up("left", (_e, cx) => this.cancelDrag(cx))
      .on_mouse_down_out((_e, cx) => this.cancelDrag(cx))
      .child(h_flex().h(42).flex_none().px(14).gap(8).items_center().border_b(1).border_color(t.border)
        .child(div().font_semibold().child("Todo"))
        .child(div().px(5).py(1).rounded(4).border_1().border_color(t.border).text_size(10).text_color(t.muted).child(`${open} open`))
        .child(div().flex_1().min_w(0).truncate().text_right().text_size(11).text_color(t.dim).child(space?.name ?? "")))
      .child(div().flex_none().p(10).border_b(1).border_color(t.border)
        .child(div().id("todo-input").h(32).px(10).flex().items_center().rounded(8).border_1().border_color(this.typing ? t.accent : t.border).cursor_text()
          .on_mouse_down("left", (_e, cx) => cx.stop_propagation()).child(Input.new(this.input).h_full().text_size(12).text_color(t.foreground))))
      .child(v_flex().id("todo-list").flex_1().min_h(0).overflow_y_scrollbar().p(6).gap(1)
        .children(items.map(row))
        .when(!items.length, (el) => el.child(div().p(8).text_color(t.dim).child(space ? "Nothing to do in this space" : "No space"))))
      .when(Boolean(space), (el) => el.child(div().flex_none().px(12).py(6).border_t(1).border_color(t.border).text_size(10.5).text_color(t.dim).truncate()
        .child(fileFor(space).replace(appHome(), "~/.config/herdr-app"))));
  }
}
