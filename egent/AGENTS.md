# AGENTS.md — egent

## What this repo is

An Emacs package (29.1+, lexical binding) that manages `agent-shell` sessions
across projects, including sessions that have no Emacs buffer. Successor to
`agent-shell-hq`.

Public symbols use the `egent-` prefix; internals use `egent-<module>--`.

## File map

| File               | Purpose                                                        |
|--------------------|----------------------------------------------------------------|
| `egent.el`         | Umbrella entry point; loads peek only when posframe is present  |
| `egent-core.el`    | `defgroup`, faces, status icons, buffer introspection, grouping |
| `egent-session.el` | Headless ACP `session/list` fetching, cache, resume             |
| `egent-sidebar.el` | Sidebar workspace                                               |
| `egent-peek.el`    | Posframe switcher (live sessions only)                          |
| `egent-session-name.el` | Session naming, by hand or via an external CLI subprocess   |

## Dependencies

- `agent-shell` ≥ 0.66.1 — `agent-shell-buffers`, `agent-shell-cwd`,
  `agent-shell--project-name`, `agent-shell-start`, `agent-shell-viewport`
- `acp` ≥ 0.13.1 — used directly by `egent-session.el`
- `posframe` ≥ 1.4 — optional, peek only
- `persp-mode` ≥ 2.9 — optional, sidebar only

## Key design decisions

**Shared code lives in `egent-core.el`**, not in whichever surface needed it
first. In `agent-shell-hq` the grouping function belonged to the peek module and
the sidebar had to pull in posframe to reach it.

**Past sessions are fetched, not read.** `agent-shell` only issues
`session/list` during shell bootstrap, and its `session-list` /
`session-selected` events carry no payload, so there is nothing to subscribe to.
`egent-session-fetch` builds its own client from the agent config's
`:client-maker`, initializes, asks, and shuts down. Parsing each agent's on-disk
history instead would tie the package to pi's JSONL layout and Claude's projects
directory.

**Fetches are async with a timeout.** `acp-send-request` accepts `:sync`, but its
wait loop (`while (not done) (accept-process-output ...)`) never gives up. Every
fetch settles exactly once — success, failure or `egent-session-timeout` — via
the `finish`/`fail` pair in `egent-session-fetch`.

**Teardown order matters.** `acp` invokes callbacks inside `with-current-buffer`
on the request's buffer, which errors if that buffer is dead. `finish` therefore
calls `acp-shutdown` (which clears pending requests and kills the process)
*before* killing the context buffer.

**Results are cached explicitly.** Rendering must never spawn a subprocess, so
`egent-session--cache` is only written by a fetch and only invalidated by `S` /
`egent-session-forget`. Auto-fetch on workspace open is guarded on
`egent-session-cached-p` and `egent-session-fetching-p`, so a re-render triggered
by a completing fetch cannot start another one.

**A resume is suppressed while it boots.** A resumed shell only reports its
session id once bootstrapping finishes, so `egent-session-resumable` would keep
offering the row and a second `RET` would start a second shell against the same
session. `egent-session--resuming` hides it meanwhile, with a bounded grace
period so a resume that never completes (failed auth, missing adapter) cannot
hide the session permanently.

**A name is told to the agent, not just to Emacs.** `egent--session-name` is
buffer-local, so a name kept only there dies with the buffer, and the session
reappears in the past list under the agent's own title — which is the only
title `session/list` ever reports. `egent-session-name--set` therefore submits
the first command in `egent-session-name-agent-commands` the agent advertises
(pi: `/name`) as an ordinary prompt, which is how a slash command reaches an
agent over ACP. Advertised commands come from `available_commands_update`,
which `agent-shell` keeps in `:available-commands`, so an agent that has no
such command is never sent one. A busy shell is skipped rather than
interrupted, and the cached session list is retitled in place
(`egent-session-retitle-cached`) so closing the buffer does not bring the old
title back before the next fetch.

**Cache keys are normalized** with `directory-file-name` + `expand-file-name`, so
a root with a trailing slash and one without hit the same entry. (The ACP request
itself needs no normalization: `acp-make-session-list-request` already applies
`directory-file-name`.)

**`persp-mode` is optional.** `egent-sidebar--enter-workspace` /
`--exit-workspace` use a perspective when one is available and save/restore the
window configuration otherwise. The file-finder advice for
`egent-sidebar-lock-perspective` is added on enter and removed on exit rather
than installed at load time.

**`default-directory` is buffer-local.** `egent-sidebar-new-shell` binds it
*inside* `with-selected-window`; binding it outside is silently undone by the
target window's own buffer, which would start the shell in the wrong project.

## Conventions

- One `defgroup` (`egent`, in `egent-core.el`); every `defcustom` uses it.
- Internal UI state is `defvar` at file top level.
- Sidebar re-renders on an explicit refresh and on a 2s timer that compares a
  state snapshot, so it only redraws when a session's busy state actually
  changed.
- Comments explain *why*. No measurements or counts in them.

## When making changes

- **New surface**: add `egent-<module>.el`, require `egent-core`, and require it
  from `egent.el`.
- **Grouping logic**: `egent-grouped-buffers` in `egent-core.el`; sidebar and
  peek both use it.
- **Keymaps**: `egent-sidebar-map` and `egent-peek-map`. The sidebar footer is
  generated from `egent-sidebar--hints` via `where-is-internal`, so rebinding a
  key updates the hints; adding a *command* means adding a hint entry.
- **Testing**: byte-compile with the straight build directories on the load path
  and expect zero warnings:

  ```sh
  LP=$(for d in ~/.emacs.d/straight/build/*/; do printf -- "-L %s " "$d"; done)
  emacs -Q --batch $LP -L . -f batch-byte-compile *.el
  ```

  `egent-session-fetch` can be exercised headlessly in batch against a real
  agent; drive it with an `accept-process-output` loop and assert the callback
  fires once, no ` *egent-session-fetch*` buffers survive, and no adapter
  processes are left behind. Cover the missing-binary and never-answers paths
  too — both must settle through the callback rather than signalling or hanging.
  The interactive surfaces have no automated tests.
