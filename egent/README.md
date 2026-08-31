# egent

Session management for [agent-shell](https://github.com/xenodium/agent-shell):
one list holding both the sessions open in Emacs and the ones only the agent
still remembers, with resume from either.

This is the successor to `agent-shell-hq`. The sidebar, peek and session-naming
surfaces are the same idea; what's new is that a session is no longer required
to have a buffer to show up.

## Why past sessions need their own machinery

`agent-shell` asks an agent for its history exactly once, while a shell is
bootstrapping, and the events it emits around that (`session-list`,
`session-selected`) carry no payload — so there's nothing to subscribe to and no
cache to read. egent instead drives a short-lived ACP client of its own:
initialize, `session/list`, shut down. That keeps it agnostic about where each
agent keeps its history, which reading pi's JSONL or Claude's projects directory
would not.

Fetches are asynchronous and time-bounded (`egent-session-timeout`, default 20s).
`acp-send-request` does support `:sync`, but its wait loop has no timeout, so an
agent that never answers would wedge Emacs.

## Commands

| Command                | Does                                                      |
|------------------------|-----------------------------------------------------------|
| `egent-sidebar-toggle` | Workspace with a project-grouped sidebar                  |
| `egent-sidebar-focus`  | Jump to the sidebar, opening the workspace if needed      |
| `egent-peek`           | Transient posframe switcher over live sessions            |
| `egent-resume`         | Pick a past session in any project and resume it          |
| `egent-delete-session` | Pick a session an agent remembers and delete it           |
| `egent-name-session`   | Name the current session using an external CLI            |
| `egent-rename-session` | Name the current session by hand                          |

### Sidebar

```
 Egent

 ▾ .emacs.d
    ✓ Pi
    ◔ Claude Code
    ↺ replace with a static text        34m ago
    ↺ agent shell hq session history    37m ago

 ▾ helios-agent
    ✓ Pi
```

Checkmark and wedge rows are live buffers; dashed-circle rows are sessions the
agent remembers that have no buffer. `RET` focuses a live one and resumes a past
one — resuming a session that is already open just switches to it rather than
starting a second shell against the same session.

| Key       | Action                                       |
|-----------|----------------------------------------------|
| `n` / `j` | Next entry, preview                          |
| `p` / `k` | Previous entry, preview                      |
| `RET`     | Focus live session / resume past session     |
| `TAB`     | Collapse or expand a project                 |
| `S`       | Re-ask this project's agents for sessions    |
| `o`       | Resume a session in another project          |
| `r` / `R` | Name current session / all sessions          |
| `K`       | Kill live session / delete past session      |
| `g`       | Refresh                                      |
| `s`       | New shell in the highlighted project         |
| `q`       | Quit, restoring the previous layout          |

Mouse click and double-click work too.

`K` on a live row kills the shell and its viewport buffer, which leaves the
session itself untouched — it comes back as a past row on the next fetch. `K` on
a past row is the other half: it asks the agent to forget the session
(`session/delete`), so it needs no buffer and cannot be undone, hence the
confirmation. `egent-delete-session` does the same for a project the sidebar
isn't showing, since it only lists projects that have a shell open. Agents that
don't advertise the `delete` session capability say so instead.

Opening the workspace fetches past sessions once per project
(`egent-sidebar-auto-fetch-sessions`). With `egent-session-agents` left at
`auto`, that only queries agents already in use in that project — one
short-lived subprocess each. Set it to a list of identifiers
(e.g. `'(pi claude-code)`) to query agents regardless.

`persp-mode` is used when installed and skipped otherwise, in which case the
window configuration is saved and restored instead.

### Session names

`egent-rename-session` names a session by hand; `egent-name-session` sends the
last `egent-session-name-context-chars` characters of a session (the tail —
every buffer opens with the same welcome banner, which would otherwise be most
of the context) to `egent-session-name-command` and takes whatever it prints.

```elisp
(setq egent-session-name-command '("claude" "-p" "--model" "haiku"))  ; default
(setq egent-session-name-command '("llm"))
(setq egent-session-name-command '("ollama" "run" "llama3.2"))
```

The name is kept beside the buffer rather than as its name, so the session
stays reachable through `switch-to-buffer` under the name `agent-shell` gave
it. A name held that way dies with the buffer, though, and every past session
is listed under the title its agent reports, so the name is passed on to the
agent as well: the first command in `egent-session-name-agent-commands` the
agent advertises (pi answers `/name`) is submitted as a prompt. Agents that
advertise none keep their own title, and the name lasts as long as the buffer
does. A busy shell is left alone — renaming does not interrupt a turn.

### Session usage

pi's TUI footer (token totals, cache-hit rate, cost, context fill) never
crosses ACP — pi-acp forwards no usage data, so `agent-shell`'s own usage
readouts stay empty for pi shells. `egent-usage-string' re-derives the same
numbers from the session file pi writes anyway, for a mode line or header
line:

```elisp
(egent-usage-string (current-buffer))
;; => "↑140k ↓11k R1.6M CH99.4% $0.900 5.4%/1.0M"
```

Parsing is incremental: a file is read fully once, later reads only consume
what was appended. `egent-show-usage' messages the same string on demand, and
`egent-usage-include-model' appends the model and thinking level (off by
default — `agent-shell`'s header already shows them). Unlike the session
list, this surface is pi-only: the other agents report usage over ACP,
which `agent-shell` already renders.

## Requirements

- Emacs 29.1+
- `agent-shell` 0.66.1+ and `acp` 0.13.1+
- `posframe` 1.4+ — optional, only for `egent-peek`
- `persp-mode` 2.9+ — optional

## Installation

```elisp
(add-to-list 'load-path "/path/to/egent")
(require 'egent)

(global-set-key (kbd "C-c a h") #'egent-sidebar-toggle)
(global-set-key (kbd "C-c a p") #'egent-peek)
(global-set-key (kbd "C-c a r") #'egent-resume)
```

With `use-package` and straight.el:

```elisp
(use-package egent
  :straight (:host github :repo "SreenivasVRao/egent")
  :commands (egent-sidebar-toggle egent-peek egent-resume egent-name-session)
  :custom
  (egent-sidebar-width 50)
  (egent-peek-position 'right)
  :bind (("C-c a h" . egent-sidebar-toggle)
         ("C-c a p" . egent-peek)
         ("C-c a r" . egent-resume)))
```
