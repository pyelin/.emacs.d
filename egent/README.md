# egent

Session management for [agent-shell](https://github.com/xenodium/agent-shell):
one list holding both the sessions open in Emacs and the ones only the agent
still remembers, with resume from either.

This is the successor to `agent-shell-hq`. The sidebar, peek and label surfaces
are the same idea; what's new is that a session is no longer required to have a
buffer to show up.

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
| `egent-label`          | Name the current session using an external CLI            |

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
| `r` / `R` | Label current session / all sessions         |
| `K`       | Kill session and its viewport buffer         |
| `g`       | Refresh                                      |
| `s`       | New shell in the highlighted project         |
| `q`       | Quit, restoring the previous layout          |

Mouse click and double-click work too.

Opening the workspace fetches past sessions once per project
(`egent-sidebar-auto-fetch-sessions`). With `egent-session-agents` left at
`auto`, that only queries agents already in use in that project — one
short-lived subprocess each. Set it to a list of identifiers
(e.g. `'(pi claude-code)`) to query agents regardless.

`persp-mode` is used when installed and skipped otherwise, in which case the
window configuration is saved and restored instead.

### Label

`egent-label` sends the last `egent-label-context-chars` characters of a session
(the tail — every buffer opens with the same welcome banner, which would
otherwise be most of the context) to `egent-label-command` and renames the
buffer to whatever it prints. The rename goes through
`shell-maker-set-buffer-name`, since `agent-shell` resolves a buffer's process by
name and a plain `rename-buffer` would detach it.

```elisp
(setq egent-label-command '("claude" "-p" "--model" "haiku"))  ; default
(setq egent-label-command '("llm"))
(setq egent-label-command '("ollama" "run" "llama3.2"))
```

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
  :commands (egent-sidebar-toggle egent-peek egent-resume egent-label)
  :custom
  (egent-sidebar-width 50)
  (egent-peek-position 'right)
  :bind (("C-c a h" . egent-sidebar-toggle)
         ("C-c a p" . egent-peek)
         ("C-c a r" . egent-resume)))
```
