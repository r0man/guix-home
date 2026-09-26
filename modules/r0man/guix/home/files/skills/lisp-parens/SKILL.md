---
name: lisp-parens
description: Keep parentheses balanced when editing Lisp code (Scheme/Guile, Emacs Lisp, Common Lisp, Clojure, Racket, Janet). Use whenever you edit Lisp files, or when a tool result reports unbalanced delimiters.
---

# Balanced parens in Lisp code

Unbalanced delimiters are the most common way edits to Lisp files break.
A missing `)` can silently swallow the rest of the file into one form, and
the error may show up far from the edit.

## While editing

- Edit whole forms. Replace a complete `(define ...)` / `(defun ...)` /
  `(defn ...)` rather than patching the middle of a closing-paren run
  like `))))`.
- Keep the `old_string` of an edit anchored on complete lines, and make
  sure the replacement closes exactly what the original closed.
- Count before you write: every `(`, `[`, `{` you add needs its closer in
  the same edit. Watch mixed delimiters in Clojure (`[` vs `(`).
- Parens inside strings, comments, and character literals (`#\(` in
  Scheme/CL, `?\(` in Emacs Lisp, `\(` in Clojure) do not count.

## After editing

Run the checker on every Lisp file you changed:

```bash
lisp-paren-check path/to/file.scm
```

- Exit 0: balanced. Exit 2: unbalanced; it prints the first problem as
  `file:line:col: error: ...`.
- Hooks in Claude Code and pi run this automatically after each edit or
  write and report failures in the tool result. Treat such a report as
  an error you must fix before continuing.

## Repairing

The report includes a suggestion from `parenmedic`, which guesses the fix
from indentation. It is often right and sometimes wrong.

1. Read the diff (`parenmedic fix --diff FILE`) and check it against
   what the code is meant to do.
2. If it is right, apply it: `parenmedic fix -i FILE`.
3. If not, fix by hand at the position the checker reported, then run
   `lisp-paren-check` again.

parenmedic gotchas:

- Options take `=`: `--dialect=clojure`, not `--dialect clojure`.
- `.edn`, `.bb`, `.asd`, `.sld` are not auto-detected; pass `--dialect=`
  (`cl`, `scheme`, `clojure`, `elisp`, `janet`, `racket`).
- `parenmedic diagnose` exits 0 even when it finds problems, and it flags
  balanced code that is oddly indented. Use `lisp-paren-check` to decide
  whether a file is broken, and parenmedic only to suggest a fix.
