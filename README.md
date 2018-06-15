# Emacs Configuration
Emacs configuration files and help.

```
C = control
M = meta, alt
S = shift
```

## Navigation
```
<left>, <right>, ...: move around by char.
C-<left>, C-<right>, ...: move around by words
M-f, M-b: move forward, backwards by words
C-f, C-b: move forward, backwards by char.
C-n, C-p: next line, previous line
C-e, C-a: end of line, start of line
M-m: first non-wspc of line
M-}, M-{: next, previous paragraph
M-e, M-a: end of sentence, start of sentence
C-M-e, C-M-a: end of defun, start of defun

C-v, M-v: scroll down, up page
C-<, C->: scroll right, left page
C-l: scroll to leave point at center

C-M-f, C-M-b: forward, backwards S-expression
C-M-d, C-M-u: into, out of S-expression
C-M-k: kill S-expression
C-M-n, C-M-p: move next, previous S-expression

M->, M-<: move to end, beggining of buffer
```

## Windows
```
M-o: cycle windows
S-<left>, S-<right>, ...: move between windows
C-x 0: Close this window
C-x 1: Close other windows
C-x 2: Split horizontal
C-x 3: Split vertical

C-c <left>: undo windows layout change
C-c <right>: redo windows layout change
```

## Regions
```
C-<spc>: set mark
C-u C-<spc>: return to last mark

M-@: mark next word
M-h: mark next paragraph
C-x h: mark the whole buffer
C-M-<spc>: mark S-expression
M-g M-g: go to line
```

## Symbols
```
M-.: go to definition of symbol on point
```

## Bookmarks
```
C-x r m: set bookmark
C-x r l: list bookmarks
C-x r b: jump to bookmark
```

## Search
```
C-s: incremental search
C-r: backward incremental search
C-M-s: regexp incremental search
C-M-r: backward regexp incremental search
M-n: next item in search history
M-p: previous item in search history
C-w: add word at point to search string

M-%: search and replace (interactive)
     y: replace, n: skip, <ret>: exit, !: replace all

M-s o: Occur mode
q: quit occur mode

M-i: imenu
```

## Exec
```
C-x C-s: save buffer
C-x k: kill buffer
C-x b, C-<tab>: switch window buffer
C-x C-b: list buffers
C-x C-c: exit
M-x: execute command
C-u <arg> <command>: exec <command> with <arg>

C-x <left>, C-x <right>: move to left, right buffer
C-x d: dired
C-x z: repeat last command
```

## Editing
```
C-/: undo
C-x C-s: save buffer
C-x C-f: visit file

C-d: kill char
M-d: kill word
C-k: kill rest of line
C-S-<backspace>: kill line
C-<backspace>: backwards kill word

C-w: kill active region (cut)
M-w: copy active region (copy)
C-y: yank last kill (paste)
M-y: cycle last yank

C-t: transpose
C-o: insert on next line

C-;: comment/uncomment line

C-c <: indent region to left
C-c >: indent region to right

M-<down>, M-<up>: Move line down, up
```

## Help
```
<something> C-h: commands that start with <something>
C-g: cancel command
C-h s: syntax table for current mode
C-h v: describe variable
```

## Dired
```
<ret>: visit file or directory
^: go up one directory
q: quit dired
n, p, C-n, C-p: move down/up
```

## Minor Modes
```
M-x subword-mode: CamelCase as distinct words
M-x superword-mode: snake_case as distinct words
```

## Emacs Lisp
```
C-x C-e: eval last S-expression
M-x ielm: interactive elisp CLI
M-:: eval expression
```

## Elpy
```
M-x pyvenv-activate: activate environment
```

## Company
```
M-n, M-p: next, previous match
M-x company-complete: initiate completion
```

## Packages
```
M-x package-install-selected-packages: install listed packages in init.el
```

## Magit
```
C-x g: magit-status (main window)
C-c C-c: save commit message
```

## Projectile
```
C-c p f: open file in project
C-c p s g: recursive grep in project
C-c p p: switch to project
```

### Purpose
```
C-, d: dedicate window to purpose
C-, D: dedicate window to buffer
```

## Shell
```
C-c M-o, C-c l: clear screen
```

# To Do
- Org Mode
- Helm
- Better shell
- Indent code
- YAML
