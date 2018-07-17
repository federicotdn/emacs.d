# Emacs Configuration
Emacs configuration files and help.

```
C = control
M = meta, alt
S = shift
```

```
a (beggining) <---> e (end)
b (backward)  <---> f (forward)
p (previous)  <---> n (next)
```

## Navigation
```
<left>, <right>, ...: move around by char.
C-<left>, C-<right>, ...: move around by words
M-f, M-b: move forward, backwards by words
C-f, C-b: move forward, backwards by char.
C-n, C-p: next line, previous line
C-e, C-a: end of line, start of line
M-m: first non-whitespace of line

M-}, M-{: next, previous paragraph
M-n, M-p: next, previous paragraph
M-e, M-a: end of sentence, start of sentence
C-M-e, C-M-a: end of defun, start of defun

C-v, M-v: scroll down, up window
C-M-v, C-M-S-v: scroll down, up other window
C-<, C->: scroll right, left window
C-l: scroll to leave point at center, top, bottom
C-M-l: scroll to leave current func. definition at top

C-M-f, C-M-b: forward, backwards S-expression
C-M-d, C-M-u: into, out of S-expression
C-M-k: kill S-expression
C-M-n, C-M-p: move next, previous S-expression

M->, M-<: move to end, beggining of buffer
M-g M-g: go to line
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

## Frames
```
C-x 5 2: create a new frame
C-x 5 0: close selected frame
```

## Point, Mark and Regions
```
C-<spc>: set mark
C-u C-<spc>: pop mark
C-x C-<spc>: pop mark (global)

M-@: mark next word
M-h: mark next paragraph
C-x h: mark the whole buffer
C-M-<spc>: mark S-expression
```

## Symbols
```
M-i: imenu
M-.: go to definition of symbol on point
M-,: pop back to where M-. was last invoked
```

## Bookmarks
```
C-x r m: set bookmark
C-x r l: list bookmarks
C-x r b: jump to bookmark
```

## Search
```
C-s: isearch
C-s C-s: repeat last isearch
C-r: backward incremental search
C-M-s: regexp incremental search
C-M-r: backward regexp incremental search
M-n: next item in search history
M-p: previous item in search history
C-w: add word at point to search string

M-%, C-,: search and replace (interactive)
		  y: replace, n: skip, <ret>: exit, !: replace all

M-s o: Occur mode
q: quit occur mode
```

## Buffers
```
C-x C-s: save buffer
C-x k, C-c k: kill buffer
C-x b, C-<tab>: switch window buffer
C-x C-b: list buffers

C-x <left>, C-x <right>: move to previous, next buffer
```

## Commands
```
M-x: execute command
C-x z: repeat last command
C-x C-c: exit
C-u <arg> <command>: exec <command> with <arg>
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
C-<backspace>: backwards delete word

C-w: kill active region (cut)
M-w: copy active region (copy)
C-y: yank last kill (paste)
M-y: cycle last yank

C-t: transpose
C-o: insert on next line

C-;: comment/uncomment line

C-x <tab>: enter indent region mode (use left-right)

M-<down>, M-<up>: Move line down, up
```

## Help
```
<something> C-h: commands that start with <something>
C-g: cancel command
C-h s: syntax table for current mode
C-h v: describe variable
C-h f: describe function
C-h i: info documentation reader
C-h k <key>: show documentation for key sequence
C-h l: view lossage (key history)
```

## Dired
```
C-x d: dired
<ret>: visit file or directory
^: go up one directory
q: quit dired
+: create directory
n, p, C-n, C-p: move down/up
R: rename file
d: flag file for deletion
x: delete flagged files
v: view selected file, read only (q to quit)
C: copy selected file
g: update dired buffer
```

## Emacs Lisp
```
C-x C-e: eval last S-expression
M-x ielm: interactive elisp CLI
M-:: eval expression
```

## Elpy
```
C-c <: indent region to left
C-c >: indent region to right
M-x pyvenv-activate: activate environment
```

## GNU Bug Tracker
```
n, p: next, previous issue
t: tag issue
n, p: next unread, previous unread message
N, P: next, previous message
```

## Company
```
M-n, M-p: next, previous match
M-x company-complete: initiate completion
```

## Packages
```
M-x package-install-selected-packages: install selected packages
M-x package-delete: delete a package
M-x package-install: install a package
```

## Magit
```
C-x g: magit-status (main window)
c: commit
l: log
k: disacrd
F: pull
P: push
s: stage
u: unstage
g: refresh
h: help
C-c C-c: save commit message
```

## Projectile
```
C-c p f: open file in project
C-c p s g: recursive grep in project
C-c p p: switch to project
C-c p D: project dired
```

## Shell
```
C-c M-o, C-c l: clear screen
C-c C-c: send interrupt
C-d: send EOF
```

## Man Mode
```
M-x man: enter man mode
n, p: next, previous section
```

# To Do
- Org Mode
- Helm
- Better shell
- Add lyrics to spotify.el
