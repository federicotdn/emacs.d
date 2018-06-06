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

M-o: cycle windows
S-<left>, S-<right>, ...: move between windows
C-x 0: Close this window
C-x 1: Close other windows
C-x 2: Split horizontal
C-x 3: Split vertical
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

M-s o: Occur mode
q: quit occur mode

M-i: imenu

```

## Exec
```
C-x C-s: save buffer
C-x k: kill buffer
C-x b: switch window buffer
C-x C-b: list buffers
C-x C-c: exit
M-x: execute command
C-u <arg> <command>: exec <command> with <arg>

C-x d: dired
```

## Editing
```
C-/: undo
C-x C-s: save buffer
C-x C-f: open file

C-d: kill char
M-d: kill word
C-k: kill rest of line
C-S-<backspace>: kill line

C-w: kill active region
M-w: copy active region
C-y: yank last kill

C-t: transpose

C-x C-;: comment/uncomment line
```

## Help
```
<something> C-h: commands that start with <something>
C-g: cancel command
C-h s: syntax table for current mode
C-h v: describe variable
```

## Minor Modes
```
M-x subword-mode: CamelCase as distinct words
M-x superword-mode: snake_case as distinct words
```

## Elpy
```
M-.: goto definition on point
M-x pyvenv-activate: activate environment
```

## Company
```
M-n, M-p: next, previous match
M-x company-complete: initiate completion
```
