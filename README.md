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
C-x <, C-x >: scroll left, right page
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

## Exec
```
C-x C-s: save buffer
C-x k: kill buffer
C-x b: switch window buffer
C-x C-b: list buffers
C-x C-c: exit
M-x: execute command
C-u <arg> <command>: exec <command> with <arg>
```

## Editing
```
C-/: undo
C-x C-s: save buffer
C-x C-f: open file
C-d: kill char
M-d: kill word
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
