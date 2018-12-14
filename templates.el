;;; templates.el --- Code templates created with tempo  -*- lexical-binding: t; -*-

(tempo-define-template "python-pdb"
		       '("import pdb; pdb.set_trace()")
		       "pdb")

(tempo-define-template "python-code-interact"
		       '("import code; code.interact(local=locals())")
		       "interact")
