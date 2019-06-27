;;; init-templates.el --- Code templates for tempo.el  -*- lexical-binding: t; -*-

(tempo-define-template "python-pdb"
		       '("import pdb; pdb.set_trace()")
		       "pdb")

(tempo-define-template "python-code-interact"
		       '("import code; code.interact(local=locals())")
		       "interact")

(tempo-define-template "python-property"
		       '("@property" n>
			 "def " (P "Property: " prop) "(self):" n>
			 "return self._" (s prop))
		       "property")

(tempo-define-template "python-traceback"
		       '("import traceback; traceback.print_stack()")
		       "traceback")
