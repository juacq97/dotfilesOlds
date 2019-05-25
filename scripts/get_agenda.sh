#!/bin/bash

emacs -batch -l ~/.emacs.d/init.el --eval "(run-hooks 'emacs-startup-hook)" --eval '(org-batch-agenda "n"
	org-agenda-span (quote day))' | sed "/Day-agenda.*/d"
