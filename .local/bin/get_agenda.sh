#!/bin/bash

emacs -batch -l ~/.emacs.d/init.el --eval "(run-hooks 'emacs-startup-hook)" --eval '(org-batch-agenda "o" org-agenda-span 3)' | sed "/Day-agenda.*/d"

