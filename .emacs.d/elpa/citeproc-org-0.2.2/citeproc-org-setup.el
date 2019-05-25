;;; citeproc-org-setup.el --- Set up citeproc-org -*- lexical-binding: t; -*-

;; Copyright (C) 2017 András Simonyi

;; Author: András Simonyi <andras.simonyi@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Function to set up citeproc-org. It is in a separate file to avoid loading
;; the whole program and citeproc-el in the setup phase.

;;; Code:

;;;###autoload
(defun citeproc-org-setup ()
  "Add citeproc-org rendering to the `org-export-before-parsing-hook' hook."
  (interactive)
  (add-hook 'org-export-before-parsing-hook 'citeproc-org-render-references))

;;; citeproc-org-setup ends here 
