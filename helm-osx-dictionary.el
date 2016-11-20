;;; helm-osx-dictionary.el --- Helm extension for OSX Dictionary.app -*- lexical-binding: t; -*-

;; Copyright (C) 2016  IKEGAMI Daisuke
;; Package-Requires ((osx-dictionary "0.2.2") (helm "1.7.8"))

;; Author: IKEGAMI Daisuke <ikegami.da@gmail.com>
;; Keywords: mac, dictionary, helm

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

;;; Code:

(require 'ispell)
(require 'osx-dictionary)

(defgroup helm-osx-dictionary nil
  "Helm extension for looking up words in OSX Dictionary.app."
  :group 'helm)

;;;###autoload
(defun helm-osx-dictionary-lookup-word ()
  "Run helm-osx-lookup."
  (interactive)
  (helm
   :sources '(helm-osx-dictionary--get-source-by-aspell)
   :buffer "*Helm Dictionary.app*"
   :prompt "Word: ")
  )

(defvar helm-osx-dictionary--get-source-by-aspell
  (helm-build-sync-source
      "Candidates by aspell."
      :candidates #'helm-osx-dictionary--get-candidates-by-aspell
      :action #'helm-osx-dictionary--search
      :volatile t
      )
  "Build sources by the interactive spell checker `aspell'."
  )

;; the following code snippet (the parser)
;; 'helm-osx-lookup--get-candidates-by-aspell
;; has been developed by helm-words former
;; See 'helm-words--get-candidates
;; https://github.com/pronobis/helm-words/
;;
(defun helm-osx-dictionary--get-candidates-by-aspell ()
  "Return candidates for a word by `aspell'."
  (let* ((command-name
	  (if ispell-program-name
	      ispell-program-name
	    "aspell"))
	 (output-all
	  (with-temp-buffer
	    (insert helm-pattern)
	    (call-process-region (point-min) (point-max)
				 command-name t t nil "-a" "--guess"
				 helm-source-name)
	    (buffer-string)))
	 (output-raw (split-string output-all "\n"))
	 (output-parsed)
	 (candidates ()))
    ;; parse the output
    (pop output-raw)
    (setq output-parsed (ispell-parse-output (pop output-raw)))
    (cond ((stringp output-parsed)
	   (setq candidates (list output-parsed)))
	  ((listp output-parsed)
	   (setq candidates (nth 2 output-parsed)))
	  (t
	   (setq candidates (list helm-pattern))))
    candidates
    )
  )

(defun helm-osx-dictionary--search (word)
  "Look up the given word."
  (with-temp-buffer
    (progn
      (insert word)
      (backward-word)
      (osx-dictionary-search-pointer)
      ))
  t
  )

(provide 'helm-osx-dictionary)

;;; helm-osx-dictionary.el ends here

