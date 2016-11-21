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
   :sources '(
	      helm-osx-dictionary--source-exact-name
	      helm-osx-dictionary--sources-by-aspell
	      )
   :buffer "*Helm Dictionary.app*"
   :prompt "Word: ")
  )

(defvar helm-osx-dictionary--source-exact-name
  (helm-build-sync-source
      "[?]"
    :candidates #'(lambda () (list (propertize helm-pattern))) ;; #'foo
    :action #'helm-osx-dictionary--search
    :volatile t
    )
  "The source of exact matching pattern."
  )

(defvar helm-osx-dictionary--sources-by-aspell
  (helm-build-sync-source
      "Candidates by aspell."
      :candidates #'helm-osx-dictionary--get-candidates-by-aspell
      :action #'helm-osx-dictionary--search
      :fuzzy-match t
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
  (let* ((output-all
	  (with-temp-buffer
	    (insert helm-pattern)
	    (ispell-call-process-region
	     (point-min) (point-max)
	     ispell-program-name t t nil "-a" "--guess")
	    (buffer-string)))
	 (output-raw (split-string output-all "\n"))
	 (output-parsed
	  (ispell-parse-output (nth 1 output-raw)))
	 )
    ;; parse the output
    (cond ((stringp output-parsed)
    	   (list output-parsed))
    	  ((listp output-parsed)
	   (append
	    (car output-parsed)
	    (nth 2 output-parsed)
	    (nth 3 output-parsed)))
    	  (t
    	   (list helm-pattern)))
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

