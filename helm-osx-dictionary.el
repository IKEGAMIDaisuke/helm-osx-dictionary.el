;;; helm-osx-dictionary.el --- Helm extension for OSX Dictionary.app -*- lexical-binding: t; -*-

;; Copyright (C) 2016  IKEGAMI Daisuke

;; Author: IKEGAMI Daisuke <ikegami.da@gmail.com>
;; Version: 0.1
;; Package-Requires ((helm "2.3.2") (osx-dictionary "0.3"))

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

(require 'cl)
(require 'ispell)
(require 'helm)
(require 'osx-dictionary)

(defgroup helm-osx-dictionary nil
  "Helm interface for Dictionary.app"
  :group 'helm-osx-dictionary)

(defcustom helm-osx-dictionary-requires 3
  "Minimum length of input for starting completion."
  :type 'integer
  :group 'helm-osx-dictionary)

;;;###autoload
(defun helm-osx-dictionary ()
  "Look a word on Dictionary.app with helm backend."
  (interactive)
  (helm
   :sources '(helm-osx-dictionary--source-by-exact-name
	      helm-osx-dictionary--source-by-ispell)
   :buffer "*OSX Dictionary.app*"
   :prompt "word: "))

(defvar helm-osx-dictionary--source-by-exact-name
  (helm-build-sync-source
      "[?]"
    :candidates #'(lambda () (list (propertize helm-pattern)))
    :action #'osx-dictionary--view-result
    :volatile t)
  "Pattern of the exact matching.")

(defvar helm-osx-dictionary--source-by-ispell
  (helm-build-sync-source
      "Ispell"
    :candidates #'helm-osx-dictionary--get-candidates-by-aspell
    :action #'osx-dictionary--view-result
    :fuzzy-match t
    :volatile t
    :requires-pattern helm-osx-dictionary-requires
    )
  "Source by running ispell."
  )

  (let* ((lines
	  (with-temp-buffer
	    (insert helm-pattern)
	    (ispell-set-spellchecker-params)
	    (ispell-call-process-region
	     (point-min) (point-max)
	     ispell-program-name t t nil "-a" "--guess")
	    (buffer-string)))
	 (output (nth 1 (split-string lines "\n")))
	 (result
	  ;; prevent annoying messages
	  ;; see also the definition of 'ispell-parse-output
	  (when (not (equal 0 (string-match "[\ra-zA-Z]" output)))
	    (ispell-parse-output output))))
    (cond ((stringp result)
    	   (list result))
    	  ((listp result)
	   (let* ((candidates
	    	   (append
	    	    (car result)
	   	    (nth 2 result)
	    	    (nth 3 result)))
		  ;; ignore separated two words.
		  ;; e.g. aspell guesses "aben"
		  ;; as "ab en","ab-en" and "ab+en"
		  ;; pcre: /^w+[-\s\+]\w+$/
		  (words "^[_[:alnum:]]+[- +][_[:alnum:]]+$")
		  (filterp
	   	   (lambda (x)
		     (if (stringp x)
			 (string-match words x)))))
		  (remove-if filterp candidates)))
    	  (t (list helm-pattern)))))

(defun helm-osx-dictionary--search (word)
  "Look up the given word."
  (with-temp-buffer
    (progn
      (insert word)
      (backward-word)
      (osx-dictionary-search-pointer))) t)

(provide 'helm-osx-dictionary)

;;; helm-osx-dictionary.el ends here

