;;; helm-osx-dictionary.el --- Helm extension for OSX Dictionary.app -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  IKEGAMI Daisuke

;; Author: IKEGAMI Daisuke <ikegami.da@gmail.com>
;; Version: 0.1
;; Package-Requires ((emacs "24.4") (helm "2.0") (osx-dictionary "0.3"))

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
  :group 'helm)

(defcustom helm-osx-dictionary-prefix-key
  (kbd "C-c C-l")
  "The prefix key for helm-osx-dictionary."
  :type 'string
  :group 'helm-osx-dictionary
  :package-version '(helm-osx-dictionary . "0.1"))

(defcustom helm-osx-dictionary-requires 3
  "Minimum length of input for starting completion."
  :group 'helm-osx-dictionary
  :type 'integer
  :package-version '(helm-osx-dictionary . "0.1"))

(defvar helm-osx-dictionary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (concat helm-osx-dictionary-prefix-key "l")
      'helm-osx-dictionary-look-up-word)
    map)
  "Minor mode keymap for helm-osx-dictionary mode.")

;;;###autoload
(defun helm-osx-dictionary-look-up-word ()
  "Look a word on Dictionary.app with helm backend."
  (interactive)
  (let ((default (thing-at-point 'word)))
    (helm
     :sources '(helm-osx-dictionary--source-by-exact-name
                helm-osx-dictionary--source-by-ispell)
     :buffer "*OSX Dictionary.app*"
     :prompt "word: "
     :preselect (and default default)
     :input default)))

(defvar helm-osx-dictionary--source-by-exact-name
  (helm-build-sync-source
      "[?]"
    :candidates #'(lambda () (list helm-pattern))
    :action #'osx-dictionary--view-result)
  "Pattern of the exact matching.")

(defvar helm-osx-dictionary--source-by-ispell
  (helm-build-sync-source
      "Ispell"
    :candidates #'helm-osx-dictionary--get-candidates-by-ispell
    :action #'osx-dictionary--view-result
    :fuzzy-match t
    :volatile t
    :requires-pattern helm-osx-dictionary-requires)
  "Source by running ispell.")

(defun helm-osx-dictionary--run-ispell (word)
  "Return spelling errors by running ispell for `word'."
  (let* ((lines
          (with-temp-buffer
            (insert word)
            (ispell-set-spellchecker-params)
            (ispell-accept-buffer-local-defs)
            (ispell-call-process-region
             (point-min) (point-max)
             ispell-program-name t t nil "-a" "--guess" "--suggest")
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
           (append (car result) (nth 2 result) (nth 3 result)))
          (t (list word)))))

(defun helm-osx-dictionary--get-candidates-by-ispell ()
  "Candidates by running ispell."
  (let* (;; pattern of separated words
         ;; pcre: /^w+[\s]\w+$/
         (separated "^[_[:alnum:]]+[ ][_[:alnum:]]+$")
         (filterp
          (lambda (x)
            (if (stringp x) (string-match separated x)))))
    (remove-if filterp (helm-osx-dictionary--run-ispell helm-pattern))))

;;;###autoload
(define-minor-mode helm-osx-dictionary-mode
  "Enable you to look up words in OSX dictionary with helm interface."
  :init-value nil
  :group helm-osx-dictionary
  :keymap helm-osx-dictionary-mode-map)

(provide 'helm-osx-dictionary)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; helm-osx-dictionary.el ends here
