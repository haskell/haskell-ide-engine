;;; read-char-spec.el --- Generalized `y-or-n-p'.

;; Copyright (C) 2009  Edward O'Connor

;; Author: Edward O'Connor <hober0@gmail.com>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; Provides a generalization of the `y-or-n-p' UI for when you have
;; other possible answers. `read-char-spec' is to `read-char' as
;; `format-spec' is to `format'. Here's how you would re-implement
;; `y-or-n-p' with `read-char-spec':
;;
;; (defun example-y-or-n-p (prompt)
;;   "Copy of `y-or-n-p', as an example use of `read-char-spec'.
;; PROMPT is as for `y-or-n-p'."
;;   (read-char-spec prompt '((?y t "Answer in the affirmative")
;;                            (?n nil "Answer in the negative"))))
;;
;; Compared to using `interactive's "c" spec, I think the programmatic
;; interface for `read-char-spec' is simpler, it keeps prompting until
;; you really type one of the characters in the spec, and it provides
;; for interactive help for the user (by typing "?").

;;; History:
;; 20009-06-17: Initial version, inspired by a conversation with ngirard
;;              in #emacs.

;;; Code:

(defun read-char-spec (prompt specification
                              &optional inherit-input-method seconds)
  "Ask the user a question with multiple possible answers.
No confirmation of the answer is requested; a single character is
enough.

PROMPT is the string to display to ask the question. It should end in a
space; `read-char-spec' adds help text to the end of it.

SPECIFICATION is a list of key specs, each of the form (KEY VALUE
HELP-TEXT).

Arguments INHERIT-INPUT-METHOD and SECONDS are as in `read-char', which
see."
  (let* ((spec-with-help
          (append (list (list ?? read-char-spec-help-cmd
                              "Get help"))
                  specification))
         (keys (mapconcat (lambda (cell)
                            (read-char-spec-format-key (car cell)))
                          specification
                          ", "))
         (prompt-with-keys (format "%s (%s, or ? for help) "
                                   prompt keys))
         char-read
         (current read-char-spec-not-found))
    ;; Loop until the user types a char actually in `specification'
    (while (eq current read-char-spec-not-found)
      (if (fboundp 'next-command-event) ; XEmacs
          (setq event (next-command-event nil prompt-with-keys)
		char-read (and (fboundp 'event-to-character)
			       (event-to-character event)))
        (setq char-read  (string-to-char (read-string prompt-with-keys))))
	(let ((entry (assoc char-read spec-with-help)))
        (when entry
          (setq current (cadr entry))))

      ;; Provide help when requested
      (when (eq current read-char-spec-help-cmd)
        (read-char-spec-generate-help prompt specification)
        (setq current read-char-spec-not-found))

      (setq prompt-with-keys
            (format "Please answer %s. %s (%s, or ? for help) "
                    keys prompt keys)))

    current))


(defun read-char-spec-1 (prompt specification
                              &optional inherit-input-method seconds)
  "Ask the user a question with multiple possible answers.
No confirmation of the answer is requested; a single character is
enough.

PROMPT is the string to display to ask the question. It should end in a
space; `read-char-spec' adds help text to the end of it.

SPECIFICATION is a list of key specs, each of the form (KEY VALUE
HELP-TEXT).

Arguments INHERIT-INPUT-METHOD and SECONDS are as in `read-char', which
see."
  (let* ((spec-with-help
          (append (list (list ?? read-char-spec-help-cmd
                              "Get help"))
                  specification))
         (keys (mapconcat (lambda (cell)
                            (read-char-spec-format-key (car cell)))
                          specification
                          ", "))
         (prompt-with-keys (format "%s (%s, or ? for help) "
                                   prompt keys))
         char-read
         (current read-char-spec-not-found))
    ;; Loop until the user types a char actually in `specification'
    (while (eq current read-char-spec-not-found)
      (if (fboundp 'next-command-event) ; XEmacs
          (setq event (next-command-event nil prompt-with-keys)
		char-read (and (fboundp 'event-to-character)
			       (event-to-character event)))
        (setq char-read  (read-event prompt-with-keys)))
	(let ((entry (assoc char-read spec-with-help)))
        (when entry
          (setq current (cadr entry))))

      ;; Provide help when requested
      (when (eq current read-char-spec-help-cmd)
        (read-char-spec-generate-help prompt specification)
        (setq current read-char-spec-not-found))

      (setq prompt-with-keys
            (format "Please answer %s. %s (%s, or ? for help) "
                    keys prompt keys)))

    current))

;;; There be dragons here

(defconst read-char-spec-not-found
  (make-symbol "read-char-spec-not-found")
  "Dummy value for when user types character not in the spec provided.")
(defconst read-char-spec-help-cmd
  (make-symbol "read-char-spec-help-cmd")
  "Dummy value for when user types `?' to produce help.")

(autoload 'edmacro-format-keys "edmacro")
(autoload 'comment-string-strip "newcomment")

(defun read-char-spec-format-key (key)
  "Format KEY like input for the `kbd' macro."
  (edmacro-format-keys (vector key)))

(defun read-char-spec-generate-help (prompt specification)
  "Generate help text for PROMPT, based on SPECIFICATION."
  (with-output-to-temp-buffer (help-buffer)
    (help-setup-xref (list #'read-char-spec) nil)
    (princ (format "Help for \"%s\":\n\n"
                   (comment-string-strip prompt t t)))
    (princ (mapconcat (lambda (cell)
                        (format "%s - %s"
                                (read-char-spec-format-key
                                 (car cell))
                                (caddr cell)))
                      specification "\n"))
    (print-help-return-message)))

(provide 'read-char-spec)
;;; read-char-spec.el ends here
