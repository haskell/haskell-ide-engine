;;; hie.el --- Haskell IDE Engine process -*- lexical-binding: t -*-

;; Copyright (c) 2015 Haskell Ide Contributors
;; Package-Requires: ((dash "2.12.1"))

;; See LICENSE for details.

;;; Code:

(require 'json)
(require 'dash)

;;;###autoload
(defcustom hie-command "hie"
  "Name of the command to use for Haskell IDE Engine executable."
  :group 'haskell
  :type 'string)

;;;###autoload
(defcustom hie-command-args ()
  "Arguments to pass to `hie-command`."
  :group 'haskell
  :type '(repeat string))

(defvar hie-process nil
  "Variable holding current Haskell IDE Engine process")

(defvar hie-log-buffer nil
  "Variable holding current Haskell IDE Engine log buffer")

(defvar hie-process-buffer nil
  "Variable holding current Haskell IDE Engine process buffer")

(defvar hie-process-handle-message nil
  "A function to handle json object.")

(defvar hie-process-handle-invalid-input nil
  "A function to handle invalid input.")

(defvar hie-post-message-hook nil
  "Function to call with message that will be send to hie process.")
(defvar hie-plugins nil
  "Plugin information gained by calling the base:plugins plugin")

(defun hie-process-filter (process input)
  (let ((prev-buffer (current-buffer)))
    (with-current-buffer hie-process-buffer
      (let ((point (point)))
        (insert input)
        (save-excursion
          (goto-char point)
          (when (re-search-forward "\^b" nil t)
            (let* ((after-stx-marker (match-end 0))
                   (input-text (buffer-substring-no-properties (point-min) (match-beginning 0)))
                   (handle-error (lambda ()
                                   (when hie-process-handle-invalid-input
                                     (hie-log "<-parse-error %s" input-text)
                                     (funcall hie-process-handle-invalid-input input-text))))
                   (json-array-type 'list))
              (goto-char (point-min))
              (condition-case nil
                  (let ((json (json-read)))
                    (when hie-process-handle-message
                      (with-current-buffer prev-buffer
                        (hie-log "<- %s" input-text)
                        (funcall hie-process-handle-message json))))
                ;; json-readtable-error is when there is an unexpected character in input
                (json-readtable-error (funcall handle-error))
                ;; json-unknown-keyword when unrecognized keyword is parsed
                (json-unknown-keyword (funcall handle-error))
                (end-of-file (funcall handle-error)))
              (delete-region (point-min) after-stx-marker))))))))

(defun hie-start-process ()
  "Start Haskell IDE Engine process.

This function returns the process. If the process is already
running this function does nothing."
  (interactive)

  (unless (hie-process-live-p)
    (setq hie-log-buffer
          (get-buffer-create "*hie-log*"))
    (setq hie-process-buffer
          (get-buffer-create "*hie-process*"))
    (setq hie-process
          (apply #'start-process
           "Haskell IDE Engine"
           hie-process-buffer
           hie-command
           hie-command-args))
    (set-process-query-on-exit-flag hie-process nil)
    (set-process-filter hie-process #'hie-process-filter))
  hie-process)

(defun hie-process-live-p ()
  "Whether the Haskell IDE Engine process is live."
  (and hie-process
       (process-live-p hie-process)))

(defun hie-kill-process ()
  "Kill the Haskell IDE Engine process if it is live."
  (interactive)
  (when (hie-process-live-p)
    (kill-process hie-process)
    (setq hie-process nil)
    (kill-buffer hie-process-buffer)
    (setq hie-process-buffer nil)))

(defun hie-log (&rest args)
  (with-current-buffer hie-log-buffer
    (goto-char (point-max))
    (insert (apply #'format args)
              "\n")))

(defun hie-post-message (json)
  "Post a message to Haskell IDE Engine.

Communication is asynchronous, response (if any) will be received
by `hie-handle-message'."

  ;; We remove values that are empty lists from assoc lists at the top
  ;; level because json serialization would use "null" for those. HIE
  ;; accepts missing fields and default to empty when possible.
  (let ((prepared-json (hie-prepare-json json)))
    (run-hook-with-args 'hie-post-message-hook prepared-json)
    (hie-log "-> %s" prepared-json)
    (process-send-string hie-process prepared-json)
    ;; send \STX marker and flush buffers
    (process-send-string hie-process "\^b\n")))

(defun hie-remove-alist-null-values (json)
  "Remove null values from assoc lists.

Items of the form '(\"key\" . ()) will be removed from assoc list
JSON. Returns the new list."
  (if (listp json)
      (-map (lambda (item)
              (if (consp item)
                  (cons (car item) (hie-remove-alist-null-values (cdr item)))
                item))
            (cl-remove-if (lambda (item) (and (consp item) (null (cdr item)))) json))
    json))

(defun hie-prepare-json (json)
  "Prepare json for sending it to HIE process.

Emacs build in json package and `json-encode' function encodes
empty objects as \"null\". We remove such objects from
association lists and count on HIE to use default values there."
  (if (stringp json)
      json
    (json-encode (hie-remove-alist-null-values json))))

(defvar hie-mode-map
  (easy-mmode-define-keymap
   '()))

(defun hie-handle-message (json)
  (-if-let ((&alist 'type_info type-info) json)
      (hie-handle-type-info type-info)
    (-if-let ((&alist 'refactor refactor) json)
        (message "refactoring"))
    (message (format "%s" json))))

(defun hie-handle-type-info (type-info)
  (-if-let (((&alist 'type type)) type-info)
      (message (format "%s" type))
    (message
     (format "Error extracting type from type-info response: %s"
             type-info))))

(defun hie-format-cmd (cmd)
  (format "%s:%s" (car cmd) (cdr cmd)))

(defun hie-get-context ()
  ;; we need to increment the column by one, since emacs column
  ;; numbers start at 0 while ghc column numbers start at 1
  (let ((start (save-excursion (if (use-region-p) (goto-char (region-beginning)))
                               `(("line" . ,(line-number-at-pos)) ("col" . ,(1+ (current-column))))))
        (end (save-excursion (if (use-region-p) (goto-char (region-end)))
                             `(("line" . ,(line-number-at-pos)) ("col" . ,(1+ (current-column))))))
        (filename (buffer-file-name)))
    `(("file" . (("file" . ,filename)))
      ("start_pos" . ,start)
      ("end_pos" . ,end))))

(defun hie-run-command (plugin command args)
  (setq hie-process-handle-message
        #'hie-handle-message)
  (let ((additional-args
         (-map
          (-lambda ((&alist 'type type 'name name 'val val 'name))
            (cons name (list (cons type val))))
          args))
        (context (hie-get-context)))
    (message (format "args: %s" additional-args))
    (hie-post-message
     `(("cmd" . ,(hie-format-cmd (cons plugin command)))
       ("params" . (,@context ,@ additional-args))))))

(defun hie-handle-first-plugins-command (json)
  "Handle first plugins call."
  (-let* (((&alist 'plugins plugins) json)
          (menu-items
           (-map
            (-lambda ((plugin-name . commands))
              (cons (symbol-name plugin-name)
                    (-map
                     (-lambda ((&alist 'name name 'ui_description description))
                       (vector description
                               (intern (concat "hie-"
                                               (symbol-name plugin-name)
                                               "-"
                                               name))))
                     commands)))
            plugins))
          (command-names plugins))
    (setq hie-plugins plugins)
    (hie-create-all-commands command-names)
    (easy-menu-define hie-menu hie-mode-map
      "Menu for Haskell IDE Engine"
      (cons "HIE" menu-items))))

(defun hie-create-command (plugin command)
  (-let* (((&alist 'name command-name 'additional_params params 'ui_description desc)
           command)
          (required-params
           (-filter (-lambda ((&alist 'required required)) required)
                    params))
          (param-names (-map (-lambda ((&alist 'name name)) (downcase name))
                             required-params))
          (param-args (-map 'intern param-names))
          (param-docstrings
           (-map
            (-lambda ((&alist 'name name 'help desc))
              (format "%s: %s" (upcase name) desc))
            required-params))
          (docstring
           (format "%s\n%s" desc (mapconcat 'identity param-docstrings "\n")))
          (param-vals (-map
                       (-lambda ((&alist 'name name 'type type))
                         `(list (cons 'name ,(downcase name))
                                (cons 'type ,type)
                                (cons 'val ,(intern (downcase name)))))
                       required-params))
          (interactive-strings
           (-map
            (-lambda ((&alist 'name name 'help desc))
              (format "s%s (%s): " name desc))
            required-params)))
    `(defun ,(intern (concat "hie-" (symbol-name plugin) "-" command-name)) ,param-args
       ,docstring
       (interactive ,(mapconcat 'identity interactive-strings "\n"))
       (hie-run-command ,(symbol-name plugin) ,command-name
                        (list ,@param-vals)))))

(defun hie-create-all-commands (command-names)
  (eval `(progn
           ,@(-mapcat
              (-lambda ((plugin-name . commands))
                (-map
                 (-lambda (command)
                   (hie-create-command plugin-name command))
                 commands))
              command-names))))

(define-minor-mode hie-mode
  "Haskell IDE Engine mode.

Keymap:
\\{hie-mode-map}"
  :group 'haskell
  :lighter "HIE"
  :keymap 'hie-mode-map

  (if hie-mode
      (unless (hie-process-live-p)
        (hie-start-process)
        (setq hie-process-handle-message
              #'hie-handle-first-plugins-command)
        (hie-post-message
         '(("cmd" . "base:plugins"))))

    ;; we need to kill hie if this is the last one buffer standing
    (unless (cl-find-if (lambda (buffer)
                          (with-current-buffer buffer
                            (bound-and-true-p hie-mode))) (buffer-list))
      (hie-kill-process))))

(provide 'hie)
