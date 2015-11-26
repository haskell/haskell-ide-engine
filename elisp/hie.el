;;; hie.el --- Haskell IDE Engine process -*- lexical-binding: t -*-

;; Copyright (c) 2015 Haskell Ide Contributors

;; See LICENSE for details.

;;; Code:

(require 'json)

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

(defvar hie-buffer nil
  "Variable holding current Haskell IDE Engine buffer")

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
    (with-current-buffer hie-buffer

      (let ((point (point)))
        (insert input)
        (save-excursion
          (goto-char point)
          (when (re-search-forward "\^b" nil t)
            (let* ((end-of-current-json-object (match-beginning 0))
                   (after-stx-marker (match-end 0))
                   (json-array-type 'list))
              (goto-char (point-min))
              (condition-case nil
                  (let ((json (json-read)))
                    (when hie-process-handle-message
                      (with-current-buffer prev-buffer
                        (funcall hie-process-handle-message json))))
                ;; json-readtable-error is when there is an unexpected character in input
                (json-readtable-error
                 (when hie-process-handle-invalid-input
                   (funcall hie-process-handle-invalid-input
                            (buffer-substring-no-properties (point-min) end-of-current-json-object))))
                ;; json-unknown-keyword when unrecognized keyword is parsed
                (json-unknown-keyword
                 (when hie-process-handle-invalid-input
                   (funcall hie-process-handle-invalid-input
                            (buffer-substring-no-properties (point-min) end-of-current-json-object))))
                (end-of-file
                 (when hie-process-handle-invalid-input
                   (funcall hie-process-handle-invalid-input
                            (buffer-substring-no-properties (point-min) end-of-current-json-object)))))
              (delete-region (point-min) after-stx-marker))))))))

(defun hie-start-process ()
  "Start Haskell IDE Engine process.

This function returns the process. If the process is already
running this function does nothing."
  (interactive)

  (unless (hie-process-live-p)
    (setq hie-buffer
          (get-buffer-create "*hie*"))
    (setq hie-process
          (apply #'start-process
           "Haskell IDE Engine"
           hie-buffer
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
    (kill-buffer hie-buffer)
    (setq hie-buffer nil)))

(defun hie-post-message (json)
  "Post a message to Haskell IDE Engine.

Communication is asynchronous, response (if any) will be received
by `hie-handle-message'."

  ;; We remove values that are empty lists from assoc lists at the top
  ;; level because json serialization would use "null" for those. HIE
  ;; accepts missing fields and default to empty when possible.
  (let ((prepared-json (hie-prepare-json json)))
    (run-hook-with-args 'hie-post-message-hook prepared-json)

    (process-send-string hie-process prepared-json)
    ;; send \STX marker and flush buffers
    (process-send-string hie-process "\^b\n")))

(defun hie-remove-alist-null-values (json)
  "Remove null values from assoc lists.

Items of the form '(\"key\" . ()) will be removed from assoc list
JSON. Returns the new list."
  (if (listp json)
      (mapcar (lambda (item)
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
  (message (format "%s" json)))

(defun hie-handle-command-detail (json)
  (let ((context
         (hie-get-context (cdr (assq 'contexts json)))))
    (setq hie-process-handle-message
          #'hie-handle-message)
    (hie-post-message
     `(("cmd" . ,(hie-format-cmd (cons (cdr (assq 'plugin_name json)) (cdr (assq 'name json)))))
       ("params" . ,context)))))

(defun hie-format-cmd (cmd)
  (format "%s:%s" (car cmd) (cdr cmd)))

(defun hie-get-context (context)
  (let ((start (save-excursion (if (use-region-p) (goto-char (region-beginning)))
                               `(("line" . ,(line-number-at-pos)) ("col" . ,(current-column)))))
        (end (save-excursion (if (use-region-p) (goto-char (region-end)))
                             `(("line" . ,(line-number-at-pos)) ("col" . ,(current-column)))))
        (filename (buffer-file-name)))
    `(("file" . (("file" . ,filename)))
      ("start_pos" . ,end)
      ("end_pos" . ,start))))

(defun hie-run-command (plugin command)
  (setq hie-process-handle-message
        #'hie-handle-command-detail)
  (setq hie-current-cmd (cons plugin command))
  (hie-post-message
   `(("cmd" . "base:commandDetail")
     ("params" . (("command" . (("text" . ,command)))
                  ("plugin" . (("text" . ,plugin))))))))

(defun hie-handle-first-plugins-command (json)
  "Handle first plugins call."
  (let ((menu-items
         (mapcar
          (lambda (plugin)
            (cons (symbol-name (car plugin))
                  (mapcar
                   (lambda (command)
                     (vector (cdr (assq 'ui_description command))
                             (intern (concat "hie-"
                                             (symbol-name (car plugin))
                                             (cdr (assq 'name command))))))
                   (cdr plugin))))
          (cdr (assq 'plugins json))))
        (command-names
         (mapcar
          (lambda (plugin)
            (cons (car plugin)
                  (mapcar
                   (lambda (command)
                     (cdr (assq 'name command)))
                   (cdr plugin))))
          (cdr (assq 'plugins json)))))
    (setq hie-plugins (cdr (assq 'plugins json)))
    (hie-create-all-commands command-names)
    (easy-menu-define hie-menu hie-mode-map
      "Menu for Haskell IDE Engine"
      (cons "HIE" menu-items))))

(defun hie-create-command (plugin command)
  `(defun ,(intern (concat "hie-" (symbol-name plugin) command)) ()
     (interactive)
     (hie-run-command ,(symbol-name plugin) ,command)))

(defun hie-create-all-commands (command-names)
  (eval `(progn
           ,@(apply 'append (mapcar
                             (lambda (plugin)
                               (mapcar
                                (lambda (command)
                                  (hie-create-command (car plugin) command))
                                (cdr plugin)))
                             command-names)))))

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
