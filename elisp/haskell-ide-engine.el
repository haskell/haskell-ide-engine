;;; haskell-ide-engine.el --- Haskell IDE Engine process -*- lexical-binding: t -*-

;; Copyright (c) 2015 Haskell Ide Contributors

;; See LICENSE for details.

;;; Code:

(require 'json)

;;;###autoload
(defcustom haskell-ide-engine-command "hie"
  "Name of the command to use for Haskell IDE Engine executable."
  :group 'haskell
  :type 'string)

;;;###autoload
(defcustom haskell-ide-engine-command-args ()
  "Arguments to pass to `haskell-ide-engine-command`."
  :group 'haskell
  :type '(repeat string))

(defvar haskell-ide-engine-process nil
  "Variable holding current Haskell IDE Engine process")

(defvar haskell-ide-engine-buffer nil
  "Variable holding current Haskell IDE Engine buffer")

(defvar haskell-ide-engine-process-handle-message nil
  "A function to handle json object.")

(defvar haskell-ide-engine-process-handle-invalid-input nil
  "A function to handle invalid input.")

(defvar haskell-ide-engine-post-message-hook nil
  "Function to call with message that will be send to hie process.")
(defvar haskell-ide-engine-plugins nil
  "Plugin information gained by calling the base:plugins plugin")

(defun haskell-ide-engine-process-filter (process input)
  (let ((prev-buffer (current-buffer)))
    (with-current-buffer haskell-ide-engine-buffer

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
                    (when haskell-ide-engine-process-handle-message
                      (with-current-buffer prev-buffer
                        (funcall haskell-ide-engine-process-handle-message json))))
                ;; json-readtable-error is when there is an unexpected character in input
                (json-readtable-error
                 (when haskell-ide-engine-process-handle-invalid-input
                   (funcall haskell-ide-engine-process-handle-invalid-input
                            (buffer-substring-no-properties (point-min) end-of-current-json-object))))
                ;; json-unknown-keyword when unrecognized keyword is parsed
                (json-unknown-keyword
                 (when haskell-ide-engine-process-handle-invalid-input
                   (funcall haskell-ide-engine-process-handle-invalid-input
                            (buffer-substring-no-properties (point-min) end-of-current-json-object))))
                (end-of-file
                 (when haskell-ide-engine-process-handle-invalid-input
                   (funcall haskell-ide-engine-process-handle-invalid-input
                            (buffer-substring-no-properties (point-min) end-of-current-json-object)))))
              (delete-region (point-min) after-stx-marker))))))))

(defun haskell-ide-engine-start-process ()
  "Start Haskell IDE Engine process.

This function returns the process. If the process is already
running this function does nothing."
  (interactive)

  (unless (haskell-ide-engine-process-live-p)
    (setq haskell-ide-engine-buffer
          (get-buffer-create "*hie*"))
    (setq haskell-ide-engine-process
          (apply #'start-process
           "Haskell IDE Engine"
           haskell-ide-engine-buffer
           haskell-ide-engine-command
           haskell-ide-engine-command-args))
    (set-process-query-on-exit-flag haskell-ide-engine-process nil)
    (set-process-filter haskell-ide-engine-process #'haskell-ide-engine-process-filter))
  haskell-ide-engine-process)

(defun haskell-ide-engine-process-live-p ()
  "Whether the Haskell IDE Engine process is live."
  (and haskell-ide-engine-process
       (process-live-p haskell-ide-engine-process)))

(defun haskell-ide-engine-kill-process ()
  "Kill the Haskell IDE Engine process if it is live."
  (interactive)
  (when (haskell-ide-engine-process-live-p)
    (kill-process haskell-ide-engine-process)
    (setq haskell-ide-engine-process nil)
    (kill-buffer haskell-ide-engine-buffer)
    (setq haskell-ide-engine-buffer nil)))

(defun haskell-ide-engine-post-message (json)
  "Post a message to Haskell IDE Engine.

Communication is asynchronous, response (if any) will be received
by `haskell-ide-engine-handle-message'."

  ;; We remove values that are empty lists from assoc lists at the top
  ;; level because json serialization would use "null" for those. HIE
  ;; accepts missing fields and default to empty when possible.
  (let ((prepared-json (haskell-ide-engine-prepare-json json)))
    (run-hook-with-args 'haskell-ide-engine-post-message-hook prepared-json)

    (process-send-string haskell-ide-engine-process prepared-json)
    ;; send \STX marker and flush buffers
    (process-send-string haskell-ide-engine-process "\^b\n")))

(defun haskell-ide-engine-remove-alist-null-values (json)
  "Remove null values from assoc lists.

Items of the form '(\"key\" . ()) will be removed from assoc list
JSON. Returns the new list."
  (if (listp json)
      (mapcar (lambda (item)
                (if (consp item)
                    (cons (car item) (haskell-ide-engine-remove-alist-null-values (cdr item)))
                  item))
              (cl-remove-if (lambda (item) (and (consp item) (null (cdr item)))) json))
    json))

(defun haskell-ide-engine-prepare-json (json)
  "Prepare json for sending it to HIE process.

Emacs build in json package and `json-encode' function encodes
empty objects as \"null\". We remove such objects from
association lists and count on HIE to use default values there."
  (if (stringp json)
      json
    (json-encode (haskell-ide-engine-remove-alist-null-values json))))

(defvar hie-mode-map
  (easy-mmode-define-keymap
   '()))

(defun hie-handle-message (json)
  (message (format "%s" json)))

(defun hie-handle-command-detail (json)
  (let ((context
         (hie-get-context (cdr (assq 'contexts json)))))
    (setq haskell-ide-engine-process-handle-message
          #'hie-handle-message)
    (haskell-ide-engine-post-message
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
  (setq haskell-ide-engine-process-handle-message
        #'hie-handle-command-detail)
  (setq haskell-ide-engine-current-cmd (cons plugin command))
  (haskell-ide-engine-post-message
   `(("cmd" . "base:commandDetail")
     ("params" . (("command" . (("text" . ,command)))
                  ("plugin" . (("text" . ,plugin))))))))

(defun haskell-ide-engine-handle-first-plugins-command (json)
  "Handle first plugins call."

  (let ((menu-items
         (mapcar
          (lambda (plugin)
            (cons (symbol-name (car plugin))
                  (mapcar
                   (lambda (command)
                     (vector (cdr (assq 'ui_description command)) (list 'hie-run-command (symbol-name (car plugin)) (cdr (assq 'name command)))))
                   (cdr plugin))))
          (cdr (assq 'plugins json)))))
    (setq haskell-ide-engine-plugins (cdr (assq 'plugins json)))
    (easy-menu-define hie-menu hie-mode-map
      "Menu for Haskell IDE Engine"
      (cons "HIE" menu-items))))


(define-minor-mode hie-mode
  "Haskell IDE Engine mode.

Keymap:
\\{hie-mode-map}"
  :group 'haskell
  :lighter "HIE"
  :keymap 'hie-mode-map

  (if hie-mode
      (unless (haskell-ide-engine-process-live-p)
        (haskell-ide-engine-start-process)
        (setq haskell-ide-engine-process-handle-message
              #'haskell-ide-engine-handle-first-plugins-command)
        (haskell-ide-engine-post-message
         '(("cmd" . "base:plugins"))))

    ;; we need to kill hie if this is the last one buffer standing
    (unless (cl-find-if (lambda (buffer)
                          (with-current-buffer buffer
                            (bound-and-true-p hie-mode))) (buffer-list))
      (haskell-ide-engine-kill-process))))

(provide 'haskell-ide-engine)
