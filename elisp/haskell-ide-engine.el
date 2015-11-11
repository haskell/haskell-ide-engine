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

(defvar haskell-ide-engine-process nil
  "Variable holding current Haskell IDE Engine process")

(defvar haskell-ide-engine-buffer nil
  "Variable holding current Haskell IDE Engine buffer")

(defvar haskell-ide-engine-process-handle-message nil
  "A function to handle json object.")

(defvar haskell-ide-engine-process-handle-invalid-input nil
  "A function to handle invalid input.")

(defun haskell-ide-engine-process-filter (process input)
  (with-current-buffer haskell-ide-engine-buffer

    (insert input)
    (condition-case nil
        (save-excursion
          (goto-char (point-min))
          (let* ((end-of-current-json-object (scan-sexps (point-min) 1))
                 (json-array-type 'list)
                 (json (json-read)))
            (delete-region (point-min) end-of-current-json-object)
            (when haskell-ide-engine-process-handle-message
              (funcall haskell-ide-engine-process-handle-message json))))
      ;; if input is partial then there will not be a closing brace we
      ;; need to wait till it comes
      (scan-error nil)
      ;; json-readtable-error is when there is an unexpected character in input
      (json-readtable-error
       (when haskell-ide-engine-process-handle-invalid-input
         (funcall haskell-ide-engine-process-handle-invalid-input)
         (delete-region (point-min) (point-max))))
      ;; json-unknown-keyword when unrecognized keyword is parsed
      (json-unknown-keyword
       (when haskell-ide-engine-process-handle-invalid-input
         (funcall haskell-ide-engine-process-handle-invalid-input)
         (delete-region (point-min) (point-max)))))))

(defun haskell-ide-engine-start-process ()
  "Start Haskell IDE Engine process.

This function returns the process. If the process is already
running this function does nothing."
  (interactive)

  (unless (haskell-ide-engine-process-live-p)
    (setq haskell-ide-engine-buffer
          (get-buffer-create "*hie*"))
    (setq haskell-ide-engine-process
          (start-process
           "Haskell IDE Engine"
           haskell-ide-engine-buffer
           haskell-ide-engine-command))
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
  (process-send-string haskell-ide-engine-process
                       (haskell-ide-engine-prepare-json json))
  ;; flush buffers
  (process-send-string haskell-ide-engine-process "\n"))

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


(provide 'haskell-ide-engine)
