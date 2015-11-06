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

(defun haskell-ide-engine-process-filter (process input)
  (with-current-buffer haskell-ide-engine-buffer

    (insert input)
    (condition-case nil
        (save-excursion
          (goto-char (point-min))
          (let* ((end-of-current-json-object (scan-sexps (point-min) 1))
                 (json (json-read)))
            (delete-region (point-min) end-of-current-json-object)
            (when haskell-ide-engine-process-handle-message
              (funcall haskell-ide-engine-process-handle-message json)))))))

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
  (process-send-string haskell-ide-engine-process
                       (if (stringp json)
                           json
                         (json-encode json)))
  ;; flush buffers
  (process-send-string haskell-ide-engine-process "\n"))

(provide 'haskell-ide-engine)
