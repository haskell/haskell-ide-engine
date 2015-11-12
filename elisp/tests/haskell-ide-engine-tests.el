;;; haskell-indentation-tests.el --- tests for indentation module -*- lexical-binding: t -*-

;; Copyright (C) 2015 Haskell Ide Engine contributors

;; See LICENSE for details.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'haskell-ide-engine)

(defun really-sleep-for (sec)
  "Emacs has a bug when `sleep-for' terminates early when a subprocess ends.

This is a workaround for http://debbugs.gnu.org/cgi/bugreport.cgi?bug=15990."

  (let ((now (cadr (current-time))))
    (while (> now (- (cadr (current-time)) sec))
      (sleep-for (- now (- (cadr (current-time)) sec))))))


(setq haskell-ide-engine-command-args
      '("-d" "-l" "/tmp/hie.log"))

(defvar haskell-ide-engine-sent-commands ()
  "Recent commands sent to hie, in reverse order.")

(defun haskell-ide-engine-push-command (json)
  (setq haskell-ide-engine-sent-commands (cons json haskell-ide-engine-sent-commands)))

(defvar haskell-ide-engine-tests-collected-invalid-input nil)

(defun haskell-ide-engine-tests-message-invalid-input ()
  (setq haskell-ide-engine-tests-collected-invalid-input
        (mapconcat 'identity
                   (list haskell-ide-engine-tests-collected-invalid-input
                         (buffer-substring-no-properties (point-min) (point-max))) nil)))

(setq haskell-ide-engine-process-handle-invalid-input
      #'haskell-ide-engine-tests-message-invalid-input)

(ert-deftest haskell-ide-engine-can-run-executable ()
  (when nil
    (should (equal 0 (call-process "hie")))))

(defmacro haskell-ide-engine-define-test (name &rest body)
  "Define a HIE test isolated and wrapped in nice reporting capabilities."
  `(ert-deftest ,name ()
    (let* (haskell-ide-engine-test-success
           haskell-ide-engine-sent-commands
           haskell-ide-engine-tests-collected-invalid-input)

      (unwind-protect
          (progn

            (add-hook 'haskell-ide-engine-post-message-hook
                      'haskell-ide-engine-push-command)

            (when (haskell-ide-engine-process-live-p)
              (haskell-ide-engine-kill-process))

            (condition-case nil
                (delete-file "/tmp/hie.log")

              ;; don't fret out if the log file is not there
              (file-error nil))

            (haskell-ide-engine-start-process)

            ,@body

            (setq haskell-ide-engine-test-success t))

        (remove-hook 'haskell-ide-engine-post-message-hook
                     'haskell-ide-engine-push-command)

        (when (haskell-ide-engine-process-live-p)
          (haskell-ide-engine-kill-process))

        (when haskell-ide-engine-tests-collected-invalid-input
          (message "Invalid HIE output:")
          (message "%s\n" haskell-ide-engine-tests-collected-invalid-input))

        (unless haskell-ide-engine-test-success
          (when haskell-ide-engine-sent-commands
            (message "Commands sent to hie during this test:")
            (mapcar (apply-partially #'message "%s") haskell-ide-engine-sent-commands))
          (when (file-exists-p "/tmp/hie.log")
            (message "Contents of /tmp/hie.log:")
            (with-current-buffer (find-file-noselect "/tmp/hie.log" t)
              (message "%s\n" (buffer-substring-no-properties (point-min) (point-max)))
              (kill-buffer (current-buffer)))))))))


(haskell-ide-engine-define-test
 haskell-ide-engine-can-start-stop-process

 ;; first invocation starts the process
 (should (haskell-ide-engine-start-process))
 ;; second invocation does nothing but does not fail either
 (should (haskell-ide-engine-start-process))

 ;; process is alive and well at this point
 (should (haskell-ide-engine-process-live-p))

 ;; lets kill it, twice
 (haskell-ide-engine-kill-process)
 (haskell-ide-engine-kill-process))

(haskell-ide-engine-define-test
 haskell-ide-engine-can-get-version-information

 ;; starts the process
 (should (haskell-ide-engine-start-process))

 (let ((response))
   (setq haskell-ide-engine-process-handle-message
         (lambda (json)
           (setq response json)))
   (haskell-ide-engine-post-message
    '(("cmd" . "base:version") ("params" . ())))

   (really-sleep-for 2)
   (should response)
   (should (equal '(tag . "Ok") (assq 'tag response)))
   (should (assq 'contents response))))

(haskell-ide-engine-define-test
 haskell-ide-engine-can-list-plugins

 ;; starts the process
 (should (haskell-ide-engine-start-process))

 (let ((response))
   (setq haskell-ide-engine-process-handle-message
         (lambda (json)
           (setq response json)))
   (haskell-ide-engine-post-message
    '(("cmd" . "base:plugins")))

   (really-sleep-for 2)
   (should response)

   (should (equal '(tag . "Ok") (assq 'tag response)))
   (should (assq 'contents response))))

(haskell-ide-engine-define-test
 haskell-ide-engine-can-list-commands-for-base

 ;; starts the process
 (should (haskell-ide-engine-start-process))

 (let ((response))
   (setq haskell-ide-engine-process-handle-message
         (lambda (json)
           (setq response json)))
   (haskell-ide-engine-post-message
    '(("cmd" . "base:commands") ("params" . (("plugin" . (("contents" . "base") ("tag" . "text")))))))

   (really-sleep-for 2)
   (should response)

   (should (equal '(tag . "Ok") (assq 'tag response)))
   (should (assq 'contents response))
   (should (assq 'responses (assq 'contents response)))
   (should (member "version" (assq 'responses (assq 'contents response))))))

(haskell-ide-engine-define-test
 haskell-ide-engine-can-list-command-details-for-base-plugins

 ;; starts the process
 (should (haskell-ide-engine-start-process))

 (let ((response))
   (setq haskell-ide-engine-process-handle-message
         (lambda (json)
           (setq response json)))
   (haskell-ide-engine-post-message
    '(("cmd" . "base:commandDetail") ("params" . (("plugin" . (("tag" . "text") ("contents"  . "base"))) ("command" . (("tag" . "text") ("contents" . "plugins")))))))

   (really-sleep-for 2)

   (should response)

   (should (equal '(tag . "Ok") (assq 'tag response)))
   (should (assq 'contents response))
   (should (equal '(name . "plugins") (assq 'name (assq 'contents response))))))

(ert-deftest haskell-ide-engine-can-handle-invalid-input ()

  (let* (response
         haskell-ide-engine-tests-collected-invalid-input
         (haskell-ide-engine-process-handle-invalid-input
          (lambda ()
            (setq response (buffer-substring-no-properties (point-min) (point-max)))))
         (haskell-ide-engine-buffer
          (get-buffer-create "*hie*")))

    (unwind-protect
        (progn
          (haskell-ide-engine-process-filter nil "not a json text")

          (should (equal "not a json text" response)))
      (kill-buffer haskell-ide-engine-buffer))))

(haskell-ide-engine-define-test
 haskell-ide-engine-can-handle-input-in-chunks

 ;; Emacs fetches input from processes in chunks, 400 bytes is one
 ;; number from documentation. Anyway we need to be ready to handle
 ;; input in chunks.
 (let* ((response)
        (haskell-ide-engine-process-handle-message
         (lambda (json)
           (setq response json)))

        (haskell-ide-engine-process-handle-invalid-input
         (lambda ()
           (setq response (buffer-substring-no-properties (point-min) (point-max))))))
   (haskell-ide-engine-process-filter nil "{")
   (haskell-ide-engine-process-filter nil "\"key\":")
   (haskell-ide-engine-process-filter nil "\"val\"}")

   (should (equal '((key . "val")) response))))






;;; haskell-ide-engine-tests.el ends here
