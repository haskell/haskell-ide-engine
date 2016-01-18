;;; hie-test.el  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Haskell Ide Engine contributors

;; See LICENSE for details.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'hie)

(buttercup-define-matcher :to-have-key (obj key)
   (if (assq key obj)
       (cons t (format "Expected %S to have a nil value for key %S" obj key))
     (cons nil (format "Expected %S to have a non nil value for key %S" obj key))))

(defmacro async-with-timeout (timeout &rest body)
  `(progn
    (setq hie-async-returned nil)
    ,@body
    (with-timeout (,timeout) (while (not hie-async-returned) (sleep-for 0.1)))))

(describe "haskell-ide-engine"
          (describe "process management"
                    (before-each
                     (when (hie-process-live-p)
                       (hie-kill-process)))
                    (it "should start process"
                        (let ((result (hie-start-process))
                              (live (hie-process-live-p)))
                          (expect result)
                          (expect live :to-be-truthy)))
                    (it "should not start a new process if there is already one"
                        (let ((result (hie-start-process))
                              (second-result (hie-start-process)))
                          (expect result)
                          (expect second-result :to-equal result)))
                    (it "should kill the process"
                        (hie-start-process)
                        (let ((live (hie-process-live-p)))
                          (expect live :to-be-truthy)
                          (hie-kill-process)
                          (let ((live (hie-process-live-p)))
                            (expect live :not :to-be-truthy)))))
          (describe "command responses"
                    (before-all (setq hie-command-args
                                      '("-d" "-l" "/tmp/hie.log"))
                                (when (hie-process-live-p)
                                  (hie-kill-process))
                                (hie-start-process)
                                (condition-case nil
                                    (delete-file "/tmp/hie.log")
                                  (file-error nil))
                                (setq hie-process-handle-message
                                      (lambda (json)
                                        (setq response json)
                                        (setq hie-async-returned t)))
                                (advice-add 'hie-handle-message :after
                                            (lambda (&rest r)
                                              (progn
                                                (setq hie-async-returned t))))
                                (advice-add 'hie-handle-first-plugins-command :after
                                            (lambda (&rest r)
                                              (progn
                                                (setq hie-async-returned t)))))
                    (before-each (setq response nil))
                    (after-each (with-current-buffer hie-process-buffer
                                  (erase-buffer)))
                    (after-all (hie-kill-process))
                    (it "can get version info"
                        (async-with-timeout 100
                                            (hie-post-message
                                             '(("cmd" . "base:version") ("params" . ()))))
                        (expect response)
                        (expect response :to-have-key 'ok))
                    (it "can list plugins"
                        (async-with-timeout 100
                                            (hie-post-message '(("cmd" . "base:plugins"))))
                        (expect response :to-have-key 'plugins)
                        (expect (assq 'plugins response) :to-have-key 'base)
                        (expect (cl-find-if
                                 (lambda (item)
                                   (equal '(name . "version") (assq 'name item)))
                                 (cdr (assq 'base (assq 'plugins response))))))
                    (it "can execute HaRe rename"
                        (save-excursion
                          (hie-kill-process)
                          (find-file "../test/testdata/HaReRename.hs")
                          (move-to-column 0)
                          (goto-line 4)
                          (async-with-timeout 100 (hie-mode))
                          (async-with-timeout 100
                                              (hie-hare-rename "foo_renamed"))
                          (let ((refactored-string (buffer-substring-no-properties (point-min) (point-max))))
                            (revert-buffer nil t)
                            (expect refactored-string :to-equal "\nmain = putStrLn \"hello\"\n\nfoo_renamed :: Int -> Int\nfoo_renamed x = x + 3\n\n")))))
          (describe "process input"
                    (before-each
                     (setq hie-process-handle-invalid-input
                           (lambda (input) (setq error-response input)))
                     (setq hie-process-handle-message
                           (lambda (input) (setq response input))))
                    (after-each
                     (kill-buffer hie-process-buffer))
                    (it "can handle invalid input"
                        (hie-process-filter nil "not a json text\^b")
                        (expect error-response :to-equal "not a json text"))
                    (it "can handle input in chunks"
                        (hie-process-filter nil "{")
                        (hie-process-filter nil "\"key\":")
                        (hie-process-filter nil "\"val\"}")
                        (hie-process-filter nil "\^b")
                        (expect response :to-equal '((key . "val")))))
          (describe "create-commands"
                    (it "can create a command with no params"
                        (let ((test-command (hie-create-command
                                             'testplugin
                                             '((name . "testcommand")
                                               (additional_params . ())
                                               (ui_description . "description")))))
                          (expect test-command :to-equal
                                  '(defun hie-testplugin-testcommand ()
                                     "description\n"
                                     (interactive "")
                                     (hie-run-command "testplugin" "testcommand" (list))))))
                    (it "can create a command with params"
                        (let ((test-command (hie-create-command
                                             'testplugin
                                             '((name . "testcommand")
                                               (additional_params . (((required . t)
                                                                      (help .  "param1 help")
                                                                      (name . "param1")
                                                                      (type . "text"))
                                                                     ((required . t)
                                                                      (help . "param2 help")
                                                                      (name . "param2")
                                                                      (type . "text"))))
                                               (ui_description . "description")))))
                          (expect test-command :to-equal
                                  '(defun hie-testplugin-testcommand (param1 param2)
                                     "description\nPARAM1: param1 help\nPARAM2: param2 help"
                                     (interactive "sparam1 (param1 help): \nsparam2 (param2 help): ")
                                     (hie-run-command "testplugin" "testcommand"
                                                      (list (list (cons 'name "param1")
                                                                  (cons 'type "text")
                                                                  (cons 'val param1))
                                                            (list (cons 'name "param2")
                                                                  (cons 'type "text")
                                                                  (cons 'val param2)))))))))
          (describe "buffer management"
                    (it "can log if buffer is killed"
                        (hie-log "Testing log buffer")
                        (kill-buffer "*hie-log*")
                        (hie-log "Testing after killing log buffer"))
                    (it "can process if buffer is killed"
                        (hie-process-filter nil "{")
                        (kill-buffer "*hie-process*")
                        (hie-process-filter nil "}"))))

;;; hie-test.el ends here
