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

(defvar async-timeout 300)

(describe "haskell-ide-engine"
          (before-all
           (setq hie-command-args
                 '("-d" "-l" "/tmp/hie.log"))
           (setq base-dir
                 (getenv "HIEBASE"))
           (setq hie-maxtimeout 10))
          (describe "process management"
                    (before-all
                     (find-file (concat base-dir "/test/testdata/HaReRename.hs")))
                    (after-each
                     (when (hie-process-live-p)
                       (hie-kill-process)))
                    (it "should start process"
                        (let ((result (hie-start-process))
                              (live (hie-process-live-p)))
                          (expect result)
                          (expect (hie-process) :to-be-truthy)
                          (expect (hie-process-tcp) :to-be-truthy)
                          (expect live :to-be-truthy)))
                    (it "should increment tcp ports"
                        (let ((old-port hie-tcp-port))
                          (hie-start-process)
                          (expect hie-tcp-port :to-equal (1+ old-port))))
                    (it "should not start a new process if there is already one"
                        (let ((result (hie-start-process))
                              (second-result (hie-start-process)))
                          (expect result)
                          (expect second-result :to-be result)))
                    (it "should kill the process"
                        (hie-start-process)
                        (let ((live (hie-process-live-p)))
                          (expect live :to-be-truthy)
                          (hie-kill-process)
                          (let ((live (hie-process-live-p)))
                            (expect live :not :to-be-truthy)
                            (expect (hie-process) :to-equal nil)
                            (expect (hie-process-tcp) :to-equal nil)))))
          (describe "session management"
                    (it "should have separate sessions"
                        (save-excursion
                          (find-file (concat base-dir "/test/testdata/HaReRename.hs"))
                          (let ((first-session (hie-session)))
                            (find-file (concat base-dir "/src/Haskell/Ide/Engine.hs"))
                            (let ((second-session (hie-session)))
                              (expect first-session :not :to-equal second-session)
                              (find-file (concat base-dir "/test/testdata/HaReRename.hs"))
                              (expect (hie-session) :to-equal first-session)
                              (find-file (concat base-dir "/src/Haskell/Ide/Engine.hs"))
                              (expect (hie-session) :to-equal second-session))))))
          (describe "command responses"
                    (before-all (when (hie-process-live-p)
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
                                                (setq hie-async-returned t))))
                                (copy-file (concat base-dir "/test/testdata/HaReRename.hs")
                                           (concat base-dir "/test/testdata/HaReRename.hs.keep"))
                                )
                    (before-each (setq response nil)
                                 (find-file (concat base-dir "/test/testdata/HaReRename.hs")))
                    (after-each (with-current-buffer (hie-process-tcp-buffer)
                                  (erase-buffer)))
                    (after-all (hie-kill-process)
                               (copy-file (concat base-dir "/test/testdata/HaReRename.hs.keep")
                                          (concat base-dir "/test/testdata/HaReRename.hs"))
                               )
                    (it "can get version info"
                        (async-with-timeout async-timeout
                                            (hie-post-message
                                             '(("cmd" . "base:version") ("params" . ()))))
                        (expect response)
                        (expect response :to-have-key 'ok))
                    (it "can list plugins"
                        (async-with-timeout async-timeout
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
                          (move-to-column 0)
                          (move-to-line 4)
                          (async-with-timeout async-timeout (hie-mode))
                          ;; See https://stackoverflow.com/questions/32961823/how-can-i-test-an-interactive-function-in-emacs
                          ;; (Thanks @cocreature)
                          (let ((unread-command-events (listify-key-sequence (kbd "c"))))
                            (async-with-timeout async-timeout
                                              (hie-hare-rename "foo_renamed")))
                          (let ((refactored-string (buffer-substring-no-properties (point-min) (point-max))))
                            (revert-buffer nil t)
                            (expect refactored-string :to-equal "\nmain = putStrLn \"hello\"\n\nfoo_renamed :: Int -> Int\nfoo_renamed x = x + 3\n\n"))))
                    (it "can run ghc-mod type"
                        (spy-on 'message)
                        (save-excursion
                          (find-file (concat base-dir "/src/Haskell/Ide/Engine/Utils.hs"))
                          (hie-kill-process)
                          (move-to-column 58)
                          (move-to-line 40)
                          (async-with-timeout async-timeout (hie-mode))
                          (async-with-timeout async-timeout (hie-ghcmod-type))
                          (expect 'message
                                  :to-have-been-called-with
                                  "(PluginId, PluginDescriptor [Command UntaggedCommandDescriptor]) -> [ParamCollision]"))))
          (describe "process input"
                    (before-all
                     (find-file (concat base-dir "/test/testdata/HaReRename.hs")))
                    (before-each
                     (setq hie-process-handle-invalid-input
                           (lambda (input) (setq error-response input)))
                     (setq hie-process-handle-message
                           (lambda (input) (setq response input))))
                    (after-each
                     (kill-buffer (hie-process-tcp-buffer)))
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
                                                                      (name . "param2CamelCase")
                                                                      (type . "text"))))
                                               (ui_description . "description")))))
                          (expect test-command :to-equal
                                  '(defun hie-testplugin-testcommand (param1 param2CamelCase)
                                     "description\nPARAM1: param1 help\nPARAM2CAMELCASE: param2 help"
                                     (interactive "sparam1 (param1 help): \nsparam2CamelCase (param2 help): ")
                                     (hie-run-command "testplugin" "testcommand"
                                                      (list (list (cons 'name "param1")
                                                                  (cons 'type "text")
                                                                  (cons 'val param1))
                                                            (list (cons 'name "param2CamelCase")
                                                                  (cons 'type "text")
                                                                  (cons 'val param2CamelCase)))))))))
          (describe "buffer management"
                    (before-all
                     (find-file (concat base-dir "/test/testdata/HaReRename.hs")))
                    (it "can log if buffer is killed"
                        (hie-log "Testing log buffer")
                        (kill-buffer "*hie-log*")
                        (hie-log "Testing after killing log buffer"))
                    (it "can process if buffer is killed"
                        (hie-process-filter nil "{")
                        (kill-buffer (hie-process-tcp-buffer))
                        (hie-process-filter nil "}"))))

;;; hie-test.el ends here
