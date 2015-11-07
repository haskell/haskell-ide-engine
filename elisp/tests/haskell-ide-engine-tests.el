;;; haskell-indentation-tests.el --- tests for indentation module -*- lexical-binding: t -*-

;; Copyright (C) 2015 Haskell Ide Engine contributors

;; See LICENSE for details.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'haskell-ide-engine)

(ert-deftest haskell-ide-engine-can-run-executable ()
  (should (equal 0 (call-process "hie"))))

(ert-deftest haskell-ide-engine-can-start-stop-process ()

  ;; first invocation starts the process
  (should (haskell-ide-engine-start-process))
  ;; second invocation does nothing but does not fail either
  (should (haskell-ide-engine-start-process))

  ;; process is alive and well at this point
  (should (haskell-ide-engine-process-live-p))

  ;; lets kill it, twice
  (haskell-ide-engine-kill-process)
  (haskell-ide-engine-kill-process)

  ;; it should be dead at this point
  (should-not (haskell-ide-engine-process-live-p)))

(ert-deftest haskell-ide-engine-can-get-version-information ()

  ;; starts the process
  (should (haskell-ide-engine-start-process))

  (let ((response))
    (setq haskell-ide-engine-process-handle-message
          (lambda (json)
            (setq response json)))
    (haskell-ide-engine-post-message
     ;; emacs json package has no representation for empty objects
     ;; '(("cmd" . "base:version") ("context" . ()) ("params" . ()))
     "{\"cmd\": \"base:version\", \"context\": {}, \"params\": {}}")

    (sit-for 2)
    (should response)
    (should (equal '(tag . "Ok") (assq 'tag response)))
    (should (assq 'contents response))))


;;; haskell-ide-engine-tests.el ends here
