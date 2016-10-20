;;; hie.el --- Haskell IDE Engine process -*- lexical-binding: t -*-

;; Copyright (c) 2015 Haskell Ide Contributors
;; Version: 0.1
;; Package-Requires: ((dash "2.12.1"))

;; See LICENSE for details.

;;; Code:

(require 'json)
(require 'dash)
(require 'cl)
(require 'haskell)
(require 'read-char-spec)

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

(defvar hie-log-buffer nil
  "Variable holding current Haskell IDE Engine log buffer")

(defvar hie-initial-tcp-port 8888
  "The initial tcp port. Other ports are allocated starting from
  this one")

(defvar hie-tcp-host "localhost")

(defvar hie-tcp-port nil)

(defvar hie-process-buffer nil
  "Variable holding current Haskell IDE Engine process buffer")

(defvar hie-process-tcp-buffer nil
  "Variable holding current Haskell IDE Engine tcp process buffer")

(defvar hie-process-handle-message nil
  "A function to handle json object.")

(defvar hie-process-handle-invalid-input nil
  "A function to handle invalid input.")

(defvar hie-refactor-buffer nil
  "Buffer holidng the diff for refactoring")

(defvar hie-post-message-hook nil
  "Function to call with message that will be send to hie process.")
(defvar hie-plugins nil
  "Plugin information gained by calling the base:plugins plugin")

(defvar hie-sessions (list)
  "All hie sessions in the Emacs session.")

(defvar hie-timeout 0.05
  "Timeout for tcp connection retries in fractions of a second.")

(defvar hie-maxtimeout 1
  "Fraction of a second after which the connection fails.")

(defun hie-session-from-buffer ()
  "Get the session based on the buffer."
  (when (buffer-file-name)
    (cl-reduce (lambda (acc a)
                 (let ((dir (hie-session-get a 'cabal-dir)))
                   (if dir
                       (if (string-prefix-p dir
					    (file-name-directory (buffer-file-name)))
                           (if acc
                               (if (and
                                    (> (length (hie-session-get a 'cabal-dir))
                                       (length (hie-session-get acc 'cabal-dir))))
                                   a
                                 acc)
                             a)
                         acc)
                     acc)))
               hie-sessions
               :initial-value nil)))

(defun hie-session-get (session key)
  "Get the SESSION's KEY value.
Returns nil if KEY not set."
  (cdr (assq key session)))

(defun hie-session-set (session key value)
  "Set the SESSION's KEY to VALUE.
Returns newly set VALUE."
  (let ((cell (assq key session)))
    (if cell
        (setcdr cell value) ; modify cell in-place
      (setcdr session (cons (cons key value) (cdr session))) ; new cell
      value)))

(defun hie-session-lookup (name)
  "Get the session by name."
  (cl-find-if (lambda (s)
                (string= name (hie-session-name s)))
              hie-sessions))

(defun hie-session-name (s)
  "Get the session name."
  (hie-session-get s 'name))

(defun hie-session-new-assume-from-cabal ()
  "Prompt to create a new project based on a guess from the nearest Cabal file.
If `haskell-process-load-or-reload-prompt' is nil, accept `default'."
  (let ((name (haskell-session-default-name)))
    (hie-session-make name)))

(defun hie-session-make (name)
  "Make a Haskell session."
  (if (hie-session-lookup name)
      (progn (hie-log "Session of name %s already exists!" name)
             (hie-session-lookup name))
    (let ((session (list (cons 'name name))))
      (add-to-list 'hie-sessions session)
      (hie-session-cabal-dir session)
      session)))

(defun hie-session-cabal-dir (s)
  "Get the session cabal-dir."
  (or (hie-session-get s 'cabal-dir)
      (let* ((cabal-file (haskell-cabal-find-file))
             (cabal-dir (if cabal-file
                            (file-name-directory cabal-file)
                            "" ;; no cabal file, use directory only
                          )))
        (progn (hie-session-set-cabal-dir s cabal-dir)
               cabal-dir))))

(defun hie-session-set-cabal-dir (s v)
  "Set the session cabal-dir."
  (let ((true-path (file-truename v)))
    (hie-session-set s 'cabal-dir true-path)))

(defun hie-session ()
  "Get the Haskell session, prompt if there isn't one or fail."
  (or (hie-session-from-buffer)
      (hie-session-new-assume-from-cabal)))

(defun hie-process-filter (process input)
  (let ((prev-buffer (current-buffer)))
    (hie-with-process-buffer
      (let ((point (point)))
        (insert input)
        (save-excursion
          (goto-char point)
          (when (re-search-forward "\^b" nil t)
            (let* ((after-stx-marker (match-end 0))
                   (input-text (buffer-substring-no-properties (point-min) (match-beginning 0)))
                   (handle-error (lambda ()
                                   (hie-log "<-parse-error %s" input-text)
                                   (when hie-process-handle-invalid-input
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

(defun really-sleep-for (sec &optional test)
  "Sleep for SEC seconds or until TEST is not-nil.

Emacs has a bug when `sleep-for' terminates early when a
subprocess ends.  This is a workaround for
http://debbugs.gnu.org/cgi/bugreport.cgi?bug=15990."

  (let ((now (cadr (current-time))))
    (while (and (or (not test) (not (funcall test)))
                (> now (- (cadr (current-time)) sec))))))

(defmacro retry-for (body maxtimeout timeout)
  "Retry BODY for MAXTIMEOUT seconds with pauses of TIMEOUT."
  `(progn
     (let ((starttime (float-time))
           (break nil)
           (result nil))
       (while (not break)
         (condition-case result
             (progn (setq result ,body) (setq break t))
           (error (if (< (- (float-time) starttime) ,maxtimeout)
                      (sleep-for ,timeout)
                    (setq break t)))))
       result)))

(defun hie-start-process (&optional additional-args)
  "Start Haskell IDE Engine process.

This function returns the process. If the process is already
running this function does nothing."
  (interactive)

  (unless (hie-process-live-p)
    (setq hie-log-buffer
          (get-buffer-create "*hie-log*"))
    (hie-set-process-buffer
     (get-buffer-create "*hie-process*"))
    (hie-set-process-tcp-buffer
     (get-buffer-create "*hie-process-tcp*"))
    (when (not hie-tcp-port)
      (setq hie-tcp-port hie-initial-tcp-port))
    (let ((process
           (apply #'start-process
                  "Haskell IDE Engine"
                  (hie-process-buffer)
                  hie-command
                  (append additional-args
                          hie-command-args
                          `("--tcp" "--tcp-port" ,(format "%s" hie-tcp-port))))))
      (let ((process-tcp
             (retry-for (open-network-stream "Haskell IDE Engine"
                                             (hie-process-tcp-buffer)
                                             hie-tcp-host
                                             hie-tcp-port)
                        hie-maxtimeout hie-timeout)))
        (setq hie-tcp-port (1+ hie-tcp-port))
        (set-process-query-on-exit-flag process-tcp nil)
        (set-process-query-on-exit-flag process nil)
        (set-process-filter process-tcp #'hie-process-filter)
        (hie-set-process-tcp process-tcp)
        (hie-set-process process))))
  (hie-process-tcp))

(defun hie-set-process (p)
  (hie-session-set (hie-session) 'hie-process p))

(defun hie-set-process-tcp (p)
  (hie-session-set (hie-session) 'hie-process-tcp p))

(defun hie-process ()
  "Get the session process."
  (hie-session-get (hie-session) 'hie-process))

(defun hie-process-tcp ()
  (hie-session-get (hie-session) 'hie-process-tcp))

(defun hie-process-buffer ()
  (hie-session-get (hie-session) 'hie-process-buffer))

(defun hie-set-process-buffer (buf)
  (hie-session-set (hie-session) 'hie-process-buffer buf))

(defun hie-process-tcp-buffer ()
  (hie-session-get (hie-session) 'hie-process-tcp-buffer))

(defun hie-set-process-tcp-buffer (buf)
  (hie-session-set (hie-session) 'hie-process-tcp-buffer buf))

(defun hie-process-live-p ()
  "Whether the Haskell IDE Engine process is live."
  (let ((process (hie-process)))
    (and process
         (process-live-p process))))

(defun hie-kill-process ()
  "Kill the Haskell IDE Engine process if it is live."
  (interactive)
  (when (hie-process-live-p)
    (let ((process (hie-process))
          (process-tcp (hie-process-tcp)))
      (delete-process process-tcp)
      (kill-process process)
      (hie-set-process nil)
      (hie-set-process-tcp nil)
      (kill-buffer (hie-process-tcp-buffer))
      (kill-buffer (hie-process-buffer))
      (hie-set-process-buffer nil)
      (hie-set-process-tcp-buffer nil))))

(defun hie-log (&rest args)
  (hie-with-log-buffer hie-log-buffer
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
  (let* ((prepared-json (hie-prepare-json json)))
    (run-hook-with-args 'hie-post-message-hook prepared-json)
    (hie-log "-> %s" prepared-json)
    (process-send-string (hie-process-tcp) prepared-json)
    ;; send \STX marker and flush buffers
    (process-send-string (hie-process-tcp) "\^b\n")))

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
        (hie-handle-refactor refactor)
      (message (format "%s" json)))))

(defun hie-handle-type-info (type-info)
  (-if-let (((&alist 'type type)) type-info)
      (message (format "%s" type))
    (message
     (format "Error extracting type from type-info response: %s"
             type-info))))

(defmacro hie-with-log-buffer (&rest body)
  `(progn (setq hie-log-buffer (get-buffer-create "*hie-log*"))
          (with-current-buffer hie-log-buffer ,@body)))

(defmacro hie-with-process-buffer (&rest body)
  `(progn (hie-set-process-tcp-buffer (get-buffer-create "*hie-process-tcp*"))
          (with-current-buffer (hie-process-tcp-buffer) ,@body)))

(defmacro hie-with-refactor-buffer (&rest body)
  `(progn (setq hie-refactor-buffer (get-buffer-create "*hie-refactor*"))
          (with-current-buffer hie-refactor-buffer ,@body)))

(defmacro hie-literal-save-excursion (&rest body)
  "Like save-excursion but preserves line and column instead of point"
  `(let ((old-col (current-column))
         (old-row (line-number-at-pos)))
     (save-excursion
       ,@body)
     (move-to-line old-row)
     (move-to-column old-col)))

(defun move-to-line (N)
  (goto-char (point-min))
  (forward-line (1- N)))

(defun hie-handle-refactor (refactor)
  (hie-with-refactor-buffer
   (erase-buffer))
  (setq r3 (mapcar `cdaddr refactor))
  (setq r4 (list 'ok r3))
  (let ((current-file-name (buffer-file-name))
        (start-line-no 0)
        (start-col-no  0)
        )

    (process-result current-file-name r4 start-line-no start-col-no )
    )
  ;; (dolist (one refactor)
  ;;   (hie-handle-one-refactor (list one))
  ;;   )
  )

;; TODO: AZ: delete, no longer used
(defun hie-handle-one-refactor (refactor)
  (-if-let (((&alist 'first first 'second second 'diff diff)) refactor)
      (progn
        (hie-log "***hie-handle-one-refactor: %s" refactor)
        (hie-with-refactor-buffer
          (insert diff))
        (let ((refactored-buffer (create-file-buffer second))
              (old-buffer (or (find-buffer-visiting first)
                              (create-file-buffer first))))
          (find-file-noselect-1 refactored-buffer second nil nil second nil)
          (with-current-buffer old-buffer
            (hie-literal-save-excursion
             (erase-buffer)
             (with-current-buffer refactored-buffer
               (copy-to-buffer old-buffer (point-min) (point-max)))
             (kill-buffer refactored-buffer)))))
      (message
       (format "Error extracting refactor information from refactor response: %s"
               refactor))))

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
        (context (hie-get-context))
        (topdir (hie-session-cabal-dir (hie-session)))
        (save (hie-plugin-command-save (intern plugin) command hie-plugins)))

    (maybe-save-buffers topdir save)

    ;; First check to see if there are any modified buffers for this project
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
          (param-names (-map (-lambda ((&alist 'name name)) name)
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
                         `(list (cons 'name ,name)
                                (cons 'type ,type)
                                (cons 'val ,(intern name))))
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

(defun* hie-plugin-command-save (plugin name list)
  (dolist (l (cdr (assoc plugin list)))
    (let ((name-cons (assoc 'name l))
          (save-cons (assoc 'save l)))
      (when (and name-cons
                 (string-equal (cdr name-cons) name)
                 save-cons)
        (return-from hie-plugin-command-save (cdr save-cons))))))

(defun maybe-save-buffers (topdir save)
  (cond
   ((string= "save_all" save)
    (when (-any (lambda (buffer)
                  (and (buffer-file-name buffer)
                       (buffer-modified-p buffer)
                       (string-prefix-p topdir (buffer-file-name buffer))))
                (buffer-list))
      (when
          (y-or-n-p
           "Project buffers have been modified. Would you like to save them?")
        (save-some-buffers t
                           (lambda ()
                             (and buffer-file-name
                                  (string-prefix-p topdir buffer-file-name)))))))))

(define-minor-mode hie-mode
  "Haskell IDE Engine mode.

Keymap:
\\{hie-mode-map}"
  :group 'haskell
  :lighter "HIE"
  :keymap 'hie-mode-map

  (if hie-mode
      (progn
        (unless (hie-process-live-p)
          (let* ((file (buffer-file-name))
                 (dir (when file (file-name-directory file)))
                 (additional-args (when dir (list "-r" dir))))
            (hie-start-process additional-args)))
        (setq hie-process-handle-message
              #'hie-handle-first-plugins-command)
        (hie-post-message
         '(("cmd" . "base:plugins"))))

    ;; we need to kill hie if this is the last one buffer standing
    (progn
      (unless (cl-find-if (lambda (buffer)
                           (with-current-buffer buffer
                             (bound-and-true-p hie-mode))) (buffer-list))
        (hie-kill-process)))))

(provide 'hie)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AZ experimenting with bringing original HaRe elisp mechanism over
;; (second copy, it came from Wrangler originally)

(defvar modified-files nil)
(defvar files-to-write nil)
(defvar files-to-rename nil)
(defvar refactoring-committed nil)
(defvar unopened-files nil)
(defvar ediff-ignore-similar-regions t)
(defvar refactor-mode nil)
(defvar has-warning 'false)
(defvar refac-result nil)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; expecting result of the form
;;  (ok FILE_LIST)
;; or
;;  (error STRING)
(defun process-result(current-file-name result line-no column-no)
  "process the result return by refactoring"
  (hie-log "***process-result: %s" current-file-name)
  (hie-log "***process-result: %s" result)
  (let (rsn modified renamed warning name_change)
  (cond ((equal (elt result 0) 'ok)
         (setq modified (if (> (length result) 1)
                            (elt result 1)
                          nil))
         (setq renamed (if (> (length result) 3)
                             (elt result 2)
                           nil))
         (setq warning (if (> (length result) 3)
                             (elt result 3)
                           (if (> (length result) 2)
                               (elt result 2)
                             nil)))
         (if warning
             (setq has-warning warning)
           nil)
         (if (equal modified nil)
             (message "Refactoring finished, and no file has been changed.")
           (preview-commit-cancel current-file-name modified renamed)
           (if (not (eq line-no 0))
             (with-current-buffer (get-file-buffer-1 current-file-name)
               (goto-line line-no)
               (goto-column column-no))
             nil)))
        ((equal (elt result 0) 'error)
         (setq rsn (elt result 1))
         (message "Refactoring failed: %S" rsn))
        ((equal (elt result 0) 'badrpc)
         (setq rsn (elt result 1))
         (message "Refactoring failed: %S" rsn))
        ((equal result ['abort])
         (message "Refactoring aborted.")))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hare-ediff(file1 file2)
  "run ediff on file1 and file2"
  (setq refactor-mode t)
  (ediff file1 file2)
)

(add-hook 'ediff-quit-hook 'my-ediff-qh)

(defun my-ediff-qh()
  "Function to be called when ediff quits."
  (if (equal refactor-mode t)
      (if (equal modified-files nil)
          (commit-or-abort)
        (if (y-or-n-p "Do you want to preview changes made to other files?")
            (progn
              (defvar file-to-diff)
              (setq file-to-diff (car modified-files))
              (setq modified-files (cdr modified-files))
              (if (get-file-buffer-1 file-to-diff)
                  nil
                (setq unopened-files (cons file-to-diff unopened-files))
                )
              (hare-ediff file-to-diff (concat (file-name-sans-extension file-to-diff)
                                                ".refactored"
                                                (file-name-extension file-to-diff t) )))
          (progn
            (setq modified-files nil)
            (commit-or-abort))))
    nil))

(defun preview-commit-cancel(current-file-name modified renamed)
  "preview, commit or cancel the refactoring result"
  (setq files-to-write modified)
  (setq files-to-rename renamed)
  (preview-commit-cancel-1 current-file-name modified)
  )


(defun preview-commit-cancel-1 (current-file-name modified)
  "preview, commit or cancel the refactoring result"
  (let ((answer (read-char-spec-1 "Do you want to preview(p)/commit(c)/cancel(n) the changes to be performed?(p/c/n):"
                  '((?p p "Answer p to preview the changes")
                    (?c c "Answer c to commit the changes without preview")
                    (?n n "Answer n to abort the changes")))))
    (cond ((equal answer 'p)
           (defvar first-file)
           (setq first-file (car modified))
           (setq modified-files (cdr modified))
           (hare-ediff first-file
                           (concat (file-name-sans-extension first-file)
                                   ".refactored"
                                   (file-name-extension first-file t))))
          ((equal answer 'c)
           (commit))
          ((equal answer 'n)
           (abort-changes)))))

(defun current-time-suffix()
  "Generate a time stamp suffix for backed up files, including a leading '.'"
  (format-time-string ".%Y%m%d%H%M%S")
)

(defun delete-swp-file-and-buffers (files)
  "delete those .refactored file and buffers generated by the refactorer. NOTE:also renames the files"
  (let ((suf (current-time-suffix)))
	(dolist (f files)
	  (let (old-file-name new-file-name swp-file-name)
		(setq old-file-name (elt f 0))
		(setq new-file-name (elt f 1))
		(setq swp-file-name (elt f 2))

		;; At this stage there are no file renaming operations, so we
		;; simply need to replace old-file-name with swp-file-name
		(rename-file old-file-name (concat old-file-name suf))
		(rename-file swp-file-name old-file-name)

		(let ((swp-buff (get-file-buffer-1 swp-file-name)))
		  (if swp-buff (kill-buffer swp-buff)
			nil))
										;(delete-file  swp-file-name)
		(let ((buffer (get-file-buffer-1 old-file-name)))
		  (if buffer
			  (if (equal old-file-name new-file-name)
				  (with-current-buffer buffer (revert-buffer nil t t))
				(with-current-buffer buffer
				  (set-visited-file-name new-file-name)
				  ;;(delete-file old-file-name)
				  (revert-buffer nil t t)))
			nil))))))

(defun abort-changes()
  "abort the refactoring results"
  (dolist (uf unopened-files)
    (kill-buffer (get-file-buffer-1 uf)))
  (setq unopened-files nil)
  (setq refactor-mode nil)
  (message "Refactoring aborted.")

  ;; (erl-spawn
  ;;   (erl-send-rpc wrangler-erl-node 'wrangler_preview_server 'abort (list))
  ;;   (erl-receive ()
  ;;       ((['rex ['badrpc rsn]]
  ;;         (setq refactor-mode nil)
  ;;         (message "Aborting refactoring failed: %S" rsn))
  ;;        (['rex ['error rsn]]
  ;;         (setq refactor-mode nil)
  ;;         (message "Aborting refactoring failed: %s" rsn))
  ;;        (['rex ['ok files]]
  ;;         (dolist (f files)
  ;;           (progn
  ;;             (let ((buff (get-file-buffer-1 f)))
  ;;               (if buff (kill-buffer (get-file-buffer-1 f))
  ;;                 nil))
  ;;             (delete-file f)))
  ;;         (dolist (uf unopened-files)
  ;;           (kill-buffer (get-file-buffer-1 uf)))
  ;;         (setq unopened-files nil)
  ;;         (setq refactor-mode nil)
  ;;         (message "Refactoring aborted."))))))
)

(defun commit-or-abort()
  "commit or abort the refactoring result."
  (if (y-or-n-p "Do you want to perform the changes?")
      (commit)
    (progn
      ;; files is returned by the wrangler backend in the orig.
      ;; Seems to be the list of modified files
      ;; (dolist (f files)
      ;;   (progn
      ;;     (let ((buff (get-file-buffer-1 f)))
      ;;       (if buff (kill-buffer (get-file-buffer-1 f))
      ;;         nil))
      ;;     (delete-file f)))
      (abort-changes)
      )))
      ;; (dolist (uf unopened-files)
      ;;   (kill-buffer (get-file-buffer-1 uf)))
      ;; (setq unopened-files nil)
      ;; (setq refactor-mode nil)
      ;; (message "Refactoring aborted."))))

(defun prepare-to-commit()
  ";make sure the files are writeable when cleaecase is used as the repository."
  (run-hook-with-args 'before-commit-functions files-to-write files-to-rename)
  (setq files-to-write nil)
  (setq files-to-rename nil)
  )

(defun commit()
  "commit the refactoring result."
  ;; (if (equal version-control-system 'ClearCase)
  ;;     (prepare-to-commit)
  ;;   nil
  ;;   )
  (do-commit)
  )

(defun do-commit()
  "commit the refactoring result."
    (let ((files (list)))
          ;; (message "files-to-write=%s" (prin1-to-string files-to-write))
          (dolist (uf files-to-write)
            (progn
              ;; (message "uf=%s" (prin1-to-string uf))
              (setq files (cons
                           (list uf uf
                           (concat (file-name-sans-extension uf)
                                   ".refactored"
                                   (file-name-extension uf t) ))
                           files))))
          ;; (message "files=%s" (prin1-to-string files))
          (delete-swp-file-and-buffers files)
          (setq refactoring-committed t)
          (dolist (uf unopened-files)
            (kill-buffer (get-file-buffer-1 uf)))
          (setq unopened-files nil)
          (setq refactor-mode nil)
          (if (equal has-warning 'true)
              (progn
                (message "Refactoring succeeded, but please read the warning message in the *erl-output* buffer.")
                (setq has-warning 'false))
            nil
            )
          )
)
(defun revert-all-buffers()
  "Refreshs all open buffers from their respective files"
      (interactive)
      (let* ((list (buffer-list))
             (buffer (car list)))
        (while buffer
          (if (string-match "\\*" (buffer-name buffer))
              (progn
                (setq list (cdr list))
                (setq buffer (car list)))
            (progn
              (set-buffer buffer)
              (if (file-exists-p (buffer-file-name buffer))
                  (revert-buffer t t t)
                nil)
              (setq list (cdr list))
              (setq buffer (car list)))))))


(defun current-buffer-saved(buffer)
  (let* ((n (buffer-name buffer)) (n1 (substring n 0 1)))
    (if (and (not (or (string= " " n1) (string= "*" n1))) (buffer-modified-p buffer))
        (if (y-or-n-p "The current buffer has been changed, and HaRe needs to save it before refactoring, continue?")
            (progn (save-buffer)
                   t)
          nil)
      t)))

(defun buffers-saved()
  (let (changed)
      (dolist (b (buffer-list) changed)
        (let* ((n (buffer-name b)) (n1 (substring n 0 1)))
          (if (and (buffer-file-name b) (buffer-modified-p b))
              (setq changed (cons (buffer-name b) changed)))))
      (if changed
          (if (y-or-n-p (format "There are modified buffers: %s, which HaRe needs to save before refactoring, continue?" changed))
              (progn
                (save-some-buffers t) t)
            nil)
        t)))


(defun buffers-changed-warning()
  (let (changed)
    (dolist (b (buffer-list) changed)
      (let* ((n (buffer-name b)) (n1 (substring n 0 1)))
        (if (and (not (or (string= " " n1) (string= "*" n1))) (buffer-modified-p b))
            (setq changed (cons (buffer-name b) changed)))))
    (if changed
        (if (y-or-n-p (format "Undo a refactoring could also undo the editings done after the refactoring, undo anyway?"))
            t
          nil)
      t)
    ))

(defun get-file-buffer-1(f)
  (if (featurep 'xemacs)
      (progn
        (setq file-buffer nil)
        (setq buffers (buffer-list))
        (setq f1 (replace-in-string f "/" "\\\\"))
        (while (and (not file-buffer) (not (equal buffers nil)))
          (let ((filename (buffer-file-name (car buffers))))
            (if filename
                (progn
                  (if (equal (downcase f1) (downcase filename))
                      (setq file-buffer (car buffers))
                    (setq buffers (cdr buffers)))
                  )
              (setq buffers (cdr buffers))
              )))
        file-buffer)
    (get-file-buffer f)))



(defun update-buffers(files)
  "update the buffers for files that have been changed"
  (dolist (f files)
    (let ((buffer (get-file-buffer-1 f)))
      (if buffer
          (with-current-buffer buffer (revert-buffer nil t t))
        nil))))


;;; hie.el ends here
