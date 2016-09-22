;;; hie.el --- Haskell IDE Engine process -*- lexical-binding: t -*-

;; Copyright (c) 2015 Haskell Ide Contributors
;; Version: 0.1
;; Package-Requires: ((dash "2.12.1"))

;; See LICENSE for details.

;;; Code:

(require 'json)
(require 'dash)
(require 'buttercup)
(require 'haskell)

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
  (when (hie-session-lookup name)
    (error "Session of name %s already exists!" name))
  (let ((session (list (cons 'name name))))
    (add-to-list 'hie-sessions session)
    (hie-session-cabal-dir session)
    session))

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
  (-if-let (((&alist 'first first 'second second 'diff diff)) refactor)
      (progn
        (hie-with-refactor-buffer
          (erase-buffer)
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

;;; hie.el ends here
