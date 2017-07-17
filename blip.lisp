;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Copyright 2017 Joyent, Inc.
;;; Copyright 2017 Nicholas Zivkovic

(load "/opt/quicklisp/setup.lisp")
(ql:quickload "let-over-lambda")
(ql:quickload "cl-quickcheck")
(ql:quickload "drakma")
(ql:quickload "cl-ppcre")
(ql:quickload "trivial-ssh")
(ql:quickload "inferior-shell") ; Convenient for synchronous command execution
(ql:quickload "cl-async") ; Libuv wrapper, convenient for background commands
(ql:quickload "blackbird") ;promises over cl-async
(ql:quickload "cl-ncurses")
(ql:quickload "series")
(ql:quickload "iterate")
(ql:quickload "uuid")
(ql:quickload "ironclad")
;Works, but not too good at validation
;(ql:quickload "unix-options")
(load "diff-sexp.lisp")

(defun sha256-file (path)
  (let ((digester (ironclad:make-digest :sha256)))
    (ironclad:digest-file digester path)
    ))

(defun quiet-load (path)
  ;;; TODO see if we can do this via a nil make-broadcast-stream...
  (with-open-file
      (*error-output* "/dev/null" :direction :output :if-exists :supersede)
    (load path))
  )

(defmacro defmacro! (&body body)
  "Shorthand for lol:defmacro"
  `(lol:defmacro! ,@body))

(defmacro! uuid ()
  (uuid:make-v4-uuid))

(defmacro! flatten (&body body)
  "Shorthand for lol:flatten"
  `(lol:flatten ,@body))

(defun timed-funcall (func arg)
  "Calls function taking 1 arg and returns the return value of func and the
   time-elapsed (in micro-seconds)"
  (let ((start (get-internal-real-time))
        (end nil)
        (ret nil))
    (setf ret (funcall func arg))
    (setf end (get-internal-real-time))
    (values ret (- end start))))

(defun mysleep (s)
  (sleep s)
  s)

(defun pipeline (arg &rest fns)
  "Takes a list of 1 args funcs, and chains them in a pipeline, so that each
   function takes the return value of the previous function as its argument"
  (iter:iter
    (iter:for f in fns)
    (iter:for (values ret time) initially (values arg 0) then (timed-funcall f ret))
    (iter:after-each (iter:collect (list f time) into timing))
    (iter:finally (return (values ret timing)))))

(defun pipeline-until-aux (arg fns)
  (if (and fns)
      (let ((ret nil))
        (setf ret (funcall (car fns) arg))
        (if (not ret)
            (progn (pipeline-until-aux arg (cdr fns)))
            (progn ret)
        ))
      (progn nil)))

(defun pipeline-until (arg &rest fns)
  "Like pipeline, except that it stops calling functions after a function
   returns a non-nil value."
  (pipeline-until-aux arg fns))

(defun test-pipeline-until ()
  (pipeline-until 42
                 #'(lambda (n) (= n 10))
                 #'(lambda (n) (= n 20))
                 #'(lambda (n) (= n 30))
                 ;#'(lambda (n) (= n 42))
                 #'(lambda (n) (= n 50))
                 ))

(defun test-pl ()
  (pipeline 1 #'mysleep #'mysleep #'mysleep))

(defun string-to-symbol (s)
  (intern (string-upcase s)))

(defun symbol-to-string (s)
  (string-downcase (symbol-name s)))

(defun str-cat-2 (s1 s2)
  "Concatenates 2 strings"
  (concatenate 'string s1 s2))

(defun str-cat (&rest strings)
  "Concatenates 2 or more strings"
  (reduce #'str-cat-2 strings))

(defun str-last-char (s)
  "Gets the last character of a string"
  (char s (- (length s) 1)))

(defun is-str-suffix (suf str)
  "Checks if the strings has the given suffix"
  (if (> (length suf) (length str))
      nil
      (let* ((off (- (length str) (length suf)))
             (end (subseq str off (length str))))
        (string= suf end)
        )))

(defun is-str-prefix (pre str)
  "Checks if the strings has the given prefix"
  (if (> (length pre) (length str))
      nil
      (let* ((off (length pre))
             (end (subseq str 0 off)))
        (string= pre end)
        )))

(defun str-has (regex str)
  (cl-ppcre:all-matches regex str))

(defun is-npm-range-version (v)
  (or (str-has "\\^" v)
      (str-has "~" v)
      (str-has "\\*" v)
      (str-has "-" v)
      (str-has ">" v)
      (str-has "<" v)
      (str-has "=" v)
      (str-has "x" v)
      (str-has "X" v)
      (str-has "\\+" v)
      ))

(defun str-split (regex str)
  "Splits the string at each place that the given pcre regex is matched"
  (cl-ppcre:split regex str))

(defun cwd ()
  "Returns the current working directory"
  (sb-posix:getcwd))

(defun chdir (d)
  "Changes the current working directory"
  (sb-posix:chdir d))

(defun mkdir (d)
  (assert (> (length d) 0))
  (if (CHAR= #\/ (str-last-char d))
      (ensure-directories-exist d)
      (ensure-directories-exist (str-cat d "/"))
      ))


(defvar dirhist '())

(defun pushdir (d)
  "Changes the current working directory and update the stack"
  (mkdir d)
  (setf dirhist (pushr dirhist (cwd)))
  (chdir d)
  )

(defun popdir ()
  "Removes the current working directory from the stack, and sets it to the new
   top element of the stack. Essentially a back-button"
  (chdir (car (last dirhist)))
  (setf dirhist (popr dirhist))
  )


(defvar blip-root "/depot/synthesis/blip/")
(defvar blip-stor (str-cat blip-root "stor/"))
(defvar blip-tickets (str-cat blip-stor "tickets/"))
(defvar blip-meta (str-cat blip-root "meta/"))
(defvar blip-tmp (str-cat blip-meta "tmp/"))
(defvar blip-du (str-cat blip-meta "disk-usage/"))
(defvar blip-bin (str-cat blip-meta "bin/"))
(defvar blip-env (str-cat blip-meta "env/"))
(defvar blip-env-stack (str-cat blip-env "stack"))
(defvar blip-env-avail (str-cat blip-env "avail"))
(defvar blip-env-sha (str-cat blip-env "sha"))
(defvar blip-core (str-cat blip-bin "blip"))
(defvar blip-logs (str-cat blip-meta "logs/"))
(defvar blip-repos (str-cat blip-stor "repos/"))
(defvar blip-asts (str-cat blip-stor "repo-asts/"))
(defvar blip-repo-meta (str-cat blip-stor "repo-meta/"))
(defvar blip-joyent-asts (str-cat blip-asts "joyent/"))
(defvar blip-joyent-meta (str-cat blip-repo-meta "joyent/"))
(defvar blip-joyent-repos (str-cat blip-repos "joyent/"))
(defvar blip-repos-index (str-cat blip-stor "repos-index/"))
(defvar blip-joyent-repo-list (str-cat blip-repos-index "joyent"))
(defvar blip-self-repo (str-cat blip-repos "blip/"))
(defvar blip-code (str-cat blip-self-repo "blip.lisp"))
(defvar blip-env-cfg (str-cat blip-env "env-cfg.lisp"))
(defvar blip-latest-ix-ver 0)
(defvar github-base-url "https://github.com/")
(defvar npm-base-url "https://registry.npmjs.com/")
(defvar gerrit-base-url "https://cr.joyent.us/p/")
(defvar github-api-url "https://api.github.com/")
(defvar blip-github-users (str-cat blip-repos-index "github-users"))

(defun blip-env-cfg-currentp ()
  (let* ((shanew (sha256-file blip-env-cfg))
         (shaold (file-to-form blip-env-sha))
         (current (equalp shaold shanew))
         )
    (if (not current)
        (form-to-file shanew blip-env-sha)
        )
    current
    ))

(defvar github-repo-blacklist '("natural-earth-vector"))

(defmacro! enclose (&rest x)
  `(list ,@x))

(defun rdep (system)
  "Return list of Quicklisp packages that use SYSTEM"
  (let (rdeps)
    (dolist (s (ql-dist:provided-systems (ql-dist:find-dist "quicklisp")) rdeps)
      (if (member system (ql-dist:required-systems s) :test #'equal)
          (push (ql-dist:name s) rdeps)))))

(defun github-user-repos-url (user)
  "Creates github URL that we can use to access a user's repos"
  (str-cat github-api-url "users/" user "/repos"))

(defun repo-spec-to-github-url (spec)
  (str-cat github-base-url spec ".git"))

(defun repo-specs-to-github-urls (specs)
  (map 'list #'repo-spec-to-github-url specs))

(defun repo-spec-to-gerrit-url (spec)
  (str-cat gerrit-base-url spec ".git"))

(defun repo-specs-to-gerrit-urls (specs)
  (map 'list #'repo-spec-to-gerrit-url specs))

(defun reload ()
  "Reloads this file"
  (load blip-code))

(defmacro! is-cmd-verb (s)
  `(string= verb ,s))

(defun str-to-pov (s)
  (cond
    ((string= s "up") :up)
    ((string= s "down") :down)
    ((and t) nil)))

(defmacro! twice (&body body)
  `(progn ,@body ,@body))

(defmacro! thrice (&body body)
  `(progn ,@body ,@body ,@body))

(defmacro! in-index-path-cli (&body body)
  `(let* ((noun nouns)
          (path nil)
          (pov nil)
          (force nil)
          (page nil))
     (setf path (car noun))
     (setf noun (cdr noun))
     (setf pov (str-to-pov (car noun)))
     (if pov (setf noun (cdr noun)))
     (twice
       (cond
         ((string= (car noun) "--page")
          (setf page (cadr noun))
          (setf noun (cddr noun))
          )
         ((string= (car noun) "--force")
          (setf force t)
          (setf noun (cdr noun))
          )
         ))
     ,@body
     ))

(defmacro! in-index-nopath-cli (&body body)
  `(let* ((noun nouns)
          (pov nil)
          (force nil)
          (page nil))
     (setf pov (str-to-pov (car noun)))
     (if pov (setf noun (cdr noun)))
     (twice
       (cond
         ((string= (car noun) "--page")
          (setf page (cadr noun))
          (setf noun (cddr noun))
          )
         ((string= (car noun) "--force")
          (setf force t)
          (setf noun (cdr noun))
          )
         ))
     ,@body
     ))

(defun load-top-env ()
  (pushenv (load-env (car (last (file-to-form blip-env-stack)))))
  )

(defmacro! in-ast-ls-cli (&body body)
  `(let* ((noun nouns)
          (pref nil)
          (force nil)
          (count nil))
     (thrice
       (cond
         ((string= (car noun) "--count")
          (setf count t)
          (setf noun (cdr noun))
          )
         ((string= (car noun) "--pref")
          (setf pref (cadr noun))
          (setf noun (cddr noun))
          )
         ((string= (car noun) "--force")
          (setf force t)
          (setf noun (cdr noun))
          )
         ))
     ,@body
     ))

(defun file-size (path)
  (if (not path) (return-from file-size nil))
  ;;; TODO need to use conditions to handle stat-failures.
  (let ((stat-obj (sb-posix:stat path)))
    (if (and stat-obj)
        (sb-posix:stat-size stat-obj)
        nil)
    )
  )

(defun compute-disk-usage ()
  "We compute disk usage, by walking our directories. We don't use `du`, because
   it it will error out if a file gets deleted while it tries to stat it. So, we
   instead walk the directory and attempt to get the stat-size, dropping any
   errors if they arise. We do this because other blips may be running and
   modifying the filesystem and we don't want to get in their way. If this is
   too slow, we can always run blip in a snapshot of the store (or something)."
  ;(let* (())
    ;)
  )

(defun print-ln (form)
  (print form)
  (format t "~%"))

(defun main ()
  "The main entry point for this program."
  (let* ((argv sb-ext:*posix-argv*)
         (verb (cadr argv))
         (nouns (cddr argv)))
    (cond
      ((is-cmd-verb "help")
       )
      ((is-cmd-verb "compute-disk-usage")
       (let ((date (get-universal-time)))
         ; Exec du on dirs of interest, crunch/agg output into a table
         ; Save tuple with date and table.
         )
       )
      ((is-cmd-verb "show-disk-usage")
       )
      ((is-cmd-verb "env-ls")
       (cond
         ((not (blip-env-cfg-currentp))
          (quiet-load blip-env-cfg)
          )
         )
       (format t "~A~%" (file-to-form blip-env-avail))
       )
      ((is-cmd-verb "env-stack")
       (format t "~A~%" (file-to-form blip-env-stack))
       )
      ((is-cmd-verb "top-env")
       (load-top-env)
       (format t "~A~%" (get-env-var 'name))
       )
      ((is-cmd-verb "pushenv")
       (let* ((env-name (string-to-symbol (car nouns)))
              (env nil)
              )
         (if (have-env env-name)
             (form-to-file (pushr (file-to-form blip-env-stack) env-name)
                           blip-env-stack))
         ))
      ((is-cmd-verb "popenv")
       (form-to-file (popr (file-to-form blip-env-stack))
                     blip-env-stack)
       )
      ((is-cmd-verb "index-prefix")
       (load-top-env)
       (in-index-path-cli (print-ln (index-prefix path pov :page page :force force)))
       )
      ((is-cmd-verb "index-suffix")
       (load-top-env)
       (in-index-path-cli (print-ln (index-suffix path pov :page page :force force)))
       )
      ((is-cmd-verb "index-word-count")
       (load-top-env)
       (in-index-path-cli (print-ln (index-word-count path pov :page page :force force)))
       )
      ((is-cmd-verb "index-uniq")
       (load-top-env)
       (in-index-nopath-cli (print-ln (index-uniq pov :page page :force force)))
       )
      ((is-cmd-verb "index-path-depth")
       (load-top-env)
       (in-index-nopath-cli (print-ln (index-path-depth pov :page page :force force)))
       )
      ((is-cmd-verb "index-print")
       (load-top-env)
       (in-index-nopath-cli (print-ln (index-print pov :page page :force force)))
       )
      ((is-cmd-verb "index-build")
       (load-top-env)
       (index-build :force (and (car nouns) (string= "--force" (car nouns))))
       )
      ((is-cmd-verb "index-get-subtree")
       (load-top-env)
       (in-index-path-cli (print-ln (index-get-subtree path pov :page page :force force)))
       )
      ((is-cmd-verb "index-get-subtree-str")
       (load-top-env)
       (in-index-path-cli (print-ln (index-get-subtree-str path pov :page page :force force)))
       )
      ((is-cmd-verb "ast-ls-files")
       (load-top-env)
       (let ((files (ast-ls-files)))
         (print-ln (if (car nouns) (length files) files))
         )
       )
      ((is-cmd-verb "ast-ls-fcalls")
       (load-top-env)
       (in-ast-ls-cli (print-ln (ast-ls-fcalls count :pref pref :force force)))
       )
      ((is-cmd-verb "ast-ls-fdefs")
       (load-top-env)
       (in-ast-ls-cli (print-ln (ast-ls-fdefs count :pref pref :force force)))
       )
      ((is-cmd-verb "ast-ls-words")
       (load-top-env)
       (in-ast-ls-cli (print-ln (ast-ls-words count :pref pref :force force)))
       )
      ((is-cmd-verb "ast-ls-fbinds")
       (load-top-env)
       (in-ast-ls-cli (print-ln (ast-ls-fbinds count :pref pref :force force)))
       )
      ((is-cmd-verb "ast-parse")
       (load-top-env)
       (ast-parse (and (car nouns)))
       )
      ((is-cmd-verb "reconstruct-repo")
       (load-top-env)
       (reconstruct-repo)
       )
      ((is-cmd-verb "outputs")
       )
      ((is-cmd-verb "jobs")
       )
      ((is-cmd-verb "xform")
       )
      ((is-cmd-verb "github-users")
       (print-ln (file-to-form blip-github-users))
       )
      ((is-cmd-verb "github-user-add")
       (let ((users (file-to-form blip-github-users)))
         (cond
           ((and (car nouns) (not (member (car nouns) users :test #'equal)))
            (pushr! users (car nouns))
            (form-to-file users blip-github-users)
            )
           )
         )
       )
      ((is-cmd-verb "github-user-rem")
       (let ((users (file-to-form blip-github-users)))
         (cond
           ((and (car nouns) (member (car nouns) users :test #'equal))
            (setf users (remove-if  #'(lambda (u) (string= u (car nouns)))
                                    users))
            (form-to-file users blip-github-users)
            )
           )
         )
       )
      ((is-cmd-verb "github-user-clone")
       (cond
         ((car nouns)
          (cache-svc-user-repo-list "github" (car nouns))
          (github-clone-user-all-bg (car nouns))
          ))
       )
      ((is-cmd-verb "github-user-pull")
       (cond
         ((car nouns)
          (cache-svc-user-repo-list "github" (car nouns))
          (github-pull-user-all-bg (car nouns))
          ))
       )
      ((is-cmd-verb "pull-env")
       (load-top-env)
       (pushdir (str-cat blip-repos (get-env-var 'repo)))
       (inferior-shell:run/ss (list "git" "pull"))
       (popdir)
       )
      ((is-cmd-verb "strap-repo")
       (load-top-env)
       (strap-git-repo (get-env-var 'repo))
       )
      ((is-cmd-verb "eval")
       (if (cadr nouns)
           (form-to-file (eval (read-from-string (car nouns)))
                         (str-car blip-root (car nouns)))
           (print (eval (read-from-string (car nouns))))
           )
       )
      ((is-cmd-verb "sleep")
       (sleep (parse-integer (car nouns)))
       )
      ((and t)
       (format t "Bad verb!~%")
       (sb-ext:exit :code -1))
      )
    ))

(defun exec-self ()
  "This function executes this file as a child process"
  (inferior-shell:run/ss (list blip-core "hi")))

(defun save-core (path)
  "Saves an executable core to the given path"
  (progn
    #+sbcl
    (let ((fork-result (sb-posix:fork)))
      (case fork-result
        (-1 (error "fork failed"))
        (0 (sb-ext:save-lisp-and-die path :toplevel #'main :executable t))
        (otherwise (sb-posix:wait)))
      (format t "stand-alone core ~a saved~%" path))
    #-sbcl
    (error "not available on this lisp~%")
        (values)))


(defun install ()
  "This saves an executable core to the proper location"
  (save-core blip-core))

(defun pushr1 (ls elem)
  "Pushes a single elem to the rightmost side of the list"
  (assert (listp ls))
  (append ls (list elem)))

(defun pushl1 (ls elem)
  "Pushes a single elem to the leftmost side of the list"
  (assert (listp ls))
  (append (list elem) ls))

(defun pushr (ls &rest elems)
  "Pushes a list of elems to the rightmost side of the list"
  (reduce #'pushr1 elems :initial-value ls))

(defmacro! pushr! (ls &rest elems)
  "Same as pushr, but overwrites the list"
  `(setf ,ls (pushr ,ls ,@elems)))

(defun pushl (ls &rest elems)
  "Pushes a list of elems to the leftmost side of the list"
  (reduce #'pushl1 elems :initial-value ls))

(defmacro! pushl! (ls &rest elems)
  "Same as pushl, but overwrites the list"
  `(setf ,ls (pushl ,ls ,@elems)))

(defun append-ls (ls)
  (apply #'append ls))

(defclass stack ()
  ((list :initarg :list
         :initform '())
   (last :initarg :last
         :initform nil)
  ))

(defun stack-list (s)
  (slot-value s 'list))

(defun stack-pushr (s e)
  (if (slot-value s 'list)
      (progn
        (setf (cdr (slot-value s 'last)) (cons e nil))
        (setf (slot-value s 'last) (cdr (slot-value s 'last))))
      (progn
        (setf (slot-value s 'list) (list e))
        (setf (slot-value s 'last) (slot-value s 'list)))))

(defun stack-last (s)
  (car (slot-value s 'last)))

(defun stack-set-last (s e)
  (setf (car (slot-value s 'last)) e))

(defun map-if (ls test function)
  "Maps over a list, ignoring elems that fail a condition"
  (loop
    for e in ls
    when (funcall test e)
      collect (funcall function e)))

(defun head-n (list n)
  "Get the first n elems of a list"
  (iter:iter
    (iter:for x from 1 to n)
    (iter:for y in list)
    (iter:collect y)))

(defun popl-n (ls n)
  "Pops the leftmost n elems of a list"
  (if (< n (length ls))
      (progn (iter:iter
               (iter:for x from 1 to n)
               (setq ls (cdr ls)))
             ls)
             nil))

(defun get-nth-page (ls n pagesz)
  (head-n (popl-n ls (* n pagesz)) pagesz))

(defmacro! popl-n! (ls n)
  "Same as popl-n but overwrites the list"
  `(setf ,ls (popl-n ,ls ,n)))


(defun popr-n (ls n)
  "Pops the rightmost n elems of a list"
  (if (< n (length ls))
      (head-n ls (- (length ls) n))
      nil))

(defmacro! popr-n! (ls n)
  "Same as popr-n but overwrites the list"
  `(setf ,ls (popr-n ,ls ,n)))

(defun popl (ls)
  "Pops the leftmost elem from the list"
  (popl-n ls 1))

(defmacro! popl! (ls)
  "Same as popl but overwrite the list"
  `(setf ,ls (popl ,ls)))

(defun popr (ls)
  "Pops the rightmost elem from the list"
  (popr-n ls 1))

(defmacro! popr! (ls)
  "Same as popr but overwrite the list"
  `(setf ,ls (popr ,ls)))


(defun num-ngrams (n list)
  "Number of possible ngrams of size `n` for the list"
  (floor (/ (length list) n)))

(defun ngrams-aux (acc n list)
  (cond ((< (length list) n)
         (append acc '()))
        ((>= (length list) n)
         (ngrams-aux (pushr acc (head-n list n)) n (cdr list)))))

(defun ngrams (n list)
  "Computes all ngrams of a list"
  (ngrams-aux '() n list))

;;; Some basic string/char-list conversion funcs.

(defun str-to-char-ls (str)
  "Convert string to character list"
  (coerce str 'list))

(defun char-ls-to-str (cls)
  "Convert character list to string"
  (coerce cls 'string))

;;; White-space grouping and related boolean tests.

(defun is-white-space (c)
  "Test if a character is white space"
  (and c (characterp c)
       (or (CHAR= c #\Space) (CHAR= c #\Tab) (CHAR= c #\Newline))))


(defun white-space-list (acc head tail)
  "Collect consecutive white-space into a list"
  (tagbody
   again
     (if (is-white-space head)
         (progn (pushr! acc head)
                (setf head (car tail))
                (setf tail (cdr tail))
                (go again))))
  (list acc (cons head tail)))

(defun test-new-ws ()
  (white-space-list% '() #\Space (str-to-char-ls "   abcde   ")))

(defmacro! advance-scanner-impl ()
  `(progn (setf head (car tail))
          (setf tail (cdr tail))))

(defmacro! advance-scanner (&optional times)
  `(if (not ,times)
      (advance-scanner-impl)
      (let ((c 0))
        (tagbody
         again
           (cond
             ((< c ,times)
              (incf c)
              (advance-scanner-impl)
              (go again)))))))


(defun white-space-aux (stack head tail)
  (tagbody
   again
     (cond
       ((and (not head) (not tail))
        )
       ((and (listp head) tail)
        (progn ;(print 2)
          (stack-pushr stack head)
          (advance-scanner)
          (go again)))
       ((and (listp head) (not tail))
        ;(print 3)
        (stack-pushr stack head))
       ((is-white-space head)
        ;(print 4)
        (let ((wsls '()))
          (tagbody
           wsagain
             (if (is-white-space head)
                 (progn
                   (pushr! wsls head)
                   (advance-scanner)
                   (go wsagain))
                 (progn
                   (stack-pushr stack wsls)
                   (go again))
             ))))
       ((and t)
        ;(print 5)
        (progn
          (stack-pushr stack head)
          (advance-scanner)
          (go again)))
       ))
  (stack-list stack))

(defun white-space (ls)
  "Returns a list that is just like the input, except that all sequences of
   whitespace are grouped in their own sublists"
  (white-space-aux (make-instance 'stack) (car ls) (cdr ls)))

(defun test-ws ()
  (white-space (str-to-char-ls "123  123  123  123  ")))

(defun is-blank (c)
  "Test if a node is a blank. This equates to being either white-space or a
   comment"
  (and (listp c) (or (is-white-space-group c) (is-comment c))))

(defun is-cl-blank (c)
  "Test if a node is a blank. This equates to being either white-space or a
   comment"
  (and (listp c) (or (is-white-space-group c) (is-cl-comment c))))

(defmacro! def-blanks-aux (name test)
  `(defun ,name (stack head tail)
     (tagbody
      again
        (cond
          ((and (not head) (not tail))
           )
          ((and (listp head) (not (,test head)) tail)
           (stack-pushr stack head)
           (advance-scanner)
           (go again))
          ((and (listp head) (not (,test head)) (not tail))
           (stack-pushr stack head)
           (advance-scanner))
          ((,test head)
           (let ((ls '()))
             (tagbody
              blsagain
                (cond
                  ((,test head)
                   (pushr! ls head)
                   (advance-scanner)
                   (go blsagain)))
                (stack-pushr stack ls)))
           (go again))
          ((and t)
           (stack-pushr stack head)
           (advance-scanner)
           (go again))
          ))
     (stack-list stack)))

(def-blanks-aux blanks-aux is-blank)
(def-blanks-aux cl-blanks-aux is-cl-blank)

(defun blanks (ls)
  "Returns the input list, with all blanks in sublists"
  (blanks-aux (make-instance 'stack) (car ls) (cdr ls)))

(defun cl-blanks (ls)
  "Same as above, but for common lisp blanks"
  (cl-blanks-aux (make-instance 'stack) (car ls) (cdr ls)))



(defun test-blanks ()
  (pipeline (str-to-char-ls "123  /* */ 123  123  123  ") #'cmt-str #'white-space #'blanks))

(defun test-cl-blanks ()
  (pipeline (str-to-char-ls "123  ;my comment ") #'cl-cmt-str #'white-space #'cl-blanks))


;;; Word grouping and related boolean tests (also used in punctuation)

(defun is-punctuation (c)
  (and c (characterp c)
       (or (CHAR= c #\() (CHAR= c #\)) (CHAR= c #\{) (CHAR= c #\}) (CHAR= c #\[)
           (CHAR= c #\]) (CHAR= c #\;) (CHAR= c #\,) (CHAR= c #\.) (CHAR= c #\:)
           (CHAR= c #\?) (CHAR= c #\<) (CHAR= c #\>) (CHAR= c #\=) (CHAR= c #\+)
           (CHAR= c #\-) (CHAR= c #\*) (CHAR= c #\/) (CHAR= c #\!) (CHAR= c #\~)
           (CHAR= c #\%) (CHAR= c #\|) (CHAR= c #\&) (CHAR= c #\^))))

(defun is-non-nestable-punctuation (c)
  (and (is-punctuation c) (CHAR/= c #\{) (CHAR/= c #\}) (CHAR/= c #\()
       (CHAR/= c #\)) (CHAR/= c #\[) (CHAR/= c #\])))

(defun is-word-char (c)
  (and c (characterp c) (not (is-white-space c)) (not (is-punctuation c))))

(defun words-aux (stack head tail)
  (tagbody
    again
      (cond
        ((and (not head) (not tail))
         )
        ((and (listp head) tail)
         (stack-pushr stack head)
         (advance-scanner)
         (go again))
        ((and (listp head) (not tail))
         (stack-pushr stack head)
         (advance-scanner)
         (go again))
        ((characterp head)
         (cond
           ((is-word-char head)
            (let ((ls '()))
              (tagbody
               lsagain
                 (cond
                   ((is-word-char head)
                    (pushr! ls head)
                    (advance-scanner)
                    (go lsagain))
                   ((and t)
                    (stack-pushr stack ls)
                    (go again))))))
           ((and t)
            (stack-pushr stack head)
            (advance-scanner)
            (go again))))

           ((and t)
            (stack-pushr stack head)
            (advance-scanner)
            (go again)
            )))
  (stack-list stack))

(defun words (ls)
  (words-aux (make-instance 'stack) (car ls) (cdr ls)))

(defun test-words ()
  (pipeline (str-to-char-ls "123  /* */ 123  123  123  ") #'cmt-str #'white-space #'blanks #'words))

;;; Punctuation grouping and related boolean tests (also used in punctuation)

(defun punctuations-list (acc head tail)
  (if (is-non-nestable-punctuation head)
      (punctuations-list (pushr acc head) (car tail) (cdr tail))
      (list acc (cons head tail))))

(defun punctuations-aux (stack head tail)
  (tagbody
   again
     (cond
       ((and (not head) (not tail))
        )
       ((and (listp head) tail)
        (stack-pushr stack head)
        (advance-scanner)
        (go again))
       ((and (listp head) (not tail))
        (stack-pushr stack head)
        (advance-scanner)
        (go again))
       ((characterp head)
        (cond
          ((is-non-nestable-punctuation head)
           (let ((ls '()))
             (tagbody
              lsagain
                (cond
                  ((is-non-nestable-punctuation head)
                   (pushr! ls head)
                   (advance-scanner)
                   (go lsagain))
                  ((and t)
                   (stack-pushr stack ls)
                   (go again))))))
          ((and t)
           (stack-pushr stack head)
           (advance-scanner)
           (go again))))))
  (stack-list stack)
  )

(defun punctuations (ls)
  (punctuations-aux (make-instance 'stack) (car ls) (cdr ls)))

(defun test-puncts ()
  (pipeline (str-to-char-ls "123 (1 2 3); 1,2,3 /* */ 123 123 123 ") #'cmt-str
  #'white-space #'blanks #'words #'punctuations))


(defun nestables-aux (acc head tail fin)
  (cond
    ((and head (listp head))
     (nestables-aux (pushr acc head) (car tail) (cdr tail) fin))
    ((and (not head) (not tail))
     (values acc tail))
    ((or (and fin head (CHAR= head fin))
         (and head (not tail)))
     (values (pushr acc head) tail))
    ((characterp head)
     (cond
       ((and acc (CHAR= head #\())
        (multiple-value-bind (nacc ntail) (nestables-aux '() head tail #\))
          (nestables-aux (pushr acc nacc) (car ntail) (cdr ntail) fin)
          )
        )
       ((and acc (CHAR= head #\{))
        (multiple-value-bind (nacc ntail) (nestables-aux '() head tail #\})
          (nestables-aux (pushr acc nacc) (car ntail) (cdr ntail) fin)
          )
        )
       ((and acc (CHAR= head #\[))
        (multiple-value-bind (nacc ntail) (nestables-aux '() head tail #\])
          (nestables-aux (pushr acc nacc) (car ntail) (cdr ntail) fin)
          )
        )
       ((and t)
        (nestables-aux (pushr acc head) (car tail) (cdr tail) fin)
        )))))

(defun nestables (ls)
  (nestables-aux '() (car ls) (cdr ls) nil))

(defun test-nestables ()
  (nestables (str-to-char-ls "fn (a, b, c) { a e; a e; a e = c; a e = c && b || {}; cb();} more() { stuff }; foo(); foo(); foo();")))

(defun nestable-assert (char head)
  (cond
    ((CHAR= char #\()
     (assert (and head (CHAR/= #\} head) (CHAR/= #\] head))))
    ((CHAR= char #\{)
     (assert (and head (CHAR/= #\) head) (CHAR/= #\] head))))
    ((CHAR= char #\[)
     (assert (and head (CHAR/= #\) head) (CHAR/= #\} head))))))

(defun is-empty-nestable (ls)
  (let ((f (car ls)) (l (cadr ls)))
    (and (= 2 (length ls)) (and (characterp f) (characterp l)
                               (or (and (CHAR= f #\() (CHAR= l #\)))
                                   (and (CHAR= f #\{) (CHAR= l #\}))
                                   (and (CHAR= f #\[) (CHAR= l #\])))))))

(defun is-empty-str (ls)
  (let ((f (car ls)) (l (cadr ls)))
    (and (= 2 (length ls)) (and (characterp f) (character l)
                                (or (and (CHAR= f #\') (CHAR= l #\'))
                                    (and (CHAR= f #\") (CHAR= l #\")))))))

; TODO: return a pair of consumed and tail
(defmacro! is-bcmt-end ()
  `(and (CHAR= head #\*) (CHAR= (car tail) #\/)))

(defmacro! is-lcmt-end ()
  `(CHAR= head #\Newline))

(defmacro! str-tagbody  (quote tag)
  `(let ((ls '())
         (nq 0))
     (tagbody
      ,tag
        (cond
          ((and (CHAR/= head ,quote) (not tail))
           (pushr! ls head))
          ((CHAR= head ,quote)
           (incf nq)
           (pushr! ls head)
           (advance-scanner)
           (if (< nq 2)
               (go ,tag)))
          ((CHAR= head #\\)
           (pushr! ls head (car tail))
           (advance-scanner 2)
           (go ,tag)
           )
          ((not (CHAR= head #\\))
           (pushr! ls head)
           (advance-scanner)
           (go ,tag))))
     (stack-pushr stack ls)
     (go again)))


(defun cmt-str-aux (stack head tail)
  (tagbody
   again
     (cond
       ((and (not head) (not tail))
        )
       ((and head (not tail))
        (stack-pushr stack head))
       ((and (CHAR= head #\/) (CHAR= (car tail) #\*))
        (let ((bcls '()))
          (tagbody
           bcmtagain
             (cond
               ((and (not (is-bcmt-end)) (not tail))
                (pushr! bcls head)
                (advance-scanner))
               ((and (is-bcmt-end))
                (pushr! bcls #\* #\/)
                (advance-scanner 2))
               ((not (is-bcmt-end))
                (pushr! bcls head)
                (advance-scanner)
                (go bcmtagain))
               )
             (stack-pushr stack bcls)
             (go again)
             )
        ))
       ((and (CHAR= head #\/) (CHAR= (car tail) #\/))
        (let ((lcls '()))
          (tagbody
           lcmtagain
             (cond
               ((and (not (is-lcmt-end)) (not tail))
                (pushr! lcls head)
                (advance-scanner))
               ((and (is-lcmt-end))
                (pushr! lcls #\Newline)
                (advance-scanner))
               ((not (is-lcmt-end))
                (pushr! lcls head)
                (advance-scanner)
                (go lcmtagain))
               )
             (stack-pushr stack lcls)
             (go again)
             )
        ))
       ((CHAR= head #\")
        (str-tagbody #\" dqagain)
        )
       ((CHAR= head #\')
        (str-tagbody #\' sqagain)
        )
       ((and t)
        (stack-pushr stack head)
        (advance-scanner)
        (go again)
        )
       )
     )
  (stack-list stack)
  )

(defun cl-cmt-str-aux (stack head tail)
  (tagbody
   again
     (cond
       ((and (not head) (not tail))
        )
       ((and head (not tail))
        (stack-pushr stack head))
       ((CHAR= head #\;)
        (let ((lcls '()))
          (tagbody
           lcmtagain
             (cond
               ((and (not (is-lcmt-end)) (not tail))
                (pushr! lcls head)
                (advance-scanner))
               ((and (is-lcmt-end))
                (pushr! lcls #\Newline)
                (advance-scanner))
               ((not (is-lcmt-end))
                (pushr! lcls head)
                (advance-scanner)
                (go lcmtagain))
               )
             (stack-pushr stack lcls)
             (go again)
             )
        ))
       ((CHAR= head #\")
        (str-tagbody #\" dqagain)
        )
       ((and t)
        (stack-pushr stack head)
        (advance-scanner)
        (go again)
        )
       )
     )
  (stack-list stack)
  )

(defun cmt-str (char-ls)
  (cmt-str-aux (make-instance 'stack) (car char-ls) (cdr char-ls)))

(defun cl-cmt-str (char-ls)
  (cl-cmt-str-aux (make-instance 'stack) (car char-ls) (cdr char-ls)))

; When called this returns a list containing all chars from /* to */ and a new
; tail.
(defun bcmt-list (acc head tail)
  (let ((is-end (and (CHAR= head #\*) (CHAR= (car tail) #\/))))
    (cond
      ((and (not is-end) (not tail))
        (progn (list (pushr acc head) tail)))
      ((and is-end)
        (progn (list (pushr acc #\* #\/) (cdr tail))))
      ((not is-end)
        (progn (bcmt-list (pushr acc head) (car tail) (cdr tail)))))))

(defun lcmt-list (acc head tail)
  (let ((is-end (or (CHAR= head #\Newline))))
    (cond
      ((and (not is-end) (not tail))
       (progn (list (pushr acc head) tail)))
      ((and is-end)
       (progn (list (pushr acc #\Newline) (cdr tail))))
      ((not is-end)
       (progn (lcmt-list (pushr acc head) (car tail) (cdr tail)))))))

; Just like bcmt-list above, we return a pair. The string in list form and the
; new tail. We leave the escape-sequences intact.
(defmacro! str-list-body (quote fname)
  `(cond
     ((is-empty-str acc)
      (progn  (list acc (cons head tail))))
     ((and (CHAR/= head ,quote) (not tail))
      (progn (list (pushr acc head tail))))
     ((CHAR= head ,quote)
      (progn (list (pushr acc head) tail)))
     ((CHAR= head #\\) ; Add the escape to list, call self on thing after
      (let ((nacc (pushr acc head (car tail))))
        (progn (,fname nacc (cadr tail) (cddr tail)))))
     ((not (CHAR= head #\\))
      (progn (,fname (pushr acc head) (car tail) (cdr tail))))))

(defun dquote-str-list (acc head tail)
  (str-list-body #\" dquote-str-list))

(defun squote-str-list (acc head tail)
  (str-list-body #\' squote-str-list))

(defun is-comment (ls)
  (cond
    ((not (listp ls))
     nil)
    ((< (length ls) 2)
     nil)
    ((and (characterp (car ls)) (characterp (cadr ls))
          (or (and (CHAR= #\/ (car ls)) (CHAR= #\* (cadr ls)))
              (and (CHAR= #\/ (car ls)) (CHAR= #\/ (cadr ls)))))
     t)
  ))

(defun is-cl-comment (ls)
  (cond
    ((not (listp ls))
     nil)
    ((< (length ls) 2)
     nil)
    ((and (characterp (car ls)) (CHAR= #\; (car ls)))
     t)
    ))

(defun is-str (ls)
  (cond
    ((not (listp ls))
     nil)
    ((< (length ls) 2)
     nil)
    ((and (characterp (car ls)) (or (CHAR= #\" (car ls))
                                    (CHAR= #\' (car ls))))
     t)
    ))

(defun drop-str-quotes (ls)
  (progn
    (assert (is-str ls))
    (head-n (cdr ls) (- (length ls) 2))))

(defun undrop-str-quotes (ls)
  (pushr (pushl ls #\") #\"))



(defun is-paren-group (ls)
  (cond
    ((not (listp ls))
     nil)
    ((< (length ls) 2)
     nil)
    ((and (characterp (car ls)) (CHAR= #\( (car ls)))
     t)
    ))

(defun is-curly-group (ls)
  (cond
    ((not (listp ls))
     nil)
    ((< (length ls) 2)
     nil)
    ((and (characterp (car ls)) (CHAR= #\{ (car ls)))
     t)
    ))

(defun is-bracket-group (ls)
  (cond
    ((not (listp ls))
     nil)
    ((< (length ls) 2)
     nil)
    ((and (characterp (car ls)) (CHAR= #\[ (car ls)))
     t)
    ))

(defun is-nestable (ls)
  (and (listp ls) (or (is-paren-group ls) (is-curly-group ls) (is-bracket-group ls))))

(defun match-lists (l1 l2)
  (equal l1 l2))

(defun match-str-list (s ls)
  (equal (str-to-char-ls s) ls))

(defun is-ctl-struct (ls)
  (or (match-str-list "if" ls)
      (match-str-list "for" ls)
      (match-str-list "while" ls)
      (match-str-list "return" ls)))

(defun is-js-ctl-struct (ls)
  (or (is-ctl-struct ls)
      (match-str-list "try" ls)
      (match-str-list "catch" ls)))

(defun is-word-group (ls)
  (and (listp ls) (is-word-char (car ls)) (not (is-str ls))))

(defmacro! validate-x (name test)
  `(defun ,name (ls)
     (and (listp ls)
          (= 1 (length
                (remove-duplicates (map 'list #',test ls))))))
  )

(validate-x validate-word is-word-char)

(defun is-c-func-name (ls)
  (and (is-word-group ls) (not (is-ctl-struct ls))))

(defun is-js-func-name (ls)
  (and (is-word-group ls) (not (is-js-ctl-struct ls))))

(defun is-white-space-group (ls)
  (and (listp ls) ls (is-white-space (car ls))))

(validate-x validate-ws is-white-space)
(defun validate-ws (ls)
  (and (listp ls)
       (= 1 (length
             (remove-duplicates (map 'list
                                     #'is-white-space
                                     ls))))))
(defun is-blank-group (ls)
  (and (listp ls) ls (or (is-white-space-group (car ls))
                         (is-comment (car ls)))))

(validate-x validate-blank (lambda (e) (or (is-white-space-group e)
                                           (is-comment e))))
(defun is-punctuation-group (ls)
  (and (listp ls) ls (not (is-nestable ls)) (is-punctuation (car ls))))

;;; TODO need to identify nestables from inner context
(validate-x validate-punc (lambda (e) (and ;(not (is-nestable e))
                                           (is-punctuation e))))

(defun match-punc-ls (s ls)
  (and (is-punctuation-group ls)
       (equal (str-to-char-ls s) ls)))

(defun is-stmt-aux (ls count)
  (cond
    ((match-punc-ls ";" (car ls))
     count)
    ((and ls)
     (is-stmt-aux (cdr ls) (+ count 1)))
    ((and t)
     nil)
    ))

(defun is-stmt (ls)
  (is-stmt-aux ls 0))

(defun is-stmt-group (ls)
  (match-punc-ls ";" (last ls)))

(defun is-colon-group (ls)
  (and (is-punctuation-group ls) (= 1 (length ls)) (CHAR= #\: (car ls))))

(defun is-eq-group (ls)
  (and (is-punctuation-group ls) (= 1 (length ls)) (CHAR= #\= (car ls))))

(defun is-binding (ls)
  (or (is-colon-group ls) (is-eq-group ls)))

(defun is-nestable-end (c)
  (and (characterp c) (or (CHAR= c #\)) (CHAR= c #\}) (CHAR= c #\]))))

;; So the function call is over if we are the end of a list, at a punctuation
;; group, at a closing )}], or at a paren/bracket group. It is possible to do:
;;    foo()() and foo()[0]
;;
;; If foo returns a function or an array.
(defun is-fcall-end (ls)
  (or (not ls) (is-punctuation-group ls) (is-nestable-end ls)
      (or (is-paren-group ls) (is-bracket-group ls))
      ))

(defun is-js-fcall-end (ls)
  (if (or (not ls) (is-punctuation-group ls) (is-nestable-end ls)
          (or (is-paren-group ls) (is-bracket-group ls)))
      0
      nil
      ))

;; Same conditions for jsarr as for fcall.
(defun is-jsarr-end (ls)
  (is-fcall-end ls))

(defun is-json-value (ls)
  (or (is-word-group ls) (is-str ls) (is-nestable ls)))

(defun is-kvp-end (ls)
  (is-json-value ls))

(defun is-c-fdef-end (ls)
  (is-curly-group ls))

(defun is-fcall (ls)
  (and (listp ls)
       (cond
         ((= (length ls) 2)
          (and (is-c-func-name (car ls)) (is-paren-group (cadr ls))))
         ((= (length ls) 3)
          (and (is-c-func-name (car ls)) (is-blank-group (cadr ls))
               (is-paren-group (caddr ls))))
         ((and t)
          nil))))

(defun is-js-word-or-fcall-or-arr (ls)
  (or (is-js-fcall ls) (is-js-arr ls) (is-word-group ls))
  )

(defun is-js-word-or-arr (ls)
  (or (is-js-arr ls) (is-word-group ls))
  )

(defun is-dot (ls)
  (and (is-punctuation-group ls) (= 1 (length ls)) (CHAR= #\. (car ls)))
  )

(defun is-word-arr-fcall-vbind-fbind (ls)
  (or (is-js-fcall ls) (is-word-group ls) (is-js-arr ls)
      (is-js-var-binding ls) (is-js-fdef-binding ls))
  )

(defun is-jsarr (ls)
  (and (listp ls)
       (cond
         ((= (length ls) 2)
          (and (is-c-func-name (car ls)) (is-bracket-group (cadr ls))))
         ((= (length ls) 3)
          (and (is-c-func-name (car ls)) (is-blank-group (cadr ls))
               (is-bracket-group (caddr ls))))
         ((and t)
          nil))))

(defun is-op (ls)
  ; Use CHAR= instead of match-str to deal with empty statements `;;...`
  (and (is-punctuation-group ls) (and (not (CHAR= (car ls) #\;))
                                      (not (CHAR= (car ls) #\,)))))

(defun c-what-is (ls)
  (cond
    ((is-op ls)
     'operator)
    ((is-c-fdef ls)
     'function-definition)
    ((is-fcall ls)
     'function-call)
    ((is-jsarr ls)
     'js-array)
    ((is-word-group ls)
     'word)
    ((is-comment ls)
     'comment)
    ((is-str ls)
     'string)
    ((is-punctuation-group ls)
     'punctuation)
    ((is-blank-group ls)
     'blank)
    ((is-white-space-group ls)
     'whitespace)
    ((characterp ls)
     'character)
    ((is-bracket-group ls)
     'brackets)
    ((is-paren-group ls)
     'parens)
    ((is-curly-group ls)
     'curlies)
    ((and t)
     'other)
    ))

(defun js-what-is (ls)
  (cond
    ((is-op ls)
     'operator)
    ((is-js-fdef ls)
     'function-definition)
    ((is-js-fcall ls)
     'function-call)
    ((is-jsarr ls)
     'js-array)
    ((is-word-group ls)
     'word)
    ((is-comment ls)
     'comment)
    ((is-str ls)
     'string)
    ((is-punctuation-group ls)
     'punctuation)
    ((is-blank-group ls)
     'blank)
    ((is-white-space-group ls)
     'whitespace)
    ((characterp ls)
     'character)
    ((is-bracket-group ls)
     'brackets)
    ((is-paren-group ls)
     'parens)
    ((is-curly-group ls)
     'curlies)
    ((and t)
     'other)
    ))


(defun fcall-name-eq (fcall name)
  (and (is-js-fcall fcall) (match-str-list name (get-fcall-name fcall))))

(defun is-c-fdef (ls)
  (and (listp ls)
       (cond
         ((= (length ls) 3)
          (and (is-c-func-name (car ls)) (is-paren-group (cadr ls))
               (is-curly-group (caddr ls))))
          ((= (length ls) 4)
           (and (is-c-func-name (car ls))
                (or (and (is-paren-group (cadr ls))
                         (is-blank-group (caddr ls))
                         (is-curly-group (cadddr ls)))
                    (and (is-blank-group (cadr ls))
                         (is-paren-group (caddr ls))
                         (is-curly-group (cadddr ls))))))
         ((= (length ls) 5)
          (and (is-c-func-name (car ls))
               (is-blank-group (cadr ls))
               (is-paren-group (caddr ls))
               (is-blank-group (cadddr ls))
               (is-curly-group (car (cddddr ls)))))
          ((and t)
           nil))))

(defun is-js-fdef (ls)
  (and ls (listp ls) (js-fdefp (car ls) (cdr ls))))

(defun is-js-curly-ctl-stmt (ls)
  (and ls (listp ls) (js-curly-ctl-stmtp (car ls) (cdr ls))))

(defun is-js-do-while-stmt (ls)
  (and ls (listp ls) (js-do-while-stmtp (car ls) (cdr ls))))

(defun is-ctl-word (ls)
  (or (match-str-list "if" ls)
      (match-str-list "while" ls)
      (match-str-list "for" ls)
      (match-str-list "switch" ls)))

(defun is-js-flat-ctl-stmt (ls)
  (and ls (listp ls) (not (is-js-curly-ctl-stmt ls)) (is-ctl-word (car ls))))

(defun is-js-any-ctl-stmt (ls)
  (or (is-js-curly-ctl-stmt ls) (is-js-flat-ctl-stmt ls)))

(defun is-js-fdef-binding (ls)
  (and ls (listp ls) (js-fdef-bindingp (car ls) (cdr ls))))

(defun is-js-var-binding (ls)
  (and ls (listp ls) (not (is-js-fdef-binding ls))
       (or (is-word-group (car ls)) (is-js-arr (car ls)))
       (or (is-eq-group (cadr ls)) (is-eq-group (caddr ls)))))

(defun is-js-binding (ls)
  (and ls (listp ls)
       (or (is-js-fdef-binding ls)
           (is-js-var-binding ls))))

(defun is-js-fcall (ls)
  (and (listp ls)
       (not (is-js-fdef ls))
       (js-fcallp (car ls) (cdr ls))))

(defun is-js-arr (ls)
  (and (listp ls)
       (js-arrp (car ls) (cdr ls))))

(defun is-c-fcall (ls)
  (and (listp ls)
       (is-c-func-name (car ls)) (is-fcall ls)))

(defun is-c-fdef-or-fcall (ls)
  (or (is-c-fdef ls) (is-c-fcall ls)))

(defun is-js-fdef-or-fcall (ls)
  (or (is-js-fdef ls) (is-js-fcall ls)))

(defun is-js-indexable-funcs (ls)
  (or (is-js-fdef ls) (is-js-fcall ls) (is-js-fdef-binding ls)))

(defun is-js-indexable-conds (ls)
  (or (is-js-indexable-funcs ls)
      (is-js-any-ctl-stmt ls) (is-js-do-while-stmt ls)))



(defmacro! def-5-state-match (fname test-slot-1 test-slot-2-opt test-slot-2
                                        test-slot-3-opt test-slot-3)
  `(defun ,fname (head tail s)
     (case s
       (0 (if (,test-slot-1 head) (,fname (car tail) (cdr tail) 1) nil))
       (1 (cond
            ((,test-slot-2-opt head) (,fname (car tail) (cdr tail)  2))
            ((,test-slot-2  head) (,fname (car tail) (cdr tail) 3))
            ((and t) nil)))
       (2 (cond
            ((,test-slot-2 head) (,fname (car tail) (cdr tail) 3))
            ((and t) nil)))
       (3 (cond
         ((,test-slot-3-opt head) (,fname (car tail) (cdr tail) 4))
         ((,test-slot-3 head) (,fname (car tail) (cdr tail) 5))
         ((and t) nil)))
       (4 (cond
         ((,test-slot-3 head) (and t))
         ((and t) nil)))
       (5 (and t)))))

(def-5-state-match fcallp is-js-func-name is-blank-group is-paren-group
  is-blank-group is-fcall-end)

(def-5-state-match fdefp is-js-func-name is-blank-group is-paren-group
  is-blank-group is-c-fdef-end)

;(def-5-state-match js-fdefp is-js-func-name is-blank-group is-paren-group
;  is-blank-group is-c-fdef-end)

(def-5-state-match jsarrp is-js-func-name is-blank-group is-bracket-group
  is-blank-group is-jsarr-end)

(def-5-state-match json-kv-pair-p is-str is-blank-group is-colon-group
  is-blank-group is-kvp-end)

(defun count-n-wsgroup (n ls)
  (let ((nls (head-n ls n)))
    (count-if #'is-white-space-group nls)))

(defun count-n-blankgroup (n ls)
  (let ((nls (head-n ls n)))
    (count-if #'is-blank-group nls)))

;;; Function-defining macro. The macro does following:
;;; Groups something like an fcall, jsarr, or kvp starting at head. Returns a tuple of:
;;;     (grouped-$thing, new-head, new-tail)
(defmacro! group-2-to-3 (name fletnm fletif fletcall)
  `(defun ,name (head tail agg)
     (let ((ws2 (count-n-blankgroup 2 (pushl tail head)))
           (ws3 (count-n-blankgroup 3 (pushl tail head))))
       (flet ((,fletnm (x)
                (if (,fletif x) (,fletcall x) x)))
         (cond
           ((= ws2 0)
            (list (map 'list #',fletnm (head-n (pushl tail head) 2))
                  (car (popl-n tail 1))
                  (popl-n tail 2)))
           ((= ws3 1)
            (list (map 'list #',fletnm (head-n (pushl tail head) 3))
                  (car (popl-n tail 2))
                  (popl-n tail 3)))
           ((and t)
            (assert (and nil)))
           )))))

(group-2-to-3 group-fcall fcalls-if-group is-paren-group fcalls)
(group-2-to-3 group-jsarr jsarrs-if-group is-bracket-group jsarrs)


(defmacro! group-3-to-5 (name fletnm fletif fletcall)
  `(defun ,name (head tail agg)
     (let ((ws3 (count-n-blankgroup 3 (pushl tail head)))
           (ws4 (count-n-blankgroup 4 (pushl tail head)))
           (ws5 (count-n-blankgroup 5 (pushl tail head)))
           )
       (flet ((,fletnm (x)
                (if (,fletif x) (,fletcall x) x)))
         (cond
           ((= ws3 0)
            (list (map 'list #',fletnm (head-n (pushl tail head) 3))
                  (car (popl-n tail 2))
                  (popl-n tail 3)))
           ((= ws4 1)
            (list (map 'list #',fletnm (head-n (pushl tail head) 4))
                  (car (popl-n tail 3))
                  (popl-n tail 4)))
           ((= ws5 2)
            (list (map 'list #',fletnm (head-n (pushl tail head) 5))
                  (car (popl-n tail 4))
                  (popl-n tail 5)))
           ((and t)
                                        ;Never get here
            (assert (and nil)))
           )))))

(group-3-to-5 group-fdef fdefs-if-group is-nestable c-fdefs)
(group-3-to-5 group-json-kvp kvp-if-group is-nestable json-kvp)

(defun get-c-fdef-triple (head tail agg)
  (group-fdef head tail agg))


; Walk the list. Call itself on every is-nestable list along the way.
(defun fcalls-aux (acc head tail)
  (cond
    ((fcallp head tail 0)
     (let* ((agg (group-fcall head tail '()))
            (nacc (pushr acc (car agg)))
            (nhead (cadr agg))
            (ntail (caddr agg)))
       (progn  (fcalls-aux nacc nhead ntail))))

     ((and (listp head) (is-nestable head))
      (progn
             (let ((nacc (pushr acc (fcalls head)))
                   (nhead (car tail))
                   (ntail (cdr tail)))
               (fcalls-aux nacc nhead ntail))))
     ((and (characterp head) tail)
      (progn  (fcalls-aux (pushr acc head) (car tail) (cdr tail))))
     ((and tail)
      (progn  (fcalls-aux (pushr acc head) (car tail) (cdr tail))))
     ((and head (not tail))
      (progn  (pushr acc head)))
     ((and (not head))
      (progn  acc))
     ))


; Function call: name pgroup [ws] [semi | comma | end-group | operator]
(defun fcalls (ls)
  (fcalls-aux '() (car ls) (cdr ls)))

(defun xform-fcall-pgroup (fc cb)
  (if (= (length fc) 2)
      (list (car fc) (funcall cb (cadr fc)))
      (list (car fc) (cadr fc)  (funcall cb (caddr fc)))
  ))

(defun xform-js-arr-bgroup (fc cb)
  (if (= (length fc) 2)
      (list (car fc) (funcall cb (cadr fc)))
      (list (car fc) (cadr fc)  (funcall cb (caddr fc)))
      ))

(defun xform-c-fdef-pgroup (fd cb)
  (cond
    ((= (length fd) 3)
     (list (car fd) (funcall cb (cadr fd)) (caddr fd)))
    ((and (= (length fd) 4) (is-blank-group (cadr fd)))
     (list (car fd) (cadr fd) (funcall cb (caddr fd)) (funcall cb (cadddr fd))))
    ((and (= (length fd) 4) (is-blank-group (caddr fd)))
     (list (car fd) (funcall cb (cadr fd)) (caddr fd) (funcall cb (cadddr fd))))
    ((= (length fd) 5)
     (list (car fd) (cadr fd) (funcall cb (caddr fd)) (cadddr fd) (car (cddddr fd))))))

(defun xform-c-fdef-cgroup (fd cb)
  (cond
    ((= (length fd) 3)
      (list (car fd) (cadr fd) (funcall cb (caddr fd))))
    ((= (length fd) 4)
     (list (car fd) (cadr fd) (caddr fd) (funcall cb (cadddr fd))))
    ((= (length fd) 5)
     (list (car fd) (cadr fd) (caddr fd) (cadddr fd) (funcall cb (car (cddddr fd)))))))

(defun xform-js-fdef-cgroup-aux (acc head tail cb)
  (cond
    ((not head)
     acc)
    ((is-curly-group head)
     (xform-js-fdef-cgroup-aux
      (pushr acc (funcall cb head)) (car tail) (cdr tail) cb))
    ((not (is-curly-group head))
     (xform-js-fdef-cgroup-aux
      (pushr acc head) (car tail) (cdr tail) cb))
    ))

(defun xform-js-vbind-rval-aux (acc head tail cb)
  (cond
    ((not head)
     acc)
    ((is-eq-group head)
     (xform-js-vbind-rval-aux
      (pushr acc head  (funcall cb tail)) (cadr tail) (cddr tail) cb))
    ((not (is-eq-group head))
     (xform-js-vbind-rval-aux
      (pushr acc head) (car tail) (cdr tail) cb))
    ))

(defun xform-js-curly-ctl-stmt-cgroup-aux (acc head tail cb)
  (cond
    ((not head)
     acc)
    ((is-curly-group head)
     (xform-js-curly-ctl-stmt-cgroup-aux
      (pushr acc (funcall cb head)) (car tail) (cdr tail) cb))
    ((not (is-curly-group head))
     (xform-js-curly-ctl-stmt-cgroup-aux
      (pushr acc head) (car tail) (cdr tail) cb))
    ))

(defun xform-js-do-while-cgroup-aux (acc head tail cb)
  (cond
    ((not head)
     acc)
    ((is-curly-group head)
     (xform-js-do-while-cgroup-aux
      (pushr acc (funcall cb head)) (car tail) (cdr tail) cb))
    ((not (is-curly-group head))
     (xform-js-do-while-cgroup-aux
      (pushr acc head) (car tail) (cdr tail) cb))
    ))

(defun xform-js-fdef-pgroup-aux (acc head tail cb)
  (cond
    ((not head)
     acc)
    ((is-paren-group head)
     (xform-js-fdef-pgroup-aux
      (pushr acc (funcall cb head)) (car tail) (cdr tail) cb))
    ((not (is-curly-group head))
     (xform-js-fdef-pgroup-aux
      (pushr acc head) (car tail) (cdr tail) cb))
    ))

(defun xform-js-fdef-cgroup (fd cb)
  (xform-js-fdef-cgroup-aux '() (car fd) (cdr fd) cb))

(defun xform-js-vbind-rval (fd cb)
  (xform-js-vbind-rval-aux '() (car fd) (cdr fd) cb))

(defun xform-js-fdef-pgroup (fd cb)
  (xform-js-fdef-pgroup-aux '() (car fd) (cdr fd) cb))

(defun xform-js-curly-ctl-stmt-cgroup (cstmt cb)
  (xform-js-curly-ctl-stmt-cgroup-aux '() (car cstmt) (cdr cstmt) cb))

(defun xform-js-do-while-cgroup (cstmt cb)
  (xform-js-do-while-cgroup-aux '() (car cstmt) (cdr cstmt) cb))

(defun c-fdefs-aux (acc head tail)
  (cond
    ((fdefp head tail 0)
     (let* ((agg (get-c-fdef-triple head tail '()))
            (nacc (pushr acc (car agg)))
            (nhead (cadr agg))
            (ntail (caddr agg)))
       (progn  (c-fdefs-aux nacc nhead ntail))))
    ((is-fcall head)
         (let* ((nacc (pushr acc (xform-fcall-pgroup head #'c-fdefs))))
           (c-fdefs-aux nacc (car tail) (cdr tail))))
    ((and (listp head) (is-nestable head))
     (let ((nacc (pushr acc (c-fdefs head))))
       (progn (c-fdefs-aux nacc (car tail) (cdr tail)))))
    ((or (and (characterp head) tail) tail)
     (progn  (c-fdefs-aux (pushr acc head) (car tail) (cdr tail))))
    ((and head (not tail))
     (progn  (pushr acc head)))
    ((and (not head))
     (progn  acc))
    ))

; Function def: name pgroup cgroup [name != if | for | while]
(defun c-fdefs (ls)
  (c-fdefs-aux '() (car ls) (cdr ls)))

(defun finite-match-aux (head tail match-fns count)
  (let* ((pair (car match-fns))
         (type (car pair))
         (func (cadr pair))
         (match (if (and func) (funcall func head) nil))
         (cmatch (if (and func) (funcall func (cons head tail)) nil))
         (cdroff cmatch)
         (caroff cdroff)
         )
    (cond
      ((not match-fns)
       count)
      ((and (equal type :m) (not match))
       nil)
      ((and (equal type :mc) (not cmatch))
       nil)
      ((and (equal type :m) match)
       (finite-match-aux (car tail) (cdr tail) (cdr match-fns) (+ count 1)))
      ((and (equal type :mc) cmatch)
       (finite-match-aux (nth caroff tail) (nthcdr cdroff tail) (cdr match-fns)
                         (+ count cmatch)))
      ((and (equal type :o) match)
       (finite-match-aux (car tail) (cdr tail) (cdr match-fns) (+ count 1)))
      ((and (equal type :oc) cmatch)
       (finite-match-aux (nth caroff tail) (nthcdr cdroff tail) (cdr match-fns)
                         (+ count cmatch)))
      ((and (equal type :o) (not match))
       (finite-match-aux head tail (cdr match-fns) count))
      ((and (equal type :oc) (not cmatch))
       (finite-match-aux head tail (cdr match-fns) count))
      ((and t) ;should never get here
       (assert nil))
    )))

(defun finite-match (ls match-fns)
  (finite-match-aux (car ls) (cdr ls) match-fns 0))

(defun finite-repeating-match (ls rmatch-fns ematch-fns)
  "Just like finite-match, except we match rmatch-fns in a loop, until it
  returns nil. We then run ematch-fns on the part that rmatch failed to match.
  If rmatch succeeds, this function returns t, otherwise nil"
  (let ((curls ls)
        (total_size 0)
        (current_rsize 0)
        (current_esize 0)
        (rmatched 0))
    (tagbody again
       (setf current_rsize (finite-match curls rmatch-fns))
       (cond
         ((and current_rsize)
          (incf total_size current_rsize)
          (setf curls (nthcdr current_rsize curls))
          (incf rmatched)
          (go again))
         ((not current_rsize)
          (setf current_esize (finite-match curls ematch-fns))
          (if (or (not current_esize) (= rmatched 0))
              (setf total_size nil)
              (incf total_size current_esize)
              )))
       )
    total_size
    )
  )

(defun js-else-if-stmtp (head tail)
      (finite-match (cons head tail)
                         (list
                          (list :m #'(lambda (x) (match-str-list "else" x)))
                          (list :o #'is-blank-group)
                          (list :m #'is-js-any-ctl-stmt)
                          )))

(defun js-curly-else-stmtp (head tail)
  (finite-match (cons head tail)
                (list
                 (list :m #'(lambda (x) (match-str-list "else" x)))
                 (list :o #'is-blank-group)
                 (list :m #'is-js-any-ctl-stmt)
                 (list :o #'is-blank-group)
                 (list :m #'is-curly-group)
                 )))

(defun js-curly-ctl-stmtp (head tail)
  (finite-match (cons head tail)
                (list
                 (list :m #'(lambda (x) (or (match-str-list "if" x)
                                            (match-str-list "while" x)
                                            (match-str-list "for" x)
                                            (match-str-list "switch" x))))
                 (list :o #'is-blank-group)
                 (list :m #'is-paren-group)
                 (list :o #'is-blank-group)
                 (list :m #'is-curly-group))))

(defun js-flat-ctl-stmtp (head tail)
  (finite-match (cons head tail)
                (list
                 (list :m #'(lambda (x) (or (match-str-list "if" x)
                                            (match-str-list "while" x)
                                            (match-str-list "for" x)
                                            (match-str-list "switch" x))))
                 (list :o #'is-blank-group)
                 (list :m #'is-paren-group)
                 (list :o #'is-blank-group)
                 (list :mc #'is-stmt))))

(defun js-do-while-stmtp (head tail)
  (finite-match (cons head tail)
                (list
                 (list :m #'(lambda (x) (match-str-list "do" x)))
                 (list :o #'is-blank-group)
                 (list :m #'is-curly-group)
                 (list :o #'is-blank-group)
                 (list :m #'(lambda (x) (match-str-list "while" x)))
                 (list :o #'is-blank-group)
                 (list :m #'is-paren-group)
                 (list :o #'is-blank-group)
                 )))

(defun js-fdefp (head tail)
  (finite-match (cons head tail)
                (list
                 (list :m #'(lambda (x) (match-str-list "function" x)))
                 (list :o #'is-blank-group)
                 (list :o #'is-js-func-name)
                 (list :o #'is-blank-group)
                 (list :m #'is-paren-group)
                 (list :o #'is-blank-group)
                 (list :m #'is-c-fdef-end)
                 )
                )
  )

(defun js-fcallp (head tail)
  (finite-match (cons head tail)
                (list
                 (list :m #'is-js-func-name)
                 (list :o #'is-blank-group)
                 (list :m #'is-paren-group)
                 )
                )
  )

(defun js-arrp (head tail)
  (finite-match (cons head tail)
                (list
                 (list :m #'is-js-func-name)
                 (list :o #'is-blank-group)
                 (list :m #'is-bracket-group)
                 )
                )
  )

(defun group-js-fdef (head tail size)
  (list (map
         'list
         #'(lambda (x) (if (is-nestable x) (js-fdefs x) x))
         (head-n (cons head tail) size))
        (car (popl-n tail (- size 1)))
        (popl-n tail size)
  ))

(defun group-js-fcall (head tail size)
  (list (map
         'list
         #'(lambda (x) (if (is-nestable x) (js-fcalls x) x))
         (head-n (cons head tail) size))
        (car (popl-n tail (- size 1)))
        (popl-n tail size)
        ))

(defun group-js-arr (head tail size)
  (list (map
         'list
         #'(lambda (x) (if (is-nestable x) (js-arrs x) x))
         (head-n (cons head tail) size))
        (car (popl-n tail (- size 1)))
        (popl-n tail size)
        ))

(defun group-js-do-while-stmt (head tail size)
  (list (map
         'list
         #'(lambda (x) (if (is-nestable x) (js-do-while-stmts x) x))
         (head-n (cons head tail) size))
        (car (popl-n tail (- size 1)))
        (popl-n tail size)))

(defun first-pgroup-pos-aux (head tail count)
  (cond
    ((is-paren-group head)
     count)
    ((not (is-paren-group head))
     (first-pgroup-pos-aux (car tail) (cdr tail) (+ count 1)))
    ((not head)
     nil)))

(defun first-pgroup-pos (ls)
  (first-pgroup-pos-aux (car ls) (cdr ls) 0))

(defun group-js-flat-ctl-stmt (head tail size)
  (let* ((ls (head-n (cons head tail) size))
         (pos (first-pgroup-pos ls))
         (blank-after (is-blank-group (nth (+ pos 1) ls)))
         (n-cdr (if (and blank-after) (+ pos 2) (+ pos 1)))
         (tail-ls (nthcdr n-cdr ls))
         (tail-group (js-flat-ctl-stmts tail-ls))
         )
    (list (append (head-n ls n-cdr) tail-group)
          (car (popl-n tail (- size 1)))
          (popl-n tail size))))

(defmacro! group-and-continue (parent grpr grp-sz)
  `(let* ((agg (,grpr head tail ,grp-sz))
          (nacc (pushr acc (car agg)))
          (nhead (cadr agg))
          (ntail (caddr agg)))
     (,parent nacc nhead ntail)))

(defmacro! xform-and-continue (parent xformer cb)
  `(let* ((nacc (pushr acc (,xformer head #',cb))))
    (,parent nacc (car tail) (cdr tail))))

(defmacro! descend-and-continue (grand-parent parent)
  `(let ((nacc (pushr acc (,grand-parent head))))
    (,parent nacc (car tail) (cdr tail))))


(defmacro! pipeline-until-lambda (test body)
  `(function (lambda (q)
    (if ,test
        ,body
        nil)))
  )

(defmacro! js-uber-aux-impl (self-parent self matcher grouper &key fcall fdef fdef-bind
                                         curly-ctl do-while flat-ctl arr nestable
                                         var-bind)
  `(defun ,self (acc head tail)
     (let ((sz (,matcher head tail)))
       (pipeline-until nil
                      (pipeline-until-lambda (and sz)
                        (group-and-continue ,self ,grouper sz))
                      (pipeline-until-lambda (is-js-fcall head) ,fcall)
                      (pipeline-until-lambda (is-js-arr head) ,arr)
                      (pipeline-until-lambda (is-js-fdef head) ,fdef)
                      (pipeline-until-lambda (is-js-fdef-binding head) ,fdef-bind)
                      (pipeline-until-lambda (is-js-var-binding head) ,var-bind)
                      (pipeline-until-lambda (is-js-curly-ctl-stmt head) ,curly-ctl)
                      (pipeline-until-lambda (is-js-do-while-stmt head) ,do-while)
                      (pipeline-until-lambda (is-js-flat-ctl-stmt head) ,flat-ctl)
                      (pipeline-until-lambda (is-nestable head) ,nestable)
                      (pipeline-until-lambda (or (and (characterp head) tail) tail)
                                            (,self (pushr acc head) (car tail) (cdr tail)))
                      (pipeline-until-lambda (and head (not tail)) (pushr acc head))
                      (pipeline-until-lambda (and (not tail)) acc))
     )
  )
  )

(js-uber-aux-impl js-fdefs js-fdefs-aux js-fdefp group-js-fdef
                  ;:fcall (xform-and-continue js-fdefs-aux xform-fcall-pgroup js-fdefs)
                  :nestable (descend-and-continue js-fdefs js-fdefs-aux))

(js-uber-aux-impl js-fcalls js-fcalls-aux js-fcallp group-js-fcall
                  :fdef (xform-and-continue js-fcalls-aux xform-js-fdef-cgroup js-fcalls)
                  :nestable (descend-and-continue js-fcalls js-fcalls-aux))


(js-uber-aux-impl js-arrs js-arrs-aux js-arrp group-js-arr
                  :fdef (xform-and-continue js-arrs-aux xform-js-fdef-cgroup js-arrs)
                  :fcall (xform-and-continue js-arrs-aux xform-fcall-pgroup js-arrs)
                  :nestable (descend-and-continue js-arrs js-arrs-aux))


(js-uber-aux-impl js-var-bindings js-var-bindings-aux js-var-bindingp
                  group-js-var-binding
                  :fdef (xform-and-continue js-var-bindings-aux xform-js-fdef-cgroup
                                            js-var-bindings)
                  :nestable (descend-and-continue js-var-bindings
                                                  js-var-bindings-aux))

(js-uber-aux-impl js-mbr-chain js-mbr-chain-aux js-mbr-chainp group-js-mbr-chain
                  :fcall (xform-and-continue js-mbr-chain-aux xform-fcall-pgroup
                                             js-mbr-chain)
                  :arr (xform-and-continue js-mbr-chain-aux xform-js-arr-bgroup js-mbr-chain)
                  :fdef (xform-and-continue js-mbr-chain-aux xform-js-fdef-cgroup
                                            js-mbr-chain)
                  ;XXX descend or xform???
                  ;:var-bind (xform-and-continue js-mbr-chain-aux xform-js-vbind-rval js-mbr-chain)
                  :var-bind (descend-and-continue js-mbr-chain js-mbr-chain-aux)
                  :fdef-bind (descend-and-continue js-mbr-chain
                                                  js-mbr-chain-aux)
                  :nestable (descend-and-continue js-mbr-chain
                                                  js-mbr-chain-aux))

(js-uber-aux-impl js-curly-ctl-stmts js-curly-ctl-stmts-aux js-curly-ctl-stmtp
                  group-js-curly-ctl-stmt
                  :fdef (xform-and-continue js-curly-ctl-stmts-aux xform-js-fdef-cgroup
                                      js-curly-ctl-stmts)
                  :fcall (xform-and-continue js-curly-ctl-stmts-aux
                          xform-fcall-pgroup js-curly-ctl-stmts)
                  :nestable (js-curly-ctl-stmts-aux (pushr acc head) (car tail) (cdr tail)))

(js-uber-aux-impl js-do-while-stmts js-do-while-stmts-aux js-do-while-stmtp
                  group-js-do-while-stmt
                  :fdef (xform-and-continue js-do-while-stmts-aux xform-js-fdef-cgroup
                                      js-do-while-stmts)
                  :fcall (xform-and-continue js-do-while-stmts-aux
                          xform-fcall-pgroup js-do-while-stmts)
                  :curly-ctl (xform-and-continue js-do-while-stmts-aux
                          xform-js-curly-ctl-stmt-cgroup js-do-while-stmts)
                  :nestable (js-do-while-stmts-aux (pushr acc head) (car tail) (cdr tail)))

(js-uber-aux-impl js-flat-ctl-stmts js-flat-ctl-stmts-aux js-flat-ctl-stmtp
                  group-js-flat-ctl-stmt
                  :fdef (xform-and-continue js-flat-ctl-stmts-aux xform-js-fdef-cgroup
                                            js-flat-ctl-stmts)
                  :fcall (xform-and-continue js-flat-ctl-stmts-aux xform-fcall-pgroup
                                             js-flat-ctl-stmts)
                  :curly-ctl (xform-and-continue js-flat-ctl-stmts-aux
                                                 xform-js-curly-ctl-stmt-cgroup
                                                 js-flat-ctl-stmts)
                  :do-while (xform-and-continue js-flat-ctl-stmts-aux xform-js-do-while-cgroup
                                                js-flat-ctl-stmts))

; Function def: 'function' name pgroup cgroup [name != if | for | while]
(defun js-fdefs (ls)
  (js-fdefs-aux '() (car ls) (cdr ls)))

(defun js-fcalls (ls)
  (js-fcalls-aux '() (car ls) (cdr ls)))

(defun js-arrs (ls)
  (js-arrs-aux '() (car ls) (cdr ls)))

(defun js-fdef-bindingp (head tail)
  (finite-match (cons head tail)
                (list
                 (list :m #'is-word-group)
                 (list :o #'is-blank-group)
                 (list :m #'is-eq-group)
                 (list :o #'is-blank-group)
                 (list :m #'is-js-fdef)
                 )
                ))

(defun js-var-bindingp (head tail)
  (finite-match (cons head tail)
                (list
                 (list :m #'is-js-word-or-arr)
                 (list :o #'is-blank-group)
                 (list :m #'is-eq-group)
                 (list :o #'is-blank-group)
                 (list :mc #'is-stmt)
                 )
                ))

(defun js-mbr-chainp (head tail)
  (finite-repeating-match (cons head tail)
       (list
        (list :m #'is-js-word-or-fcall-or-arr)
        (list :o #'is-blank-group)
        (list :m #'is-dot)
        (list :o #'is-blank-group))
       (list
        (list :m #'is-word-arr-fcall-vbind-fbind))))

(defun js-var-bindings (ls)
  (js-var-bindings-aux '() (car ls) (cdr ls)))

(defun js-mbr-chain (ls)
  (js-mbr-chain-aux '() (car ls) (cdr ls)))

(defun js-obj-lit (ls)
  (js-obj-lit-aux '() (car ls) (cdr ls)))


(defun group-js-curly-ctl-stmt (head tail size)
  (list (map
         'list
         #'(lambda (x) (if (is-nestable x) (js-curly-ctl-stmts x) x))
         (head-n (cons head tail) size))
        (car (popl-n tail (- size 1)))
        (popl-n tail size)))

(defun group-js-var-binding (head tail size)
  (list (map
        'list
        #'(lambda (x)  (if (is-js-fdef x) (js-var-bindings x) x))
        (head-n (cons head tail) size))
        (car (popl-n tail (- size 1)))
        (popl-n tail size)))

(defun group-js-mbr-chain (head tail size)
  (list (map
         'list
         #'(lambda (x)
             (if (or (is-js-fdef-binding x) (is-js-fcall x)
                     (is-js-var-binding x) (is-js-arr x)
                     )
                 (js-mbr-chain x)
                 x))
         (head-n (cons head tail) size))
        (car (popl-n tail (- size 1)))
        (popl-n tail size)))

(defun group-js-mbr-chain2 (head tail size)
  (list (map
         'list
         #'(lambda (x)
             (if (or (is-js-fdef-binding x) (is-js-fcall x))
                 (js-mbr-chain x)
                 x))
         (head-n (cons head tail) size))
        (car (popl-n tail (- size 1)))
        (popl-n tail size)))

(defun js-curly-ctl-stmts (ls)
  (js-curly-ctl-stmts-aux '() (car ls) (cdr ls)))

(defun js-do-while-stmts-aux (acc head tail)
  (let ((sz (js-do-while-stmtp head tail)))
    (cond
      ((and sz)
       (group-and-continue js-do-while-stmts-aux group-js-do-while-stmt sz))
      ((is-js-fdef head)
       (xform-and-continue js-do-while-stmts-aux xform-js-fdef-cgroup js-do-while-stmts))
      ((is-js-fcall head)
       (xform-and-continue js-do-while-stmts-aux xform-fcall-pgroup js-do-while-stmts))
      ((is-js-curly-ctl-stmt head)
       (xform-and-continue js-do-while-stmts-aux
                           xform-js-curly-ctl-stmt-cgroup js-do-while-stmts))
      ((and (listp head) (is-nestable head))
       (descend-and-continue js-do-while-stmts js-do-while-stmts-aux))
      ((or (and (characterp head) tail) tail)
       (js-do-while-stmts-aux (pushr acc head) (car tail) (cdr tail)))
      ((and head (not tail))
       (pushr acc head))
      ((and (not head))
       acc))))

(defun js-do-while-stmts (ls)
  (js-do-while-stmts-aux '() (car ls) (cdr ls)))

(defun js-flat-ctl-stmts (ls)
  (js-flat-ctl-stmts-aux '() (car ls) (cdr ls)))

(defun jsarrs-aux (acc head tail)
  (cond
    ((jsarrp head tail 0)
     (let* ((agg (group-jsarr head tail '()))
            (nacc (pushr acc (car agg)))
            (nhead (cadr agg))
            (ntail (caddr agg)))
       (progn  (jsarrs-aux nacc nhead ntail))))
    ((is-c-fdef head)
     (let* ((nacc (pushr acc (xform-js-fdef-cgroup
                              (xform-js-fdef-pgroup head #'jsarrs) #'jsarrs))))
       (progn (jsarrs-aux nacc (car tail) (cdr tail)))))
    ((is-fcall head)
     (let* ((nacc (pushr acc (xform-fcall-pgroup head #'jsarrs))))
       (progn  (jsarrs-aux nacc (car tail) (cdr tail)))))
    ((and (listp head) (is-nestable head))
     (let ((nacc (pushr acc (jsarrs head))))
       (progn (jsarrs-aux nacc (car tail) (cdr tail)))))
    ((or (and (characterp head) tail) tail)
     (progn (jsarrs-aux (pushr acc head) (car tail) (cdr tail))))
    ((and head (not tail))
     (progn  (pushr acc head)))
    ((and (not head))
     (progn  acc))
    ))


;;; Match array-accesses. Should be similar to fcalls.
(defun jsarrs (ls)
  (jsarrs-aux '() (car ls) (cdr ls)))

(defun js-vars (ast)
  ast)

;;; Match object-literal bindingr (assignments using `:`)
;;; Note that we don't want to accidentally match the `:` of the ternary
;;; `?:` operator.
;;; Obviously, if the l-val is not a word (and is not true or false) we can
;;; ignore it. If it is a word,
(defun obj-bindings (ls)
  )


;;; Match json K-V pairs.
;;;
;;; word [ws] colon [ws] word | string | group
;;; XXX this looks like a 5-state match
;;; Yes. But extraction does not look like fcall or jsarr. But more like fdef
;;; (we keep the last elem)
(defun json-kvp-aux (acc head tail)
  (cond
    ((json-kv-pair-p head tail 0)
     (let* ((agg (group-json-kvp head tail '()))
            (nacc (pushr acc (car agg)))
            (nhead (cadr agg))
            (ntail (caddr agg)))
       (progn (json-kvp-aux nacc nhead ntail))))
    ((is-nestable head)
     (let ((nacc (pushr acc (json-kvp head))))
       (progn (json-kvp-aux nacc (car tail) (cdr tail)))))
    ((and (not head))
     (progn acc))
    ((and head (not tail))
     (progn (pushr acc head)))
    ((and head tail)
     (progn (json-kvp-aux (pushr acc head) (car tail) (cdr tail))))
  ))

(defun json-kvp (ls)
  (json-kvp-aux '() (car ls) (cdr ls)))

(defun jsontest ()
  (let ((input (str-to-char-ls "\"A\" : \"B\", \"C\" : \"D\"")))
    (json-to-ast input)))


(defun walk-tree-until (ls test work until path)
  (if (funcall test (car ls))
      (funcall work (car ls) path))
  (if (funcall until (car ls))
      (return-from walk-tree-until))
  (if (and (car ls) (listp (car ls)))
      (walk-tree-until (car ls) test work until (pushr path (car ls))))
  (if (and (cdr ls))
      (walk-tree-until (cdr ls) test work until path)))

(defun walk-tree (ls test work path walk)
  (if (funcall test (car ls))
      (funcall work (car ls) path walk))
  (if (and (car ls) (listp (car ls)))
      (walk-tree (car ls) test work (pushr path (car ls))
                 (pushr walk 'd)))
  (if (and (cdr ls))
      (walk-tree (cdr ls) test work path
                 (pushr walk 'r))))

; Walk over the top level of ls and calls func on any fdefs it finds on that
; level.

(defmacro! def-apply (name test)
  `(defun ,name (ls func)
    (cond ((,test (car ls))
           (progn
             (funcall func (car ls))
             (,name (cdr ls) func)))
          ((and ls (not (,test (car ls))))
            (progn
              (,name (cdr ls) func))))))

(def-apply fdefs-apply is-c-fdef)
(def-apply fcalls-apply is-fcall)
(def-apply nestable-apply is-nestable)
(def-apply paren-group-apply is-paren-group)
(def-apply bracket-group-apply is-bracket-group)
(def-apply curly-group-apply is-curly-group)
(def-apply word-apply is-word-group)
(def-apply space-apply is-white-space-group)
(def-apply pnctn-apply is-punctuation-group)

(defun get-first-of-type (ls test)
  (cond
    ((not ls)
     nil)
    ((funcall test (car ls))
     (car ls))
    ((and t)
     (get-first-of-type (cdr ls) test))
    )
  )

(defun get-c-fdef-params (ls)
  (progn
    (assert (is-c-fdef ls))
    (get-first-of-type ls #'is-paren-group)))

(defun get-c-fdef-body (ls)
  (progn
    (assert (is-c-fdef ls))
    (get-first-of-type ls #'is-curly-group)))

(defun get-c-fdef-name (ls)
  (progn
    (assert (is-c-fdef ls))
    (car ls)))

(defun get-js-fdef-params (ls)
  (get-c-fdef-params ls))

(defun get-js-fdef-body (ls)
  (get-c-fdef-body ls))

(defun get-js-fdef-name-aux (prev-word ls)
  (cond
    ((is-paren-group (car ls))
     prev-word)
    ((and (not (is-paren-group (car ls)))
          (not (is-word-group (car ls))))
     (get-js-fdef-name-aux prev-word (cdr ls)))
    ((is-word-group (car ls))
     (get-js-fdef-name-aux (car ls) (cdr ls)))
    ))

(defun get-js-fdef-name (ls)
  (assert (is-js-fdef ls))
  (get-js-fdef-name-aux nil ls))

(defun get-js-ctl-name (ls)
  (assert (is-js-any-ctl-stmt ls))
  (car ls))

(defun get-js-fbind-name (ls)
  (assert (is-js-fdef-binding ls))
  (car ls))

(defun get-fcall-params (ls)
  (progn
    (assert (is-js-fcall ls))
    (get-first-of-type ls #'is-paren-group)))

(defun get-fcall-param-aux (ls r)
  (cond
    ((or (and (listp ls) (listp (car ls)) (characterp (caar ls)) (CHAR= (caar ls) #\,))
         (and (characterp (car ls)) (CHAR= (car ls) #\))))
     r)
    ((and t)
     (get-fcall-param-aux (cdr ls) (pushr r (car ls))))
  ))

(defun get-param-start-aux (ls n c)
  (assert (and (listp ls) (listp (car ls))))
  (cond
    ((= c n)
     ls)
    ((CHAR= (caar ls) #\,)
     (get-param-start-aux (cdr ls) n (+ c 1)))
    ((or (CHAR/= (caar ls) #\,) (and (characterp (car ls)) (CHAR= (car ls) #\()))
     (get-param-start-aux (cdr ls) n c))))


(defun get-param-start (ls n)
  (if (= n 0)
      (cdr ls)
      (get-param-start-aux ls n 0)))

(defun get-fcall-param (ls n)
  (let ((params (get-fcall-params ls)))
    (cond
      ((< n 0)
       nil)
      ((>= n (get-fcall-n-args params))
       nil)
      ((and t)
        (get-fcall-param-aux (get-param-start params n) '()))
    )
    )
  )

(defun get-fcall-name (ls)
  (progn
    (assert (is-js-fcall ls))
    (car ls)))


;;; File IO.
;;; Copied from Practical Common Lisp (because wildcards (why?!))

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p  (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name      nil
         :type      nil
         :defaults pathname)
        pathname)))

(defun directory-wildcard (dirname)
  (make-pathname
  :name :wild
  :type #-clisp :wild #+clisp nil
  :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (directory (directory-wildcard dirname)))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))
    #+(or sbcl cmu lispworks)
    (directory wildcard)
    #+openmcl
    (directory wildcard :directories t)
    #+allegro
    (directory wildcard :directories-are-files nil)
    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))
    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)
  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))
  #+clisp
  (or (ignore-errors
       (probe-file (pathname-as-file pathname)))
      (ignore-errors
       (let ((directory-form (pathname-as-directory pathname)))
         (when (ext:probe-directory directory-form)
           directory-form))))
  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "list-directory not implemented"))

(defun is-file-p (pathname)
  (let ((p (file-exists-p pathname)))
    (if (and p) (pathname-name p))
  ))

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
        (let* ((directory (pathname-directory pathname))
               (name-and-type (pathname (first (last directory)))))
          (make-pathname
           :directory (butlast directory)
           :name (pathname-name name-and-type)
           :type (pathname-type name-and-type)
           :defaults pathname))
        pathname)))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))
           ((funcall test name) (funcall fn name)))))
                (walk (pathname-as-directory dirname))))

(defun file-to-char-ls-impl (input)
  (let ((str-ls '())
        (in (open input :if-does-not-exist nil)))
    (when in
      (setf str-ls (loop for char = (read-char in nil)
            while char collect char))
      (close in)
      str-ls)))

(defun file-to-form-impl (input)
  (let* ((in (open input :if-does-not-exist nil))
         (form nil)
         (empty nil))
    (cond
      ((and in)
       (setf form (read in))
       (close in)
       (if (not form)
        (setf empty t)))
      )
    (values form empty)))


(defun file-to-forms-impl (input)
  (let* ((in (open input :if-does-not-exist nil))
         (form nil)
         (forms '())
         (empty nil))
    (cond
      ((and in)
       (tagbody again
          (setf form (read in nil 'eof))
          (cond
            ((not (equal form 'eof))
             (pushr! forms form)
             (go again))
            )
          )
       (close in)
       (if (not forms)
           (setf empty t)))
      )
    (values forms empty)))

(defun file-to-forms (input)
  (file-to-forms-impl input))

(defun form-to-file-impl (form output)
  (let ((out (open output :direction :output :if-exists :supersede
                                    :if-does-not-exist :create)))
    (prin1 form out)
    (finish-output out)
    (close out)
  ))

(defun file-to-char-ls (input)
  (file-to-char-ls-impl input))

(defun file-to-form (input)
  (file-to-form-impl input))

(defun form-to-file (form output)
  (form-to-file-impl form output))

(defun test-cmt-str ()
  (let ((in "abc/* \"xyz\" */||\"abc/* xyz */\"//abc\"a")
        (in2 "abc/*xyz*/\"some str in here\"")
        (in3 "abc/*xyz*/"))
    (cmt-str (str-to-char-ls in3))))

;(nestables (str-to-char-ls "abc(123[xyz]789)pqr"))

;(white-space (str-to-char-ls "abc  123 \t  xyz "))

(defun c-name-to-pathname (ls)
  (cond
    ((is-c-fcall ls)
     (concatenate 'list (get-fcall-name ls) "()"))
    ((is-c-fdef ls)
     (concatenate 'list (get-c-fdef-name ls) "{}"))
    ))

(defun js-name-to-pathname (ls)
  (cond
    ((is-js-flat-ctl-stmt ls)
     (concatenate 'list (get-js-ctl-name ls) ";"))
    ((is-js-curly-ctl-stmt ls)
     (get-js-ctl-name ls))
    ((is-js-do-while-stmt ls)
     (str-to-char-ls "do"))
    ((is-js-fcall ls)
     (concatenate 'list (get-fcall-name ls) "()"))
    ((is-js-fdef ls)
     (concatenate 'list (get-js-fdef-name ls) "{}"))
    ((is-js-fdef-binding ls)
     (concatenate 'list (get-js-fbind-name ls) "="))
    ((and t)
     (assert nil))))

(defun js-refine-path (path idx-bool)
  (map 'list #'js-name-to-pathname (remove-if-not idx-bool path)))

(defun c-refine-path (path idx-bool)
  (map 'list #'c-name-to-pathname (remove-if-not idx-bool path)))

(defun fold-list (ls)
  (if (= (length ls) 0)
      (return-from fold-list nil))
  (if (= (length ls) 1)
      (return-from fold-list (list (list 1 (car ls)))))
  (let* ((prev (car ls))
         (count 1)
         (acc '()))
    (map 'nil
         #'(lambda (e)
             (cond
               ((equal e prev)
                (incf count))
               ((not (equal e prev))
                (pushr! acc (list count prev))
                (setf prev e)
                (setf count 1))
               ))
         (cdr ls))
    (pushr! acc (list count prev))

  ))

(defun unfold-list (ls)
  (if (= (length ls) 0)
      (return-from unfold-list nil))
  (let* ((unfls '()))
    (map 'nil
         #'(lambda (p)
             (dotimes (z (car p))
               (pushr! unfls (cadr p))))
         ls
         )
    unfls
  ))

(defun mk-js-path-index (ast idx-bool)
  ;; We keep the non-str lists around for backwards compatibility.
  ;; Removed the code that populates these lists because these lists were
  ;; unused, but consumed tons of space.
  ;; Same holds for all non-JS variants of this code.
  (let ((str-stkls '())
        (rev-str-stkls '())
        )
    (values
     (lambda (x y z)
       (pushr! str-stkls (list (js-refine-path (pushr y x) idx-bool) (fold-list z)))
       (pushr! rev-str-stkls (list (js-refine-path (reverse (pushr y x)) idx-bool)
                                   (fold-list z)))
       )
     (lambda () (list str-stkls rev-str-stkls ast blip-latest-ix-ver))
    )))

(defun mk-c-path-index (ast idx-bool)
  (let ((str-stkls '())
        (rev-str-stkls '())
        )
    (values
     (lambda (x y z)
       (pushr! str-stkls (list (c-refine-path (pushr y x) idx-bool) (fold-list z)))
       (pushr! rev-str-stkls (list (c-refine-path (reverse (pushr y x)) idx-bool)
                                   (fold-list z)))
       )
     (lambda () (list str-stkls rev-str-stkls ast blip-latest-ix-ver))
     )))

(defun path-index-file (ls)
  (car ls))

(defun path-index-str (ls)
  (car (cadr ls)))

(defun path-index-revstr (ls)
  (cadr (cadr ls)))

(defun path-index-ast (ls)
  (caddr (cadr ls)))

(defun path-index-ver (ls)
  (cadddr (cadr ls)))

(defun path-index-setroot (ls)
  (pushl! (car ls) (list '((#\/)) nil))
  (pushl! (cadr ls) (list '((#\/)) nil))
  ls
  )


(defun js-idx-type-to-test (type)
  (let ((idx-bool nil))
    (cond
      ((or (equal type :funcs) (equal type nil))
       (setf idx-bool #'is-js-indexable-funcs))
      ((equal type :funcs-conds)
       (setf idx-bool #'is-js-indexable-conds))
      )
    idx-bool
    )
  )

(defun js-idx-type-to-name (type)
  (if (equal type nil)
      (symbol-name :funcs)
      (symbol-name type)))

(defun c-idx-type-to-test (type)
  (let ((idx-bool nil))
    (cond
      ((or (equal type :funcs) (equal type nil))
       (setf idx-bool #'is-c-fdef-or-fcall))
      ((equal type :funcs-conds)
       (setf idx-bool #'is-c-fdef-or-fcall))
      )
    idx-bool
    )
  )

(defun c-idx-type-to-name (type)
  (js-idx-type-to-name type))

(defmacro! index-all-x-paths-impl (name type-to-test mk-x-path-index)
  `(defun ,name (ast &optional type)
     (let ((idx-bool (,type-to-test type)))
       (assert idx-bool)
       (multiple-value-bind (update-indices get-indices)
           (,mk-x-path-index ast idx-bool)
         (walk-tree ast idx-bool update-indices '() '())
         (funcall get-indices)
       ))
     )
  )

(index-all-x-paths-impl index-all-js-paths-impl js-idx-type-to-test mk-js-path-index)
(index-all-x-paths-impl index-all-c-paths-impl c-idx-type-to-test mk-c-path-index)

(defmacro! cache-x-path-index (name indexer type-namer)
  `(defun ,name (repo file commit &optional type ast)
     (if (not ast)
         (setf ast (load-ast repo file commit)))
     (let* ((type-nm (,type-namer type))
            (fcommit (git-file-latest-commit-until repo file commit))
            (outdir (str-cat blip-repo-meta repo "/root/" file "/" fcommit "/path-index/"))
            (out (str-cat outdir type-nm)))
       (mkdir outdir)
       (form-to-file (,indexer ast type) out))))

(cache-x-path-index cache-js-path-index index-all-js-paths-impl js-idx-type-to-name)
(cache-x-path-index cache-c-path-index index-all-c-paths-impl c-idx-type-to-name)

(defmacro! load-x-path-index (name type-namer)
  `(defun ,name (repo file commit &optional type ast)
     (let* ((type-nm (,type-namer type))
            (fcommit (git-file-latest-commit-until repo file commit))
            (in (str-cat blip-repo-meta repo "/root/" file "/" fcommit "/path-index/"
                         type-nm))
            (ix (file-to-form in)))
       ix
       )))

(load-x-path-index load-js-path-index js-idx-type-to-name)
(load-x-path-index load-c-path-index c-idx-type-to-name)

(defun is-latest-ix (ix)
  (and (numberp (cadddr ix)) (= (cadddr ix) blip-latest-ix-ver)))

(defmacro! index-all-x-paths (name cacher loader)
  `(defun ,name (repo file commit &optional type ast &key force)
     (expand-commit! repo commit)
     (let* ((ix (if (not force) (,loader repo file commit type ast) nil)))
       (cond
         ((or (not ix) (not (is-latest-ix ix)))
          (,cacher repo file commit type ast)
          (setf ix (,loader repo file commit type ast)))
         )
       ix
       ))
  )

(index-all-x-paths index-all-js-paths cache-js-path-index load-js-path-index)
(index-all-x-paths index-all-c-paths cache-c-path-index load-c-path-index)

(defun test-index-js (&optional force)
  (index-all-js-paths "github/davepacheco/node-vasync" "examples/barrier-basic.js"
                      :head :funcs nil :force force))

(defun count-node-in-ast (test idx-bool ast &key deep)
  (let ((count 0))
    (walk-tree ast test
               #'(lambda (n stack walk)
                   (let ((depth (length (remove-if-not idx-bool stack))))
                     (if (or (and (> depth 0) deep)
                             (and (= depth 0)))
                         (if (funcall test n)
                             (incf count)))))
               '() '())
    count
  ))

(defun list-node-in-ast (test fmt dedup ast &optional count)
  (let ((list '())
        (ddl '())
        (cl '()))
    (walk-tree ast test
               #'(lambda (n stack walk)
                   (if (funcall test n)
                       (pushr! list (funcall fmt n))))
               '() '()
               )
    (setf ddl (remove-duplicates list :test dedup))
    (if (and count)
        (progn
          (map 'list #'(lambda (e)
                         (pushr! cl
                                 (list e (count e list :test dedup))))
               ddl)
          (setf cl (stable-sort cl #'< :key #'cadr))
          ))
    (if (and cl)
        cl
        ddl)
    ))

(defmacro! load-list-nodes (name suf)
  `(defun ,name (repo file commit)
     (let* ((fcommit (git-file-latest-commit-until repo file commit))
            (in (str-cat blip-repo-meta repo "/root/" file "/" fcommit ,suf)))
       (file-to-form in)
       )
     )
  )


(defmacro! x-list-node-impl (name test getter)
  `(defun ,name (ast &optional count)
    (list-node-in-ast #',test
                      #'(lambda (n)
                          (char-ls-to-str (,getter n)))
                      #'string=
                      ast
                      count)))


(defmacro! cache-x-list-node (name list-impl suf)
  `(defun ,name (repo file commit)
    (let* ((ast (load-ast repo file commit))
           (fcommit (git-file-latest-commit-until repo file commit))
           (outdir (str-cat blip-repo-meta repo "/root/" file "/" fcommit))
           (out (str-cat outdir ,suf))
          )
      ;TODO implement a cache-ast function that will cache ASTs on a per-file basis
      ;TODO cleanup parse-x-files-at-commit
      (if (not ast)
          (assert ast))
      (mkdir outdir)
      (form-to-file (,list-impl ast t) out)
      )
    )
  )


(defmacro! x-list-node (name cacher loader)
  `(defun ,name (repo file commit &optional count &key force)
     (expand-commit! repo commit)
     (multiple-value-bind
           (ls empty) (if (not force) (,loader repo file commit)
                          (values nil nil))

       (cond
         ((and (not ls) (not empty))
          (,cacher repo file commit)
          (setf ls (,loader repo file commit)))
         )
       (cond
         ((not count)
          (setf ls (map 'list #'car ls)))
         )
       ls
       )
     ))

(load-list-nodes load-list-words "/ls-words")
(load-list-nodes load-list-fcalls "/ls-fcalls")
(load-list-nodes load-list-fdefs "/ls-fdefs")
(load-list-nodes load-list-fbinds "/ls-fbinds")

(x-list-node-impl js-list-fcalls-impl is-js-fcall get-fcall-name)
(cache-x-list-node cache-js-list-fcalls js-list-fcalls-impl "/ls-fcalls")

(x-list-node-impl js-list-fdefs-impl is-js-fdef get-js-fdef-name)
(cache-x-list-node cache-js-list-fdefs js-list-fdefs-impl "/ls-fdefs")

(x-list-node-impl js-list-fbinds-impl is-js-fdef-binding get-js-fbind-name)
(cache-x-list-node cache-js-list-fbinds js-list-fbinds-impl "/ls-fbinds")

(x-list-node-impl js-list-words-impl is-word-group identity)
(cache-x-list-node cache-js-list-words js-list-words-impl "/ls-words")

(x-list-node-impl c-list-fcalls-impl is-fcall get-fcall-name)
(cache-x-list-node cache-c-list-fcalls c-list-fcalls-impl "/ls-fcalls")

(x-list-node-impl c-list-fdefs-impl is-c-fdef get-c-fdef-name)
(cache-x-list-node cache-c-list-fdefs c-list-fdefs-impl "/ls-fdefs")

(x-list-node-impl c-list-words-impl is-word-group identity)
(cache-x-list-node cache-c-list-words c-list-words-impl "/ls-words")

(x-list-node js-list-fcalls cache-js-list-fcalls load-list-fcalls)
(x-list-node c-list-fcalls cache-c-list-fcalls load-list-fcalls)
(x-list-node js-list-fdefs cache-js-list-fdefs load-list-fdefs)
(x-list-node js-list-fbinds cache-js-list-fbinds load-list-fbinds)
(x-list-node c-list-fdefs cache-c-list-fdefs load-list-fdefs)
(x-list-node js-list-words cache-js-list-words load-list-words)
(x-list-node c-list-words cache-c-list-words load-list-words)




(defun test-ls-js (&optional count force)
  (js-list-fcalls "github/davepacheco/node-vasync" "examples/barrier-basic.js"
                      :head count :force force))

(defun hash-table-to-alist (table)
  (let ((alist nil))
    (maphash #'(lambda (k v)
                 (push (cons k v) alist))
             table)
    alist
  ))

(defmacro! agg-ast (ast test work sort key)
  `(let ((counts (make-hash-table :test #'equal)))
    (walk-tree ,ast ,test ,work '() '())
    (sort (hash-table-to-alist counts) ,sort :key ,key)
    ))

(defun file-commit-count (ast)
  (agg-ast ast #'is-file-mod
           #'(lambda (n s w)
               (incf (gethash (file-mod-stringify-path n) counts 0)))
           #'<
           #'cdr
   ))

(defun js-word-count (ast)
  (agg-ast ast #'is-word-group
           #'(lambda (n s w)
               (incf (gethash (char-ls-to-str (flatten n)) counts 0)))
           #'<
           #'cdr
           ))

(defun js-fcall-count (ast)
  (agg-ast ast #'is-js-fcall
           #'(lambda (n s w)
               (incf (gethash (char-ls-to-str (get-fcall-name n)) counts 0)))
           #'<
           #'cdr
           ))

(defun js-fcall-params-count (ast)
  (agg-ast ast #'is-js-fcall
           #'(lambda (n s w)
               (incf (gethash (char-ls-to-str (flatten (get-fcall-params n))) counts 0)))
           #'<
           #'cdr
           ))

(defun js-fdef-count (ast)
  (agg-ast ast #'is-js-fdef
           #'(lambda (n s w)
               (incf (gethash (char-ls-to-str (get-js-fdef-name n)) counts 0)))
           #'<
           #'cdr
           ))

(defun js-word-count-old (ast)
  (let ((counts (make-hash-table :test #'equal)))
    (walk-tree ast #'is-word-group
               #'(lambda (n s w)
                   (incf (gethash (char-ls-to-str n) counts 0)))
               '() '()
              )
    (sort (hash-table-to-alist counts) #'< :key #'cdr)
    ))

(defun is-require (f)
  (fcall-name-eq f "require"))

(defun walk-print-fcall-args (f s w)
  (print-fcall-args f))

(defun print-all-requires (ast)
  (walk-tree ast #'is-require #'walk-print-fcall-args '() '())
   )

(defun print-var-names (ast)
  (let ((counts (make-hash-table :test #'equal))
        (count 0))
    (walk-tree ast #'is-js-var-binding
               #'(lambda (v s w)
                   (incf (gethash (char-ls-to-str (car v)) counts 0))
                   ;(pushr! names (char-ls-to-str (car v)))
                   (incf count)
                   )
               '() '())
    (sort (hash-table-to-alist counts) #'< :key #'cdr)
    ;count
  ))

(defun print-all-arrays (ast)
  (walk-tree ast #'is-jsarr #'(lambda (e s w) (print (flatten e))) '() '()))

(defun test-arr-pr ()
  (print-all-arrays (load-ast "github/joyent/sdc-docker" "lib/moray.js" :head)))

(defun get-require-arg0 (ast)
  (let ((args '()))
        (walk-tree ast #'is-require
                   #'(lambda (f s w)
                       (let ((param (flatten (get-fcall-param f 0))))
                         (if (is-str param)
                             (pushr! args (char-ls-to-str
                                           (drop-str-quotes
                                            param))))))
                   '() '())
    (map 'list #'(lambda (e) (str-split "/" e)) args)))

(defun pathname-to-string (p)
  (char-ls-to-str p))

(defun inter-aux (c ls e nl)
  (cond ((or (and ls (not nl)) (and ls nl (cdr ls)))
         (inter-aux (pushr c (car ls) e) (cdr ls) e nl))
        ((and ls nl (not (cdr ls)))
         (inter-aux (pushr c (car ls)) (cdr ls) e nl))
        ((not ls)
         c)))

(defun intersperse (ls e &optional not-last)
  (inter-aux '() ls e not-last))

(defun fmt-path (path)
  (reduce #'str-cat (intersperse (map 'list #'pathname-to-string path) "/")))

(defun fmt-paths (pathls)
  (map 'list #'fmt-path pathls))

(defun print-js-path-tree-pair (pair)
  (print (fmt-path (car pair)))
  (print (cadr pair))
  )

(defun print-js-path (pair)
  (fmt-path (car pair)))

(defun print-js-path-raw (pair)
  (car pair))

(defun print-js-path-tree-pairs (pairs)
  (map 'list #'print-js-path-tree-pair pairs))

(defun print-js-paths (index &optional pov)
  (let* ((pairs nil))
    (if (or (not pov) (equal pov :down))
        (setf pairs (path-index-str index))
        (setf pairs (path-index-revstr index)))
    (list (path-index-file index)
          (map 'list #'print-js-path pairs))))

(defun match-path (p1 p2)
  (equal p1 p2))

(defun uniq-path-aux (stack pairs fmt)
  (let ((prev (stack-last stack)))
    (cond
      ((not pairs)
       (stack-list stack))
      ((and fmt prev (equal (car prev) (fmt-path (caar pairs))))
       (stack-set-last stack (list (fmt-path (caar pairs)) (incf (cadr prev))))
       (uniq-path-aux stack (cdr pairs) fmt))
      ((and prev (equal (car prev) (caar pairs)))
       (stack-set-last stack (list (caar pairs) (incf (cadr prev))))
       (uniq-path-aux stack (cdr pairs) fmt))
      ((and fmt)
       (stack-pushr stack (list (fmt-path (caar pairs)) 1))
       (uniq-path-aux stack (cdr pairs) fmt))
      ((and t)
       (stack-pushr stack (list (caar pairs) 1))
       (uniq-path-aux stack (cdr pairs) fmt))
       )))

(defun sort-by-path (p1 p2)
    (string< (fmt-path p1) (fmt-path p2)))

(defun uniq-path (index &key pov fmt sort-path sort-count)
  (let* ((pairs nil)
         (ret nil))
    (if (or (not pov) (equal pov :down))
        (setf pairs (path-index-str index))
        (setf pairs (path-index-revstr index)))
    (if (and sort-path)
        (setf pairs (stable-sort pairs #'sort-by-path
                                 :key #'car)))
    (setf ret (list (path-index-file index)
                    (uniq-path-aux (make-instance 'stack) pairs fmt)))
    (if (and sort-count)
        (stable-sort (cadr ret) #'< :key #'cadr))
    ret
  ))

(defun paths-by-depth (index &key pov fmt)
  (let ((pairs nil))
    (if (or (not pov) (equal pov :down))
        (setf pairs (path-index-str index))
        (setf pairs (path-index-revstr index)))
    ;(print (fmt-path (caar pairs)))
    (stable-sort pairs #'(lambda (x y) (< (length x) (length y)))
                 :key #'car)
    (list (path-index-file index)
          (if (and fmt)
              (map 'list #'(lambda (p)
                             (fmt-path (car p)))
                   pairs)
              (map 'list #'car pairs))
  )))

(defun test-pbd ()
  (paths-by-depth (test-docker-create-index) :pov nil :fmt t))

(defun auto-walk-tree-aux (tree walk)
  (cond
    ((not walk)
     (car tree))
    ((eql 'd (car walk))
     (auto-walk-tree-aux (car tree) (cdr walk)))
    ((eql 'r (car walk))
     (auto-walk-tree-aux (cdr tree) (cdr walk)))
    ((and t)
     (assert nil))
  ))

(defun auto-walk-tree (tree walk)
  (auto-walk-tree-aux tree walk)
  ;walk
  )

(defun get-path-subtree (path index &optional pov)
  (let* ((pairs nil))
    (if (or (not pov) (equal pov :down))
        (setf pairs (path-index-str index))
        (setf pairs (path-index-revstr index)))
    (list (path-index-file index) (remove nil
            (map 'list
                 #'(lambda (p) (if
                                (match-path path (fmt-path (car p)))
                                (auto-walk-tree (path-index-ast index)
                                                (unfold-list (cadr p)))))
                 pairs))))
  )

(defun get-path-subtree-str (path index &optional pov)
  (map 'list #'ast-to-str (get-path-subtree path pov)))

(defun get-path-node-count (test index idx-bool &optional pov &key zero pre)
  (let* ((pairs nil))
    (if (or (not pov) (equal pov :down))
        (setf pairs (path-index-str index))
        (setf pairs (path-index-revstr index)))
    (list (path-index-file index)
              (remove nil (map 'list
                               ;;; TODO use get-subtree here
                   #'(lambda (p)
                       (let ((count (count-node-in-ast test idx-bool
                                                       (get-path-subtree
                                                        (fmt-path (car p))
                                                        index pov) :deep t)))
                         (if (and (or (and zero (= 0 count)) (> count 0))
                                  (or (not pre) (is-str-prefix pre (fmt-path (car p)))))
                             (list (fmt-path (car p)) count))))
                   pairs)))))

(defun get-path-with-prefix (prefix index &optional pov)
  (let* ((pairs nil))
    (if (or (not pov) (equal pov :down))
        (setf pairs (path-index-str index))
        (setf pairs (path-index-revstr index)))
    (list (path-index-file index) (remove nil
          (map 'list
               #'(lambda (p) (progn
                              ;(print prefix)
                              ;(print (fmt-path (car p)))
                              (if
                              (is-str-prefix prefix (fmt-path (car p)))
                              (fmt-path (car p)))))
               pairs))))
  )

(defun get-path-with-suffix (suffix index &optional pov)
  (let* ((pairs nil))
    (if (or (not pov) (equal pov :down))
        (setf pairs (path-index-str index))
        (setf pairs (path-index-revstr index)))
    (list (path-index-file index) (remove nil
            (map 'list
                 #'(lambda (p) (if
                                (is-str-suffix suffix (fmt-path (car p)))
                                (fmt-path (car p))))
                 pairs))))
  )

(defun get-index-length (index)
  (map 'list #'(lambda (pair) (list (car pair) (length (cadr pair))))  index))


(defun print-js-paths-raw (index &optional pov)
  (let* ((pairs nil))
    (if (or (not pov) (equal pov :down))
        (setf pairs (path-index-str index))
        (setf pairs (path-index-revstr index)))
    (map 'list #'print-js-path-raw pairs)))

(defun print-fcall-name (fls path)
  (print (js-refine-path path))
  (format t "~%name: ~a~%" (char-ls-to-str (get-fcall-name fls)))
  )

(defun print-fcall-args(fls)
  (format t "name: ~a args: ~a~%" (char-ls-to-str (get-fcall-name fls))
          (get-fcall-params fls)))

(defun get-c-fdef-n-args (p)
  (let ((count 0))
    (labels ((counter (pg)
               (if (and (= 1 (length pg)) (CHAR= #\, (car pg)))
                   (incf count 1))))
      (pnctn-apply p #'counter)
      (+ count 1))))

(defun get-fcall-n-args (p)
  (get-c-fdef-n-args p))

(defun strtest ()
  (cmt-str (str-to-char-ls "word''drow")))

(defun nestest()
  (nestables (str-to-char-ls "word()drow")))

(defun ast-to-str (ast)
  (pipeline (flatten ast) #'char-ls-to-str))

(defun mapahead-aux (res func ls prev)
  (cond
    ((not ls)
     res)
    ((and ls)
     (mapahead-aux
      (pushr res (funcall func prev (car ls) (cdr ls)))
      func
      (cdr ls)
      (car ls)))))

(defun mapahead (func ls)
  (mapahead-aux '() func ls nil))

(defun xform-ast (ast test xform stack)
  (if (not (listp ast))
      (return-from xform-ast ast))
  (let* ((ret nil))
    (setf ret
          (remove nil
                  (reduce #'append
                          (mapahead
                               #'(lambda (prev n rest)
                                   (if (funcall test n)
                                       (multiple-value-list
                                        (funcall xform prev n rest stack))))
                               ast))))
    (setf ret (map 'list
                   #'(lambda (n)
                       (xform-ast n test xform (pushr stack n)))
                   ret))
    ret
    )
  )

(defun copy-ast (ast test)
  (xform-ast ast test #'(lambda (p n r s) n) '()))

(defun gen-ws-aux (acc n)
  (cond
    ((> n 0)
     (gen-ws-aux (pushr acc #\Space) (- n 1))
     )
    ((= n 0)
     acc)
    )
  )

(defun gen-ws (ns)
  (gen-ws-aux '() ns))

(defun gen-nl-ws-aux (acc n)
  (cond
    ((> n 0)
     (gen-nl-ws-aux (pushr acc #\Space) (- n 1))
    )
    ((= n 0)
     acc)
    )
  )

(defun gen-nl-ws (ns)
  (gen-nl-ws-aux '(#\Newline) ns))


(defun is-js-hyperlink (n)
  (and (listp n) (> (length n) 4)
       (and (characterp (car n)) (CHAR= (car n) #\<))
       (and (characterp (cadr n)) (CHAR= (cadr n) #\<))
       (and (characterp (caddr n)) (CHAR= (caddr n) #\<))
       (and (characterp (cadddr n)) (CHAR= (cadddr n) #\Space))
       ))

;;; We want to not indent `};`
;;; Also, we may want to count depty by {} and not fdefs
(defun js-simtupid-fmt-cb (prev n rest s)
  (let* ((depth 0))
    (incf depth (count-if #'is-curly-group s))
    (cond
      ((is-js-hyperlink (car (last s)))
       (values n))
      ((and (is-punctuation-group n) (CHAR= #\; (car n))
            (or (and (characterp (car rest)) (CHAR/= #\} (car rest)))
                (not (characterp (car rest)))))
       (values n (gen-nl-ws (* 4 depth))))
      ((and (characterp n) (CHAR= #\{ n)
            (or (and (characterp (car rest)) (CHAR/= #\} (car rest)))
                (not (characterp (car rest)))))
       (values (gen-ws 1) n (gen-nl-ws (* 4 depth))))
      ((and (characterp n) (CHAR= #\{ n)
            (and (characterp (car rest)) (CHAR= #\})))
       (print (last s))
       (values (gen-ws 1) n (gen-nl-ws (* 4 (- depth 1)))))
      ((and (characterp n) (CHAR= #\} n))
       (values (gen-nl-ws (* 4 (- depth 1))) n (gen-nl-ws (* 4 (- depth 1)))))
      ((is-js-ctl-struct n)
       (values n (gen-ws 1)))
      ((and (is-str n) (is-word-group prev))
       (values (gen-ws 1) n))
      ((and (is-str n) (is-str prev))
       (values (gen-nl-ws (* 4 depth)) n))
      ((and (or (is-word-group n) (is-jsarr n)) ;(is-punctuation-group (car rest))
            (or (match-punc-ls "," prev)
                (is-word-group prev)))
       (values (gen-ws 1) n))
      ((match-punc-ls "=" n)
       (values (gen-ws 1) n (gen-ws 1)))
      ((and (> depth 0) (is-js-fdef n))
       (values (gen-nl-ws (* 4 depth))
               (append (str-to-char-ls "<<< ")
                       (get-js-fdef-name n)
                       (str-to-char-ls "{}/ >>>") (gen-nl-ws (* 4 depth)))))
      ((and (> depth 0) (is-js-fdef-binding n))
       (values (gen-nl-ws (* 4 depth)) (str-to-char-ls "<<< ")
               (get-js-fbind-name n) (str-to-char-ls "=/ >>>")))
      ((and t)
       (values n))
      )
    )
  )

(defun js-ast-fmt-simtupid (out ast)
  (let* ((ret nil)
         (idx-bool (js-idx-type-to-test :funcs)))
    (setf ret (copy-ast ast
                        #'(lambda (n)
                            (not (is-blank-group n)))))
    (setf ret (xform-ast ret
                         #'identity
                         #'js-simtupid-fmt-cb
                         '()))
    ret
    )
  )

(defun c-simtupid-fmt-cb (prev n rest s)
  (let* ((depth 0))
    (incf depth (count-if #'is-curly-group s))
    (cond
      ((and (is-punctuation-group n) (CHAR= #\; (car n))
            (or (and (characterp (car rest)) (CHAR/= #\} (car rest)))
                (not (characterp (car rest)))))
       (values n (gen-nl-ws (* 4 depth))))
      ((and (characterp n) (CHAR= #\{ n)
            (or (and (characterp (car rest)) (CHAR/= #\} (car rest)))
                (not (characterp (car rest)))))
       (values (gen-ws 1) n (gen-nl-ws (* 4 depth))))
      ((and (characterp n) (CHAR= #\{ n)
            (and (characterp (car rest)) (CHAR= #\})))
       (print (last s))
       (values (gen-ws 1) n (gen-nl-ws (* 4 (- depth 1)))))
      ((and (characterp n) (CHAR= #\} n))
       (values (gen-nl-ws (* 4 (- depth 1))) n (gen-nl-ws (* 4 (- depth 1)))))
      ((is-ctl-struct n)
       (values n (gen-ws 1)))
      ((and (is-str n) (is-word-group prev))
       (values (gen-ws 1) n))
      ((and (is-str n) (is-str prev))
       (values (gen-nl-ws (* 4 depth)) n))
      ((and (or (is-word-group n) (is-c-fdef n) (is-jsarr n)) ;(is-punctuation-group (car rest))
            (or (match-punc-ls "," prev)
                (is-word-group prev)))
       (values (gen-ws 1) n))
      ((match-punc-ls "=" n)
       (values (gen-ws 1) n (gen-ws 1)))
      ((and (is-punctuation-group n) (CHAR= (car n) #\*))
       (values (gen-ws 1) n))
      ((and t)
       (values n))
      )
    )
  )

(defun c-ast-fmt-simtupid (out ast)
  (let* ((ret nil)
         (idx-bool (js-idx-type-to-test :funcs)))
    (setf ret (copy-ast ast
                        #'(lambda (n)
                            (not (is-blank-group n)))))
    (setf ret (xform-ast ret
                         #'identity
                         #'c-simtupid-fmt-cb
                         '()))
    ret
    )
  )

(defun test-js-fmt ()
  (let* ((in (str-to-char-ls
              "function foo (a b c) { function bar (x y z) { hello.world(); } bar();}"))
         (ast (js-to-ast in)))
    (js-ast-fmt-simtupid nil ast)
    )
  )

(defun test-js-fmt-2 ()
  (let* ((in (str-to-char-ls
              "function foo (a b {d:function c (a b c) { stuff }}) { function bar (x y z) { hello.world(); } bar();}"))
         (ast (js-to-ast in)))
    (js-ast-fmt t ast :simtupid)
    )
  )

(defun js-ast-fmt (out ast style)
  (if (equal out t)
      (setf out *standard-output*))
  (cond
    ((equal style :simtupid)
     (js-ast-fmt-simtupid nil ast))
    ((and t)
     (assert nil))
    ))

(defun c-ast-fmt (out ast style)
  (if (equal out t)
      (setf out *standard-output*))
  (cond
    ((equal style :simtupid)
     (c-ast-fmt-simtupid nil ast))
    ((and t)
     (assert nil))
    ))

(defun gen-chars (&rest cs)
  (let* ((res '()))
    (map 'nil #'(lambda (c)
                  (cond
                    ((characterp c)
                     (pushr! res c))
                    ((stringp c)
                     (setf res (append res (str-to-char-ls c))))
                    ((and t)
                     (assert nil))
                    )
                  )
         cs)
    res
    )
  )

(defun valve (test val)
  (if (funcall test val) val (assert nil)))

(defmacro! gen-word (&rest cs)
  `(valve #'validate-word (gen-chars ,@cs)))

(defmacro! gen-blank ()
  `(valve #'validate-blank (enclose (gen-ws 1))))

(defun gen-js-var (lv &optional rv)
  "We expect lv to be a string and rv to be an AST"
  (cond
    ((and rv)
     (enclose (gen-word "var") (gen-blank) (gen-word lv)
              (gen-blank) (gen-chars "=") rv (gen-chars ";")))
    ((not rv)
     (enclose (gen-word "var") (gen-blank) (gen-word lv)
              (gen-chars ";")))
    ))

;;; XXX We currently don't group non-func bindings. This means we will produce
;;; ASTs that would be parsed in differently. It doesn't break anything, but I
;;; feel this difference should be documented.
(defun gen-js-bind (lv rv)
  "We expect lv to be a string and rv to be an AST"
  (enclose (gen-word lv) (gen-blank) (gen-chars "=") rv (gen-chars ";"))
  )

(defun gen-js-fargs (words)
  (enclose (gen-chars "(") (mapahead #'(lambda (p n r)
                                         (cond
                                           ((and r)
                                            (gen-word n)
                                            (gen-chars ","))
                                           ((and t)
                                            (gen-word n))))
                                     words) (gen-chars ")"))
  )

(defun gen-js-fcargs (asts)
  (enclose (gen-chars "(") (mapahead #'(lambda (p n r)
                                         (cond
                                           ((and r)
                                            n
                                            (gen-chars ","))
                                           ((and t)
                                            n)))
                                     asts) (gen-chars ")"))
  )

(defun gen-js-func (nm args body)
  "Function name, list of arg-name, and ast-body (without { or })"
  (enclose (gen-word nm) (gen-blank) (gen-js-fargs args)
           (gen-blank) (enclose (gen-chars "{") body (gen-chars "}")))
  )

(defun gen-js-ctl-struct (ctl-word test body)
  (enclose (gen-word ctl-word) (gen-blank)
           (enclose (gen-chars "(") test (gen-chars ")"))
           (enclose (gen-chars "{") body (gen-chars "}"))
           ))


;;; XXX Want to supply a list of 1 or more cond-pairs (a cond-test ast and a
;;; body ast) and a final, optional else-body.
(defun gen-js-if (conds &optional else)
  (let ((if-asts '())
        (res '()))
    (map 'nil #'(lambda (c)
                  (pushr! if-asts
                          (gen-js-ctl-struct "if" (car cond) (cadr cond))
                  ))
         conds
         )
    (map 'nil #'(lambda (a)
                  (pushr! res a)
                  (pushr! res (gen-word "else"))
                  )
         if-asts)
    (if (and else)
        (pushr! res (enclose (gen-char "{") else (gen-char "}")))
        (popr! res))
    res
    )
  )

(defun gen-js-while (test body)
  (gen-js-ctl-struct "while" test body))

(defun gen-js-for (test body)
  (gen-js-ctl-struct "for" test body))

(defun gen-js-do-while (test body)
  (enclose (gen-word "do") (gen-blank)
           (enclose (gen-chars "{") body (gen-chars "}"))
           (gen-blank) (gen-word "while") (gen-blank)
           (enclose (gen-chars "(") test (gen-chars ")"))
           (gen-chars ";")
           ))

(defun gen-js-fcall (name args)
  (enclose (gen-word name) (gen-blank) (gen-js-fcargs args) (gen-chars ";")))

(defun gen-js-mbr (&rest wds)
  "Generates a chain of at least 2 members. Arguments are either words or
   strings."
  (assert (> (length wds) 1))
  (let ((ret '()))
    (mapahead #'(lambda (p n r)
                  (cond
                    ((and r (stringp n))
                     (pushr! ret (gen-word n) (gen-chars "."))
                     )
                    ((and r (is-word-group n))
                     (pushr! ret n (gen-chars "."))
                     )
                    ((and (not r) (stringp n))
                     (pushr! ret (gen-word n))
                     )
                    ((and (not r) (is-word-group n))
                     (pushr! ret n)
                     )
                  ))
              wds)
    ret
    )
  )

(defun gen-js-mtd (wds func args)
  "Generate a method call and attach it to the the end of a member-chain"
  (append (apply #'gen-js-mbr wds) (gen-chars ".") (gen-js-fcall func args)))

(defun gen-js-obj-lit (&rest kvps)
  "Takes list of KV-pairs to create obj lit. Keys are strings/words,
   values are ASTs."
  )

(defun glue (&rest lss)
  (let* ((res '()))
    (map 'nil #'(lambda (ls)
                  (setf res (append res ls)))
         lss)
    res
    )
  )

(defun js-to-ast (input)
  (pipeline input #'cmt-str #'white-space
            #'blanks #'words #'punctuations
            #'nestables
            #'js-fdefs
            #'js-fcalls
            #'js-arrs
            #'js-var-bindings
            #'js-mbr-chain
            ;#'js-obj-lit
            #'js-vars
            #'js-curly-ctl-stmts
            #'js-do-while-stmts
            #'js-flat-ctl-stmts
            ))

(defun cl-to-ast (input)
  (pipeline input #'cl-cmt-str #'white-space
            #'cl-blanks #'words #'punctuations
            #'nestables
   ))

(defun test-if-parse ()
  (let ((in1 "call(\"arg\", function (a, b) { if (true) if (true) { if (true) { return 1 }};});")
        (in2 " if (x) if (y) {(10 * 10)};")
        (in3 "if (x) if (y) (10 * 10);")
        (in4 "if (x) { (10 * 10); }")
        (in5 "if (z) {do { if (a) do { (10 * 10) } while (y); } while (x);}")
        (in6 "if (x) { if (z) while (y) { (10 * 10); };}")
        )
  (js-to-ast
   (str-to-char-ls in6))))

(defun test-varbind-parse ()
  (let ((in1 "if (x) { function foo (a, b) {var mvar = 2 + 2 + call(); for (mvar = 0; true; mvar++) { print('heeeeeeey'); }}; bar(); foo(); x = 4.2; }")
        (in2 "if (a, b) {call(); var func = function foo (a, b) {hello = function (a) {b};};}")
        (in3 "function a (1, 2) {foo = function (1, 2) {4};}"))
    (js-to-ast
     (str-to-char-ls in2))))

(defun test-mbrchain-parse ()
  (let ((in2 "if (a, b) {call().a.b.c; var func = function foo (a, b) {hello.fn = function (a) {b.mycall().mymbr = {a: b, b: c, c: d.e, e: f};};};}")
        (in3 "function (a) {a.b[10].c[10] = function (a) {myCall(); function () {a.b.c}; return a = a.b.call(function () {a.b.c});};}")
        (in4 "a.b.c = q.w.e;"))
    (js-to-ast
     (str-to-char-ls in3))))

;;; TODO implement this
(defun c-pre-proc (ls)
 ;;;(c-pre-proc-aux '() (car ls) (cdr ls)
  )

(defun c-to-ast (input)
  (pipeline input #'cmt-str #'white-space
            #'blanks #'words #'punctuations
            #'nestables #'fcalls #'c-fdefs #'jsarrs))


(defun test-ast ()
  (js-to-ast
   (str-to-char-ls "var foo = require /*cmt*/ ('module'); var bar = require('module');var myfunc = function foo (a, b, c) { var func2 = function (abc) { return x } };")))

(defun test-fcop ()
  (js-to-ast
   (str-to-char-ls "var myfunc = function foo (a, b, c) { var func2 = function (abc) { return x } };")))



(defun test-fcall-cb-old ()
  (let ((in "'operations': funcs.map(function (func) { return ({ }); }),"))
    (js-to-ast (str-to-char-ls in))
    )
  )

(defun test-fcall-cb()
  (let ((in "funcs.map(function (func) {\n return ({\n 'func': func,\n 'funcname': func.name || '(anon)',\n 'status': 'waiting'\n });\n })"))
    (js-to-ast (str-to-char-ls in))
    ))

(defun test-docker-ast ()
  (multiple-value-bind (ast time)
      (js-to-ast (file-to-char-ls
                   (str-cat blip-repos "github/joyent/sdc-docker/lib/backends/sdc/containers.js")))
    time))

(defun test-js-ast ()
  (let ((in "function addPublishFirewallRules(opts, container, img, payload, callback) { var e; var exposed; var hostConf = container.HostConfig; var imageExposedPorts = img.config && img.config.ExposedPorts || {}; callback(); }")
        (in2 "if { if { }"))
    (js-to-ast (str-to-char-ls in2))
    )
  )

;;; TODO group bindings `$STRING : $STUFF`
;;; A JSON binding is a string, a colon, and a value followed by a comma or nil.
;;; A value is either a string, word, or group.
;;; TODO allow walking of JSON and 1-level listing of keys
(defun json-to-ast (input)
  (pipeline input #'cmt-str #'white-space #'blanks #'words
            #'punctuations #'nestables #'json-kvp))

(defun is-json-kvp (ls)
    (and (listp ls)
         (cond
           ((= (length ls) 3)
            (and (is-str (car ls)) (is-colon-group (cadr ls))
                 (is-json-value (caddr ls))))
           ((= (length ls) 4)
            (and (is-str (car ls))
                 (or (and (is-colon-group (cadr ls))
                          (is-blank-group (caddr ls))
                          (is-json-value (cadddr ls)))
                     (and (is-blank-group (cadr ls))
                          (is-colon-group (caddr ls))
                          (is-json-value (cadddr ls))))))
           ((= (length ls) 5)
            (and (is-str (car ls))
                 (is-blank-group (cadr ls))
                 (is-colon-group (caddr ls))
                 (is-blank-group (cadddr ls))
                 (is-json-value (car (cddddr ls)))))
           ((and t)
            nil))))


(defun get-json-kvp-value (ls)
  (progn
    (assert (is-json-kvp ls))
    (cond
      ((is-json-value (caddr ls))
       (caddr ls))
      ((is-json-value (cadddr ls))
       (cadddr ls))
      ((is-json-value (car (cddddr ls)))
       (car (cddddr ls))))))

(defun get-json-kvp-key (ls)
  (progn
    (assert (is-json-kvp ls))
    (car ls)))

;;; The quotes around `str` are implied
(defun json-key-eq (ls str)
  (match-str-list str (drop-str-quotes (get-json-kvp-key ls))))

(defun test-docker-json ()
  (multiple-value-bind (ast time)
      (json-to-ast (file-to-char-ls
                    (str-cat blip-repos "gerrit/joyent/sdc-docker/package.json")))
    ast))

(defun json-get-kvp (ast &optional key)
  (let ((kvps '()))
    (walk-tree ast #'is-json-kvp
               #'(lambda (n ignore w)
                   (if (or
                        (and key (json-key-eq n key))
                        (not key))
                       (pushr! kvps (list (get-json-kvp-key n)
                                          (get-json-kvp-value n)))))
               '() '())
    kvps
    )
  )

(defun pkg-deps-json (ast)
  (let ((deps '())
        (dep-tree '()))
    (setf dep-tree (cadar (json-get-kvp ast "dependencies")))
    (setf deps (map 'list
                    #'(lambda (x) (list (char-ls-to-str (drop-str-quotes (car x)))
                                        (char-ls-to-str (drop-str-quotes (cadr x)))))
                    (json-get-kvp dep-tree)))
    deps))

(defun test-docker-deps ()
  (pkg-deps-json (test-docker-json)))

(defun pkg-type (pkg)
  ;TODO validate the semver
  (let ((v (cadr pkg)))
    (cond
      ((is-str-prefix "https" v)
       'http-commit)
      ((is-str-prefix "git+https" v)
       'git+https-commit)
      ((is-str-prefix "git" v)
       'git-commit)
      ((is-npm-range-version v)
       'semver-range)
      ((and t)
       'semver))))

(defun git-commit-split (pkg)
  (str-split "\\+|#" (cadr pkg)))

(defun get-repo-url-from-pkg-ver (pkg)
  (if (eq (pkg-type pkg) 'git+https-commit)
      (let ((vparts (git-commit-split pkg)))
        (if (> (length vparts) 1)
            (if (string= "git" (car vparts))
                (cadr vparts)
                (car vparts))))))

(defun test-docker-git-urls ()
  (remove nil (map 'list #'get-repo-url-from-pkg-ver (test-docker-deps))))

(defun test-docker-pkg-json ()
  (map 'list #'(lambda (tup) (npm-get-pkg (car tup) (cadr tup)))
       (remove nil
               (map 'list
                    #'(lambda (p) (if (eq (pkg-type p) 'semver) p))
                    (test-docker-deps))))
  )


(defun test-docker-dep-vers ()
  (pipeline (car (test-docker-pkg-json)) #'json-to-ast
            #'(lambda (x) (json-get-kvp x "dependencies"))
            ;TODO tell above func to return value from above kvp
    ;(map 'list #'json-to-ast (test-docker-pkg-json)
  ))

(defun test-docker-dep-types ()
  (map 'list #'pkg-type (test-docker-deps)))

(defun npm-pkg-url (p v)
  (str-cat npm-base-url p "/" v))

(defun npm-get-pkg (pkg version)
  (pushnew '("application" . "json") drakma:*text-content-types*)
  (drakma:http-request (npm-pkg-url pkg version) :method :get))

(defun npm-parse-ver-str (v)
  (pipeline v #'white-space #'blanks #'words #'punctuation))



(defun mk-str-index ()
  (let ((strs '()))
    (values
     (lambda (x y z) (if (json-key-eq x "full_name")
                       (setf strs
                             (pushr strs (char-ls-to-str
                                          (drop-str-quotes
                                           (get-json-kvp-value x)))))))
     (lambda () (list strs))
     )))

(defun index-repo-names (ast)
  (multiple-value-bind (update-str-index get-index) (mk-str-index)
    (walk-tree ast #'is-json-kvp update-str-index '() '())
    (funcall get-index)
    )
  )


(defun list-user-repos-http (user page)
  (let ((url (github-user-repos-url user))
        (pg (write-to-string page)))
    (pushnew '("application" . "json") drakma:*text-content-types*)
        (drakma:http-request url :method :get :parameters
                             (pairlis '("page" "per_page")
                                   (list pg "100")))))

(defun user-repos-json (user page)
  (json-to-ast (str-to-char-ls (list-user-repos-http user page))))

(defun user-repo-list-aux (user pg rl)
  (let* ((repo-list (car (index-repo-names (user-repos-json user pg))))
        (all (append rl repo-list)))
    (if (and repo-list)
        (user-repo-list-aux user (+ pg 1) all)
        all)))

(defun fetch-user-repo-list (user)
  (user-repo-list-aux user 1 '()))

(defun load-svc-user-repo-list (svc user)
  (file-to-form (str-cat blip-repos-index svc "/" user)))

(defun cache-svc-user-repo-list (svc user)
  (form-to-file (fetch-user-repo-list user) (str-cat blip-repos-index svc "/" user)))

(setf git-jobs 0)

(defun update-git-cmd-status (proc stat sig)
  (print "stat")
  (print stat)
  (decf git-jobs)
  (if (= git-jobs 0)
      (cl-async:exit-event-loop)))


(defun git-cmd (cmd dir &optional remote)
  (pushdir dir)
  (cl-async:spawn "git" (if (and remote) (list cmd remote) (list cmd))
                   :exit-cb #'update-git-cmd-status)
  (incf git-jobs)
  (popdir)
  )

(defun git-cmd-svc-user (cmd svc user &optional remote)
  (git-cmd cmd (str-cat blip-repos svc "/" user "/") remote))


(defun github-pull-joyent (user-repo)
  )

(defun localhost-clone (path)
  ;(git-clone path
             (str-cat blip-repos "localhost/"
                           (car (last (str-split "/" path)))));)

(defun github-clone-user-all (user)
  (map 'list #'(lambda (remote)
                 (git-cmd-svc-user "clone" "github" user remote))
       (repo-specs-to-github-urls (load-svc-user-repo-list "github" user))))

(defun github-clone-user-all-bg (user)
  (cl-async:start-event-loop #'(lambda () (github-clone-user-all user))))

(defun github-clone-joyent-all ()
  (github-clone-user-all-bg "joyent"))

(defun github-pull-user-all (user)
  (map 'list #'(lambda (user-repo) (git-cmd-svc-user "pull" "github" user-repo))
        (load-svc-user-repo-list "github" user)))

(defun github-pull-user-all-bg (user)
  (cl-async:start-event-loop
   #'(lambda () (github-pull-user-all user))))

(defun github-pull-joyent-all ()
  (github-pull-user-all-bg "joyent"))

(defun github-clone-daly (remote)
  (git-cmd-svc-user "clone" "github" "daly" remote))

(defun github-pull-daly (user-repo)
  (git-cmd-svc-user "pull" "github" user-repo))

(defun github-clone-daly-all% ()
  (map 'list #'github-clone-daly
       (repo-specs-to-github-urls (load-svc-user-repo-list "github" "daly"))))

(defun github-clone-daly-all ()
  (cl-async:start-event-loop #'github-clone-daly-all%))

(defun github-pull-daly-all% ()
  (map 'list #'github-pull-daly (load-svc-user-repo-list "github" "daly")))

(defun github-pull-daly-all ()
  (cl-async:start-event-loop #'github-pull-daly-all%))

(defun gerrit-clone-joyent (remote)
  (git-cmd-svc-user "clone" "gerrit" "joyent" remote))

(defun gerrit-pull-joyent (user-repo)
  (if (file-exists-p (str-cat blip-repos svc "/" user-repo "/"))
      (git-cmd-user "pull" "gerrit" user-repo)))

(defun gerrit-clone-joyent-all% ()
  (map 'list #'gerrit-clone-joyent
       (repo-specs-to-gerrit-urls (load-svc-user-repo-list "github" "joyent"))))

(defun gerrit-clone-joyent-all ()
  (cl-async:start-event-loop #'gerrit-clone-joyent-all%))

(defun gerrit-fetch-joyent-all-patches ()
  (cl-async:start-event-loop #'gerrit-clone-joyent-all%))

(defun gerrit-pull-joyent-all% ()
  (map 'list #'gerrit-pull-joyent (load-svc-user-repo-list "github" "joyent")))

(defun gerrit-pull-joyent-all ()
  (cl-async:start-event-loop #'gerrit-pull-joyent-all%))

(defun git-switch-branch (branch)
  (assert (and branch))
  (inferior-shell:run/ss (list "git" "checkout" branch)))

(defun git-clone (path dest)
  (assert (and path dest))
  (inferior-shell:run/ss (list "git" "clone" path dest)))

(defun git-log-repo (repo &optional branch)
  (let ((curbr (git-current-branch repo)))
    (pushdir (str-cat blip-repos repo "/"))
    (if (and branch)
        (git-switch-branch branch))
    (let ((log (inferior-shell:run/ss (list "git" "--no-pager" "log" "--name-status"))))
      (if (and branch)
          (git-switch-branch curbr))
      (popdir)
      log)
    )
  )

(defun git-ls-remote (repo)
  (pushdir (str-cat blip-repos repo "/"))
  (let ((remotes (inferior-shell:run/ss (list "git" "ls-remote" "origin"))))
    (popdir)
    remotes)
  )

(defun git-ls-branches (repo)
  (pushdir (str-cat blip-repos repo "/"))
  (let ((branches (inferior-shell:run/ss (list "git" "branch"))))
    (popdir)
    (let ((bs (str-split "[\\n\\r\\s]+" branches)))
      (iter:iter
        (iter:for b in bs)
        (if (and (not (equal "*" b)) ( not(equal "" b)))
            (iter:collect b))))
  ))


(defun git-fetch-remote (repo targ)
  (let ((branches (git-ls-branches repo))
        (branch (cadr (str-split ":" targ))))
    (if (not (member branch branches :test #'equal))
        (progn
          (pushdir (str-cat blip-repos repo "/"))
          (let ((out (inferior-shell:run/ss (list "git" "fetch" "origin" targ))))
            (popdir)))
        )))

(defun git-log-urepo (svc user repo)
  (git-log-repo (str-cat svc "/" user "/" repo "/"))
  )

(defun git-current-branch (repo)
  (pushdir (str-cat blip-repos repo "/"))
  (let ((cb (inferior-shell:run/ss
             (list "git" "rev-parse" "--abbrev-ref" "HEAD"))))
    (popdir)
    cb
    )
  )

(defun git-branch-commit (commit)
  (assert (and commit))
  (inferior-shell:run/ss (list "git" "checkout" "-B" "blip_tmp_branch" commit))
  )

(defun git-unbranch (curbr)
  (inferior-shell:run/ss (list "git" "checkout" curbr))
  (inferior-shell:run/ss (list "git" "branch" "-d" "blip_tmp_branch"))
  )

(defun starts-w-newline (ls)
  (and ls (listp ls) (characterp (car ls)) (CHAR= #\Newline (car ls))))

(defun is-just-newline (ls)
  (and (starts-w-newline ls) (< (length ls) 3)))

(defun commits-aux (stack head tail)
  (tagbody
   again
     (cond
       ((and (is-just-newline head) (match-str-list "commit" (car tail)))
        (stack-pushr stack head)
        (stack-pushr stack (head-n tail 3))
        (advance-scanner 4)
        (go again))
       ((and tail)
        (stack-pushr stack head)
        (advance-scanner)
        (go again))
     ))
  (stack-list stack)
  )

(defun commits (ls)
  (pushr (commits-aux (make-instance 'stack) '(#\Newline) ls) '(#\Newline)))

(defun is-author-or-date (ls)
  (or (match-str-list "Author" ls) (match-str-list "Date" ls)))

(defun author-date-aux (stack head tail)
  (tagbody
   again
     (cond
       ((and (is-just-newline (stack-last stack)) (is-author-or-date head))
        (let ((ls '()))
          (tagbody
           lsagain
             (cond
               ((not (starts-w-newline head))
                (pushr! ls head)
                (advance-scanner)
                (go lsagain))
               ((and t)
                (stack-pushr stack ls)
                (go again))
               ))))
       ((and tail)
        (stack-pushr stack head)
        (advance-scanner)
        (go again))
       ((and t)
        (stack-pushr stack head)
        )
       )
     )
  (stack-list stack)
  )


(defun author-date (ls)
  (author-date-aux (make-instance 'stack) '(#\Newline) ls))

(defun file-mod-list (acc head tail)
  (if (not (starts-w-newline head))
      (file-mod-list (pushr acc head) (car tail) (cdr tail))
      (list acc (cons head tail))))

(defun is-file-mod-char (ls)
  (and (is-word-group ls) (= (length ls) 1)))

(defun file-mods-aux (stack head tail)
  (tagbody
   again
     (cond
       ((and (is-just-newline (stack-last stack)) (is-file-mod-char head))
        (let ((ls '()))
          (tagbody
           lsagain
             (cond
               ((not (starts-w-newline head))
                (pushr! ls head)
                (advance-scanner)
                (go lsagain))
               ((and t)
                (stack-pushr stack ls)
                (go again))
               ))))
       ((and tail)
        (stack-pushr stack head)
        (advance-scanner)
        (go again))
       ((and t)
        (stack-pushr stack head))
       ))
  (stack-list stack))

(defun file-mods (ls)
  (file-mods-aux (make-instance 'stack) '(#\Newline) ls))

(defun git-strip-commit-msgs (ast)
  (let ((acc nil))
    (walk-tree ast #'is-commit-or-file-mod
               #'(lambda (n s w)
                   (pushr! acc n))
               '() '())
    acc
    )
  )


(defun cache-git-log (repo &optional branch)
  (let* ((branchname (if (and branch) branch (git-current-branch repo)))
         (dir (str-cat blip-repo-meta repo "/br/" branchname "/"))
         (status (mkdir dir))
         (log-file (str-cat dir "git.log"))
         (full-ast nil))

    (mkdir dir)
    (setf full-ast
     (pipeline repo #'(lambda (r) (git-log-repo r branch))
               #'str-to-char-ls #'white-space #'words #'punctuations
               #'commits #'author-date #'file-mods))
    (form-to-file (git-strip-commit-msgs full-ast) log-file)
    )
  )

(defun cache-git-log-breakdown (repo &optional branch)
  (let* ((branchname (if (and branch) branch (git-current-branch repo)))
         (dir (str-cat blip-repo-meta repo "/br/" branchname "/"))
         (status (mkdir dir))
         (log-file (str-cat dir "git.log")))

     (multiple-value-bind (form timing) (pipeline repo #'(lambda (r) (git-log-repo r branch))
               #'str-to-char-ls #'white-space #'words #'punctuations
               #'commits #'author-date #'file-mods)
       (print timing))
     ))

(defun test-cache-git-log ()
  (cache-git-log "gerrit/joyent/sdc-docker" "CH-145-1"))

(defun test-cache-git-log-breakdown ()
  (cache-git-log-breakdown "gerrit/joyent/sdc-docker" "CH-145-1"))

(defun cache-git-log-all-branches (repo)
  (let ((branches (git-ls-branches repo)))
    (map 'nil #'(lambda (b) (cache-git-log repo b)) branches)))

(defun git-refs-aux (stack head tail)
  (tagbody
   again
     (cond
       ((and (is-blank-group (stack-last stack))
             (CHAR= #\Tab (caar (stack-last stack))))
        (let ((ls '()))
          (tagbody
           lsagain
             (cond
               ((or (is-blank-group head)
                    (not head))
                (stack-pushr stack ls)
                (go again))
               ((and t)
                (pushr! ls head)
                (advance-scanner)
                (go lsagain))
               )
          )
        ))
       ((and head)
        (stack-pushr stack head)
        (advance-scanner)
        (go again))
       )
     )
  (stack-list stack)
  )

(defun git-refs (ls)
  (git-refs-aux (make-instance 'stack) (car ls) (cdr ls)))

(defun is-git-ref (ls)
  (and ls (listp ls) (= (length ls) 9) (match-str-list "refs" (car ls))
       (match-str-list "/" (cadr ls))
       (match-str-list "changes" (caddr ls))
       (match-str-list "/" (cadddr ls))))

(defun change-patch-triple (ls)
  (list (nth 4 ls) (nth 6 ls) (nth 8 ls)))

(defun refs-to-triples (ls)
  (map 'list #'change-patch-triple ls))

(defun triple-to-branch (ls)
  (let ((changelet (char-ls-to-str (car ls)))
        (change (char-ls-to-str (cadr ls)))
        (patch (char-ls-to-str (caddr ls))))
    (str-cat "CH-" change "-" patch)))

(defun triple-to-ref (ls)
  (let ((changelet (char-ls-to-str (car ls)))
        (change (char-ls-to-str (cadr ls)))
        (patch (char-ls-to-str (caddr ls))))
    (str-cat "refs/changes/" changelet "/" change "/" patch)))

(defun triple-to-target-str (ls)
    (str-cat (triple-to-ref ls) ":" (triple-to-branch ls)))

(defun triples-to-targs (ls)
  (map 'list #'triple-to-target-str ls))

(defun cache-git-remotes (repo &optional branch)
  (let* ((branchname (if (and branch) branch (git-current-branch repo)))
         (dir (str-cat blip-repo-meta repo "/br/" branchname "/"))
         (status (mkdir dir))
         (remote-file (str-cat dir "git.remotes")))
    (form-to-file
     (pipeline repo #'git-ls-remote #'str-to-char-ls
               #'white-space #'blanks #'words
               #'punctuations #'git-refs)
     remote-file
    )))

(defun load-git-remotes (repo &optional branch)
  (let* ((branchname (if (and branch) branch (git-current-branch repo)))
         (dir (str-cat blip-repo-meta repo "/br/" branchname "/"))
         (remote-file (str-cat dir "git.remotes")))
    (file-to-form remote-file)))

(defun extract-git-refs (ast)
  (let ((ref-ls '()))
    (walk-tree ast #'is-git-ref
               #'(lambda (x p w)
                   (if (is-git-ref x)
                       (pushr! ref-ls x))) '() '())
    ref-ls))

(defun fetchable-remote-targs (repo)
  (triples-to-targs (refs-to-triples
   (extract-git-refs (load-git-remotes repo)))))

(defun fetch-git-remotes (repo)
  (map 'nil #'(lambda (targ) (git-fetch-remote repo targ))
       (fetchable-remote-targs repo)))

(defun load-git-log (repo &optional branch)
  (let ((branchname (if (and branch) branch (git-current-branch repo))))
    (file-to-form (str-cat blip-repo-meta repo "/br/" branchname "/" "git.log"))))

(defun cache-git-log-urepo (svc user repo)
  (cache-git-log (str-cat svc "/" user "/" repo))
  )

(defun load-git-log-urepo (svc user repo)
  (load-git-log (str-cat svc "/" user "/" repo))
  )

(defun is-commit (ls)
  (and (listp ls) (match-str-list "commit" (car ls))))

(defun is-file-mod (ls)
  (and (listp ls) (is-word-group (car ls)) (= 1 (length (car ls)))))

(defun is-commit-or-file-mod (ls)
  (or (is-commit ls) (is-file-mod ls)))

(defun file-mod-stringify-path (ls)
  (if (is-file-mod ls)
  (char-ls-to-str (cddr (reduce #'append ls)))))


(defun print-form (n s w)
  (if (is-commit n)
      (print (caddr n))
      (print (file-mod-stringify-path n))
      ))


(defun build-ast-dir (repo)
  (pushdir (str-cat blip-asts repo))
  (map 'nil #'(lambda (f)
                (mkdir (str-cat (cwd) "/" f)))
       (git-show-ftree-all-time repo))
  (popdir)
  )

(defun build-meta-dir (repo)
  (let* ((dir (str-cat blip-repo-meta repo "/root")))
    (mkdir dir)
    (pushdir dir)
    (map 'nil #'(lambda (f)
                  (mkdir (str-cat (cwd) "/" f)))
         (git-show-ftree-all-time repo))
    (popdir)
    )
  )

(defun strap-git-repo (repo)
  (build-meta-dir repo)
  (build-ast-dir repo))

(defun git-show-ftree (repo commit)
  (assert (and repo commit))
  (let ((tree nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf tree
          (inferior-shell:run/ss
           (list "git" "ls-tree" "-r" "--name-only"
                 "--full-tree" commit)))
    (popdir)
    (str-split "\\n" tree)
    ))

(defun files-present-at-commit (repo commit)
  (git-show-ftree repo commit))

(defun git-show-ftree-all-time (repo)
  (let ((tree nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf tree
          (inferior-shell:run/ss
           (list "git" "log" "--pretty=format:" "--name-only"
                 "--diff-filter=A")))
    (popdir)
    (sort (remove-duplicates
           (remove-if #'(lambda (e) (equal e ""))
                      (str-split "\\n" tree)) :test #'equal)
          #'string<)
    )
  )

(defun git-head-commit (repo)
  (let ((hc nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf hc (inferior-shell:run/ss (list "git" "rev-parse" "HEAD")))
    (popdir)
    hc
    ))

(defun git-root-commits (repo)
  "Used to get root commits (like the first commit, and merges)"
  (let ((rc nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf rc (inferior-shell:run/ss (list "git" "rev-list"
                                          "--max-parents=0"
                                          "HEAD")))
    (popdir)
    (str-split "\\n" rc)
    ))

(defun git-file-latest-commit (repo path)
  "Only works for files that are present in current branch"
  (let ((lc nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf lc (inferior-shell:run/ss (list "git" "rev-list"
                                          "-1" "HEAD"
                                          path)))
    (popdir)
    lc
    ;(str-split "\\n" lc)
    ))

(defun git-file-latest-commit-until (repo path commit)
  "Only works for files that are present in current branch"
  (let ((lc nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf lc (inferior-shell:run/ss (list "git" "rev-list"
                                          "-1" commit
                                          path)))
    (popdir)
    lc
                                        ;(str-split "\\n" lc)
    ))

(defun git-file-all-commits (repo path)
  "Only works for files that are present in current branch"
  (let ((lc nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf lc (inferior-shell:run/ss (list "git" "rev-list"
                                          "HEAD"
                                          path)))
    (popdir)
    (str-split "\\n" lc)
    ))

(defun git-amend-log-misdeletion (repo file commit)
  (let* ((outdir (str-cat blip-repo-meta repo "/amends/"))
         (out (str-cat outdir commit))
         (append-to nil))
    (mkdir outdir)
    (setf append-to (file-to-form out))
    (form-to-file (remove-duplicates (pushr append-to (list 'misdeletion file)))
                  out)
    )
  )

(defun load-git-log-amendments (repo commit)
  (let* ((file (str-cat blip-repo-meta repo "/amends/" commit)))
    (file-to-form file)
    )
  )

(defun is-misdeleted (amends file)
  (let ((ret nil))
    (map 'nil #'(lambda (a)
                  (if (and (equal (car a) 'misdeletion)
                           (equal (cadr a) file))
                      (setf ret t)))
         amends)
    ret
    )
  )

(defun apply-git-log-amends (repo commit file-list)
  (let* ((amends (load-git-log-amendments repo commit)))
    (remove nil (map 'list #'(lambda (f) (if (is-misdeleted amends f) nil f)) file-list))
    )
  )


(defun parse-x-files-at-commit (repo commit &key force suf pref parser whitelist)
  (expand-commit! repo commit)
  (let ((files (files-present-at-commit repo commit))
        (curbr (git-current-branch repo)))
    (pushdir (str-cat blip-repos repo))
    (git-branch-commit commit)
    (map 'nil #'(lambda (file)
                 (if (and (if (and suf) (is-str-suffix suf file) t)
                          (if (and pref) (is-str-prefix pref file) t)
                          )
                     (let* ((slot (str-cat blip-asts repo "/" file))
                            (revid (git-file-latest-commit repo file))
                            (fullpath (str-cat slot "/" revid))
                            (full-src-path (str-cat (cwd) "/" file)))
                       (cond
                         ((and (is-file-p full-src-path)
                               (or (and force (file-exists-p fullpath))
                                   (not (file-exists-p fullpath))))
                          (if (or (not whitelist)
                                  (member file whitelist :test #'equal))
                              (form-to-file
                               (funcall parser
                                        (file-to-char-ls full-src-path))
                               fullpath)))
                         ((not (is-file-p full-src-path))
                          (assert nil)
                         ))
                       )))
             files)
    (git-unbranch curbr)
    (popdir)
    ))

(defun expand-commit (repo commit)
  (if (equal commit :head)
      (git-head-commit repo)
      commit))

(lol:defmacro! expand-commit! (r c)
  `(setf ,c (expand-commit ,r ,c)))

(defun list-files-at-commit (repo commit &key suf pref)
  (expand-commit! repo commit)
  (let ((files (files-present-at-commit repo commit))
        (list '()))
    (assert files)
    (map 'nil #'(lambda (file)
                 (if (and (if (and suf) (is-str-suffix suf file) t)
                          (if (and pref) (is-str-prefix pref file) t)
                          )
                     (pushr! list file)))
             files)
    (stable-sort list #'string<)
    )
  )

(defun ast-path (repo file commit)
  (expand-commit! repo commit)
  (let* ((revid (git-file-latest-commit repo file)))
    (str-cat blip-asts repo "/" file "/" revid)))

(defun load-ast (repo file commit)
  (file-to-form (ast-path repo file commit)))

(defun js-path-cat-aux (str ls files)
  (let* ((dstr (str-cat str (car ls) "/"))
         (fstr (str-cat str (car ls) ".js"))
         (lfstr (str-cat str (car ls)))
         (istr (str-cat str (car ls) "/index.js"))
         (exists (member lfstr files))
         (fexists (member fstr files))
         (iexists (member istr files)) )
    (cond
      ((and ls (cdr ls))
       (js-path-cat-aux dstr (cdr ls) files))
      ((and ls (not (cdr ls)) fexists)
       (js-path-cat-aux fstr (cdr ls) files))
      ((and ls (not (cdr ls)) iexists)
       (js-path-cat-aux istr (cdr ls) files))
      ((and ls (not (cdr ls)) exists)
       (js-path-cat-aux lfstr (cdr ls) files))
      ((and t)
        str))))

(defun js-path-cat (pls files)
  (js-path-cat-aux "" pls files))

(defun expand-path (current path &key when-module)
  (let ((c (popr (str-split "/" current))))
    (if (or (= 1 (length path))
            (= 0 (length (car path))) ;an absolute path in the FS
            (CHAR/= #\. (char (car path) 0)))
        (if (not when-module)
            nil
            path)
        (progn
          (map 'nil
               #'(lambda (p)
                   (cond ((equal p "..")
                          (popr! c))
                         ((not (equal p "."))
                          (pushr! c p))))
             path)
          c)
        )))

(defun js-file-deps (repo file commit &optional files)
  (if (not files)
      (setf files (files-present-at-commit repo commit)))
  (let* ((ast (load-ast repo file commit))
         (reqs (get-require-arg0 ast))
         (exp-reqs (map 'list #'(lambda (r) (expand-path file r :when-module nil)) reqs))
         (raw (map 'list #'(lambda (r) (list file r)) (remove-if #'not exp-reqs)))
         (pretty (map 'list
                      #'(lambda (e)
                          (list (car e) (js-path-cat (cadr e) files))) raw)))
    pretty))

(defun js-all-file-deps (repo commit)
  (expand-commit! repo commit)
  (let* ((files (files-present-at-commit repo commit))
         (deps '()))
    (map 'nil #'(lambda (f) (pushr! deps (js-file-deps repo f commit))) files)
    (reduce #'append (remove-if #'not deps)))
  )

(defun js-cache-file-deps (repo commit &optional dep-table)
  (expand-commit! repo commit)
  (if (not dep-table)
      (setf dep-table (js-all-file-deps repo commit)))
  (let ((dir (str-cat blip-repo-meta repo "/deps")))
    (mkdir dir)
    (pushdir dir)
    (form-to-file dep-table (str-cat dir "/" commit))
    (popdir)
    )
  )

(defun js-load-file-deps (repo commit)
  (expand-commit! repo commit)
  (let* ((file (str-cat blip-repo-meta repo "/deps/" commit)))
    (file-to-form file)
  ))

(defun js-fdeps (repo commit)
  (expand-commit! repo commit)
  (let ((deps (js-load-file-deps repo commit)))
    (cond
      ((not deps)
       (js-cache-file-deps repo commit)
       (setf deps (js-load-file-deps repo commit))
       ))
    deps))


(defun what-requires (dep table)
  (let ((ret '()))
    (map 'nil #'(lambda (x) (if (equal dep (cadr x)) (pushr! ret (car x)))) table)
    ret)
  )

(defun requires-what (dep table)
  (let ((ret '()))
    (map 'nil #'(lambda (x) (if (equal dep (car x)) (pushr! ret (cadr x)))) table)
    ret)
  )

(defun get-column (list n)
  (remove-duplicates (map 'list #'(lambda (e) (car (popl-n e n))) list) :test #'equal))

(defun top-deps (table)
  (let* ((left (get-column table 0))
         (right (get-column table 1))
         (left-only '()))
    (map 'nil #'(lambda (e) (if (not (member e right :test #'equal)) (pushr! left-only e))) left)
    left-only))

(defun bottom-deps (table)
  (let* ((left (get-column table 0))
         (right (get-column table 1))
         (right-only '()))
    (map 'nil #'(lambda (e) (if (not (member e left :test #'equal)) (pushr! right-only e))) right)
    right-only))

(defun remove-dep (table dep side)
  (remove-if #'(lambda (x) (equal dep x)) table
             :key (if (equal side :left) #'car #'cadr)))

(defun mk-ticket (str)
  (mkdir (str-cat blip-tickets str)))

(defun test-fwapi-parse ()
  (let ((target "github/joyent/sdc-fwapi"))
    (parse-x-files-at-commit target (git-head-commit target)
                             :force t :suf ".js" :parser #'js-to-ast)
    ))

(defun test-mike-sdb-parse ()
  (let ((target "github/sdimitro/minions"))
    (parse-x-files-at-commit target (git-head-commit target)
                             :force t
                             :suf ".c"
                             :pref "sdb"
                             :parser #'c-to-ast)
    (parse-x-files-at-commit target (git-head-commit target)
                             :force t
                             :suf ".h"
                             :pref "sdb"
                             :parser #'c-to-ast)
    ))

(defun test-docker-parse ()
  (let ((target "github/joyent/sdc-docker"))
    (parse-x-files-at-commit target (git-head-commit target)
                             :force t :suf ".js" :parser #'js-to-ast)
    ))

(defun test-bunyan-parse ()
  (let ((target "github/trentm/node-bunyan"))
    (parse-x-files-at-commit target (git-head-commit target)
                             :force t :suf ".js" :parser #'js-to-ast)
    ))
(defun test-warden-parse ()
  (let ((target "github/joyent/node-restify-warden"))
    (parse-x-files-at-commit target (git-head-commit target)
                             :force t :suf ".js" :parser #'js-to-ast)
    ))

(defun test-vasync-parse ()
  (let ((target "github/davepacheco/node-vasync"))
    (parse-x-files-at-commit target (git-head-commit target)
                             :force t :suf ".js" :parser #'js-to-ast)
    ))

(defun test-async-parse ()
  (let ((target "github/caolan/async"))
    (parse-x-files-at-commit target (git-head-commit target)
                             :force t :suf ".js" :parser #'js-to-ast)
    ))

(defun test-tmux-parse ()
  (let ((target "github/caolan/async"))
    (parse-x-files-at-commit target (git-head-commit target)
                             :force t :suf ".c" :parser #'c-to-ast)
    ))

(defun test-cueball-parse ()
  (let ((target "github/joyent/node-cueball"))
    (parse-x-files-at-commit target (git-head-commit target)
                             :force t :suf ".js" :parser #'js-to-ast)
    ))

(defun test-mooremachine-parse ()
  (let ((target "github/joyent/node-mooremachine"))
    (parse-x-files-at-commit target (git-head-commit target)
                             :force t :suf ".js" :parser #'js-to-ast)
    ))

(defun test-docker-create-index ()
  (list "lib/backends/sdc/containers.js"
        (pipeline (load-ast "github/joyent/sdc-docker"
                            "lib/backends/sdc/containers.js"
                            :head)
                  #'index-all-js-paths)))

(defun test-mooremachine-ast ()
  (load-ast "github/joyent/node-mooremachine"
            "lib/fsm.js"
            :head))

(defun test-mooremachine-create-index ()
  (list "lib/fsm.js"
        (pipeline (load-ast "github/joyent/node-mooremachine"
                            "lib/fsm.js"
                            :head)
                  #'index-all-js-paths)))

(defun test-mike-create-index ()
  (list "sdb/libsdc/libsdb_lwrap.c"
        (pipeline (load-ast "github/sdimitro/minions"
                            "sdb/libsdb/libsdb_lwrap.c"
                            :head)
                  #'index-all-c-paths)))

(defun test-docker-print-index ()
  (print-js-paths (test-docker-create-index)))

(defun test-mooremachine-print-index ()
  (print-js-paths (test-mooremachine-create-index)))

(defun test-mooremachine-get-subtree (path &optional pov)
  (map 'list #'ast-to-str (get-path-subtree path (test-mooremachine-create-index) pov)))

(defun test-mike-print-index ()
  (print-js-paths (test-mike-create-index)))

(defun test-docker-print-index-raw ()
  (print-js-paths-raw (test-docker-create-index)))

(defun test-docker-get-subtree (path &optional pov)
  (map 'list #'ast-to-str (get-path-subtree path (test-docker-create-index) pov)))

(defun test-mike-get-subtree (path &optional pov)
  (map 'list #'ast-to-str (get-path-subtree path (test-mike-create-index) pov)))

(defun test-docker-prefix (pref &optional pov)
  (get-path-with-prefix pref (test-docker-create-index) pov))

(defun test-docker-suffix (suf &optional pov)
  (get-path-with-suffix suf (test-docker-create-index) pov))

(defun test-docker-uniq (&optional pov)
  (uniq-path (test-docker-create-index) :pov pov
                                        :fmt t
                                        :sort-path t
                                        :sort-count t))

(defun test-docker-path-depth (&optional pov)
  (paths-by-depth (test-docker-create-index)
                  :pov pov
                  :fmt t))

(defun test-mooremachine-path-depth (&optional pov)
  (paths-by-depth (test-mooremachine-create-index)
                  :pov pov
                  :fmt t))

(defun test-mike-path-depth (&optional pov)
  (paths-by-depth (test-mike-create-index)
                  :pov pov
                  :fmt t))

(defun test-mooremachine-prefix (pref &optional pov)
  (get-path-with-prefix pref (test-mooremachine-create-index) pov))

(defun format-alist (alist)
  (map nil #'(lambda (cell) (format t "~a ~a ~%" (car cell) (cdr cell))) alist))

(defun test-docker-word-count ()
  (format-alist (js-word-count
                 (load-ast "github/joyent/sdc-docker"
                           "lib/backends/sdc/containers.js" :head))))

(defun test-docker-fcall-count ()
  (format-alist (js-fcall-count
                 (load-ast "github/joyent/sdc-docker"
                           "lib/backends/sdc/containers.js" :head))))

(defun test-docker-fcall-params-count ()
  (format-alist (js-fcall-params-count
                 (load-ast "github/joyent/sdc-docker"
                           "lib/backends/sdc/containers.js" :head))))

(defun test-docker-fdef-count ()
  (format-alist (js-fdef-count
                 (load-ast "github/joyent/sdc-docker"
                           "lib/backends/sdc/containers.js" :head))))

(defun test-docker-fcommit-count ()
  (format-alist (file-commit-count (load-git-log "github/joyent/sdc-docker"))))

(defun test-docker-req-strs ()
  (let* ((repo "github/joyent/sdc-docker")
         (file "lib/backends/sdc/containers.js")
         (reqs (get-require-arg0 (load-ast repo file
                                           :head)))
         (exp-reqs (map 'list #'(lambda (r) (expand-path file r :when-module nil)) reqs)))

    (map 'list #'(lambda (r) (list file r)) (remove-if #'not exp-reqs))
  ))

(load "envs.lisp")
