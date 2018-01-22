;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Copyright 2017 Joyent, Inc.
;;; Copyright 2017 Nicholas Zivkovic

(load blip-quicklisp)
(ql:quickload "let-over-lambda")
(ql:quickload "cl-quickcheck")
(ql:quickload "drakma")
(ql:quickload "cl-ppcre")
;(ql:quickload "cl-graph")
;(ql:quickload "vecto")
(ql:quickload "cl-conspack")
(ql:quickload "lhstats")
;(ql:quickload "trivial-ssh")
(ql:quickload "inferior-shell") ; Convenient for synchronous command execution
(ql:quickload "cl-async") ; Libuv wrapper, convenient for background commands
(ql:quickload "blackbird") ;promises over cl-async
(ql:quickload "cl-ncurses")
(ql:quickload "iterate")
(ql:quickload "uuid")
(ql:quickload "ironclad")
;(ql:quickload "alexandria")
;Works, but not too good at validation
;(ql:quickload "unix-options")
(load "diff-sexp.lisp")

(defmacro defmacro! (&body body)
  "Shorthand for lol:defmacro"
  `(lol:defmacro! ,@body))

(defmacro! symb (&body args)
  "Shorthand for lol:symb"
  `(lol:symb ,@args))

(defmacro! mkstr (&body args)
  `(lol:mkstr ,@args)
  )

(defun repeat (x n)
  (let ((ls '())
        (i 0))
    (tagbody again
       (pushr! ls x)
       (incf i)
       (if (< i n)
           (go again)))
    ls))

(defmacro! dosquare (args &body body)
  (assert (= (length args) 3))
  (let ((x (car args))
        (y (cadr args))
        (ls (caddr args))
        )
    (assert (equal (type-of x) 'symbol))
    (assert (equal (type-of y) 'symbol))
    ;(assert (equal (type-of ls) 'cons))
    `(do-group (,x ,ls)
       (do-group (,y ,ls)
         ,@body
         ))
    )
  )

(defmacro! diff-forms (a b)
  `(mw-diff-sexp:diff-sexp ,a ,b))

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

(defun pushlr (ls &rest elems)
  (pushr (pushl ls elems) elems)
  )

(defun pushrl (ls &rest elems)
  "Same as above. But added for symmetry"
  (pushl (pushr ls elems) elems)
  )

(defmacro! pushl! (ls &rest elems)
  "Same as pushl, but overwrites the list"
  `(setf ,ls (pushl ,ls ,@elems)))

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

(defun pipeline-old (arg &rest fns)
  "Takes a list of 1 args funcs, and chains them in a pipeline, so that each
   function takes the return value of the previous function as its argument"
  (iter:iter
    (iter:for f in fns)
    (iter:for (values ret time) initially (values arg 0) then (timed-funcall f ret))
    (iter:after-each (iter:collect (list f time) into timing))
    (iter:finally (return (values ret timing)))))

(defun pipeline (arg &rest fns)
  (let ((ret arg))
    (do-group (f fns)
      (setf ret (funcall f ret))
      )
    ret
    )
  )

(defmacro! pipeline+ (arg &rest fns)
  (let (
        (ret '())
        )
    (setf ret (list (car fns) arg))
    (do-group (f (cdr fns))
      (setf ret (list f ret))
      )
    ret
    )
  )

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

(defun string-to-symbol (s &optional case-sensitive)
  (if (and s)
      (intern (if case-sensitive s (string-upcase s)))
      nil))

(defun symbol-to-string (s &optional upcase)
  (if (and upcase)
      (symbol-name s)
      (string-downcase (symbol-name s))
      )
  )

(defun string-to-keyword (s)
  (if (and s)
      (intern (string-upcase s) "KEYWORD")
      nil))

(defun symbol-to-keyword (s)
  (string-to-keyword (symbol-to-string s)))

(defun keyword-to-string (k &optional upcase)
  (symbol-to-string k  upcase)
  )

(defun str-cat-2 (s1 s2)
  "Concatenates 2 strings"
  (concatenate 'string s1 s2))

(defun str-cat (&rest strings)
  "Concatenates 2 or more strings"
  (reduce #'str-cat-2 strings))

(defun str-last-char (s)
  "Gets the last character of a string"
  (char s (- (length s) 1)))

(defun is-str-suffix-one (suf str)
  "Checks if the strings has the given suffix"
  (if (> (length suf) (length str))
      nil
      (let* ((off (- (length str) (length suf)))
             (end (subseq str off (length str))))
        (string= suf end)
        )))

(defun is-str-prefix-one (pre str)
  "Checks if the strings has the given prefix"
  (if (> (length pre) (length str))
      nil
      (let* ((off (length pre))
             (end (subseq str 0 off)))
        (string= pre end)
        )))

(defun fn-or (&rest args)
  "Apparently `or` is a macro, not a function, so we can't just pass it as such"
  (let ((cur args))
    (tagbody again
       (cond
         ((not cur)
          (return-from fn-or nil))
         ((not (car cur))
          (setf cur (cdr cur))
          (go again))
         ((and t)
          (return-from fn-or t))
         )
       )
    )
  )

(defmacro! def-are-strs-*ix (ls-name str-name)
  `(defun ,ls-name (*s str)
     "Applies is-str-suffix-str over a list, returns true if any match"
     (apply #'fn-or (map 'list #'(lambda (s) (,str-name s str)) *s))
     ))

(def-are-strs-*ix are-strs-suffix is-str-suffix-one)
(def-are-strs-*ix are-strs-prefix is-str-prefix-one)

(defmacro! def-is-str-*ix (gen-name ls-name str-name)
  `(defun ,gen-name (*/s str)
     (if (listp */s)
         (,ls-name */s str)
         (,str-name */s str)
         )
     ))

(def-is-str-*ix is-str-suffix are-strs-suffix is-str-suffix-one)
(def-is-str-*ix is-str-prefix are-strs-prefix is-str-prefix-one)


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

(defun str-split (regex str &key unlistify)
  "Splits the string at each place that the given pcre regex is matched"
  (let ((res (cl-ppcre:split regex str))
        )
    (if (and unlistify (= 1 (length res)))
        (setf res str)
        )
    res
    )
  )

(defun str-matches (regex str)
  "returns nil if there is not match in str, t otherwise"
  (if (cl-ppcre:all-matches regex str) t nil)
  )

(defun list-split (elem ls)
  (let ((ret '())
        (sub-ls '())
        )
    (do-group (e1 e2 ls)
      (when (not (equal e1 elem))
        (pushr! sub-ls e1)
        )
      (when (and sub-ls (or (not e2) (equal e2 elem)))
        (pushr! ret sub-ls)
        (setf sub-ls '())
        )
      )
    ret
    )
  )

(defun str-replace (regex targ str)
  (cl-ppcre:regex-replace-all regex str targ))

(defun is-char-ls-prefix (pref ls)
  (let ((str (char-ls-to-str (head-n ls (length pref))))
        )
    (is-str-prefix pref str)
    )
  )

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

(defvar blip-dirs '())
(defvar blip-files '())

(defun blip-dir (dir)
  ;(pushr! blip-dirs dir)
  (mkdir dir)
  dir)

(defun blip-file (file)
  (pushr! blip-files file)
  file)


;;; All of these dirs get created at compile-time. This file must be in the
;;; `blip-self-repo` dir (see below).
(mkdir blip-root)
(defvar blip-stor (blip-dir (str-cat blip-root "stor/")))
(defvar blip-tickets (blip-dir (str-cat blip-stor "tickets/")))
(defvar blip-meta (blip-dir (str-cat blip-root "meta/")))
(defvar blip-tmp (blip-dir (str-cat blip-meta "tmp/")))
(defvar blip-du (blip-dir (str-cat blip-meta "disk-usage/")))
(defvar blip-bin (blip-dir (str-cat blip-meta "bin/")))
(mkdir (str-cat blip-bin "/" blip-flavor "/"))
(defvar blip-env (blip-dir (str-cat blip-meta "env/")))
(defvar blip-in-xform nil)
(defvar blip-xform-args nil)
(defvar blip-xform (blip-dir (str-cat blip-meta "xform/")))
(defvar blip-env-stack (blip-file (str-cat blip-env "stack")))
(defvar blip-env-avail (blip-file (str-cat blip-env "avail")))
(defvar blip-env-sha (blip-file (str-cat blip-env "sha")))
(defvar blip-core (str-cat blip-bin "/" blip-flavor "/blip"))
(defvar blip-logs (blip-dir (str-cat blip-meta "logs/")))
(defvar blip-repos (blip-dir (str-cat blip-stor "repos/")))
(defvar blip-asts (blip-dir (str-cat blip-stor "repo-asts/")))
(defvar blip-repo-meta (blip-dir (str-cat blip-stor "repo-meta/")))
(defvar blip-repos-index (blip-dir (str-cat blip-stor "repos-index/")))
(defvar blip-self-repo (blip-dir (str-cat blip-repos "blip/")))
(defvar blip-code (str-cat blip-self-repo "blip.lisp"))
(defvar blip-env-cfg (blip-file (str-cat blip-env "env-cfg.lisp")))
(defvar github-base-url "https://github.com/")
(defvar npm-base-url "https://registry.npmjs.com/")
(defvar gerrit-base-url "https://cr.joyent.us/p/")
(defvar github-api-url "https://api.github.com/")
(defvar blip-github-users (blip-file (str-cat blip-repos-index "github-users")))
(defvar blip-fmt-out t)
(defvar blip-err-out *error-output*)
(defvar blip-pre-loaded-envs nil)


(defun set-blip-fmt-out (s)
  (setf blip-fmt-out s)
  )

(defun set-blip-err-out (s)
  (setf blip-err-out s)
  )

(defun pre-load-envs (&rest envs)
  (setf blip-pre-loaded-envs envs)
  nil
  )

(defun pre-load-env (&rest envs)
  "For backwards compatibility"
  (apply #'pre-load-envs envs)
  )

(defmacro! puts (fmtstr &rest vars)
  (let ((ctl-str (str-cat fmtstr "~%")))
    `(progn
       (format blip-fmt-out ,ctl-str ,@vars)
       )
    )
  )

(defmacro! e-puts (fmtstr &rest vars)
  (let ((ctl-str (str-cat fmtstr "~%")))
    `(progn
       (format blip-err-out ,ctl-str ,@vars)
       )
    )
  )

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

(defun load-top-env ()
  (cond
    ((and blip-pre-loaded-envs)
     (do-group (e blip-pre-loaded-envs)
       (pushenv e)
       )
     )
    ((and t)
     (pushenv (load-env (car (last (file-to-form blip-env-stack)))))
     )
    )
  )

(defun load-xform (s)
  (load (str-cat blip-xform s))
  )

(defun run-xform (n)
  (let ((verbose nil))
    (cond
      ((not (string= (car n) "-v"))
       (setf blip-in-xform (car n))
       (setf blip-xform-args (cdr n)))
      ((string= (car n) "-v")
       (setf verbose t)
       (setf blip-in-xform (cadr n))
       (setf blip-xform-args (cddr n)))
      )
    (cond
      ((not blip-in-xform)
       (print "Bad args")
       (print n)
       (return-from run-xform))
      )
    (if (and verbose)
        (load (str-cat blip-xform (cadr n) ".lisp"))
        (quiet-load (str-cat blip-xform (car n) ".lisp"))
        )
    )
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
  (puts "~A~%" form)
  )

(defun mk-blip-dirs ()
  (do-group (d blip-dirs) (mkdir d))
  )

(defun print-row-strs (n ls)
  (let ((i 0))
    (do-group (s ls)
      (cond
        ((= i n)
         (print "~%")
         (setf i 0)
         )
        )
      (print "~d ")
      (incf i)
      )
    )
  )

(defvar cli-help-table '())

(defun is-flag-or-param (str)
  (and str (stringp str) (> (length str) 2)
       (equal "--" (subseq str 0 2)))
  )

(defun get-arg (pos nouns)
  (let ((n (nthcadr pos nouns)))
    (cond
      ((is-flag-or-param n)
       (e-puts "Not expecting param or flag arg.")
       (sb-ext:exit :code -1)
       )
      ((equal nil n)
       (e-puts "Expecting argument at position ~a." pos)
       (sb-ext:exit :code -1)
       )
      )
    (str-split "," n :unlistify t)
    )
  )

(defun get-opt-arg (pos nouns)
  (let ((n (nthcadr pos nouns)))
    (cond
      ((is-flag-or-param n)
       nil
       )
      ((and t)
       (str-split "," n :unlistify t)
       )
      )
    )
  )

(defun get-flag-arg (name nouns)
  (do-group (n nouns)
    (cond
      ((and (is-flag-or-param n)
            (equal name (subseq n 2 (length n))))
       (return-from get-flag-arg t)
       )
      )
    )
  nil
  )

(defun get-param-arg (name nouns)
  (do-cons (ns nouns)
    (let ((n (car ns))
          (n+1 (cadr ns)))
      (cond
        ((and (is-flag-or-param n)
              (equal name (subseq n 2 (length n))))
         (cond
           ((not (is-flag-or-param n+1))
            (return-from get-param-arg (str-split "," (cadr ns) :unlistify t))
            )
           ((is-flag-or-param n+1)
            (e-puts "Expect an argument for param ~a" n)
            (sb-ext:exit :code -1)
            )
           )
         )
        )
      )
    )
  nil
  )

(defmacro! def-cli-verb (name &key args opt-args flags
                              params body help)
  "This defines a new cli command. Args are the non-optional arguments. They are
   simple strings, and can't start with '--'. opt-args are optional args that
   come right after args. flags are any strings that start with '--' and don't
   an argument. params are any strings that start with '--' and do take an
   argument. body is just a block of code that will be executed.

   All arguments, optional-arguments, and params will be checked for the
   presence of a comma (,). If there is at least 1 comma, the argument is
   interpreted as a list. Nested lists are not possible.

   This macro also builds up the list that will be used to print the
   help-output. You can't define a help verb -- that gets defined implicitly.
   You specify an alternative help message for the command.

   Every name _must_ be unique, even if they are of different types.

   Use this macro inside of the def-cli macro.

   This macro expects two variables pre-defined in its scope: a string named
   'verb' and an argv list named 'nouns'."
  (let ((name-list '())
        (help-rec '())
        (lex-bindings '((noun nouns)))
        (arg-num 0)
        )
    (pushr! help-rec name)
    (do-group (a args)
      (assert (not (member a name-list)))
      (pushr! name-list a)
      (pushr! help-rec a)
      (pushr! lex-bindings (list (string-to-symbol a)
                                 (list 'get-arg arg-num 'nouns)))
      (incf arg-num)
      )
    (do-group (a opt-args)
      (assert (not (member a name-list)))
      (pushr! name-list a)
      (pushr! help-rec (str-cat "[" a "]"))
      (pushr! lex-bindings (list (string-to-symbol a)
                                 (list 'get-opt-arg arg-num 'nouns)))
      (incf arg-num)
      )
    (setf arg-num 0)
    (do-group (a flags)
      (assert (not (member a name-list)))
      (pushr! name-list a)
      (pushr! help-rec (str-cat "--" a))
      (pushr! lex-bindings (list (string-to-symbol (str-replace "--" "" a))
                                 (list 'get-flag-arg a 'nouns)))
      )
    (do-group (a params)
      (assert (not (member a name-list)))
      (pushr! name-list a)
      (pushr! help-rec (str-cat "--" a))
      (pushr! help-rec (str-cat "$" a))
      (pushr! lex-bindings (list (string-to-symbol (str-replace "--" "" a))
                                 (list 'get-param-arg a 'nouns)))
      )
    (if (and help)
        (setf help-rec help)
        )
    (pushr! cli-help-table help-rec)
    `(if (is-cmd-verb ,name)
         (let (,@lex-bindings)
           ,@body
           (return)
           )
         )
    )
  )

(defmacro! def-cli (verb nouns &body body)
  `(block nil
     (let ((verb ,verb) (nouns ,nouns))
       ,@body
       (if (is-cmd-verb "help")
           (progn
             (do-group (e cli-help-table)
               (puts "~a" (apply #'str-cat (flatten (intersperse e " " t))))
               )
             (return)
             )
           )
       (e-puts "Bad verb!")
       (sb-ext:exit :code -1)
       )
     )
  )


(defun quotify-list (ls)
  (if (listp ls) (list 'quote ls) ls)
  )

(defmacro! def-cli-verb-env (verb)
  `(def-cli-verb ,verb
     :args ("name" "repo" "commit")
     :flags ("verbose" "set")
     :params ("pref" "antipref" "depth" "req" "style")
     :body (
            (let ((envs (file-to-forms blip-env-cfg))
                  (entry '())
                  )
              (setf commit (string-downcase commit))
              (setf name (string-to-symbol name))
              (cond
                ((or set (not (member-if #'(lambda (n) (equalp name n))
                                         envs :key #'cadr)))
                 (pushr! entry (string-to-symbol verb))
                 (pushr! entry name repo)
                 (pushr! entry (if (equal commit "head") '(quote head) commit))
                 (if (and verbose)
                     (pushr! entry :verbose verbose)
                     )
                 (if (and pref)
                     (pushr! entry :pref (quotify-list pref))
                     )
                 (if (and antipref)
                     (pushr! entry :antipref (quotify-list antipref))
                     )
                 (if (and depth)
                     (pushr! entry :depth depth)
                     )
                 (if (and req)
                     (pushr! entry :req req)
                     )
                 (if (and style)
                     ;(puts "style ~a" style)
                     (pushr! entry :style (string-to-symbol style))
                     )
                 (puts "entry ~a" entry)
                 (if (and set)
                     (setf envs (remove-if #'(lambda (n) (equal name n))
                                           envs :key #'cadr))
                     )
                 (pushr! envs entry)
                 (forms-to-file envs blip-env-cfg)
                 ;;; TODO write the file, but test this out first
                 )
                ((and t)
                 (e-puts "Env with name ~d already exists." name)
                 )
                )
              )
            ;;; We want to modify env-cfg.lisp and add a new entry if it does
            ;;; not exist.
            )
    )
  )

(defun hash-to-list (ht &key cmp key)
  (let ((ret '()))
    (maphash #'(lambda (k v) (pushr! ret (list k v))) ht)
    (if (and cmp)
        (setf ret (sort ret cmp))
        )
    )
  )

(defun main-impl (argv)
  (def-cli (cadr argv) (cddr argv)
    (def-cli-verb "env-ls"
      :body ((let ((envs (file-to-forms blip-env-cfg))
                   (names '())
                   )
               (do-group (e envs)
                 (pushr! names (symbol-to-string (cadr e)))
                 )
               (setf names (sort names #'string<))
               (do-group (n names)
                 (puts "~a" n)
                 )
               )
             )
      )
    (def-cli-verb "env-stack"
      :body ((puts "~A" (file-to-form blip-env-stack)))
     )
    (def-cli-verb "top-env"
      :body ((load-top-env)
             (puts "~A" (get-env-var 'name))
             )
     )
    (def-cli-verb "pushenv"
      :args ("env-name")
      :body (
             (let ((env (string-to-symbol env-name)))
               (if (have-env env)
                   (form-to-file (pushr (file-to-form blip-env-stack)
                                        env)
                                 blip-env-stack)
                   )
               )
             )
      )
    (def-cli-verb "popenv"
      :body (
             (form-to-file (popr (file-to-form blip-env-stack))
                           blip-env-stack)
             )
     )
    (def-cli-verb "index-prefix"
      :args ("path")
      :opt-args ("pov")
      :flags ("force")
      :params ("page" "type")
      :body (
             (load-top-env)
             (index-prefix path (string-to-keyword pov)
                           :page page :force force
                           :alt-idx-type type)
             )
     )
    (def-cli-verb "index-suffix"
      :args ("path")
      :opt-args ("pov")
      :flags ("force")
      :params ("page" "type")
      :body (
             (load-top-env)
             (index-suffix path (string-to-keyword pov)
                           :page page :force force
                           :alt-idx-type type)
             )
      )
    (def-cli-verb "index-word-count"
      :args ("path")
      :opt-args ("pov")
      :flags ("force")
      :params ("page" "type")
      :body (
             (load-top-env)
             (index-word-count path (string-to-keyword pov)
                               :page page :force force
                               :alt-idx-type type)
             )
      )
    (def-cli-verb "index-uniq"
      :opt-args ("pov")
      :flags ("force")
      :params ("page" "type")
      :body (
             (load-top-env)
             (index-uniq (string-to-keyword pov)
                               :page page :force force
                               :alt-idx-type type)
             )
      )
    (def-cli-verb "index-path-depth"
      :opt-args ("pov")
      :flags ("force")
      :params ("page" "type")
      :body (
             (load-top-env)
             (index-path-depth (string-to-keyword pov)
                         :page page :force force
                         :alt-idx-type type)
             )
      )
    (def-cli-verb "index-print"
      :opt-args ("pov")
      :flags ("force")
      :params ("page" "type")
      :body (
             (load-top-env)
             (index-print (string-to-keyword pov)
                               :page page :force force
                               :alt-idx-type type)
             )
      )
    (def-cli-verb "index-print-sort"
      :opt-args ("pov")
      :flags ("force")
      :params ("page" "type")
      :body (
             (load-top-env)
             (index-print-sort (string-to-keyword pov)
                          :page page :force force
                          :alt-idx-type type)
             )
      )
    (def-cli-verb "index-build"
      :flags ("force")
      :params ("type")
      :body (
             (load-top-env)
             (index-build :force force :alt-idx-type type)
             )
      )
    (def-cli-verb "index-get-subtrees"
      :args ("path")
      :opt-args ("pov")
      :flags ("force")
      :params ("page" "type")
      :body (
             (load-top-env)
             (index-get-subtrees path (string-to-keyword pov)
                                 :page page :force force
                                 :alt-idx-type type)
             )
      )
    (def-cli-verb "index-get-subtrees-str"
      :args ("path")
      :opt-args ("pov")
      :flags ("force" "unfold")
      :params ("page" "type")
      :body (
             (load-top-env)
             (index-get-subtrees-str path (string-to-keyword pov)
                                     :page page :force force
                                     :alt-idx-type type :unfold unfold)
             )
      )
    (def-cli-verb "ast-ls-files"
      :flags ("count")
      :body (
             (load-top-env)
             (let ((files (ast-ls-files)))
               (print-ln (if count (length files) files))
               )
             )
     )
    (def-cli-verb "ast-ls-fcalls"
      :flags ("count" "force")
      :params ("pref")
      :body (
             (load-top-env)
             (print-ln (ast-ls-fcalls count :pref pref :force force))
             )
     )
    (def-cli-verb "ast-ls-fdefs"
      :flags ("count" "force")
      :params ("pref")
      :body (
             (load-top-env)
             (print-ln (ast-ls-fdefs count :pref pref :force force))
             )
     )
    (def-cli-verb "ast-ls-words"
      :flags ("count" "force")
      :params ("pref")
      :body (
             (load-top-env)
             (print-ln (ast-ls-words count :pref pref :force force))
             )
      )
    (def-cli-verb "ast-ls-fbinds"
      :flags ("count" "force")
      :params ("pref")
      :body (
             (load-top-env)
             (print-ln (ast-ls-fbinds count :pref pref :force force))
             )
      )
    (def-cli-verb "ast-parse"
      :flags ("force")
      :body (
             (load-top-env)
             (ast-parse force)
             )
     )
    (def-cli-verb "ast-dump-file"
      :args ("file")
      :body (
             (load-top-env)
             (puts "~d"
                   (char-ls-to-str
                    (flatten
                      (load-ast (get-env-var 'repo)
                                file
                                (get-env-var 'commit)))))
             )
     )
    (def-cli-verb "ast-stringify"
      :args ("file")
      :body (
             (load-top-env)
             (puts "~a"
                   (stringify-ast-leaves
                      (load-ast (get-env-var 'repo)
                                file
                                (get-env-var 'commit))))
             )
      )
    (def-cli-verb "what-requires"
      :args ("file")
      :flags ("force")
      :body (
             (load-top-env)
             (print-ln (ast-what-requires file :force force))
             )
     )
    (def-cli-verb "requires-what"
      :args ("file")
      :flags ("force")
      :body (
             (load-top-env)
             (print-ln (ast-requires-what file :force force))
             )
      )
    (def-cli-verb "bottom-deps"
      :args ("file")
      :flags ("force")
      :body (
             (load-top-env)
             (print-ln (ast-bottom-deps :force force))
             )
     )
    (def-cli-verb "top-deps"
      :flags ("force")
      :body (
             (load-top-env)
             (print-ln (ast-top-deps :force force))
             )
     )
    (def-cli-verb  "what-exports"
      :args ("name")
      :body (
             (load-top-env)
             (print-ln (ast-what-exports name))
             )
     )
    (def-cli-verb  "exports-what"
      :args ("name")
      :body (
             (load-top-env)
             (print-ln (ast-exports-what name))
             )
      )
    (def-cli-verb "reconstruct-repo"
      :body (
             (load-top-env)
             (reconstruct-repo)
             )
     )
    (def-cli-verb "xform"
      :help ("xform" "-v" "xform-name" "args...")
      :body (
             (load-top-env)
             (run-xform nouns)
             )
      )
    (def-cli-verb "github-users"
      :body ((print-ln (file-to-form blip-github-users)))
     )
    (def-cli-verb "github-user-add"
      :args ("user-name")
      :body (
             (let ((users (file-to-form blip-github-users)))
               (cond
                 ((and user-name (not (member user-name users :test #'equal)))
                  (pushr! users user-name)
                  (form-to-file users blip-github-users)
                  )
                 )
               )
             )
     )
    (def-cli-verb "github-user-rem"
      :args ("user-name")
      :body (
             (let ((users (file-to-form blip-github-users)))
               (cond
                 ((and user-name (member user-name users :test #'equal))
                  (setf users (remove-if  #'(lambda (u) (string= u user-name))
                                          users))
                  (form-to-file users blip-github-users)
                  )
                 )
               )
             )
      )
    (def-cli-verb "github-user-clone"
      :args ("user-name")
      :body (
             (cond
               ((and user-name)
                (cache-svc-user-repo-list "github" user-name)
                (github-clone-user-all-bg user-name)
                ))
             )
      )
    (def-cli-verb "github-user-pull"
      :args ("user-name")
      :body (
             (cond
               ((and user-name)
                (cache-svc-user-repo-list "github" user-name)
                (github-pull-user-all-bg user-name)
                ))
             )
      )
    (def-cli-verb "github-blacklist"
      :args ("repo-name")
      :params ("user")
      :body (
             )
      )
    (def-cli-verb "pull-env"
      :body (
             (load-top-env)
             (pushdir (str-cat blip-repos (get-env-var 'repo)))
             (inferior-shell:run/ss (list "git" "pull"))
             (popdir)
             )
     )
    (def-cli-verb "strap-repo"
      :body (
             (load-top-env)
             (strap-git-repo (get-env-var 'repo))
             )
     )
    (def-cli-verb "eval"
      ;;; WTF does this do?
      :body (
             (load-top-env)
             (if (cadr nouns)
                 (form-to-file (eval (read-from-string (car nouns)))
                               (str-cat blip-root (car nouns)))
                 (print (eval (read-from-string (car nouns))))
                 )
             )
     )
    (def-cli-verb "perf-ls"
      :body (
             (walk-directory blip-logs
                            #'(lambda (nm)
                                (puts "~a" (pathname-name nm))
                                )
                            )
             )
        )
    (def-cli-verb "perf-find"
      :params ("verb")
      )
    (def-cli-verb "perf-stack-time"
      :opt-args ("timestamp")
      ;;; Note that we ignore the "true" latest one because that one is the
      ;;; log for this very run of perf-stack-stack.
      :flags ("latest" "fg-compat")
      :body ((perf-x-time vdefun-stack-time)
             )
      )
    (def-cli-verb "perf-func-time"
      :opt-args ("timestamp")
      ;;; Note that we ignore the "true" latest one because that one is the
      ;;; log for this very run of perf-stack-stack.
      :flags ("latest" "fg-compat")
      :body ((perf-x-time vdefun-func-time)
             )
      )
    (def-cli-verb-env "js-env")
    (def-cli-verb-env "c-env")
    (def-cli-verb "env-show"
      :args ("name")
      :body ((let* ((envs (file-to-forms blip-env-cfg))
                    (env (find (string-to-symbol name) envs :key #'cadr))
                    (cmd '())
                    )
               (do-group (elem env)
                 (cond
                   ((keywordp elem)
                    (pushr! cmd (str-cat "--" (keyword-to-string elem)))
                    )
                   ((symbolp elem)
                    (pushr! cmd (symbol-to-string elem))
                    )
                   ((stringp elem)
                    (pushr! cmd elem)
                    )
                   ((and (consp elem) (equal (car elem) 'quote)
                         (symbolp (cadr elem)))
                    (pushr! cmd (symbol-to-string (cadr elem)))
                    )
                   ((and (consp elem) (equal (car elem) 'quote)
                         (listp (cadr elem)))
                    (pushr! cmd (apply #'str-cat (intersperse (cadr elem) "," t)))
                    )
                   ((listp elem)
                    (pushr! cmd (apply #'str-cat (intersperse elem "," t)))
                    )
                   )
                 )
               (prin1 env)
               (format blip-fmt-out "~%")
               (puts "~a" (apply #'str-cat (intersperse cmd " " t)))
               )
             )
        )
    (def-cli-verb "env-rm"
      :args ("name")
      :body (
             (setf name (string-to-symbol name))
             (let ((envs (file-to-forms blip-env-cfg)))
               (setf envs
                     (remove-if #'(lambda (n) (equal n name))
                                envs :key #'cadr))
               (forms-to-file envs blip-env-cfg)
               )
             )
        )
    (def-cli-verb "sleep"
      :args ("seconds")
      :body ((sleep (parse-integer seconds)))
     )
    )
  )

(defun main ()
  "The main entry point for this program."
  (init-vdefun-stream)
  (set-blip-err-out *error-output*)
  (let* ((argv sb-ext:*posix-argv*))
    (stack-pushr blip-vdefun-log (list 'args argv))
    (stack-pushr blip-vdefun-log (list 'flavor blip-flavor))
    (main-impl argv)
    (cond
      ((and blip-vdefun-stream)
       (flush-vdefun-log)
       (finish-output blip-vdefun-stream)
       )
      )
    )
  )

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
      (puts "stand-alone core ~a saved" path))
    #-sbcl
    (error "not available on this lisp")
        (values)))


(defun install ()
  "This saves an executable core to the proper location"
  (save-core blip-core))


(defun append-ls (ls)
  (apply #'append ls))


(defclass stack ()
  ((list :initarg :list
         :initform '())
   (last :initarg :last
         :initform nil)
   (length :initarg :length
           :initform 0)
  ))

(defun stack-list (s)
  (slot-value s 'list))

(defun stack-length (s)
  (slot-value s 'length))

(defun stack-pushr (s e)
  (if (slot-value s 'list)
      (progn
        (setf (cdr (slot-value s 'last)) (cons e nil))
        (setf (slot-value s 'last) (cdr (slot-value s 'last))))
      (progn
        (setf (slot-value s 'list) (list e))
        (setf (slot-value s 'last) (slot-value s 'list))))
  (incf (slot-value s 'length)))

(defun stack-last-until (s test)
  (let ((last (stack-last s))
        (list (stack-list s)))
    (if (funcall test last)
        (return-from stack-last-until last)
        )
    (tagbody again
       (setf list (popr list))
       (setf last (car (last list)))
       (if (or (not last) (funcall test last))
           (return-from stack-last-until last)
           )
       (go again)
       )
    )
  )

(defun stack-last (s)
  (car (slot-value s 'last)))

(defun stack-set-last (s e)
  (setf (car (slot-value s 'last)) e))


(defun elem (arr i)
  (cond
    ((>= i (length arr))
     nil)
    ((and t)
     (elt arr i))
    )
  )

;;; TODO set a max-length that triggers a flush, this way we won't slow
;;; performance via creeping memory exhaustion.
(defvar blip-vd-logs (blip-dir (str-cat blip-meta "logs/")))
(defvar blip-vdefun-log-max 100)
(defvar blip-vdefun-enabled t)
(defvar blip-vdefun-log (make-instance 'stack))
(defvar blip-vdefun-stream )

(defun init-vdefun-stream ()
  (cond
    ((and blip-vdefun-enabled)
     (setf blip-vdefun-stream
           (open (str-cat blip-vd-logs
                          (write-to-string (get-universal-time)))
                 :direction :output :if-exists :supersede
                 :if-does-not-exist :create)
           ))))

(defun reset-vdefun-log ()
  (setf blip-vdefun-log nil)
  (setf blip-vdefun-log (make-instance 'stack)))

(defun flush-vdefun-log ()
  "This function serializes the log to a stream, and resets the data
   structure. This function 'flushes' the data structure from memory into the
   stream. The stream will periodically get drained into a file. Must call
   finish-output at the end of the program's lifetime to force the last few
   straggler bytes out onto disk."
  (cond
    ((and blip-vdefun-stream (stack-list blip-vdefun-log))
     (print (stack-list blip-vdefun-log) blip-vdefun-stream)
     ;(finish-output blip-vdefun-stream)
     )
    )
  (reset-vdefun-log)
  )

(defun load-vdefun-log (time)
  (apply #'append (file-to-forms-impl (str-cat blip-vd-logs (write-to-string time))))
  )

(defun vdefun-count-names (time)
  (let ((log (load-vdefun-log time))
        )
    (quantize (mapcar #'(lambda (r)
                          (caddr r)
                          )
                      log))
    )
  )

(defun vdefun-count-types (time)
  (let ((log (load-vdefun-log time))
        )
    (quantize (mapcar #'(lambda (r)
                          (cadr r)
                          )
                      log))
    )
  )

(defun vdefun-func-time (time)
  (let ((log (load-vdefun-log time))
        (dict (make-hash-table :test #'equal))
        (stack '())
        (target nil)
        (ret '())
        )
    (do-group (r log)
      (when (equal (car r) 'e)
        (pushr! stack (list (caddr r) (cadddr r)))
        )
      (when (equal (car r) 'r)
        (setf target (car (last stack)))
        (incf (gethash (car target) dict 0) (- (cadddr r) (cadr target)))
        (popr! stack)
        )
      )
    (maphash #'(lambda (k v)
                 (pushr! ret (list k v))
                 )
             dict
             )
    (sort ret #'< :key #'cadr)
    )
  )

(defun fmt-perf-stacks (pairs delim &key (placement 'after))
  (mapcar #'(lambda (p)
              (let ((stack (car p))
                    (str "")
                    )
                (cond
                  ((listp stack)
                   (do-group (s stack)
                     (cond
                       ((equal placement 'before)
                        (setf str (str-cat str delim (symbol-to-string s)))
                        )
                       ((equal placement 'after)
                        (setf str (str-cat str (symbol-to-string s) delim))
                        )
                       ((and t)
                        (assert nil)
                        )
                       )
                     )
                   (list str (cadr p))
                   )
                  ((and t)
                   (setf str (symbol-to-string stack))
                   (list str (cadr p))
                   )
                  )
                )
              )
          pairs)
  )

(defmacro! perf-x-time (func-name)
  `(let ((fnm1 nil)
         (fnm2 nil)
         (fmt-stacks '())
         )
     (cond
       ((and (not timestamp) (not latest))
        (e-puts "Expected either a timestamp or '--latest'")
        (sb-ext:exit :code 0)
        )
       ((and latest)
        (walk-directory blip-logs
                        #'(lambda (nm)
                            (setf fnm1 fnm2)
                            (setf fnm2 (pathname-name nm))
                            )
                        )
        (if (and fnm1)
            (setf timestamp (parse-integer fnm1)))
        )
       ((and timestamp)
        (setf timestamp (parse-integer timestamp))
        )
       )
     (if (not fg-compat)
         (puts "timestamp: ~a" timestamp))
     (setf fmt-stacks (fmt-perf-stacks (,func-name timestamp) ";"
                                       :placement 'after))
     (puts "hi ~a" fmt-stacks)
     (do-group (s fmt-stacks)
       (puts "~a ~a" (car s) (cadr s))
       )

     (if (not fg-compat)
         (puts "timestamp: ~a" timestamp))
     )
  )

(defun vdefun-stack-time (time)
  (let ((log (load-vdefun-log time))
        (dict (make-hash-table :test #'equal))
        (stack '())
        (name-stack '())
        (target nil)
        (ret '())
        )
    (puts "~A" (car log))
    (if (equal 'args (caar log))
        (setf log (cdr log))
        )
    (do-group (r log)
      (when (equal (car r) 'e)
        (pushr! stack (list (caddr r) (cadddr r)))
        (pushr! name-stack (caddr r))
        )
      (when (equal (car r) 'r)
        (setf target (car (last stack)))
        (incf (gethash name-stack dict 0) (- (cadddr r) (cadr target)))
        (popr! stack)
        (popr! name-stack)
        )
      )
    (maphash #'(lambda (k v)
                 (pushr! ret (list k v))
                 )
             dict
             )
    (sort ret #'< :key #'cadr)
    )
  )

(defun raw-args (args)
  (remove-if #'(lambda (s)
                 (is-str-prefix "&" (symbol-to-string s))
                 )
             args)
  )

(defun method-args (args)
  "Turn method args into function args: (var classname) -> var"
  (mapcar #'(lambda (arg)
              (if (listp arg)
                  (car arg)
                  arg)
              )
          args)
  )

(defun raw-method-args (args)
  (raw-args (method-args args))
  )

(defun replace-head-elem (elem with list)
  (cond
    ((and (listp list) (equal elem (car list)))
     (setf (car list) with)
     (replace-head-elem elem with (cdr list))
     )
    ((and (listp list) (listp (car list)))
     (replace-head-elem elem with (car list))
     )
    )
  list
  )

;;; The v-variants of defun and defmethod are intended to provide visibility
;;; into the performance of functions and methods that we define. This follows
;;; the convention set by joyent with the creation of vasync, verror, vstream,
;;; etc, for the node.js ecosystem. We currently store all the data in memory
;;; and flush it out when we're done. But we should also have the option for
;;; dtrace probes in the future.
(defmacro! vdefun (name args &body body)
  (let ((local-fn (gensym)))
    (cond
      ((and blip-vdefun-enabled)
       `(defun ,name ,args
          (labels ((,local-fn ,(raw-args `,args) ,@body))
            (let ((ret nil))
              (stack-pushr blip-vdefun-log (list 'e 'f ',name (get-internal-real-time)))
              (setf ret (multiple-value-list (,local-fn ,@(raw-args `,args))))
              (stack-pushr blip-vdefun-log (list 'r 'f ',name (get-internal-real-time)))
              (if (>= (stack-length blip-vdefun-log) blip-vdefun-log-max)
                  (flush-vdefun-log)
                  )
              (values-list ret)
              )
            )
          )
       )
      ((not blip-vdefun-enabled)
       `(defun ,name ,args ,@body)
       )
      )
    )
  )

(defmacro! vdefmethod (name args &body body)
  (cond
    ((and blip-vdefun-enabled)
     `(defmethod ,name ,args
        (labels ((local-fn ,(raw-method-args `,args) ,@body))
          (let ((ret nil))
            (stack-pushr blip-vdefun-log (list 'e 'm ',name (get-internal-real-time)))
            (setf ret (multiple-value-list (local-fn ,@(raw-method-args `,args))))
            (stack-pushr blip-vdefun-log (list 'r 'm ',name (get-internal-real-time)))
            (if (>= (stack-length blip-vdefun-log) blip-vdefun-log-max)
                (flush-vdefun-log)
                )
            (values-list ret)
            )
          )
        )
     )
    ((not blip-vdefun-enabled)
     `(defmethod ,name ,args ,@body)
     )
    )
  )

(vdefun vdtt (n &optional blah)
  (sleep n)
  )

(defclass foo ()
  ((bar :initarg :bar :initform '()))
  )

(vdefmethod get-bar ((f foo))
  (slot-value f 'bar)
  )

(vdefmethod set-bar ((f foo) v)
  (setf (slot-value f 'bar) v)
  )

(defun vd-timing-info ()
  (stack-list blip-vdefun-log)
  )

(defun empty-vdefun-log ()
  (not (stack-list blip-vdefun-log))
  )

(defun create-list-bindings (names-arr list)
  (let ((ret '()))
    (dotimes (i (length names-arr))
      (pushr! ret (list (elem names-arr i) `(car (nthcdr (+ ,i) ,list))))
      )
    ret
    )
  )

(defmacro! do-cons (args &body body)
  (assert (= (length args) 2))
  (let ((e (car args))
        (l (cadr args))
        )
    `(do ((,e ,l (cdr ,e))) ((not ,e) ,e)
       ,@body
       )
    )
  )

(defun parse-float (str)
  (let ((ret nil))
    (with-input-from-string (in str)
      (setf ret (read in))
      )
    (assert (floatp ret))
    ret
    )
  )

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


(defun poprl (ls)
  (popl (popr ls))
  )

(defun poplr (ls)
  (popr (popl ls))
  )


(defmacro! do-group (args &body body)
  "Just like dolist, except it allows arbitrary look-ahead"
  (assert (>= (length args) 2))
  (let* ((e (gensym))
         (es (coerce (popr args) 'vector))
         (g (car (last args)))
         (i (gensym)))
    (assert (symbolp e))
    `(progn
       (do-cons (,e ,g)
         (let (,@(create-list-bindings `,es `,e))
           ,@body
           )
         )
       )
    )
  )

(defmacro! try-each (&rest conds)
  "Conds is a list of lists. Each first elem is a test-body, and the remaining
  elems are the code to evaluate if test-body is true (just like the
  cond-macro). The difference from the cond-macro is that we keep going down the
  list of conds until our return value becomes non-nil (whereas cond stops at
  the first successful test, regardless of return value). Also, we have an
  implicit nil-return if everything returns nil or no branches are taken."
  (let ((body '())
        (if-inner'())
        (if-outer '())
        (ret (gensym))
        )
    (do-group (c conds)
      (pushr! if-inner 'if (car c) (append '(progn) (cdr c)) 'nil)
      (pushr! if-outer 'if (list 'setf ret if-inner) (list 'return ret))
      (pushr! body if-outer)
      (setf if-inner '())
      (setf if-outer '())
      )
    `(block nil
       (let ((,ret nil))
         ,@body
         nil
         )
       )
    )
  )


(defmacro! xor (&rest exprs)
  (let ((body '())
        (and-body '())
        (pos 0)
        )
    (pushr! body 'or)
    (dotimes (i (length exprs))
      (pushr! and-body 'and)
      (do-group (e exprs)
        (cond
          ((= i pos)
           (pushr! and-body e)
           )
          ((not (= i pos))
           (pushr! and-body (list 'not e))
           )
          )
        (incf pos)
        )
      (pushr! body and-body)
      (setf and-body '())
      (setf pos 0)
      )
    body
    )
  )

(defmacro! nand (&rest exprs)
  `(or (xor ,@exprs) (not (and ,@exprs)))
  )

(defmacro! nor (&rest exprs)
  `(and (not (xor ,@exprs)) (not (and ,@exprs)))
  )

(defmacro! xnor (&rest exprs)
  `(not (xor ,@exprs))
  )


(defun map-if (ls test function)
  "Maps over a list, ignoring elems that fail a condition"
  (loop
    for e in ls
    when (funcall test e)
      collect (funcall function e)))


(defun get-nth-page (ls n pagesz)
  (head-n (popl-n ls (* n pagesz)) pagesz))



(defun ngrams-aux (acc n list)
  (cond ((< (length list) n)
         (append acc '()))
        ((>= (length list) n)
         (ngrams-aux (pushr acc (head-n list n)) n (cdr list)))))

(defun ngrams (n list)
  "Computes all ngrams of a list"
  (ngrams-aux '() n list))

(defun quantize (list)
  (let ((tbl (make-hash-table :test #'equal))
        (list-tbl '()))
    (do-group (e list)
      (incf (gethash e tbl 0))
      )
    (maphash #'(lambda (k v)
                 (pushr! list-tbl (list k v))
                 )
             tbl)
    (sort list-tbl #'< :key #'cadr)
    )
  )

;;; Some basic string/char-list conversion funcs.

(defun str-to-char-ls (str)
  "Convert string to character list"
  (coerce str 'list))

(defun char-ls-to-str (cls)
  "Convert character list to string"
  (coerce cls 'string))

(defun is-char-ls (cls)
  (if (not (listp cls))
      (return-from is-char-ls nil)
      )
  (do-group (c cls)
    (cond
      ((not (characterp c))
       (return-from is-char-ls nil)
       )
      )
    )
  t
  )

;;; White-space grouping and related boolean tests.

(defun is-white-space (c)
  "Test if a character is white space"
  (and c (characterp c)
       (or (CHAR= c #\Space) (CHAR= c #\Tab) (CHAR= c #\Newline)
           (CHAR= c #\Return))))


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
        (progn
          (stack-pushr stack head)
          (advance-scanner)
          (go again)))
       ((and (listp head) (not tail))
        (stack-pushr stack head))
       ((is-white-space head)
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
        (progn
          (stack-pushr stack head)
          (advance-scanner)
          (go again)))
       ))
  (stack-list stack))

(vdefun white-space (ls)
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

(vdefun blanks (ls)
  "Returns the input list, with all blanks in sublists"
  (blanks-aux (make-instance 'stack) (car ls) (cdr ls)))

(vdefun cl-blanks (ls)
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
           (CHAR= c #\%) (CHAR= c #\|) (CHAR= c #\&) (CHAR= c #\^) (CHAR= c #\#))))

(defun is-number (c)
  (and c (characterp c)
       (or (CHAR= c #\0)
           (CHAR= c #\1)
           (CHAR= c #\2)
           (CHAR= c #\3)
           (CHAR= c #\4)
           (CHAR= c #\5)
           (CHAR= c #\6)
           (CHAR= c #\7)
           (CHAR= c #\8)
           (CHAR= c #\9)
           )
       )
  )

(defun is-cl-punctuation (c)
  (and c (character c)
       (or (CHAR= c #\() (CHAR= c #\)) (CHAR= c #\')))
    )

(defun is-non-nestable-punctuation (c)
  (and (is-punctuation c) (CHAR/= c #\{) (CHAR/= c #\}) (CHAR/= c #\()
       (CHAR/= c #\)) (CHAR/= c #\[) (CHAR/= c #\])))

(defun is-non-nestable-cl-punctuation (c)
  (and (is-cl-punctuation c) (CHAR/= c #\() (CHAR/= c #\))))

(defun is-word-char (c)
  (and c (characterp c) (not (is-white-space c)) (not (is-punctuation c))))

(defun is-symbol-char (c)
  (and c (characterp c) (not (is-white-space c)) (not (is-cl-punctuation c))))

(defmacro! def-symbols (name char-test)
  (let ((aux (string-to-symbol (str-cat `,name "-aux")))
        (name (string-to-symbol `,name)))
    `(progn
       (defun ,aux (stack head tail)
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
                 ((,char-test head)
                  (let ((ls '()))
                    (tagbody
                     lsagain
                       (cond
                         ((,char-test head)
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
               )
              )
            )
         (stack-list stack)
         )
       (vdefun ,name (ls)
         (,aux (make-instance 'stack) (car ls) (cdr ls)))
       )
    )
  )

(def-symbols "words" is-word-char)
(def-symbols "cl-symbols" is-symbol-char)

(defun test-words ()
  (pipeline (str-to-char-ls "123  /* */ 123  123  123  ")
            #'cmt-str-regex #'white-space #'blanks #'words
            ))

(defun test-symbols ()
  (pipeline (str-to-char-ls "123  \"123\"  ;123  123  ")
            #'cl-cmt-str #'white-space #'cl-blanks #'symbols
            ))

;;; Punctuation grouping and related boolean tests (also used in punctuation)

(defun punctuations-list (acc head tail)
  (if (is-non-nestable-punctuation head)
      (punctuations-list (pushr acc head) (car tail) (cdr tail))
      (list acc (cons head tail))))

(defmacro! def-punctuation (name is-punc)
  (let ((aux (string-to-symbol (str-cat `,name "-aux")))
        (name (string-to-symbol `,name)))
    `(progn
       (defun ,aux (stack head tail)
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
                 ((,is-punc head)
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
       (vdefun ,name (ls)
         (,aux (make-instance 'stack) (car ls) (cdr ls)))
       )
    )
  )

(def-punctuation "punctuations" is-non-nestable-punctuation)
(def-punctuation "cl-punctuations" is-non-nestable-cl-punctuation)

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

(defun cl-nestables-aux (acc head tail fin)
  (cond
    ((and head (listp head))
     (cl-nestables-aux (pushr acc head) (car tail) (cdr tail) fin))
    ((and (not head) (not tail))
     (values acc tail))
    ((or (and fin head (CHAR= head fin))
         (and head (not tail)))
     (values (pushr acc head) tail))
    ((characterp head)
     (cond
       ((and acc (CHAR= head #\())
        (multiple-value-bind (nacc ntail) (cl-nestables-aux '() head tail #\))
          (cl-nestables-aux (pushr acc nacc) (car ntail) (cdr ntail) fin)
          )
        )
       ((and t)
        (cl-nestables-aux (pushr acc head) (car tail) (cdr tail) fin)
        )))))

(defun cl-nestables (ls)
  (cl-nestables-aux '() (car ls) (cdr ls) nil))

(defun test-nestables ()
  (nestables (str-to-char-ls "fn (a, b, c) { a e; a e; a e = c; a e = c && b || {}; cb();} more() { stuff }; foo(); foo(); foo();")))

(defun test-cl-nestables ()
  (cl-nestables (str-to-char-ls "fn (a, b, c) { a e; a e; a e = c; a e = c && b || {}; cb();} more() { stuff }; foo(); foo(); foo();")))

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

(defmacro! is-bcmt-end ()
  `(and (CHAR= head #\*) (CHAR= (car tail) #\/)))

(defmacro! is-html-bcmt-end ()
  `(is-char-ls-prefix "-->" (cons head tail))
  )

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

(defun char-eq-any (char charls)
  (apply #'fn-or
         (map 'list #'(lambda (e) (CHAR= char e)) charls))
   )

(defun is-regex (stack)
  "We need to check the preceding character to determine if we are (likely)
   looking at a regex literal or instead of a division. If we are preceded by
   any of the following, we assume we are looking at the start of a regex:
        (,=:[!&|?{};

   Clearly we can't handle regexes that are expressed as
       `if (cond) return /rgx/g;`"
  (let ((last-glyph (stack-last-until stack
                                      #'(lambda (e)
                                          (not (is-white-space e))))))
    (char-eq-any last-glyph (str-to-char-ls "(,=:[!&|?{};"))
    )
  )

(defun cmt-str-regex-aux (stack head tail)
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
       ((and (CHAR= head #\/) (is-regex stack))
        (str-tagbody #\/ rgxagain)
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


(defun html-cmt-str-aux (stack head tail)
  (tagbody
   again
     (cond
       ((and (not head) (not tail))
        )
       ((and head (not tail))
        (stack-pushr stack head))
       ((is-char-ls-prefix "<!--" (cons head tail))
        (let ((bcls '()))
          (tagbody
           bcmtagain
             (cond
               ((and (not (is-html-bcmt-end)) (not tail))
                (pushr! bcls head)
                (advance-scanner))
               ((and (is-html-bcmt-end))
                (pushr! bcls #\* #\/)
                (advance-scanner 2))
               ((not (is-html-bcmt-end))
                (pushr! bcls head)
                (advance-scanner)
                (go bcmtagain))
               )
             (stack-pushr stack bcls)
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
(vdefun cmt-str (char-ls)
  (cmt-str-aux (make-instance 'stack) (car char-ls) (cdr char-ls)))

(vdefun cmt-str-regex (char-ls)
  (cmt-str-regex-aux (make-instance 'stack) (car char-ls) (cdr char-ls)))

(vdefun cl-cmt-str (char-ls)
  (cl-cmt-str-aux (make-instance 'stack) (car char-ls) (cdr char-ls)))

(vdefun html-cmt-str (char-ls)
  (html-cmt-str-aux (make-instance 'stack) (car char-ls) (cdr char-ls)))

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

(defun is-ctl-struct-name (ls)
  (or (match-str-list "if" ls)
      (match-str-list "for" ls)
      (match-str-list "while" ls)
      (match-str-list "else" ls)
      (match-str-list "switch" ls)
      (match-str-list "return" ls)))

(defun is-js-ctl-struct-name (ls)
  (or (is-ctl-struct-name ls)
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
  (and (is-word-group ls) (not (is-ctl-struct-name ls))))

(defun is-js-func-name (ls)
  (and (is-word-group ls) (not (is-js-ctl-struct-name ls))))

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

(defun match-any-puncs-ls (ss ls)
  (let ((c ss))
    (tagbody again
       (cond
         ((not c)
          (return-from match-any-puncs-ls nil))
         ((match-punc-ls (car c) ls)
          (return-from match-any-puncs-ls t))
         ((and t)
          (setf c (cdr c))
          (go again))
         )
       )
    )
  )

(defun is-comma (n)
  (match-punc-ls "," n)
  )

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

(defun is-obj-lit-rval-aux (ls count)
  (cond
    ((or (and (characterp (car ls)) (CHAR= #\} (car ls)))
         (match-punc-ls "," (car ls)))
     count)
    ((and ls)
     (is-obj-lit-rval-aux (cdr ls) (+ count 1)))
    ((and t)
     nil)
    ))

(defun is-obj-lit-rval (ls)
  (is-obj-lit-rval-aux ls 0))

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


(defun is-js-word-or-str (ls)
  (or (is-str ls) (is-word-group ls))
  )

(defun is-dot (ls)
  (and (is-punctuation-group ls) (= 1 (length ls)) (CHAR= #\. (car ls)))
  )

(defun is-semi (ls)
  (and (is-punctuation-group ls) (= 1 (length ls)) (CHAR= #\; (car ls)))
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
    ((is-c-fcall ls)
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


(defun js-fcall-name-eq (fcall name)
  (and (is-js-fcall fcall) (match-str-list name (get-fcall-name fcall))))

(defun js-vbind-name-eq (vbind name)
  (and (is-js-var-binding vbind) (match-str-list name (get-js-vbind-name vbind))))

(defun js-obj-key-eq (obj-lit-rec name)
  (and (is-js-obj-lit-rec obj-lit-rec)
       (match-str-list name (get-js-obj-key obj-lit-rec)))
  )

(defun is-c-fdef (ls)
  (and ls (listp ls) (c-fdefp (car ls) (cdr ls))))




(defun is-ctl-word (ls)
  (or (match-str-list "if" ls)
      (match-str-list "while" ls)
      (match-str-list "for" ls)
      (match-str-list "switch" ls)))



(defun is-js-any-ctl-stmt (ls)
  (or (is-js-curly-ctl-stmt ls) (is-js-flat-ctl-stmt ls)
      (is-js-do-while-stmt ls) (is-js-if-else-chain ls)))


(defun is-c-fcall (ls)
  (and ls (listp ls)
       (not (is-c-fdef ls))
       (c-fcallp (car ls) (cdr ls))))

(defun is-c-fdef-or-fcall (ls)
  (or (is-c-fdef ls) (is-c-fcall ls)))

(defun is-js-fdef-or-fcall (ls)
  (or (is-js-fdef ls) (is-js-fcall ls)))

(defun is-c-indexable-funcs (ls)
  (or (is-c-fdef ls) (is-c-fcall ls)))

(defun is-js-indexable-funcs (ls)
  (or (is-js-fdef ls) (is-js-fcall ls) (is-js-fdef-binding ls)))

(defun is-js-indexable-conds (ls)
  (or (is-js-indexable-funcs ls)
      (is-js-any-ctl-stmt ls) (is-js-do-while-stmt ls)))

(defun is-js-indexable-binds (ls)
  (or (is-js-fdef-binding ls)
      (is-js-var-binding ls) (is-js-obj-lit-rec ls)
      (is-js-mbr-chain-word-bind ls) (is-js-fdef ls)
      )
  )


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

(def-5-state-match json-kv-pair-p is-str is-blank-group is-colon-group
  is-blank-group is-kvp-end)

(defun count-n-wsgroup (n ls)
  (let ((nls (head-n ls n)))
    (count-if #'is-white-space-group nls)))

(defun count-n-blankgroup (n ls)
  (let ((nls (head-n ls n)))
    (count-if #'is-blank-group nls)))

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

(group-3-to-5 group-json-kvp kvp-if-group is-nestable json-kvp)


; Walk the list. Call itself on every is-nestable list along the way.

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

(defun xform-js-flat-ctl-stmt-aux (acc head tail cb)
  (cond
    ((not head)
     acc)
    ((is-paren-group (last acc))
     (xform-js-flat-ctl-stmt-aux
      (pushr acc (funcall cb (cons head tail))) nil nil cb))
    ((not (is-paren-group (last acc)))
     (xform-js-flat-ctl-stmt-aux
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

(defun xform-c-fdef-cgroup (fd cb)
  (xform-js-fdef-cgroup fd cb)
  )

(defun xform-js-vbind-rval (fd cb)
  (xform-js-vbind-rval-aux '() (car fd) (cdr fd) cb))

(defun xform-js-fdef-pgroup (fd cb)
  (xform-js-fdef-pgroup-aux '() (car fd) (cdr fd) cb))

(defun xform-js-curly-ctl-stmt-cgroup (cstmt cb)
  (xform-js-curly-ctl-stmt-cgroup-aux '() (car cstmt) (cdr cstmt) cb))

(defun xform-js-flat-ctl-stmt (cstmt cb)
  (xform-js-flat-ctl-stmt-aux '() (car cstmt) (cdr cstmt) cb))

(defun xform-js-do-while-cgroup (cstmt cb)
  (xform-js-do-while-cgroup-aux '() (car cstmt) (cdr cstmt) cb))

(defun finite-match (ls match-fns)
  (declare (optimize (speed 3) (safety 0)))
  (let ((count 0)
        (type nil)
        (fn nil)
        (tail (cdr ls))
        (head (car ls))
        (state '())
        )
    (do-group (pair match-fns)
      (let* ((type (car pair))
             (func (cadr pair))
             (match (funcall func head))
             (cmatch (funcall func (cons head tail)))
             (cdroff cmatch)
             (caroff cdroff)
             )
        (cond
          ((or (and (equal type :m) (not match))
               (and (equal type :mc) (not cmatch))
               )
           (return-from finite-match (values nil state))
           )
          ((or (and (equal type :mc) cmatch)
               (and (equal type :oc) cmatch))
           (setf head (nth caroff tail))
           (setf tail (nthcdr cdroff tail))
           (incf count cmatch)
           )
          ((or (and (equal type :o) match)
               (and (equal type :m) match))
           (setf head (car tail))
           (setf tail (cdr tail))
           (incf count)
           )
          )
        )
      )
    (values count state)
    )
  )

(defun finite-repeating-match (ls rmatch-fns ematch-fns &key opt-rmatch opt-ematch)
  "Just like finite-match, except we match rmatch-fns in a loop, until it
  returns nil. We then run ematch-fns on the part that rmatch failed to match.
  If rmatch succeeds, this function returns t, otherwise nil"
  (assert (not (and opt-rmatch opt-ematch)))
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
          (if (or (and (not current_esize) (not opt-ematch))
                  (and (= rmatched 0) (not opt-rmatch)))
              (setf total_size nil)
              (incf total_size current_esize)
              )))
       )
    total_size
    )
  )



(defmacro! defgrouper (name body)
  `(defun ,name (head tail size)
     (list (map
            'list
            #'(lambda (x) (,@body))
            (head-n (cons head tail) size))
           (car (popl-n tail (- size 1)))
           (popl-n tail size)
           ))
  )

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

(defmacro! c-stage-impl (self-parent self matcher grouper &key fcall fdef
                                     curly-ctl do-while
                                     flat-ctl arr nestable var-bind mbr-chain)
  `(progn
     (defun ,self (acc head tail)
       (let ((sz (,matcher head tail)))
         (try-each
           ((and sz)
            (group-and-continue ,self ,grouper sz))
           ((is-c-fcall head)
            ,fcall)
 ;;;((is-c-arr head) ,arr)
           ((is-c-fdef head)
            ,fdef)
 ;;;((is-c-var-binding head) ,var-bind)
 ;;;((is-c-curly-ctl-stmt head) ,curly-ctl)
 ;;;((is-c-do-while-stmt head) ,do-while)
 ;;;((is-c-flat-ctl-stmt head) ,flat-ctl)
 ;;;((is-c-mbr-chain head) ,mbr-chain)
           ((is-nestable head)
            ,nestable)
           ((or (and (characterp head) tail) tail)
            (,self (pushr acc head) (car tail) (cdr tail)))
           ((and head (not tail))
            (pushr acc head))
           ((and (not tail))
            acc)
           ((and t)
            nil)
           )
         )
       )
     (vdefun ,self-parent (ls)
       (,self '() (car ls) (cdr ls))
       )
     )
  )



(defmacro! js-stage-impl (self-parent self matcher grouper &key fcall fdef
                                         fdef-bind curly-ctl do-while flat-ctl
                                         arr nestable var-bind obj-lit-rec
                                         mbr-chain)
  `(progn
     (defun ,self (acc head tail)
       ;(declare (optimize (speed 3) (safety 0)))
       (let ((sz (,matcher head tail)))
         (try-each
           ((and sz)
            (group-and-continue ,self ,grouper sz))
           ((is-js-fcall head)
            ,fcall)
           ((is-js-arr head)
            ,arr)
           ((is-js-fdef head)
            ,fdef)
           ((is-js-fdef-binding head)
            ,fdef-bind)
           ((is-js-var-binding head)
            ,var-bind)
           ((is-js-curly-ctl-stmt head)
            ,curly-ctl)
           ((is-js-do-while-stmt head)
            ,do-while)
           ((is-js-flat-ctl-stmt head)
            ,flat-ctl)
           ((is-js-mbr-chain head)
            ,mbr-chain)
           ((is-js-obj-lit-rec head)
            ,obj-lit-rec)
           ((is-nestable head)
            ,nestable)
           ((or (and (characterp head) tail) tail)
            (,self (pushr acc head) (car tail) (cdr tail)))
           ((and head (not tail))
            (pushr acc head))
           ((and (not tail))
            acc)
           ((and t)
            nil)
           )
         )
       )
     (vdefun ,self-parent (ls)
       (,self '() (car ls) (cdr ls))
       )
     )
  )

(defmacro! html-stage-impl (self-parent self matcher grouper &key link bold emph
                                        paragraph blockquote linebreak)
  `(progn
     (defun ,self (acc head tail)
       (let ((sz (,matcher head tail)))
         (try-each
          ((and sz)
           (group-and-continue ,self ,grouper sz))
          ((is-html-paragraph head)
           ,paragraph
           )
          ((is-html-link head)
           ,link
           )
          ((is-html-bold head)
           ,bold
           )
          ((is-html-emph head)
           ,emph
           )
          ((is-html-blockquote head)
           ,blockquote
           )
          ((or (and (characterp head) tail) tail)
           (,self (pushr acc head) (car tail) (cdr tail)))
          ((and head (not tail))
           (pushr acc head))
          ((and (not tail))
           acc)
          ((and t)
           nil)
          )
         )
       )
     (vdefun ,self-parent (ls)
       (,self '() (car ls) (cdr ls))
       )
     )
  )

(defmacro! descend-stage ()
  `(let ((ls '()))
     (pushr! ls 'descend-and-continue pref aux)
     (pushr! body ls)
     )
  )

(defmacro! xform-stage ()
  `(let ((ls '()))
     (pushr! ls 'xform-and-continue aux xformer pref)
     (pushr! body ls)
     )
  )

;;; TODO make this work with dynamic start and end tests.
;;; For example, in XML we only know at run-time what the corresponding end-test
;;; is to each start-test.
(defmacro! defnestable (l stagename &key start-test end-test when descend-if)
  "
  This defines a nestable grouper. It takes a language `l` (i.e. js, c, go) as a
  string. It takes a `stagename` (i.e. parenthesis, curlies, etc) also as a
  string. And it takes as keys two function _bodies_ that operate on a single
  variable named `x`.

  The `when` argument is analogous to defstage's `when` argument. However the
  only valid action `descend`.
  "
  (let* ((pref (string-to-symbol (str-cat `,l "-" `,stagename "s")))
         (aux (string-to-symbol (str-cat `,l "-" `,stagename "s-aux")))
         (startp (gensym))
         (endp (gensym))
         (descp (gensym))
         (startdef (append (list 'defun startp '(x)) (list start-test)))
         (enddef (append (list 'defun endp '(x)) (list end-test)))
         (descdef (append (list 'defun descp '(x)) (list descend-if)))
         )
    `(progn
       ,enddef
       ,startdef
       ,descdef
       (defun ,aux (acc head tail)
         (cond
           ((and (not head) (not tail))
            (values acc tail)
            )
           ((or (and head (,endp head))
                (and head (not tail)))
            (values (pushr acc head) tail)
            )
           ((and acc (,startp head))
            (multiple-value-bind (nacc ntail) (,aux '() head tail)
              (,aux (pushr acc nacc) (car ntail) (cdr ntail))
              )
            )
           ((,descp head)
            (multiple-value-bind (nacc ntail) (,aux '() (car head) (cdr head))
              (,aux (pushr acc nacc) (car ntail) (cdr ntail))
              )
            )
           ((and t)
            (,aux (pushr acc head) (car tail) (cdr tail))
            )
           )
         )
       (defun ,pref (ls)
         (,aux '() (car ls) (cdr ls))
         )
       )
    )
  )

(defun nestable-match (list start-test end-test)
  ;;; XXX this will also mistakenly match things like:
  ;;;     ))))((((
  ;;; fix it
  (if (or (not list) (not (funcall start-test (car list))))
      (return-from nestable-match nil)
      )
  (let ((count 0)
        (started nil)
        (open 0)
        )
    (do-group (n list)
      (if (funcall start-test n)
          (progn (incf open) (setf started t)))
      (if (funcall end-test n)
          (decf open))
      (incf count)
      (if (and (= open 0) started)
          (return-from nestable-match count)
          )
      )
    (if (and started (> count 0) (= open 0))
        count
        nil)
    )
  )

(defmacro! defstage (l stagename &key when group-if group-custom test tests-aux
                       tests-pub)
  "
  This defines a parsing stage that will be used in a parsing pipeline.

  The `l` argument is a string that denotes tha language (i.e. js, c, cpp, go).

  The `stagename` argument is a string that denotes the name of the stage (i.e.
  fdefs, fcalls, conds).

  The following arguments are key-arguments that define how the stage functions.

  `test` takes a body of code that has access to variables `head` and `tail`,
  and uses this code to match a sequence of elements. If there is a match, the
  stage will try to group those elems into a list. This function gets defined
  globally and is usable from any other code in blip.

  `group-if` takes a boolean expression that operates on an implicit argument
  named `x`. We run this boolean over each element of a group that has just been
  created by the current stage. When this expression evaluates to true, we try
  to run the stage on that element, creating a recursion. This can be used, for
  example, to group nested function definitions.

  `group-custom` is mutually exclusive with `group-if`. It simply takes a
  function-body that implicitly has access to variables `head`, `tail`, and
  `size` and returns a list of 3 elems containing: the new grouping, the new
  head element, and the new tail. This is useful for grouping nodes that have
  variable length (i.e. flat control structures:
  `if (foo !== NULL) $arbitrarily-long-stmt`).

  NOTE: if both `group-if` and `group-custom` are nil, we will simply group the
  matched expression

  `tests-aux` takes a list of function definitions (without the 'defun' keyword)
  that will be used by the code you pass to `test`. These functions get defined
  globally, and are usable from any other code in blip.

  `tests-pub` takes a list of function definitions just like `tests-aux`. These
  functions will be defined after the functions that are defined by `tests-aux`
  aux and `test`. Typically they are used to specifiy higher level test
  functions that utilize the test-functions defined by the other two params. For
  example, you would define is-js-fdef in `tests-pub`, and js-fdefp in `test`.
  The former is suitable for testing single grouped nodes, while the latter is
  suitable to testing an ungrouped sequence of nodes.

  `when` takes a list of pairs or tripples, where the first element is a keyword,
  the second element is an action, and the third element is a refinement of the
  action. The keyword is language specific and indicates an event or condition
  (i.e. :fdef, :fcall, :nestable, :mbr-chain -- meaning that we are
  looking-at/standing-on one of these things). The action can either be `xform`
  or `descend`. `descend` takes no refinements, and simply tells the stage to
  start matching/grouping stuff inside the element that it is standing on
  (instead of moving to the next element). The `xform` action takes a
  refinement. This refinement is the name of a function that transforms the
  element/node in a very particular way. For example:
  `(:fdef xform xform-js-fdef-cgroup)`
  will tell the stage to only run itself inside the curly braces of of a
  function definition (and ignore the parenthesis holding the arglist). Note
  that recursion cannot be achieved with `when`-specs, since they do not test
  recursively. Instead use `group-if` or `group-custom`.

  For each language, you will have to define a `$lang-stage-impl` macro for the
  `when` keyword-arg to work. See js-stage-impl and c-stage-impl for an example.
  "
  (assert (nand group-if group-custom))
  (let ((stage (string-to-symbol (str-cat `,l "-stage-impl")))
        (pref (string-to-symbol (str-cat `,l "-" `,stagename "s")))
        (aux (string-to-symbol (str-cat `,l "-" `,stagename "s-aux")))
        (test-nm (string-to-symbol (str-cat `,l "-" `,stagename "p")))
        (group (string-to-symbol (str-cat "group-" `,l "-" `,stagename)))
        (xformer nil)
        (body '())
        (ret '())
        )
    (pushr! ret 'progn)
    (cond
      ((and group-if)
       (pushr! ret (list 'defgrouper group (list 'if group-if
                                                 (list 'progn
                                                       (list pref 'x))
                                                 (list 'progn
                                                       'x))))
       )
      ((and group-custom)
       (pushr! ret (list 'defun group (list 'head 'tail 'size) group-custom))
       )
      ((and t)
       (pushr! ret (list 'defgrouper group 'x))
       )
      )
    (do-group (n tests-aux)
      (pushr! ret (append '(defun) n))
      )
    (pushr! ret (list 'defun test-nm (list 'head 'tail) test))

    (do-group (n tests-pub)
      (pushr! ret (append '(defun) n))
      )
    (do-group (n when)
      (pushr! body (car n))
      (cond
        ((equal 'descend (cadr n))
         (descend-stage)
         )
        ((equal 'xform (cadr n))
         (setf xformer (caddr n))
         (assert (and xformer))
         (xform-stage)
         )
        )
      )
    (pushr! ret (append (list stage pref aux test-nm group) body))
    ret
    )
  )

(defstage "js" "fdef"
  :when ((:nestable descend))
  :group-if (is-nestable x)
  :test (finite-match (cons head tail)
                       (list
                        (list :m #'(lambda (x)
                                     (match-str-list "function" x)))
                        (list :o #'is-blank-group)
                        (list :o #'is-js-func-name)
                        (list :o #'is-blank-group)
                        (list :m #'is-paren-group)
                        (list :o #'is-blank-group)
                        (list :m #'is-c-fdef-end)
                        )
                      )
  :tests-pub ((is-js-fdef (ls)
                          (and ls (listp ls) (js-fdefp (car ls) (cdr ls))))
              )
  )

(defstage "c" "fdef"
  :when ((:nestable descend))
  :group-if (is-nestable x)
  :test (finite-match (cons head tail)
                      (list
                       (list :m #'is-c-func-name)
                       (list :o #'is-blank-group)
                       (list :m #'is-paren-group)
                       (list :o #'is-blank-group)
                       (list :m #'is-c-fdef-end)
                       )
                      )
  )

(defstage "js" "fcall"
  :when ((:fdef xform xform-js-fdef-cgroup) (:nestable descend))
  :group-if (is-nestable x)
  :test (finite-match (cons head tail)
                       (list
                        (list :m #'is-js-func-name)
                        (list :o #'is-blank-group)
                        (list :m #'is-paren-group)
                        )
                      )
  :tests-pub (
              (is-js-fcall (ls)
                (and ls (listp ls)
                     (not (is-js-fdef ls))
                     (js-fcallp (car ls) (cdr ls))))
              )
  )

(defstage "c" "fcall"
  :when ((:fdef xform xform-c-fdef-cgroup) (:nestable descend))
  :group-if (is-nestable x)
  :test (finite-match (cons head tail)
                      (list
                       (list :m #'is-c-func-name)
                       (list :o #'is-blank-group)
                       (list :m #'is-paren-group)
                       )
                      )
  )

(defstage "js" "arr"
  :when ((:fdef xform xform-js-fdef-cgroup)
         (:fcall xform xform-fcall-pgroup)
         (:nestable descend))
  :group-if (is-nestable x)
  :test (finite-match (cons head tail)
                      (list
                       (list :m #'is-js-func-name)
                       (list :o #'is-blank-group)
                       (list :m #'is-bracket-group)
                       )
                      )
  :tests-pub (
              (is-js-arr (ls)
                (and (listp ls)
                     (js-arrp (car ls) (cdr ls))))
              (is-js-word-or-fcall-or-arr (ls)
                (or (is-js-fcall ls) (is-js-arr ls) (is-word-group ls))
                )
              (is-js-word-or-arr (ls)
                (or (is-js-arr ls) (is-word-group ls))
                )
              )
  )

(defstage "js" "var-binding"
  :when ((:fdef xform xform-js-fdef-cgroup)
         (:fcall xform xform-fcall-pgroup)
         (:nestable descend))
  :group-if (is-js-fdef x)
  :tests-aux ((js-fdef-bindingp (head tail)
               (finite-match (cons head tail)
                             (list
                              (list :m #'is-word-group)
                              (list :o #'is-blank-group)
                              (list :m #'is-eq-group)
                              (list :o #'is-blank-group)
                              (list :m #'is-js-fdef)
                              )
                             )))
  :test (progn
          (let ((ret nil))
            (setf ret (js-fdef-bindingp head tail))
            (if (and ret)
                (return-from js-var-bindingp ret)
                )
            )
          (finite-match (cons head tail)
                        (list
                         (list :m #'is-js-word-or-arr)
                         (list :o #'is-blank-group)
                         (list :m #'is-eq-group)
                         (list :o #'is-blank-group)
                         (list :mc #'is-stmt)
                         )
                        )
          )
  :tests-pub (
              (is-js-fdef-binding (ls)
                (and ls (listp ls) (js-fdef-bindingp (car ls) (cdr ls))))
              (is-js-var-binding (ls)
                (and ls (listp ls) (not (is-js-fdef-binding ls))
                     (or (is-word-group (car ls)) (is-js-arr (car ls)))
                     (or (is-eq-group (cadr ls)) (is-eq-group (caddr ls)))))
              (is-js-binding (ls)
                (and ls (listp ls)
                     (or (is-js-fdef-binding ls)
                         (is-js-var-binding ls))))
              )
  )

(defstage "js" "mbr-chain"
  :when ((:fcall xform xform-fcall-pgroup)
          (:arr xform xform-js-arr-bgroup)
          (:fdef xform xform-js-fdef-cgroup)
          (:var-bind descend)
          (:fdef-bind descend)
          (:nestable descend))
  :group-if (or (is-js-fdef-binding x) (is-js-fcall x)
                (is-js-var-binding x) (is-js-arr x)
                )
  :test (finite-repeating-match (cons head tail)
                                (list
                                 (list :m #'is-js-word-or-fcall-or-arr)
                                 (list :o #'is-blank-group)
                                 (list :m #'is-dot)
                                 (list :o #'is-blank-group))
                                (list
                                 (list :m #'is-word-arr-fcall-vbind-fbind)))
  :tests-pub (
              (is-js-mbr-chain (ls)
                (and ls (listp ls) (js-mbr-chainp (car ls) (cdr ls))))
              (is-js-mbr-chain-word-bind (ls)
                "Detects mbr-chains that contain words and end in a binding"
                (let ((fail nil))
                  (if (and (is-js-mbr-chain ls) (is-js-binding (car (last ls))))
                      (do-group (e ls)
                        (if (not (or (is-blank-group e)
                                     (is-punctuation-group e)
                                     (is-word-group e)
                                     (is-js-binding e))
                                 )
                            (setf fail t))
                        (if (and (is-js-binding e) (is-js-arr (car e)))
                            (setf fail t))
                        ls)
                      (setf fail t)
                      )
                  (not fail)
                  )
                )
              )
          )

(defstage "js" "obj-lit-rec"
  :when (
          (:fdef-bind descend)
          (:var-bind descend)
          (:mbr-chain descend)
          (:fdef xform xform-js-fdef-cgroup)
          (:fcall xform xform-fcall-pgroup)
          (:nestable descend))
  :group-if (or (is-js-fdef-binding x) (is-js-fcall x)
                (is-js-var-binding x) (is-js-arr x)
                (is-js-mbr-chain x)
                )
  :test (finite-match (cons head tail)
                      (list
                       (list :m #'is-js-word-or-str)
                       (list :o #'is-blank-group)
                       (list :m #'is-colon-group)
                       (list :o #'is-blank-group)
                       (list :mc #'is-obj-lit-rval)
                       )
                      )
  :tests-pub (
              (is-js-obj-lit-rec (ls)
                (and ls (listp ls)
                     (or (is-word-group (car ls)) (is-str (car ls)))
                     (or (is-colon-group (cadr ls)) (is-eq-group (caddr ls)))
                     ))
              )
          )

(defstage "js" "curly-ctl-stmt"
  :when (
          (:fdef xform xform-js-fdef-cgroup)
          (:fcall xform xform-fcall-pgroup)
          (:mbr-chain descend)
          (:var-bind descend)
          (:obj-lit-rec descend)
          (:nestable descend))
  :group-if (is-nestable x)
  :test (finite-match (cons head tail)
                      (list
                       (list :m #'(lambda (x) (or (match-str-list "if" x)
                                                  (match-str-list "while" x)
                                                  (match-str-list "for" x)
                                                  (match-str-list "switch" x))))
                       (list :o #'is-blank-group)
                       (list :m #'is-paren-group)
                       (list :o #'is-blank-group)
                       (list :m #'is-curly-group)))
  :tests-pub ((is-js-curly-ctl-stmt (ls)
                  (and ls (listp ls) (js-curly-ctl-stmtp (car ls) (cdr ls))))
              )
           )

(defstage "js" "do-while-stmt"
  :when (
          (:fdef xform xform-js-fdef-cgroup)
          (:fcall xform xform-fcall-pgroup)
          (:mbr-chain descend)
          (:var-bind descend)
          (:obj-lit-rec descend)
          (:curly-ctl xform xform-js-curly-ctl-stmt-cgroup)
          (:nestable descend))
  :group-if (is-nestable x)
  :test (finite-match (cons head tail)
                      (list
                       (list :m #'(lambda (x) (match-str-list "do" x)))
                       (list :o #'is-blank-group)
                       (list :m #'is-curly-group)
                       (list :o #'is-blank-group)
                       (list :m #'(lambda (x) (match-str-list "while" x)))
                       (list :o #'is-blank-group)
                       (list :m #'is-paren-group)
                       (list :o #'is-blank-group)
                       ))
  :tests-pub ((is-js-do-while-stmt (ls)
                (and ls (listp ls) (js-do-while-stmtp (car ls) (cdr ls)))))
          )

(defstage "js" "flat-ctl-stmt"
  :when (
          (:fdef xform xform-js-fdef-cgroup)
          (:fcall xform xform-fcall-pgroup)
          ;;; Do we need these? vv
          (:mbr-chain descend)
          (:var-bind descend)
          ;;; Do we need these? ^^
          (:obj-lit-rec descend)
          (:curly-ctl xform xform-js-curly-ctl-stmt-cgroup)
          (:do-while xform xform-js-do-while-cgroup))
  :group-custom (let* ((ls (head-n (cons head tail) size))
                       (pos (first-pgroup-pos ls))
                       (blank-after (is-blank-group (nth (+ pos 1) ls)))
                       (n-cdr (if (and blank-after) (+ pos 2) (+ pos 1)))
                       (tail-ls (nthcdr n-cdr ls))
                       (tail-group (js-flat-ctl-stmts tail-ls))
                       )
                  (list (append (head-n ls n-cdr) tail-group)
                        (car (popl-n tail (- size 1)))
                        (popl-n tail size)))
  :test (finite-match (cons head tail)
                      (list
                       (list :m #'(lambda (x) (or (match-str-list "if" x)
                                                  (match-str-list "while" x)
                                                  (match-str-list "for" x)
                                                  (match-str-list "switch" x))))
                       (list :o #'is-blank-group)
                       (list :m #'is-paren-group)
                       (list :o #'is-blank-group)
                       (list :mc #'is-stmt)))
  :tests-pub ((is-js-flat-ctl-stmt (ls)
                (and ls (listp ls) (not (is-js-curly-ctl-stmt ls)) (is-ctl-word (car ls)))))
          )

(defstage "js" "if-else-chain"
  :when (
          (:fdef xform xform-js-fdef-cgroup)
          (:fcall xform xform-fcall-pgroup)
          ;;; Do we need these? vv
          (:mbr-chain descend)
          (:var-bind descend)
          ;;; Do we need these? ^^
          (:obj-lit-rec descend)
          (:curly-ctl xform xform-js-curly-ctl-stmt-cgroup)
          (:flat-ctl xform xform-js-flat-ctl-stmt)
          (:do-while xform xform-js-do-while-cgroup))
  :group-if (is-js-fdef x)
  :tests-aux (
              (is-if-block (n)
                (and (listp n) n (match-str-list "if" (car n)))
                )

              (is-else-word (n)
                (and (listp n) n (match-str-list "else" n)))

              (is-if-block-or-curly-group-or-stmt (n)
                (or (is-if-block n)
                    (is-curly-group n)
                    )
                )
              )
  :test (finite-repeating-match
         (cons head tail)
         (list
          (list :m #'is-if-block)
          (list :o #'is-blank)
          (list :m #'is-else-word)
          (list :o #'is-blank)
          )
         (list
          (list :m #'is-if-block-or-curly-group-or-stmt)
          )
         )
  :tests-pub ((is-js-if-else-chain (ls)
                (and ls (listp ls) (js-if-else-chainp (car ls) (cdr ls)))))
  )


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

(defmacro! optfuncall (fn arg)
  `(if (and ,fn) (funcall ,fn ,arg) t)
  )

(defun walk-tree (ls test work path walk &key backoff-if)
  (if (and (listp ls) (funcall test (car ls)))
      (funcall work (car ls) path (reverse (cons 'd walk))))
  (if (and (listp ls) (car ls))
      (walk-tree (car ls) test work (pushr path (car ls))
                 (cons 'd walk) :backoff-if backoff-if))
  (if (and (listp ls) (cdr ls))
      (walk-tree (cdr ls) test work path
                 (cons 'r walk) :backoff-if backoff-if)))

(defmacro walk-tree2 (ls node-name stack-name walk-name &body body)
  (let ((ls-name (gensym))
        (cur-name (gensym))
        )
    `(let ((,stack-name '())
           (,walk-name '())
           (,ls-name ,ls)
           (,node-name ,ls)
           )
       (cond
         ((and (listp ,ls-name))
          (setf ,stack-name (cons ,node-name ,stack-name))
          ,@body
          (cond
            ((and (listp ,ls-name) (car ,ls-name))
             (setf ,walk-name (cons 'd ,walk-name))
             (setf ,node-name (car ,ls-name))
             )
            ((and (listp ,ls-name) (cdr ,ls-name))
             (setf ,walk-name (cons 'r ,walk-name))
             (setf ,node-name (cdr ,ls-name))
             )
            ((and t)
             (setf ,node-name (cdr ,stack-name))
             ;; TODO GOTO TOP
             )
            )
          )
         )
       )
    )
  )

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
(def-apply fcalls-apply is-c-fcall)
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

(defun get-c-fdef-name-aux (prev-word ls)
  (cond
    ((is-paren-group (car ls))
     prev-word)
    ((and (not (is-paren-group (car ls)))
          (not (is-word-group (car ls))))
     (get-c-fdef-name-aux prev-word (cdr ls)))
    ((is-word-group (car ls))
     (get-c-fdef-name-aux (car ls) (cdr ls)))
    ))

(defun get-c-fdef-name (ls)
  (assert (is-c-fdef ls))
  (get-c-fdef-name-aux nil ls))

(defun get-js-ctl-name (ls)
  (assert (is-js-any-ctl-stmt ls))
  (car ls))

(defun get-js-fbind-name (ls)
  (assert (is-js-fdef-binding ls))
  (car ls))

(defun get-js-vbind-name (ls)
  (assert (is-js-var-binding ls))
  (car ls))

(defun get-js-obj-key (ls)
  (assert (is-js-obj-lit-rec ls))
  (car ls))

(defun set-js-obj-key (ls key)
  (assert (is-js-obj-lit-rec ls))
  (assert (or (stringp key) (listp key)))
  (if (stringp key)
      (setf (car ls) (str-to-char-ls key)))
  (if (listp key)
      (setf (car ls) key))
  )

(defun set-js-fdef-name (ls name)
  (assert (is-js-fdef ls))
  (tagbody again
     (cond
       ((and (is-paren-group (cadr ls))
             (not (match-str-list "function" (car ls))))
         (setf ls name)
        )
       ((and (is-paren-group (cadr ls))
             (match-str-list "function" (car ls)))
        (setf (cdr ls) (cons name (cdr ls)))
        )
       ((and t)
        (setf ls (cdr ls))
        (go again)
        )
       )
     )
  )

(defun set-js-fcall-name (ls name)
  (assert (is-js-fcall ls))
  (setf (car ls) name)
  )

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
            (do-group (x (list-directory name)) (walk x)))
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

(defun bin-to-form-impl (input)
  (let* ((form nil)
         (empty nil))
    (cond
      ((is-file-p input)
       (setf form (cpk:decode-file input))
       (if (not form)
           (setf empty t))
       )
      )
    (values (car form) empty)))

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

(vdefun file-to-forms (input)
  (file-to-forms-impl input))

(defun format-to-file (output fmt input)
  (let ((out nil)
        (pathls (str-split "/" output)))
    (if (> (length pathls) 1)
        (mkdir (apply #'str-cat (intersperse (popr pathls) "/")))
        )
    (setf out (open output :direction :output :if-exists :supersede
                           :if-does-not-exist :create))
    (format out fmt input)
    (finish-output out)
    (close out)
    ))

(defun form-to-file-impl (form output)
  (let ((out nil)
        (pathls (str-split "/" output)))
    (if (> (length pathls) 1)
        (mkdir (apply #'str-cat (intersperse (popr pathls) "/")))
        )
    (setf out (open output :direction :output :if-exists :supersede
                 :if-does-not-exist :create))
    (prin1 form out)
    (finish-output out)
    (close out)
  ))

(defun forms-to-file-impl (forms output)
  (let ((out nil)
        (pathls (str-split "/" output)))
    (if (> (length pathls) 1)
        (mkdir (apply #'str-cat (intersperse (popr pathls) "/")))
        )
    (setf out (open output :direction :output :if-exists :supersede
                           :if-does-not-exist :create))
    (do-group (form forms)
      (prin1 form out)
      (format out "~%")
      )
    (finish-output out)
    (close out)
    ))

(defun form-to-bin-impl (form output)
  (let ((out nil)
        (pathls (str-split "/" output)))
    (if (> (length pathls) 1)
        (mkdir (apply #'str-cat (intersperse (popr pathls) "/")))
        )
    (setf out (open output :direction :output :if-exists :supersede
                           :element-type '(unsigned-byte 8)
                           :if-does-not-exist :create))
    (cpk:encode form :stream out)
    (finish-output out)
    (close out)
    ))

(defun str-to-file (str output)
  (let ((out (open output :direction :output :if-exists :supersede
                          :if-does-not-exist :create)))
    (write-string str out)
    (finish-output out)
    (close out)
    )
  )

(vdefun char-ls-to-file (cls output)
  (str-to-file (char-ls-to-str cls) output)
  )

(vdefun file-to-char-ls (input)
  (file-to-char-ls-impl input))

(vdefun bin-to-form (input)
  (bin-to-form-impl input))

(vdefun file-to-form (input)
  (file-to-form-impl input))

(vdefun form-to-bin (form output)
  (form-to-bin-impl form output))

(vdefun form-to-file (form output)
  (form-to-file-impl form output))

(vdefun forms-to-file (form output)
  (forms-to-file-impl form output))

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
     (concatenate 'string (get-fcall-name ls) "()"))
    ((is-c-fdef ls)
     (concatenate 'string (get-c-fdef-name ls) "{}"))
    ))

(defun get-js-mbr-nested-words (ls)
  (let ((words '()))
    (do-group (e ls)
      (cond
        ((is-word-group e)
         (pushr! words e)
         )
        )
      )
    (flatten (intersperse words '(#\/) t))
    )
  )

(defun js-name-to-pathname (ls)
  (cond
    ((is-js-flat-ctl-stmt ls)
     (concatenate 'string (get-js-ctl-name ls) ";"))
    ((is-js-curly-ctl-stmt ls)
     (char-ls-to-str (get-js-ctl-name ls)))
    ((is-js-do-while-stmt ls)
      "do")
    ((is-js-fcall ls)
     (concatenate 'string (get-fcall-name ls) "()"))
    ((is-js-fdef ls)
     (concatenate 'string (get-js-fdef-name ls) "{}"))
    ((is-js-fdef-binding ls)
     (concatenate 'string (flatten (get-js-fbind-name ls)) "="))
    ((is-js-var-binding ls)
     (concatenate 'string (flatten (get-js-vbind-name ls)) "="))
    ((is-js-obj-lit-rec ls)
     (concatenate 'string (flatten (get-js-obj-key ls)) ":"))
    ((is-js-mbr-chain-word-bind ls)
     (char-ls-to-str (get-js-mbr-nested-words ls)))
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
    (do-group (e (cdr ls))
      (cond
        ((equal e prev)
         (incf count))
        ((not (equal e prev))
         (pushr! acc (list count prev))
         (setf prev e)
         (setf count 1))
        )
      )
    (pushr! acc (list count prev))
  ))

(defun unfold-list (ls)
  (if (= (length ls) 0)
      (return-from unfold-list nil))
  (let* ((unfls '()))
    (do-group (p ls)
      (dotimes (z (car p))
        (pushr! unfls (cadr p))))
    unfls
  ))

(defclass indexer ()
  ((str-stkls :initarg :str-stkls :initform '() :accessor str-stkls)
   (repo-commits :initarg :repo-commits :initform '() :accessor repo-commits)
   (commit-cmp :initarg :commit-cmp :initform '() :accessor commit-cmp)
   (ast :initarg :ast :initform '() :accessor ast)
   (ast-sha2 :initarg :ast-sha2 :initform nil :accessor ast-sha2)
   (test-name :initarg :test-name :initform :funcs :accessor test-name))
  )

(vdefmethod indexer-reset ((ix indexer))
  (setf (str-stkls ix) '())
  (setf (ast ix) '())
  (setf (ast-sha2 ix) nil)
  )

(defclass js-indexer (indexer)
  ((test :initarg :test :initform #'is-js-indexable-funcs :accessor test))
  )

(defclass c-indexer (indexer)
  ((test :initarg :test :initform #'is-c-indexable-funcs :accessor test))
  )

(vdefmethod name-to-test ((ix js-indexer) &optional test-name)
  (if (not test-name)
      (setf test-name (test-name ix)))
  (cond
    ((equal test-name :funcs)
     #'is-js-indexable-funcs)
    ((equal test-name :funcs-conds)
     #'is-js-indexable-conds)
    ((equal test-name :binds)
     #'is-js-indexable-binds)
    ((and t)
     (assert nil))
    )
  )

(vdefmethod name-to-test ((ix c-indexer) &optional test-name)
  (if (not test-name)
      (setf test-name (test-name ix))
      (setf (test-name ix) test-name))
  (cond
    ((equal test-name :funcs)
     #'is-c-indexable-funcs)
    ((equal test-name :funcs-conds)
     #'is-c-indexable-funcs)
    ((and t)
     (assert nil))
    )
  )

(vdefmethod test-name-str ((ix indexer))
  (symbol-name (test-name ix)))

(vdefmethod set-test ((ix indexer) test-name)
  (setf (test-name ix) test-name)
  (setf (test ix) (name-to-test ix test-name)))

(vdefmethod refine-path ((ix js-indexer) path)
  (map 'list #'js-name-to-pathname (remove-if-not (test ix) path)))

(vdefmethod refine-path ((ix c-indexer) path)
  (map 'list #'c-name-to-pathname (remove-if-not (test ix) path)))

(vdefmethod update ((ix indexer) node path walk)
  (pushr! (str-stkls ix) (list (refine-path ix (pushr path node))
                               (fold-list walk)))
  )

(vdefmethod listify-index ((ix indexer))
  "Takes object and listifies it so we can just form-to-file it"
  (list (str-stkls ix) (ast-sha2 ix)))

(vdefmethod build-index ((ix indexer))
  (let ((up #'(lambda (x y z) (update ix x y z))))
    (walk-tree (ast ix) (test ix) up '() '())
    nil
    ;(listify-index ix)
    )
  )

(vdefun bin-file-to-idx (input)
  (let* ((form nil)
         (empty nil))
    (cond
      ((is-file-p input)
       (setf form (cpk:with-index (l r) (cpk:decode-file input)))
       (if (not form)
           (setf empty t))
       )
      )
    (values (car form) empty)))

(vdefun idx-to-bin-file (form output)
  (let ((out nil)
        (pathls (str-split "/" output)))
    (if (> (length pathls) 1)
        (mkdir (apply #'str-cat (intersperse (popr pathls) "/")))
        )
    (setf out (open output :direction :output :if-exists :supersede
                           :element-type '(unsigned-byte 8)
                           :if-does-not-exist :create))
    (cpk:with-index (l r) (cpk:encode form :stream out))
    (finish-output out)
    (close out)
    ))

(vdefun idx-to-file (form output)
  ;(form-to-file form output)
  (idx-to-bin-file form output)
  )

(vdefun file-to-idx (input)
  ;(file-to-form input)
  (bin-file-to-idx input)
  )

(defun nthcadr (n ls)
  (car (nthcdr n ls)))

(defun column (n rows)
  (let ((col '()))
    (do-group (row rows)
      (pushr! col (nthcadr n row))
      )
    col
    )
  )

(defun zip2 (l1 l2)
  (assert (= (length l1) (length l2)))
  (mapcar #'(lambda (a b)
              (list a b)
              )
          l1 l2
          )
  )

(vdefmethod serialize-index ((ix indexer) repo file commit)
  (let* ((fcommit (git-file-latest-commit-until repo file commit (commit-cmp ix)))
         (outdir (str-cat blip-repo-meta repo "/root/" file "/" fcommit "/path-index/"))
         (out1 (str-cat outdir (test-name-str ix) "_paths"))
         (out2 (str-cat outdir (test-name-str ix) "_walks"))
         )
    (build-index ix)
    (idx-to-file (list (column 0 (str-stkls ix)) (ast-sha2 ix)) out1)
    (idx-to-file (column 1 (str-stkls ix)) out2)
    )
  )

(vdefmethod commit< ((ix indexer) c1 c2)
  "c1 < c2 if c1 is older than c2"
  (if (equal c1 c2)
      (return-from commit< nil))
  (let ((n1 nil)
        (n2 nil))
    (do-group (e (repo-commits ix))
      (cond
        ((equal c1 (car e))
         (setf n1 (cadr e)))
        ((equal c2 (car e))
         (setf n2 (cadr e)))
        )
      )
    (> n1 n2) ;newer commits have lower numbers
    )
  )

(vdefmethod commit> ((ix indexer) c1 c2)
  (if (equal c1 c2)
      (return-from commit> nil))
  (not (commits< ix c1 c2))
  )

(vdefmethod commit-num ((ix indexer) c)
  (cadr (find-if #'(lambda (e) (equal c (car e))) (repo-commits ix))))

(vdefmethod cache-index ((ix indexer) repo file commit ast ast-sha2)
  (indexer-reset ix)
  (if (not ast)
      (setf ast (load-ast repo file commit ix))
      )
  (setf (ast ix) ast)
  (if (not ast-sha2)
      (setf ast-sha2 (load-ast-sha2 repo file commit ix))
      )
  (setf (ast-sha2 ix) ast-sha2)
  (serialize-index ix repo file commit)
  )

(vdefmethod load-index ((ix indexer) repo file commit &optional ast)
  (let* ((fcommit (git-file-latest-commit-until repo file commit (commit-cmp ix)))
         (in (str-cat blip-repo-meta repo "/root/" file "/" fcommit "/path-index/"
                      (test-name-str ix)))
         (ret (file-to-idx in)))
    ret
    )
  )

(vdefmethod load-index-paths ((ix indexer) repo file commit &optional ast)
  (let* ((fcommit (git-file-latest-commit-until repo file commit (commit-cmp ix)))
         (old-in (str-cat blip-repo-meta repo "/root/" file "/" fcommit "/path-index/"
                          (test-name-str ix)))
         (in (str-cat old-in "_paths"))
         (ret (file-to-idx in))
         )
    (if (is-file-p old-in)
        (delete-file old-in))
    ret
    ))

(vdefmethod load-index-walks ((ix indexer) repo file commit &optional ast)
  (let* ((fcommit (git-file-latest-commit-until repo file commit (commit-cmp ix)))
         (in (str-cat blip-repo-meta repo "/root/" file "/" fcommit "/path-index/"
                      (test-name-str ix) "_walks"))
         (ret (file-to-idx in)))
    ret
    ))

(defmacro! init-ix-repo-commits (ix)
  `(progn
     (if (not (repo-commits ,ix))
         (setf (repo-commits ,ix) (number-each-elem (git-all-commits repo))))
     (setf (commit-cmp ,ix) #'(lambda (c1 c2) (commit< ,ix c1 c2)))
    )
  )

(vdefmethod index-paths-walks ((ix indexer) repo file commit &optional ast &key force test)
  (expand-commit! repo commit)
  (init-ix-repo-commits ix)
  (if (and test)
      (set-test ix test)
      )
  (let* ((loaded-idx-paths (if (not force) (load-index-paths ix repo file commit ast) nil))
         (ast-sha2 (load-ast-sha2 repo file commit ix)))
    (cond
      ((or (not loaded-idx-paths)
           (not (equalp (path-index-ast-sha2 loaded-idx-paths)
                       ast-sha2)))
       (cache-index ix repo file commit ast ast-sha2)
       (setf loaded-idx-paths (load-index-paths ix repo file commit ast))
       )
      )
    (list (zip2 (car loaded-idx-paths) (load-index-walks ix repo file commit ast))
          (path-index-ast-sha2 loaded-idx-paths))
    ;loaded-idx
    )
  )

(vdefmethod index-paths ((ix indexer) repo file commit &optional ast &key force test)
  (expand-commit! repo commit)
  (init-ix-repo-commits ix)
  (if (and test)
      (set-test ix test)
      )
  (let* ((loaded-idx-paths (if (not force) (load-index-paths ix repo file commit ast) nil))
         (ast-sha2 (load-ast-sha2 repo file commit ix)))
    (cond
      ((or (not loaded-idx-paths)
           (not (equalp (path-index-ast-sha2 loaded-idx-paths)
                        ast-sha2)))
       (cache-index ix repo file commit ast ast-sha2)
       (setf loaded-idx-paths (load-index-paths ix repo file commit ast))
       )
      )
    (list (zip2 (car loaded-idx-paths) (mapcar #'(lambda (a) nil) (car loaded-idx-paths)) )
          (path-index-ast-sha2 loaded-idx-paths))
    )
  )

(defun path-index-file (ls)
  (car ls))

(defun path-index-str (ls)
  (car (cadr ls)))

(defun reverse-paths (pairs)
  (let ((rev-pairs '()))
    (do-group (e pairs)
      (pushr! rev-pairs (list (reverse (car e)) (cadr e)))
      )
    rev-pairs
    )
  )

(defun path-index-revstr (ls)
  (reverse-paths (path-index-str ls)))

(defun path-index-ast-sha2 (ls)
  (cadr ls))

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
      ((equal type :binds)
       (setf idx-bool #'is-js-indexable-binds))
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
       (setf idx-bool #'is-c-indexable-funcs))
      ((equal type :funcs-conds)
       (setf idx-bool #'is-c-indexable-funcs))
      )
    idx-bool
    )
  )

(defun c-idx-type-to-name (type)
  (js-idx-type-to-name type))

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

(defun list-node-in-ast (test fmt dedup ast &key agg backoff-if)
  (let ((list '())
        (ddl '())
        (cl '())
        )
    (if (not agg)
        (setf agg 'set)
        )
    (walk-tree ast test
               #'(lambda (n stack walk)
                   (if (funcall test n)
                       (pushr! list (funcall fmt n))))
               '() '() :backoff-if backoff-if
               )
    (cond
      ((equal agg 'frequency)
       (setf ddl (remove-duplicates list :test dedup))
       (do-group (e ddl)
         (pushr! cl (list e (count e list :test dedup)))
         )
       (stable-sort cl #'< :key #'cadr)
       )
      ((equal agg 'set)
       (setf ddl (remove-duplicates list :test dedup))
       ddl
       )
      ((equal agg 'list)
       list
       )
      )
    )
  )

(defmacro! load-list-nodes (name suf)
  `(vdefun ,name (repo file commit &optional indexer)
     ;(assert commit-cmp)
     (let* ((cmp (if indexer (commit-cmp indexer)))
            (fcommit (git-file-latest-commit-until repo file commit cmp))
            (in (str-cat blip-repo-meta repo "/root/" file "/" fcommit ,suf)))
       (bin-to-form in)
       )
     )
  )


(defmacro! x-list-node-impl (name test getter)
  `(vdefun ,name (ast &optional count)
    (list-node-in-ast #',test
                      #'(lambda (n)
                          (char-ls-to-str (,getter n)))
                      #'string=
                      ast
                      :agg (if (and count) 'frequency)
                      )))


(defmacro! cache-x-list-node (name list-impl suf)
  `(vdefun ,name (repo file commit &optional indexer)
     (let* ((ast (load-ast repo file commit indexer))
            (cmp (if indexer (commit-cmp indexer)))
            (fcommit (git-file-latest-commit-until repo file commit cmp))
            (outdir (str-cat blip-repo-meta repo "/root/" file "/" fcommit))
            (out (str-cat outdir ,suf))
            )
      ;(assert commit-cmp)
      ;TODO implement a cache-ast function that will cache ASTs on a per-file basis
      ;TODO cleanup parse-x-files-at-commit
      (if (not ast)
          (assert ast))
      (mkdir outdir)
      (form-to-bin (,list-impl ast t) out)
      )
    )
  )


(defmacro! x-list-node (name cacher loader)
  `(vdefun ,name (repo file commit &optional count &key force indexer)
     (expand-commit! repo commit)
     ;(assert commit-cmp)
     (multiple-value-bind
           (ls empty) (if (not force) (,loader repo file commit indexer)
                          (values nil nil))

       (cond
         ((and (not ls) (not empty))
          (,cacher repo file commit indexer)
          (setf ls (,loader repo file commit indexer)))
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

(x-list-node-impl c-list-fcalls-impl is-c-fcall get-fcall-name)
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
                      'head count :force force))

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
  (js-fcall-name-eq f "require"))

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
  (print-all-arrays (load-ast "github/joyent/sdc-docker" "lib/moray.js" 'head)))

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

(defun get-js-fdef-arg-list (def)
  "Returns a list of identifies (aka words) that represent func args."
  (let ((args (get-js-fdef-params def))
        (wds '())
        )
    (do-group (w args)
      (if (is-word-group w)
          (pushr! wds w)
          )
      )
    wds
    )
  )

(defun get-js-fdef-arg (def n)
  (nthcadr n (get-js-fdef-arg-list def))
  )

(defun get-js-fcall-arg (call n)
  (let* ((params (get-fcall-params call))
         (count 0)
         (pls (list-split '(#\,) (remove-if #'is-blank-group
                                            (popl (popr params)))))
         (cursor 0)
         )
    (do-group (p params)
      (if (match-punc-ls "," p)
          (incf count)
          )
      )
    (cond
      ((and (> count 0) (> count n))
       ;;; TODO Remember we have to ignore parens, commas, and white-space
       (nthcadr n pls)
       )
      )
    )
  )

(defun pathname-to-string (p)
  "Note that the 'flatten' is in there because some bindings, (such as binding
   to an array) contain lists at the toplevel instead of characters"
  (char-ls-to-str (flatten p)))

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
  (reduce #'str-cat (intersperse path "/")))

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

(defun print-js-paths (index &optional pov &key sort)
  (let* ((pairs nil)
         (printable nil))
    (if (or (not pov) (equal pov :down))
        (setf pairs (path-index-str index))
        (setf pairs (path-index-revstr index)))
    (setf printable (map 'list #'print-js-path pairs))
    (list (path-index-file index)
          (if (and sort)
              (sort printable #'string<)
              printable))
    )
  )

(defun match-path (p1 p2)
  (cond
    ((equal p1 p2)
     ;(puts "p1: ~A ~% p2: ~A " p1 p2)
     t)
    ((and t)
     nil)
    )
  )

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

(defun auto-walk-tree (tree walk &key mutate-node)
  (assert tree)
  (let ((cur tree))
    (do-group (m n walk)
      (cond
        ((eql 'd m)
         (if (and mutate-node (not n))
             (setf (car cur) (funcall mutate-node (car cur)))
             )
         (setf cur (car cur))
         (assert cur)
         )
        ((eql 'r m)
         (if (and mutate-node (not n))
             (setf (cdr cur) (funcall mutate-node (cdr cur)))
             )
         (setf cur (cdr cur))
         (assert cur)
         )
        )
      )
    cur
    )
  )

(defun splice-subtree-aux (tree walk subtree)
  (cond
    ((not walk)
     (setf (car tree) subtree))
    ((eql 'd (car walk))
     (splice-subtree-aux  (car tree) (cdr walk) subtree))
    ((eql 'r (car walk))
     (splice-subtree-aux (cdr tree) (cdr walk) subtree))
    ((and t)
     (assert nil))
    ))

(defun splice-subtree (tree walk subtree)
  (splice-subtree-aux tree walk subtree)
  tree ;;; XXX VERIFY that this contains a spliced-in subtree
  )

(defun get-path-subtrees (path index ast &optional pov)
  (let* ((pairs nil))
    (if (or (not pov) (equal pov :down))
        (setf pairs (path-index-str index))
        (setf pairs (path-index-revstr index)))
    (list (path-index-file index) (remove nil
            (map 'list
                 #'(lambda (p)
                     (cond
                       ((string= path "/")
                        ast)
                       ((match-path path (fmt-path (car p)))
                        (auto-walk-tree ast (unfold-list (cadr p))))))
                 pairs))))
  )

(defun get-path-walk (path index ast &optional pov)
  (let* ((pairs nil))
    (if (or (not pov) (equal pov :down))
        (setf pairs (path-index-str index))
        (setf pairs (path-index-revstr index)))
    (list (path-index-file index) (remove nil
                (map 'list
                      #'(lambda (p)
                          (cond
                            ((string= path "/")
                             '())
                            ((match-path path (fmt-path (car p)))
                             (unfold-list (cadr p)))))
                      pairs))))
  )

(defun get-path-subtrees-str (path index ast &optional pov)
  (map 'list #'ast-to-str (get-path-subtrees path index ast pov)))

(defun get-path-node-count (test index ast idx-bool &optional pov &key zero pre)
  (let* ((pairs nil))
    (if (or (not pov) (equal pov :down))
        (setf pairs (path-index-str index))
        (setf pairs (path-index-revstr index)))
    (list (path-index-file index)
              (remove nil (map 'list
                               ;;; TODO use get-subtree here
                   #'(lambda (p)
                       (let ((count (count-node-in-ast test idx-bool
                                                       (get-path-subtrees
                                                        (fmt-path (car p))
                                                        index ast pov) :deep t)))
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
               #'(lambda (p)
                   (cond
                     ((is-str-prefix prefix (fmt-path (car p)))
                      (fmt-path (car p)))
                     )
                   )
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
  (puts "~%name: ~a" (char-ls-to-str (get-fcall-name fls)))
  )

(defun print-fcall-args(fls)
  (puts "name: ~a args: ~a" (char-ls-to-str (get-fcall-name fls))
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

(defun xform-ast (ast xform stack)
  (if (not (listp ast))
      (return-from xform-ast ast))
  (let* ((ret nil))
    (setf ret
          (remove nil
                  (reduce #'append
                          (mapahead
                               #'(lambda (prev n rest)
                                       (multiple-value-list
                                        (funcall xform prev n rest stack)))
                               ast))))
    (setf ret (map 'list
                   #'(lambda (n)
                       (xform-ast n xform (pushr stack n)))
                   ret))
    ret
    )
  )

(defun copy-ast (ast test)
  (xform-ast ast #'(lambda (p n r s) (if (funcall test n) n)) (list ast)))

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
  (list (gen-ws-aux '() ns)))

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
  (list (gen-nl-ws-aux '(#\Newline) ns)))


(defun is-js-hyperlink (n)
  (and (listp n) (> (length n) 4)
       (and (characterp (car n)) (CHAR= (car n) #\<))
       (and (characterp (cadr n)) (CHAR= (cadr n) #\<))
       (and (characterp (caddr n)) (CHAR= (caddr n) #\<))
       (and (characterp (cadddr n)) (CHAR= (cadddr n) #\Space))
       ))

(defun ast-count-chars (ast)
  (length (flatten ast))
  )

(defun never (x)
  nil
  )

(defun count-leading-spaces (ls)
  (let ((c 0)
        (stop nil)
        )
    (do-group (e1 e2 ls)
      (when (and (not stop) (equal e1 #\Space))
        (incf c)
        )
      (when (not (equal e2 #\Space))
        (setf stop t)
        )
      )
    c
    )
  )

(defun space-indices (ls)
  (let ((ret '())
        (ix 0)
        )
    (do-group (c ls)
      (if (equal c #\Space)
          (pushr! ret ix)
          )
      (incf ix)
      )
    ret
    )
  )

(defun furthest-space (char-ind &optional max-ix)
  (setf max-ix 80)
  (let ((ret 0))
    (do-group (n char-ind)
      (if (< n max-ix)
          (setf ret n)
          )
      )
    ret
    )
  )

(defun break-at (line pos new-ind)
  (let ((l1 '())
        (l2 '())
        (ix 0)
        )
    (do-group (c line)
      (when (< ix pos)
        (pushr! l1 c)
        )
      (when (= ix pos)
        (dotimes (i new-ind)
          (pushr! l2 #\Space)
          )
        )
      (when (> ix pos)
        (pushr! l2 c)
        )
      (incf ix)
      )
    (list l1 l2)
    )
  )

(defun trim-ws (ln)
  (let ((rev-ln (reverse ln))
        )
    (if (not (equal #\Space (car rev-ln)))
        (return-from trim-ws ln)
        )
    (do-cons (e rev-ln)
      (when (not (equal #\Space (car e)))
        (return-from trim-ws (reverse e))
        )
      )
    )
  )

(defmacro! is-breakable (x &optional stack)
  `(funcall breakable-test ,x ,stack)
  )

(defmacro! is-unbreakable-char (x)
  `(and (characterp ,x) (not (is-breakable ,x)))
  )


(defun build-break-table (ast max-line-len breakable-test)
  (let ((table '())
        (line-num 1)
        (col-num 1)
        (pre-brk 0)
        (pre-brk-walk '())
        (post-brk 0)
        (count-lws t)
        (lead-ws 0)
        )
    (walk-tree ast
               #'(lambda (n)
                   (or (is-unbreakable-char n) (is-breakable n))
                   )
               #'(lambda (n stack w)
                   (cond
                     ((and (is-unbreakable-char n) (CHAR= #\Newline n))
                      (pushr! table (list line-num col-num pre-brk post-brk
                                          lead-ws pre-brk-walk))
                      (incf line-num)
                      (setf col-num 1)
                      (setf lead-ws 0)
                      (setf pre-brk 0)
                      (setf pre-brk-walk '())
                      (setf post-brk 0)
                      (setf count-lws t)
                      )
                     ((and (is-unbreakable-char n) (CHAR= n #\Space) count-lws)
                      (incf lead-ws)
                      (incf col-num)
                      )
                     ((and (is-unbreakable-char n) (not (CHAR= n #\Space)) count-lws)
                      (setf count-lws nil)
                      (incf col-num)
                      )
                     ((and (is-unbreakable-char n) (not (CHAR= #\Newline n)))
                      (incf col-num)
                      )
                     ((is-breakable n stack)
                      (cond
                        ((<= (+ (length (flatten n)) col-num) max-line-len)
                         (incf pre-brk)
                         (setf pre-brk-walk w)
                         )
                        ((> (+ (length (flatten n)) col-num) max-line-len)
                         (incf post-brk)
                         )
                        )
                      )
                     )
                   )
               '() '())
    table
    )
  )

(defun fmt-xform-string (x)
  x)

(defun have-breakable-overflowing-lines (tbl max-ln)
  (let ((ret nil))
    (do-group (r tbl)
      (cond
        ((and (> (cadr r) max-ln)
              (> (caddr r) 0))
         (setf ret t)
         (return)
         )
        )
      )
    ret
    )
  )

(defun fmt-break-lines (ast max-line-len breakable-after)
  (let ((brk-tbl '())
        (new-ast '())
        (extra-indent 0))
    (setf brk-tbl (build-break-table ast max-line-len
                                     #'(lambda (x stack)
                                         (let ((ret nil))
                                           (do-group (b breakable-after)
                                             (cond
                                               ((funcall (car b) x stack)
                                                (setf ret t)
                                                (setf extra-indent (cadr b))
                                                (return)
                                                )
                                               )
                                             )
                                           ret
                                           )
                                         )))
    ;(puts "brk-tbl: ~a" brk-tbl)
    (if (not (have-breakable-overflowing-lines brk-tbl max-line-len))
        (return-from fmt-break-lines ast)
        )
    (do-group (ln brk-tbl)
      (cond
        ((and (> (nthcadr 1 ln) max-line-len)
              (> (nthcadr 2 ln) 0))
         (auto-walk-tree ast (nthcadr 5 ln)
                         :mutate-node #'(lambda (y)
                                          (list 'break (nthcadr 4 ln) y)
                                          ))
         )
        )
      )
    (setf new-ast (xform-ast ast
               #'(lambda (p n r stk)
                   (cond
                     ((and (listp n) (equal (car n) 'break))
                      (values (caddr n) (gen-nl-ws (+ (cadr n)
                                                      (- extra-indent 1))))
                      )
                     ((and t)
                      (values n)
                      )
                     )
                   )
               (list ast)))
    (fmt-break-lines new-ast max-line-len breakable-after)
    )
  )

(defun replace-subseq (sub rep ls)
  (let ((ret '())
        (dead 0)
        (sub-len (length sub))
        )
    (do-cons (e ls)
      (cond
        ((and (= dead 0) (equal (head-n e sub-len) sub))
         (setf dead (- sub-len 1))
         (setf ret (append ret rep))
         )
        ((= dead 0)
         (pushr! ret (car e))
         )
        ((> dead 0)
         (decf dead)
         )
        )
      )
    ret
    )
  )

(defun fmt-tabify (ast tabify)
  (assert (numberp tabify))
  (xform-ast ast
             #'(lambda (p n r s)
                 (cond
                   ((is-white-space-group n)
                    (values (replace-subseq (car (gen-ws tabify)) '(#\Tab) n))
                    )
                   ((and t)
                    (values n)
                    )
                   )
                 )
             (list ast))
  )

(defun fmt-rules (ast &key fold unfold max-line-len indent-chars indent
                          breakable-after break-indent spacing
                          special-breaks tabify)
  (let ((ret '())
        )

    ;;; strip out all blanks
    (setf ret (copy-ast ast
                        #'(lambda (n)
                            (not (is-blank-group n)))))

    ;(puts "copy-ast:~% ~A" ret)
    ;;; spacing
    (setf ret (xform-ast ret
                         #'(lambda (p n r stk)
                             (let ((ret '()))
                               (do-group (s spacing)
                                 (cond
                                   ((funcall (car s) n (car r) stk)
                                    (setf ret (cadr s))
                                    (return)
                                    )
                                   )
                                 )
                               (if (not ret)
                                   (values n)
                                   (values n ret))
                               )
                             )
                         (list ret)))
    ;(puts "spacing:~% ~A" ret)
    ;;; folding
    (setf ret (if (not unfold)
                  (xform-ast ret
                             #'(lambda (p n r stk)
                                 (let ((ret '()))
                                   (do-group (f fold)
                                     (cond
                                       ((funcall (car f) n stk)
                                        (setf ret (funcall (cadr f) n))
                                        (return)
                                        )
                                       ((and t)
                                        )
                                       )
                                     )
                                   (if (not ret)
                                       (values n)
                                       (values ret))
                                   )
                                 )
                             (list ret))
                  ret))
    ;(puts "folding:~% ~A" ret)
    ;;; indents

    (setf ret (xform-ast ret
                         #'(lambda (p n r stk)
                             (let ((ret '())
                                   (fn nil))
                               ;(puts "N: ~A ~A" n (car r))
                               (do-group (x indent)
                                 (cond
                                   ((funcall x n (car r) stk)
                                    (setf ret t)
                                    (setf fn x)
                                    (return)
                                    )
                                   )
                                 )
                               (if (not ret)
                                   (values n)
                                   (funcall fn n (car r) stk))
                               )
                             )
                         (list ret)))

    ;(puts "indents:~% ~A" ret)
    ;;; breaks
    (setf ret (fmt-break-lines ret max-line-len breakable-after))
    ;;; special-breaks

    ;;; tabification
    (if (and tabify)
        (setf ret (fmt-tabify ret tabify))
        )
    ret
    )
  )

(defmacro! with-indent-width (var num &body body)
  `(let ((,var ,num))
     ,@body
     )
  )

(defmacro! fmt-rules-with-indent-width (var num &body body)
  `(with-indent-width ,var ,num
     (fmt-rules ,@body))
  )

(defmacro! js-joyent-style-base (ast &key spaces indent (max-line-len 80)
                                     tabify unfold)
  `(fmt-rules-with-indent-width
    spaces ,spaces ,ast
             :unfold ,unfold
             :fold (list
                    (list
                     #'(lambda (n stack)
                         (and (is-js-fdef-binding n)
                              (member-if #'(lambda (x)
                                             (or (is-js-fdef x)
                                                 (is-js-fdef-binding x)))
                                         stack)
                              )
                         )
                     #'(lambda (n)
                         (append (str-to-char-ls "<<< ")
                                 (get-js-fbind-name n)
                                 (str-to-char-ls "=/ >>>"))
                         )
                     )
                    (list
                     #'(lambda (n stack)
                         (and (is-js-fdef n)
                              (member-if #'(lambda (x)
                                             (or (is-js-fdef x)
                                                 (is-js-fdef-binding x)))
                                         stack)
                              (not (is-js-fdef-binding (car (last stack))))
                              )
                         )
                     #'(lambda (n)
                         (append (str-to-char-ls "<<< ")
                                 (get-js-fdef-name n)
                                 (str-to-char-ls "{}/ >>>"))
                         )
                     )
                    )
             :max-line-len ,max-line-len
             :indent (list
                            #'(lambda (x y stack)
                                (cond
                                  ((and (equal #\{ x) (not (equal  #\} y)))
                                   (let ((depth (count-if #'is-curly-group
                                                          stack)))
                                     (values x (gen-nl-ws (* depth spaces)))
                                     )
                                   )
                                  ((and t)
                                   nil
                                   )
                                  )
                                )
                            #'(lambda (x y stack)
                                (cond
                                  ((and (not (equal #\{ x))
                                        (equal #\} y))
                                   (let ((depth (count-if #'is-curly-group
                                                          stack)))
                                     (values x (gen-nl-ws (* (- depth 1) spaces)))
                                     )
                                   )
                                  ((and t)
                                   nil)
                                  )
                                )

                            #'(lambda (x y stack)
                                (cond
                                  ((and (or (is-js-any-ctl-stmt x) (is-curly-group x))
                                        (match-str-list "else" y)
                                        )
                                   (let ((depth (count-if #'is-curly-group
                                                          stack)))
                                     (values x (gen-ws 1))
                                     )
                                   )
                                  ((and t)
                                   nil)
                                  )
                                )
                            #'(lambda (x y stack)
                                (cond
                                  ((and (is-js-any-ctl-stmt x)
                                        (not (match-str-list "else" y))
                                        (not (is-semi y)))
                                   (let ((depth (count-if #'is-curly-group
                                                          stack)))
                                     (values x (gen-nl-ws (* depth spaces)))
                                     )
                                   )
                                  ((and t)
                                   nil)
                                  )
                                )
                            #'(lambda (x y stack)
                                (cond
                                  ((and y
                                        (is-curly-group x)
                                        (not (match-str-list "else" y))
                                        (not (is-semi y))
                                        (not (is-paren-group (car (last stack)))))
                                   (let ((depth (count-if #'is-curly-group
                                                          stack)))
                                     (values x (gen-nl-ws (* depth spaces)))
                                     )
                                   )
                                  ((and t)
                                   nil)
                                  )
                                )

                            #'(lambda (x y stack)
                                (cond
                                  ((and (is-js-hyperlink x) (not (match-str-list ";" y)))
                                   (let ((depth (count-if #'is-curly-group
                                                          stack)))
                                     (values x (gen-nl-ws (* depth 8)))
                                     )
                                   )
                                  ((and t)
                                   nil)
                                  )
                                )
                            #'(lambda (x y stack)
                                (cond
                                  ((and (match-str-list ";" x)
                                        (not (is-paren-group
                                              (car (last stack)))))
                                   (let ((depth (count-if #'is-curly-group
                                                          stack)))
                                     (values x (gen-nl-ws (* depth spaces)))
                                     )
                                   )
                                  ((and t)
                                   nil)
                                  )
                                )
                            #'(lambda (x y stack)
                                (cond
                                  ((and (is-comma x) ;(is-word-group y)
                                    (and
                                     (is-curly-group (car (last stack)))
                                     (not (is-js-fdef (cadr (reverse stack))))
                                     ))
                                   (let ((depth (count-if #'is-curly-group
                                                          stack)))
                                     (values x (gen-nl-ws (* depth spaces)))
                                     )
                                   )
                                  ((and t)
                                   nil)
                                  )
                                )
                            ,@indent
                           )
             :breakable-after (list
                               (list #'(lambda (x stack)
                                         (and (equal #\( x)
                                              (not (is-str (car (last stack)))))
                                         ) 4)
                               (list #'(lambda (x stack)
                                         (and (match-any-puncs-ls
                                               '("|" "||" "&" "&&" "^"
                                                 "," "+" ":" "?" "-"
                                                 "*" "/" "%" ;">" "<"
                                                 "=" "(" ">>>"
                                                 ;">=" "<=" "=" "=="
                                                 ;"!=" "!==" "==="
                                                ) x)
                                              (not (is-str (car (last stack))))
                                              )) 4)
                                    )
             :break-indent '(gte-prev lte-next)
             :spacing (list (list
                             #'(lambda (x y stack)
                                 (and (is-comma x)
                                      (or (is-word-group y)
                                          (is-js-fcall y)
                                          (is-js-mbr-chain y)
                                          (is-paren-group y)
                                          (is-js-fdef y))
                                      (not (is-curly-group (car (last stack)))))
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (y x stack)
                                 (and (or (is-js-any-ctl-stmt x)
                                          (is-curly-group x))
                                      (match-str-list "else" y))
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (is-comma x) (is-str y))
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (is-semi x)
                                      (is-paren-group (car (last stack))))
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (or (is-word-group x)
                                          (is-js-mbr-chain x)
                                          (is-str x)
                                          (is-js-fcall x)
                                          (is-js-arr x)
                                          (is-bracket-group x)
                                          )
                                      (and (not (is-comma y))
                                           (not (is-dot y))
                                           (not (or (is-semi y)))
                                           (is-punctuation-group y))
                                      )
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (and (is-punctuation-group x)
                                          )
                                      (and ;(not (is-comma y))
                                           ;(not (is-dot y))
                                           ;(not (or (is-semi y)))
                                           (is-curly-group y))
                                      )
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (y x stack)
                                 (and (or (is-word-group x)
                                          (is-js-mbr-chain x)
                                          (is-str x)
                                          (is-js-fcall x)
                                          (is-js-fdef x)
                                          (is-js-arr x)
                                          (is-bracket-group x))
                                      (and (not (is-comma y))
                                           (not (is-dot y))
                                           (not (or (is-semi y)))
                                           (is-punctuation-group y)
                                           (not (match-str-list "!" y)))
                                      )
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (is-word-group x)
                                      (or (is-word-group y)
                                          (is-js-binding y)
                                          (is-js-fcall y)
                                          (is-js-mbr-chain y))
                                      )
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (is-punctuation-group x)
                                      (is-punctuation-group y)
                                      )
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (is-word-group x)
                                      (or (is-js-ctl-struct-name x)
                                          (match-str-list "function" x))
                                      (is-paren-group y))
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (is-paren-group x)
                                      (or (is-curly-group y)
                                          (is-word-group y)
                                          (is-js-fcall y)
                                          )
                                      )
                                 )
                             (gen-ws 1))
                            )
             :special-breaks (list
                              #'(lambda (x y stack)
                                  (is-str x)
                                  )
                              #'fmt-xform-string)
             :tabify ,tabify
             )
  )

(defun js-joyent-style (ast &key unfold)
  (js-joyent-style-base ast :spaces 4 :unfold unfold)
  )

(defun js-dap-like-style (ast &key unfold)
  (js-joyent-style-base ast :spaces 8 :tabify 8
                            :unfold unfold
                            :indent (
                                     #'(lambda (x y stack)
                                         (cond
                                           ((and (is-curly-group y))
                                            (let ((depth (count-if #'is-curly-group
                                                                   stack)))
                                              (if (= depth 0)
                                                  (values x (gen-nl-ws (* 0 spaces))
                                                          )
                                                  (values x))
                                              )
                                            )
                                           ((and t)
                                            nil
                                            )
                                           )
                                         )
                                     )
                            )
  )

(defun c-joyent-style (ast)
  (fmt-rules ast
             :fold (list
                    (list
                     #'(lambda (n stack)
                         (and (is-c-fdef n)
                              (member-if #'(lambda (x)
                                             (is-c-fdef x))
                                         stack)
                              )
                         )
                     #'(lambda (n)
                         (append (str-to-char-ls "<<< ")
                                 (get-js-fdef-name n)
                                 (str-to-char-ls "{}/ >>>"))
                         )
                     )
                    )
             :max-line-len 80
             :indent (list
                            #'(lambda (x y stack)
                                (cond
                                  ((and (equal #\{ x) (not (equal  #\} y)))
                                   (let ((depth (count-if #'is-curly-group
                                                          stack)))
                                     (values x (gen-nl-ws (* depth 8)))
                                     )
                                   )
                                  ((and t)
                                   nil
                                   )
                                  )
                                )
                            #'(lambda (x y stack)
                                (cond
                                  ((and (not (equal #\{ x))
                                        (equal #\} y))
                                   (let ((depth (count-if #'is-curly-group
                                                          stack)))
                                     (values x (gen-nl-ws (* (- depth 1) 8)))
                                     )
                                   )
                                  ((and t)
                                   nil)
                                  )
                                )
                            #'(lambda (x y stack)
                                (cond
                                  ((and y
                                        (is-curly-group x)
                                        (not (match-str-list "else" y))
                                        (not (is-semi y))
                                        (not (is-paren-group (car (last stack)))))
                                   (let ((depth (count-if #'is-curly-group
                                                          stack)))
                                     (values x (gen-nl-ws (* depth 8)))
                                     )
                                   )
                                  ((and t)
                                   nil)
                                  )
                                 )
                            #'(lambda (x y stack)
                                (cond
                                  ((and (is-js-hyperlink x) (not (match-str-list ";" y)))
                                   (let ((depth (count-if #'is-curly-group
                                                          stack)))
                                     (values x (gen-nl-ws (* depth 8)))
                                     )
                                   )
                                  ((and t)
                                   nil)
                                  )
                                )
                            #'(lambda (x y stack)
                                (cond
                                  ((and (match-str-list ";" x)
                                        (not (is-paren-group
                                              (car (last stack)))))
                                   (let ((depth (count-if #'is-curly-group
                                                          stack)))
                                     (values x (gen-nl-ws (* depth 8)))
                                     )
                                   )
                                  ((and t)
                                   nil)
                                  )
                                )
                            #'(lambda (x y stack)
                                (cond
                                  ((and (is-comma x)
                                    (and
                                     (is-curly-group (car (last stack)))
                                     (not (is-c-fdef (cadr (reverse stack))))
                                     ))
                                   (let ((depth (count-if #'is-curly-group
                                                          stack)))
                                     (values x (gen-nl-ws (* depth 8)))
                                     )
                                   )
                                  ((and t)
                                   nil)
                                  )
                                )
                           )
             :breakable-after (list
                               (list #'(lambda (x stack)
                                         (and (equal #\( x)
                                              (not (is-str (car (last stack)))))
                                         ) 4)
                               (list #'(lambda (x stack)
                                         (and (match-any-puncs-ls '("|" "||" "&" "&&" "^"
                                                                    "," "+" ":" "?" "-"
                                                                    "*" "/" "%" ;">" "<"
                                                                    "="
                                                                    ;">=" "<=" "=" "=="
                                                                    ;"!=" "!==" "==="
                                                                    ) x)
                                              (not (is-str (car (last stack))))
                                              )
                                         )
                                     4)
                               )
             :break-indent '(gte-prev lte-next)
             :spacing (list (list
                             #'(lambda (x y stack)
                                 (and (is-comma x)
                                      (or (is-word-group y)
                                          (is-c-fcall y)
                                          ;(is-js-mbr-chain y)
                                          (is-paren-group y))
                                      (not (is-curly-group (car (last stack)))))
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (y x stack)
                                 (and (or ;(is-js-any-ctl-stmt x)
                                          (is-curly-group x))
                                      (match-str-list "else" y))
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (is-comma x) (is-str y))
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (is-semi x)
                                      (is-paren-group (car (last stack))))
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (or (is-word-group x)
                                          ;(is-js-mbr-chain x)
                                          (is-str x)
                                          (is-c-fcall x)
                                          ;(is-js-arr x)
                                          (is-bracket-group x)
                                          )
                                      (and (not (is-comma y))
                                           (not (is-dot y))
                                           (not (or (is-semi y)))
                                           (is-punctuation-group y))
                                      )
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (and (is-punctuation-group x)
                                          )
                                      (and ;(not (is-comma y))
                                           ;(not (is-dot y))
                                           ;(not (or (is-semi y)))
                                           (is-curly-group y))
                                      )
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (y x stack)
                                 (and (or (is-word-group x)
                                          ;(is-js-mbr-chain x)
                                          (is-str x)
                                          (is-c-fcall x)
                                          (is-bracket-group x))
                                      (and (not (is-comma y))
                                           (not (is-dot y))
                                           (not (or (is-semi y)))
                                           (is-punctuation-group y)
                                           (not (match-str-list "!" y)))
                                      )
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (is-word-group x)
                                      (or (is-word-group y)
                                          ;(is-js-binding y)
                                          (is-c-fcall y)
                                          ;(is-js-mbr-chain y)
                                          )
                                      )
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (is-punctuation-group x)
                                      (is-punctuation-group y)
                                      )
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (is-word-group x)
                                      (or (is-ctl-struct-name x)
                                          (match-str-list "function" x))
                                      (is-paren-group y))
                                 )
                             (gen-ws 1))
                            (list
                             #'(lambda (x y stack)
                                 (and (is-paren-group x)
                                      (or (is-curly-group y)
                                          (is-word-group y)
                                          (is-c-fcall y)
                                          )
                                      )
                                 )
                             (gen-ws 1))
                            )
             :special-breaks (list
                              #'(lambda (x y stack)
                                  (is-str x)
                                  )
                              #'fmt-xform-string)
             :tabify 8
             )
  )

(defun gen-chars (&rest cs)
  (let* ((res '()))
    (do-group (c cs)
      (cond
        ((characterp c)
         (pushr! res c))
        ((stringp c)
         (setf res (append res (str-to-char-ls c))))
        ((and t)
         (assert nil))
        )
      )
    res
    )
  )

(defun valve (test val)
  (if (funcall test val) val (assert nil)))

(defmacro! gen-word (&rest cs)
  `(valve #'validate-word (gen-chars ,@cs)))

(defmacro! gen-blank ()
  `(valve #'validate-blank (enclose (gen-ws 1))))

(vdefun js-to-ast (input)
  (pipeline+ input cmt-str-regex white-space
            blanks words punctuations
            nestables
            js-fdefs
            js-fcalls
            js-arrs
            js-var-bindings
            js-mbr-chains
            js-obj-lit-recs
            js-vars
            js-curly-ctl-stmts
            js-do-while-stmts
            js-flat-ctl-stmts
            ;#'js-if-else-chains
            ))


(defun html-start-tag (tag)
  (pushr '() '(#\<) (str-to-char-ls tag) '(#\>))
  )

(defun html-end-tag (tag)
  (pushr '() '(#\< #\/) (str-to-char-ls tag) '(#\>))
  )



(defmacro! html-recursive-grouper (parent tag)
  `(let* ((ls (head-n (cons head tail) size))
          (inside (poprl ls))
          (res (pushr (pushl (,parent inside)
                             (html-start-tag ,tag))
                      (html-end-tag ,tag)))
          )
    (list res
          (car (popl-n tail (- size 1)))
          (popl-n tail size)
          )
    )
  )


;;; XXX can this be used as an argument to a macro
(defun html-test-pub (name)
  (let* ((pub-fn (string-to-symbol (str-cat "is-html-" name)))
         (inner-test (string-to-symbol (str-cat "html-" name "p")))
         (ret (list pub-fn (list 'ls) (list 'and (list 'listp 'ls)
                                            (list inner-test
                                                  (list 'car 'ls)
                                                  (list 'cdr 'ls)
                                                  ))))

        )
    ;`(,pub-fn (ls)
              ;(and (lisp ls)
                   ;(,inner-test (car ls) (cdr ls))
                   ;)
              ;)
    (list ret)
    )
  )

(defmacro! def-html-stage (name tag &key when tests-pub)
  (let* ((default-test-pub (html-test-pub name))
         (tests-pub (append default-test-pub tests-pub))
         (parent (string-to-symbol (str-cat "html-" name "s")))
         )
    `(defstage "html" ,name
       :test (nestable-match (cons head tail)
                             #'(lambda (x) (match-html-start-tag ,tag x))
                             #'(lambda (x) (match-html-end-tag ,tag x))
                             )
       :group-custom (html-recursive-grouper ,parent ,tag)
       :when ,when
       :tests-pub ,tests-pub
       )
    )
  )

(defstage "html" "tag"
  :test (finite-match (cons head tail)
                      (list
                       (list :m #'(lambda (n)
                                    (match-any-puncs-ls '("</" "<")  n)))
                       (list :o #'is-blank-group)
                       (list :m #'is-word-group)
                       (list :o #'is-blank-group)
                       (list :mc #'is-html-attribute-list)
                       (list :m #'(lambda (n) (match-punc-ls '(">") n)))
                       )
         )
  :tests-aux ((is-html-attribute-list (head tail)
                                      (finite-repeating-match (cons head tail)
                                        (list
                                          (list :m #'is-word-group)
                                          (list :o #'is-blank-group)
                                          (list :m #'is-eq-group)
                                          (list :o #'is-blank-group)
                                          (list :m #'(lambda (x)
                                                      (or (is-word-group x)
                                                          (is-str x))))
                                          )
                                        (list
                                          (list :m #'(lambda (x) nil)))
                                        :opt-ematch t
                                      )
              ))
  )

(defun match-html-start-tag (str x)
  (and (html-tagp x) (match-punc-ls "<" (car x))
       (or (match-str-list str (cadr x))
           (match-str-list str (caddr x)))
       )
  )

(defun match-html-end-tag (str x)
  (and (html-tagp x) (match-punc-ls "</" (car x))
       (or (match-str-list str (cadr x))
           (match-str-list str (caddr x)))
       )
  )


(def-html-stage "paragraph" "p")

(def-html-stage "link" "a"
  :when ((:paragraph descend))
  )

(def-html-stage "bold" "b"
  :when ((:paragraph descend)
         (:link descend)
         )
  )

(def-html-stage "emph" "em"
  :when ((:paragraph descend)
         (:link descend)
         (:bold descend)
         )
  )

(def-html-stage "blockquote" "blockquote"
  :when ((:paragraph descend)
         (:link descend)
         (:bold descend)
         (:emph descend)
         )
  )

(def-html-stage "bod" "body"
  :when ((:paragraph descend)
         (:link descend)
         (:bold descend)
         (:emph descend)
         (:blockquote descend)
         )
  )

(def-html-stage "div" "div"
  :when ()
  )

;(def-html-stage "tag-pair"
  ;)

(vdefun html-to-ast (input)
  (pipeline input
            #'html-cmt-str
            #'white-space
            #'blanks
            #'words
            #'punctuations
            #'html-tags
            #'html-paragraphs
            #'html-links
            #'html-bolds
            #'html-emphs
            #'html-blockquotes
            )
  )



(defun ws-has-newline (e)
  (and e (is-white-space-group e) (member #\Newline e)))

(defun lines (ls)
  (let ((ret '())
        (acc '())
        )
    (do-group (e ls)
      (cond
        ((and (is-white-space-group e) (member #\Newline e))
         (pushr! acc e)
         (pushr! ret acc)
         (setf acc '())
         )
        ((and t)
         (pushr! acc e)
         )
        )
      )
    (if (and acc)
        (pushr! ret acc))
    ret
    )
  )

(vdefun eu-csv-to-ast (input)
  (pipeline input #'cmt-str #'white-space #'words #'punctuations
            #'lines)
  )

(defun grep (ast cmp)
  (let ((new '())
        )
    (do-group (l ast)
      (cond
        ((funcall cmp l)
         (pushr! new l)
         )
        )
      )
    new
    )
  )

(defun cl-to-ast (input)
  (pipeline input #'cl-cmt-str #'white-space
            #'cl-blanks #'symbols #'cl-punctuations
            #'cl-nestables
   ))

(defun test-cl-to-ast ()
  (cl-to-ast (str-to-char-ls "(defun foo (a b c) (+ a b c))")))

(defun test-if-parse ()
  (let ((in1 "call(\"arg\", function (a, b) { if (true) if (true) { if (true) { return 1 }};});")
        (in2 " if (x) if (y) {(10 * 10)};")
        (in3 "if (x) if (y) (10 * 10);")
        (in4 "if (x) { (10 * 10); }")
        (in5 "if (z) {do { if (a) do { (10 * 10) } while (y); } while (x);}")
        (in6 "if (x) { if (z) while (y) { (10 * 10); };}")
        (in7 "function (a) { return {a: function () { if (a) { return a.b.c } } }}")
        (in8 "a.b.c = {a: function () { if (a) { return a.b.c } } };")
        (in9 "{a: b, c: d}")
        )
  (js-to-ast
   (str-to-char-ls in9))))

(defun test-varbind-parse ()
  (let ((in1 "if (x) { function foo (a, b) {var mvar = 2 + 2 + call(); for (mvar = 0; true; mvar++) { print('heeeeeeey'); }}; bar(); foo(); x = 4.2; }")
        (in2 "if (a, b) {call(); var func = function foo (a, b) {hello = function (a) {b};};}")
        (in3 "function a (1, 2) {foo = function (1, 2) {4};}"))
    (js-to-ast
     (str-to-char-ls in2))))

(defun test-mbrchain-parse ()
  (let ((in2 "if (a, b) {call().a.b.c; var func = function foo (a, b) {hello.fn = function (a) {b.mycall().mymbr = {a: b, b: c, c: d.e, e: f};};};}")
        (in3 "function (a) {a.b[10].c[10] = function (a) {myCall(); function () {a.b.c}; return a = a.b.call(function () {a.b.c});};}")
        (in4 "a. b .c = q.w.e;"))
    (js-to-ast
     (str-to-char-ls in4))))


;;; TODO implement this
(defun c-pre-proc (ls)
 ;;;(c-pre-proc-aux '() (car ls) (cdr ls)
  )

(vdefun c-to-ast (input)
  (pipeline input #'cmt-str #'white-space
            #'blanks #'words #'punctuations
            #'nestables #'c-fdefs #'c-fcalls))


(defun test-ast ()
  (js-to-ast
   (str-to-char-ls "var foo = require /*cmt*/ ('module'); var bar = require('module');var myfunc = function foo (a, b, c) { var func2 = function (abc) { return x } };")))

(defun test-c-ast ()
  (c-to-ast
   (str-to-char-ls "/*cmt*/ void mdb_set_config(const char *s) { return (1 + 2 + 3); }")))

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

(defvar git-jobs 0)

(defun update-git-cmd-status (proc stat sig)
  ;(print "stat")
  ;(print stat)
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
  (if (file-exists-p (str-cat blip-repos "gerrit" "/" user-repo "/"))
      (git-cmd-svc-user "pull" "gerrit" user-repo)))

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
    (do-group (b branches) (cache-git-log repo b))))

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
  (do-group (targ (fetchable-remote-targs repo))
    (git-fetch-remote repo targ)))

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


(vdefun print-form (n s w)
  (if (is-commit n)
      (print (caddr n))
      (print (file-mod-stringify-path n))
      ))


(vdefun build-ast-dir (repo)
  (pushdir (str-cat blip-asts repo))
  (do-group (f (git-show-ftree-all-time repo))
    (mkdir (str-cat (cwd) "/" f)))
  (popdir)
  )

(vdefun build-meta-dir (repo)
  (let* ((dir (str-cat blip-repo-meta repo "/root")))
    (mkdir dir)
    (pushdir dir)
    (do-group (f (git-show-ftree-all-time repo))
      (mkdir (str-cat (cwd) "/" f))
      )
    (popdir)
    )
  )

(vdefun strap-git-repo (repo)
  (build-meta-dir repo)
  (build-ast-dir repo))

(vdefun git-show-ftree (repo commit)
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

(vdefun files-present-at-commit (repo commit)
  (git-show-ftree repo commit))

(vdefun git-show-ftree-all-time (repo)
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

(vdefun git-head-commit (repo)
  (let ((hc nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf hc (inferior-shell:run/ss (list "git" "rev-parse" "HEAD")))
    (popdir)
    hc
    ))

(vdefun git-all-commits (repo)
  "Used to get list of all commit SHAs."
  (let ((rc nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf rc (inferior-shell:run/ss (list "git" "rev-list"
                                          "HEAD")))
    (popdir)
    (str-split "\\n" rc)
    ))

(vdefun number-each-elem (ls)
  "Turns a list of e, into a list (e, n) where n is the number of the element"
  (let ((n 0)
        (new-ls '()))
    (do-group (e ls)
      (setf new-ls (cons (list e n) new-ls))
      ;(pushr! new-ls (list e n))
      (incf n)
      )
    (reverse new-ls)
    )
  )

(vdefun git-root-commits (repo)
  "Used to get root commits (like the first commit, and merges)"
  (let ((rc nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf rc (inferior-shell:run/ss (list "git" "rev-list"
                                          "--max-parents=0"
                                          "HEAD")))
    (popdir)
    (str-split "\\n" rc)
    ))

(vdefun git-file-latest-commit-legacy (repo path)
  "Only works for files that are present in current branch"
  (let ((lc nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf lc (inferior-shell:run/ss (list "git" "rev-list"
                                          "-1" "HEAD" "--"
                                          path)))
    (popdir)
    lc
    ;(str-split "\\n" lc)
    ))

(vdefun git-file-latest-commit (repo path &optional use-index)
  (cond
    ((not use-index)
     (git-file-latest-commit-legacy repo path))
    ((and use-index)
     (let* ((dir (str-cat blip-repo-meta repo "/root/" path))
            (commits (file-to-form (str-cat dir "/LOG")))
            (res nil)
           )
       (setf res (car commits))
       ;;; Sometimes, we don't actually store the history in
       ;;; the on-disk cache, for some reason. So we
       ;;; compensate for this here, with this hack.
       ;;; TODO root cause this
       (cond
         ((not res)
          (setf res (git-file-latest-commit-legacy repo path))
          )
         )
       res
       )
     )
    )
  )

(vdefun git-files-commits (repo paths &optional stop-at)
  "Only works for files that are present in current branch"
  (let ((lc nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf lc (inferior-shell:run/ss (append (list "git" "--no-pager" "log"
                                                  "--pretty=format:%H"
                                                  "--name-only"
                                                  ;"--no-merges"
                                                  ;(if (and stop-at)
                                                      ;(str-cat stop-at "..HEAD")
                                                      ;"")
                                                  "--"
                                                  )
                                            paths)))
    (popdir)
    (mapcar #'(lambda (s) (str-split "\\n" s)) (str-split "\\n\\n" lc))
    ))

(vdefun git-rotate-log (log)
  (let ((dict (make-hash-table :test #'equal))
        (list-dict '())
        (commit nil))
    (do-group (commit-info log)
      (setf commit (car commit-info))
      (do-group (file (cdr commit-info))
        (pushr! (gethash file dict) commit)
        )
      )
    (maphash #'(lambda (f cl)
                 (pushr! list-dict (list f cl)))
             dict)
    (sort list-dict #'string< :key #'car)
    )
  )

(vdefun git-save-file-logs (repo paths)
  (let* ((cur-head (git-head-commit repo))
         (logs (git-rotate-log (git-files-commits repo paths)))
         (root (str-cat blip-repo-meta repo "/root/"))
         (target nil)
         )
    (do-group (l logs)
      (setf target (str-cat root (car l) "/"))
      (form-to-file cur-head (str-cat target "HEAD"))
      (form-to-file (cadr l) (str-cat target "LOG"))
      )
    )
  )

(vdefun git-update-file-logs (repo paths)
  (let* ((cur-head (git-head-commit repo))
         (logs (git-rotate-log (git-files-commits repo paths)))
         (known-head nil)
         (root (str-cat blip-repo-meta repo "/root/"))
         (target nil)
         (do-update nil)
         )
    (do-group (p paths)
      (setf target (str-cat root p "/"))
      (cond
        ((not do-update)
         (setf known-head (file-to-form (str-cat target "HEAD")))
         (cond
           ((not (equal cur-head known-head))
            (setf do-update t)
            )
           )
         )
        )
      )
    (if (and do-update)
        (git-save-file-logs repo paths))

    ))

(vdefun git-file-latest-commit-until-legacy (repo path commit)
  "Only works for files that are present in current branch"
  (let ((lc nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf lc (inferior-shell:run/ss (list "git" "rev-list"
                                          "-1" commit "--"
                                          path)))
    (popdir)
    lc
    ))

(vdefun git-file-latest-commit-until (repo path commit &optional cmp)
  "We expect (cmp c1 c2) to tell us if `c1` is older than the one we are looking at"
  (cond
    ((not cmp)
     (git-file-latest-commit-until-legacy repo path commit)
     )
    ((and cmp)
     (let* ((dir (str-cat blip-repo-meta repo "/root/" path))
            (commits (file-to-form (str-cat dir "/LOG")))
            (youngest-ancestor nil))
       ;;; Sometimes, we don't actually store the history in
       ;;; the on-disk cache, for some reason. So we
       ;;; compensate for this here, with this hack.
       ;;; TODO root cause this
       (cond
         ((not commits)
          (setf youngest-ancestor
                (git-file-latest-commit-until-legacy repo path commit))
          )
         ((and commits)
          (do-group (c commits)
            (cond
              ((and (equal c commit) (not youngest-ancestor))
               (setf youngest-ancestor c))
              ((and (funcall cmp c commit) (not youngest-ancestor))
               (setf youngest-ancestor c))
              ((and t)
               )
              )
            )
          )
         )
       youngest-ancestor
       )
     )
    )
  )

(vdefun git-file-all-commits (repo path)
  "Only works for files that are present in current branch"
  (let ((lc nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf lc (inferior-shell:run/ss (list "git" "rev-list"
                                          "HEAD"
                                          path)))
    (popdir)
    (str-split "\\n" lc)
    ))

(defun git-commit-info (repo commit)
  (let ((ret nil))
    (pushdir (str-cat blip-repos repo "/"))
    (setf ret (inferior-shell:run/ss (list "git" "--no-pager" "show"
                                          "-s" "--pretty=fuller"
                                          commit)))
    (popdir)
    ret
    )
  )

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
    (do-group (a amends)
      (if (and (equal (car a) 'misdeletion)
               (equal (cadr a) file))
          (setf ret t))
      )
    ret
    )
  )

(defun apply-git-log-amends (repo commit file-list)
  (let* ((amends (load-git-log-amendments repo commit)))
    (remove nil (map 'list #'(lambda (f) (if (is-misdeleted amends f) nil f)) file-list))
    )
  )


(vdefun parse-x-files-at-commit (repo commit &key force suf pref parser whitelist antipref)
  (expand-commit! repo commit)
  (let ((files (files-present-at-commit repo commit))
        (curbr (git-current-branch repo)))
    (pushdir (str-cat blip-repos repo))
    (git-branch-commit commit)
    (map 'nil #'(lambda (file)
                 (if (and (if (and suf) (is-str-suffix suf file) t)
                          (if (and pref) (is-str-prefix pref file) t)
                          (if (and antipref) (not (is-str-prefix antipref file)) t)
                          )
                     (let* ((slot (str-cat blip-asts repo "/" file))
                            (revid (git-file-latest-commit repo file t))
                            (fullpath (str-cat slot "/" revid))
                            (full-src-path (str-cat (cwd) "/" file)))
                       ;(puts "revid: ~a" revid)
                       ;(puts "full-src: ~a" full-src-path)
                       ;(puts "full-path: ~a" fullpath)
                       (assert revid)
                       (cond
                         ((and (is-file-p full-src-path)
                               (or (and force (file-exists-p fullpath))
                                   (not (file-exists-p fullpath))))
                          (cond
                            ((or (not whitelist)
                                 (member file whitelist :test #'equal))
                             (form-to-bin
                              (funcall parser
                                       (file-to-char-ls full-src-path))
                              fullpath)
                             (form-to-bin
                              (sha256-file fullpath)
                              (str-cat fullpath "_sha2")))))
                         ((not (is-file-p full-src-path))
                          (assert nil)
                         ))
                       )))
             files)
    (git-unbranch curbr)
    (popdir)
    ))

(vdefun expand-commit (repo commit)
  (if (equal commit 'head)
      (git-head-commit repo)
      commit))

(lol:defmacro! expand-commit! (r c)
  `(setf ,c (expand-commit ,r ,c)))

(vdefun list-files-at-commit (repo commit &key suf pref antipref)
  (expand-commit! repo commit)
  (let ((files (files-present-at-commit repo commit))
        (list '()))
    (assert files)
    (map 'nil #'(lambda (file)
                 (if (and (if (and suf) (is-str-suffix suf file) t)
                          (if (and pref) (is-str-prefix pref file) t)
                          (if (and antipref) (not (is-str-prefix antipref file)) t)
                          )
                     (pushr! list file)))
             files)
    (stable-sort list #'string<)
    )
  )

(vdefun ast-path (repo file commit &optional indexer)
  (expand-commit! repo commit)
  (let* ((cmp (if indexer (commit-cmp indexer)))
         (revid (git-file-latest-commit-until repo file commit cmp)))
    (str-cat blip-asts repo "/" file "/" revid)))

(vdefun ast-sha2-path (repo file commit &optional indexer)
  (expand-commit! repo commit)
  (let* ((cmp (if indexer (commit-cmp indexer)))
         (revid (git-file-latest-commit-until repo file commit cmp)))
    (str-cat blip-asts repo "/" file "/" revid "_sha2")))

(vdefun load-ast (repo file commit &optional indexer)
  (bin-to-form (ast-path repo file commit indexer)))

(vdefun load-ast-sha2 (repo file commit &optional indexer)
  (bin-to-form (ast-sha2-path repo file commit indexer)))

(vdefun stringify-ast-leaves (ast)
  "Turns every flat group of characters into a string"
  (xform-ast ast
             #'(lambda (p n r s)
                 (cond
                   ((and (is-char-ls n))
                    (values (char-ls-to-str n))
                    )
                   ((and t)
                    (values n)
                    )
                   )
                 )
             (list ast))
  )

(defun js-path-cat-aux (str ls files)
  (let* ((dstr (str-cat str (car ls) "/"))
         (fstr (str-cat str (car ls) ".js"))
         (lfstr (str-cat str (car ls)))
         (istr (str-cat str (car ls) "/index.js"))
         (exists (member lfstr files :test #'equal))
         (fexists (member fstr files :test #'equal))
         (iexists (member istr files :test #'equal)))
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

(defun js-file-deps (repo file commit files)
  (let* ((ast (load-ast repo file commit))
         (reqs (get-require-arg0 ast))
         (exp-reqs (map 'list #'(lambda (r) (expand-path file r :when-module nil)) reqs))
         (raw (map 'list #'(lambda (r) (list file r)) (remove-if #'not exp-reqs)))
         (pretty (map 'list
                      #'(lambda (e)
                          (list (car e) (js-path-cat (cadr e) files))) raw)))
    pretty))

(defun js-all-file-deps (repo commit files)
  (expand-commit! repo commit)
  (let* ((deps '()))
    (map 'nil #'(lambda (f) (pushr! deps (js-file-deps repo f commit files))) files)
    (reduce #'append (remove-if #'not deps)))
  )

(defun js-cache-file-deps (repo commit files)
  (expand-commit! repo commit)
  (let ((dir (str-cat blip-repo-meta repo "/deps"))
        (dep-table (js-all-file-deps repo commit files)))
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

(defun js-fdeps (repo commit files &optional force)
  (expand-commit! repo commit)
  (let ((deps (if (and force) nil (js-load-file-deps repo commit))))
    (cond
      ((not deps)
       (js-cache-file-deps repo commit files)
       (setf deps (js-load-file-deps repo commit))
       ))
    deps))


(defun js-what-requires (repo commit dep files &optional force)
  (let ((ret '())
        (table (js-fdeps repo commit files force)))
    (map 'nil #'(lambda (x) (if (equal dep (cadr x)) (pushr! ret (car x)))) table)
    ret)
  )

(defun js-requires-what (repo commit dep files &optional force)
  (let ((ret '())
        (table (js-fdeps repo commit files force)))
    (map 'nil #'(lambda (x) (if (equal dep (car x)) (pushr! ret (cadr x)))) table)
    ret)
  )

(defun get-column (list n)
  (remove-duplicates (map 'list #'(lambda (e) (car (popl-n e n))) list) :test #'equal))

(defun js-top-deps (repo commit files &optional force)
  (let* ((table (js-fdeps repo commit files force))
         (left (get-column table 0))
         (right (get-column table 1))
         (left-only '()))
    (map 'nil #'(lambda (e)
                  (if (not (member e right :test #'equal))
                      (pushr! left-only e)))
         left)
    left-only))

(defun js-bottom-deps (repo commit files &optional force)
  (let* ((table (js-fdeps repo commit files force))
         (left (get-column table 0))
         (right (get-column table 1))
         (right-only '()))
    (map 'nil #'(lambda (e)
                  (if (not (member e left :test #'equal))
                      (pushr! right-only e))) right)
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

(defun test-mooremachine-ast ()
  (load-ast "github/joyent/node-mooremachine"
            "lib/fsm.js"
            'head))

(defun test-docker-print-index ()
  (print-js-paths (test-docker-create-index)))

(defun test-mooremachine-print-index ()
  (print-js-paths (test-mooremachine-create-index)))

(defun test-mooremachine-get-subtree (path &optional pov)
  (map 'list #'ast-to-str (get-path-subtrees path (test-mooremachine-create-index) pov)))

(defun test-mike-print-index ()
  (print-js-paths (test-mike-create-index)))

(defun test-docker-print-index-raw ()
  (print-js-paths-raw (test-docker-create-index)))

(defun test-docker-get-subtree (path &optional pov)
  (map 'list #'ast-to-str (get-path-subtrees path (test-docker-create-index) pov)))

(defun test-mike-get-subtree (path &optional pov)
  (map 'list #'ast-to-str (get-path-subtrees path (test-mike-create-index) pov)))

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
  (map nil #'(lambda (cell) (puts "~a ~a " (car cell) (cdr cell))) alist))

(defun test-docker-word-count ()
  (format-alist (js-word-count
                 (load-ast "github/joyent/sdc-docker"
                           "lib/backends/sdc/containers.js" 'head))))

(defun test-docker-fcall-count ()
  (format-alist (js-fcall-count
                 (load-ast "github/joyent/sdc-docker"
                           "lib/backends/sdc/containers.js" 'head))))

(defun test-docker-fcall-params-count ()
  (format-alist (js-fcall-params-count
                 (load-ast "github/joyent/sdc-docker"
                           "lib/backends/sdc/containers.js" 'head))))

(defun test-docker-fdef-count ()
  (format-alist (js-fdef-count
                 (load-ast "github/joyent/sdc-docker"
                           "lib/backends/sdc/containers.js" 'head))))

(defun test-docker-fcommit-count ()
  (format-alist (file-commit-count (load-git-log "github/joyent/sdc-docker"))))

(defun test-docker-req-strs ()
  (let* ((repo "github/joyent/sdc-docker")
         (file "lib/backends/sdc/containers.js")
         (reqs (get-require-arg0 (load-ast repo file
                                           'head)))
         (exp-reqs (map 'list #'(lambda (r) (expand-path file r :when-module nil)) reqs)))

    (map 'list #'(lambda (r) (list file r)) (remove-if #'not exp-reqs))
  ))

(load "envs.lisp")
