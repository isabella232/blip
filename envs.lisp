;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Copyright 2017 Joyent, Inc.
;;; Copyright 2017 Nicholas Zivkovic

(defvar env-stack nil)
(defvar env-pool nil)

;(setf env-avail (file-to-form blip-env-avail))
(defvar env-avail nil)


(defun all-files (repo commit suf pref antipref &optional depth)
  (let* ((fs (list-files-at-commit
                 repo commit :suf suf :pref pref :antipref antipref)))
    (cond
      ((and depth)
       (map 'list #'(lambda (p) (apply #'str-cat p))
            (map 'list #'(lambda (p) (intersperse p "/" t))
                 (remove-if #'(lambda (p) (> (length p) depth))
                            (map 'list #'(lambda (f) (str-split "/" f)) fs)))))
      ((not depth)
       fs))
    ))

(defun all-files-if (repo commit suf pref antipref test)
  (remove nil
          (map 'list #'(lambda (f) (if (funcall test f) f nil))
               (all-files repo commit suf pref antipref))))

(defun set-env-all-files ()
  (set-env-var 'files (all-files (get-env-var 'repo)
                                 (get-env-var 'commit)
                                 (get-env-var 'parser-suf)
                                 (get-env-var 'parser-pref)
                                 (gen-env-var 'parser-antipref))))

(defun set-env-file (&rest fs)
    (set-env-var 'files fs)
  )

(defun set-env-files (fs)
  (set-env-var 'files fs)
  )

(defun set-env-file-depth (max-depth)
  (let ((fs (all-files (get-env-var 'repo)
                       (get-env-var 'commit)
                       (get-env-var 'parser-suf)
                       (get-env-var 'parser-pref)
                       (get-env-var 'parser-antipref))))
    (set-env-var 'files (map 'list #'(lambda (p) (apply #'str-cat p))
         (map 'list #'(lambda (p) (intersperse p "/" t))
              (remove-if #'(lambda (p) (> (length p) max-depth))
                         (map 'list #'(lambda (f) (str-split "/" f)) fs)
                         ))))
    )
  )

(defun get-env-npages ()
  (ceiling (float (/ (length (get-env-var 'files)) (get-env-var 'pagesz)))))

(defun test-fdepth ()
  (map 'list #'(lambda (p) (intersperse p "/" t)) (set-env-file-depth 2)))

(defclass env ()
  ((repo :initarg :repo)
   (commit :initarg :commit)
   (name :initarg :name)
   (indexer :initarg :indexer)
   (idx-type :initarg :idx-type)
   (idx-type-conv :initarg :idx-type-conv)
   (files :initarg :files)
   (pagesz :initarg :pagesz)
   (ast-fmt :initarg :ast-fmt)
   (parser :initarg :parser)
   (parser-suf :initarg :parser-suf)
   (parser-pref :initarg :parser-pref)
   (parser-antipref :initarg :parser-antipref)
   (ast-ls-call :initarg :ast-ls-call)
   (ast-ls-def :initarg :ast-ls-def)
   (ast-ls-fbind :initarg :ast-ls-fbind)
   (ast-ls-word :initarg :ast-ls-word)
   (what-requires :initarg :what-requires)
   (requires-what :initarg :requires-what)
   (bottom-deps :initarg :bottom-deps)
   (top-deps :initarg :top-deps)
   (what-exports :initarg :what-exports)
   (exports-what :initarg :exports-what))
   )

(defun env-ls ()
  env-avail)

(defun env-stack ()
  (map 'list #'(lambda (e) (slot-value e 'name)) env-stack))

(defmacro! new-env%% (name repo commit indexer idx-type idx-type-conv
                           files pagesz ast-fmt parser parser-suf parser-pref
                           parser-antipref
                           ast-ls-call ast-ls-def ast-ls-fbind ast-ls-word
                           what-requires requires-what bottom-deps top-deps
                           what-exports exports-what)
  `(let ((tmp (make-instance 'env :repo ,repo :name ',name :commit ,commit
                  :indexer ,indexer :idx-type ,idx-type :idx-type-conv
                  ,idx-type-conv :files ,files :pagesz ,pagesz :ast-fmt ,ast-fmt
                  :parser ,parser :parser-suf ,parser-suf :parser-pref
                  ,parser-pref :parser-antipref ,parser-antipref :ast-ls-call
                  ,ast-ls-call :ast-ls-def ,ast-ls-def
                  :ast-ls-fbind ,ast-ls-fbind :ast-ls-word ,ast-ls-word
                  :what-requires ,what-requires :requires-what ,requires-what
                  :bottom-deps ,bottom-deps :top-deps ,top-deps
                  :what-exports ,what-exports :exports-what ,exports-what)))
    (pushr! env-avail ',name)
    (pushr! env-pool tmp)
    ;(setf ,name tmp)
     ;;; TODO: save this env to a file
    tmp
    )
  )

(defmacro! new-env (name &body body)
  `(let* (,@body)
    ;(assert (and repo commit indexer idx-type idx-type-conv files parser
                 ;parser-suf parser-pref ast-ls-call ast-ls-def))
     (new-env%% ,name repo commit indexer idx-type idx-type-conv files pagesz
                ast-fmt parser parser-suf parser-pref parser-antipref ast-ls-call
                ast-ls-def ast-ls-fbind ast-ls-word what-requires requires-what
                bottom-deps top-deps what-exports exports-what)
    )
  )

(defmacro! new-env-req (r v)
  `(cond
     ((and ,r ,v)
      (load (str-cat blip-env "/lib/" ,r ".lisp"))
      )
     ((and ,r)
      (quiet-load (str-cat blip-env "/lib/" ,r ".lisp"))
      )
     ))

(defmacro! new-js-env (name repo-nm commit &key pref antipref depth verbose
                            require extend-idx)
  `(progn
     (new-env-req ,require ,verbose)
     (new-env ,name
       (repo ,repo-nm)
       (commit (expand-commit ,repo-nm ,commit))
       (ext-idx (if (and ,extend-idx) ,extend-idx (lambda (ix) ix)))
       (indexer (funcall ext-idx (make-instance 'js-indexer)))
       (idx-type :funcs)
       (idx-type-conv #'js-idx-type-to-test)
       (files (all-files repo commit ".js" ,pref ,antipref ,depth))
       (pagesz 70)
       (ast-fmt #'(lambda (a) (js-ast-fmt nil a :simtupid)))
       (parser #'js-to-ast)
       (parser-suf ".js")
       (parser-pref ,pref)
       (parser-antipref ,antipref)
       (ast-ls-call #'js-list-fcalls)
       (ast-ls-def #'js-list-fdefs)
       (ast-ls-fbind #'js-list-fbinds)
       (ast-ls-word #'js-list-words)
       (requires-what #'js-requires-what)
       (what-requires #'js-what-requires)
       (bottom-deps #'js-bottom-deps)
       (top-deps #'js-top-deps)
       (exports-what nil)
       (what-exports nil)
       )
     ))

(defmacro! new-cl-env (name repo-nm commit &key pref antipref depth verbose
                            require extend-idx)
  `(progn
     (new-env-req ,require ,verbose)
     (new-env ,name
       (repo ,repo-nm)
       (commit (expand-commit ,repo-nm ,commit))
       (ext-idx (if (and ,extend-idx) ,extend-idx (lambda (ix) ix)))
       (indexer (funcall ext-idx (make-instance 'cl-indexer)))
       (idx-type :funcs)
       (idx-type-conv #'cl-idx-type-to-test)
       (files (all-files repo commit ".lisp" ,pref ,antipref ,depth))
       (pagesz 70)
       (ast-fmt #'(lambda (a) (cl-ast-fmt nil a :simtupid)))
       (parser #'cl-to-ast)
       (parser-suf ".lisp")
       (parser-pref ,pref)
       (parser-antipref ,antipref)
       (ast-ls-call #'js-list-fcalls)
       (ast-ls-def #'js-list-fdefs)
       (ast-ls-fbind #'js-list-fbinds)
       (ast-ls-word #'js-list-words)
       (requires-what #'js-requires-what)
       (what-requires #'js-what-requires)
       (bottom-deps #'js-bottom-deps)
       (top-deps #'js-top-deps)
       (exports-what nil)
       (what-exports nil)
       )
  ))

(defmacro! new-c-env (name repo-nm commit &key pref antipref depth verbose
                           require extend-idx)
  `(progn
     (new-env-req ,require ,verbose)
     (new-env ,name
       (repo ,repo-nm)
       (commit (expand-commit ,repo-nm ,commit))
       (ext-idx (if (and ,extend-idx) ,extend-idx (lambda (ix) ix)))
       (indexer (funcall ext-idx (make-instance 'c-indexer)))
       (idx-type :funcs)
       (idx-type-conv #'c-idx-type-to-test)
       (files (all-files repo commit ".c" ,pref ,antipref ,depth))
       (pagesz 70)
       (ast-fmt #'(lambda (a) (c-ast-fmt nil a :simtupid)))
       (parser #'c-to-ast)
       (parser-suf ".c")
       (parser-pref ,pref)
       (parser-antipref ,antipref)
       (ast-ls-call #'c-list-fcalls)
       (ast-ls-def #'c-list-fdefs)
       (ast-ls-fbind nil)
       (ast-ls-word #'c-list-words)
       (requires-what nil)
       (what-requires nil)
       (bottom-deps nil)
       (top-deps nil)
       (exports-what nil)
       (what-exports nil)
       )
     ))

;(form-to-file (env-ls) blip-env-avail)

(defun pushenv (e)
  (pushr! env-stack e)
  )

(defun popenv ()
  (popr! env-stack)
  )

(defun get-env-var (var)
  (let ((e (car (last env-stack))))
    (slot-value e var)))

(defun get-env ()
  (car (last env-stack))
  )

(defun set-env-var (var val)
  (let ((e (car (last env-stack))))
    (setf (slot-value e var) val)
    )
  )

(defmacro! do-indices (args force alt-idx-type &body body)
  (assert (= (length args) 2))
  (let ((f (car args))
        (ix (cadr args))
        )
    (assert (equal (type-of f) 'symbol))
    (assert (equal (type-of ix) 'symbol))
    `(let* ((repo (get-env-var 'repo))
            (files (get-env-var 'files))
            (cmt (get-env-var 'commit))
            (pgsz (get-env-var 'pagesz))
            (ast-fmt (get-env-var 'ast-fmt))
            (idx-type (if (and ,alt-idx-type) ,alt-idx-type (get-env-var 'idx-type)))
            (indexer (get-env-var 'indexer))
            (idx-type-conv (get-env-var 'idx-type-conv))
            )
       (do-group (file files)
         (let ((,f file)
               (,ix (index-paths indexer repo file cmt nil :force ,force)))
           ,@body
           )
         )
       )))

(defmacro! do-paths (args force alt-idx-type &body body)
  (assert (and (>= (length args) 3) (<= (length args) 4)))
  (let* ((f (car args))
         (ix (cadr args))
         (ld-ast (if (= (length args) 4) t nil))
         (ast (if (and ld-ast) (cadddr args) (gensym)))
         (path (caddr args))
         (paths (gensym))
        )
    (assert (equal (type-of path) 'symbol))
    `(do-indices (,f ,ix) force alt-idx-type
         (let ((,paths (path-index-str (list ,f ,ix)))
               (,ast nil)
               )
           (if (and ,ld-ast)
               (setf ,ast (load-ast repo ,f cmt))
               )
           (do-group (,path ,paths)
             (setf ,path (fmt-path (car ,path)))
             ,@body
             )
           )
        )
    )
  )

(defmacro! in-ls-env (&body body)
  `(let* ((repo (get-env-var 'repo))
          (files (get-env-var 'files))
          (cmt (get-env-var 'commit))
          (ast-ls-call (get-env-var 'ast-ls-call))
          (ast-ls-def (get-env-var 'ast-ls-def))
          (ast-ls-fbind (get-env-var 'ast-ls-fbind))
          (ast-ls-word (get-env-var 'ast-ls-word))
          )
     ,@body))

(defmacro! in-req-env (&body body)
  `(let* ((repo (get-env-var 'repo))
          (files (get-env-var 'files))
          (cmt (get-env-var 'commit))
          (requires-what (get-env-var 'requires-what))
          (what-requires (get-env-var 'what-requires))
          (top-deps (get-env-var 'top-deps))
          (bottom-deps (get-env-var 'bottom-deps))
          (what-exports (get-env-var 'what-exports))
          (exports-what (get-env-var 'exports-what))
          )
     ,@body))

(defun print-header (s)
  (format t "~d:~%~%" s)
  )

(defun print-paths (ls)
  (do-group (s ls)
    (format t "~d~%" s)
    )
  (format t "~%~%")
  )

(defun print-header-and-paths (pair)
  (print-header (car pair))
  (print-paths (cadr pair))
  )

(defun print-pairs (pairs)
  (do-group (p pairs)
    (format t "~d    ~d~%" (car p) (cadr p))
    )
  )

(defun index-build-impl (&optional pov &key page force alt-idx-type)
  (do-indices (f ix) force alt-idx-type
    (print-js-paths (list f ix) pov)
    t))

(defun index-print (&optional pov &key page force alt-idx-type)
  (do-indices (f ix) force alt-idx-type
    (print-header-and-paths (print-js-paths (list f ix) pov))))

(defun index-print-sort (&optional pov &key page force alt-idx-type)
  (do-indices (f ix) force alt-idx-type
    (print-header-and-paths (print-js-paths (list f ix) pov :sort t))))

(defun index-build (&key force alt-idx-type)
  (index-build-impl :down :force force :alt-idx-type alt-idx-type))

(defmacro! ast-ls (name lister)
  `(defun ,name (&optional count &key pref force)
    (in-ls-env
      (let ((full-list nil)
            (filt-list nil))
        (setf full-list (map 'list
                             #'(lambda (f)
                                 (list f
                                       (if ,lister
                                           (funcall ,lister repo f cmt count
                                                    :force force)
                                           nil))) files))

        (if (and pref)
            (setf filt-list
                  (map 'list
                       #'(lambda (pair)
                           (cond
                             ((and count)
                              (if (not (member pref (cadr pair) :test #'is-str-prefix
                                                                :key #'car))
                                  nil
                                  pair))
                             ((not count)
                              (if (not (member pref (cadr pair) :test #'is-str-prefix))
                                  nil
                                  pair))))
                       full-list)))
        (if (not pref)
            (remove-if #'not full-list :key #'cadr)
            (remove nil filt-list))
        )))
  )

(ast-ls ast-ls-fdefs ast-ls-def)
(ast-ls ast-ls-fbinds ast-ls-fbind)
(ast-ls ast-ls-words ast-ls-word)
(ast-ls ast-ls-fcalls ast-ls-call)

(defun ast-ls-files ()
  (in-ls-env
    files))

(defun env-file-to-path (f)
  (in-ls-env
    (str-cat blip-repos (get-env-var 'repo) "/" f)))

(defun ast-extract-files (ast-ls-out)
  (map 'list #'car ast-ls-out))

(defun ast-extract-entities (ast-ls-out)
  (map 'list #'cadr ast-ls-out))

(defun defs-uncalled ()
  (let ((defs (remove-duplicates (append
                                  (append-ls (ast-extract-entities (ast-ls-fbinds)))
                                  (append-ls (ast-extract-entities (ast-ls-fdefs))))
                                 :test #'equal))
        (calls (remove-duplicates (append-ls (ast-extract-entities (ast-ls-fcalls)))
                                  :test #'equal)))
    (set-difference defs calls :test #'equal)
    ))

(defun calls-undefed ()
  (let ((defs (remove-duplicates (append
                                  (append-ls (ast-extract-entities (ast-ls-fbinds)))
                                  (append-ls (ast-extract-entities (ast-ls-fdefs))))
                                 :test #'equal))
        (calls (remove-duplicates (append-ls (ast-extract-entities (ast-ls-fcalls)))
                                  :test #'equal)))
    (set-difference calls defs :test #'equal)
    ))



(defun ast-parse (&optional force)
  (let* ((repo (get-env-var 'repo))
         (cmt (get-env-var 'commit))
         (suf (get-env-var 'parser-suf))
         (pref (get-env-var 'parser-pref))
         (antipref (get-env-var 'parser-antipref))
         (parser (get-env-var 'parser))
         )
    (parse-x-files-at-commit repo cmt :force force :suf suf
                                      :pref pref :parser parser
                                      :whitelist (ast-ls-files))
    )
  )

(defun reconstruct-repo ()
  (ast-parse t)
  (index-build :force t)
  (ast-ls-fcalls t :force t)
  (ast-ls-fdefs t :force t)
  (ast-ls-words t :force t)
  (ast-ls-fbinds t :force t)
  t
  )


(defun index-get-subtrees-impl (index path ast &optional pov)
  ;(map 'list #'(lambda (e) (ast-to-str (cdr e))) (get-path-subtrees path index pov)))
  (get-path-subtrees path index ast pov))

(defun index-get-path-walk-impl (index path ast &optional pov)
  (get-path-walk path index ast pov))

(defun index-get-subtrees-str (path &optional pov &key page force alt-idx-type)
  (pre-filter-files path
    (let ((pf nil)
          (ast-str nil)
          (trees nil))
      (do-indices (f ix) force alt-idx-type
        (setf trees (cadr (index-get-subtrees-impl
                           (list f ix) path
                           (load-ast repo f cmt)
                           pov)))
        (do-group (tree trees)
          (setf ast-str (ast-to-str (funcall ast-fmt tree)))
          (if (and (not (equal f pf)) (not (equal ast-str "")))
              (format t "~d:~%~%" f))
          (setf pf f)
          (if (not (equal ast-str ""))
              (format t "~d~%~%" ast-str)))
        )
      )
    )
  )

(defun split-ix-path (path)
  (str-split "/|{|}|\\(|\\)|=|:" path))

(defun get-mod-exp-key (ls)
  (assert (listp ls))
  (let ((key nil))
    (cond
      ((and (string= "module" (car ls)) (string= "exports" (cadr ls)))
       (setf key (caddr ls)))
      ((and (string= "exports" (car ls)))
       (setf key (cadr ls)))
      )
    )
  )

(defun remove-empty-strs (ls)
  (remove-if #'(lambda (s) (string= s "")) ls))

(defun ix-get-mod-exp-key (ix)
  (map 'list #'(lambda (p) (list (car p)
                                 (remove nil
                                         (map 'list #'get-mod-exp-key
                                              (map 'list #'remove-empty-strs
                                                      (map 'list #'split-ix-path
                                                           (cadr p))))))) ix)
  )

(defmacro! pre-filter-files (pref &body body)
  `(let ((spref (if (listp ,pref)
                    (map 'list #'split-ix-path ,pref)
                    (split-ix-path ,pref)))
         (f1 '())
         (f2 '())
         (f3 '())
         (f4 '())
         (rpref ,pref)
         (orig-files (ast-ls-files))
         (res nil))

     (setf rpref (if (listp ,pref)
                     (map 'list #'car spref)
                     (car spref)))
     ;;; XXX Given that f4 is a superset of f1,2,3 we might want to get rid of
     ;;; those.
     ;(setf f1 (ast-extract-files (ast-ls-fcalls t :pref rpref)))
     ;(setf f2 (ast-extract-files (ast-ls-fdefs t :pref rpref)))
     ;(setf f3 (ast-extract-files (ast-ls-fbinds t :pref rpref)))
     (cond
       ((or (listp ,pref) (and (stringp ,pref) (not (string= ,pref "/"))))
        (setf f4 (ast-extract-files (ast-ls-words t :pref rpref)))
        (set-env-files (remove-duplicates (append f1 f2 f3 f4) :test #'equal))
        (setf res ,@body)
        (set-env-files orig-files)
        res
        )
       ((and t)
        (setf res ,@body)
        res
        )
       )
     ))

(defun index-get-subtrees (path &optional pov &key page force alt-idx-type)
  (pre-filter-files path
    (do-indices (f ix) force alt-idx-type
      (print-ln (index-get-subtrees-impl (list f ix) path (load-ast repo f cmt) pov))
      )))

(defun index-get-subtree-walks (path &optional pov &key page force alt-idx-type)
  (pre-filter-files path
    (do-indices (f ix) force alt-idx-type
      (print-ln (index-get-path-walk-impl (list f ix) path
                                          (load-ast repo f cmt) pov)))
    )
  )

(defun index-prefix (pref &optional pov &key page force alt-idx-type)
  (let ((res nil)
        (pf nil))
    (pre-filter-files pref
      (do-indices (f ix) force alt-idx-type
        (setf res (get-path-with-prefix pref (list f ix) pov))
        (if (and (cadr res))
            (print-header-and-paths res)
            )
        )
      )
    )
  )

;;; TODO update this function to use pre-filtered files like index-prefix does.
(defun index-suffix (suf &optional pov &key page force alt-idx-type)
  (let ((res nil))
    (do-indices (f ix) force alt-idx-type
      (setf res (get-path-with-suffix suf (list f ix) pov))
      (if (and (cadr res))
          (print-header-and-paths res)
          )
      )
    )
  )

(defun index-word-count-old (word &optional pov &key zero pre page force alt-idx-type)
  (pre-filter-files word
    (do-indices (f ix) force alt-idx-type
      (assert idx-type)
      ;;; TODO return path, count. Print using format
      (do-group (p1 (get-path-node-count
                     #'(lambda (n)
                         (and (is-word-group n)
                              (match-str-list word n)))
                     (list f ix)
                     (load-ast repo f cmt)
                     (funcall idx-type-conv idx-type)
                     pov :zero zero :pre pre))
        (print-pairs (cadr p1)))
      )))


(defun index-word-count (word &optional pov &key zero pre page force alt-idx-type)
  (pre-filter-files word
    (do-paths (f ix path ast) force alt-idx-type
      (let ((trees (cadr (index-get-subtrees-impl (list f ix) path ast pov)))
            (count 0)
            )
        (do-group (tree trees)
          (walk-tree tree #'(lambda (n)
                              (and (listp n) (is-word-group n)
                                   (match-str-list word n)))
                     #'(lambda (n s w)
                         (incf count)
                         ) '() '())
          (cond
            ((> count 0)
             (format t "~d   ~d ~A~%" f path count)
             )
            )
          (setf count 0))
        )
      )
    )
  )

(defun index-line-count (&optional pov &key zero pre page force alt-idx-type)
  (do-indices (f ix) force alt-idx-type
    (get-path-node-count
     #'(lambda (n)
         (and (is-white-space-group n)
              (member #\Newline n)))
     (list f ix) (load-ast repo f cmt) (funcall idx-type-conv idx-type)
     pov :zero zero :pre pre)))

(defun index-uniq (&optional pov &key page force alt-idx-type)
  (do-indices (f ix) force alt-idx-type
    (uniq-path (list f ix)
               :pov pov
               :fmt t
               :sort-path t
               :sort-count t)))

(defun index-path-depth (&optional pov &key page force alt-idx-type)
  (do-indices (f ix) force alt-idx-type
    (paths-by-depth (list f ix)
                    :pov pov
                    :fmt t)))

(defun ast-file-deps (&key force)
  )

(defun ast-what-requires (dep &key force)
  (in-req-env (if (and what-requires)
                  (funcall what-requires repo cmt dep files force)
                  "what-requires not supported in this env"))
  )

(defun ast-requires-what (dep &key force)
  (in-req-env (if (and requires-what)
                  (funcall requires-what repo cmt dep files force)
                  "requires-what not supported in this env"))
  )

(defun ast-bottom-deps (&key force)
  (in-req-env (if (and bottom-deps)
                  (funcall bottom-deps repo cmt files force)
                  "bottom-deps not supported in this env"))
  )

(defun ast-top-deps (&key force)
  (in-req-env (if (and top-deps)
                  (funcall top-deps repo cmt files force)
                  "top-deps not supported in this env"))
  )

(defmacro! in-exp-env (&body body)
  `(let* ((pref '("module/exports/" "module/exports=" "exports/" "exports=/"))
          (ix (index-prefix pref :down :alt-idx-type :binds))
          (keys (ix-get-mod-exp-key ix))
          )
    ,@body
    )
  )

(defun ast-what-exports (func)
  (in-exp-env
    (map 'list #'car
         (remove-if #'(lambda (p)
                        (not (member func (cadr p) :test #'equal)))
                    keys))
    )
  )

(defun ast-exports-what (file)
  (in-exp-env
    (car (map 'list #'cadr
         (remove-if #'(lambda (p)
                        (not (string= file (car p))))
                    keys))
    ))
  )

(defun ifdef-metrics ()
  (sort
   (remove-if #'not
          (map 'list
               #'(lambda (p)
                   (list (car p) (find-if
                    #'(lambda (pp)
                        (string= "#ifdef" (car pp))
                        )
                    (cadr p)
                    )
                   ))
               (ast-ls-words t :pref "#ifdef"))
          :key #'cadr)
        #'< :key #'cadadr)
  )

(defun ifndef-metrics ()
  (sort
   (remove-if #'not
              (map 'list
                   #'(lambda (p)
                       (list (car p) (find-if
                                      #'(lambda (pp)
                                          (string= "#ifndef" (car pp))
                                          )
                                      (cadr p)
                                      )
                             ))
                   (ast-ls-words t :pref "#ifndef"))
              :key #'cadr)
   #'< :key #'cadadr)
  )


(defun load-env (env)
  "Reads in the cfg file, walks each record until it finds the appropriate
   env-name and evals it. Saves us a lot of (git-related) IO."
  (let ((ls (file-to-forms blip-env-cfg))
        (result nil))
    (map 'nil #'(lambda (e)
                  (if (equal (cadr e) env)
                      (progn (setf result (eval e)))))
         ls)
    result
    ))

(defun have-env (env)
  "Confirms that an env is present in the cfg file. Does not eval it."
  (let ((ls (file-to-forms blip-env-cfg))
        (have nil))
    (map 'nil #'(lambda (e)
                  (if (equal (cadr e) env)
                      (setf have t)))
         ls)
    have
    ))

(defun find-env (name)
  (find-if #'(lambda (e) (equal (slot-value e 'name) name)) env-avail))
