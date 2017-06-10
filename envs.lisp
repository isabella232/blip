;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Copyright 2017 Joyent, Inc.
;;; Copyright 2017 Nicholas Zivkovic

(setf envs nil)

(setf env-avail nil)

(defmacro! all-files (repo commit suf pref)
  `(list-files-at-commit
                 ,repo ,commit :suf ,suf :pref ,pref))

(defun all-files-if (repo commit suf pref test)
  (remove nil
          (map 'list #'(lambda (f) (if (funcall test f) f nil))
               (all-files repo commit suf pref))))

(defun set-env-all-files ()
  (set-env-var 'files (all-files (get-env-var 'repo)
                                 (get-env-var 'commit)
                                 (get-env-var 'parser-suf)
                                 (get-env-var 'parser-pref))))

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
                       (get-env-var 'parser-pref))))
    (set-env-var 'files (map 'list #'(lambda (p) (apply #'str-cat p))
         (map 'list #'(lambda (p) (intersperse p "/" t))
              (remove-if #'(lambda (p) (> (length p) max-depth))
                         (map 'list #'(lambda (f) (str-split "/" f)) fs)
                         ))))
    )
  )

(defun get-env-npages ()
  (float (/ (length (get-env-var 'files)) (get-env-var 'pagesz))))

(defun test-fdepth ()
  (map 'list #'(lambda (p) (intersperse p "/" t)) (set-env-file-depth 2)))

(defclass env ()
  ((repo :initarg :repo)
   (commit :initarg :commit)
   (indexer :initarg :indexer)
   (idx-type :initarg :idx-type)
   (idx-type-conv :initarg :idx-type-conv)
   (files :initarg :files)
   (pagesz :initarg :pagesz)
   (ast-fmt :initarg :ast-fmt)
   (parser :initarg :parser)
   (parser-suf :initarg :parser-suf)
   (parser-pref :initarg :parser-pref)
   (ast-ls-call :initarg :ast-ls-call)
   (ast-ls-def :initarg :ast-ls-def)
   (ast-ls-fbind :initarg :ast-ls-fbind)
   (ast-ls-word :initarg :ast-ls-word))
   )

(defun ls-envs ()
  env-avail)

(defmacro! new-env%% (name repo commit indexer idx-type idx-type-conv
                           files pagesz ast-fmt parser parser-suf parser-pref
                           ast-ls-call ast-ls-def ast-ls-fbind ast-ls-word)
  `(let ((tmp (make-instance 'env :repo ,repo :commit ,commit :indexer ,indexer
                  :idx-type ,idx-type :idx-type-conv ,idx-type-conv :files ,files
                  :pagesz ,pagesz :ast-fmt ,ast-fmt :parser ,parser
                  :parser-suf ,parser-suf :parser-pref
                  ,parser-pref :ast-ls-call ,ast-ls-call :ast-ls-def ,ast-ls-def
                  :ast-ls-fbind ,ast-ls-fbind :ast-ls-word ,ast-ls-word)))
    (pushr! env-avail ',name)
    (setf ,name tmp)
    tmp
    )
  )

(defmacro! new-env (name &body body)
  `(let* (,@body)
    ;(assert (and repo commit indexer idx-type idx-type-conv files parser
                 ;parser-suf parser-pref ast-ls-call ast-ls-def))
     (new-env%% ,name repo commit indexer idx-type idx-type-conv files pagesz
                ast-fmt parser parser-suf parser-pref ast-ls-call ast-ls-def ast-ls-fbind
                ast-ls-word)
    )
  )

(defmacro! new-js-env (name repo-nm commit &optional pref)
  `(new-env ,name
     (repo ,repo-nm)
     (commit (expand-commit ,repo-nm ,commit))
     (indexer #'index-all-js-paths)
     (idx-type :funcs)
     (idx-type-conv #'js-idx-type-to-test)
     (files (all-files repo commit ".js" ,pref))
     (pagesz 70)
     (ast-fmt #'(lambda (a) (js-ast-fmt nil a :simtupid)))
     (parser #'js-to-ast)
     (parser-suf ".js")
     (parser-pref ,pref)
     (ast-ls-call #'js-list-fcalls)
     (ast-ls-def #'js-list-fdefs)
     (ast-ls-fbind #'js-list-fbinds)
     (ast-ls-word #'js-list-words)
     )
  )

(defmacro! new-c-env (name repo-nm commit &optional pref)
  `(new-env ,name
    (repo ,repo-nm)
    (commit (expand-commit ,repo-nm ,commit))
    (indexer #'index-all-c-paths)
    (idx-type :funcs)
    (idx-type-conv #'c-idx-type-to-test)
    (files (all-files repo commit ".c" ,pref))
    (pagesz 70)
    (ast-fmt #'(lambda (a) (c-ast-fmt nil a :simtupid)))
    (parser #'c-to-ast)
    (parser-suf ".c")
    (parser-pref ,pref)
    (ast-ls-call #'c-list-fcalls)
    (ast-ls-def #'c-list-fdefs)
    (ast-ls-fbind nil)
    (ast-ls-word #'c-list-words)
    )
  )

(new-js-env mmachine "github/joyent/node-mooremachine" :head)

(new-js-env vmapi "github/joyent/sdc-vmapi" :head)

(new-js-env vasync "github/davepacheco/node-vasync" :head)

(new-js-env kang "github/davepacheco/kang" :head)

(new-js-env marlin-dash "github/joyent/manta-marlin-dashboard" :head)

(new-js-env marlin "github/joyent/manta-marlin" :head)

(new-js-env muskie "github/joyent/manta-muskie" :head)

(new-js-env async "github/caolan/async" :head)

(new-js-env warden "github/joyent/node-restify-warden" :head)

(new-js-env bunyan "github/trentm/node-bunyan" :head)

(new-js-env sdc-docker "github/joyent/sdc-docker" :head)

(new-js-env napi "github/joyent/sdc-napi" :head)

(new-c-env mike-sdb "github/sdimitro/minions" :head "sdb/libsdb")

(new-c-env libuv "github/libuv/libuv" :head "src/")

(new-c-env bk "github/bitkeeper-scm/bitkeeper" :head)

(new-c-env tmux "github/tmux/tmux" :head)

(new-c-env pg "github/postgres/postgres" :head)

(defun pushenv (e)
  (pushr! envs e))

(defun popenv ()
  (popr! envs))

(defun get-env-var (var)
  (let ((e (car (last envs))))
    (slot-value e var)))

(defun set-env-var (var val)
  (let ((e (car (last envs))))
    (setf (slot-value e var) val)
    )
  )

(defmacro! in-index-env (force page &body body)
  `(let* ((repo (get-env-var 'repo))
          (files (get-env-var 'files))
          (cmt (get-env-var 'commit))
          (pgsz (get-env-var 'pagesz))
          (indexer (get-env-var 'indexer))
          (ast-fmt (get-env-var 'ast-fmt))
          (idx-type (get-env-var 'idx-type))
          (idx-type-conv (get-env-var 'idx-type-conv))
          (indices (map 'list
                        #'(lambda (f)
                            (pipeline
                             (load-ast repo f cmt)
                             #'(lambda (ast) (path-index-setroot
                                              (funcall indexer repo f cmt idx-type ast
                                                       :force force)))
                             #'(lambda (ix)
                                 (list f ix)))
                                )
                        (get-nth-page files ,page pgsz))))
      ,@body))

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

(defun index-print (&optional pov &key page force)
  (in-index-env force (if (and page) page 0)
    (map 'list #'(lambda (ix) (print-js-paths ix pov)) indices)))

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
         (parser (get-env-var 'parser))
         )
    (parse-x-files-at-commit repo cmt :force force :suf suf
                                      :pref pref :parser parser
                                      :whitelist (ast-ls-files))
    )
  )


(defun index-get-subtree-impl (index path &optional pov)
  ;(map 'list #'(lambda (e) (ast-to-str (cdr e))) (get-path-subtree path index pov)))
  (get-path-subtree path index pov))

(defun index-get-subtree-str (path &optional pov &key page force)
  (pre-filter-files path
    (in-index-env force (if (and page) page 0)
      (remove-if #'(lambda (x) (equal (cadr x) ""))
                 (map 'list #'(lambda (ix)
                                (list (car ix)
                                      (map 'list
                                           #'(lambda (ast)
                                               (ast-to-str
                                                (funcall ast-fmt ast)))
                                           (cadr ix))))
        (map 'list
         #'(lambda (ix)
             (index-get-subtree-impl ix path pov))
         indices))))))

(defmacro! pre-filter-files (pref &body body)
  `(let ((spref (str-split "/|{|}|\\(|\\)|=" ,pref))
         (f1 '())
         (f2 '())
         (f3 '())
         (f4 '())
         (rpref ,pref)
         (orig-files (ast-ls-files))
         (res nil))
         (setf rpref (car spref))
     (setf f1 (ast-extract-files (ast-ls-fcalls t :pref rpref)))
     (setf f2 (ast-extract-files (ast-ls-fdefs t :pref rpref)))
     (setf f3 (ast-extract-files (ast-ls-fbinds t :pref rpref)))
     (setf f4 (ast-extract-files (ast-ls-words t :pref rpref)))
     (set-env-files (remove-duplicates (append f1 f2 f3 f4) :test #'equal))
     (setf res ,@body)
     (set-env-files orig-files)
     res
     ))

(defun index-get-subtree (path &optional pov &key page force)
  (pre-filter-files path
    (in-index-env force (if (and page) page 0)
      (map 'list
           #'(lambda (ix)
               (index-get-subtree-impl ix path pov))
           indices))))

(defun index-prefix (pref &optional pov &key page force)
  (pre-filter-files pref
    (in-index-env force (if (and page) page 0)
      (remove nil
              (map 'list #'(lambda (i)
                             (get-path-with-prefix pref i pov))
                   indices)
              :key #'cadr)
      ))
    )

;;; TODO update this function to use pre-filtered files like index-prefix does.
(defun index-suffix (suf &optional pov &key page force)
  (in-index-env force (if (and page) page 0)
    (remove nil
            (map 'list #'(lambda (i)
                           (get-path-with-suffix suf i pov))
                 indices)
            :key #'cadr)
    ))

(defun index-word-count (word &optional pov &key zero pre page force)
  (pre-filter-files word
    (in-index-env force (if (and page) page 0)
      (assert idx-type)
      (map 'list #'(lambda (i)
                     (get-path-node-count
                      #'(lambda (n)
                          (and (is-word-group n)
                               (match-str-list word n)))
                      i (funcall idx-type-conv idx-type)
                      pov :zero zero :pre pre))
           indices))))

(defun index-line-count (&optional pov &key zero pre page force)
  (in-index-env force (if (and page) page 0)
    (map 'list #'(lambda (i)
                   (get-path-node-count
                    #'(lambda (n)
                        (and (is-white-space-group n)
                             (member #\Newline n)))
                    i (funcall idx-type-conv idx-type)
                    pov :zero zero :pre pre))
         indices)))

(defun index-uniq (&optional pov &key page force)
  (in-index-env force (if (and page) page 0)
   (map 'list #'(lambda (i)
                  (uniq-path i
                             :pov pov
                             :fmt t
                             :sort-path t
                             :sort-count t))
        indices)))

(defun index-path-depth (&optional pov &key page force)
  (in-index-env force (if (and page) page 0)
   (map 'list #'(lambda (i)
                  (paths-by-depth  i
                             :pov pov
                             :fmt t))
        indices)))

(defun default-bk-files ()
  (set-env-file-depth 2))
(pushenv napi)
(pushenv muskie)
(pushenv sdc-docker)
(set-env-file "lib/backends/sdc/containers.js")
(pushenv bk)
(default-bk-files)
(pushenv pg)
;(pushenv tmux)
;(set-env-file "format.c")
;postmaster.c: demerit!
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
