(defpackage :omg
  (:use cl clack websocket-driver bordeaux-threads trivial-utf-8)
  (:import-from :event-emitter #:emit)
  (:export add-to-boot       ;; add a code to boot sequence
           set-boot          ;; set boot code
           add-to-root-html  ;; add a text to html body
           set-root-html     ;; set html body
           rm-from-boot      ;; remove a code from boot sequence
           start-server      ;; start a http(s)-server
           kill-server       ;; kill a http(s)-server
           restart-server    ;; restart a http(s)-server
           defclass-f        ;; define a browser-side class
           defun-f           ;; define a browser-side function
           defmacro-f        ;; define a browser-side macro
           defmethod-f       ;; define a browser-side method
           defgeneric-f      ;; define a browser-side generic
           defvar-f          ;; define a browser-side variable
           defparameter-f    ;; define a browser-side parameter
           defconstant-f     ;; define a browser-side constant
           defun-r           ;; define a RPC-function
           def-session-var   ;; define server-side session var
           remote-exec       ;; execute a code in browser(s)
           with-session      ;; execute e code block on the specific browser
           find-session      ;; find session object by ID
           current-session-id  ;; get ID of current session (returns NIL if no session)
           set-debug-session  ;; mark current session as "debug"
           in-debug-session ;; execute code in debug session
           make-pwa       ;; serve web page as a Progressive Web App
           thread-in-session)) ;; spawn thread in current session

(in-package :omg)

(defconstant |sid-length| 10) ;; the length for all random IDs generated

(defvar *local-compile* t) ;; if T -- compile all lisp code to JS on the server side
                           ;; can be altered without page reloading or server restart

;; Here are the paths for all HTTP(s) queries (the page reload required after change):

(defvar *root-path* "/") ;; must be started with "/"
(defvar *html-path* "") ;; the path (relative to *root-path*) for simple html page with injcetced js
(defvar *js-path* "j")  ;; the path of js for injection (relative to *root-path*)
(defvar *ws-path* "s")  ;; websocket path (relative to *root-path*)
(defvar *rpc-path* "r") ;; rpc call path (relative to *root-path*)
(defvar *gimme-path* "g") ;; the path to query undefined symbols and functions (relative to *root-path*)
(defvar *takit-path* "t") ;; the auxilary path, nedded to return macro expansion results if *local-compile* is set
(defvar *port* 7500) ;; default server port

(defvar *use-wss* nil) ;; if T -- use wss:// protocol for websocket
(defvar *ssl-key* nil) ;; SSL key path
(defvar *ssl-cert* nil) ;; SSL cert path

(defvar *giant-hash-lock* nil) ;; A giant lock to make hashes thread safe

(defvar *omg-thread-list* nil)

(defun gethash-lock (key hash)
  (if *giant-hash-lock*
      (bt:with-lock-held (*giant-hash-lock*)
        (gethash key hash))
      (gethash key hash)))

(defun (setf gethash-lock) (val key hash)
  (if *giant-hash-lock*
      (bt:with-lock-held (*giant-hash-lock*)
        (setf (gethash key hash) val))
      (setf (gethash key hash) val)))

;; If we have both SSL key and SSL cert, start server with SSL support
(defun has-ssl-p () (and *ssl-key* *ssl-cert*))

;; If our server serves SSL, use wss:// or you may enforce wss:// protocol with *use-wss*
;;    if you are behind reverse proxy with SSL
(defun use-wss-p () (or *use-wss* (has-ssl-p)))


(defun random-string (len)
  "Generate a random stgring of length len"
  (let ((chrs "ABCDEFGHIJKLMOPQRSTUVWXYZ"))
    (map 'string
          (lambda (x)
             (declare (ignore x))
             (char chrs (random (length chrs))))
          (make-sequence 'string len))))

(defun random-symbol (len)
  "Make a random symbol"
  (intern (random-string len) :omg))

(defun random-key (h &optional (len 10))
  "Make a random hash-table key"
  (let ((k (random-symbol len)))
    (if (nth-value 1 (gethash-lock k h))
        (random-key h len)
        k)))

(defvar *exportable-expressions* (make-hash-table)) ;; All exportable functions are here
(defvar *rpc-functions* (make-hash-table))          ;; The allowed RPC functions registry
(defvar *gimme-wait-list* (make-hash-table))        ;; The temporary storage for gimme-threads,
                                                    ;;   waiting compilation results
                                                    ;; FIXME: periodic cleanup procedure needed!
(defvar *takit-wait-list* (make-hash-table))        ;; The temporary storage for compilation threads,
                                                    ;;   waiting for macro expansion results from browser side
                                                    ;; FIXME: periodic cleanup procedure needed!

(defvar *in-f-macro* nil)   ;; If T -- do not convert -f function calls to (remote-exec ...) (don't change manually!!!)

(defvar *exported-function-names* nil) ;; list of browser-side functions
(defvar *exported-classes-methods* (make-hash-table)) ;; methods for browser-side classess, which are not in omg packages
(defvar *accessor-classes* (make-hash-table))
(defvar *local-lambdas* (make-hash-table)) ;; list of unnamed functions, passed as arguments to browser-side ones
                                           ;; used by exec-local-lambda RPC-function to determine what lambda to execute

(defun is-system-pkg (pkg)
  (or (equal pkg (find-package :common-lisp))
      (equal pkg (find-package :common-lisp-user))
      (equal pkg (find-package :keyword))
      (equal pkg (find-package :cl))
      (equal pkg (find-package :cl-user))
      (equal pkg (find-package :jscl))
      (equal pkg (find-package :jscl/loop))))

(defun register-rpc (name)
  (remhash name *exportable-expressions*)
  (setf (gethash-lock name *rpc-functions*) t)
  (setf (gethash-lock name *exportable-expressions*)
       `(defun ,name (&rest argl)
          (funcall (jscl::oget (jscl::%js-vref "self") "OMG" "RPC")
                   (write-to-string (list ,(package-name *package*)
                                          ',name
                                          argl
                                          *session-id*))))))

(defmacro defun-r (name args &rest body)
  "Define a server-side function and allow to call it from browser side"
  `(progn
     (register-rpc ',name)
     (defun ,name ,args ,@body)))

(defun-r exec-local-lambda (idargs)
  (let ((h (gethash-lock (car idargs) *local-lambdas*)))
    (if h
        (apply h (cdr idargs))
        nil))) ;; return nil if function not found. FIXME: must return an error object!

(defun f-eval (arg)
  "Smart-eval -- evaluate everything, but expotrable expressions, lambdas and function names"
  (cond ((and (symbolp arg) (or (boundp arg) (fboundp arg)) (gethash-lock arg *exportable-expressions*))
         arg)
        ((and (consp arg)
              (equal (car arg) 'function) ;; in-browser function name
              (gethash-lock (cadr arg) *exportable-expressions*))
         arg)
        ((and (listp arg) (fboundp (car arg)))
         (cond ((equal (car arg) 'lambda) ;; lambdas are always evaluates in browser
                arg)
               ((gethash-lock (car arg) *exportable-expressions*) ;; browser-side function
                (cons (car arg) (mapcar #'f-eval (cdr arg))))
               ((equal (car arg) 'CONS)
                `(quote (,(f-eval (cadr arg)) . (f-eval (caddr arg)))))
               ((equal (car arg) 'LIST)
                `(list ,@(mapcar #'f-eval (cdr arg))))
               ((equal (car arg) 'QUOTE)
                arg)
               (t (let ((res (eval (cons (car arg) (mapcar #'f-eval (cdr arg))))))
                    (if (listp res)
                        (list 'quote (mapcar #'f-eval res))
                        (f-eval res))))))
        ((and (symbolp arg) (boundp arg))
         (eval arg))
        ((functionp arg)
         (let ((f (find (nth-value 2 (function-lambda-expression arg)) *exported-function-names*)))
           (if f
               `(symbol-function (quote ,f))  ;; browser-side function
               (let ((id (random-key *local-lambdas*)))
                 (warn "Passing a local function as a parameter of browser-side function is dangerous!")
                 (setf (gethash-lock id *local-lambdas*) arg)
                 `(lambda (&rest args) (exec-local-lambda (cons ',id args)))))))
        (t arg)))

(defun get-all-sup (cls)
  (let ((scls (caddr (gethash-lock cls *exportable-expressions*))))
    (if scls
        (append scls (mapcan #'get-all-sup scls)))))

(defmacro make-def-macro-f (op)
  "Just a macro to generate macros for f-functions and f-macros definintions (defun-f, defmacro-f, etc...)"
  `(defmacro ,(read-from-string (format nil "~a-f" op)) (name args &rest body)
     (let* ((op ',op)
            (clos-args (if (and (equal 'defmethod op)
                                (not (listp args)))
                           (cons args (car body))
                           args))
            (is-setf (and (listp name) (equal (car name) 'setf)))
            (ex-sym (if is-setf
                        (intern (format nil "(SETF_~A)" (symbol-name (cadr name)))
                                *package*)
                        name))
            (f-form `(,op ,name ,args ,@body))
            (macro-hook (if (equal ',op 'defmacro) ;; Macro require an injection into JSCL lexenv
                            (let ((ar (gensym)))
                               `((jscl::%compile-defmacro ',name
                                   (lambda (,ar) (exec-remote-macro ',name ,ar)))))
                            nil))
            (lam (if is-setf (gensym)))
            (args2 (if is-setf
                        (mapcar (lambda (a)
                                  (declare (ignore a))
                                  `(quote ,(gensym)))
                                args)))
            (asym (gensym))
            (a2sym (gensym))
            (bdsym (gensym)))
       `(progn
         ,@macro-hook
         (remhash ',ex-sym *rpc-functions*) ;; Unregister possible RPC function with the same name
         (remote-unintern ',ex-sym) ;; unintern the function in all connected browsers
         ,(cond (is-setf
                 `(let* ((,asym ',args)
                         (,bdsym ',body)
                         (,a2sym (list ,@args2))
                         (,lam (lambda ,(cdr args)
                                 (values
                                  (list ,@(cdr args2))
                                  (list ,@(cdr args))
                                  (list ,(car args2))
                                  `(apply (lambda ,,asym ,@,bdsym)
                                          (list ,@,a2sym))
                                  (list ',(cdr name) ,@(cdr args2))))))
                     (if (assoc ',(cadr name) jscl::*setf-expanders*)
                         (setf (cdr (assoc ',(cadr name) jscl::*setf-expanders*)) ,lam)
                         (push (cons ',(cadr name) ,lam) jscl::*setf-expanders*))))
                ((or (equal ',op 'defmethod)
                     (equal ',op 'defgeneric))
                 `(let* ((classes (remove-duplicates
                                    (remove-if-not
                                      (lambda (cls)
                                        (gethash-lock cls *exportable-expressions*))
                                      (mapcar #'cadr (remove-if-not #'listp ',clos-args))))))
                    (if (not (find ',name *exported-function-names*))
                        (progn
                          (push ',name *exported-function-names*)
                          (setf (gethash-lock ',ex-sym *exportable-expressions*) (list 'progn ',f-form)))
                        (let* ((defs (cdr (gethash-lock ',ex-sym *exportable-expressions*)))
                               (pos (position ',clos-args
                                              defs :test
                                              #'tree-equal ;; FIXME: need to compare lambda lists here, not just to use #'tree-equal
                                              :key (lambda (l)
                                                     (if (listp (caddr l))
                                                         (caddr l)
                                                         (cons (caddr l) (cadddr l)))))))
                          (setf (gethash-lock ',ex-sym *exportable-expressions*)
                                (if pos
                                    `(progn ,@(subseq defs 0 pos) ,',f-form ,@(subseq defs (+ pos 1)))
                                    `(progn ,@defs ,',f-form)))))
                    (if (and classes (is-system-pkg (symbol-package ',name)))
                        (progn
                          (jscl::with-compilation-environment  ;; We need to compile specific methods locally to proper setup jscl clos environment
                            (jscl::compile-toplevel ',f-form))
                          (map nil
                            (lambda (cls)
                              (pushnew ',name (gethash-lock cls *exported-classes-methods*)))
                            classes)
                          (remote-update-methods ',name classes)))
                    (if (intersection (gethash-lock ',name *accessor-classes*)
                                      (append classes (mapcan #'get-all-sup classes)))
                        (map nil
                          (lambda (cls)
                            (pushnew ',name (gethash-lock cls *exported-classes-methods*)))
                          classes))))
                (t `(progn
                      (setf (gethash-lock ',ex-sym *exportable-expressions*) ',f-form)
                      (defmacro ,name (&rest args)
                         (if *in-f-macro*
                            `',(cons ',name (mapcar #'f-eval args))
                            (let ((*in-f-macro* t)) ;; Evaluate all -f functions and macros on the browser-side
                               `(remote-exec ',(cons ',name (mapcar #'f-eval args))))))
                      (if (not (find ',name *exported-function-names*))
                          (push ',name *exported-function-names*)))))
         ',name))))

(defmacro make-var-macro-f (op)
  "A macro for variables-parameters-constants definitions"
  `(defmacro ,(read-from-string (format nil "~a-f" op)) (name val)
     (let* ((op ',op)
            (f-form `(,op ,name ,val)))
           `(progn
              (remhash ',name *rpc-functions*)
              (setf (gethash-lock ',name *exportable-expressions*) ',f-form)
              (,op ,name ,val)
              (remote-unintern ',name)))))

(defmacro defclass-f (name sup slots)
  (let ((f-form `(defclass ,name ,sup ,slots))
        (tmp (gensym)))
    `(let ((,tmp (jscl::with-compilation-environment     ;; We need to compile defclass locally
                   (jscl::compile-toplevel ',f-form))))  ;;    to proper setup jscl compilation environment
       (declare (ignore ,tmp))
       (setf (gethash-lock ',name *exportable-expressions*) ',f-form)
       (if (not (gethash-lock ',name *exported-classes-methods*))
           (setf (gethash-lock ',name *exported-classes-methods*) (list)))
       (map nil
         (lambda (rec)
           (mapcan
             (lambda (s)
               (let ((r (getf (cdr rec) s)))
                 (if r
                     (setf (gethash-lock r *accessor-classes*)
                           (remove-duplicates (cons ',name
                                                     (gethash-lock r *accessor-classes*)))))))
             '(:accessor :reader :writer)))
         ',slots)
       (remote-rdefclass ',name))))

(make-def-macro-f defun)    ;; defun-f
(make-def-macro-f defmacro) ;; defmacro-f
(make-def-macro-f defmethod) ;; defmethod-f
(make-def-macro-f defgeneric) ;; defgeneric-f

(make-var-macro-f defvar)       ;; defvar-f
(make-var-macro-f defparameter) ;; defparameter-f
(make-var-macro-f defconstant)  ;; defconstant-f

(defvar *extra-html* "")

(defvar *pwa-mainfest* nil)
(defvar *pwa-icon* nil)
(defvar *pwa-path* "pwa")
(defvar *pwa-icon-file* nil)
(defvar *pwa-icon-type* nil)
(defvar *pwa-name* nil)
(defvar *pwa-sw-js-name* nil)

(defun make-pwa (&key (name "Application")
                      (short-name "app")
                      (display "standalone")
                      (theme-color "#000000")
                      (background-color "#ffffff")
                      icon-path)
  (setf *pwa-name* name)
  (setf *pwa-sw-js-name* (format nil "~A/pwa-sw-~A.js" (string-right-trim '(#\/) *root-path*) (random-string 10)))
  (setf *pwa-mainfest* (format nil "{
\"name\":\"~A\",\"short_name\":\"~A\",\"display\":\"~A\",\"start_url\":\"~A/\",\"theme_color\":\"~A\",\"background_color\":\"~A\"~A}"
                         name short-name display *root-path* theme-color background-color
                         (if icon-path
                             (let* ((path (if (equal (type-of icon-path) 'pathname)
                                              icon-path
                                              (parse-namestring icon-path)))
                                    (typ (media-types:extension-media-type (pathname-type path)))
                                    (size (cond ((equal typ "image/png")
                                                 (let ((png (pngload:load-file path :decode nil)))
                                                   (format nil "~Ax~A" (pngload:get-metadata png :width)
                                                                       (pngload:get-metadata png :height))))
                                                ((equal typ "image/jpeg")
                                                 (multiple-value-bind (h w ncomp trans) (cl-jpeg:jpeg-file-dimensions path)
                                                   (declare (ignore ncomp))
                                                   (declare (ignore trans))
                                                   (format nil "~Ax~A" w h)))
                                                ((equal typ "image/gif")
                                                 (let ((gif (skippy:load-data-stream path)))
                                                   (format nil "~Ax~A" (skippy:width gif) (skippy:height gif))))
                                                (t (error (format nil "Unsupported icon format: ~A" typ))))))
                               (setf *pwa-icon* (format nil "~A/~A/~A" (string-right-trim '(#\/) *root-path*) *pwa-path* (file-namestring path)))
                               (setf *pwa-icon-file* path)
                               (setf *pwa-icon-type* typ)
                               (format nil ",\"icons\"\: [{\"src\": \"~A\",\"sizes\": \"~A\",\"type\": \"~A\"}]"
                                           *pwa-icon*
                                           size
                                           typ))
                             ""))))

(defun get-root-html ()
  "Return a somple HTML with JS injected."
  (concatenate 'string "<html><head><title>"
               (if *pwa-name* *pwa-name* "")
               "</title>"
               (if *pwa-mainfest*
                   (format nil "<meta name=\"viewpo\rt\" content=\"width=device-width, user-scalable=no\" />
<link rel=\"manifest\" href=\"~A/~A/manifest.json\" />~A"
                           (string-right-trim '(#\/) *root-path*) *pwa-path*
                           (if *pwa-icon*
                               (format nil "<link rel=\"icon\" href=\"~A\" type=\"~A\" />"
                                       *pwa-icon*
                                       *pwa-icon-type*)
                               ""))
                   "")
               "<script src='"
               *root-path* *js-path*
               "' type='text/javascript'></script></head><body>"
               *extra-html*
               "</body></html>"))

(defun add-to-root-html (html)
  (setf *extra-html* (concatenate 'string *extra-html* html)))

(defun set-root-html (html)
  (setf *extra-html* html))

(defun get-main-js ()
  "Return the JS code, including JSCL and OMG parts glued."
  (concatenate 'string "(()=>{" jscl::*jscl-js* "

var OMG = Object.create(null)
self.OMG=OMG

OMG.URL=new URL(document.currentScript.src)
OMG.Path=OMG.URL.pathname.replace(/\\/[^\\\/]+$/,'')
OMG.HostPath=(OMG.URL.username?(OMG.URL.username+
                                 (OMG.URL.password?(':'+OMG.URL.password):'')+'@'):'')+
                               OMG.URL.host+
                               OMG.Path
OMG.Base=OMG.URL.protocol+'//'+OMG.HostPath
OMG.WS='" (if (use-wss-p) "wss://" "ws://") "'+OMG.HostPath+'" *root-path* *ws-path* "'

jscl.packages['COMMON-LISP-USER'] = jscl.packages.CL;

OMG.random_key=(()=>{
  const achars=Array(26).fill(0).reduce((...args)=>{args[0].push(String.fromCharCode('A'.charCodeAt(0)+args[2]));return args[0]},[])
  return (len)=>{
    return Array(len).fill().map(()=>{return achars[Math.floor(Math.random()*achars.length)]}).join('')
  }})()

OMG.root_ws=undefined
OMG.InFetch={}

OMG.OriginalSymbolValue=jscl.internals.symbolValue
jscl.internals.symbolValue=(symbol)=>{
  if(symbol.package) {
    const full_name=symbol.package.packageName+':'+symbol.name
    if(symbol.value===undefined&&symbol.package.omgPkg&&!OMG.InFetch[full_name]) {
      //console.log('SYMVALUE FETCH:', full_name)
      OMG.InFetch[full_name]=true
      let xhr=new XMLHttpRequest()
      xhr.open('POST', OMG.Base+'" *root-path* *gimme-path* "', false)
      xhr.send(full_name)
      if (xhr.status === 200) {
        eval(xhr.response)
      } else {
        throw new Error('Cannot fetch symbol '+name)
      }
    }
  }
  return OMG.OriginalSymbolValue(symbol)
}

OMG.OriginalIntern=jscl.internals.intern

OMG.Fetch=(sym)=>{
  const full_name=sym.package.packageName+':'+sym.name
  //console.log('FVALUE FETCH:',full_name)
  if(!OMG.InFetch[full_name]) {
    OMG.InFetch[full_name]=true
    let xhr=new XMLHttpRequest()
    xhr.open('POST', OMG.Base+'" *root-path* *gimme-path* "', false)
    xhr.send(full_name)
    if (xhr.status === 200) {
      //console.log(xhr.response)
      eval(xhr.response)
      OMG.InFetch[full_name]=false
    } else {
      OMG.InFetch[full_name]=false
      throw new Error('Cannot fetch symbol '+sym.package.packageName+'::'+sym.name)
    }
  } else {
    console.log('Already in fetch:',sym.name)
  }
}

OMG.FetchFvalue=(sym)=>{
  let isFetched=false
  return (...args)=>{
    if(isFetched) return sym.fvalue.apply(null,args)
    OMG.Fetch(sym)
    isFetched=true
    sym.fvalue=sym.package.symbols[sym.name].fvalue
    return sym.fvalue.apply(null,args)
  }
}

OMG.InMakePackage=false
OMG.MakePackage=(package_name)=>{
  if(!OMG.InMakePackage) {
    OMG.InMakePackage=true
    jscl.evaluateString('(DEFPACKAGE :'+package_name+' (:USE :CL :JSCL))')
    OMG.InMakePackage=false
    jscl.packages[package_name].omgPkg=true
  }
}

jscl.internals.intern=(name, package_name)=>{
  if(package_name && !(package_name in jscl.packages)) {
    OMG.MakePackage(package_name)
  }
  let sym=OMG.OriginalIntern(name, package_name)
  const full_name=package_name+':'+name
  if('package' in sym&&sym.package.omgPkg&&sym.value===undefined&&
     !jscl.internals.fboundp(sym)&&!OMG.InFetch[full_name]) {
    sym.fvalue=OMG.FetchFvalue(sym)
  }
  return sym
}

OMG.RPC=(cmd)=>{
  let xhr=new XMLHttpRequest()
  xhr.open('POST', OMG.Base+'" *root-path* *rpc-path* "', false)
  xhr.send(cmd)
  if (xhr.status === 200) {
    return eval(xhr.response)
  } else {
    throw new Error('Cannot call RPC')
  }
}

OMG.AsyncRPC=(cmd, cb)=>{
  let xhr=new XMLHttpRequest()
  xhr.open('POST', OMG.Base+'" *root-path* *rpc-path* "', true)
  xhr.onload=function (e) {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        cb(eval(xhr.response))
      } else {
        throw new Error('Cannot call RPC')
      }
    }
  }
  xhr.onerror = function (e) {
    throw new Error('Cannot call RPC')
  }
  xhr.send(cmd)
}

OMG.OriginalFP=jscl.packages.CL.symbols['FIND-PACKAGE'].fvalue
jscl.packages.CL.symbols['FIND-PACKAGE'].fvalue=(values,pkg)=>{
  let res=OMG.OriginalFP(values,pkg)
  if(!OMG.InMakePackage&&typeof(pkg)==='object'&&typeof(res)==='object'&&res.name==='NIL'&&res.package.packageName==='CL') {
    OMG.MakePackage(pkg.name)
    res=OMG.OriginalFP(values,pkg)
  }
  return res
}

OMG.OriginalLIL=jscl.packages.JSCL.symbols['LOOKUP-IN-LEXENV'].fvalue
jscl.packages.JSCL.symbols['LOOKUP-IN-LEXENV'].fvalue=(mv,name,lexenv,ns)=>{
  let res=OMG.OriginalLIL(mv,name,lexenv,ns)
  if(ns.name==='FUNCTION' && 'package' in name && name.package.omgPkg && 'name' in res && res.name==='NIL') {
    OMG.Fetch(name)
    res=OMG.OriginalLIL(mv,name,lexenv,ns)
  }
  return res
}

OMG.OriginalGSE=jscl.packages.JSCL.symbols['!GET-SETF-EXPANSION'].fvalue
jscl.packages.JSCL.symbols['!GET-SETF-EXPANSION'].fvalue=(mv,fn)=>{
  const set_name = '(SETF_'+fn.car.name+')'
  if('car' in fn && 'name' in fn.car && 'package' in fn.car && fn.car.package.omgPkg && !(set_name in fn.car.package.symbols)) {
    //console.log('NEED FETCH:',set_name, fn.car.package.packageName)
    const full_name=fn.car.package.packageName+':'+set_name
    OMG.InFetch[full_name]=true
    let xhr=new XMLHttpRequest()
    xhr.open('POST', OMG.Base+'" *root-path* *gimme-path* "', false)
    xhr.send(full_name)
    if (xhr.status === 200) {
      eval(xhr.response)
    } else {
      throw new Error('Cannot fetch set symbol '+fn.car.package.packageName+'::'+set_name)
    }
  }
  return OMG.OriginalGSE(mv,fn)
}

OMG.OriginalCAMUC=jscl.packages.JSCL.symbols['COMPUTE-APPLICABLE-METHODS-USING-CLASSES'].fvalue
jscl.packages.JSCL.symbols['COMPUTE-APPLICABLE-METHODS-USING-CLASSES'].fvalue=(mv,gf,clss)=>{
  let res=OMG.OriginalCAMUC(mv,gf,clss)
  if ('name' in res && res.name=='NIL') {
    OMG.Fetch(gf.cdr.cdr.car[0])
    res=OMG.OriginalCAMUC(mv,gf,clss)
  }
  return res
}

OMG.OriginalFC=jscl.packages.JSCL.symbols['!FIND-CLASS'].fvalue
jscl.packages.JSCL.symbols['!FIND-CLASS'].fvalue=(mv,cls,arg2)=>{
  if('package' in cls && cls.package.omgPkg &&
     !cls.package.symbols[cls.name].omgClass) {
    cls.package.symbols[cls.name].omgClass=true
    OMG.Fetch(cls)
  }
  return OMG.OriginalFC(mv,cls,arg2)
}

OMG.OriginalFC1=jscl.packages.CL.symbols['FIND-CLASS'].fvalue
jscl.packages.CL.symbols['FIND-CLASS'].fvalue=(mv,cls,arg2)=>{
  if('package' in cls && cls.package.omgPkg &&
     !cls.package.symbols[cls.name].omgClass) {
    cls.package.symbols[cls.name].omgClass=true
    OMG.Fetch(cls)
  }
  return OMG.OriginalFC1(mv,cls,arg2)
}


/*
OMG.OriginalCO=jscl.packages.JSCL.symbols['!CLASS-OF'].fvalue
jscl.packages.JSCL.symbols['!CLASS-OF'].fvalue=(mv,cls)=>{
  if('name' in cls) {
    console.log('CO:',cls.name,cls)
  }
  if(typeof(cls)=='object' && 'package' in cls && cls.package.omgPkg &&
     !cls.package.symbols[cls.name].omgClass) {
    cls.package.symbols[cls.name].omgClass=true
    console.log('COF:', cls.name)
    OMG.Fetch(cls)
    console.log('COF OK:', cls.name)
  }
  return OMG.OriginalCO(mv,cls)
}
*/

OMG.make_conn=()=>{
  console.log('Connecting to host')
  OMG.root_ws=new WebSocket(OMG.WS)
  OMG.root_ws.onopen=()=>{console.log('Socket connected')}
  OMG.root_ws.onclose=(ev)=>{
    console.log('Socket closed ('+(ev.wasClean?'normally':'by error')+'), reconnecting')
    delete(OMG.root_ws)
    setTimeout(OMG.make_conn,1000)
  }
  OMG.root_ws.onerror=(err)=>{
    console.log('Socket error ('+err.message+')')
    OMG.root_ws.close()
  }
  OMG.root_ws.onmessage=function (ev) {
    const cmd=ev.data;
    setTimeout(()=>{eval(cmd)},1)
  }
}

"   (if *pwa-mainfest*
      (concatenate 'string "if (navigator.serviceWorker != null) {
      navigator.serviceWorker.register('" *pwa-sw-js-name* "', {scope: '" *root-path* "'}).then(function(registration) {
    console.log('Registered events at scope: ', registration.scope);
    });}")
      "")
    "if(document.readyState==='complete') {
  OMG.make_conn()
} else {
  document.addEventListener('DOMContentLoaded',()=>{
    OMG.make_conn()
  })
}})()
"))

(defvar *current-session* nil)       ;; The current session, usually set by remote-exec
(defvar *session-list* (make-hash-table))  ;; The store for session objects, keys are session-ids
(defvar *current-res* nil) ;; The key for gimme-wait-list hash, denotes the place where to store result for
                                 ;;   current gimme request
(defvar *in-rpc* nil) ;; If T -- we are in RPC call, all remote-execs must be done via takit-mechanism

(defun current-session-id ()
  (if *current-session*
      (get-id *current-session*)))

(defclass omg-session ()
  ((socket :initarg :socket
           :initform (error "socket required")
           :reader socket)
   (ses-id :initform (random-symbol |sid-length|)
           :reader get-id)
   (last-active :initform (get-universal-time)
                :reader last-active)
   (wait-list :initform (make-hash-table)
              :reader wait-list))
  (:documentation
    "The session object, holding the session socket, the wait-list holding semaphores and storing result data from remote-exec calls"))

(defun find-session (sid)
  "Return session object with specific ID"
  (gethash-lock sid *session-list*))

(defmacro with-session (sess &rest body)
  "Execute commands inside session sess"
  `(let ((*current-session* ,sess))
      ,@body))

(defvar *debug-session-id* nil)

(defun set-debug-session (sid)
  (setf *debug-session-id* sid)
  nil)

(defmacro in-debug-session (&rest body)
  (let ((ses (gensym)))
    `(let ((,ses (find-session *debug-session-id*)))
       (if ,ses
           (with-session ,ses
             ,@body)
           (warn "Cannot find debug session!")))))

(defmacro def-session-var (vr &optional init)
  (let ((h (gentemp))
        (def (gentemp)))
    `(progn
       (defvar ,h (make-hash-table))
       (defparameter ,def ,init)
       (define-symbol-macro ,vr
         (gethash (current-session-id) ,h ,def)))))

(defun remote-unintern (sym)
  "Unintern the symbol within all active sessions, mandatory to reflect symbol redefinition.
   The next try to intern the symbol in browser-side will cause new symbol fetch."
  (loop for s being the hash-values of *session-list* do
       (let* ((sym-name (symbol-name sym))
              (sym-pkg (package-name (symbol-package sym)))
              (cmd (format nil "if(\"~A\" in jscl.packages && \"~A\" in jscl.packages[\"~A\"].symbols) {
            delete(OMG.InFetch[\"~A:~A\"])
            jscl.packages[\"~A\"].symbols[\"~A\"].fvalue=OMG.FetchFvalue(OMG.OriginalIntern(\"~A\", \"~A\"))
            jscl.packages[\"~A\"].symbols[\"~A\"].value=undefined}"
                                sym-pkg sym-name sym-pkg sym-pkg sym-name sym-pkg sym-name sym-name sym-pkg sym-pkg sym-name)))
         (send-text (socket s) cmd))))

(defun remote-rdefclass (cls)
  (loop for s being the hash-values of *session-list* do
       (let* ((cls-name (symbol-name cls))
              (cls-pkg (package-name (symbol-package cls)))
              (cmd (format nil "if(\"~A\" in jscl.packages && \"~A\" in jscl.packages[\"~A\"].symbols &&
                                   \"omgClass\" in jscl.packages[\"~A\"].symbols[\"~A\"]) {
            delete(OMG.InFetch[\"~A:~A\"])
            OMG.Fetch(jscl.packages[\"~A\"].symbols[\"~A\"])}"
                                cls-pkg cls-name cls-pkg cls-pkg cls-name
                                cls-pkg cls-name
                                cls-pkg cls-name)))
         (send-text (socket s) cmd))))

(defun remote-update-methods (method classess)
  (loop for s being the hash-values of *session-list* do
       (let* ((mathod-name (symbol-name method))
              (mathod-pkg (package-name (symbol-package method)))
              (cmd (format nil "~{{~A}~}"
                     (mapcar
                       (lambda (cls-name cls-pkg)
                         (format nil "if(\"~A\" in jscl.packages && \"~A\" in jscl.packages[\"~A\"].symbols &&
                                         \"omgClass\" in jscl.packages[\"~A\"].symbols[\"~A\"] &&
                                         \"~A\" in jscl.packages && \"~A\" in jscl.packages[\"~A\"].symbols) {
                                       OMG.Fetch(jscl.packages[\"~A\"].symbols[\"~A\"])
                                     }"
                           cls-pkg cls-name cls-pkg
                           cls-pkg cls-name
                           mathod-pkg mathod-name mathod-pkg
                           mathod-pkg mathod-name))
                       (mapcar #'symbol-name classess)
                       (mapcar #'package-name (mapcar #'symbol-package classess))))))
         (send-text (socket s) cmd))))


;; From LISP Cookbook
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun put-wl-result (res key)
  (let* ((gwl (gethash-lock key *gimme-wait-list*))
         (sem (cdr (assoc :sem gwl)))
         (time (cdr (assoc :time gwl))))
    (setf (gethash-lock key *gimme-wait-list*) `((:result . ,res) (:time . ,time)))
    (signal-semaphore sem)))

(defun compile-to-js (code pkg)
  "Return JS for the code, pkg is current package for compilation context."
  (let* ((*package* pkg)
         (c1 (write-to-string
               `(let ((*package* (find-package (intern ,(package-name pkg) :keyword))))
                  ,code)))
         (compile-local *local-compile*)
         (code (if compile-local
                   (jscl::with-compilation-environment
                     (jscl::compile-toplevel (jscl::ls-read-from-string c1) t t))
                   (write-to-string c1)))
         (rcode (if compile-local
                    (replace-all (replace-all (replace-all (replace-all code "\\" "\\\\") (string #\linefeed) "\\n") (string #\return) "\\\\r") "\"" "\\\"")
                    (replace-all (replace-all code (string #\linefeed) " ") (string #\return) " ")))
         (res (if compile-local
                  (concatenate 'string "jscl.internals.lisp_to_js(jscl.internals.globalEval(\"" rcode "\"))")
                  (concatenate 'string "jscl.evaluateString(" rcode ")"))))
     res))

(defun takit (key res)
  "The handler for takit-requests. This requests are used to return macro expansion results from browser-side"
  (let* ((sem-dat-sym (gethash-lock key *takit-wait-list*)))
    (if sem-dat-sym
      (let ((newsem (make-semaphore)))
        (setf (gethash-lock key *takit-wait-list*)
              (let ((*package* (symbol-package (cdr (assoc :symbol sem-dat-sym))))
                    (*read-eval* nil))
                `((:result . ,(read-from-string res))
                  ,(assoc :time sem-dat-sym))))
        (setf (gethash-lock key *gimme-wait-list*) `((:sem . ,newsem) (:time . ,(get-universal-time)) ,(assoc :symbol sem-dat-sym)))
        (signal-semaphore (cdr (assoc :sem sem-dat-sym)))
        (wait-on-semaphore newsem)
        `(200 (:content-type "text/plain; charset=utf-8") (,(cdr (assoc :result (gethash-lock key *gimme-wait-list*))))))
      `(404 (:content-type "text/plain; charset=utf-8") ("")))))

(defun gimme (sym)
  "The handler for gimme-requests, which are used to request unknown symbols from the server-side."
  (let* ((datp (gethash-lock sym *exportable-expressions*))
         (auto-funcs (if (and (listp datp)
                              (equal 'defclass (car datp)))
                         (mapcan
                           (lambda (sym)
                             (let ((cod (gethash-lock sym *exportable-expressions*)))
                               (if cod
                                   (list cod))))
                           (remove-duplicates
                             (gethash-lock sym *exported-classes-methods*))))))
    (if datp
       (let* ((dat (if (boundp sym)
                       (list (car datp) (cadr datp)
                         (let ((sv (symbol-value sym)))
                           (if (listp sv)
                               `(quote ,sv)
                               sv)))
                       datp))
              (sem (make-semaphore))
              (key (random-key *gimme-wait-list* |sid-length|)))
          (setf (gethash-lock key *gimme-wait-list*) `((:sem . ,sem) (:time . ,(get-universal-time)) (:symbol . ,sym)))
          (push (bt:make-thread
                   (lambda ()
                     (put-wl-result
                       (format nil "{~A};~{{~A}~^;~}"
                         (compile-to-js dat (symbol-package sym))
                         (mapcar (lambda (f)
                                   (compile-to-js f (symbol-package sym)))
                                 auto-funcs))
                       key))
                   :initial-bindings `((*current-res* . ',key)))
                *omg-thread-list*)
          (wait-on-semaphore sem)
          (let ((res (cdr (assoc :result (gethash-lock key *gimme-wait-list*)))))
             (remhash key *gimme-wait-list*)
             (unintern key)
            `(200 (:content-type "text/plain; charset=utf-8") (,res))))
       `(404 (:content-type "text/plain; charset=utf-8") ("")))))

(defun rpc-wrapper (op args pkg)
  "The wrapper for RPC requests, used to allow call browser-side functions from RPC funcs."
   (let* ((sem (make-semaphore))
          (key (random-key *gimme-wait-list* |sid-length|)))
      (setf (gethash-lock key *gimme-wait-list*) `((:sem . ,sem) (:time . ,(get-universal-time)) (:symbol . ,(intern "omg-rpc-symbol" pkg))))
      (push (bt:make-thread
              (lambda ()
                (put-wl-result (compile-to-js `(read-from-string ,(write-to-string (apply op args))) pkg) key))
              :initial-bindings `((*current-res* . ',key)
                                  (*current-session* . ,*current-session*)
                                  (*in-rpc* . t)))
            *omg-thread-list*)
      (wait-on-semaphore sem)
      (let ((res (cdr (assoc :result (gethash-lock key *gimme-wait-list*)))))
         (remhash key *gimme-wait-list*)
         (unintern key)
        `(200 (:content-type "text/plain; charset=utf-8") (,res)))))

(defun exec-remote-macro (name args)
  "Execute in a browser a code of the macro with the specific name and argments.
   Called by JSCL compiler while compling lisp code to JS. We have to execute macros on the
   browser-side, because all side-effects, produced by macros, must have a place in the browser."
  (if *current-res*
      (let* ((sem-tim-sym (gethash-lock *current-res* *gimme-wait-list*))
             (sem (cdr (assoc :sem sem-tim-sym)))
             (sym (cdr (assoc :symbol sem-tim-sym)))
             (takit-sem (make-semaphore))
             (mcod (compile-to-js
                      `(write-to-string (apply (lambda ,@(cddr (gethash-lock name *exportable-expressions*))) ',args))
                       (symbol-package sym))))
        (setf (gethash-lock *current-res* *takit-wait-list*) `((:sem . ,takit-sem) (:time . ,(get-universal-time)) (:symbol . ,sym)))
        (setf (gethash-lock *current-res* *gimme-wait-list*)
              `((:result . ,(format nil (concatenate 'string "xhr=new XMLHttpRequest();xhr.open('POST','" *root-path* *takit-path* "',false);"
                                                             "xhr.send('~A'+(~A));if(xhr.status===200){eval(xhr.response);}else"
                                                             "{throw new Error('Cannot fetch symbol (takit fails).');}")
                                        (symbol-name *current-res*)
                                        mcod))
                (assoc :time sem-tim-sym)))
        (signal-semaphore sem)
        (wait-on-semaphore takit-sem)
        (let ((macro-res (cdr (assoc :result (gethash-lock *current-res* *takit-wait-list*)))))
          (remhash *current-res* *takit-wait-list*)
          (unintern *current-res*)
          macro-res))
      (remote-exec `(apply (lambda ,@(cddr (gethash-lock name *exportable-expressions*))) ',args))))


(defun remote-exec (cmd &optional nowait)
  "Execute the code on the browser-side. If the *current-session* set, the code will be executed
   within the specific session, otherwise, the code will be executed in all sessions and all the return
   values are returned as a list. If the nowait is T, the function will retrurn NIL immediately, without waiting
   result from the remote side."
  (if *in-rpc*
    (let* ((sem-tim-sym (gethash-lock *current-res* *gimme-wait-list*))
           (sem (cdr (assoc :sem sem-tim-sym)))
           (takit-sem (make-semaphore))
           (mcod (compile-to-js `(write-to-string ,cmd) *package*)))
      (setf (gethash-lock *current-res* *takit-wait-list*) `((:sem . ,takit-sem) (:time . ,(get-universal-time)) ,(assoc :symbol sem-tim-sym)))
      (setf (gethash-lock *current-res* *gimme-wait-list*)
            `((:result . ,(format nil (concatenate 'string "xhr=new XMLHttpRequest();xhr.open('POST','" *root-path* *takit-path* "',false);"
                                                           "xhr.send('~A'+(~A));if(xhr.status===200){eval(xhr.response);}else"
                                                           "{throw new Error('Cannot fetch symbol (takit fails).');}")
                                      (symbol-name *current-res*)
                                      mcod))
              ,(assoc :time sem-tim-sym)))
      (signal-semaphore sem)
      (wait-on-semaphore takit-sem)
      (let ((res (cdr (assoc :result (gethash-lock *current-res* *takit-wait-list*)))))
        (remhash *current-res* *takit-wait-list*)
        (unintern *current-res*)
        res))
    (flet ((exec () (let* ((wlist (wait-list *current-session*))
                           (sock (socket *current-session*))
                           (sock-state (ready-state sock))
                           (key (random-key wlist |sid-length|))
                           (sem (if nowait nil (make-semaphore)))
                           (rcmd (compile-to-js `(write-to-string (multiple-value-list ,cmd)) *package*))
                           (pkgname (package-name *package*))
                           (pkg-hook (if *local-compile*
                                          ""
                                          (format nil "jscl.evaluateString(\"(IN-PACKAGE :~A)\")" pkgname)))
                           (jscmd (if nowait
                                      (format nil "~A;~A;"
                                          pkg-hook
                                          rcmd)
                                      (format nil "~A;OMG.root_ws.send((\"~~~A\"+~A));"
                                          pkg-hook
                                          (symbol-name key)
                                          rcmd))))
                     (if (equal sock-state :open)
                       (progn
                         (if (not nowait) (setf (gethash-lock key wlist) `(,(current-thread) ,sem nil)))
                         (send-text sock jscmd)
                         (setf (slot-value *current-session* 'last-active) (get-universal-time))
                         (if (not nowait)
                             (progn
                               (wait-on-semaphore sem)
                               (let ((ret (let ((*read-eval* nil))
                                            (read-from-string (caddr (gethash-lock key wlist))))))
                                   (remhash key wlist)
                                (unintern key)
                                (apply #'values ret)))))
                       (if (equal sock-state :closed)
                           (progn
                             (emit :close sock)
                             nil))))))
      (if *current-session*
          (exec)
          (loop for s being the hash-values of *session-list* collect (with-session s (exec)))))))

(defvar *boot-functions* nil)

(defun add-to-boot (f)
  (push f *boot-functions*))

(defun set-boot (f)
  (setf *boot-functions* (list f)))

(defun rm-from-boot (f)
  (delete f *boot-functions*))

(defun boot-f ()
  "The boot code, will be executed on the browser-side just after the socket is connected."
  (remote-exec `(defparameter *session-id* ',(get-id *current-session*)) :nowait)
  (setf (slot-value *current-session* 'last-active) (get-universal-time))
  (map nil (lambda (f) (remote-exec f :nowait)) *boot-functions*))

(defun make-ws (env)
  "Return the websocket for the new session. Also, creates the session object."
  (let* ((ws (websocket-driver.server:make-server env))
         (ses (make-instance 'omg-session :socket ws))
         (sid (get-id ses)))
    (setf (gethash-lock sid *session-list*) ses)
    (on :open ws
      (lambda ()
        (push (bt:make-thread
                (lambda ()
                  (let ((*current-session* ses)
                        (*package* (find-package :omg)))
                    (setf (slot-value *current-session* 'last-active) (get-universal-time))
                    (boot-f)))
                :name (concatenate 'string (symbol-name sid) "-BOOT"))
              *omg-thread-list*)))
    (on :error ws
      (lambda (error)
        (format t "WS error: ~S~%" error)))
    (on :message ws
      (lambda (msg)
        (let* ((m (subseq msg 0 1))
               (rid (intern (subseq msg 1 (+ |sid-length| 1)) :omg))
               (val (subseq msg (+ |sid-length| 1))))
          (setf (slot-value ses 'last-active) (get-universal-time))
          (cond ((equal m "~")
                 (let* ((wlist (wait-list ses))
                        (trsem (gethash-lock rid wlist)))
                  (if trsem
                     (progn
                       (setf (gethash-lock rid wlist) (list (car trsem) (cadr trsem) val))
                       (signal-semaphore (cadr trsem))))))))))
    (on :close ws
       (lambda (&key code reason)
        (format t "WS closed (~a ~a)~%" code reason)
        (maphash (lambda (k v)
                   (if (and (car v) (not (equal (current-thread) (car v))) (thread-alive-p (car v)))
                       (destroy-thread (car v)))
                   (unintern k))
                 (wait-list ses))
        (remhash sid *session-list*)
        (remove-all-listeners ws)
        (unintern sid)))
    ws))

(defun get-str-from (s len)
  (let ((*read-eval* nil)
        (tseq (make-array (list len) :element-type '(unsigned-byte 8))))
    (read-sequence tseq s)
    (utf-8-bytes-to-string tseq)))

(defvar *pwa-sw-js* "
if(OMG===undefined) {
  var OMG={}
}
OMG.bc = new BroadcastChannel('omg_service_worker');

self.addEventListener('install', function(e) {
  console.log('Install event!')
  return self.skipWaiting()
})

self.addEventListener('activate', function(e) {
  console.log('Activate event!')
  return self.clients.claim()
})

self.addEventListener('fetch', function(e) {
  console.log('Fetch event:', e.request.url)
  OMG.bc.postMessage({type:'fetch', uri:e.request.url})
  return fetch(e.request.clone())
})
")

(defvar *user-uri-handler* (lambda (env)
                             (declare (ignore env))
                             '(404 (:content-type "text/plain") ("File not found"))))

(defun serv (env)
  (let ((uri (getf env :REQUEST-URI))
        (*read-eval* nil))
    (cond ((equal uri (concatenate 'string *root-path* *pwa-path* "/manifest.json"))
           (if *pwa-mainfest*
               `(200 (:content-type "application/json; charset=utf-8") (,*pwa-mainfest*))
               '(404 (:content-type "text/plain") ("File not found"))))
          ((equal uri *pwa-sw-js-name*)
           (if *pwa-mainfest*
               `(200 (:content-type "text/javascript; charset=utf-8")
                     (,*pwa-sw-js*))
               '(404 (:content-type "text/plain") ("File not found"))))
          ((equal uri *pwa-icon*)
           `(200 (:content-type ,*pwa-icon-type*
                  :content-length ,(with-open-file (f *pwa-icon-file* :direction :input) (file-length f)))
                 ,*pwa-icon-file*))
          ((equal uri (concatenate 'string *root-path* *js-path*))
           `(200
               (:content-type "text/javascript; charset=utf-8")
               (,(get-main-js))))
          ((equal uri (concatenate 'string *root-path* *html-path*))
           `(200 (:content-type "text/html; charset=utf-8") (,(get-root-html))))
          ((and (equal uri (concatenate 'string *root-path* *rpc-path*))
                (getf env :content-length))
           (let* ((cmd (read-from-string (omg::replace-all (get-str-from (getf env :raw-body) (getf env :content-length))
                                                           "\\n"
                                                           (make-string 1 :initial-element #\newline))))
                  (pkg (find-package (car cmd)))
                  (op (intern (symbol-name (cadr cmd)) pkg))
                  (args (caddr cmd))
                  (*current-session* (find-session (intern (symbol-name (cadddr cmd)) :omg))))
              (setf (slot-value *current-session* 'last-active) (get-universal-time))
              (if (gethash-lock op *rpc-functions*)
                (rpc-wrapper op args pkg)
               `(404 (:content-type "text/plain; charset=utf-8") ("")))))
          ((and (equal uri (concatenate 'string *root-path* *gimme-path*))
                (getf env :content-length))
           (let* ((str (get-str-from (getf env :raw-body) (getf env :content-length)))
                  (pos (position #\: str))
                  (pkg (find-package (subseq str 0 pos))))
             (gimme (intern (subseq str (+ 1 pos)) pkg))))
          ((and (equal uri (concatenate 'string *root-path* *takit-path*))
                (getf env :content-length))
           (let* ((str (get-str-from (getf env :raw-body) (getf env :content-length))))
             (takit (intern (subseq str 0 |sid-length|) :omg)
                    (subseq str |sid-length|))))
          ((equal uri (concatenate 'string *root-path* *ws-path*))
           (let ((ws (make-ws env)))
             (lambda (responder)
               (declare (ignorable responder))
               (start-connection ws))))
          (t (funcall *user-uri-handler* env)))))

(defvar *serv* nil)

(defvar *last-args* nil)

(defparameter *wl-timeout* 600)
(defparameter *session-timeout* (* 60 60 24))

(defun start-server (&rest args)
  (if args (setf *last-args* args))
  (start-multiprocessing) ;; bordeaux-threads requirement
  (if (not *giant-hash-lock*)
      (setf *giant-hash-lock* (bt:make-lock)))
  (setf *serv* (apply #'clack:clackup
                      `(,#'serv
                        :port ,*port*
                        :ssl ,(has-ssl-p)
                        :ssl-key-file ,*ssl-key*
                        :ssl-cert-file ,*ssl-cert*
                        ,@*last-args*)))
  (push (bt:make-thread
          (lambda ()
            (loop do
              (let ((tim (get-universal-time)))
                (labels ((clwl (h)
                           (map nil
                             (lambda (x)
                               (let ((t1 (cdr (assoc :time (gethash-lock x h)))))
                                 (if (and t1 (> (- tim t1) *wl-timeout*))
                                     (progn
                                       (remhash x h)
                                       (unintern x)))))
                             (loop for k being each hash-key of h collect k))))
                  (map nil #'clwl (list *gimme-wait-list* *takit-wait-list*))
                  (map nil
                    (lambda (k)
                      (let ((ses (gethash-lock k *session-list*)))
                        (if (not (equal (ready-state (socket ses)) :open))
                            (if (> (- tim (last-active ses)) *session-timeout*)
                                (progn
                                  (remhash k *session-list*)
                                  (unintern k)))
                            (setf (slot-value ses 'last-active) tim))))
                    (loop for k being each hash-key of *session-list* collect k))
                  (map nil
                    (lambda (thr)
                      (delete thr *omg-thread-list*))
                    (remove-if #'bt:thread-alive-p *omg-thread-list*)))
                (sleep 60)))))
        *omg-thread-list*))

(defun kill-server ()
  (map nil #'clrhash (list *gimme-wait-list* *takit-wait-list* *session-list*))
  (map nil #'bt:destroy-thread (remove-if-not #'bt:thread-alive-p *omg-thread-list*))
  (setf *omg-thread-list* nil)
  (if *serv* (clack::stop *serv*)))

(defun restart-server (&rest args)
  (progn
    (kill-server)
    (apply #'start-server args)))

(defmacro thread-in-session (&rest code)
  `(push (bt:make-thread
           (lambda ()
             ,@code)
           :initial-bindings (list (cons '*current-session* *current-session*)))
         *omg-thread-list*))
