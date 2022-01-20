(defpackage :omg
  (:use cl clack websocket-driver bordeaux-threads trivial-utf-8)
  (:import-from :event-emitter #:emit)
  (:export add-to-boot       ;; add a code to boot sequence
           add-to-root-html  ;; add a text to html body
           rm-from-boot      ;; remove a code from boot sequence
           start-server      ;; start a http(s)-server
           kill-server       ;; kill a http(s)-server
           restart-server    ;; restart a http(s)-server
           defun-f           ;; define a browser-side function
           defmacro-f        ;; define a browser-side macro
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
           make-pwa))       ;; serve web page as a Progressive Web App

(in-package :omg)

(defconstant |sid-length| 10) ;; the length for all random IDs generated

(defparameter *local-compile* t) ;; if T -- compile all lisp code to JS on the server side
                                 ;; can be altered without page reloading or server restart

;; Here are the paths for all HTTP(s) queries (the page reload required after change):

(defparameter *root-path* "") ;; must be started with "/"
(defparameter *html-path* "") ;; the path (relative to *root-path*) for simple html page with injcetced js
(defparameter *js-path* "j")  ;; the path of js for injection (relative to *root-path*)
(defparameter *ws-path* "s")  ;; websocket path (relative to *root-path*)
(defparameter *rpc-path* "r") ;; rpc call path (relative to *root-path*)
(defparameter *gimme-path* "g") ;; the path to query undefined symbols and functions (relative to *root-path*)
(defparameter *takit-path* "t") ;; the auxilary path, nedded to return macro expansion results if *local-compile* is set
(defparameter *port* 7500) ;; default server port

(defparameter *use-wss* nil) ;; if T -- use wss:// protocol for websocket
(defparameter *ssl-key* nil) ;; SSL key path
(defparameter *ssl-cert* nil) ;; SSL cert path

(defparameter *giant-hash-lock* nil) ;; A giant lock to make hashes thread safe

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

(defparameter *exportable-expressions* (make-hash-table)) ;; All exportable functions are here
(defparameter *rpc-functions* (make-hash-table))          ;; The allowed RPC functions registry
(defparameter *gimme-wait-list* (make-hash-table))        ;; The temporary storage for gimme-threads,
                                                    ;;   waiting compilation results
                                                    ;; FIXME: periodic cleanup procedure needed!
(defparameter *takit-wait-list* (make-hash-table))        ;; The temporary storage for compilation threads,
                                                    ;;   waiting for macro expansion results from browser side
                                                    ;; FIXME: periodic cleanup procedure needed!

(defparameter *in-f-macro* nil)   ;; If T -- do not convert -f function calls to (remote-exec ...) (don't change manually!!!)

(defparameter *exported-function-names* nil) ;; association list where browser-side functions associated with their names

(defparameter *local-lambdas* (make-hash-table)) ;; list of unnamed functions, passed as arguments to browser-side ones
                                           ;; used by exec-local-lambda RPC-function to determine what lambda to execute

(defun register-rpc (name)
  (remhash name *exportable-expressions*)
  (setf (gethash-lock name *rpc-functions*) t)
  (setf (gethash-lock name *exportable-expressions*)
       `(defun ,name (&rest argl)
          (funcall (jscl::oget (jscl::%js-vref "jscl") "omgRPC")
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
         (let ((f (assoc arg *exported-function-names*)))
           (if f
               `(symbol-function (quote ,(cdr f)))  ;; browser-side function
               (let ((id (random-key *local-lambdas*)))
                 (warn "Passing a local function as a parameter of browser-side function is dangerous!")
                 (setf (gethash-lock id *local-lambdas*) arg)
                 `(lambda (&rest args) (exec-local-lambda (cons ',id args)))))))
        (t arg)))

(defmacro make-def-macro-f (op)
  "Just a macro to generate macros for f-functions and f-macros definintions (defun-f, defmacro-f, etc...)"
  `(defmacro ,(read-from-string (format nil "~a-f" op)) (name args &rest body)
     (let* ((op ',op)
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
         (setf (gethash-lock ',ex-sym *exportable-expressions*) ',f-form)
         (remote-unintern ',ex-sym) ;; unintern the function in all connected browsers
         ,(if is-setf
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
                      (push (cons ',(cadr name) ,lam) jscl::*setf-expanders*)))
              `(progn
                 (defmacro ,name (&rest args)
                    (if *in-f-macro*
                       `',(cons ',name (mapcar #'f-eval args))
                       (let ((*in-f-macro* t)) ;; Evaluate all -f functions and macros on the browser-side
                          `(remote-exec ',(cons ',name (mapcar #'f-eval args))))))
                 (if (not (assoc (function ,name) *exported-function-names*))
                     (setf *exported-function-names* (cons (cons (function ,name) ',name) *exported-function-names*))))))))) ;; It is strange, but PUSH causes error here!

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


(make-def-macro-f defun)    ;; defun-f
(make-def-macro-f defmacro) ;; defmacro-f

(make-var-macro-f defvar)       ;; defvar-f
(make-var-macro-f defparameter) ;; defparameter-f
(make-var-macro-f defconstant)  ;; defconstant-f

(defparameter *extra-html* "")

(defparameter *pwa-mainfest* nil)
(defparameter *pwa-icon* nil)
(defparameter *pwa-path* "pwa")
(defparameter *pwa-icon-file* nil)
(defparameter *pwa-icon-type* nil)
(defparameter *pwa-name* nil)
(defparameter *pwa-sw-js-name* nil)

(defun make-pwa (&key (name "Application")
                      (short-name "app")
                      (display "standalone")
                      (theme-color "#000000")
                      (background-color "#ffffff")
                      icon-path)
  (setf *pwa-name* name)
  (setf *pwa-sw-js-name* (format nil "~A/pwa-sw-~A.js" *root-path* (random-string 10)))
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
                               (setf *pwa-icon* (format nil "~A/~A/~A" *root-path* *pwa-path* (file-namestring path)))
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
                           *root-path* *pwa-path*
                           (if *pwa-icon*
                               (format nil "<link rel=\"icon\" href=\"~A\" type=\"~A\" />"
                                       *pwa-icon*
                                       *pwa-icon-type*)
                               ""))
                   "")
               "<script src='"
               *root-path* "/" *js-path*
               "' type='text/javascript'></script></head><body>"
               *extra-html*
               "</body></html>"))

(defun add-to-root-html (html)
  (setf *extra-html* (concatenate 'string *extra-html* html)))

(defun get-main-js ()
  "Return the JS code, including JSCL and OMG parts glued."
  (concatenate 'string "(()=>{" jscl::*jscl-js* "
const omgURL=new URL(document.currentScript.src)
const omgPath=omgURL.pathname.replace(/\\/[^\\\/]+$/,'')
const omgHostPath=(omgURL.username?(omgURL.username+
                                  (omgURL.password?(':'+omgURL.password):'')+'@'):'')+
                  omgURL.host+
                  omgPath
const omgBase=omgURL.protocol+'//'+omgHostPath
const omgWS='" (if (use-wss-p) "wss://" "ws://") "'+omgHostPath+'" *root-path* "/" *ws-path* "'

jscl.packages['COMMON-LISP-USER'] = jscl.packages.CL;

const random_key=(()=>{
  const achars=Array(26).fill(0).reduce((...args)=>{args[0].push(String.fromCharCode('A'.charCodeAt(0)+args[2]));return args[0]},[])
  return (len)=>{
    return Array(len).fill().map(()=>{return achars[Math.floor(Math.random()*achars.length)]}).join('')
  }})()

let root_ws=undefined

const omgInFetch={}

const omgOriginalSymbolValue=jscl.internals.symbolValue
jscl.internals.symbolValue=(symbol)=>{
  if(symbol.package) {
    const full_name=symbol.package.packageName+':'+symbol.name
    if(symbol.value===undefined&&symbol.package.omgPkg&&!omgInFetch[full_name]) {
      //console.log('SYMVALUE FETCH:', full_name)
      omgInFetch[full_name]=true
      let xhr=new XMLHttpRequest()
      xhr.open('POST', omgBase+'" *root-path* "/" *gimme-path* "', false)
      xhr.send(full_name)
      if (xhr.status === 200) {
        eval(xhr.response)
      } else {
        throw new Error('Cannot fetch symbol '+name)
      }
    }
  }
  return omgOriginalSymbolValue(symbol)
}

const omgOriginalIntern=jscl.internals.intern

const omgFetchFvalue=(sym)=>{
  let isFetched=false
  return (...args)=>{
    if(isFetched) return sym.fvalue.apply(null,args)
    const full_name=sym.package.packageName+':'+sym.name
    //console.log('FVALUE FETCH:',full_name)
    omgInFetch[full_name]=true
    let xhr=new XMLHttpRequest()
    xhr.open('POST', omgBase+'" *root-path* "/" *gimme-path* "', false)
    xhr.send(full_name)
    if (xhr.status === 200) {
      eval(xhr.response)
      isFetched=true
      sym.fvalue=sym.package.symbols[sym.name].fvalue
      return sym.fvalue.apply(null,args)
    } else {
      throw new Error('Cannot fetch symbol '+sym.package.packageName+'::'+sym.name)
    }
  }
}

let omgInMakePackage=false
const omgMakePackage=(package_name)=>{
  if(!omgInMakePackage) {
    omgInMakePackage=true
    jscl.evaluateString('(DEFPACKAGE :'+package_name+' (:USE :CL :JSCL))')
    omgInMakePackage=false
    jscl.packages[package_name].omgPkg=true
  }
}

jscl.internals.intern=(name, package_name)=>{
  if(package_name && !(package_name in jscl.packages)) {
    omgMakePackage(package_name)
  }
  let sym=omgOriginalIntern(name, package_name)
  const full_name=package_name+':'+name
  if('package' in sym&&sym.package.omgPkg&&sym.value===undefined&&
     sym.fvalue===jscl.internals.unboundFunction&&!omgInFetch[full_name]) {
    sym.fvalue=omgFetchFvalue(sym)
  }
  return sym
}

jscl.omgRPC=(cmd)=>{
  let xhr=new XMLHttpRequest()
  xhr.open('POST', omgBase+'" *root-path* "/" *rpc-path* "', false)
  xhr.send(cmd)
  if (xhr.status === 200) {
    return eval(xhr.response)
  } else {
    throw new Error('Cannot call RPC')
  }
}

jscl.omgAsyncRPC=(cmd, cb)=>{
  let xhr=new XMLHttpRequest()
  xhr.open('POST', omgBase+'" *root-path* "/" *rpc-path* "', true)
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



const omgOriginalFP=jscl.packages.CL.symbols['FIND-PACKAGE'].fvalue
jscl.packages.CL.symbols['FIND-PACKAGE'].fvalue=(values,pkg)=>{
  let res=omgOriginalFP(values,pkg)
  if(!omgInMakePackage&&typeof(pkg)==='object'&&typeof(res)==='object'&&res.name==='NIL'&&res.package.packageName==='CL') {
    omgMakePackage(pkg.name)
    res=omgOriginalFP(values,pkg)
  }
  return res
}

const omgOriginalLIL=jscl.packages.JSCL.symbols['LOOKUP-IN-LEXENV'].fvalue
jscl.packages.JSCL.symbols['LOOKUP-IN-LEXENV'].fvalue=(mv,name,lexenv,ns)=>{
  let res=omgOriginalLIL(mv,name,lexenv,ns)
  if(ns.name==='FUNCTION' && 'package' in name && name.package.omgPkg && 'name' in res && res.name==='NIL') {
    const full_name=name.package.packageName+':'+name.name
    if(!omgInFetch[full_name]) {
      //console.log('LIL FETCH:',full_name,omgInFetch)
      omgInFetch[full_name]=true
      let xhr=new XMLHttpRequest()
      xhr.open('POST', omgBase+'" *root-path* "/" *gimme-path* "', false)
      xhr.send(full_name)
      if (xhr.status === 200) {
        eval(xhr.response)
        res=omgOriginalLIL(mv,name,lexenv,ns)
      } else {
        throw 'Cannot fetch symbol: '+full_name
      }
    }
  }
  return res
}

const omgOriginalGSE=jscl.packages.JSCL.symbols['!GET-SETF-EXPANSION'].fvalue
jscl.packages.JSCL.symbols['!GET-SETF-EXPANSION'].fvalue=(mv,fn)=>{
  const set_name = '(SETF_'+fn.car.name+')'
  if('car' in fn && 'name' in fn.car && 'package' in fn.car && fn.car.package.omgPkg && !(set_name in fn.car.package.symbols)) {
    //console.log('NEED FETCH:',set_name, fn.car.package.packageName)
    const full_name=fn.car.package.packageName+':'+set_name
    omgInFetch[full_name]=true
    let xhr=new XMLHttpRequest()
    xhr.open('POST', omgBase+'" *root-path* "/" *gimme-path* "', false)
    xhr.send(full_name)
    if (xhr.status === 200) {
      eval(xhr.response)
    } else {
      throw new Error('Cannot fetch set symbol '+fn.car.package.packageName+'::'+set_name)
    }
  }
  return omgOriginalGSE(mv,fn)
}


const make_conn=()=>{
  console.log('Connecting to host')
  root_ws=new WebSocket(omgWS)
  root_ws.onopen=()=>{console.log('Socket connected')}
  root_ws.onclose=(ev)=>{
    console.log('Socket closed ('+(ev.wasClean?'normally':'by error')+'), reconnecting')
    delete(root_ws)
    setTimeout(make_conn,1000)
  }
  root_ws.onerror=(err)=>{
    console.log('Socket error ('+err.message+')')
    root_ws.close()
  }
  root_ws.onmessage=function (ev) {
    //console.log('Message received: '+ev.data)
    eval(ev.data)
  }
}

"   (if *pwa-mainfest*
      (concatenate 'string "if (navigator.serviceWorker != null) {
      navigator.serviceWorker.register('" *pwa-sw-js-name* "', {scope: '" *root-path* "'}).then(function(registration) {
    console.log('Registered events at scope: ', registration.scope);
    });}")
      "")
    "if(document.readyState==='complete') {
  make_conn()
} else {
  document.addEventListener('DOMContentLoaded',()=>{
    make_conn()
  })
}})()
"))

(defparameter *current-session* nil)       ;; The current session, usually set by remote-exec
(defparameter *session-list* (make-hash-table))  ;; The store for session objects, keys are session-ids
(defparameter *current-res* nil) ;; The key for gimme-wait-list hash, denotes the place where to store result for
                                 ;;   current gimme request
(defparameter *in-rpc* nil) ;; If T -- we are in RPC call, all remote-execs must be done via takit-mechanism

(defun current-session-id ()
  (if *current-session*
      (get-id *current-session*)))

(defclass omg-session ()
  ((socket :initarg :socket
           :initform (error "socket required")
           :reader socket)
   (ses-id :initform (random-symbol |sid-length|)
           :reader get-id)
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

(defparameter *debug-session-id* nil)

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
  (let ((h (gensym)))
    `(progn
       (defparameter ,h (make-hash-table))
       (define-symbol-macro ,vr
         (gethash (current-session-id) ,h ,init)))))

(defun remote-unintern (sym)
  "Unintern the symbol within all active sessions, mandatory to reflect symbol redefinition.
   The next try to intern the symbol in browser-side will cause new symbol fetch."
  (loop for s being the hash-values of *session-list* do
       (let* ((sym-name (symbol-name sym))
              (sym-pkg (package-name (symbol-package sym)))
              (cmd (format nil "if(\"~A\" in jscl.packages && \"~A\" in jscl.packages[\"~A\"].symbols) {
            delete(omgInFetch[\"~A:~A\"])
            jscl.packages[\"~A\"].symbols[\"~A\"].fvalue=omgFetchFvalue(omgOriginalIntern(\"~A\", \"~A\"))
            jscl.packages[\"~A\"].symbols[\"~A\"].value=undefined}"
                                sym-pkg sym-name sym-pkg sym-pkg sym-name sym-pkg sym-name sym-name sym-pkg sym-pkg sym-name)))
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


(defun compile-to-js (code pkg &optional reskey)
  "Return JS for the code, pkg is current package for compilation context.
   The reskey is the *gimme-wait-list* key, the place to store compilation result."
  (let* ((*package* pkg)
         (c1 (write-to-string
               `(let ((*package* (find-package (intern ,(package-name pkg) :keyword))))
                  ,code)))
         (code (if *local-compile*
                   (jscl::with-compilation-environment
                     (jscl::compile-toplevel (jscl::ls-read-from-string c1) t t))
                   (write-to-string code)))
         (rcode (replace-all (replace-all (replace-all (replace-all code "\\" "\\\\") (string #\linefeed) "\\n") (string #\return) "\\\\r") "\"" "\\\""))
         (res (if *local-compile*
                  (concatenate 'string "jscl.internals.lisp_to_js(jscl.internals.globalEval(\"" rcode "\"))")
                  (concatenate 'string "jscl.evaluateString(\"" rcode "\")"))))
     (if reskey
         (let ((sem (car (gethash-lock reskey *gimme-wait-list*))))
           (setf (gethash-lock reskey *gimme-wait-list*) res)
           (signal-semaphore sem)))
     res))

(defun takit (key res)
  "The handler for takit-requests. This requests are used to return macro expansion results from browser-side"
  (let* ((sem-dat-sym (gethash-lock key *takit-wait-list*)))
    (if sem-dat-sym
      (let ((newsem (make-semaphore)))
        (setf (gethash-lock key *takit-wait-list*)
              (let ((*package* (symbol-package (caddr sem-dat-sym)))
                    (*read-eval* nil))
                (read-from-string res)))
        (setf (gethash-lock key *gimme-wait-list*) (list newsem (get-universal-time) (caddr sem-dat-sym)))
        (signal-semaphore (car sem-dat-sym))
        (wait-on-semaphore newsem)
        `(200 (:content-type "text/plain; charset=utf-8") (,(gethash-lock key *gimme-wait-list*))))
      `(404 (:content-type "text/plain; charset=utf-8") ("")))))

(defun gimme (sym)
  "The handler for gimme-requests, which are used to request unknown symbols from the server-side."
  (let ((datp (gethash-lock sym *exportable-expressions*)))
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
          (setf (gethash-lock key *gimme-wait-list*) (list sem (get-universal-time) sym))
          (bt:make-thread
             (lambda ()
               (compile-to-js dat (symbol-package sym) key))
             :initial-bindings `((*current-res* . ',key)))
          (wait-on-semaphore sem)
          (let ((res (gethash-lock key *gimme-wait-list*)))
             (remhash key *gimme-wait-list*)
            `(200 (:content-type "text/plain; charset=utf-8") (,res))))
       `(404 (:content-type "text/plain; charset=utf-8") ("")))))

(defun rpc-wrapper (op args pkg)
  "The wrapper for RPC requests, used to allow call browser-side functions from RPC funcs."
   (let* ((sem (make-semaphore))
          (key (random-key *gimme-wait-list* |sid-length|)))
      (setf (gethash-lock key *gimme-wait-list*) (list sem (get-universal-time) (intern "omg-rpc-symbol" pkg)))
      (bt:make-thread
         (lambda ()
           (compile-to-js `(read-from-string ,(write-to-string (apply op args)))
                          pkg
                          key))
         :initial-bindings `((*current-res* . ',key)
                             (*current-session* . ,*current-session*)
                             (*in-rpc* . t)))
      (wait-on-semaphore sem)
      (let ((res (gethash-lock key *gimme-wait-list*)))
         (remhash key *gimme-wait-list*)
        `(200 (:content-type "text/plain; charset=utf-8") (,res)))))


(defun exec-remote-macro (name args)
  "Execute in a browser a code of the macro with the specific name and argments.
   Called by JSCL compiler while compling lisp code to JS. We have to execute macros on the
   browser-side, because all side-effects, produced by macros, must have a place in the browser."
  (if *current-res*
      (let* ((sem-tim-sym (gethash-lock *current-res* *gimme-wait-list*))
             (sem (car sem-tim-sym))
             (sym (caddr sem-tim-sym))
             (takit-sem (make-semaphore))
             (mcod (compile-to-js
                      `(write-to-string (apply (lambda ,@(cddr (gethash-lock name *exportable-expressions*))) ',args))
                       (symbol-package sym))))
        (setf (gethash-lock *current-res* *takit-wait-list*) (list takit-sem (get-universal-time) sym))
        (setf (gethash-lock *current-res* *gimme-wait-list*)
              (format nil (concatenate 'string "xhr=new XMLHttpRequest();xhr.open('POST','" *root-path* "/" *takit-path* "',false);"
                                               "xhr.send('~A'+(~A));if(xhr.status===200){eval(xhr.response);}else"
                                               "{throw new Error('Cannot fetch symbol (takit fails).');}")
                          (symbol-name *current-res*)
                          mcod))
        (signal-semaphore sem)
        (wait-on-semaphore takit-sem)
        (let ((macro-res (gethash-lock *current-res* *takit-wait-list*)))
          (remhash *current-res* *takit-wait-list*)
          macro-res))
      (remote-exec `(apply (lambda ,@(cddr (gethash-lock name *exportable-expressions*))) ',args))))


(defun remote-exec (cmd &optional nowait)
  "Execute the code on the browser-side. If the *current-session* set, the code will be executed
   within the specific session, otherwise, the code will be executed in all sessions and all the return
   values are returned as a list. If the nowait is T, the function will retrurn NIL immediately, without waiting
   result from the remote side."
  (if *in-rpc*
    (let* ((sem-tim-sym (gethash-lock *current-res* *gimme-wait-list*))
           (sem (car sem-tim-sym))
           (takit-sem (make-semaphore))
           (mcod (compile-to-js `(write-to-string ,cmd) *package*)))
      (setf (gethash-lock *current-res* *takit-wait-list*) (list takit-sem (get-universal-time) (caddr sem-tim-sym)))
      (setf (gethash-lock *current-res* *gimme-wait-list*)
            (format nil (concatenate 'string "xhr=new XMLHttpRequest();xhr.open('POST','" *root-path* "/" *takit-path* "',false);"
                                             "xhr.send('~A'+(~A));if(xhr.status===200){eval(xhr.response);}else"
                                             "{throw new Error('Cannot fetch symbol (takit fails).');}")
                        (symbol-name *current-res*)
                        mcod))
      (signal-semaphore sem)
      (wait-on-semaphore takit-sem)
      (let ((res (gethash-lock *current-res* *takit-wait-list*)))
        (remhash *current-res* *takit-wait-list*)
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
                                      (format nil "~A;root_ws.send((\"~~~A\"+~A));"
                                          pkg-hook
                                          (symbol-name key)
                                          rcmd))))
                     (if (equal sock-state :open)
                       (progn
                         (if (not nowait) (setf (gethash-lock key wlist) `(,(current-thread) ,sem nil)))
                         (send-text sock jscmd)
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

(defparameter *boot-functions* nil)

(defun add-to-boot (f)
  (push f *boot-functions*))

(defun rm-from-boot (f)
  (delete f *boot-functions*))

(defun boot-f ()
  "The boot code, will be executed on the browser-side just after the socket is connected."
  (remote-exec `(defparameter *session-id* ',(get-id *current-session*)) :nowait)
  (map nil (lambda (f) (remote-exec f :nowait)) *boot-functions*))

(defun make-ws (env)
  "Return the websocket for the new session. Also, creates the session object."
  (let* ((ws (websocket-driver.server:make-server env))
         (ses (make-instance 'omg-session :socket ws))
         (sid (get-id ses)))
    (setf (gethash-lock sid *session-list*) ses)
    (on :open ws
      (lambda ()
          (bt:make-thread
            (lambda ()
              (let ((*current-session* ses)
                    (*package* (find-package :omg)))
                (boot-f)))
            :name (concatenate 'string (symbol-name sid) "-BOOT"))))
    (on :error ws
      (lambda (error)
        (format t "WS error: ~S~%" error)))
    (on :message ws
      (lambda (msg)
        (let* ((m (subseq msg 0 1))
               (rid (intern (subseq msg 1 (+ |sid-length| 1)) :omg))
               (val (subseq msg (+ |sid-length| 1))))
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

(defparameter *pwa-sw-js* "
const bc = new BroadcastChannel('omg_service_worker');

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
  bc.postMessage({type:'fetch', uri:e.request.url})
  return fetch(e.request.url)
})
")

(defun serv (env)
  (let ((uri (getf env :REQUEST-URI)))
    (cond ((equal uri (concatenate 'string *root-path* "/" *pwa-path* "/manifest.json"))
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
          ((equal uri (concatenate 'string *root-path* "/" *js-path*))
           `(200
               (:content-type "text/javascript; charset=utf-8")
               (,(get-main-js))))
          ((equal uri (concatenate 'string *root-path* "/" *html-path*))
           `(200 (:content-type "text/html; charset=utf-8") (,(get-root-html))))
          ((equal uri (concatenate 'string *root-path* "/" *rpc-path*))
           (let* ((cmd (read-from-string (get-str-from (getf env :raw-body) (getf env :content-length))))
                  (pkg (find-package (car cmd)))
                  (op (intern (symbol-name (cadr cmd)) pkg))
                  (args (caddr cmd))
                  (*current-session* (find-session (intern (symbol-name (cadddr cmd)) :omg))))
              (if (gethash-lock op *rpc-functions*)
                (rpc-wrapper op args pkg)
               `(404 (:content-type "text/plain; charset=utf-8") ("")))))
          ((equal uri (concatenate 'string *root-path* "/" *gimme-path*))
           (let* ((str (get-str-from (getf env :raw-body) (getf env :content-length)))
                  (pos (position #\: str))
                  (pkg (find-package (subseq str 0 pos))))
             (gimme (intern (subseq str (+ 1 pos)) pkg))))
          ((equal uri (concatenate 'string *root-path* "/" *takit-path*))
           (let* ((str (get-str-from (getf env :raw-body) (getf env :content-length))))
             (takit (intern (subseq str 0 |sid-length|) :omg)
                    (subseq str |sid-length|))))
          ((equal uri (concatenate 'string *root-path* "/" *ws-path*))
           (let ((ws (make-ws env)))
             (lambda (responder)
               (declare (ignorable responder))
               (start-connection ws))))
          (t '(404 (:content-type "text/plain") ("File not found"))))))

(defparameter *serv* nil)

(defparameter *last-args* nil)

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
                        ,@*last-args*))))

(defun kill-server () (if *serv* (clack::stop *serv*)))

(defun restart-server (&rest args)
  (progn
    (kill-server)
    (apply #'start-server args)))
