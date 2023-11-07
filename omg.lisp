(defpackage :omg
  (:use cl clack websocket-driver bordeaux-threads trivial-utf-8)
  (:import-from :event-emitter #:emit)
  (:export add-to-boot       ;; add a code to boot sequence
           set-boot          ;; set boot code
           add-to-root-html  ;; add a text to html body
           add-to-root-html-head ;; add a text to html head
           add-serve-path    ;; add custom handler for specific URI (relative to *root-path*)
           set-root-html     ;; set html body
           set-root-head     ;; set html head
           rm-from-boot      ;; remove a code from boot sequence
           start-server      ;; start a http(s)-server
           kill-server       ;; kill a http(s)-server
           restart-server    ;; restart a http(s)-server
           defclass-f        ;; define a browser-side class
           defun-f           ;; define a browser-side function
           defmacro-f        ;; define a browser-side macro
           def-local-macro-f ;; define a browser-side macro, but executing locally
           defmethod-f       ;; define a browser-side method
           defmethod-r       ;; define a rpc method for both-side classess
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
(defvar *service-worker-path* "sw") ;; path to the service worker code
(defvar *web-worker-path* "ww") ;; path to the web worker code
(defvar *port* 7500) ;; default server port

(defvar *use-wss* nil) ;; if T -- use wss:// protocol for websocket
(defvar *ssl-key* nil) ;; SSL key path
(defvar *ssl-cert* nil) ;; SSL cert path

(defvar *giant-hash-lock* nil) ;; A giant lock to make hashes thread safe

(defvar *omg-thread-list* nil)

(defun gethash-lock (key hash)
  (apply #'values
        (if *giant-hash-lock*
            (bt:with-lock-held (*giant-hash-lock*)
              (multiple-value-list (gethash key hash)))
            (multiple-value-list (gethash key hash)))))

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

(defun register-rpc (name &optional (def 'defun))
  (remhash name *exportable-expressions*)
  (setf (gethash-lock name *rpc-functions*) t)
  (setf (gethash-lock name *exportable-expressions*)
       `(,def ,name (&rest argl)
          (funcall (jscl::oget (jscl::%js-vref "self") "OMG" "RPC")
                   (let ((*package* (find-package ,(package-name *package*))))
                     (jscl::omg-write-to-string (intern (jscl::oget (jscl::%js-vref "self") "OMG" "session_id") :OMG)
                                                (intern ,(package-name *package*))
                                                (list ',name argl)))))))


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

(defparameter *disable-remote-macro* nil)

(defmacro make-def-macro-f (op1)
  "Just a macro to generate macros for f-functions and f-macros definintions (defun-f, defmacro-f, etc...)"
  (let ((op (if (equal op1 'def-local-macro) 'defmacro op1)))
    `(defmacro ,(read-from-string (format nil "~a-f" op1)) (name args &rest body)
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
                                    (lambda (,ar)
                                      ; (exec-remote-macro ',name ,ar)))))))
                                      ,(if (or *disable-remote-macro*
                                               (equal ',op1 'def-local-macro))
                                           `(apply (lambda ,args ,@body) ,ar)
                                           `(exec-remote-macro ',name ,ar))))))))
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
                      (labels ((replace-method (cls)
                                 (setf (gethash-lock cls *exported-classes-methods*)
                                       (cons ',f-form
                                             (remove-if
                                               (lambda (rec)
                                                 (and (equal 'defmethod (car rec))
                                                      (equal ',name (cadr rec))
                                                      (or (and (listp (caddr rec))
                                                               (tree-equal (caddr rec) ',clos-args))
                                                          (and (symbolp (caddr rec))
                                                               (tree-equal (cons (caddr rec) (cadddr rec))
                                                                           ',clos-args)))))
                                               (gethash-lock cls *exported-classes-methods*))))))
                        (if (and classes (is-system-pkg (symbol-package ',name)))
                            (progn
                              (jscl::with-compilation-environment  ;; We need to compile specific methods locally to proper setup jscl clos environment
                                (jscl::compile-toplevel ',f-form))
                              (map nil #'replace-method classes)
                              (remote-update-methods ',name classes)))
                        (if (intersection (gethash-lock ',name *accessor-classes*)
                                          (concatenate 'list classes (mapcan #'get-all-sup classes)))
                            (map nil #'replace-method classes)))))
                  (t `(progn
                        (setf (gethash-lock ',ex-sym *exportable-expressions*) ',f-form)
                        (defmacro ,name (&rest args)
                           (if *in-f-macro*
                              `',(cons ',name (mapcar #'f-eval args))
                              (let ((*in-f-macro* t)) ;; Evaluate all -f functions and macros on the browser-side
                                 `(remote-exec ',(cons ',name (mapcar #'f-eval args))))))
                        (if (not (find ',name *exported-function-names*))
                            (push ',name *exported-function-names*)))))
           ',name)))))

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

(defvar *classes-f-slots* (make-hash-table))
(defvar *classes-f-superclasses* (make-hash-table))

(defmacro defclass-f (name sup slots)
  (let ((f-form `(defclass ,name ,sup ,slots))
        (tmp (gensym)))
    (setf (gethash name *classes-f-slots*) slots)
    (setf (gethash name *classes-f-superclasses*) sup)
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
(make-def-macro-f def-local-macro) ;; defmacro-f
(make-def-macro-f defmethod) ;; defmethod-f
(make-def-macro-f defgeneric) ;; defgeneric-f

(make-var-macro-f defvar)       ;; defvar-f
(make-var-macro-f defparameter) ;; defparameter-f
(make-var-macro-f defconstant)  ;; defconstant-f

(defvar *extra-html* "")
(defvar *extra-head* "")

(defun get-root-html ()
  "Return a somple HTML with JS injected."
  (concatenate 'string "<html><head>"
               *extra-head*
               "<script src='"
               *root-path* *js-path*
               "' type='text/javascript'></script></head><body>"
               *extra-html*
               "</body></html>"))

(defun add-to-root-html (html)
  (setf *extra-html* (concatenate 'string *extra-html* html)))

(defun add-to-root-html-head (html)
  (setf *extra-head* (concatenate 'string *extra-head* html)))

(defun set-root-html (html)
  (setf *extra-html* html))

(defun set-root-head (html)
  (setf *extra-head* html))

(defun get-main-js ()
  "Return the JS code, including JSCL and OMG parts glued."
  (concatenate 'string "(()=>{" jscl::*jscl-js*
          "
var OMG = Object.create(null)
self.OMG=OMG

OMG.session_id=false

OMG.PersistentCache=false
OMG.inWorker=(typeof window==='undefined')
OMG.inServiceWorker=(OMG.inWorker&&(typeof XMLHttpRequest==='undefined'))

OMG.objectRegistry={}

OMG.get_session_id=()=>{
  if(OMG.session_id) return 'OMG::'+OMG.session_id
  return 'OMG::NO-SESSION'
}

OMG.find_object=(id)=>{
  if(id in OMG.objectRegistry) return OMG.objectRegistry[id]
  return false
}

OMG.register_object=(obj)=>{
  const find_rnd_key=()=>{
    const id=parseInt((Math.random() + 1).toString().substring(3)+'0')
    if(id in OMG.objectRegistry) find_rnd_key()
    return id
  }
  const obj_id=('omgObjId' in obj)?obj.omgObjId:find_rnd_key()
  obj.omgObjId=obj_id
  OMG.objectRegistry[obj_id]=obj
  return obj_id
}

OMG.MOPr=jscl.packages.JSCL.symbols['MOP-OBJECT-PRINTER'].fvalue
OMG.WrStr=jscl.packages.CL.symbols['WRITE-STRING'].fvalue
OMG.in_omg_write=false
jscl.packages.JSCL.symbols['MOP-OBJECT-PRINTER'].fvalue=(values,form,stream)=>{
  if(OMG.in_omg_write) {
    const obj_id=OMG.register_object(form)
    return OMG.WrStr(values,jscl.internals.js_to_lisp('#'+obj_id+'Ё'),stream)
  }
  return OMG.MOPr(values,form,stream)
}

OMG.WRAux=jscl.packages.JSCL.symbols['WRITE-AUX'].fvalue
jscl.packages.JSCL.symbols['WRITE-AUX'].fvalue=(values,form,stream,known_objects,object_ids)=>{
  if(OMG.in_omg_write) {
    const is_js=jscl.packages.JSCL.symbols['JS-OBJECT-P'].fvalue(null,form)
    if(is_js && typeof(is_js)==='object' && 'name' in is_js && is_js.name==='T') {
      const obj_id=OMG.register_object(form)
      return OMG.WrStr(values,jscl.internals.js_to_lisp('#'+obj_id+'Ё'),stream)
    }
  }
  return OMG.WRAux(values,form,stream,known_objects,object_ids)
}


if(!OMG.inWorker) {
  OMG.URL=new URL(document.currentScript.src)
  OMG.Path=OMG.URL.pathname.replace(/\\/[^\\\/]+$/,'')
  OMG.HostPath=(OMG.URL.username?(OMG.URL.username+
                                   (OMG.URL.password?(':'+OMG.URL.password):'')+'@'):'')+
                                 OMG.URL.host+
                                 OMG.Path
  OMG.Base=OMG.URL.protocol+'//'+OMG.HostPath
  OMG.WS='" (if (use-wss-p) "wss://" "ws://") "'+OMG.HostPath+'" *root-path* *ws-path* "'
}

jscl.packages['COMMON-LISP-USER'] = jscl.packages.CL;

if(!OMG.inServiceWorker) {
  OMG.root_ws=undefined
  OMG.InFetch={}

  OMG.OriginalSymbolValue=jscl.internals.symbolValue
  jscl.internals.symbolValue=(symbol)=>{
    if(symbol.package) {
      const full_name=symbol.package.packageName+'::'+symbol.name
      if(symbol.value===undefined&&symbol.package.omgPkg&&!OMG.InFetch[full_name]) {
        //console.log('SYMVALUE FETCH:', full_name)
        if(OMG.symbolValueFetchHandler) return OMG.symbolValueFetchHandler(symbol)
        OMG.InFetch[full_name]=true
        let xhr=new XMLHttpRequest()
        xhr.open('POST', OMG.Base+'" *root-path* *gimme-path* "', false)
        xhr.send(OMG.get_session_id()+' '+
                 jscl.packages.CL.symbols['*PACKAGE*'].value.packageName+
                 ' '+full_name)
        if (xhr.status === 200) {
          eval(xhr.response)
        } else {
          throw new Error('Cannot fetch symbol '+symbol.name)
        }
      }
    }
    return OMG.OriginalSymbolValue(symbol)
  }

  OMG.OriginalIntern=jscl.internals.intern

  OMG.Fetch=(sym)=>{
    const full_name=sym.package.packageName+'::'+sym.name
    //console.log('FVALUE FETCH:',full_name)
    if(!OMG.InFetch[full_name]) {
      OMG.InFetch[full_name]=true
      let xhr=new XMLHttpRequest()
      xhr.open('POST', OMG.Base+'" *root-path* *gimme-path* "', false)
      xhr.send(OMG.get_session_id()+' '+
               jscl.packages.CL.symbols['*PACKAGE*'].value.packageName+
               ' '+full_name)
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
    const ffv=(...args)=>{
      if(isFetched||sym.fvalue!=ffv) return sym.fvalue.apply(null,args)
      OMG.Fetch(sym)
      isFetched=true
      sym.fvalue=sym.package.symbols[sym.name].fvalue
      return sym.fvalue.apply(null,args)
    }
    return ffv
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
    const full_name=package_name+'::'+name
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
    if(!OMG.InMakePackage&&typeof(res)==='object'&&res.name==='NIL'&&res.package.packageName==='CL') {
      OMG.MakePackage(jscl.internals.lisp_to_js(pkg))
      res=OMG.OriginalFP(values,pkg )
    }
    return res
  }

  OMG.disableLIL=false

  OMG.OriginalLIL=jscl.packages.JSCL.symbols['LOOKUP-IN-LEXENV'].fvalue
  jscl.packages.JSCL.symbols['LOOKUP-IN-LEXENV'].fvalue=(mv,name,lexenv,ns)=>{
    let res=OMG.OriginalLIL(mv,name,lexenv,ns)
    if(!OMG.disableLIL && ns.name==='FUNCTION' && 'package' in name && name.package.omgPkg && 'name' in res && res.name==='NIL') {
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
      const full_name=fn.car.package.packageName+'::'+set_name
      OMG.InFetch[full_name]=true
      let xhr=new XMLHttpRequest()
      xhr.open('POST', OMG.Base+'" *root-path* *gimme-path* "', false)
      xhr.send(OMG.get_session_id()+' '+
               jscl.packages.CL.symbols['*PACKAGE*'].value.packageName+
               ' '+full_name)
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

  OMG.make_conn=()=>{
    console.log('Connecting to host')
    OMG.root_ws=new WebSocket(OMG.WS)
    OMG.root_ws.onopen=()=>{
      console.log('Socket connected')
      if(OMG.session_id) OMG.root_ws.send('!!SESID:'+OMG.session_id)
    }
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

  if(!OMG.inWorker) {
    if(document.readyState==='complete') {
      OMG.make_conn()
    } else {
      document.addEventListener('DOMContentLoaded',()=>{
        OMG.make_conn()
      })
    }
  }
}
})()
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
              :reader wait-list)
   (disconnected-at :initform nil
                    :accessor disconnected-at)
   (session-ws :initarg :ws
               :initform (error "Session WS required")
               :accessor session-ws))
  (:documentation
    "The session object, holding the session socket, the wait-list holding semaphores and storing result data from remote-exec calls"))

(defvar *remote-objects* (make-hash-table))

(defclass remote-object ()
  ((session :initarg :session
            :initform nil
            :accessor session)
   (id :initarg :id
       :initform (error "ID is required")
       :accessor id)))

(defmethod initialize-instance :after ((obj remote-object) &key &allow-other-keys)
  (setf (session obj) *current-session*))

(defparameter *in-omg-writer* nil)

(defmethod print-object ((obj remote-object) s)
  (if *in-omg-writer*
      (write `((jscl::oget (jscl::%js-vref "self") "OMG" "find_object") ,(id obj)) :stream s)
      (format s "#<REMOTE-JS-OBJECT ~A ~A>" (if (session obj) (get-id (session obj)) nil) (id obj))))

(defun omg-data-to-compile-form (form)
  (cond ((listp form)
         (cons 'list (mapcar #'omg-data-to-compile-form form)))
        ((symbolp form)
         (list 'quote form))
        (t form)))

(defmethod print-m-slots-init (obj nvar s)
  nil)

(defvar *m-initialized* nil)
(defvar *m-classess* (list))

(defun scope-session (obj scope)
  (if (and (equal scope :auto)
           (slot-boundp obj 'scope)
           (not (slot-value obj 'scope)))
      nil
      *current-session*))

(defvar *m-scopes* (make-hash-table))
(defvar *m-scope-funcs* (make-hash-table))

(defmacro defclass-m (name superclasses slots &rest options)
  (labels ((is-f-slot (slt)
             (and (getf (cdr slt) :browser-side)
                  (not (getf (cdr slt) :mirrored))))
           (is-m-slot (slt)
             (getf (cdr slt) :mirrored))
           (is-o-slot (slt)
             (and (not (is-f-slot slt))
                  (not (is-m-slot slt))))
           (clr-bs (slt)
             (remf (cdr slt) :browser-side)
             (remf (cdr slt) :mirrored))
           (fcls (cls) (find-class cls nil)))
    (let* ((f-super (remove-if
                      (lambda (cls)
                        (and (fcls cls)
                             (not (position cls *m-classess*))))
                      superclasses))
           (o-super (remove-if-not #'fcls superclasses))
           (f-slots (remove-if-not #'is-f-slot slots))
           (m-slots (remove-if-not #'is-m-slot slots))
           (o-slots (remove-if-not #'is-o-slot slots))
           (all-f-slots (labels ((get-f-slts (cls)
                                   (append (gethash cls *classes-f-slots*)
                                           (mapcan #'get-f-slts (gethash cls *classes-f-superclasses*)))))
                          (remove-duplicates (append f-slots (get-f-slts name)) :key #'car)))
           (f-args (remove-if #'null (mapcar
                                       (lambda (slt)
                                         (getf (cdr slt) :initarg))
                                       all-f-slots)))
           (has-m-super (notany #'null (mapcar (lambda (cls) (position cls *m-classess*)) superclasses)))
           (class-scope (getf options :scope
                          (let ((vrs (remove-duplicates (remove-if #'null (mapcar (lambda (cls)
                                                                                    (gethash-lock cls *m-scopes*))
                                                                                  o-super)))))
                            (if vrs
                                (if (cdr vrs)
                                    (error (format nil "No default scope defined for m-class ~A and multiple different scopes found for superclasses" name))
                                    (car vrs))
                                :auto)))))

      (setf (gethash name *classes-f-superclasses*) f-super) ;;  defclass-f macros may not be evaluated immediately
      (push name *m-classess*)
      (map nil #'clr-bs f-slots)
      (map nil #'clr-bs o-slots)
      (map nil #'clr-bs m-slots)
      `(progn
         (defclass ,name ,o-super
           (,@o-slots
            ,@m-slots
            (omg-id)
            (f-init)
            (scope)))

         (defclass-f ,name (,@(if (not has-m-super) '(mirrored-object))
                            ,@f-super)
           (,@f-slots
            ,@m-slots))

        (setf (gethash-lock ',name *m-scopes*) ,class-scope)
        (setf (gethash-lock ',name *m-scope-funcs*)
              (lambda (obj)
                ,(cond ((functionp class-scope)
                        `(funcall ,class-scope obj))
                       ((or (equal :session class-scope)
                            (equal :auto class-scope))
                        `(scope-session obj ,class-scope))
                       (t (error (format nil "Invalid m-class scope: ~A" class-scope))))))

        (defmethod initialize-instance :around ((obj ,name) &rest args &key &allow-other-keys)
          ,(if (equal class-scope :session)
               `(if (equal *current-session* nil)
                    (error "Trying to create instance with :sesson scope while not in session")))
          (setf (slot-value obj 'scope) (funcall (gethash-lock ',name *m-scope-funcs*) obj))
          (let ((id (+ 1 (* 10 (random 100000000000000)))))
            (setf (gethash-lock id *remote-objects*) obj)
            (setf (slot-value obj 'omg-id) id)
            (setf (slot-value obj 'f-init)
                  (loop for arg in args by #'cddr
                        when (position arg ',f-args)
                        append (list arg (getf args arg)))))
          (call-next-method))

        (defmethod-f sync-slot ((obj ,name) slot)
          (setf (slot-value obj slot) (sync-slot-r obj slot)))
        ,@(mapcar
            (lambda (slt)
              `(progn
                 (defmethod-r sync-slot-r ((obj ,name) (slot (eql ',(car slt))))
                   (slot-value obj slot))
                 (defmethod sync-slot ((obj ,name) (slot (eql ',(car slt))))
                   (let ((slt-name ',(car slt))
                         (scf (gethash-lock ',name *m-scope-funcs*)))
                     (loop for *current-session* being the hash-values of *session-list* do
                       (if (equal (slot-value obj 'scope) (funcall scf obj))
                           (remote-exec `(sync-slot ,obj ',slt-name) :nowait)))))))
            m-slots)

        (defmethod print-m-slots-init :around ((obj ,name) nvar s)
           `(,@(if (next-method-p)
                   (call-next-method))
             ,@(mapcar
                 (lambda (slt)
                   `(setf (slot-value ,nvar ',(car slt))
                          ,(omg-data-to-compile-form (slot-value obj (car slt)))))
                 ',m-slots)))

        (defmethod print-object ((obj ,name) s)
          (if *in-omg-writer*
              (let ((name ',name)
                    (obj-id (slot-value obj 'omg-id))
                    (f-init (slot-value obj 'f-init)))
                (if (equal (slot-value obj 'scope) (funcall (gethash-lock ',name *m-scope-funcs*) obj))
                    (write `(let* ((ovar ((jscl::oget (jscl::%js-vref "self") "OMG" "find_object") ,obj-id)))
                              (if ovar
                                  ovar
                                  (let ((nvar (make-instance ',name :omg-internal-id ,obj-id ,@f-init)))
                                    ,@(print-m-slots-init obj 'nvar s)
                                    nvar)))
                           :stream s)
                    (error "Trying to send an instance to invalid scope")))
              (format s "#<~A ~A>" ',name (slot-value obj 'omg-id))))))))

(defmacro defmethod-r (name args &rest body)
  `(progn
     (defmethod ,name ,args ,@body)
     (register-rpc ',name 'defmethod)))


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

(defun omg-write-to-string (&rest forms)
  (let ((*in-omg-writer* t)
        (*print-circle* t))
    (with-output-to-string (s)
      (map nil (lambda (f) (write f :stream s)) forms))))

(defun compile-to-js (code pkg)
  "Return JS for the code, pkg is current package for compilation context."
  (let* ((*package* pkg)
         (c1 (omg-write-to-string `(let ((*package* (find-package ,(package-name pkg)))
                                         (*print-circle* t))
                                     ,code)))
         (code (if *local-compile*
                   (jscl::with-compilation-environment
                      (let ((jscl::*macroexpander-cache* (make-hash-table :test #'eq))) ;; workaround for JSCL macroexpander race condition
                        (jscl::compile-toplevel
                          (let ((jscl::*labelled-objects* nil))   ;; workaround for JSCL reader race condition
                            (jscl::ls-read-from-string c1))
                          t t)))
                   c1))
         (rcode (if *local-compile*
                    (replace-all (replace-all (replace-all (replace-all code "\\" "\\\\") (string #\linefeed) "\\n") (string #\return) "\\\\r") "\"" "\\\"")
                    (replace-all (replace-all code (string #\linefeed) " ") (string #\return) " ")))
         (res (if *local-compile*
                  (concatenate 'string "jscl.internals.lisp_to_js(jscl.internals.globalEval(\"" rcode "\"))")
                  (concatenate 'string "jscl.evaluateString(" rcode ")"))))
     res))

(defun omg-read (s)
  (let ((*readtable* (copy-readtable))
        (*read-eval* nil))
    (set-dispatch-macro-character #\# #\Ё
      (lambda (st subch infix)
        (declare (ignore subch st))
        (let ((fnd (gethash-lock infix *remote-objects*)))
          (if fnd
              fnd
              (setf (gethash-lock infix *remote-objects*) (make-instance 'remote-object :id infix :session *current-session*))))))
    (read s)))



(defun omg-read-from-string (s)
  (with-input-from-string (st s)
    (omg-read st)))

(defun takit (key res)
  "The handler for takit-requests. This requests are used to return macro expansion results from browser-side"
  (let* ((sem-dat-sym (gethash-lock key *takit-wait-list*)))
    (if (and sem-dat-sym (assoc :sem sem-dat-sym))
      (let ((newsem (make-semaphore)))
        (setf (gethash-lock key *takit-wait-list*)
              `((:result . ,res)
                ,(assoc :time sem-dat-sym)))
        (setf (gethash-lock key *gimme-wait-list*) `((:sem . ,newsem) (:time . ,(get-universal-time)) ,(assoc :symbol sem-dat-sym)))
        (signal-semaphore (cdr (assoc :sem sem-dat-sym)))
        (wait-on-semaphore newsem)
        `(200 (:content-type "text/plain; charset=utf-8") (,(cdr (assoc :result (gethash-lock key *gimme-wait-list*))))))
      `(404 (:content-type "text/plain; charset=utf-8") ("")))))

(defun gimme (sym)
  "The handler for gimme-requests, which are used to request unknown symbols from the server-side."
  (multiple-value-bind (datp fnd) (gethash-lock sym *exportable-expressions*)
    (if fnd
        (let* ((auto-funcs (if (and (listp datp)
                                    (equal 'defclass (car datp)))
                               (remove-duplicates
                                 (gethash-lock sym *exported-classes-methods*)))))
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
                         :initial-bindings `((*current-res* . ',key)
                                             (*current-session* . ,*current-session*)))
                      *omg-thread-list*)
                (wait-on-semaphore sem)
                (let ((res (cdr (assoc :result (gethash-lock key *gimme-wait-list*)))))
                   (remhash key *gimme-wait-list*)
                   (multiple-value-bind (x fnd) (gethash key *takit-wait-list*)
                     (declare (ignore x))
                     (if (not fnd) (unintern key)))
                  `(200 (:content-type "text/plain; charset=utf-8") (,res))))))
        `(404 (:content-type "text/plain; charset=utf-8") ("")))))

(defun rpc-wrapper (op args pkg)
  "The wrapper for RPC requests, used to allow call browser-side functions from RPC funcs."
   (let* ((sem (make-semaphore))
          (key (random-key *gimme-wait-list* |sid-length|)))
      (setf (gethash-lock key *gimme-wait-list*) `((:sem . ,sem) (:time . ,(get-universal-time)) (:symbol . ,(intern "omg-rpc-symbol" pkg))))
      (push (bt:make-thread
              (lambda ()
                (put-wl-result (compile-to-js (omg-data-to-compile-form (apply op args))
                                              pkg)
                               key))
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
      (let* ((cur-res *current-res*)
             (sem-tim-sym (gethash-lock cur-res *gimme-wait-list*))
             (sem (cdr (assoc :sem sem-tim-sym)))
             (sym (cdr (assoc :symbol sem-tim-sym)))
             (takit-sem (make-semaphore))
             (mcod (compile-to-js
                      `(jscl::omg-write-to-string (apply (lambda ,@(cddr (gethash-lock name *exportable-expressions*))) ',args))
                       (symbol-package sym))))
        (setf (gethash-lock cur-res *takit-wait-list*) `((:sem . ,takit-sem) (:time . ,(get-universal-time)) (:symbol . ,sym)))
        (setf (gethash-lock cur-res *gimme-wait-list*)
              `((:result . ,(format nil (concatenate 'string "xhr=new XMLHttpRequest();xhr.open('POST','" *root-path* *takit-path* "',false);"
                                                             "xhr.send(OMG.get_session_id()+' '+"
                                                                      "\"" (package-name (symbol-package sym)) "\"+"
                                                                      "' OMG::~A '+(~A));if(xhr.status===200){eval(xhr.response);}else"
                                                             "{throw new Error('Cannot fetch symbol (takit fails).');}")
                                        (symbol-name cur-res)
                                        mcod))
                (assoc :time sem-tim-sym)))
        (signal-semaphore sem)
        (wait-on-semaphore takit-sem)
        (let ((macro-res (cdr (assoc :result (gethash-lock cur-res *takit-wait-list*)))))
          (remhash cur-res *takit-wait-list*)
          (unintern cur-res)
          macro-res))
      (remote-exec `(apply (lambda ,@(cddr (gethash-lock name *exportable-expressions*))) ',args))))


(defun remote-exec (cmd &optional nowait)
  "Execute the code on the browser-side. If the *current-session* set, the code will be executed
   within the specific session, otherwise, the code will be executed in all sessions and all the return
   values are returned as a list. If the nowait is T, the function will retrurn NIL immediately, without waiting
   result from the remote side."
  (if *in-rpc*
    (let* ((cur-res *current-res*)
           (sem-tim-sym (gethash-lock *current-res* *gimme-wait-list*))
           (sem (cdr (assoc :sem sem-tim-sym)))
           (takit-sem (make-semaphore))
           (mcod (compile-to-js (if nowait
                                    cmd
                                    `(jscl::omg-write-to-string ,cmd))
                                *package*)))
      (setf (gethash-lock cur-res *takit-wait-list*) `((:sem . ,takit-sem) (:time . ,(get-universal-time)) ,(assoc :symbol sem-tim-sym)))
      (setf (gethash-lock cur-res *gimme-wait-list*)
            `((:result . ,(format nil (concatenate 'string "xhr=new XMLHttpRequest();xhr.open('POST','" *root-path* *takit-path* "',false);"
                                                           "xhr.send(OMG.get_session_id()+' '+"
                                                                    "jscl.packages.CL.symbols['*PACKAGE*'].value.packageName+"
                                                                    "' OMG::~A '+(~A));if(xhr.status===200){eval(xhr.response);}else"
                                                           "{throw new Error('Cannot fetch symbol (takit fails).');}")
                                      (symbol-name cur-res)
                                      mcod))
              ,(assoc :time sem-tim-sym)))
      (signal-semaphore sem)
      (wait-on-semaphore takit-sem)
      (let ((res (cdr (assoc :result (gethash-lock cur-res *takit-wait-list*)))))
        (remhash cur-res *takit-wait-list*)
        (unintern cur-res)
        res))
    (flet ((exec () (let* ((wlist (wait-list *current-session*))
                           (sock (socket *current-session*))
                           (sock-state (ready-state sock))
                           (key (random-key wlist |sid-length|))
                           (sem (if nowait nil (make-semaphore)))
                           (rcmd (compile-to-js (if nowait
                                                    cmd
                                                    `(jscl::omg-write-to-string (multiple-value-list ,cmd)))
                                                *package*))
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
                                            (omg-read-from-string (caddr (gethash-lock key wlist))))))
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

(defvar *pre-boot-functions* nil)
(defvar *boot-functions* nil)

(defun add-to-boot (f)
  (push f *boot-functions*))

(defun set-boot (f)
  (setf *boot-functions* (list f)))

(defun rm-from-boot (f)
  (delete f *boot-functions*))

(defvar-f *boot-done* nil)

(defun boot-f ()
  "The boot code, will be executed on the browser-side just after the page is loaded and socket connected."
  (setf (slot-value *current-session* 'last-active) (get-universal-time))
  (remote-exec `(if (not *boot-done*)
                    (progn
                      (defun jscl::omg-write-to-string (&rest forms)
                        (with-output-to-string (s)
                          (let ((*print-circle* t))
                            (setf (jscl::oget (jscl::%js-vref "self") "OMG" "in_omg_write") t)
                            (map nil (lambda (f) (print f s)) forms)
                            (setf (jscl::oget (jscl::%js-vref "self") "OMG" "in_omg_write") (jscl::lisp-to-js nil))
                            s)))
                      (setf (jscl::oget (jscl::%js-vref "self") "OMG" "session_id") ,(symbol-name (get-id *current-session*)))
                      (setf *boot-done* t)
                      ,@*pre-boot-functions*
                      ,@*boot-functions*
                      nil)
                    (if (fboundp 'sync-all-data)
                        (sync-all-data)))

               :nowait))

(defun make-ws (env)
  "Return the websocket for the new session. Also, creates the session object."
  (let* ((ws (websocket-driver.server:make-server env))
         (ses (make-instance 'omg-session :socket ws :ws ws))
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
        (if (and (> (length msg) 8)
                 (equal (subseq msg 0 8) "!!SESID:"))
            (let* ((new-sid (intern (subseq msg 8) :omg))
                   (new-ses (gethash new-sid *session-list*)))
              (if new-ses
                  (progn
                    (setf (disconnected-at ses) 0)
                    (setf (disconnected-at new-ses) nil)
                    (setf ses new-ses)
                    (setf sid new-sid))))
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
                           (signal-semaphore (cadr trsem)))))))))))
    (on :close ws
       (lambda (&key code reason)
        (format t "WS closed (~a ~a)~%" code reason)
        (setf (disconnected-at ses) (get-universal-time))))
    ws))

(defun get-str-from (s len)
  (let ((*read-eval* nil)
        (tseq (make-array (list len) :element-type '(unsigned-byte 8))))
    (read-sequence tseq s)
    (utf-8-bytes-to-string tseq)))

(in-package :omg)

(defun get-service-worker-js ()
  (concatenate 'string
    "
   self.Deno=1; // Just a fake to disable JSCL worker code
    "
    (get-main-js)
    "
self.addEventListener('install', (e)=>{
 console.log('Service worker installed!')
 return self.skipWaiting()
})

self.addEventListener('activate', (e)=>{
 console.log('Service worker activated!')
 return self.clients.claim()
})

self.addEventListener('message', (currentEvent)=>{
 jscl.internals.globalEval(currentEvent.data)
})

OMG.fetchHandler=(ev)=>{return}

self.addEventListener('fetch', function(e) {
  OMG.fetchHandler(e)
})

"))

(defun get-worker-js ()
  (concatenate 'string
    "
   self.Deno=1; // Just a fake to disable JSCL worker code
    "
    (get-main-js)
    "
self.addEventListener('message', (currentEvent)=>{
  jscl.internals.globalEval(currentEvent.data)
})
self.postMessage('BOOT')
"))


(defvar *user-uri-handler* (lambda (env)
                             (declare (ignore env))
                             '(404 (:content-type "text/plain") ("File not found"))))

(defvar *serve-paths* (make-hash-table :test #'equal))

(defun add-serve-path (path result)
  (setf (gethash path *serve-paths*) result))

(defun serv (env)
  (let ((uri (getf env :REQUEST-URI))
        (*read-eval* nil))
    (cond ((equal uri (concatenate 'string *root-path* *service-worker-path*))
           `(200
               (:content-type "text/javascript; charset=utf-8"
                :cache-control "no-store"
                :Cross-Origin-Opener-Policy "same-origin"
                :Cross-Origin-Embedder-Policy "require-corp")
               (,(get-service-worker-js))))
          ((equal uri (concatenate 'string *root-path* *web-worker-path*))
           `(200
               (:content-type "text/javascript; charset=utf-8"
                :Cross-Origin-Opener-Policy "same-origin"
                :Cross-Origin-Embedder-Policy "require-corp")
               (,(get-worker-js))))
          ((equal uri (concatenate 'string *root-path* *js-path*))
           `(200
               (:content-type "text/javascript; charset=utf-8")
               (,(get-main-js))))
          ((equal uri (concatenate 'string *root-path* *html-path*))
           `(200 (:content-type "text/html; charset=utf-8"
                  :Cross-Origin-Opener-Policy "same-origin"
                  :Cross-Origin-Embedder-Policy "require-corp")
                 (,(get-root-html))))
          ((and (equal uri (concatenate 'string *root-path* *rpc-path*))
                (getf env :content-length))
           (with-input-from-string (s (omg::replace-all (get-str-from (getf env :raw-body) (getf env :content-length))
                                          "\\n"
                                          (make-string 1 :initial-element #\newline)))
             (let* ((session-id (intern (symbol-name (omg-read s)) :omg))
                    (*current-session* (find-session session-id))
                    (*package* (find-package (omg-read s)))
                    (cmd (omg-read s))
                    (op (car cmd))
                    (args (cadr cmd)))
               (if (and *current-session* (not (equal session-id 'no-session)))
                   (setf (slot-value *current-session* 'last-active) (get-universal-time)))
               (if (gethash-lock op *rpc-functions*)
                 (rpc-wrapper op args *package*)
                `(404 (:content-type "text/plain; charset=utf-8") (""))))))
          ((and (equal uri (concatenate 'string *root-path* *gimme-path*))
                (getf env :content-length))
           (let ((str (get-str-from (getf env :raw-body) (getf env :content-length))))
             (with-input-from-string (s str)
               (let* ((session-id (intern (symbol-name (omg-read s)) :omg))
                      (*current-session* (find-session session-id))
                      (*package* (find-package (omg-read s)))
                      (sym (omg-read s)))
                 (if (and *current-session* (not (equal session-id 'no-session)))
                     (setf (slot-value *current-session* 'last-active) (get-universal-time)))
                 (if (symbolp sym)
                     (gimme sym))))))
          ((and (equal uri (concatenate 'string *root-path* *takit-path*))
                (getf env :content-length))
           (let ((str (get-str-from (getf env :raw-body) (getf env :content-length))))
             (with-input-from-string (s str)
               (let* ((session-id (intern (symbol-name (omg-read s)) :omg))
                      (*current-session* (find-session session-id))
                      (*package* (find-package (omg-read s))))
                 (if (not (equal session-id 'no-session))
                     (setf (slot-value *current-session* 'last-active) (get-universal-time)))
                 (takit (omg-read s) (omg-read s))))))
          ((equal uri (concatenate 'string *root-path* *ws-path*))
           (let ((ws (make-ws env)))
             (lambda (responder)
               (declare (ignorable responder))
               (start-connection ws))))
          (t (if (equal (string>= uri *root-path*) (length *root-path*))
                 (let ((res (gethash (subseq uri (length *root-path*)) *serve-paths*)))
                   (if res
                       res
                       (funcall *user-uri-handler* env)))
                 (funcall *user-uri-handler* env))))))

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
                    (lambda (sid)
                      (let ((ses (gethash-lock sid *session-list*)))
                        (if (and (disconnected-at ses)
                                 (> (- (get-universal-time) (disconnected-at ses))
                                    *session-timeout*))
                            (progn
                              (maphash (lambda (k v)
                                         (if (and (car v) (not (equal (current-thread) (car v))) (thread-alive-p (car v)))
                                             (destroy-thread (car v)))
                                         (unintern k))
                                       (wait-list ses))
                              (map nil
                                   (lambda (id)
                                     (remhash id *remote-objects*))
                                   (loop for obj being each hash-value of *remote-objects*
                                         when (equal (session obj) ses) collect (id obj)))
                              (remhash sid *session-list*)
                              (remove-all-listeners (session-ws ses))
                              (unintern sid)))))
                    (loop for k being each hash-key of *session-list*
                      when (typep (gethash-lock k *session-list*) 'remote-object) collect k))
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
