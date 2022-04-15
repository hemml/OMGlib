# OMGlib

## A Common Lisp library to build fully dynamic web interfaces

The main idea of this library is to vanish a border between frontend and backend as much as possible. You can develop a CL application using REPL and SLIME on a backend, but some of you functions and macros can be declared as _browser-side_ (_b-s_) with `defun-f` and `defmacro-f` respectively:

```
(require :omg)
(defpackage mytest (:use cl omg))
(in-package :mytest)
(defun-f js-add (x y)
  (+ x y)) ;; this code will be executed in a browser
```

The `(start-server)` function starts `clack` web server on default port 7500 (can be changed by setting `omg::*port*` variable). Now, in the html-code you can inject the following line:

```
<script src='http://localhost:7500/j' type='text/javascript'></script>
```

or just open a page <http://localhost:7500/> (a blank page with js-injection code).

Now you call a browser-side function, just executing it in REPL, for example:

```
MYTEST> (js-add 1 2)
;; (3)
```

The function call will be converted to javascript with [JSCL](https://github.com/jscl-project/jscl) and sent to the browser via websocket, where the code will be executed and result will be returned to the backend. The result in this example will be returned as a list, because we are not specified a session (see below) and the code will be executed in _all_ connected browsers, and the list of result values will be returned.

The more complicated example you can find in in _example.lisp_ file, where `omgui` package is used to manipulate DOM objects.

## OMGUI package

`OMG` package is all-sufficient and can be used as is to control a frontend. You can execute any JS code on the browser-side using [JSCL](https://github.com/jscl-project/jscl) FFI:

```
(defun-f jslog (&rest args) ;; call console.log(...args)
  "Log function for js"
  (apply (jscl::oget (jscl::%js-vref "console") "log") args)
  nil)
```

But many of useful JS and DOM-manipulating functions are provided by `omgui` package, where you can find the following functions:

- `(create-element type ...attrs)` - create and return a DOM element (like `document.createElement()`). You can provide attributes in JS notation. For example:

  ```
  (create-element "div" :|innerHTML| "Some text" :|style.fontWeight| "lighter")
  ```

  There is an easy way to append sub-elements to the created one:

  ```
  (create-element "div"
    :append-element (create-element "h1"
                                    :append-element "Some text"))
  ```

- `(append-element element &optional parent)` - append DOM `element` as a child to the `parent`. If the `parent` is omitted, the element will be appended to `document`. A text string can be supplied instead of DOM element to add a text node.

- `(check-element ID)` - check if DOM element with `ID` exists.

- `(remove-element element)` - remove `element` from DOM.

- `(element-width)` `(element-height)` - return `element` dimensions in pixels.

- `(get-element-id element)` - return DOM ID of the `element`. If the element has no ID, a random ID will be created and assigned to it.

- `(js-get-element-by-id ID)` - get DOM element by `ID`

- `(parent-element element)` returns a parent of DOM `element`

- `(page-width)` `(page-height)` `(visible-width)` `(visible-height)` `(visible-left)` `(visible-top)` - get browser page dimensions.

- `(execute-after time callback)` - execute the `(callback)` after `time`, where `time` specified in seconds.

- `(jsfloor num)` `(jsmax ...nums)` `(jsmin ...nums)` `(jsrandom)` `(jssin num)` `(jscos num)` `(jstan num)` `(jsasin num)` `(jsacos num)` `(jsatan num)` `(jsatan2 y x)` - JS Math functions `Math.floor()`, `Math.max()`, `Math.min()`, `Math.random()`, `Math.sin()`, `Math.cos()`, `Math.tan()`, `Math.asin()`, `Math.acos()`, `Math.atan()` and `Math.atan2()` .

- `(jslog ...args)` - wrapper for `console.log()`

- `(prevent-page-close)` `(allow-page-close)` - prevent and allow page closing.

- `(disable-back-button)` `(enable-back-button)` - disable/enable "back" button in browser.

- `(disable-scroll)` `(enable-scroll)` - disable/enable page scroll.

- `(make-js-object :attr1 value1 attr2 value2 ...)` - return a JS dict object with specefied keys and values.

- `(make-js-function name lambda)` - create an JS function from `lambda` which can be accesssed via `window.name`

- `(load-js-script url &optional callback)` - load JS script from `url`. The optional `callback` lambda will be executed then the script is succesfully loaded.

- `(register-hash-cb hash cb)` - register a callback to call when URL hash part changes. With this function you can, for example, automatically mark session as debug one, when you open URL like `http://localhost:7500/#debug`:

  ```
  (defun-r debug-me ()
    (set-debug-session (current-session-id)))

  (defun-r my-boot ()
    (register-hash-cb "#debug" (lambda () (debug-me))))

  (add-to-boot '(my-boot))
  ```

  If the page is loaded with the registered hash part, the callback will be executed immediately during `register-hash-cb` call.

- `(browser-case (brosser* code)*)` macro - executing a code depending from user browser. `browser` must be a symbol `:safari`, `:firefox`, `:chrome`, `:opera` or `:edge`. The `T` means other (undetected or unlisted) browser. Examples:

  ```
  (browser-case
     (:safari (jslog "Safari detected!"))
     ((:firefox :chrome) (jslog "FF or Chrome!"))
     (:opera (jslog "Opera!"))
     (:edge (jslog "Edge!")))
     (t (jslog "Other browser!"))

  (browser-case
     (:safari (safari-specific-code)) ;; will be executed in Safari
     (t (default-code)) ;; will be executed in non-Safari browsers
  ```

- `(gensym2)` - like `(gensym)`, but returns interned symbol. Use for hygienic macros, defined with `defmacro-f`, because all symbols in the macro output must pass write-read chain to be transferred to the host.

- `(add-style element css)` - add css style to the element

  Example: `(add-style el ":hover {background: #f0f0ff;}")`

- `(winref name)` - a short form for `(jscl::oget (jscl::%js-vref "window") name))`

- `(js-parse-float s)` - convert a string to float (browser-side)

- `(local-storage key &optional def-val)` `(setf (local-storage key) val)` - interface to the `localStorage` API. Accepts any values, returns strings only.

- `(show-notification header body &key period check)` - show notification (not a browser notification, just a widget) with header and body (might be strings or dom elements). Supply `period` parameter if you want to show the notification again after some period (in seconds), if user closes it. If function `check` supplied, it will called before notification reappear and if returns `nil` the notification will not be shown anymore.

  Example:

  ```
  (let ((cnt 3))
    (show-notification
      (create-element "div" :|innerHTML| "New notofocation!"
                            :|style.color| "red")
      (create-element "div"
        :append-element "Click "
        :append-element (create-element "a"
                          :|href| "#"
                          :|onclick| (lambda (ev)
                                       (remove-element
                                         (find-widget ev "notification")) ;; Just remove notification
                                                ;; The (find-widget) used to get a DOM object of notification widget
                                       nil)
                          :append-element "here")
        :append-element " and see what will happens!")
      :period 5
      :check (lambda ()
               (setf cnt (- cnt 1))
               (> cnt 0))))
  ```

- `(find-widget ev &optional name)` - the function can be used to get a DOM element of the widget, which received an event `ev`. See `(show-notification)` example.

### Creating SVG elements

You can create `SVG` elements with `make-svg` function. The function accepts parameter pairs like `:|attributename| value` for attributes and `(tag-name ...attributes and subtags)` for inner elements. A string parameter will be inserted as a tag body. For exanple, the following code will return SVG-object with circle:

```
(make-svg :|viewBox| "0 0 100 100"
          '(circle :|cx| 50 :|cy| 50 :|r| 50 :|fill| "red"))
```

The following function returns animated spinner:

```
(defun-f make-spinner ()
  (let* ((width 20)
         (ncirc 10)
         (r (/ width 2))
         (rc (/ width 6)))
    (apply #'make-svg
           `(:|viewBox| ,(format nil "0 0 ~A ~A" width width)
             ,@(loop for i below ncirc
                     for ang = (* i (/ (* 2 pi) ncirc))
                     collect
                 `(circle :|cx| ,(+ r (* (- r rc) (jscos ang)))
                          :|cy| ,(+ r (* -1 (- r rc) (jssin ang)))
                          :|r| 0
                          :|fill| "#505050"
                          (animate :|attributeName| "r"
                                   :|from| ,rc
                                   :|to| ,(* rc (/ 3 (+ ncirc 2)))
                                   :|begin| ,(format nil "~As" (/ (- ncirc i 1) ncirc))
                                   :|dur| "1s"
                                   :|repeatCount| "indefinite")
                          (animate :|attributeName| "fill-opacity"
                                   :|from| 1
                                   :|to| 0
                                   :|begin| ,(format nil "~As" (/ (- ncirc i 1) ncirc))
                                   :|dur| "1s"
                                   :|repeatCount| "indefinite")))))))
```

To include text nodes use a string:

```
'(text (:|textPath| :|href| "#MyTextCurve" "The curved text!"))
```

### Modal dialogs

You can display modal dialog in the browser using the `modal-dialog` macro:

```
(in-debug-session
  (print (modal-dialog "Dialog header"
                       "Dialog text"))
                       :lines (list :line1 "field 1"
                                    :line2 "field 2"
                                    :buttons (list (list "OK" #'dialog-ok)
                                                   (list "Cancel" #'close-current-dialog))))
;; ((:line1 "Text in the line 1"))
    (:line2 "Text in the line 2")
```

This code will display a modal dialog in debug session and print the result, returned as a `plist` or `nil` if the `Cancel` button was pressed.

You can provide validation functions for input lines in the form `... :line1 (list "field1" #'func)`, where `#'func` can be a browser-side or RPC function. Also you can provide `(lambda (s) ...)` as a validator function, and even a #'any-backend-function but you will get a security warning in last case. The function must accept a string as an argument and return an (optionally) modified string which will replace the string in the input field. See the _example.lisp_.

You callbacks can use the following supplementary functions:

- `(close-current-dialog)` - close current modal dialog, `modal-dialog` will return `nil`.

- `(dialog-ok)` - close current modal dialog, `modal-dialog` will return data entered in fields.

- `(get-dialog-data)` - returns `plist` with dialog data, can be called anytime on frontend or backend.

### Yotube player

- `(add-youtube-player element &key onready onstatechange onqualitychange onratechange onerror onapichange width height video-id)` - add YouTube player on the page. The `video-id` is a string with YouTube video ID; `element` must be parent element for the player; `width` and `height` -- player dimensions; `onready`, `onstatechange`, `onqualitychange`, `onratechange`, `onerror` and `onapichange` - the callbacks. See the `example.lisp`.

## Restrictions

- **All browser-side functions must be declared in your own package(s), not in CL-USER.** See [How it works](#how-it-works) for details.

- The library tested in SBCL, but may work in other CL impementations too. The browser code will be executed in JSCL environment, where not all of the standard CL library functions are implemented yet, so you have to respect JCSL limitations on browser-side.

- All the function parameters and their results must be serializable via standard LISP reader/writer, so you cannot return, for example, DOM object from browser-side function to backend, and put a hash-table as a parameter to browser-side function when calling it on backend. But you can use any allowed data types while you are still on backend or browser-side, if a b-s function will be called only by another b-s functions it can return DOM objects. Also, you can pass a lambdas as arguments of b-s functions, they will be compiled and executed on browser-side. The backend functions can be passed as parameter to bs-functions, they will work on the backend, while called on brower-side, but you will get a security warning during compilation.

- Lists can be passed as parameters to b-s functions, but they must be implicitly constructed with LIST function, quotes and backquotes cannot be used:

  ```
  (some-bs-function (list a b c)) ;; will work
  (some-bs-function '(a b c)) ;; will not work
  ```

  This is because the CL macros cannot distinct function calls and lists, constructed by quotation and quasiquotations. The second line will be threated as `(some-bs-function (a b c))`.

- CLOS on browser-side is not implemented yet. And there may be some fundamental difficulties to implement it (see [How it works](#how-it-works) section).

- There is no error propagation yet between browser and backend. If bs-function causes a error, `nil` will be returned.

- Hygienic macros _must_ use `gensym2` instead of `gensym`

## How it works

There are the following macros to define browser-side functions, macros and variables:

- `defun-f`
- `defmacro-f`
- `defvar-f`
- `defparameter-f`

Also you can define RPC-functions, with

- `defun-r`

### Browser-side functions

`defun-f` has similar syntax as a `defun`, but defines macro with the same name, instead of function. The macro has to check its arguments and evaluate only some of them on backend, passing the rest to the browser-side as is. For example:

```
(some-bs-function "value1" ;; will be passed as is
                  (some-backend-function) ;; will be evaluated on backend and the result will be passed as a parameter
                  (another-bs-function) ;; will be passed as is and evaluated on browser-side
                  #'another-bs-function ;; will be passed as is
                  (lambda (x) (+ x 1)) ;; will be passed as is and evaluated on browser-side
                  #'another-backend-function) ;; will be automatically converted to RPC-call, security warning will be printed
```

Just after websocket connection, the browser-side has no declared symbols and functions. When you trying to execute `(some-bs-function)` the symbol `some-bs-function` will be undeclared (in browser) and the library will ask backend for the symbol via synchronous XHR query and will get it as an already compiled to JS code. You can change this behavior by setting the `omg::*local-compile*` variable to `nil`, after that the CL code will be sent as is and compiled in browser before execution. This can provide a bit more compatibility, but code compilation will be much slower. The `omg::*local-compile*` may be changed on-the-fly without browser page reloading.

The library determines which symbols must be fetched from backend just by their packages: if the package still not exists, it will be created on-the-fly with `defpackage`, `cl` and `jscl` packages will be used. All created packages will be marked as _remote_ and the library will try to fetch unknown symbols from that packages from the backend. This is the reason, why you cannot define browser-side functions just in `CL-USER`.

The CLOS is not implemented in browser-side. It is implemented in JSCL, but I see no way to catch access to undefined methods if several methods are defined with same names. If you are know how to implement this - tell me :)

### Browser-side macros

Browser-side macros are called while code is compiled to JS and must be evaluated on the browser side. So, if you are use such macros while the `omg::*local-compile*` is set to `T` (by default), JSCL will parse the code and execute code of each macro in the browser, get the results and finish the compilation using them. This means that while local compilation is enabled, macro expansion will be rather expensive, especially, if you are using recursive macros. Also, all of the macro output must be transferred from the browser to the host, so you cannot use just `(gensym)` to generate temporary symbols, they are must be interned to be properly transferred. Intern all of your symbols manually or just use helper function `(gensym2)` from OMGUI package.

### Browser-side variables and parameters

You can declare browser-side variables and parameters with `defvar-f` and `defparameter-f`. They a similar to standard `defvar` and `defparameter`, but the symbols will be available to browser-side functions. This is one-way availability: if the variable will be changed on browser-side, backend will see no update, also, the data will not be updated on other connected browsers. If you are changing such variable or parameter on the backend, the changes will be seen only for newly connected browsers or by browser, which are not fetched they yet.

### RPC functions

Due to security reasons, you can call only some specially marked functions of backend from browser side. You can declare them with `defun-r` macro, which acts as standard `defun`, but places the function name in the list of allowed RPC functions. The RPC function can be simply called on browser-side as any other function.

Also, you can asynchronously call any RPC function using `async-bind` macro:

```
(async-bind (res (some-rpc-function arg1 arg2))
  (jslog res) ;; The code will be executed asynchronously after RPC call completion
```

### Sessions

Each connected browser starts a new _session_ which is determined by unique random symbol - _session ID_. When RPC-function is called from browser-side, it will be executed in the session context, so, it will execute all bs-functions in the specific browser. You can implicitly set the current session by executing a code inside `with-session` macro:

```
(with-session (find-session 'ASDXCASA) ;; find a session object by ID
  (some-bs-function)) ;; will be called in session 'ASDXCASA
```

If you are executing bs-function without session, it will be executed in **ALL** connected browsers and a list of results will be returned.

The library provides some other utility functions to work with sessions:

- `(set-debug-session session)` - execute this function to mark specific _session_ as **debug**.
- `(in-debug-session code)` - execute a code in the debug session. If there are no active debug session a warning will be printed and code will not executed.

To define global server-side variable, which value will be different for different sessions, use `def-session-var` macro:

```
(def-session-var *search-in-progress* nil)
```

The macro will create a symbol-macro with name `*search-in-progress*` and a hash table to store values for each session. Now you can use `*search-in-progress*` as a normal setf-able variable.

**WARNING:** use init value with caution! Value, passed as a third (optional) parameter of `def-session-var` will be evaluated with every access to the variable!

### Boot functions

Just after connection, the some boot code will be executed in browser. You can control this using the following functions:

- `(add-to-boot code)` - add some code to boot sequence. This function can be called more then once to to codes which will be executed sequentally in the order of addition. The code must be a lisp form, for _example:

  ```
  (add-to-boot '(jslog "Hi!")) ;; print "Hi!" to JS console.
  ```

  If you want to execute some backend code, use RPC call:

  ```
  (defun-r my-boot ()
    (print "New browser is connected!")
    (set-debug-session (current-session-id))) ;; Mark the session as debug
  (add-to-boot '(my-boot))
  ```

- `(rm-from-boot code)` - remove code from boot sequence.

### REMOTE-EXEC function

You can use `(remote-exec cmd [nowait])` function to execute any CL code `cmd` in the browser. This function has optional argument `nowait` - set it to `T` if you are not needed to return value(s) and the function will return nil immediately.

### Adding a custom HTML into default document body

The default HTML page returned for "/" just contains the main js script link, but you can add any extra HTML into document body, for example to display "Loading..." message:

```
 (add-to-root-html "<span id='loadBanner'>The page is loading, please wait...</span>")
 (add-to-boot '(remove-element (js-get-element-by-id "loadBanner")))
```

### PWA mode

The library can be used to create Progressive Web App (PWA), just by adding one single function call:

```
  (make-pwa (:name "Application Name" :short-name "Application Short Name" :display "standalone" :theme-color "#000000" :background-color "#ffffff" :icon-path "/path/to/icon/file.png")
```

All of the arguments are optional (default values will be used), but to get installable app you need to provide a path to icon file. Now only `.png` `.jpeg` and `.gif` image formats are accepted.

Worker process sending broadcast message for every `fetch` request, the app can listen for them with the following code:

```
(setf (jscl::oget (jscl::make-new (jscl::oget (jscl::%js-vref "window") "BroadcastChannel")
                                    (jscl::lisp-to-js "omg_service_worker"))
                  "onmessage")
      (lambda (msg)
        (let ((typ (jscl::oget msg "data" "type")))
          (if (equal typ "fetch")
              (let ((uri (jscl::oget msg "data" "uri")))
                (jslog "FETCH:" uri))))))
```

## OMG daemon mode

**WARNING:** the following functionality is in early alpha version now, it will work only in POSIX compatible environments (Linux, MacOS X) and only with `SBCL`, but easely can be ported to another CL compilers.

OMGlib allows developers to work with code via REPL, but all of the changes are applied immediately, so, you need two copies of your code -- local development version and production one, which is updated less frequently. Each production code update causes a server restart, so all connected clients will lose they connections and will have to reconnect. After the reconnection, browser still has functions from previous version, which may lead to problems.

OMGlib offers a better way to maintain version updating. The special daemon `omgdaemon` is spawned and works as a reverse-proxy, which accepts http connections and, basing on special cookie `OMGVERSION`, connects clients to specific versions. Here is one special version with name `"devel"`, where developer(s) can do any development works, using swank connection via standard port 4008\. When the version will be good enough to be pushed into production, the `(commit-production)` function is called and lisp image is saved to disk and gets an unique version name. After that, all new connections will be redirected to this version and new `"devel"` version will be spawned again. If there was previous versions with clients connected, they will receive notification (the `(commit-notify new-version)` function will be called) so they can offer version update to the clients. Also, the browser-side function `(ensure-last-version)` can be called at boot-time, to check is it a latest version and perform page reload to proceed to the top version if necessary.

The following code can be used to work in `omgdaemon` environment:

```
(defpackage :my-package
  (:use cl omg omgui jscl omgdaemon)) ;; Import omgdaemon

(defparameter-f *page-shown* nil)

(defun-f my-boot () ;; this function will be called after each WebSocket connection
  (if (not *page-shown*) ;; The page is just loaded
      (progn
        (register-hash-cb "#devel" ;; The way to proceed to "devel" version using #devel hash
          (lambda ()
            (if (not (equal (get-my-version) "devel")) ;; If the version is not a "devel" already
                (progn
                  (setf (jscl::oget (jscl::%js-vref "document") "cookie")
                        (format nil "~A=devel" (get-omg-cookie-name))) ;; set the version cookie
                  (allow-page-close)
                  ((jscl::oget (jscl::%js-vref "location") "reload") t))))) ;; Hard reload!
        (ensure-last-version)
        (setf *page-shown* t)
        (init-gui)))) ;; Show page contents

(defun-f show-commit-notify (cookie-name version) ;; Show a notification if a new version is available
  (show-notification
    (create-element "div" :|innerHTML| "New version available!"
                          :|style.color| "red")
    (create-element "div"
      :append-element "Save you work and click "
      :append-element (create-element "a"
                        :|href| "#"
                        :|onclick|
                          (lambda (ev)
                            (setf (jscl::oget (jscl::%js-vref "document") "cookie")
                                  (format nil "~A=~A" cookie-name version))
                            (allow-page-close)
                            ((jscl::oget (jscl::%js-vref "location") "reload") t)
                            nil)
                        :append-element "here")
      :append-element " to proceed to new version.")
    :period 600) ;; Remind every 10 minutes
  nil)

(defun commit-notify (version)
  (remote-exec `(show-commit-notify ,+omg-version-cookie+ ,version))
  nil)

;; omg-init will be called after version spawn

(defvar old-init #'omg-init) ;; Save the default init function

(defun omg-init (port)
  ;; Put here all your initialization -- connect to the database, etc...
  (funcall old-init port))

(add-to-boot '(my-boot)) ;; Add #'mu-boot to the boot sequence
```

You can use standadrd slime/swank to connect to the `devel` image and perform any REPL work.

## Making an `omgdaemon` image

The `omgdaemon` image can be built with the following command:

```
sbcl --eval "(require :omg)" --eval "(omgdaemon:make-omg-daemon 8080)"
```

where `8080` is a port where proxy will accept HTTP connections. The daemon will store version images in `.omg` subdirectory.

## Using docker container

The most convinient way to run `omgdaemon` is to put it into a docker container. You can build a docker image with the following command:

```
sbcl --eval "(require :omg)" --eval "(omgdaemon::make-docker-image)" --quit
```

The image can be started in the following way to serve on port `7575` on `localhost`:

```
docker run -d -p 127.0.0.1:7575:8080 -p 127.0.0.1:4008:4008 omgdaemon
```
