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
    :append-elements element-list
  ```

  Also, it is possible to add custom CSS style to the element:

  ```
  (create-element "div"
    :add-style ":hover {cursor:pointer;}"
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

- `(jsfloor num)` `(jsfceil num)` `(jsftrunc num)` `(jsmax ...nums)` `(jsmin ...nums)` `(jsrandom)` `(jssin num)` `(jscos num)` `(jstan num)` `(jsasin num)` `(jsacos num)` `(jsatan num)` `(jsatan2 y x)` - JS Math functions `Math.floor()`, `Math.ceil()`, `Math.trunc()`, `Math.max()`, `Math.min()`, `Math.random()`, `Math.sin()`, `Math.cos()`, `Math.tan()`, `Math.asin()`, `Math.acos()`, `Math.atan()` and `Math.atan2()` .

- `(jssorg array &optional (fn #'>))` - destructive sort an array by compare function fn, return a sorted array

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

- `(is-nan n)` - check if `n` is NaN (browser-side)

- `(local-storage key &optional def-val)` `(setf (local-storage key) val)` - interface to the `localStorage` API. Accepts any values, returns strings only.

- `(session-storage key &optional def-val)` `(setf (session-storage key) val)` - interface to the `sessionStorage` API. Accepts any values, returns strings only.

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

- `(ensure-element el &rest body)` - macro, execute `body`, when element `el` become visible (will have non-zero dimensions)

- `(on-element-remove el callback)` - execute `(callback el)` when element `el` is removed from page

- `(add-event-handler path callback)`, `(rm-event-handler path callback)` - add and remove global event handlers (use it for "document.body.on..." like events). Example:

  ```
  (defun-f my-callback (ev)
    ...)
  ...
  (add-event-handler "document.body.onmousemove" #'my-callback)
  ...
  (rm-event-handler "document.body.onmousemove" #'my-callback)
  ```

- `(add-event-listener event fn &key passive once capture)` - just a wrapper around `addEventListener`

- `(make-tab-form tab-definitions)` - return DOM element, a form with multiple tabs. The tab definitions must be in the following form:

  ```
  (make-tab-form `(("Tab1 title" . ,tab1-contents-element)
                   ("Tab2 title" . ,tab2-contents-element)))
  ```

- `(now)` - returns current time is seconds, but with subsecond precision (a float number)

- `(with-self var &rest code)` - an useful local macro, allows to use an element itself inside its events, for example:

  ```
    (with-self div
      (create-element "div" :|style.padding| "1em" ;; this DOM element will be stored in div variable
                            :|style.border| "1px solid red"
        :append-element (create-element "button" :|innerHTML| "REMOVE ME"
                          :|onclick| (lambda (ev) (remove-element div))))) ;; remove the div when button clicked
  ```

- `(with-promise p &key then catch)` - a wrapper around JS Promise interface (a local macro):

   ```
     (with-promise (funcall (jscl::oget (jscl::lisp-to-js (jscl::%js-vref "DeviceMotionEvent")) "requestPermission"))
       :then (lambda (res)
               (format t "OK: ~A" res))
       :catch (lambda (err)
               (format t "ERR: ~A" err)))
     ;; Equivalent to: DeviceMotionEvent.requestPermission().then((res)=>{console.log(res)}).catch((err)=>{console.log(err)})
   ```

- `(oget-bind vars el keys &rest code)` - a local macro, like `destructuring-bind`, but for js object properties (not setfable):

  ```
    (oget-bind (ax ay) (ev "accelerationIncludingGravity") ("x" "y")
      (jslog x y)) ;; print ev.accelerationIncludingGravity.x and ev.accelerationIncludingGravity.y

    (oget-bind (ofs-x ofs-y w h) element ("offsetLeft" "offsetTop" "offsetWidth" "offsetHeight")
      ...) ;; binds elelent.offsetLeft, element.offsetTop
  ```

### Creating SVG elements

You can create `SVG` elements with `make-svg` function. The function accepts parameter pairs like `:|attributename| value` for attributes and `(tag-name ...attributes and subtags)` for inner elements. A string parameter will be inserted as a tag body. For example, the following code will return SVG-object with circle:

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
                       :lines '(:line1 "field 1"
                                :line2 ("field 2" :default "12345")
                                :pass ("Password:" :type "password")
                                :buttons (("OK" #'dialog-ok)
                                          ("Cancel" #'close-current-dialog))))
;; ((:line1 "Text in the line 1"))
    (:line2 "Text in the line 2")
```

This code will display a modal dialog in debug session and print the result, returned as a `plist` or `nil` if the `Cancel` button was pressed.

You can provide validation functions for input lines in the form `... :line1 (list "field1" :filter #'func)`, where `#'func` can be a browser-side or RPC function. Also you can provide `(lambda (s) ...)` as a validator function, and even a #'any-backend-function but you will get a security warning in last case. The function must accept a string as an argument and return an (optionally) modified string which will replace the string in the input field. See the _example.lisp_.

You callbacks can use the following supplementary functions:

- `(close-current-dialog)` - close current modal dialog, `modal-dialog` will return `nil`.

- `(dialog-ok)` - close current modal dialog, `modal-dialog` will return data entered in fields.

- `(get-dialog-data)` - returns `plist` with dialog data, can be called anytime on frontend or backend.

To make a password input use line `(:mypass "Password" :type "password")`

### Yotube player

- `(add-youtube-player element &key onready onstatechange onqualitychange onratechange onerror onapichange width height video-id)` - add YouTube player on the page. The `video-id` is a string with YouTube video ID; `element` must be parent element for the player; `width` and `height` -- player dimensions; `onready`, `onstatechange`, `onqualitychange`, `onratechange`, `onerror` and `onapichange` - the callbacks. See the `example.lisp`.

## Widgets

The OMG library now includes support of CLOS-based widgets (see `omgwidgets.lisp`). The base class is `omg-widget`, which has slot `root` (and accessor `root`) for the root DOM object. Also a `(render-widget (w omg-widget))` and `(redraw (w omg-widget))` methods are provided.

The `render-widget` method must create a root DOM and store it in the `root` slot of the widget. The root DOM must be returned. You have to subclass `omg-widget` and provide your own `render-widget` method.

The `redraw` method can be used to completely rebuild root DOM (by calling `render-widget` method) and replace it on the page. For the most cases you can just use default `redraw` method of `omg-widget` class.

To make a simple widget with static DOM, just use:

```
(defclass-f my-widget (omg-widget)
  ((root :initform (create-element "span"
                     :|style.textDecorationStyle| "dashed"
                     :|style.textDecorationLine| "underline"
                     :|style.textDecorationThicknes| "1.75pt"
                     :|style.color| "blue"
                     :|title| "change"
                     :add-style ":hover {cursor:pointer;}"))
                     :append "TEST WIDGET"
```

and add it to the page:

`(append-element (render-widget (make-instance 'my-widget)))`

### editable-field

The editable-field widget shows a value (generally a string) with dashed underline. It is clickable, and shows a text input line to change the value.
Typical use:

```
(append-element
  (render-widget
    (make-instance 'editable-field
      :input-size 5 ;; Size of the input field (when editing)
      :value (format nil "~A" *current-value*) ;; Initial value
      :ok (lambda (v)   ;; The callback is called, when user updates the value, will get a entered value as a
            (let ((v1 (js-parse-float v))) ;; parameter and must return a value, which will replace an old one, or
              (if (> v1 0)                 ;; nil to keep old value
                  v1)))
      :cancel (lambda (w) ;; w is an editable-field instance itself
                (jslog "User canceled input!")))))
```

### modal-dialog-window

You can subclass this widget to show modal dialogs - elements, which are on top of all elements on the screen and all content below are shaded and unclickable, while wis widget is on the screen. There are some differences from an ordinary widget. First, you have to override `render` method of its subclass with `:after` keyword and modify `root` property to show your content. Second, you have to use `close` method to remove this widget:

```
(defclass-f my-dialog (modal-dialog-window) ())

(defmethod-f render-widget :after ((w my-dialog))
  (append-element
    (create-element "div" :|style.border| "0.1em solid black" ;; rendering a window content
                          :|style.border-radius| "0.5em"
                          :|style.padding| "2em"
                          :|style.background| "#fffff0"
      :append-element (create-element "button" :|innerHTML| "OK"  
                                               :|onclick| (lambda (ev)
                                                            (close w)))) ;; remove the widget
    (root w)))
```

### progress-bar

Renders a simple progress bar, you can use `set-progress` method to change it's value:

```
(let ((bar (make-instance 'progress-bar
              :bg-style '(:|style.border| "1px solid black" ;; specify the outer frame style
                          :|style.background| "white"
                          :|style.padding| "1em")
              :width "20em" ;; specify width
              :height "2em" ;; and height
              :fg-style '(:|style.background| "green") ;; specify a bar style
              :value 0.1))) ;; initial value
  (append-element (render-widget bar)) ;; show it
  (execute-after 3 ;; change the value after 3 sec.
    (set-progress bar 0.5))) ;; value must be from 0 to 1
```

### list-view

The `list-view` widget is a mirrored class (see below), displaying a list of elements, synchronized with a list on backend. The list offers lazy loading of elements, so you can display a really huge lists and browser will not hang. Each element of the list must be an `omg-widget`.
Use it with the following way:

```
(defclass-f lst-element (omg-widget)
  ((pos :initarg :pos
        :accessor pos)))

(defmethod-f render-widget ((el lst-element))
  (setf (slot-value el 'root)
        (create-element "div" :|innerHTML| (format nil "Element ~A" (pos el)))))

(let ((lst (make-instance 'list-view ;; Creating a list-view with 1000 elements
              :current-position 100  ;; And default view position is 100
              :elements (loop for i below 1000 collect (make-instance 'lst-element :pos i)))))
  (append-element (render-widget lst)
)
```

The `elements` slot of the `list-view` instance holding a list of elements. When you changing it on the backend, call `(sync-data lst)` method on the host to update it in browser(s).

### Scientific plots

You can display 1D and 2D scientific plots. First, draw a coordinate system with `graph` widget:

```
(append-element
  (create-element "div" :|style.width| "20em"  ;; you have to define an outer element, the graph will fill its bounds
                        :|style.height| "20em"
    :append-element (render-widget (make-instance 'graph :xmin -1 :xmax 1 :ymin 0 :ymax 1))))
```

The `graph` class has the following slots (with corresponding initargs):

- `xmin`, `xmax`, `ymin`, `ymax` - graph bounds (user coordinates)
- `xticks`, `yticks` - number of ticks on both axes
- `adjust` - if `T` (default) ticks will be started not from `xmin` and `ymin`, but from nearest round values
- `xdelta`, `ydelta` - if set (by default is not) will be the distance between ticks on corresponding axes
- `xcaption`, `ycaption` - axes labels
- `show-scales` - a list, which may contain keywords `:left`, `:right`, `:top` and `:bottom`, controls which axes scales will be shown
- `preserve-aspect-ratio` - if `T` (by default is `nil`) the graph aspect ratio will be calculated as `(/ (- xmax xmin) (- ymax ymin))`, otherwise the aspect ratio will depend from outer element sizes

To display data you can add `func-plot`, `tabular-plot` and `matrix-plot` instances to the `graph` via `add-plot` method:

```
(append-element
  (create-element "div" :|style.width| "20em"  ;; you have to define an outer element, the graph will fill its bounds
                        :|style.height| "20em"
    :append-element
      (render-widget
        (let ((g (make-instance 'graph :xmin (- pi) :xmax pi :ymin -1 :ymax 1 :preserve-aspect-ratio t :yticks 3)))
          (add-plot g (make-instance 'func-plot :func (lambda (x) (jssin x))))
          g))))
```

All of that classes has a `color` slot (by default is "red"), you can set any color in the form, supported by css.
The when creating a `func-plot` you must provide a `:func` parameter which is a function of single argument, returning a number.
To the `tabular-plot` you must provide a `:table` argument, which is a list of points, each is a `(x . y)` cons.
The `matrix-plot` is to display a 2D map and has the following slots:

- `matrix` - a matrix to display, a 2D `array` filled by numbers
- `palette` - a function of a single argument (from 0 to 1), returning a list of 3 values (also from 0 to 1), corresponding to reg, green and blue components. By default it is `(lambda (x) (list x x x))` to display b/w graphs.
- `norm` - if `T` (by default is `nil`) the matrix will be shifted and scaled before display to fit a whole (0..1) values range. Very useful if your matrix may contain negative values, or values higher then 1, for example.

You can use `(remove-plot graph plot)` and `(remove-all-plots graph)` to remove plots from the graph.
To rescale (and redraw) the graph you can use `rescale` method:

```
(rescale graph :xmin new-xmin :xmax new-xmax :ymin new-ymin :ymax new-ymax :xdelta new-xdelta :ydelta new-ydelta)
```

Also, you can use `(rescale-auto graph)` method to rescale the graph to fit containing tabilar- and matrix-plots.

## Restrictions

- **All browser-side functions must be declared in your own package(s), not in CL-USER.** See [How it works](#how-it-works) for details.

- The library tested in SBCL, but may work in other CL implementations too. The browser code will be executed in JSCL environment, where not all of the standard CL library functions are implemented yet, so you have to respect JCSL limitations on browser-side.

- All the function parameters and their results must be serializable via standard LISP reader/writer, you cannot return, for example, hash-table object from browser-side function to backend, and put a hash-table as a parameter to browser-side function when calling it on backend. But you can use any allowed data types while you are still on backend or browser-side, if a b-s function will be called only by another b-s functions it can return any objects. Also, you can pass a lambdas as arguments of b-s functions, they will be compiled and executed on browser-side. The backend functions can be passed as parameter to bs-functions, they will work on the backend, while called on brower-side, but you will get a security warning during compilation.

- DOM and MOP objects are specially serialized on browser-side and passed to backend as `omg::remote-object` instances. They can be sent back and used as parameters within `remote-exec`, for example. The `omg::remote-object` instances are belongs to their sessions, so you cannot send, for example, a DOM object, created in one browser to another. The `omg::remote-object` instances itself are "black boxes" for the backend, you can only store them and pass to browser-side functions as parameters.

- Lists can be passed as parameters to b-s functions, but they must be implicitly constructed with LIST function, quotes and backquotes cannot be used:

  ```
  (some-bs-function (list a b c)) ;; will work
  (some-bs-function '(a b c)) ;; will not work
  ```

  This is because the CL macros cannot distinct function calls and lists, constructed by quotation and quasiquotations. The second line will be treated as `(some-bs-function (a b c))`.

- There is no error propagation yet between browser and backend. If bs-function causes a error, `nil` will be returned.

- Hygienic macros _must_ use `gensym2` instead of `gensym`

## How it works

There are the following macros to define browser-side functions, macros and variables:

- `defun-f`
- `defmacro-f` and `def-local-macro-f`
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

### Browser-side macros

Browser-side macros are called while code is compiled to JS and must be evaluated on the browser side. So, if you are use such macros while the `omg::*local-compile*` is set to `T` (by default), JSCL will parse the code and execute code of each macro in the browser, get the results and finish the compilation using them. This means that while local compilation is enabled, macro expansion will be rather expensive, especially, if you are using recursive macros. Also, all of the macro output must be transferred from the browser to the host, so you cannot use just `(gensym)` to generate temporary symbols, they are must be interned to be properly transferred. Intern all of your symbols manually or just use helper function `(gensym2)` from OMGUI package.

You can disable this behavior by setting `omg::*disable-remote-macro*` to `t`, or by using `def-local-macro-f` - in this case the code of macros will be executed locally (on the backend).

### Browser-side variables and parameters

You can declare browser-side variables and parameters with `defvar-f` and `defparameter-f`. They a similar to standard `defvar` and `defparameter`, but the symbols will be available to browser-side functions. This is one-way availability: if the variable will be changed on browser-side, backend will see no update, also, the data will not be updated on other connected browsers. If you are changing such variable or parameter on the backend, the changes will be seen only for newly connected browsers or by browser, which are not fetched they yet.

### RPC functions

Due to security reasons, you can call only some specially marked functions of backend from browser side. You can declare them with `defun-r` macro, which acts as standard `defun`, but places the function name in the list of allowed RPC functions. The RPC function can be simply called on browser-side as any other function.

Also, you can asynchronously call any RPC function using `async-bind` macro:

```
(async-bind (res (some-rpc-function arg1 arg2))
  (jslog res)) ;; The code will be executed asynchronously after RPC call completion

(async-bind ((r1 r2 r3) (some-rpc-function arg1 arg2)) ;; multiple return values
  (jslog r1 r2 r3))
```


### CLOS

The preliminary CLOS support is added. You can use `defclass-f`, `defmethod-f` and `defgeneric-f` to create browser-side classes, methods and generics. May be buggy, please report all bugs found. Browser-side methods cannot be invoked directly from the host, use `remote-exec` instead. The CLOS implementation is JSCL is very old and may be obsolete, see JSCL documentation and issues on github. For example there is problems with accessors setf expanders.

#### Mirrored objects

You can declare specific classes and create instances on backend, using `defclass-m` macro, which is defined in `omgutil` package:

```
(defclass-m avatar ()
  ((xxx :initarg :xxx)
   (yyy :initarg :yyy
        :browser-side t)
   (zzz :initarg :zzz
        :mirrored t))
   :scope :session)

(defmethod-r xxx ((obj avatar))
  (slot-value obj 'xxx))

(defparameter a (make-instance 'avatar :xxx 10 :yyy 20 :zzz 30))

(remote-exec `(jslog (xxx ,q)))
```

Here is a local variable `a`, contains an instance of `avatar` class. This instance can be transferred to the browser-side, where an instance with the same class name will be created. On browser-side only methods marked with `:browser-side t` can be accessible (an they cannot be accessed on backend). All other slots are directly accessible only at backend side. The `defmethod-r` is like `defun-r`, creates a method on the backend, which can be called on browser-side: `(jslog (xxx ,q)))` and executed on the backend with instance `a` in `obj` parameter, so it can be used to access value of the slot `xxx`.

You also can define browser-side methods with `defmethod-f` and they are completely independed from local methods. So, you can define `(defmethod initialize-instance :after ((obj avatar) ...` and `(defmethod-f initialize-instance :after ((obj avatar) ...` simultaneously, one will work on backend, while another will work on frontends.

The mirrored class can inherit other classes, both normal an browser-side ones (like `omg-widget`). The backend class will inherit only local classes from the given list, while browser-side will inherit only classes, declared with `defclass-f` and `defclass-m`.

Each class can have a `:scope` option defined to increase instance security. If the scope is `:session`, the instance can be used only within a (browser) session, where it was created. If you will try to send the object into browser with different session, a error will be raised. The default scope is `:auto`, which is same as `:session` if the instance was created within a session, and will work with any session, if no current session on instance creation time. Also, you can provide an arbitrary function as a parameter of `:scope`, each instance will work in sessions, where the function returns the same value. For example, you can provide a function, returning current user ID, and your instances will be shared with session, where the same user is logged in.

**WARNING:** the single backend instance can have multiple (one per session) browser-side siblings, they will be created any time, when you use this instance as a parameter for browser-side function. When you create an instance on backend, you can provide initargs also for browser-side slots, like `:yyy 20` in the example. This parameters will be supplied each time when the new browser-side instance is created. Also, you can hook `initialize-instance` method with `defmethod-f`, to perform data synchronization between browser-side and backend instances, if needed. The library itself don't provide any synchronization service, it will only guarantee the connection between backend and frontend instances.

Slots marked with `:mirrored t` will present both on backend and browser sides. All instances, created on browser-side, will load values on that slots from the backend. But, only on creation time, and there are no backward (browser-to-backend) synchronization.

The method `(sync-slot (m-object slot))` can be called both on backend and in a browser to synchronize slot value with backend instance. If it called on backend, instances in all active sessions will be updated.

#### Data synchronization between objects

The `OMG` library provides a special m-class `data-sync`, defined in `omgutil` package. This class provides a notification mechanism for browser-side instances of mirrored objects. This class don't provides any real syncronization and even don't specify data storage itself. After each data update, you must call `sync-data` method on backend providing the m-instance as a parameter.  If the browser loses it's server connection, the `sync-data` method will be called after reconnection for all instances of `data-sync` which needed. To make it useful, you have to subclass `data-sync` class, providing a data storage slots and addind `sync-data :after` browser-side method, to perform a real data synchronization.


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

Sometimes you need to return from RPC function immediately to not block JS execution and do some work in background. In this case use `thread-in-session` macro:

```
(thread-in-session
  ;; This code will be executed in a separate thread, but within current session
)
```

To define global server-side variable, which value will be different for different sessions, use `def-session-var` macro:

```
(def-session-var *search-in-progress* nil)
```

The macro will create a symbol-macro with name `*search-in-progress*` and a hash table to store values for each session. Now you can use `*search-in-progress*` as a normal setf-able variable.

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

- `(set-boot code)` - set a boot code with clearing all of the previous added code

### REMOTE-EXEC function

You can use `(remote-exec cmd [nowait])` function to execute any CL code `cmd` in the browser. This function has optional argument `nowait` - set it to `T` if you are not needed to return value(s) and the function will return nil immediately. If there is a timeout occurred while waiting a response, a error condition will be raised, when you call it within a session context. If no current session is set, the code will be executed in all connected browsers and list of return values will be returned.

### Adding a custom HTML into default document body

The default HTML page returned for "/" just contains the main js script link, but you can add any extra HTML into document body, for example to display "Loading..." message:

```
 (add-to-root-html "<span id='loadBanner'>The page is loading, please wait...</span>")
 (add-to-root-head "<title>My application</title>")
 (add-to-boot '(remove-element (js-get-element-by-id "loadBanner")))
```

Also you can use `(set-root-html str)` to set root html with clearing the old one.

### User URI handler

If the library cannot serve http request it will respond with `404` error by default. You can redirect this requests to your function by setting `omg::*user-uri-handler*` variable:

```
(setf omg::*user-uri-handler*
      (lambda (env)
        (let ((uri (getf env :REQUEST-URI)))
          `(200 (:content-type "text/plain")
                (,(format nil "The ~A URI was requested." uri))))))
```

Also you can define a custom responder for a specific path (relative to the `omg::*root-path*`):

```
(add-serve-path "mypage"
                `(200 (:content-type "text/html; charset=utf-8")
                      ("My custom page")
```

### PWA mode

The library can be used to create Progressive Web App (PWA), just by adding one single function call:

```
  (make-pwa (:name "Application Name" :short-name "Application Short Name" :display "standalone" :theme-color "#000000" :background-color "#ffffff" :icon-path "/path/to/icon/file.png")
```

All of the arguments are optional (default values will be used). Only `.png` `.jpeg` and `.gif` image formats of icon file are accepted.

### ServiceWorker

**WARNING:** The ServiceWorker code is in early experimental state, the ServiceWorker can lose all it context sometimes. This will be fixed later.

You can install and use ServiceWorker, just create a service-worker object (must be executed on browser-side):

```
(make-instance 'service-worker)
```

Or just execute a code in a service-worker (the service-worker instance will be created automatically, if needed) with the followin macro:

```
(in-service-worker
  ((jscl::oget (jscl::%js-vref "self") "console" "log") "Hello!"))
```

**NB:** The ServiceWorker is executed in a separate context, so you cannot use any symbols and functions from your main code. Also, the ServiceWorker cannot use syncronous XHR, so it cannot fetch any symbols from backend. So, when you executing a code, all needed functions will be compiled locally and sent to the browser during a call. This means, for example, what if you update a function, which is referenced in the code inside `in-service-worker` macro, you have to recompile all functions, where `in-service-worker` macro using this function has a place, or refresh browser page to promote the changes to the service worker code.

You can use ServiceWorker to catch and handle network requests with the following macro:

```
(set-service-worker-uri-handler (uri req event)
   (cond ((equal (uri-path uri) "/my-path")
          (jslog "Request:" req)
          (respond-with "SOME DATA"))
         (t (default-action)))))
```

In the following macro the utility functions `jslog`, `respond-with` and `default-action` are pre-defined, for your convenience. You can use this macro multiple times, to add several handlers, just don't forget to call `(default-action)` if the request is not handled.

### Classic WebWorker

**NB:** To use Web Workers you have to open your web-page via SSL.

You can run (almost) any code in a separate WebWorker threads, utilizing poor JS multiprocessing capabilities. The macro `omgui:run-in-web-worker` doing the job:

```
(run-in-web-worker (make-instance 'classic-worker)
  (jslog "I'm in the worker!"))
```

The first parameter is a `classic-worker` instance or `nil`. In the second case a free webworker (if any) will be got from the pool or a new one will be started (and will go into the pool after). The macro returns the `classic-worker` instance itself.
The code inside the macro will be executed in a webworker context, but all global and local variables will be available to it. This is one-way availability, any changes performed in main context (if you are not using the caching, see below) will be visible in workers, but if you modify a variable in a worker, it will not be modified in other contexts. After the modification the variable will be "detached" from the main context and worker will not see any its changes anymore.

The function `test` will print "3" in console:
```
(defparameter-f x 1)

(defun-f test ()
  (let ((y 2)) ;; Yes, local variables are also visible!
    (run-in-web-worker nil
      (jslog (+ x y)))))
    ;; Here the execution continues, without waiting for worker code completion
```

To get return values from the webworker, you have to register a callback function with `bind-exit-values-for` macro:

```
(bind-exit-values-for (x y z)
  (run-in-web-worker nil
    (values 1 2 3)))
  (jslog x y z) ;; The "1 2 3" will be printed when the worker ends execution
```

You can call main thread lambdas from a webworker, but you have to register them before with `register-main-lambda` and pass:

```
(let ((l1 (lambda (x y)
            (jslog "Adding" x "and" y) ;; This will be printed in main thread
            (+ x y))))
  (register-main-lambda l1)
  (run-in-web-worker nil
    (jslog "The main thread returns:" (funcall l1 1 2))))          
```

Ofcourse, the only way to pass data to such lambda is parameters, it will be executed in real main-thread context, all varaibles, modified in the worker, will not be visible.

#### Data transferring and caching

The variables visibility between main thread and webworker is achieved by implicit data transfer from main thread to webworker, when you trying to access a variable. This may be an expensive process when data is large, and there are some limitations. The only numbers, strings, symbols, conses (and lists), arrays and MOP-objects are transferred. Lambdas (except ones, registered with `register-main-lambda`) and DOM-objects will be replaced by `nil`, all other types will cause a error. MOP instances are just transferred as a set of their slots, so a new MOP object will be created in webworker (with `allocate-instance`) and all its slots will be filled by the data. Conses, lists, arrays and strings are re-created each time when transferred, so, if you modify their parts, the modifications will gone if you are not using caching.

You can cache the data by yourself:

```
(let ((x '(1 2 3 4 5)))
  (run-in-web-worker nil
    (let ((x x)) ;; The caching, by creating a new variable
      (setf (car x) 100) ;; The modification will persist
      (format t "~A" x))) ;; (100 2 3 4 5) will be printed as expected
  (run-in-web-worker nil
    (setf (car x) 100)
    (format t "~A" x))) ;; (1 2 3 4 5) will be printed here, because x will be requested each time from the main thread
```

Or you can use `cache-vars` pseudo-function as a first element of the code:

```
(let ((x '(1 2 3 4 5)))
  (run-in-web-worker nil
    (cache-vars x) ;; Instruct the system to cache x variable
    (setf (car x) 100)
    (format t "~A" x))) ;; (100 2 3 4 5) will be printed
```

Also you can use `(cache-vars t)` to cache all variables.

The `classic-worker` instance will have no internal state by default, all variables, cached or not, will be reset each time, when you use the same instance multiple times. You can disable this behavior, by setting `:persistent-cache t` when creating the instance:

```
(let ((ww (make-instance 'classic-worker :persistent-cache t))
      (x 100))
  (bind-exit-values-for () ;; Use just to execute a code when worker completes
    (run-in-web-worker ww
      (jslog x) ;; 100 will be printed
      (setf x 200))
    (progn
      (setf x 300)
      (run-in-web-worker ww
        (jslog x))))) ;; 200 will be printed
```

The caching can improve the performance drastically sometimes.
**NB:** with `:persistent-cache t` ALL variables, even local ones, will be cached and saved within the worker state. If you call the same code next time, it will ignore any changes in local variables. If you want to avoid *permanent* cache of the variable, you can add it to the `(cache-vars)` list. It will be cached temporarely, only while the `run-in-web-worker` code is executing, but not on the next call of the same `run-in-web-worker` block.

If you can inherit your class from `dont-transfer` superclass, it instances will not be transferred to the webworkers contexts, a `nil` values will be transferred instead. This may be useful, if your objects, which needs to be transferred, references some instances, which is not.

Also, two useful macros are presented:

```
(when-worker-free ww
  (some-code)) ;; The code will be executed when worker will finish the job (if any)

(when-worker-ready ww
  (some-code)) ;; The code will be executed when worker is spawned
```

You can kill the webworker with method `kill`:

```
(bind-exit-values-for ()
  (run-in-web-worker ww
    (some-code))
  (kill ww)) ;; Kill the worker after completion
```

**WARINING:** The WebWorker code is not well tested, so bugs (especially in different browsers) may exists. Use with caution! Also, be careful with main thread lambdas - if such lambda throws an exception, the worker will hang, consuming some CPU permanently (al least in the current Firefox). Chrome has strange issues with massive parallel worker jobs, they are much slower when running in Firefox. All other browsers are not tested yet, sorry.

### Binary serialization/deserialization

The `omgui` package introduces two browser-side functions for serialization/deserialization almost any lisp objects. Now supported object types are `integer` (32 bit), `real` (32 bit), `symbol`, `cons/list`, `vector/array` and `mop-object`. Data stored in `ArrayBuffer`/`SharedArrayBuffer` objects to save it, for example, in `indexedDB`. JS-objects and lambdas are stored as `nil` values.
Example:

```
(let* ((buf (jscl::make-new (winref "ArrayBuffer") (* 1024 1024))) ;; Allocate 1 MB buffer
       (len (store-to-buffer my-object buf :start 0))) ;; start offset can be provided, amount of data stored is returned
  (format t "Loaded data: ~A (~A bytes used)" (load-from-buffer buf) len)) ;; load data back and show it with amount of used memory in bytes
```

If there is not enough room in the buffer supplied to store an object, `store-to-buffer` will just return a needed size, so you can allocate a 1-byte buffer, call `store-to-buffer`, to determine a needed buffer size, allocate a new buffer of that size and call `store-to-buffer` again.

**NB:** store/load functions retains object equality, but only within a single store/load calls. So, you can store a list, containing multiple references to one mop-object, for example, and, after loading, all the references in the returned list will point to the same object. But, if you call `load-from-buffer` again, all references will point to another instance in the returned list.

If you have to store/load huge objects, you may prevent interface freezes by making all the process asynchronous by using `:background`, `progress-cb` abd `final-cb` options of `store-to-buffer` and `load-from-buffer` functions. When you specify `:background t` option, the process will be executed in background with 0.1 sec chunks (you can supply another time chunk value with `:background 0.5` for example), after each chunk finishing the function  provided with `progress-cb` will be called with single argm equal to current load/store position (in bytes). When the process is done, `final-cb` function will be called. The arguments will be an `ArrayBuffer` (for store-to-buffer) or the loaded object (for `load-from-buffer`) and the last position (in bytes).


### indexedDB

There are some useful macros in `omgui` package to work with IndexedDB:

`setup-indexed-db` (the code will be executed only when database is not exists):
```
(setup-indexed-db (db "MyDatabase")
  (make-instance 'idb-object-store :connection db :name "table1"))
  (make-instance 'idb-object-store :connection db :name "table2" :key "MyKey" :auto-increment t))
```

`indexed-db-add` / `indexed-db-put` - add/modify an object to the store:
```
(indexed-db-add "MyDatabase" "table1" key val ;; Key must be a string, val can be any object, suitable for binary serialization (see above)
  :when-ok (lambda ()
             (jslog "The data is saved!")))))))))))
  :when-err (lambda ()
             (jslog "The data is not saved!")))))))))))
```

`indexed-db-get` - get a value for a key:
```
(indexed-db-get (val ("MyDatabase" "table1" key))
  (jslog "Value loaded:" val)) ;; will be called asynchronously, when data is retrieved.
```

`indexed-db-delete` - delete a key from the store:
```
(indexed-db-delete ("MyDatabase" "table1" key))
  (jslog "The key is deleted!")) ;; will be called asynchronously
```

`indexed-db-get-all-keys` - get e list of all keys in the store
```
(indexed-db-get-all-keys (lst ("MyDatabase" "table1"))
  (jslog "The keys are:") ;; will be called asynchronously
  (map nil #'jslog lst))
```

`if-idb-key` - execute a code when the key exists/not exists:
```
(if-idb-key ("MyDatabase" "table1" key)
  (jslog "Key exists!") ;; will be called asynchronously if the key is exists
  (jslog "Key not exists!")) ;; will be called asynchronously if the key is not exists
```

## OMG daemon mode

**WARNING:** the following functionality is in early alpha version now, it will work only in POSIX compatible environments (Linux, MacOS X) and only with `SBCL`, but easily can be ported to another CL compilers.

OMGlib allows developers to work with code via REPL, but all of the changes are applied immediately, so, you need two copies of your code -- local development version and production one, which is updated less frequently. Each production code update causes a server restart, so all connected clients will lose they connections and will have to reconnect. After the reconnection, browser still has functions from previous version, which may lead to problems.

OMGlib offers a better way to maintain version updating. The special daemon `omgdaemon` is spawned and works as a reverse-proxy, which accepts http connections and, basing on special cookie `OMGVERSION`, connects clients to specific versions. Here is one special version with name `"devel"`, where developer(s) can do any development works, using swank connection via standard port 4008\. When the version will be good enough to be pushed into production, the `(commit-production)` function is called and lisp image is saved to disk and gets an unique version name. After that, all new connections will be redirected to this version and new `"devel"` version will be spawned again. If there was previous versions with clients connected, they will receive notification (the `(commit-notify new-version)` function will be called) so they can offer version update to the clients. Also, the browser-side function `(ensure-last-version)` can be called at boot-time, to check is it a latest version and perform page reload to proceed to the top version if necessary.

The following code can be used to work in `omgdaemon` environment:

```
(defpackage :my-package
  (:use cl omg omgui jscl omgdaemon)) ;; Import omgdaemon

(defparameter-f *page-shown* nil)

(defun-f my-boot () ;; this function will be called after the first WebSocket connection
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

`(omgdaemon::make-docker-image)` accepts two keys - `:tag` for name of the new image (default "omgdaemon") and `:sbcl-version` (default "2.3.1" for now`). The image can be started in the following way to serve on port `7575` on `localhost` and port 4008 for swank-server:

```
docker run -d -p 127.0.0.1:7575:8081 -p 127.0.0.1:4008:4008 omgdaemon
```
