# OMGlib

## A Common Lisp library to build fully dynamic web interfaces

The main idea of this library is to vanish a border between frontend and backend as much as possible. You can develop a CL application using REPL and SLIME on a backend, but some of you functions and macros can be declared as _browser-side_ (_b-s_) with `defun-f` and `defmacro-r` respectively:

```
(defpackage mytest (:use cl omg))
(in-package :mytest)
(defun-r js-add (x y)
  (+ x y) ;; this code will be executed in a browser
```

The `(start-server)` function starts `clack` web server on default port 7500 (can be changed by setting `omg::*port*` variable). Now, in the html-code you can inject the following line:

```
<script src='http://localhost/j' type='text/javascript'></script>
```

or just open a page <http://localhost/> (a blank page with js-injection code).

Now you call a browser-side function, just executing it in REPL, for example:

```
MYTEST> (js-add 1 2)
;; (3)
```

The function call will be converted to javascript with [JSCL](https://github.com/jscl-project/jscl) and sent to the browser via websocket, where the code will be executed and result will be returned to the backend. The result in this example will be returned as a list, because we are not specified a session (see below) and the code will be executed in _all_ connected browsers, and the list of result values will be returned.

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

## How it works

There are the following macros to define browser-side functions, macros and variables:

- `defun-f`
- `defmacro-f`
- `defvar-f`
- `defparameter-f`

Also you can define RPC-functions, with

- `defun-r`

### Browser-side functions

`defun-r` has similar syntax as a `defun`, but defines macro with the same name, instead of function. The macro has to check its arguments and evaluate only some of them on backend, passing the rest to the browser-side as is. For example:

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

Browser-side macros are called while code is compiled to JS and must be evaluated on the browser side. So, if you are use such macros while the `omg::*local-compile*` is set to `T` (by default), JSCL will parse the code and execute code of each macro in the browser, get the results and finish the compilation using them. This means that while local compilation is enabled, macro expansion will be rather expensive, especially, if you are using recursive macros.

### Browser-side variables and parameters

You can declare browser-side variables and parameters with `defvar-f` and `defparameter-f`. They a similar to standard `defvar` and `defparameter`, but the symbols will be available to browser-side functions. This is one-way availability: if the variable will be changed on browser-side, backend will see no update, also, the data will not be updated on other connected browsers. If you are changing such variable or parameter on the backend, the changes will be seen only for newly connected browsers or by browser, which are not fetched they yet.

### RPC functions

Due to security reasons, you can call only some specially marked functions of backend from browser side. You can declare them with `defun-r` macro, which acts as standard `defun`, but places the function name in the list of allowed RPC functions. The RPC function can be simply called on browser-side as any other function.

### Sessions

Each connected browser starts a new _session_ which is determined by unique random symbol - _session ID_. When RPC-function is called from browser-side, it will be executed in the session context, so, it will execute all bs-functions in the specific browser. You can implicitly set the current session by executing a code inside `with-session` macro:

```
(with-session (find-session 'ASDXCASA) ;; find a session object by ID
  (some-bs-function) ;; will be called in session 'ASDXCASA
```

If you are executing bs-function without session, it will be executed in **ALL** connected browsers and a list of results will be returned.

### REMOTE-EXEC function

You can use `(remote-exec cmd [nowait])` function to execute any CL code `cmd` in the browser. This function has optional argument `nowait` - set it to `T` if you are not needed to return value(s) and the function will return nil immediately.
