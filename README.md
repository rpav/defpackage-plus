# defpackage-plus

DEFPACKAGE-PLUS is an *extensible* `DEFPACKAGE` variant with some
utilities useful for versioning.

A common occurrence: you want to alter your API in some incompatible
way.  Change how a function works.  Alter a default.  Not a huge
change, but it would be a big improvement.  You have a few options:

*  **Just do it**: Break everything else.  Your new feature is great, but
   your users are upset.
*  **Don't do it**: It was a great idea, but too bad... you can't
   afford to change anything, ever.
*  **New system**: Release an entirely new system, hope people notice.
   You continue development on the new system, but people are still
   using the old stuff, so now you're maintaining two... or more.

This is where `defpackage-plus` comes in:

```lisp
(defpackage+ :system-1.0
  ...
  (:export #:function-1 #:function-2 #:function-3))

(defpackage+ system-1.1
  ...
  (:inherit-except :system-1.0 #:function-3)
  (:export #:function-3))
```

This does nothing fancy: it simply imports *and* exports all external
symbols from `:system-1.0`, except `FUNCTION-3`.  At this point, a
*different* `FUNCTION-3` is exported, and it may be defined in
whatever new way that's desired.

Both versions may exist simultaneously, even share code, and vary in
whatever way the developer needs.  Simple, but effective.

For more complex uses, see *Idioms* below.

## defpackage+

The `DEFPACKAGE+` macro may be used similarly to `DEFPACKAGE`.  The
package `:defpackage+-user-<version>` is provided for easily accessing
this and other defpackage-plus functionality.  For example:

```lisp
(in-package :defpackage+-user-1.0)

(defpackage+ :my-package
  (:use #:cl #:alexandria)
  (:export #:some-symbol))
```

### Options

The following options are available:

* `:use PACKAGES...`: `PACKAGES` are inherited as per `USE-PACKAGE`.
  Notably, this is different from `defpackage`'s `:use`, because it
  only *adds* packages.
* `:use-only PACKAGES...`: `PACKAGES` are inherited *exclusively*; any
  packages not named are removed as per `UNUSE-PACKAGE`.  This is like
  `defpackage`'s `:use`.
* `:export SYMBOLS...`: Ensure and export `SYMBOLS`.  Unlike
  `defpackage`, it is not an error or warning for this list to be
  incomplete.
* `:export-only SYMBOLS...`: Ensure and export `SYMBOLS`.  Any symbols
  not named will be *unexported*.
* `:export-warning SYMBOLS...`: Ensure and export `SYMBOLS`.  If there
  are symbols already exported that are not named, issue a warning.
* `:documentation STRING`: Set the package documentation string to
  `STRING`.
* `:inherit-from PACKAGE SYMBOLS...`: Import `SYMBOLS` from `PACKAGE`,
  and also export them.
* `:inherit PACKAGE`: Import *all* symbols external to `PACKAGE`, and
  also export them.
* `:inherit-except PACKAGE SYMBOLS...`: Import all symbols external to
  `PACKAGE` *except* those named by `SYMBOLS`.  (Note that this will
  not *remove* symbols *previously imported*.)
* `:import-from PACKAGE SYMBOLS...`: Import `SYMBOLS` from `PACKAGE`.
* `:shadow SYMBOLS...`: Ensure and add `SYMBOLS` to the shadowing
  symbols list.
* `:shadowing-import-from PACKAGE SYMBOLS...`: Import `SYMBOLS` from
  `PACKAGE`, and add the to the shadowing symbols list.
* `:nicknames NAMES...`: Make `NAMES` the nicknames.  Nicknames not
  named will be removed.
* `:intern SYMBOLS...`: Intern `SYMBOLS` if they are not already
  accessible.

### Differences with `defpackage`

For the most part, `defpackage+` is simply a more convenient
`defpackage`.  You can likely use them interchangeably.  However,
there are some notable differences:

* **Order is significant**:  Unlike `defpackage`, which allows you to
  specify options in any order, and executes them in a specific order,
  `defpackage+` executes options *in the order given*.
* Some options such as `:use` and `:export` have more permissive
  behavior.
* The `:use` list is by default `NIL`.  You must specify all packages,
  such as `CL`.
* There is no `:size`.
* There may be other effective differences due to the above.

### Extensibility

All options to `defpackage+` are implemented via calls to
`DEFPACKAGE+-DISPATCH`:

    defpackage+-dispatch OPTION PARAMS PACKAGE

`OPTION` is the `car` of the option, `PARAMS` is the `cdr`, and
`PACKAGE` is the *name* of the package as specified to `defpackage+`.
Specialize on `OPTION`.  Keywords and symbols from `COMMON-LISP` are
*reserved* by `defpackage-plus` for internal use.

Note, this function does *not* return an expansion as for a macro.
Rather, `defpackage+` itself expands into a series of calls to this
function.  This makes extensibility much easier.

```lisp
(defpackage+ :my-package
  (:use #:cl)
  (:export #:symbol))

;; expands to:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage+-dispatch ':use '(#:cl))
  (defpackage+-dispatch ':export '(#:symbol)))
```

## Utilities

Numerous additional functions are provided (many of which are the
actual implementation of `defpackage+`):

* `ensure-use-only`, `ensure-package`, `ensure-nicknames`,
  `ensure-export`, `ensure-export-only`, `ensure-export-warning`:
  These are all functions which ensure the relevant package state
  (including `ensure-package`, which ensures the package named
  exists).
* `import-from`, `shadowing-import-from`:  These provide a "-FROM"
  form to complement the Common Lisp functions.

* `inherit-from-package`, `inherit-package`, `inherit-package-except`:
  These provide import/export functionality.

* `package-external-symbols`, `package-symbols`: These return new
  lists of external symbols and all symbols, respectively, to
  complement the Common Lisp `DO-` forms.

## Idioms

There is nothing specific about versions provided per se, but
following the stated idiom should provide your users with a stable API
as well as the ability to improve the API without conflicts.

### Versioning

Simply append an appropriate version number to your package.  When you
wish to introduce significant (i.e., API-incompatible) changes, simply
create a new package with `defpackage+` with a higher version, and use
the various `:inherit` options to push forward anything that will
*not* change.

For example:

```lisp
(defpackage+ my-package-1.0
  (:use #:cl)
  (:export function-1 function-2))

(defpackage+ my-package-1.1
  (:use #:cl)
  (:inherit-from :my-package-1.0 #:function-1)
  (:export #:function-3))
```

In this case `FUNCTION-3` may be an entirely different interface
intended to replace `FUNCTION-2`.  However, `FUNCTION-1` will remain
unchanged and available.

Users will simply do the following:

```lisp
(defpackage+ user-package-1.0
  (:use ... #:my-package-1.1))
```

This will ensure they see only the relevant `1.1` symbols.  Of
course, users using `defpackage+` is not strictly necessary, but it
may be useful in the following examples.

Not all users may, of course, wish to import all of your symbols into
their packages, for all the regular reasons.  And typing
`my-package-1.1:` before every symbol may be a bit onerous to some.
The following is also a possibility:

```lisp
(defpackage+ user-apis-1.0
  ;; Alternatively we could simply (:inherit #:my-package-1.1 #:some-other-3.2)
  (:inherit-from #:my-package-1.1 symbol-1 ...)
  (:inherit-from #:some-other-3.2 another-symbol ...))

(defpackage+ :user-package-1.0
  (:use ... :user-apis-1.0))
```

There are many ways to combine this based on preference, of course.
If package-local nicknames are available, this may provide the
cleanest option, but not all Lisps support this.

### What not to do

**Leaving off version numbers.** It may occur to some people to simply
maintain "old" versions with version numbers, and the "latest" version
without:

```lisp
;;; This is a TERRIBLE idea.  You just defeated the point:
(defpackage+ my-package
  (:inherit-from :my-package-3.2 ...))

(defpackage+ my-package-3.2
  (:inherit-from :my-package-3.1 ...))

...
```

This resolves nothing; changing the current package will still break
everyone's code, defeating the point.  Requiring people to figure out
after the fact which version works with their code is even worse.
You change things wildly.  Much code rot ensues.

**Importing everything always.** It may occur to you to do the
following, if you are unfamiliar with how packages and symbols work:

```lisp
(defpackage my-package-1.0
  ...
  (:export #:function-1))

(defpackage my-package-1.1
  ...
  (:inherit my-package-1.0))

;;; This is BAD.  You just broke 1.0:
(in-package :my-package-1.1)

(defun function-1 (...)
  ...)
```

The fact `FUNCTION-1` is being redefined while "in" `MY-PACKAGE-1.1`
is irrelevant: `FUNCTION-1` is still imported from `MY-PACKAGE-1.0`,
and you are *redefining the old function* here.  What you *should*
have done was `:inherit-except` and exported a new version.
