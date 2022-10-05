# Common-lisp Error and Exception Handling


<!--more-->

这一节后半段需要CLOS的知识, 所以为了不产生疑惑，在读到define-condition的时候先去看看CLOS

-   [CLOS](https://lispcookbook.github.io/cl-cookbook/clos.html)


## Ignoring all errors, returning nil {#ignoring-all-errors-returning-nil}

有时候你知道函数会失败，并且你想忽略这个错误，可以使用ignore-errors

```lisp
(ignore-errors
  (/ 3 0))
                                        ; in: IGNORE-ERRORS (/ 3 0)
                                        ;     (/ 3 0)
                                        ;
                                        ; caught STYLE-WARNING:
                                        ;   Lisp error during constant folding:
                                        ;   arithmetic error DIVISION-BY-ZERO signalled
                                        ;   Operation was (/ 3 0).
                                        ;
                                        ; compilation unit finished
                                        ;   caught 1 STYLE-WARNING condition
NIL
#<DIVISION-BY-ZERO {1008FF5F13}>
```

3/0 是一个错误，我们收到了警告。但是函数会正常返回nil，函数执行的状态被返回，失败了。


## Catching any condition (handler-case) {#catching-any-condition--handler-case}

ignore-errors是建立在handler-case上的。我们可以通过捕获err重写上面的例子,但是我们可以返回我们想返回的东西

```lisp
(handler-case (/ 3 0)
  (error (c)
    (format t "We caught a condition.~&")
    (values 0 c)))
                                        ; in: HANDLER-CASE (/ 3 0)
                                        ;     (/ 3 0)
                                        ;
                                        ; caught STYLE-WARNING:
                                        ;   Lisp error during constant folding:
                                        ;   Condition DIVISION-BY-ZERO was signalled.
                                        ;
                                        ; compilation unit finished
                                        ;   caught 1 STYLE-WARNING condition
We caught a condition.
0
#<DIVISION-BY-ZERO {1004846AE3}>
```

可以看到我们返回了0 和 c
handler-case的通用模板是

```lisp
(handler-case (code that errors out)
  (condition-type (the-condition) ;; <-- optional argument
    (code))
  (another-condition (the-condition)
    ...))
```

我们也可以用t 来捕获条件

```lisp
(handler-case
    (progn
      (format t "This won`t work...~%")
      (/3 0))
  (t (c)
    (format t "Got an exception: ~a~%" c)
    (values 0 c)))
;; …
;; This won't work…
;; Got an exception: arithmetic error DIVISION-BY-ZERO signalled
;; Operation was (/ 3 0).
;; 0
;; #<DIVISION-BY-ZERO {100608F0F3}>
```


## 捕获一个特殊的条件 {#捕获一个特殊的条件}

我们可以指定哪些条件需要处理

```lisp
(handler-case (/ 3 0)
  (division-by-zero (c)
    (format t "Caught division by zero: ~a~%" c)))
;; …
;; Caught division by zero: arithmetic error DIVISION-BY-ZERO signalled
;; Operation was (/ 3 0).
;; NIL
```

这个和try/catch形式非常像，但是我们可以做更多的事情


## handler-case vs handler-bind {#handler-case-vs-handler-bind}

handler-case和其他语言的try/catch非常像
handler-bind 用在哪些当错误出现时我们需要绝对控制的情况下。他允许我们交互的使用debugger和编程化的restart,


## defining and make conditions {#defining-and-make-conditions}

我们可以使用define-condition 定义condtions，使用make-condition 初始化他们

```lisp
(define-condition my-division-by-zero (error)
  ())

(make-condition 'my-division-by-zero)
;; #<MY-DIVISION-BY-ZERO {1005A5FE43}>
```

定义condition时，最好给出更多的信息

```lisp
(define-condition my-division-by-zero (error)
  ((dividend :initarg :dividend
             :initform nil
             :reader dividend)) ;; <-- we'll get the dividend with (dividend condition). See the CLOS tutorial if needed.
  (:documentation "Custom error when we encounter a division by zero.")) ;; good practice ;)
```

