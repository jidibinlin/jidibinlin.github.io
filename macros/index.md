# common lisp macros


<!--more-->

macro 在计算机科学中的意义是语言语法上的拓展。lisp 的 macro非常强大，但是需要多实践。


## macros 是如何工作的 {#macros-是如何工作的}

一个macro 是一段原始的lisp代码，它对另一段假定的lisp代码进行操作，将其转换成更接近于可执行的Lisp代码。有点复杂，举个例子。

```lisp
(setq2 x y e)
```

当lisp解释器看到这段代码时，会将它视为

```lisp
(progn
  (setq v1 e)
  (setq v2 e))
```

实际上，这并不完全正确。macro允许我们通过一段程序，将输入的代码加工成另一个代码


## Quote {#quote}

我们可以这样定义seq2macro:

```lisp
(defmacro setq2 (v1 v2 e)
  (list 'progn (list 'setq v1 e) (list 'setq v2 e)))
```

这段macro接受两个variables 和 一个expression
他会返回一段代码。在lisp中，code是用lists表示的，所以 返回list也就代表一段code.

我们使用了quote符号，一个非常特殊的操作符
每个被quated的对象会被运算成它自己

-   (+ 1 2) evaluates to 3 但是 (quote (+ 1 2)) evalutes to (+ 1 2)
-   (quote (foo bar baz)) evalutes to (foo bar baz)
-   ' 是quote的所缩写 'foo 和 (quote foo 是一样的)

还记得之前说过 '用来保护表达式不被求值。'开头的表达式都会保持原样
所以最后生成的代码是这样的

```lisp
(progn
  (setq v1 e)
  (setq v2 e))
```

这时我们就可以使用他了

```lisp
(defparameter v1 1)
(defparameter v2 2)
(setq v1 v2 3)
;; 3
```


## Macroexpand {#macroexpand}

当你开始写macro的时候，你肯定想知道macro究竟生成了哪些代码。macroexpandkey可以做到这样的事情。

```lisp
(macroexpand '(setq2 v1 v2 3))
;; (PROGN (SETQ V1 3) (SETQ V2 3))
;; T
```

更复杂一点的

```lisp
(macroexpand '(setq2 v1 v2 (+ z 3)))
;; (PROGN (SETQ V1 (+ z 3)) (SETQ V2 (+ z 3)))
;; T
```

可以看到，e并没有被evaluated 而是保持原样。 我们会需要一个comma(,)符号，用来将被保护的表达式求值


## Evaluation Context {#evaluation-context}

在宏定义中，不能再次调用宏，这会导致宏嵌套。如果需要调用某些功能，可以重新定义一个函数，而不是宏。这是因为在执行态时执行的是编译时宏的展开。也就是说，在编译一个函数时，中途遇到了表达式 (setq2 x y (+ z 3)，编译器就会将 setq2 宏展开并编译成可执行状态（如机器语言或是字节码）。也就是说，当编译器遇到 setq2 表达式时，他就要切换去执行 setq2 主体内容。而如果在 setq2 里面再嵌套宏时，编译器就会去处理另一个宏，然后就出不来了，导致 setq2 无法继续执行。

在编译时，所有代码都是可以处理的，但是宏嵌套打破了这个规则
错误示例:

```lisp
(defmacro setq2 (v1 v2 e)
  (let ((e1 (some-computation e)))
    (list 'progn (list 'setq v1 e1) (list 'setq v2 e1))))

(defmacro some-computation (exp) ...) ;; _Wrong!_
```

正确示例:

```lisp
(defmacro setq2 (v1 v2 e)
  (let ((e1 (some-compatation e)))
    (list 'progn (list 'setq v1 e1) (list 'setq v2 e1))))

(defun some-computation (exp) ...) ;; _Right!_
```

我们必须要告诉macro 参数是如何传递给幕后函数的。在函数调用中我们很容易做到这样的事情，我们使用lambda-list 语法，例如&amp;optional, &amp;rest, &amp;key，但是在macro中所有的形参都是macro形式的，而不是他们的值。看下面的例子

```lisp
(defmacro foo (x &optional y &key (ctx 'null)) ...)
```

```lisp
_If we call it thus ..._     |_The parameters' values are ..._
-----------------------------|-----------------------------------
`(foo a)`                    | `x=a`, `y=nil`, `cxt=null`
`(foo (+ a 1) (- y 1))`      |`x=(+ a 1)`, `y=(- y 1)`, `cxt=null`
`(foo a b :cxt (zap zip))`   |`x=a`, `y=b`, `cxt=(zap zip)`
```

你需要清楚的是在宏中变量是不会被求值的，只会保持原样。

看下面的例子

```lisp
(defmacro setq-reversible (e1 e2 direction)
  (case direction
    (:normal (list 'setq e1 e2))
    (:backward (list 'setq e2 e1))
    (t (error "Unknown direction: ~a" direction))))
```

看看他的展开

```lisp
(macroexpand '(setq-reversible x y :normal))
;;(SETQ X Y)
;;T
(macroexpand '(setq-reversible x y :backward))
;;(SETQ Y X)
;;T
```

如果你传递了一个错误的参数, 宏展开就会报错

```lisp
(macroexpand '(setq-reversible x y :other-way-around))
```

我们可以使用backquote 和 comma 来解决宏展开时报错的问题

```lisp
(defmacro setq-reversible (v1 v2 direction)
  (case direction
    (:normal (list 'setq v1 v2))
    (:backward (list 'setq v2 v1))
    (t `(error "Unknown direction: ~a" ,direction))))
    ;; ^^ backquote                    ^^ comma: get the value inside the backquote.

(macroexpand '(SETQ-REVERSIBLE v1 v2 :other-way-around))
;; (ERROR "Unknown direction: ~a" :OTHER-WAY-AROUND)
;; T
```

使用宏的时候传入错误的方向还是会报错，但是在展开宏的时候，不会报错。


## Backquote and comma {#backquote-and-comma}

backquote(\`)字符表明，在他后面的expression,任何不以comma为前缀的表达式都会被quoted,而以comma为前缀的将会被evaluate

```lisp
  `(progn (setq ,v1 ,e) (setq ,v2 ,e))
;;^ backquote   ^   ^         ^   ^ commas
```

```lisp
`(v1 = ,v1) ;; => (V1 = 3)
```


### comma-splice ,@ {#comma-splice}

,@会将（本来应该是列表的）参数展开。将列表的元素插入模板来取代列表

```lisp
(setf lst '(a b c))
;; => (A B C)

`(lst is ,lst)
;; => (LST IS (A B C))

`(its elements are ,@lst)
;; => (ITS ELEMENTS ARE A B C)
```


### Quote-comma ', {#quote-comma}

如果想把表达式的字面打出来，我们需要使用',

```lisp
(defmacro explain-exp (exp)
  `(format t "~s = ~s" ',exp ,exp))

(explain-exp (+ 2 3))
;; (+ 2 3) = 5
```


## Gensym {#gensym}

如果想创建零时变量，我们使用gensym function. 他会返回一个全新的变量，并且不会在别的地方出现

```lisp
(defmacro setq2 (v1 v2 e)
  (let ((tempvar (gensym)))
    `(let ((,tempvar ,e))
       (progn (setq ,v1 ,tempvar)
              (setq ,v2 ,tempvar)))))
```

现在 (setq2 x y (+ x 2)) 会被展开成

```lisp
(let ((#:g2003 (+ x 2)))
  (progn (setq x #:g2003) (setq y #:g2003)))
```

