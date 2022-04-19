# common lisp 函数


<!--more-->


## 定义一个函数 {#定义一个函数}

可以使用defunc 关键字定义一个有名字的函数:

```lisp
(defun <name> (list of arguments)
  "docstring"
  (function body))

```

用学语言必学的打印hello world! 来举例

```lisp
(defun hello-world () ;;  define a function named hello-world
  (format t "hello world!")) ;; print hello world! and return nil
```

调用

```lisp
(hello-world)
;; "hello world!" <-- output printed by `foramt' function
;; nil <-- return value return by format
```

{{< admonition type="tip" title="why docstring" open="true" >}}
common lisp 可以再函数体开始之前 改写 **docstring** 为函数添加文档
lisp习惯使用这种方式来给函数写一些帮助文档，类似readme这种自述文件
docstring 是对函数的描述
{{< /admonition >}}

{{< admonition type="tip" title="about the return value" open="true" >}}
common lisp 的函数默认会return 最后一个表达式的值 再这里就是

```lisp
(format t "hello world!")
```

的返回值也就是nil
{{< /admonition >}}


## 函数返回值 {#函数返回值}


### required arguments 函数的必要参数 {#required-arguments-函数的必要参数}

先看这个函数定义

```lisp
(defun hello (name) ;;name 就是必要参数
  "say hello to `name'."
  (format t "hello ~a !~&" name) ;; 这里直接使用name参数
  )
```

直接调用

```lisp
(hello "me")
;; hello me! <-- printed by `format'
;; Nil <-- return value
```

{{< admonition type="tip" title="format 的格式控制符" open="true" >}}
common lisp format 类似c语言的printf。但是common lisp的格式控制符是以 ~ 为开头的 而且字符的意义也不同 比如C语言中 \n 代表换行 而format中 ~&amp;就代表换行符。
{{< /admonition >}}


### Optional arguments 函数的可选参数: &amp;optional {#optional-arguments-函数的可选参数-and-optional}

可选参数是定义再 _&amp;optional_ 关键字后面。并且要保持有序，必须一个跟着一个出现。这里有点懵逼吧。废话不多说，直接上例子。

```lisp
(defun hello (name &optional age gender)
  (format t "name: ~a age ~a gender ~a ~&" name age gender) )

```

这条函数定义中 name 是必要函数，age 和 gender是可选参数。name是必填的，就是你在调用这个函数的时候，name是必须的。而 age 和 gender 你可以选择提供或者省略。但是当你想提供 gender 这个参数的时候，你必须也要提供 age 这个参数。不严谨的说，一个可选参数要想出现，就必须建立在他前面的一个可变参数已经提供的情况下。

```lisp
(hello "me") ;; supply required argument name. avoid optional arguments age and gender
(hello "me" 7) ;; supply required argument name, optional argument age and void gender
(hello "me" 7 "female") ;; supply name age and gender
(hello "me" "female") ;; wrong for practice。 You may use hello in this way grammaly but female won`t supplied to the gender arguments
```

如果可选参数再调用的时候没有绑定值 那值就为 nil

```lisp
(hello "me")
;; name: me age NIL gender NIL
```


### Named paramenters 具名参数: &amp;key {#named-paramenters-具名参数-and-key}

有时候记住参数的顺序非常不方便，lisp 提供了使用参数名来提供参数的方式. 在&amp;key 后面跟上参数名即可定义具名参数

```lisp
(defun hello (name &key happy)
  "if `happy' is `t,print a smiley"
  (format t "hello ~a" name)
  (when happy
    (format t ":) ~&")))
```

调用的时候用 :name value 这样的形式指定参数,定义了具名参数但是调用的时候不指定value 那具名参数的value 会为nil

```lisp
(hello "me") ;; ignore the happy paramenter,happy will be set to nil defaultly
(hello "me" :happy t) ;; bind happy to t
(hello "me" :happy nil) ;; bind happy to nil
(hello "me" :happy) ;; wrong! this form is definitely wrong
```

{{< admonition type="tip" title="lisp中的 t and nil" open="true" >}}
lisp 中用 t 代表true nil 代表false 和 空
{{< /admonition >}}

{{< admonition type="tip" title="有关函数参数数量的想法" open="true" >}}
具名参数是为了解决记住参数的顺序不方便而诞生的，但是如果函数的参数过多的话，会导致函数的行为会变得复杂多样。clean code 中并不鼓励函数参数的数量超过3个
{{< /admonition >}}

你也可以定义多个具名参数

```lisp
(defun hello (name &key happy lisper cookbook-contributor-p) ...)
```

使用示例

```lisp
(hello "me" :lisper t)
(hello "me" :lisper t :happy t)
(hello "me" :cookbook-contributor-p t :happy t)
```

{{< admonition type="tip" title="keys can be variable" open="true" >}}
类似 :happy 这样的符号其实可以作为一个变量的值就像这样。

```lisp
(let ((key :happy) ;; bind :happy to key
      (val t)) ;; bind t to val
  (hello "me" key val)) ;; quote key and  val
```

let 可以声明一些只能在let代码快中使用的变量这个后面会细说
{{< /admonition >}}


### 混合可选和具名参数 {#混合可选和具名参数}

先看一个例子

```lisp
(defun hello (&optional name &key happy)
  (format t "hello ~a" name)
  (when happy
    (format t ":)~&")))
```

如果你直接这么定义函数，lisp的解释器会报warning

```lisp
; in: DEFUN HELLO
;     (SB-INT:NAMED-LAMBDA HELLO
;         (&OPTIONAL NAME &KEY HAPPY)
;       (BLOCK HELLO (FORMAT T "hello ~a " NAME) (WHEN HAPPY (FORMAT T ":)~&"))))
;
; caught STYLE-WARNING:
;   &OPTIONAL and &KEY found in the same lambda list: (&OPTIONAL (NAME "John") &KEY
;                                                      HAPPY)
;
; compilation unit finished
;   caught 1 STYLE-WARNING condition
```

虽然也能调用

```lisp
(hello "me" :happy t)
;; hello me :)
;;nil
```

{{< admonition type="warning" title="这种情况不能直接跳过optional的参数 直接指定key 参数" open="true" >}}
还是上面这个函数

```lisp
(defun hello (&optional name &key happy)
  (format t "hello ~a" name)
  (when happy
    (format t ":)~&")))
```

如果你尝试省略name 直接指定 :happy 就会报错

```lisp
(hello :happy t)
;; odd number of &KEY arguments
```

因为optional 的参数必须是有序出现的，如果前面的参数未出现，就不能直接指定后面的参数。但是当指定完所有的optional参数后，你就可以按照key参数的规则去指定key参数
看这个函数定义(一个不恰当的例子)

```lisp
(defun hello (&optional name &key happy age)
  (format t "hello ~a " name)
  (when happy
    (format t ":)~&")))
```

```lisp
(hello "me" :age 1)
```

这里直接忽略happy 关键字 指定age
{{< /admonition >}}


### key parameters 的默认值 {#key-parameters-的默认值}

可以使用一对包含键值的括号来指定默认值 (happy t)

```lisp
(defun hello (name &key (happy t)))
```

这样happy 的默认值就被设成了t


### 如何确定具名参数(key parameter) 是否被指定 {#如何确定具名参数--key-parameter--是否被指定}

前面有提到过，具名参数如果不指定，就会默认为NIL。那问题就是，如何知道具名参数的调用者是否故意指定这个参数为NIL呢。因为参数为NIL可能会有自己特殊的意义，所以需要判断是否是调用者故意设为NIL.
我们使用二元组 tuple 设置默认值

> &amp;key (:happy t)

我们可以使用三元组 triple来解决参数知否被故意指定为NIL

> &amp;key (:happy t happy-p)

```lisp
(defun hello (name &key (happy nil happy-p))
  (format t "Key supplied? ~a~&" happy-p)
  (format t "hello ~a " name)
  (when happy-p
    (if happy
        (format t ":)")
        (format t ":("))))
```

```lisp
(hello "me" :happy t)
;; Ky supplied? T
;; hllo me :)
```

从返回的结果可以看到 如果指定了:happy 那么happy-p 会被自动设置为 t 即被指定状态

{{< admonition type="tip" title="lisp 中有关断言命名的convenstion(传统)" open="true" >}}
lisp 中习惯对用来做判断的函数或变量后面加一个p 比如

-   stringp :用来判断是否为字符串的函数
-   listp : 用来判断是否为列表类型的函数

p是predicate的缩写 意为断言
{{< /admonition >}}


### 可变数量参数: &amp;rest {#可变数量参数-and-rest}

的时候，你会想要定义一些函数，可以接受很多个参数，但是具体多少个，你也不清楚。比如你想定义个函数把所有的参数都相加进行求和。

```lisp
(defun sum (arg1 arg2 arg3 ... to arg100....))
```

这样太累了，你不可能一个一个的敲。我们可以这样

```lisp
(defun hello (&rest numbers)
  (apply #'+ numbers))
```

调用

```lisp
(hello 1 2 3 4)
;; 10 <-- returned by hello
```

{{< admonition type="tip" title="apply 函数" open="true" >}}
apply 的第一个参数接收一个拥有两个参数的回调函数 第二个参数接受一个列表（这里为了方便理解代码这么说，其实这是不严谨的）
就像这样

```lisp
(apply #'func '(arg1 arg2 arg3))
```

arg1 arg2 arg3 将会作为func 的参数传递给func 并进行运算. 也就是说 apply的操作是将 list 展开成一个个参数并传给func。

比如(apply #+ '(1 2 3 4)) 和 (+ 1 2 3 4) 是等价的。 列表 '(1 2 3 4) 被展开成 1 2 3 4 传递给了func
{{< /admonition >}}


### 允许额外的key: &amp;allow-other-keys {#允许额外的key-and-allow-other-keys}

先看下下面的例子

```lisp
(defun hello (name &key happy)
  (format t "hello ~a~&" name))
(hello "me" :lisper t)
;; Error: unknown keyword argument
```

再看看这个

```lisp
(defun hello (name &key happy &allow-other-keys)
  (format t "hello ~a~&" name))

(hello "me" :lisper t)
;; hello me
```

这里我们并没有给出 :lisper 这个关键字的定义 但是函数仍然允许我们这么调用而不是直接报错

《common lisp cook book》 中给出过一个实用的案例

```lisp
(defun open-supersede (f &rest other-keys &key &allow-other-keys)
  (print  other-keys)
  (apply #'open f :if-exists :supersede other-keys))
```

这个函数的定义要求必须有一个f参数，以及任意数量的other-keys 其实 &amp;key 以及&amp;allow-other-keys 被忽略了只做提示用（告诉调用者可以接受其他具名参数）。因为有&amp;rest 这个标志符在 除了f外 参数都会传到other-keys里面。所以&amp;key &amp;allow-other-keys就失去了语法上的意义。起码在编译器看来，他是无意义的。
但是如果你不按照函数签名指示的方式调用，内层的函数就很有可能报错，所以即使&amp;key &amp;allow-other-keys被编译器忽略(这种函数在定义时，甚至可以忽略&amp;key &amp;allow-other-keys关键字，功能并不会因为没有他们而受影响)，你也应该按照签名去调用函数。因为内层的函数需要依赖这个签名规则。

来看这个函数调用

```lisp
(open-supersede "test.log" :if-does-not-exist :create)
;;(:IF-DOES-NOT-EXIST :CREATE) <<-- 由(print other-keys)打印 可以看到具名参数都被传给了&rest
```

{{< admonition type="tip" title="必要的函数文档" open="true" >}}
在写这种函数的时候，应该写上docstring 因为函数签名已经不能很好的描述函数了，我们应该加上docstring来描述函数，不要让调用者疑惑。
{{< /admonition >}}


## 函数返回值 {#函数返回值}

common lisp 的返回值默认是函数的最后一个执行语句
你也可以使用(return-from &lt;function name&gt; &lt;name&gt; &lt;value&gt;) 显示的从具体函数内返回。注意这里是直接从&lt;function name&gt; 指定的函数中返回,不仅仅是从当前运行的函数中返回，也有可能直接从外层函数返回。

多数时候我们并不使用return-from


### 多返回值：values,multiple-value-bind and nth-value {#多返回值-values-multiple-value-bind-and-nth-value}

我们使用values来构造一个多返回值

```lisp
(defun foo (a b c)
  (values a b c))
```

因为函数的最后一条语句的结果会被返回，所以values构造的多返回值会被直接返回给上层

```lisp
(setf res (foo :a :b :c))
;; :A <<-- res 为:A
```

这里res为:A而不是 :A :B :C 是因为这里的返回值接受者只有res 所以只有第一个:A 被接受 :B :C 都被自动忽略掉了. 接受多返回值得方法为multiple-value-bind。


#### multiple-value-bind {#multiple-value-bind}

使用multiple-value-bind来解构多返回值。

```lisp
(multiple-value-bind (res1 res2 res3) (foo :a :b :c)
  (format t "res1 is ~a, res2 is ~a, res3 is ~a ~&" res1 res2 res3))
                                        ;res1 is A, res2 is B res3 is C
                                        ;nil
```

通用格式

```lisp
(multiple-value-bind (var-1 .. var-n) expr
  body)
```

{{< admonition type="tip" title="multiple-value-bind绑定的变量是局部的" open="true" >}}
使用multiple-value-bind的变量再它外面是访问不到的, 也就是说上面的例子中 res1 只能再(multipe-value-bind)的内部使用。而无法再外部使用
{{< /admonition >}}


#### nth-value {#nth-value}

nth-value 是可以直接拿到指定索引的返回值

```lisp
(nth-value 0 (values :a :b :c))  ;; => :A
(nth-value 2 (values :a :b :c))  ;; => :C
(nth-value 9 (values :a :b :c))  ;; => NIL
```

但是如果将 nth-value 用在List上，结果就不一样了

```lisp
(nth-value 0 '(:a :b :c)) ;; => (:A :B :C)
(nth-value 1 '(:a :b :c)) ;; => NIL
```

{{< admonition type="note" open="true" >}}
(values) 将不会返回任何值
{{< /admonition >}}


#### multiple-value-list {#multiple-value-list}

multiple-value-list 将返回值构造成list

```lisp
(multiple-value-list (values 1 2 3))
;; (1 2 3)
```

相反的操作有将list 构造成多返回值

```lisp
(value-list '(1 2 3))
```


## 匿名函数 lambda {#匿名函数-lambda}

使用lambda函数创建匿名函数

```lisp
(lambda (x) (print x))
```

我们可以用funcall 或者 apply 来调用匿名函数
如果一个未被引用的括号表达式内部第一个元素是一个匿名函数，那么这个匿名函数就会被调用

```lisp
((lambda (x) (print x)) "hello")
```

{{< admonition type="tip" title="引用" open="true" >}}
lisp中 引用是指以'为开头的表达式或符号。'将保护表达式或符号维持其本身。

比如(func arg) 这种形式的表达式将会自动被当做函数执行,有些情况我们并不想让他执行（比如传一个list 给函数做参数的时候的时候）

```lisp
(defun hello (arg))
(hello (1 2 3))
;; illegal function call
```

这里(1 2 3) 将会被求值 并不会真的传一个(1 2 3) 给arg且会报

<div class="note">

illegal function call

</div>

这样的错误。因为没有被' 保护的函数会被自动当成函数并且运算。 所以正确的形式应该是

```lisp
(defun hello (arg))
(hello '(1 2 3))
```

'就是保护表达式或变量维持其本身
{{< /admonition >}}


### 使用funcall 和 apply 调用函数 {#使用funcall-和-apply-调用函数}

funcall 和 apply 类似，都是将参数规整成正确的形式然后传给回调函数

```lisp
(funcall #'func arg1 arg2 .. argn)
;;          ^^ 回调        要传给func的参数
```

不同点在于， funcall 不会自动展开list 列表 而apply 可以将列表展开成一个个参数传给回调func

```lisp
(funcall #'func arg1 arg2 '(arg3 arg4));; '(arg3 arg4) 会原封不动的传给func
(apply #'func arg1 arg2 '(arg3 arg4));; '(arg3 arg4) 会被展开成 arg3 arg4 最后的形式就是 (apply #'func arg1 arg2 arg3 arg4)
```


### 使用 single quote ' 还是 sharpsign-quote #' 来引用函数? {#使用-single-quote-还是-sharpsign-quote-来引用函数}

single quote ' 和 sharpsign-quote #'的不同在于 #' 使用的是词法作用域(lexical scope) 会更安全点

```lisp
(defun foo (x)
  (* x 100))

(flet ((foo (x) (1+ x)))
  (funcall #'foo 1))
;; => 2, as expected
;;
;; But:

(flet ((foo (x) (1+ x)))
  (funcall 'foo 1))
;; => 100
```

{{< admonition type="note" title="lexical scope" open="true" >}}
词法作用域的意思是，当找一个变量或函数的时候（函数此时作为值进行传递）会默认去定义的地方去找而不是再函数运行的环境中去找。

上面的foo 一个是在外面声明的 一个是使用flet 保护起来的foo 对于funcall 来说 #’会去找flet 出来的foo 因为词法作用域要求在定义的地方去找需要的函数。

single quote ' 使用的是动态作用域 会默认去运行环境中找所以 即使第二个flet 也声明了 foo 但是funcall 的时候还是去到外层寻找foo
{{< /admonition >}}

\#' 其实就是(function ... )的语法糖

```lisp
(function +)
;; #<FUNCTION +>

(flet ((foo (x) (1+ x)))
  (print (function foo))
  (funcall (function foo) 1))
;; #<FUNCTION (FLET FOO) {1001C0ACFB}>
;; 2
```


## 高级函数 能够返回函数的函数 (functions that return functions) {#高级函数-能够返回函数的函数--functions-that-return-functions}

```lisp
(defun adder (n)
  (lambda (x) (+ x n)))
```

这样就定义了一个能够返回一个函数对象的adder函数

要想调用这个函数对象我们需要funcall 或者 apply

```lisp
(adder 5)
;; #<CLOSURE (LAMBDA (X) :IN ADDER) {100994ACDB}>
(funcall (adder 5) 3)
;; 8
```

如果你想以正常的思维去调用这个函数对象，会报错

```lisp
((adder 3) 5)
;;In: (ADDER 3) 5
;;((ADDER 3) 5)
;;Error: Illegal function call.
```

在common lisp 中 对于变量和方法 他们有不同的命名空间。 比如一个变量和一个函数可以有同一个名字, 这取决于他被运算的环境

```lisp
;; The symbol foo is bound to nothing:
CL-USER> (boundp 'foo)
NIL
CL-USER> (fboundp 'foo)
NIL
;; We create a variable:
CL-USER> (defparameter foo 42)
FOO
* foo
42
;; Now foo is "bound":
CL-USER> (boundp 'foo)
T
;; but still not as a function:
CL-USER> (fboundp 'foo)
NIL
;; So let's define a function:
CL-USER> (defun foo (x) (* x x))
FOO
;; Now the symbol foo is bound as a function too:
CL-USER> (fboundp 'foo)
T
;; Get the function:
CL-USER> (function foo)
#<FUNCTION FOO>
;; and the shorthand notation:
* #'foo
#<FUNCTION FOO>
;; We call it:
(funcall (function adder) 5)
#<CLOSURE (lambda (X) :IN ADDER) {100991761B}>
;; and call the lambda:
(funcall (funcall (function adder) 5) 3)
8
```

可以认为，每个symbol 在CL(common lisp)中多有多个cell(空间)存储不同的信息，比如有的表示一个value-cell 你可以用boundp 来测试是否绑定了一个value. 你可以使用symbol-value访问value-cell.
还有其他cell 比如function-cell 可以保存symbol 的function. 你可以用fboundp来测试，使用symbol-function 来访问这个function.

如果symbol 被运算，那它就被当做是一个variable 并且他的value-cell将被返回。如果是符合形式，即cons,被求值并且他的car是一个symbol，那么使用这个symbol的function-cell. 所以上面的((addr 3) 5)的调用会报错。因为(addr 3) 既不是符号 也不是lambda表达式.

```lisp
;;; continued from above
CL-USER> (fboundp '*my-fun*)
NIL
CL-USER> (setf (symbol-function '*my-fun*) (adder 3))
#<CLOSURE (lambda (X) :IN ADDER) {10099A5EFB}>
CL-USER> (fboundp '*my-fun*)
T
CL-USER> (*my-fun* 5)
8
```


## Closures 闭包 {#closures-闭包}

Closures 允许捕获词法绑定的值

```lisp
(let ((limit 3)
      (counter -1))
  (defun my-counter()
    (if (< counter limit)
        (incf counter)
        (setf counter 0))))
(my-counter)
0
(my-counter)
1
(my-counter)
2
(my-counter)
3
(my-counter)
0
```

{{< admonition type="tip" title="lexical" open="true" >}}
这里可以看见，counter 和 limit 是在let后就已经存在的值。my-counter 会去找被let的limit 和 counter. let就是将let定义的值与下面的函数体(body)做绑定。
{{< /admonition >}}

还可以这样

```lisp
(defun repeater (n)
  (let ((counter -1))
    (lambda () (if (< counter n) (incf counter)
          (setf counter 0)))))
(defparameter *my-repeater* (repeater 3))
(defparameter *my-repeater2* (repeater 2))


(funcall *my-repeater*)
0
(funcall *my-repeater*)
1
(funcall *my-repeater*)
2
(funcall *my-repeater2*)
0
(funcall *my-repeater*)
3
(funcall *my-repeater*)
0
```

这里可以看到my-repeater 和my-repeater之间是相互隔离的 各自拥有自己的counter. 这是因为repeater 被执行了两次而 lambda 表达式被定义了两次，而被let包裹的函数会去其定义的环境中寻找值，所以两者的counter是隔离的。


## <span class="org-todo todo TODO">TODO</span> setf functions {#setf-functions}


## 柯里化 currying {#柯里化-currying}

维基百科上的解释是，把接受多个参数的函数转换成接受一个单一参数的函数

```lisp
(defun curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))
```

这个函数再第一次调用的时候会返回lambda函数

```lisp
(funcall (curry #'+ 3) 5)
;;8
```

