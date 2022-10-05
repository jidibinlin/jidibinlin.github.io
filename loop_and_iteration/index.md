# Common-Lisp loop and iteration


<!--more-->


## Introduction: loop,iterate,for,mapcar,series {#introduction-loop-iterate-for-mapcar-series}


### loop {#loop}

loop 是一个内置的用来迭代的宏, 它最简单的形式是(loop (print "hello")): 这个语句会无限打印"hello"
一个简单的迭代List的例子

```lisp
(loop for x in '(1 2 3)
      do (print x))
```

这个例子会返回Nil但是会打印他应该打印的
如果你需要将结果收集成一个list使用collect

```lisp
(loop for x in '(1 2 3)
      collect (* x 10))
```

loop 表达式有四个部分

-   设置要被迭代的变量
-   结束迭代的条件表达式
-   每次迭代要做的事情
-   每次退出时要做的事情
-   除此之外,loop表达式还能返回一个值

正常来说，我们很少使用所有的部分，但是我们可以随意组合他们


### iterate {#iterate}

iterate 是一个很流行的迭代宏，要比loop更容易懂，但是拓展性要差一些。iterate并不是内置的，要先导入a。

```lisp
(ql:quickload "iterate")
(ues-package :iterate)
```

iterate看起来是这样的

```lisp
(iter (for in from 1 to 5)
      (collect (* i i)))
```

{{< admonition type="tip" title="warning" open="true" >}}
如果同时使用iterate 和 loop 会有命名冲突
使用display-iterate-clauses可以解决这个问题

```lisp
(display-iterate-clauses '(for))
;; FOR PREVIOUS &OPTIONAL INITIALLY BACK     Previous value of a variable
;; FOR FIRST THEN            Set var on first, and then on subsequent iterations
;; ...
```
{{< /admonition >}}


### for {#for}

for是一个可拓展的迭代宏，通常要比loop短。
for的最大好处就是，可以用在任何数据类型上(lists,vectors,hash-tables...)

```lisp
(for:for ((x over <your data structure>))
         (print ...))
```

for是一个第三方库，需要先quickload

```lisp
(ql:quickload "for")
```


### map族 {#map族}

后面还有很多mapcar 和 map这样的例子。map 族还有mapcon,mapcan,maplist,mapc 和 mapl.

```lisp
(mapcar (lambda (it) (+ it 10)) '(1 2 3))
;; => (11 12 13)
```

map 更加通用 他可以接受List 和 vectors 作为参数，需要在第一个参数指定结果类型

```lisp
(map 'vector (lambda (it) (+ it 10)) '(1 2 3))
;; #(11 12 13)
(map 'list (lambda (it) (+ it 10)) #(1 2 3))
;; (11 12 13)
(map 'string (lambda (it) (code-char it)) '#(97 98 99))
;; "abc"
```

{{< admonition type="tip" title="简写lambda函数" open="true" >}}
是不是觉得写lambda太烦， 其实有一些库可以提供简写lambda函数方法你可以去这里看看简写lambda的库有哪些[lambda shorthand libraries](https://github.com/CodyReichert/awesome-cl#lambda-shorthands)
这里给出一个cl-punch的例子

```lisp
(mapcar ^(* _ 10) '(1 2 3))
;; => (10 20 30)
```
{{< /admonition >}}


### series {#series}

最后 你可能会喜欢series,一个库通过结合sequences,streams,和loop 来描述自己. Series 表达式看起来就像是在操作序列，但是可以获得相比loop 更高的效率。 Series第一次出现在 "Common Lisp the Language"

```lisp
(collect
    (mapping ((x (scan-range :from 1 :upto 5)))
             (* x x)))
;; => (1 4 9 16 25)
```

{{< admonition type="tip" title="Generators The Way I Want Them Generated Library" open="true" >}}
这是一个lazy sequences 库，和series类似，景观他很年轻，切不完全，但是他有很多现代化的API 比如take,filter,for,fold 并且易用

```lisp
range :from 20)
;; #<GTWIWTG::GENERATOR! {1001A90CA3}>

(take 4 (range :from 20))
;; (20 21 22 23)
```
{{< /admonition >}}


## 小妙招 {#小妙招}


### Looping forever,return {#looping-forever-return}

```lisp
(loop
  (print "hello"))
```

return 用来返回结果

```lisp
(loop for i in '(1 2 3)
      when (> i 1)
        return i)
;; => 2
```


### 固定循环n次 {#固定循环n次}


#### dotimes {#dotimes}

```lisp
(dotimes (n 3)
    (print n))
```

这里dotimes只会返回Nil 有两种方法返回一个值

-   设置result在lambda list 中
    ```lisp
    (dotimes (n 3 :done)
      print(n))
    ;; =>
    ;; 0
    ;; 1
    ;; 2
    ;; :DONE
    ```
-   使用return
    ```lisp
    (dotimes (i 3)
      (if (> i 1)
          (return :early-exit!)
          (print i)))
    ;; =>
    ;; 0
    ;; 1
    ;; :EARLY-EXIT!
    ```


#### loop...repeat {#loop-dot-dot-dot-repeat}

```lisp
(loop repeat 10
      do (format t "Hello!~%"))
```

打印10次hello 返回nil

```lisp
(loop repeat 10
      collect (random 10))
;; => (5 1 3 5 4 0 7 4 9 1)
```

使用collect 会返回一个list


#### Series {#series}

```lisp
(iterate ((n (scan-range :below 10)))
         (print n))
```


### 循环无限次，在一个循环list上循环 {#循环无限次-在一个循环list上循环}

前面提到了一个无限循环的方法，但是我们如何在一个list上无限循环呢
我们可以构造一个循环list

```lisp
(loop with list-a = '(1 2 3)
      with infinite-list = (setf (cdr (last list-a)) list-a)
      for item in infinite-list
      repeat 8
      collect item)
```

构造循环列表有一个非常简单的方法使用#=语法

```lisp
(defparameter list-a '#1=(1 2 3 . #1#))
(setf print-circle t)
list-a
```

如果你只想再两个值之间交替地带，使用for...then

```lisp
(loop repeat 4
      for up = t then (not up)
      do (print up))
T
NIL
T
NIL
```


### Iterate 的for 循环 {#iterate-的for-循环}

对于list 和 vectors:

```lisp
(iter (for item in '(1 2 3))
      (print item))
(iter (for i in-vector #(1 2 3))
      (print i))
```


### 在一个list上进行循环 {#在一个list上进行循环}


#### dolist {#dolist}

```lisp
(dolist (item '(1 2 3))
  (print item))
```


#### loop {#loop}

-   使用in

<!--listend-->

```lisp
(loop for x in '(a b c)
      do (print x))
;; A
;; B
;; C
;; NIL
```

```lisp
(loop for x in '(a b c)
      collect x)
;; (A B C)
```

-   使用on 我们在cdr上迭代
    ```lisp
    (loop for i on '(1 2 3) do (print i))
    ;; (1 2 3)
    ;; (2 3)
    ;; (3)
    ```
-   mapcar
    ```lisp
    (mapcar (lambda (x)
              (print (* x 10)))
            '(1 2 3))
    10
    20
    30
    (10 20 30)
    ```

mapcar 会将lambda函数的返回值组合成一个List返回


### 在一个vector上循环 {#在一个vector上循环}


#### loop:across {#loop-across}

```lisp
(loop for i across #(1 2 3) do (print i))
```


#### Series {#series}

```lisp
(iterate ((i (scan #(123))))
         (print i))
```


### 在一个hash-table上循环 {#在一个hash-table上循环}

先创建一个hasht-table:

```lisp
(defparameter h (make-hash-table))
(setf (gethash 'a h) 1)
(setf (gethash 'b h) 2)
```


#### loop {#loop}

-   在key上循环
    ```lisp
    (loop for k being the hash-key of h do(print k))
    ;; b
    ;; a
    ```
-   在value上循环
    ```lisp
    (loop for k
            being the hash-key
              using (hash-value v) of h
          do (format t "~a ~a~%" k v))
    ;; b 2
    ;; a 1
    ```


#### maphash {#maphash}

maphash 的lambda函数时一个拥有两个参数的函数两个参数分别是key,value

```lisp
(maphash (lambda (key val))
         (format t "key: ~a val: ~a~&" key val)
         h)
;; key: A val:1
;; key: B val:2
;; NIL
```


#### dohash {#dohash}

dohash 是第三方库trivial-do的一个macro,类似dolist

```lisp
(dohash (key value h)
  (format t "key: ~A, value: ~A ~%" key value))
```


### 并行的在两个list上循环 {#并行的在两个list上循环}


#### loop {#loop}

```lisp
(loop for x in '(a b c)
      for y in '(1 2 3)
      collect (list x y))
;; ((A 1) (B 2) (C 3))
```

如果想返回一个平整过的list(flat list),使用nconcing 替代collect:

```lisp
(loop for x in '(a b c)
      for y in '(1 2 3)
      nconcing (list x y))
;; (A 1 B 2 C 3)
```

如果两个list的长度不同，会在短的结束的时候退出循环

```lisp
(loop for x in '(a b c)
      for y in '(1 2 3 4 5)
      collect (list x y))
;; ((A 1) (B 2) (C 3))
```

我们可以在一个大的list上循环，并且手动的通过index访问小一点的List的元素,但是这样的效率是非常低的，我们可以让loop自动拓展短的list

```lisp
(loop for y in '(1 2 3 4 5)
      for x-list = '(a b c) then (cdr x-list)
      for x = (or (car x-list) 'z)
      collect (list x y))
;; ((A 1) (B 2) (C 3) (Z 4) (Z 5))
```

在这个代码段中，for ... = ... then (cdr ...) 在每一次的循环中都会缩短一次list. 他的值一开始是'(a b c) 然后是 '(b c) 然后 '(c) 最后 nil


#### mapcar {#mapcar}

```lisp
(mapcar (lambda (x y) (list x y))
        '(a b c)
        '(1 2 3))
;; ((A 1) (B 2) (C 3))
```

或者更简单:

```lisp
(mapcar #'list
        '(a b c)
        '(1 2 3))
;; ((A 1) (B 2) (C 3))
```

返回一个flat list:

```lisp
(mapcan (lambda (x y)
          (list x y))
        '(a b c))
```


### 嵌套循环(Nested loops) {#嵌套循环--nested-loops}


#### loop {#loop}

```lisp
(loop for x from 1 to 3
      collect (loop for y from 1 to x
                    collect y))
;;((1) (1 2) (1 2 3))
```

如果要返回一个flat list,使用nconcing 替换第一个collect


### 计算一个中间值 {#计算一个中间值}


#### 使用= 与 for结合 {#使用-与-for结合}

```lisp
(loop for x from 1 to 3
      for y = (* x 10)
      collect y)
;; (10 20 30)
```

如果使用with,那么只会计算一次

```lisp
(loop for x from 1 to 3
      for y = (* x 10)
      with z = x
      collect (list x y z))
;; ((1 10 1) (2 20 1) (3 30 1))
```

HyperSpec 对 with 的定义时这样的

```lisp
with-clause::= with var1 [type-spec] [= form1] {and var2 [type-spec] [= form2]}*
```

所以我们可以再=前面指明类型 并且用and 串起来

```lisp
(loop for x from 1 to 3
      for y integer = (* x 10)
      with z integer = x
      collect (list x y z))
```

```lisp
(loop for x upto 3
      with foo = :foo
      and bar = :bar
      collect list (x foo bar))
```

我们也可以给for 一个 then 让他没次迭代都执行一次

```lisp
(loop repeat 3
      for intermediate = 10 then (incf intermediate)
      do (print intermediate))
10
11
12
```

这里是一个在bool值之间不断切换的例子

```lisp
(loop repeat 4
      for up = t then (not up)
      do (print up))
T
NIL
T
NIL
```


### 循环计数器 {#循环计数器}


#### loop {#loop}

对一个List进行迭代的同时进行计数。list的长度决定了迭代合适结束。

```lisp
(loop for x in '(a b c d e)
      for y from 1
      when (> y 1)
      do (format t ", ")

      do (format t "~A" x))

A,B,C,D,E
NIL
```

也可以用if语句

```lisp
(loop for x in '(a b c d e)
      for y from 1

      if (> y 1)
      do (format t ", ~A" x)
      else do (format t "~A" x))

A,B,C,D,E
NIL
```


### 升降序，limits {#升降序-limits}


#### loop {#loop}

<!--list-separator-->

-  升序

    -   from... to...: include the last
        ```lisp
        (loop for i from 0 to 10
              do (print i))
        ;; 0 1 2 3 4 5 6 7 8 9 10
        ```
    -   from... below...: not include the last
        ```lisp
        (loop for i from 0 below 10
              do (print i))
        ;; 0 1 2 3 4 5 6 7 8 9
        ```

<!--list-separator-->

-  降序

    -   from... downto...: include
        ```lisp
        (loop for i from 10 downto 0
              do (print i))
        ;; 10 9 8 7 6 5 4 3 2 1 0
        ```
    -   from... above...: not include
        ```lisp
        (loop for i from 10 above 0
              do (print i))
        ;; 10 9 8 7 6 5 4 3 2 1
        ```


### 步长 {#步长}


#### loop {#loop}

loop 使用by:

```lisp
(loop for i from 1 to 10 by 2
      do (print i))
```

如果by后面跟的是一个表达式那么只会执行一次


### Loop 和条件 {#loop-和条件}


#### loop {#loop}

使用if, else 和 finally:

```lisp
(loop repeat 10
      for x = (random 100)
      if (evenp x)
        collect x into evens
      else
        collect x into odds
      finally (return (values evens odds)))
```

```lisp
(42 82 24 92 92)
(55 89 59 13 49)
```

如果要结合多个语句，那么if的body需要and关键字(and do, and count)

```lisp
(loop repeat 10
      for x = (random 100)
      if (evenp x)
        collect x into evens
        and do (format t "~a is even!~%" x)
      else
        collect x into odds
        and count t into n-odds
      finally (return (values evens odds n-odds)))
```

```lisp
46 is even!
8 is even!
76 is even!
58 is even!
0 is even!
(46 8 76 58 0)
(7 45 43 15 69)
5
```


### 用一个语句作为loop的开始(initially) {#用一个语句作为loop的开始--initially}

```lisp
(loop initially
  (format t "~a " 'loop-begin)
      for x below 3
      do (format t "~a " x))
;;LOOP-BEGIN 0 1 2
```


### 用一个test（until,while）来结束循环 {#用一个test-until-while-来结束循环}


#### loop {#loop}

-   until

<!--listend-->

```lisp
(loop for x in '(1 2 3 4 5)
      until (> x 3)
      collect x)
;; (1 2 3)
```

-   while
    ```lisp
    (loop for x in '(1 2 3 4 5)
          while (< x 4)
          collect x)
    ```


### 循环命名 和 提前退出 {#循环命名-和-提前退出}


#### loop {#loop}

loop named foo 语法允许你创建一个能够提前退出的循环。使用return-form,即可退出已经命名的循环，甚至可以退出嵌套的循环。

```lisp
(loop named loop-1
      for x from 0 to 10 by 2
      do (loop for y from 0 to 100 by (1+ (random 3))
               when (< x y)
                 do (return-from loop-1 (values x y))))
0
2
```

有的时候你想要提前退出，但是一定要执行一些语句，你可以使用loop-finish

```lisp
(loop for x from 0 to 100
      do (print x)
      when (>= x 3)
        return x
      finally (print :done))

;; 0
;; 1
;; 2
;; 3
;; 3

(loop for x from 0 to 100
      do (print x)
      when (>= x 3)
        do (loop-finish)
      finally (print :done)
              (return x))

;; 0
;; 1
;; 2
;; 3
;; :DONE
;; 3
```


### Loop thereis never always {#loop-thereis-never-always}

-   thereis

<!--listend-->

```lisp
(loop for x in '(foo 2)
      thereis (numberp x))
T
```

-   never
    ```lisp
    (loop for x in '(foo 2)
          never (numberp x))
    NIL
    ```
-   always
    ```lisp
    (loop for x in '(foo 2)
          always (numberp x))
    NIL
    ```

他们和some,notany,every对应：

```lisp
(some #'numberp '(foo 2))
(notany #'numberp '(foo 2))
(every #'numberp '(foo 2))
```


### Count {#count}

```lisp
(loop for i from 1 to 3 count (oddp i))
;; 2
```


### Summation {#summation}

```lisp
(loop for i from 1 to 3 sum (* i i ))
;; 14
```


#### 将求和的结果放入变量中 {#将求和的结果放入变量中}

```lisp
(loop for i from 1 to 3
      sum (* i i) into total
      do (print i)
      finally (print total))
1
2
3
14
```


### Max and Min {#max-and-min}

```lisp
(loop for i from 1 to 3 maximize (mod i 3))
;; 2
```


### 解构, 对 list 进行dotted pairs aka 模式匹配 {#解构-对-list-进行dotted-pairs-aka-模式匹配}

```lisp
(loop for (a b) in '((x 1) (y 2) (z 3))
      collect (list b a))
;; ((1 X) (2 Y) (3 Z))

(loop for (x . y) in '((1 . a) (2 . b) (3 . c)) collect y)
;; (A B C)
```

使用nil忽略

```lisp
(loop for (a nil) in '((x 1) (y 2) (z 3))
      collect a)
;; (X Y Z)
```

两个两个的遍历

```lisp
(loop for (key value) on '(a 2 b 2 c 3) by #'cddr
      collect (list key (* 2 value)))
;;((A 2) (B 4) (C 6))
```

