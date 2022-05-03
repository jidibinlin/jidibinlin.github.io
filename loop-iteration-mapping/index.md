# common lisp 循环操作


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

