# Common-Lisp data_structure


<!--more-->


## Lists {#lists}


### Building lists. Cons cells,list {#building-lists-dot-cons-cells-list}

list(列表) 也是一个 sequence(序列). list 由cons组成。 cons 有两个cell 一个叫 "car" 一个 叫 "cdr" 我们可以像这样构建一个简单的list

```lisp
(cons 1 2)
;;=> (1 . 2) ;; 这是一个dotted pair 表示一个cons
```

它的底层结构可以抽象成这样

```nil
[o|o]--- 2
|
1
```

如果第一个cons的cdr 是另一个cons 并且最后一个cons 的cdr 是nil ,那么我们就得到了一个list

```lisp
(cons 1 (cons 2 nil))
;; => (1 2)
```

底层结构是这样的

```nil
[o|o]---[o|/]
 ^       ^
 1       2
```

{{< admonition type="tip" title="为什么不是dotted pair" open="true" >}}
lisp printer 遇到这种结构的时候会自动将逗号省略。但是list还是由一个个dotted pair组成的。
{{< /admonition >}}

更简单的 可以用list 函数来构建list

```lisp
(list 1 2)
;; => (1 2)
```

或者使用一个引用符号'

```lisp
'(1 2)
;; => (1 2)
```


### Circular lists 循环列表 {#circular-lists-循环列表}

cons 的两个cell 可以 c存放任何值，所以也可以存放自己，或者列表的最后一个元素的cdr存储列表的第一个元素.这个时候就构成了循环列表。
引文最后一个元素的cdr 存的是列表的开始。
在开始之前，我们要将\*print-circle\* 设为t告诉printer 不要无限答应循环列表。

```lisp
(setf *print-circle* t)
```

首先定义一个能构造循环列表的函数

```lisp
(defun circular! (items)
  "Modifies the last cdr of list ITEMS, returning a circular list"
  (setf (cdr (last items)) items))
(circular! (list 1 2 3))

(fifth (circular! (list 1 2 3)))
;; => 2
```

对于循环列表list-length将会返回nil

reader 也可以构建一个循环列表，使用sharpsign equal-sign notation. 一个object(对象) 可以以#n=作为前缀，n是一个无符号十进制整数。可以用#n#去引用已经定义的对象

```lisp
'#42=(1 2 3 . #42#)
;; => #1=(1 2 3 . #1#)
```


### car/cdr or first/rest (and second... to tenth) {#car-cdr-or-first-rest--and-second-dot-dot-dot-to-tenth}

```lisp
(car (cons 1 2)) ;; return the car cell of a cons. here return 1
(cdr (cons 1 2)) ;; return the cdr cell of a cons.here return 2
(first '(1 2 3 4 5)) ;; return the car of first. here return 1
(second '(1 2 3 4 5)) ;; return the car of second. here return 2
(third '(1 2 3 4 5)) ;; return the car of third. here return 3
.
.
.
(tenth '(1 2 3 4 5 6 7 8 9 10)) ;; return the car of tenth. here return 10

(rest '(rest)) ;; return (2 3) rest return cdr  of the first cons
```

setf 可以用来给列表中的元素赋值

```lisp
(defparameter ll '(1 2 3 4))
(setf (first ll) 2)
ll ;; now ll is (2 2 3 4)
```


### last,butlast,nbutlast(&amp;optional n) {#last-butlast-nbutlast--and-optional-n}

```lisp
(last '(1 2 3)) ;; return last of cons of (1 2 3). here return (3)
;; => (3)
(car (last '(1 2 3))) ;; or (first (last ...))
;; => 3
(butlast '(1 2 3));; return the list except the last
;; => (1 2)
```


### reverse,nreverse 倒序 {#reverse-nreverse-倒序}

reverse 和 nreverse 都返回一个新的序列(sequence)
nreverse 是毁灭性的(destructive)。前缀 n 代表 non-consing,代表他是直接在原有List上进行修改。

```lisp
(defparameter mylist '(1 2 3))
;; => (1 2 3)
(reverse mylist)
;; => (3 2 1)
mylist
;; => (1 2 3)
(nreverse mylist)
;; => (3 2 1)
mylist
;; => (1) in SBCL but implementation dependent.
```

{{< admonition type="note" title="destructive" open="true" >}}
如果一个函数会修改他的他的操作元素 我们就说他是destructive的
{{< /admonition >}}


### append {#append}

append 会将两个list合并成一个list

```lisp
(append (list 1 2) (list 3 4))
;; => (1 2 3 4)
```

{{< admonition type="warning" title="append 的坑" open="true" >}}
append 返回的List会共享原有的list的cons 所以在其返回的list进行修改，也会影响原有的list
{{< /admonition >}}


### push (item,place) {#push--item-place}

push 是destructive的 他会将新的元素插入list的最前面

```lisp
(defparameter mylist '(1 2 3))
(push 0 mylist)
;;=> (0 1 2 3)
mylist
;;=> (0 1 2 3)
```

```lisp
(defparameter x ’(a (b c) d))
;; => (A (B C) D)
(push 5 (cadr x))
;; => (5 B C)
x
;; => (A (5 B C) D)
```

CL 没有默认的尾插函数，如果想要尾插你可以先将list 进行 reverse 或者使用别的数据结构


### pop {#pop}

pop 是 desctructive 和 push 相对 push 会往里面插入 而pop会将最前面的元素返回并删除


### nthcdr (index ,list) {#nthcdr--index-list}

返回第n个cons 的 cdr。

```lisp
(defparameter mylist '(1 2 3))
(nthcdr 1 mylist)
;; => (2)
```


### 复合的car 和 cdr {#复合的car-和-cdr}

我们可以复合 a 和 d 来做一些方便的操作。比如cadr 就是 返回 the car of cdr caar 就是返回 the car of car
这些操作通常在嵌套list中有些意义

```lisp
(caar (list 1 2 3));; ==> error
(caar (list (list 1 2) 3)) ;; ==> 1
(cadr (list (list 1 2) (list 3 4))) ;; ==> (3 4)
(caadr (list (list 1 2) (list 3 4))) ;; == > 3
```


### destructuring-bind (parameter\*,list) {#destructuring-bind--parameter-list}

这个函数将List中的元素与参数绑定，我们可以析构tree,plists

```lisp
(destructuring-bind (x y z) (list 1 2 3)
  (list :x x :y y :z z))
;; => (:X 1 :Y 2 :Z 3)
```

内部的sublist也可以绑定

```lisp
(destructuring-bind (x (y1 y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z))
;; => (:X 1 :Y1 2 :Y2 20 :Z 3)
```

parameter list 还可以用&amp;optional,&amp;rest 和 &amp;key

```lisp
(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2) 3)
  (list :x x :y1 y1 :y2 y2 :z z))
;; => (:X 1 :Y1 2 :Y2 NIL :Z 3)
```

```lisp
(destructuring-bind (&key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z))
;; => (:X 3 :Y 2 :Z 1)
```

&amp;whole parameter 会被绑定到整个list. 他必须再第一个 其余的参数必须在它后面

```lisp
(destructuring-bind (&whole whole-list &key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z :whole whole-list))
;; => (:X 3 :Y 2 :Z 1 :WHOLE (:Z 1 :Y 2 :X 3))
```


### null,listp 断言 {#null-listp-断言}

null 和 not 相同，但是看起来更好 用来判断一个列表是否为空
listp 判断一个对象是否是一个cons cell


### ldiff,tailp,list\*,make-list,fill,revappend,nreconc,consp,atom {#ldiff-tailp-list-make-list-fill-revappend-nreconc-consp-atom}

只介绍几个常用的，其余的可以去查文档（太多了，不想写（小声bb） 很多其实不常用，就不写了）

-   make-list 构造一个list
    ```lisp
    (make-list 3 :initial-element "ta")
    ;; => ("ta" "ta" "ta")
    ```
-   fill 用指定对象填充list
    ```lisp
    (make-list 3)
    ;; => (NIL NIL NIL)
    (fill * "hello")
    ;; => ("hello" "hello" "hello")
    ```
-   tailp 用来判断是否有共享的列表结构，也就是是否有相同的cons cell(use EQL for predicate),而不仅仅是cons cell的内容
    ```lisp
    (tailp t '(1 2 3 4 . t))
    ;; => T

    (tailp nil '(1 2 3 4 . nil))
    ;; => T

    (tailp nil '(1 2 3 4))
    ;; => T

    (tailp #1="e" '(1 2 3 4 . #1#))
    ;; => T

    (defparameter *tail* (list 3 4 5))
    (defparameter *larger* (list* 1 2 *tail*))
    (defparameter *replica* (copy-list *larger*))

    (tailp *tail* *replica*) ;; ==> nil
    (tailp *tail* *larger*) ;; ==> t
    ```


### member (elt,list) {#member--elt-list}

返回以elt开始的剩下的元素 默认使用eql作为比较函数
接受:test :test-not :key(functions or symbols)

```lisp
(member 2 '(1 2 3))
```


### 替换tree中的对象:subst,sublis {#替换tree中的对象-subst-sublis}

subst 和 subst-if 在tree中查找所有和element相同的元素 并 用另一个element 替换 (可以用:test 指定判断相同的函数)

```lisp
(subst 'one 1 '(1 2 3))
;; => (one 2 3)
(subst  '(1 . one) '(1 . 1) '((1 . 1) (2 . 2) (3 . 3)) :test #'equal)
;; ((1 . ONE) (2 . 2) (3 . 3))
```

sublis 第一个参数是一个list(每个元素都是一个 dot list),每个dot list 有2个元素(x . y) 表示将 x 替换 为 y
sublis 接受:test 和 :key 参数 :test 是一个接受key 和 subtree为参数的函数

```lisp
(sublis '((t . "foo"))
        ("one" 2 ("three" (4 5)))
        :key #'stringp)
;; ("foo" 2 ("foo" (4 5)))
```


## Sequences 序列 {#sequences-序列}

lists vectors strings 都是序列

大多数和序列有关的函数都接受keyword 参数。所有keyword 参数都是可选的，并且可以以任意顺序出现

:test参数默认使用eql (对于strings 来说 用:equal)

:key 参数可以接受nil 和 单参数的函数（这个函数声明了我们具体要怎么看待序列中的每个元素）

```lisp
(defparameter my-alist (list (cons 'foo "foo")
                             (cons 'bar "bar")))
;; => ((FOO . "foo") (BAR . "bar"))

(find 'bar my-alist)
;; => NIL
(find 'bar my-alist :key 'car)
```


### 断言: every , some.... {#断言-every-some-dot-dot-dot-dot}

-   every 的作用是判断列表中的元素是否都满足某个条件 (都满足就返回真)
    ```lisp
    (defparameter foo '(1 2 3))
    (every #'evenp foo)
    ;; => NIL
    ```
-   some 则只需要一些元素满足条件即可 (一些满足就返回真)
    ```lisp
    (defparameter foo '(1 2 3))
    (every #'evenp foo)
    ;; => NIL
    ```
-   notany 只要不是所有都满足就返回真


### 常用函数 {#常用函数}

{{< admonition type="tip" title="alexandria库" open="true" >}}
《common lisp cookbook》中一直提到有一个"alexandria"的库里面有很多非常实用的函数，是对common lisp 的一种扩充，可以去看一看
{{< /admonition >}}


#### length(sequence) {#length--sequence}

返回序列的长度


#### elt(sequence,index) {#elt--sequence-index}

find by index


#### count (foo sequence) {#count--foo-sequence}

返回 sequence中 出现foo的次数 可接受:from-end,:start,:end
相关的还有count-if,count-not(test-function sequence)


#### subseq (sequence start,[end]) {#subseq--sequence-start-end}

返回子序列左闭右开[start,end):

```lisp
(subseq (list 1 2 3) 0)
;; (1 2 3)
(subseq (list 1 2 3) 1 2)
;; (2)
```

end不能大于( + (length sequence) 1) 因为这样会越界:

```lisp
(defparameter my-list '(1 2 3 4))
(subseq my-list 0 (+ length(my-list) 1))
;; => Error: the bounding indices 0 and 5 are bad for a sequence of length 3.
```

如果果可能越界，用alexandria-2:subseq\*:

```lisp
(alexandria-2:subseq* (list 1 2 3) 0 99)
```

subseq 是"setf" able 的 但是只有当新的序列的长度和要被替换的序列长度一致的时候才会生效

```lisp
(defparameter my-list '(1 2 3 4))
;; =>(1 2 3 4)
(setf (subseq my-list 0 2) '(2 3))
my-list
;; => (2 2 3 4)
```


#### sort,stable-sort(sequence,test[,key function]) {#sort-stable-sort--sequence-test-key-function}

这两个函数时destructive的，所以在排序前，可能会优先使用copy-seq 拷贝一份出来

```lisp
(sort (copy-seq seq) :test #'string<)
```

就像名字中说的一样 sort是不稳定的而stable-sort是稳定的


#### find,position(foo,sequence) - get index {#find-position--foo-sequence--get-index}

also find-if,find-if-not,position-if,position-if-not(test sequence)

```lisp
(find 20 '(10 20 30))
;; 20
(position 20 '(10 20 30))
;; 1
```


#### search and mismatch (sequence-a, sequence-b) {#search-and-mismatch--sequence-a-sequence-b}

search 会在sequence-b 中寻找和sequence-a 一样的subsequence。他会返回sequence-a 在 sequence-b 中的位置，如果没找到就是nil. 参数有 from-end,end1,end2 以及上面提到的test 和 key

```lisp
(search '(20 30) '(10 20 30 40))
;; 1
(search '("b" "c") '("a" "b" "c"))
;; nil
(search '("b" "c") '("a" "b" "c") :test #'equal)
;;1
(search "bc" "abc")
;;1
```

mismatch 返回两个序列开始不一样的地方

```lisp
(mismatch '(10 20 99) '(10 20 30))
;; 2
(mismatch "hellolisper" "helloworld")
;; 5
(mismatch "same" "same")
;; NIL
(mismatch "foo" "bar")
;; 0
```


#### substitute,nsubstitute[if,if-not] {#substitute-nsubstitute-if-if-not}

返回一个同类型的序列，这个序列将会用新的元素替代就的元素

```lisp
(substitute #\o #\x "hellx") ;; => "hello"
(substitute :a :x '(:a :x :x)) ;; => (:A :A :A)
(substitute "a" "x" '("a" "x" "x") :test #'string=) ;; => ("a" "a" "a")
```


#### replace (sequence-a,sequence-b,&amp;key start1,end1) {#replace--sequence-a-sequence-b-and-key-start1-end1}

破坏性的用sequence-b 中的元素替换sequence-a中的元素
完整的函数签名:

```lisp
(replace sequence1 sequence2 &rest args &key (start1 0) (end1 nil) (start2 0)(end2 nil))
```

```lisp
(replace "xxx" "foo")
"foo"

(replace "xxx" "foo" :start1 1)
"xfo"

(replace "xxx" "foo" :start1 1 :start2 1)
"xoo"

(replace "xxx" "foo" :start1 1 :start2 1 :end2 2)
"xox"
```


#### remove,delete(foo sequence) {#remove-delete--foo-sequence}

返回一个不包含foo的新sequence 接受:start/end 和 :count参数

```lisp
(remove "foo" '("foo" "bar" "foo") :test 'equal)
;; => ("bar")
```


#### remove-duplicates,delete-duplicates(sequence) 去重操作 {#remove-duplicates-delete-duplicates--sequence--去重操作}

delete与remove 不同的地方在于 delete会直接操作原始的sequence
接受 from-end test test-not start end key

```lisp
(remove-duplicates '(:foo :foo :bar))
(:FOO :BAR)

(remove-duplicates '("foo" "foo" "bar"))
("foo" "foo" "bar")

(remove-duplicates '("foo" "foo" "bar") :test #'string-equal)
("foo" "bar")
```


#### mapping (map,mapcar,remove-if[-not],...) {#mapping--map-mapcar-remove-if-not-dot-dot-dot}

mapcar 只能用于list 而map 适用于vector list 但是由于map 可以用于多种结构 所以要指定返回值类型实参。(map 'list function vector)
mapcar 可以接受多个list 映射操作会在最短的list被操作完是中断

```lisp
(defparameter foo '(1 2 3))
(map 'list (lambda (it) (* 10 it)) foo)
```

reduce (function,sequence). 首先会将function作用于列表第一个和第二个元素，然后将function作用于这个结果与下一个元素直到处理完所有元素
如果指定了特殊参数:initial-value. 首先就不会将function作用于列表的第一个和第二个元素，而是首先将function作用于:initial-value 和第一个值

```lisp
(reduce '- '(1 2 3 4))
;; => -8
(reduce '- '(1 2 3 4) :initial-value 100)
;; => 90
```


#### Creating lists with variables {#creating-lists-with-variables}

由于quote ' 会保护表达式不被求值，所以我们得用list来新建列表，但是list 函数所有的参数都会先被求值然后再构造成列表.
如果只是想特定的变量被求职，我们可以使用backquote \` 来构造List
先来个正常的quote

```lisp
(defparameter *var* "bar")
;; First try:
'("foo" *var* "baz") ;; no backquote
;; => ("foo" *VAR* "baz") ;; nope
```

再来个backquote

```lisp
`("foo" ,*var* "baz")     ;; backquote, comma
;; => ("foo" "bar" "baz") ;; good
```

在backquote 后面 以comma , 开头的变量会被正常求值
如果变量是一个list的话

```lisp
(defparameter *var* '("bar" "baz"))
;;First try:
`("foo" ,*var*)
;; => ("foo" ("bar" "baz")) ;; nested list
`("foo" ,@*var*)
;; => ("foo" "bar" "baz")
```


### 集合操作Set {#集合操作set}

首先，一个集合不会包含两个同样的元素，并且集合内部是无需的.
大多数函数都有循环利用已经有的变量的函数版本(破坏性的) 区别于正常的 他们以"n"开头. 他们都可以接受:key 和 :test 参数


#### intersection  list的交集 {#intersection-list的交集}

```lisp
(defparameter list-a '(0 1 2 3))
(defparameter list-b '(0 2 4))
(intersection list-a list-b)
;; => (2 0)
```


#### set-difference 作差 {#set-difference-作差}

```lisp
(set-difference list-a list-b)
;; => (3 1)
(set-difference list-b list-a)
;; => (4)
```


#### union 并集 {#union-并集}

```lisp
(union list-a list-b)
;; => (3 1 0 2 4) ;; order can be different in your lisp
```


#### 补集 set-exclusive-or {#补集-set-exclusive-or}

```lisp
(set-exclusive-or list-a list-b)
;; => (4 3 1)
```


#### adjoin 添加 {#adjoin-添加}

会返回新的集合，原有集合不会被修改

```lisp
(adjoin 3 list-a)
;; => (0 1 2 3)

(adjoin 5 list-a)
;; => (5 0 1 2 3) ;; <-- element added in front.

list-a
;; => (0 1 2 3)  ;; <-- original list unmodified.
```


#### subsetp 是否为子集 {#subsetp-是否为子集}

```lisp
(subsetp '(1 2 3) list-a)
;; => T

(subsetp '(1 1 1) list-a)
;; => T

(subsetp '(3 2 1) list-a)
;; => T

(subsetp '(0 3) list-a)
;; => T
```


### Fset-immutable data structure {#fset-immutable-data-structure}

也可以去quicklisp 看看FSet库


## array 和 vector {#array-和-vector}

Arrays 的访问时间复杂度是常数时间的
他们可以是fixed 或者 adjustable的。 一个simple array  既不能被displaced(置换) 也不能被adjust(调整) 更不会有fill-pointer 除非我们用:displaced-to, :adjust-array,:fill-pointer 指定,

Vector 不同于 array的地方在于 他的维度只能固定再1维 vector 也是一个序列(sequence)


### 创建和调整array {#创建和调整array}

make-array(sizes-list :adjustable  :initial-element )
common lisp 至少可以指定7个维度，买个维度可以至少容纳1023个元素，如果提供了:initial-element 那么这个值会作为初始值

```lisp
(make-array '(2 3) :initial-element nil)
```

adjust-array(array,sizes-list :element-type : initial-element)
adjust-array 用于调整函数的维度


### 访问 : aref (array i [j...]) {#访问-aref--array-i-j-dot-dot-dot}

aref (array i j k ...) 取指定索引上的元素
row-major-aref(array i) 和 (aref i i i ....) 相同 用来去主轴上的元素
返回的结果都可以被setf 赋值

```lisp
(defparameter myarray (make-array '(2 2 2) :initial-element 1))
myarray
;; => #3A(((1 1) (1 1)) ((1 1) (1 1)))
(aref myarray 0 0 0)
;; => 1
(setf (aref myarray 0 0 0) 9)
;; => 9
(row-major-aref myarray 0)
;; => 9
```


### sizes {#sizes}

array-total-size(array): 返回array 中一共有多少个元素
array-dimensions(array): 返回array每一维的长度
array-dimension(array i): 返回第i维的长度
array-rank: 返回函数一共有多少维

```lisp
(defparameter myarray (make-array '(2 2 2)))
;; => MYARRAY
myarray
;; => #3A(((0 0) (0 0)) ((0 0) (0 0)))
(array-rank myarray)
;; => 3
(array-dimensions myarray)
;; => (2 2 2)
(array-dimension myarray 0)
;; => 2
(array-total-size myarray)
;; => 8
```


### Vector {#vector}

创建vector 可以使用vector 或者读取宏 #(). 他会返回一个simple vector

```lisp
(vector 1 2 3)
;; => #(1 2 3)
#(1 2 3)
;; => #(1 2 3)
```

-   vector-push (foo vector): 将fill-pointer 指向的元素替换成foo
-   vector-push-extend (foo vector [extension-num])t: 与vector-push相似，但是如果fill-pointer比较大，他会拓展数组
-   vector-pop (vector): 返回fill-pointer 指向的元素 并删除
-   fill-pointer (vector). setfable.: 返回array的fill-pointer

and see also the sequence functions.

{{< admonition type="tip" title="fill-pointer" open="true" >}}
fill-pointer 就是vector-push要push的index位置（或者vector-push-extend）.也叫填充句柄。如果一个元素他的index大于这个fill-pointer 那么他就是active的 如果小于fill-pointer 就是inactive的
{{< /admonition >}}


## Hash Table {#hash-table}


### 创建一个hash table {#创建一个hash-table}

make-hash-table 创建一个hash table。make-hash-table 没有必要参数。:test 参数指定用来判断key是否相同的函数

{{< admonition type="note" title="Note" open="true" >}}
see shorter notations in the Serapeum or Rutils libraries. For example, Serapeum has dict, and Rutils a #h reader macro.
{{< /admonition >}}


### 获取hash table 中的元素 {#获取hash-table-中的元素}

gethash 接受两个必要参数 一个要获取的元素的key 一个hash table. 它返回key绑定的value 和 一个bool 值表示是否在hash table中找到这个值.
它还有一个可选参数default 表示找不到时的默认值

```lisp
(defparameter my-table (make-hash-table))
(gethash 'bar my-table 'default-value)
;; => default-value,NIL
```

获取 hashtable中所有的value or key

```lisp
(ql:quickload "alexandria")
(alexandria:hash-table-keys my-table)
```


### 向hash table 添加一个元素 {#向hash-table-添加一个元素}

可以使用gethash+setf 向hash table 中添加元素

```lisp
CL-USER> (defparameter *my-hash* (make-hash-table))
*MY-HASH*
CL-USER> (setf (gethash 'one-entry *my-hash*) "one")
"one"
CL-USER> (setf (gethash 'another-entry *my-hash*) 2/4)
1/2
CL-USER> (gethash 'one-entry *my-hash*)
"one"
T
CL-USER> (gethash 'another-entry *my-hash*)
1/2
T
```


### 判断hash table 中某个key是否存在 {#判断hash-table-中某个key是否存在}

gethash 的第一个返回值会返回对应key绑定的value 但是有的时候我们可能会刻意让这个value 为 nil。 所以使用第一个返回值是不可靠的。第二个返回值会明确告诉我们这个键值对是否存在，所以判断是否存在可以这样。

```lisp
(if (nth-value 1 (gethash 'bar my-table))
    "Key exists"
    "Key does not exist")
```


### 从hash table 中删除 {#从hash-table-中删除}

remhash (remove-hash的缩写) 专门用来删除一个哈希表条目。当hash table中有这个条目的时候 返回T 否则就返回 NIL

```lisp
(defparameter *my-hash* (make-hash-table))
;; ==> ,*MY-HASH*
(setf (gethash 'first-key *my-hash*) 'one)
;; => ONE
(gethash 'first-key *my-hash*)
;; => ONE,T
(remhash 'first-key *my-hash*)
;; =>T
(gethash 'first-key *my-hash*)
;; => NIL,NIL
CL-USER> (gethash 'no-entry *my-hash*)
;; => NIL,NIL
CL-USER> (remhash 'no-entry *my-hash*)
;; => NIL
CL-USER> (gethash 'no-entry *my-hash*)
;; => NIL,NIL
```


### clear a hash table {#clear-a-hash-table}

使用clrhash 清空一个hash table

```lisp
(defparameter my-hash (make-hash-table))
(setf (gethash 'first-key my-hash) 'one)
(setf (gethash 'second-key my-hash) 'two)
my-hash
;; => #<hash-table :TEST eql :COUNT 2 {10097BF4E3}>
(clrhash my-hash)
;; => #<hash-table :TEST eql :COUNT 0 {10097BF4E3}>
(gethash 'first-key myhash)
;; => NIL,NIL
(gethash 'second-key myhash)
;; => NIL,NIL
```


### 遍历hash table {#遍历hash-table}


#### 使用maphash 遍历 {#使用maphash-遍历}

```lisp
(defparameter my-hash (make-hash-table))
(setf (gethash 'first-key *my-hash*) 'one)
(setf (gethash 'second-key *my-hash*) 'two)
(setf (gethash 'third-key *my-hash*) nil)
(setf (gethash nil *my-hash*) 'nil-value)

(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))

(maphash #'print-hash-entry *my-hash*)
;; => The value associated with the key FIRST-KEY is ONE
;; => The value associated with the key SECOND-KEY is TWO
;; => The value associated with the key THIRD-KEY is NIL
;; => The value associated with the key NIL is NIL-VALUE
```


#### 使用with-hash-table-iterator {#使用with-hash-table-iterator}

with-hash-table-iterator 是一个宏 他会为hash-table 创建迭代器，每次迭代会返回三个返回值

-   第一个返回值返回是否有这个entry
-   第二个返回值返回key
-   第三个返回值返回value

如果遍历到了最后，只会返回一个nil

```lisp
(with-hash-table-iterator (my-iterator my-hash)
  (loop
    (multiple-value-bind (entry-p key value)
        (my-iterator)
      (if entry-p
          (print-hash-entry key value)
          (return)))))
;; => The value associated with the key FIRST-KEY is ONE
;; => The value associated with the key SECOND-KEY is TWO
;; => The value associated with the key THIRD-KEY is NIL
;; => The value associated with the key NIL is NIL-VALUE
;; => NIL
```


#### 遍历keys 或者 values {#遍历keys-或者-values}

如果只想遍历keys 或者 values 可以使用Alexandria的maphash-keys 和 maphash-values


### 计算hash table 中的entry 数量 {#计算hash-table-中的entry-数量}

可以使用hash-table-count

```lisp
(defparameter *my-hash* (make-hash-table))
(hash-table-count *my-hash*)
;; => 0
(setf (gethash 'first *my-hash*) 1)
(setf (gethash 'second *my-hash*) 2)
(setf (gethash 'third *my-hash*) 3)
(hash-table-count *my-hash*)
(setf (gethash 'second *my-hash*) 'two)
(hash-table-count *my-hash*)
;; => 3
(clrhash *my-hash*)
(hash-table-count *my-hash*)
```


### 线程安全的hash table {#线程安全的hash-table}

common lisp 的标准中，hash-table不是线程安全的，但是在SBCL中，我们可以给make-hash-table传一个 :synchronized 参数来构造一个线程安全的hash table

```lisp
(defparameter *my-hash* (make-hash-table :synchronized t))
```

但是在修改时，需要使用sb-ext:with-locked-hash-table将操作保护起来

```lisp
(sb-ext:with-locked-hash-table (my-hash)
  (setf (gethash :a my-hash) :new-value))
```

{{< admonition type="tip" title="cl-gserver library" open="true" >}}
cl-gserver 实现了一套actors/agent system 可以简化多线程变成的困难
{{< /admonition >}}


## Alist (associated list) {#alist--associated-list}

一个association list 就是一个存放cons cells 的list

```lisp
(defparameter *my-alist* (list (cons 'foo "foo")
                             (cons 'bar "bar")))
;; => ((FOO . "foo") (BAR . "bar"))
```

内部构造就像这样

```nil
[o|o]---[o|/]
 |       |
 |      [o|o]---"bar"
 |       |
 |      BAR
 |
[o|o]---"foo"
 |
FOO
```


### 构造 Construct {#构造-construct}

第一种方法

```lisp
(setf *my-alist* '((:foo . "foo")
                   (:bar . "bar")))
```

第二种方法是使用pairlis

```lisp
(pairlis '(:foo :bar)
         '("foo" "bar"))
;; => ((:BAR . "bar") (:FOO . "foo"))
```

Alist 就是一个list 所以一个key可以绑定多个值

```lisp
(setf *alist-with-duplicate-keys*
      '((:a . 1)
        (:a . 2)
        (:b . 3)
        (:a . 4)
        (:c . 5)))
```


### 访问 Access {#访问-access}

要获取一个key 我们使用assoc (可以指定 :test  如果key的类型为strings默认用 equal) 它会返回整个cons cell, 所以可以配合cdr 或者second 获取对应的值。Alexandria 还提供了assoc-value list key 这个函数更加的方便

```lisp
(alexandria:assoc-value my-alist :foo)
;; => :foo , (:foo . "foo")
```

assoc-if(predicate alist) 找到第一个满足predicate的cons cell

```lisp
(setf alist '((1 . :a)
              (2 . :b)
              (3 . :c)))
(assoc-if #'evenp alist)
```

rassoc 按值查找

```lisp
(setf alist '((1 . :a)
              (2 . :b)
              (4 . :d)
              ))
(rassoc :a alist)
;; => (1 . :a)
```

去重 使用remove-if-not 这个函数的作用是返回一个只包含不满足predicate的列表

```lisp
(remove-if-not
 #'(lambda (entry) (eq :a entry))
 alist-with-duplicate-keys
 :key #'car)
```


### 插入和删除 {#插入和删除}

push 插入一个entry

```lisp
(push (cons 'team "team") my-alist)
;; => ((TEAM . "team") (FOO . "foo") (BAR . "bar"))

```

删除remove pop 等 remove 需要指定key 关键字 告诉remove 怎么读取entry

```lisp
(remove 'team my-alist)
;; => ((TEAM . "team") (FOO . "foo") (BAR . "bar")) ;; didn't remove anything
(remove 'team my-alist :key 'car)
;; => ((FOO . "foo") (BAR . "bar")) ;; returns a copy
```


### 更新entries {#更新entries}

-   replace a value

<!--listend-->

```lisp
*my-alist*
;; => '((:FOO . "foo") (:BAR . "bar"))
(assoc :foo *my-alist*)
;; => (:FOO . "foo")
(setf (cdr (assoc :foo *my-alist*)) "new-value")
;; => "new-value"
*my-alist*
;; => '((:foo . "new-value") (:BAR . "bar"))
```

-   replace a key

<!--listend-->

```lisp
*my-alist*
;; => '((:FOO . "foo") (:BAR . "bar")))
(setf (car (assoc :bar *my-alist*)) :new-key)
;; => :NEW-KEY
*my-alist*
;; => '((:FOO . "foo") (:NEW-KEY . "bar")))
```


## Plist (property list) {#plist--property-list}

plist 是一个简单地list 内部又 key ,value 交错组成。他的key是一个symbol

```lisp
(defparameter my-plist (list 'foo "foo" 'bar "bar"))
```

底层结构就像这样

```lisp
[o|o]---[o|o]---[o|o]---[o|/]
 |       |       |       |
FOO     "foo"   BAR     "bar"
```

我们可以使用getf (list elt)来获取一个元素（返回value）

```lisp
(defparameter my-plist (list 'foo "foo" 'bar "bar"))
;; => (FOO "foo" BAR "bar")
(setf (getf my-plist 'foo) "foo!!!")
;; => "foo!!!"
```

remf 用来删除一个元素

```lisp
(remf my-plist 'foo)
;; => (BAR "bar")
```


## Structures 结构体 {#structures-结构体}

structures 可以将数据存储在具名的slots中，并且支持单继承。
CLOS (Common Lisp Object System)提供一个更灵活的Classes. 但是structures 可以提供更好的性能


### 定义 {#定义}

使用defstruct

```lisp
(defstruct person
  id name age)
```

在创建时 slots 默认值为nil
要想自定义默认值

```lisp
(defstruct person
  id
  (name "john doe")
  age)
```

也可以指定默认类型

```lisp
(defstruct person
  id
  (name "john doe" :type string)
  age)
```


### 创建 {#创建}

使用make-&lt;structure-name&gt;, 对于上面的person 使用make-person

```lisp
(defparameter me (make-person))
;; => ME
me
;;=> #S(PERSON :ID NIL :NAME "john doe" :AGE NIL)
```

也可以使用关键字指定参数值

```lisp
(defparameter me (make-person :name 123))
```

还可以自定义构造函数，但是一旦自定义了构造函数，那么默认构造函数就将不发再被使用

```lisp
(defstruct (person (:constructor create-person (id name age)))
  id
  name
  age)
```


### 访问slot的值 {#访问slot的值}

structure被定义后，会自动定义 &lt;name-of-the-struct&gt;-+slot-name这样的函数

```lisp
(person-name me)
```


### 设置slot的值 {#设置slot的值}

使用setf来设置值

```lisp
(setf (person-name me) "Cookbook author")
```


### 判断类型 {#判断类型}

就像默认构造，默认也会生成断言函数来判断类型

```lisp
(person-p me)
```


### 限制 {#限制}

在定义改变后，实例并不会更新

```lisp
(defstruct person
  id
  (name "john doe" :type string)
  email)

attempt to redefine the STRUCTURE-OBJECT class PERSON
incompatibly with the current definition
[Condition of type SIMPLE-ERROR]

Restarts:
0: [CONTINUE] Use the new definition of PERSON, invalidating already-loaded code and instances.
1: [RECKLESSLY-CONTINUE] Use the new definition of PERSON as if it were compatible, allowing old accessors to use new instances and allowing new accessors to use old instances.
2: [CLOBBER-IT] (deprecated synonym for RECKLESSLY-CONTINUE)
3: [RETRY] Retry SLIME REPL evaluation request.
4: [*ABORT] Return to SLIME's top level.
5: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1002A0FFA3}>)
```

{{< admonition type="tip" title="CLOS" open="true" >}}
CLOS是没有这种限制的 后面会介绍到
{{< /admonition >}}


## Tree {#tree}

tree-equal,copy-tree. 会自顶向下递归进入cons cell 的car 和 cdr


### Sycamore purely functional weight-balanced binary trees {#sycamore-purely-functional-weight-balanced-binary-trees}

<https://github.com/ndantam/sycamore>

Features:

-   Fast, purely functional weight-balanced binary trees.
-   Leaf nodes are simple-vectors, greatly reducing tree height.
-   Interfaces for tree Sets and Maps (dictionaries).
-   Ropes
-   Purely functional pairing heaps
-   Purely functional amortized queue.


## 控制打印多少数据 \\\*print-length\\\*, \\\*print-level\\\*) {#控制打印多少数据-print-length-print-level}

print-length 和 print-level默认都为nil
默认，如果你有一个非常长的list, 那么在REPL中打印就会非常的长。使用print-length 控制打印元素的最大数量

```lisp
(setf *print-length* 2)
(list :A :B :C :D :E)
;; (:A :B ...)
```

print-level用来控制打印的深度

```lisp
(let ((*print-level* 2))
  (print '(:a (:b (:c (:d :e))))))
;; (:A (:B #))             <= *print-level* in action
;; (:A (:B (:C (:D :E))))  <= the list is returned, the let binding is not in effect anymore.
```

