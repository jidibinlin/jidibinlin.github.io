# common lisp strings


<!--more-->

你应该知道，string 在 common lisp 中它既是arrays 也是 sequences. 也就是说，arrays 和 sequences的操作都可以应用在string上。如果你找不到某个string特有的函数，你应该去找一找arrays 和 sequences的函数。

还有一些额外的libraries 托管在 quicklisp上，这里只给出英文介绍
ASDF3, which is included with almost all Common Lisp implementations, includes Utilities for Implementation- and OS- Portability (UIOP), which defines functions to work on strings (strcat, string-prefix-p, string-enclosed-p, first-char, last-char, split-string, stripln).
Some external libraries available on Quicklisp bring some more functionality or some shorter ways to do.

-   str defines trim, words, unwords, lines, unlines, concat, split, shorten, repeat, replace-all, starts-with?, ends-with?, blankp, emptyp, …
-   Serapeum is a large set of utilities with many string manipulation functions.
-   cl-change-case has functions to convert strings between camelCase, param-case, snake_case and more. They are also included into str.
-   mk-string-metrics has functions to calculate various string metrics efficiently (Damerau-Levenshtein, Hamming, Jaro, Jaro-Winkler, Levenshtein, etc),
-   and cl-ppcre can come in handy, for example ppcre:replace-regexp-all. See the regexp section.

Last but not least, when you’ll need to tackle the format construct, don’t miss the following resources:

the official CLHS documentation

-   a quick reference
-   a CLHS summary on HexstreamSoft
-   plus a Slime tip: type C-c C-d ~ plus a letter of a format directive to open up its documentation. Again more useful with ivy-mode or helm-mode.


## 创建 字符串 {#创建-字符串}

最简单的，我们可以使用双引号创建string.但是其实我们还有别的方法:

-   使用format nil

    ```lisp
    (defparameter person "you")
    (format nil "hello ~a" person) ;; => "hello you"
    ```
-   make-string count 创建指定长度的字符串。 :initial-element 字符会被重复count次

    ```lisp
    (make-string 3 :initial-element #\♥) ;; => "♥♥♥"
    ```


## 访问子串 {#访问子串}

string 是一个sequence,你可以使用subseq 来访问它的子串
先给出一个比较易懂的签名

```lisp
(subseq my-string start end)
```

这里是调用

```lisp
* (defparameter *my-string* (string "Groucho Marx"))
*MY-STRING*
* (subseq *my-string* 8)
"Marx"
* (subseq *my-string* 0 7)
"Groucho"
* (subseq *my-string* 1 5)
"rouc"
```

也可以像序列那样用setf 和 subseq 配合来操作字符串

```lisp
* (defparameter *my-string* (string "Harpo Marx"))
*MY-STRING*
* (subseq *my-string* 0 5)
"Harpo"
* (setf (subseq *my-string* 0 5) "Chico")
"Chico"
* *my-string*
"Chico Marx"
```

{{< admonition type="warning" title="string isn`t stretchable" open="true" >}}
字符串的长度是不可变的，如果新的子串的长度和原始子串的长度不同，短的那一个将决定多少个字符将被替换，

```lisp
* (defparameter *my-string* (string "Karl Marx"))
*MY-STRING*
* (subseq *my-string* 0 4)
"Karl"
* (setf (subseq *my-string* 0 4) "Harpo")
"Harpo"
* *my-string*
"Harp Marx"
* (subseq *my-string* 4)
" Marx"
* (setf (subseq *my-string* 4) "o Marx")
"o Marx"
* *my-string*
"Harpo Mar"
```
{{< /admonition >}}


## 访问单个字符 {#访问单个字符}

char函数专门用来访问字符串中的单个字符，char也可以和setf配合使用

```lisp
(defparameter *my-string* (string "Groucho Marx"))
*MY-STRING*
(char *my-string* 11)
#\x
(char *my-string* 7)
#\Space
(char *my-string* 6)
#\o
(setf (char *my-string* 6) #\y)
#\y
*my-string*
"Grouchy Marx"
```

还有一个schar也可以做到同样的事情，但是在特定情况下，schar会更快一些
因为strings 既是 arrays 也是 sequence. 你也可以用更加通用的aref 和 elt (但是char的效率会更高)

```lisp
(defparameter *my-string* (string "Groucho Marx"))
*MY-STRING*
(aref *my-string* 3)
#\u
(elt *my-string* 8)
#\M
```


## 从string中删除和替换 {#从string中删除和替换}

可以使用 sequence的函数来对string中的子串进行删除和替换操作

-   从string中删除一个字符

<!--listend-->

```lisp
(remove #\o "Harpo Marx")
"Harp Marx"
(remove #\a "Harpo Marx")
"Hrpo Mrx"
(remove #\a "Harpo Marx" :start 2)
"Harpo Mrx"
(remove-if #'upper-case-p "Harpo Marx")
"arpo arx"
```

-   使用substitute(non destructive) 或者 replace (destructive) 来替换一个字符

<!--listend-->

```lisp
(substitute #\u #\o "Groucho Marx")
"Gruuchu Marx"
(substitute-if #\_ #'upper-case-p "Groucho Marx")
"_roucho _arx"
(defparameter *my-string* (string "Zeppo Marx"))
*MY-STRING*
(replace *my-string* "Harpo" :end1 5)
"Harpo Marx"
*my-string*
"Harpo Marx"
```


## 拼接字符串 (Concatenating string) {#拼接字符串--concatenating-string}

concatenate 是sequence的通用函数，在对string进行操作时，应该指定返回值的类型

```lisp
(concatenate 'string "karl" " " "Marx")
;; => "Karl Marx"
(concatenate 'list "Karl" " " "Marx")
;; => (#\K #\a #\r #\l #\Space #\M #\a #\r #\x)
```

使用UIOP库的话，可以用strcat:

```lisp
(uiop:strcat "karl" " " marx")
```

或者是str library 使用concat:

```lisp
(str:concat "foo" "bar")
```


## 一次操作一个字符 {#一次操作一个字符}

使用Map函数一次操作一个字符

```lisp
(defparameter *my-string* (string "Groucho Marx"))
*MY-STRING*
(map 'string #'(lambda (c) (print c)) *my-string*)
#\G
#\r
#\o
#\u
#\c
#\h
#\o
#\Space
#\M
#\a
#\r
#\x
"Groucho Marx"
```

或者使用loop 函数

```lisp
(loop for char across "Zeppo"
      collect char)
(#\Z #\e #\p #\p #\o)
```


## 根据word 或 character翻转string {#根据word-或-character翻转string}

使用reverse (或者destructive 版的 nreverse) 来根据character反转字符串

```lisp
(defparameter *my-string* (string "DSL"))
*MY-STRING*
(reverse *my-string*)
"LSD"
```

在CL中 没有直接根据word反转字符串的函数，你可以使用第三方库 比如SPLIT-SEQUENCE 或者你自己实现一套解决方案
我们可以使用str库

```lisp
(defparameter *singing* "singing in the rain")
*SINGING*
(str:words *SINGING*)
;; => ("singing" "in" "the" "rain")
(str:unwords (reverse (str:words *singing*)))
;; => "rain the in singing"
```


## Breaking strings into graphenes,sentences,lines and words {#breaking-strings-into-graphenes-sentences-lines-and-words}

These functions use SBCL’s sb-unicode: they are SBCL specific.

-   sb-unicode:sentences 将string 以段落切割，根据他默认的段落分割规则
-   sb-unicode:lines 将string 分割成行（长度不会超过:margin 指定的参数 默认80）

<!--listend-->

```lisp
(sb-unicode:lines "A first sentence. A second somewhat long one." :margin 10)
;; => ("A first"
;; "sentence."
;; "A second"
;; "somewhat"
;; "long one.")
```

-   sb-unicode:words 和 sb-unicode:graphenes 可以自己去查看

{{< admonition type="tip" title="确保运行在sbcl中" open="true" >}}
```lisp
#+sbcl
(runs on sbcl)
#-sbcl
(runs on other implementations)
```
{{< /admonition >}}


## Controlling Case 控制大小写 {#controlling-case-控制大小写}

Common lisp 提供了大量的函数来控制字符串的大小写

```lisp
(string-upcase "cool")
;; => "COOL"
(string-upcase "Cool")
;; => "COOL"
(string-downcase "COOL")
;; => "cool"
(string-downcase "Cool")
;; => "cool"
(string-capitalize "cool")
;; => "Cool"
(string-capitalize "cool example")
;; => "Cool Example"
```

这些函数可以接受:start 和 :key 所以你可以只对字符串的指定部分进行操作。 这些函数也有destructive的版本都以n开头

```lisp
(string-capitalize "cool example" :start 5)
;; => "cool Example"
(string-capitalize "cool example" :end 5)
;; => "Cool example"
(defparameter *my-string* (string "BIG"))
;; => *MY-STRING*
(defparameter *my-downcase-string* (nstring-downcase *my-string*))
;; => *MY-DOWNCASE-STRING*
*my-downcase-string*
;; => "big"
*my-string*
;; => "big"
```

{{< admonition type="warning" title="warning" open="true" >}}
对于 string-upcase,string-downcase 和 string-capitalize,string 是没有被修改的。但是如果在string中没有任何字符需要转换，那么返回值有可能是源string 或者 源string的副本
{{< /admonition >}}

{{< admonition type="tips" title="tips" open="true" >}}
在CL中 n开头的函数一般是destructive的
{{< /admonition >}}


### 使用format函数控制 {#使用format函数控制}

-   To lower case:

    ```lisp
    (format t "~(~a~)" "HELLO WORLD")
    ;; => hello world
    ```
-   Capitalize every word:

    ```lisp
    (format t "~:(~a~)" "HELLO WORLD")
    ;; => Hello World
    ```
-   Capitalize the first word:

    ```lisp
    (format t "~@(~a~)" "hello world")
    ;; => Hello world
    ```
-   To upper case

    ```lisp
    (format t "~@:(~a~)" "hello world")
    ;; => HELLO WORLD
    ```


## 将字符串左右的空格截掉 {#将字符串左右的空格截掉}

其实不单单可以截掉空格，还可以丢弃一些不需要的字符。string-trim,string-left-trim,string-right-trim 返回一个子串，子串不包含第一个参数中的字符。

```lisp
(string-trim " " " trim me ")
;; => "trim me"
(string-trim " et" " trim me ")
;; => "rim m"
(string-left-trim " et" " trim me ")
;; => "rim me "
(string-right-trim " et" " trim me ")
;; => " trim m"
(string-right-trim '(#\Space #\e #\t) " trim me ")
;; = >" trim m"
(string-right-trim '(#\Space #\e #\t #\m) " trim me ")
```


## 在symbol 和 字符串之间转换 {#在symbol-和-字符串之间转换}

-   intern 将string转化成symbol

    ```lisp
    (in-package "COMMON-LISP-USER")
    ;; => #<The COMMON-LISP-USER package, 35/44 internal, 0/9 external>
    (intern "MY-SYMBOL")
    ;; => MY-SYMBOL
    (intern "MY-SYMBOL")
    ;; => MY-SYMBOL
    ;; =>:INTERNAL
    (export 'MY-SYMBOL)
    ;; => T
    (intern "MY-SYMBOL")
    ;; => MY-SYMBOL
    ;; => :EXTERNAL
    (intern "My-Symbol")
    ;; => |My-Symbol|
    ;; => NIL
    (intern "MY-SYMBOL" "KEYWORD")
    ;; => :MY-SYMBOL
    ;; => NIL
    (intern "MY-SYMBOL" "KEYWORD")
    ;; => :MY-SYMBOL
    ;; => :EXTERNAL
    ```
-   symbol-name 和 string 将symbol 转换成 string

    ```lisp
    (symbol-name 'MY-SYMBOL)
    ;; => "MY-SYMBOL"
    (symbol-name 'my-symbol)
    ;; => "MY-SYMBOL"
    (symbol-name '|my-symbol|)
    ;; => "my-symbol"
    (string 'howdy)
    ;; => "HOWDY"
    ```


## 在string 和 character之间转换 {#在string-和-character之间转换}

-   coerce 将string(长度为1)转换成character.

    ```lisp
    (coerce "a" 'character)
    ;; => #\a
    (coerce (subseq "cool" 2 3) 'character)
    ;; => #\o
    ```
-   coerce 将字符串转换中字符list

    ```lisp
    (coerce "cool" 'list)
    ;; => (#\c #\o #\o #\l)
    ```
-   coerce 将字符list转换成string

    ```lisp
    (coerce '(#\h #\e #\y) 'string)
    ;; => "hey"
    ```
-   coerce 将array 转换成string

    ```lisp
    (defparameter *my-array* (make-array 5 :initial-element #\x))
    ;; => *MY-ARRAY*
    *my-array*
    ;; => #(#\x #\x #\x #\x #\x)
    (coerce *my-array* 'string)
    ;; => "xxxxx"
    ```


## 在string中寻找一个元素 {#在string中寻找一个元素}

使用find,position 和他们的-if后缀的函数 查找string中的character

```lisp
(find #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equal)
;; => #\t
(find #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equalp)
;; => #\T
(find #\z "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equalp)
;; => NIL
(find-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks.")
;;=> #\1
(find-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks." :from-end t)
;; => #\0
(position #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equal)
;; => 17
(position #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equalp)
;; => 0
(position-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks.")
;; => 37
(position-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks." :from-end t)
;; => 43
```

使用count族函数计算字符在字符串中出现的次数

```lisp
(count #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equal)
;; => 2
(count #\t "The Hyperspec contains approximately 110,000 hyperlinks." :test #'equalp)
;; => 3
(count-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks.")
;; => 6
(count-if #'digit-char-p "The Hyperspec contains approximately 110,000 hyperlinks." :start 38)
;; => 5
```


## 在字符串中查找一个子串 {#在字符串中查找一个子串}

```lisp
(search "we" "If we can't be free we can at least be cheap")
;; => 3
(search "we" "If we can't be free we can at least be cheap" :from-end t)
;; => 20
(search "we" "If we can't be free we can at least be cheap" :start2 4)
;; => 20
(search "we" "If we can't be free we can at least be cheap" :end2 5 :from-end t)
;; => 3
(search "FREE" "If we can't be free we can at least be cheap")
;; => NIL
(search "FREE" "If we can't be free we can at least be cheap" :test #'char-equal)
;; => 15
```


## 将string 转换成number {#将string-转换成number}

-   to integer 会返回两个值，一个是被转换后的值，另一个是转换停止的位置

    ```lisp
    (parse-integer "42")
    ;; => 42
    ;; => 2
    (parse-integer "42" :start 1)
    ;; => 2
    ;; => 2
    (parse-integer "42" :end 1)
    ;; => 4
    ;; => 1
    (parse-integer "42" :radix 8)
    ;; => 34
    ;; =>2
    (parse-integer " 42 ")
    ;; => 42
    ;; => 3
    (parse-integer " 42 is forty-two" :junk-allowed t)
    ;; => 42
    ;; => 3
    (parse-integer " 42 is forty-two")

    Error in function PARSE-INTEGER:
    There's junk in this string: " 42 is forty-two".
    ```
-   转换成任意number: read-from-string

    ```lisp
    (read-from-string "#X23")
    ;; => 35,4
    (read-from-string "4.5")
    ;; => 4.5,3
    (read-from-string "6/8")
    ;; => 3/4,3
    (read-from-string "#C(6/8 1)")
    ;; => #C(3/4 1),9
    (read-from-string "1.2e2")
    ;; => 120.00001,5
    (read-from-string "symbol")
    ;; SYMBOL.6
    (defparameter *foo* 42)
    ;; => *FOO*
    (read-from-string "#.(setq *foo* \"gotcha\")")
    ;; => "gotcha",23
    *foo*
    ;; => "gotcha"
    ```


## 转换成float {#转换成float}

parse-float 库提供转换成float的函数

```lisp
(ql:quickload "parse-float")
(parse-float:parse-float "1.2e2")
;; => 120.00001,5
```


## number 转 string {#number-转-string}

```lisp
(write-to-string 250)
;; => "250"
(write-to-string 250.02)
;; => "250.02"
(write-to-string 250 :base 5)
;; => "2000"
(write-to-string (/ 1 3))
;; => "1/3"
```


## 字符串比较 {#字符串比较}

equal 和 equalp 都可以比较两个字符串是否相同，但是equal是大小写敏感的，而equalp不是。还有一些string专用的函数。

```lisp
(string= "Marx" "Marx")
;; => T
(string= "Marx" "marx")
;; => NIL
(string-equal "Marx" "marx")
;; => T
(string< "Groucho" "Zeppo")
;; => 0
(string< "groucho" "Zeppo")
;; => NIL
(string-lessp "groucho" "Zeppo")
;; => 0
(mismatch "Harpo Marx" "Zeppo Marx" :from-end t :test #'char=)
;; => 3
```


## String formatting {#string-formatting}

see <https://lispcookbook.github.io/cl-cookbook/strings.html#string-formatting>


## 捕获哪些东西被打印进了stream {#捕获哪些东西被打印进了stream}

在(with-output-to-string (mystream) ...) 中任何打印进stream中的内容都会被捕获

```lisp
(defun greet (name &key (stream t))
   ;; by default, print to standard output.
   (format stream "hello ~a" name))

(let ((output (with-output-to-string (stream)
                (greet "you" :stream stream))))
   (format t "Output is: '~a'. It is indeed a ~a, aka a string.~&" output (type-of output)))
;; Output is: 'hello you'. It is indeed a (SIMPLE-ARRAY CHARACTER (9)), aka a string.
;; NIL
```


## 删除标点符号 {#删除标点符号}

使用(str:remove-punctuation s) 或者 (str:no-case s)

```lisp
(str:remove-punctuation "HEY! What's up ??")
;; "HEY What s up"

(str:no-case "HEY! What's up ??")
;; "hey what s up"
```

