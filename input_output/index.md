# Common-Lisp Input-Output


<!--more-->


## 流 {#流}

和很多流行的语言一样，流用来控制数据的读取和写入。
输入的缺省是\*standard-input\*,输出的缺省是\*standard-output\*,这两个是标准的输入输出流。


### pathname {#pathname}

pathname 指定了一个文件的可以直接方式，路径包含：host、device、directory、name、type和version。make-pathname 函数专门用来构造一个pathname。

```lisp
(setf path (make-pathname :name "myfile"))
;; #P"myfile
```


### open {#open}

开启文件的最基本的函数时open, 需要一个pathname, 和一些选择性的关键字参数。开启成功后，会创建一个文件的流。

-   参数 :direction
    用来控制流的方向可以 指定 :input 表示只输入, :output 表示只输出, :io 表示既可以输入也可以输出。
-   参数 :if-exists
    说明当文件已经存在时该怎么做；通常与:supersede配合使用表示如果存在就取代它

<!--listend-->

```lisp
(setf str (open path :direction :output
                     :if-exists :supersede))
;; <Stream C017E6>
```


### 对流进行操作 {#对流进行操作}


#### 输出 {#输出}

打开一个可以输出的文件

```lisp
(setf str (open path :direction :output
                     :if-exists :supersede))
```

最简单的，我们可以使用format 函数将内容输出到流中

```lisp
(format str "Something~%")
;; NIL
```

此时文件中可能有Something这个内容。也可能没有。因为流并不会马上将内容写入文件。我们可以直接close 这样内容就会立刻进入文件中。

```lisp
(close str)
;; NIL
```

{{< admonition type="warning" title="warning" open="true" >}}
输出完后，记得关闭文件。因为流不保证写入流的内容会立刻保存到文件中
{{< /admonition >}}


#### 输入 {#输入}

打开一个可以输入的文件

```lisp
(setf str (open path :direction :input))
```

可以使用read-line 读取一行文字


#### with-open-file {#with-open-file}

大部分时间，我们不使用open和close来操作文件的I/O。with-open-file宏更方便。第一个参数是一个列表(stream-value， path ,parameters-to-open-func), 第二个参数是要执行的代码主题。当代码主体执行完毕后，这个流就会被自动关闭，所以操作自然被保存在文件中。

```lisp
(with-open-file (str path :direction :output
                          :if-exists :supersede)
  (format str "Something~%"))
```


## 流的输入函数 {#流的输入函数}

流的输入有两个常用函数read-line和read.


#### read-line {#read-line}

read-line 读取一行，并用字符串返回。默认从标准输入中读取

```lisp
(progn
  (format t "Please enter you name: ")
  (read-line))
```

read-line接受4个可选参数

```lisp
(read-line stream errp 'return-value-if-errp-is-nil)
```

这里errp 表示在遇到end-of-file时是否产生错误
return-value-if-errp-is-nil表示若errp为nil应该返回什么

```lisp
(defun pseudo-cat (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof))
      (format t "~A~%" line))))
```


#### read {#read}

如果想把read解析成lisp对象 使用read. read一次读取一个表达式。在表达式结束时停止读取。读取的内容必须是合法的Lisp语法。


## 流的输出函数 {#流的输出函数}

流有三个最基本的输出函数prin1, princ, terpri

prin1和 princ 的差别在于prin1 给程序产生输出，而princ 给人类产生输出。也就是说prin1产生的输出是程序可读的，而princ 产生的输出更接近人的读取直觉。

```lisp
(prin1 "Hello")
;; "Heool"

(princ "Hello")
;; Hello
```

terpri只会打印出一个新行


#### format {#format}

format类似c语言的printf 和其他语言中的格式化输出函数。format主要的复杂在于格式化控制，可以在使用的时候查阅文档，这里就不赘述了。


## 宏字符(Macro Characters) {#宏字符--macro-characters}

宏字符是获得read特别待遇的字符，一个宏字符或者宏字符组合也叫做read-macro(读取宏)。Common Lisp预定义的读取宏很多都是缩写。比如quote的读取宏是' 'a 会被展开成 (quote a)


### dispatching {#dispatching}

派发读取宏都已#作为派发字符，比如#'是(function ...)的缩写，同样 '是 (quote ...)的缩写。#(...)产生一个vector, #nA(...)产生数组；#\\产生一个字符; #S(n ...)产生一个结构。

