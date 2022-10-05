# Common-Lisp numbers


<!--more-->


## integer types {#integer-types}

CL 提供一个true integer类型，叫bignum,只受内存控制。
为了效率考虑，integers 可以被限制成fixnum type。integers 的范围可以这样查看

```lisp
most-positive-fixnum
4611686018427387903
most-negative-fixnum
-4611686018427387904
```

integer 相关的函数有

-   isqrt, 返回小于且最接近于指定数的平方根
    ```lisp
    (isqrt 10)
    ;; => 3
    (isqurt 4)
    ;; => 2
    ```
-   gcd 返回最大的公分母
-   lcm 返回最小的公倍数

CL还提供了表示16进制和其他进制的方法

```lisp
#xFF
;; => 255
#2r1010
;; => 10
#4r33
;; => 15
#8r11
;; => 9
#16rFF
;; => 255
#36rz
;; => 35
```


## 有理数 {#有理数}

ratio 类型由两个bignums组成

```lisp
* (/ (1+ (expt 2 100)) (expt 2 100))
1267650600228229401496703205377/1267650600228229401496703205376
```

ratio 是 rational 的子类型


## 浮点类型 {#浮点类型}

CL 提供精度由小到大排列的short-float, single-float, double-float, and long-float 类型
常量short-float-epsilon, single-float-epsilon, double-float-epsilon and long-float-epsilon 表示了浮点类型的精度


### floating point literals (浮点字面量) {#floating-point-literals--浮点字面量}

\*read-default-float-format\*控制了浮点数读取的默认类型，默认是single-float,如果你想读入双精度的浮点数，你需要d0后缀

```lisp
(type-of 1.24)
;; => SINGLE-FLOAT

(type-of 1.24d0)
;; => DOUBLE-FLOAT
```

Other suffixes are s (short), f (single float), d (double float), l (long float) and e (default; usually single float).
默认的type 是可以修改的

```lisp
(setq *read-default-float-format* 'double-float)
(type-of 1.24)
;; => DOUBLE-FLOAT
```

{{< admonition type="warning" title="warning" open="true" >}}
和其他语言不同的是，在十进制后面加小数点并不能将该数表示成浮点数

```lisp
(type-of 10.)
;; => (INTEGER 0 4611686018427387903)

(type-of 10.0)
;; => SINGLE-FLOAT
```
{{< /admonition >}}


### Float point errors {#float-point-errors}

这个错误一般发生在小数点溢出的时候

```lisp
(exp 1000)
;; Evaluation aborted on #<FLOATING-POINT-OVERFLOW {10041720B3}>.
```

这个错误可以被捕获和解决，或者他的行为可以被改变

```lisp
(sb-int:set-floating-point-modes :traps '(:INVALID :DIVIDE-BY-ZERO))

(exp 1000)
;; => #.SB-EXT:SINGLE-FLOAT-POSITIVE-INFINITY

(/ 1 (exp 1000))
;; => 0.0
```

现在不会报任何的错误
在sbcl中，float-point 的模式可以被检查

```lisp
(sb-int:get-floating-point-modes)
;; => (:TRAPS (:OVERFLOW :INVALID :DIVIDE-BY-ZERO) :ROUNDING-MODE :NEAREST
;;  :CURRENT-EXCEPTIONS NIL :ACCRUED-EXCEPTIONS NIL :FAST-MODE NIL)
```


### 高精度计算 {#高精度计算}

quicklisp 中有一个computable-reals库用于高精度计算

```lisp
(ql:quickload :computable-reals)
(use-package :computable-reals)

(sqrt-r 2)
;; => +1.41421356237309504880...

(sin-r (/r +pi-r+ 2))
;; => +1.00000000000000000000...
```


## 复数 {#复数}

see <https://lispcookbook.github.io/cl-cookbook/numbers.html#complex-types>


## Rounding floating-point and rational numbers {#rounding-floating-point-and-rational-numbers}

ceiling,floor,round 和 truncate 可以将float数转换成integer.
see <https://lispcookbook.github.io/cl-cookbook/numbers.html#reading-numbers-from-strings>


## 比较数字 {#比较数字}

这个真的没啥要将的，除了lisp使用前缀表达式。
直接看common lisp cookbook吧
see <https://lispcookbook.github.io/cl-cookbook/numbers.html#comparing-numbers>


## 随机数 {#随机数}

random 函数用来生成随机数

```lisp
(random 10)
;; => 7

(type-of (random 10))
;; => (INTEGER 0 4611686018427387903)
(type-of (random 10.0))
;; => SINGLE-FLOAT
(type-of (random 10d0))
;; => DOUBLE-FLOAT
```

随机种子被存放在\*random-state\*中 使用make-random-state可以生成新的随机状态
如果想时候相同的随机集合多次，可以使用(make-random-state nil)

```lisp
(dotimes (i 3)
  (let ((*random-state* (make-random-state nil)))
    (format t "~a~%"
            (loop for i from 0 below 10 collecting (random 10)))))

(8 3 9 2 1 8 0 0 4 1)
(8 3 9 2 1 8 0 0 4 1)
(8 3 9 2 1 8 0 0 4 1)
```


## 按位操作 {#按位操作}

这个地方common lisp cookbook 写的很好
see <https://lispcookbook.github.io/cl-cookbook/numbers.html#bit-wise-operation>

