# golang syntax


<!--more-->


## 定义(Declarations) {#定义--declarations}

定义用来描述一个编程元素的基本属性，比较难理解。通俗来说，就是指定这个元素是干什么的。元素被定义的那一刻就被赋予了职能。比如总统有总统该干的事，猫天生就会抓老鼠，水可以呈现液态，固态，气态三种形式。
golang有三种形式的定义

-   var 用来定义变量
-   const 用来定义常量
-   type 用来定义类型
-   func 用来定义函数


### 定义的作用域 {#定义的作用域}

定义是分作用范围的，golang的作用范围分为

-   package-level 包级别: 在包中都能被访问到
-   func-level 函数级别: 只能在函数中被访问到
-   block-level 块级别: 只能在代码块中被访问到

三个作用域级别由大到小，block-level最小.


#### package-level {#package-level}

```go
package main //申明所在的包

import "fmt"

const boilingF = 212.0
func main() {
}
```

在这段代码中 boilingF 就是包级别的常量. 只要项目文件的开头申明了 **package main** 那么boilingF就可以在这个文件中被访问到


#### func-level {#func-level}

```go
package main

import "fmt"

func main() {
    const  boilingF = 212.0
}
```

这段代码的boilingF就只能在main函数中被访问到


#### block-level {#block-level}

```go
package main

import "fmt"

func main() {
    if true {
        const boilingF = 212.0
    }
}
```

这里的boilingF就只能在if代码块中被访问


#### 同名定义的访问顺序自底向上 {#同名定义的访问顺序自底向上}

试想一种场景，若在三个作用域级别中，我们分别都定义了 **boilingF** 那我们将访问到哪一个 **boilingF**

```go
package main

import "fmt"
const boilingF = 212.0
func main() {
    const boilingF = 213.0
    fmt.Printf("boilingF %d\n",boilingF)
    if true {
        const boilingF = 214.0
        fmt.Printf("boilingF %d\n",boilingF)
    }
}
```

执行结果

```nil
boilingF = 213.000000
boilingF = 214.000000
```

可以看到第一个打印打印的是213而不是212.0 说明212被213隐藏了. 而在if代码块中 打印的是214而不是213说明213被隐藏了。
这说明若有同名变量，那么低作用域的定义将会覆盖高作用域的定义


#### 同作用域内，不可重定义同名变量 {#同作用域内-不可重定义同名变量}

```go
package main

import "fmt"

const boilingF = 212.0
const boilingF = 212.0

func main() {

}
```

这段代码编译是不会通过的，因为定义了两次boilingF属于重定义错误

{{< admonition type="note" title="note" open="true" >}}
这些只是一些基本的知识，更多的有关定义的坑将会在下面的变量中介绍
{{< /admonition >}}


## 变量(variable) {#变量--variable}

变量有两种定义方式

-   **var**

<!--listend-->

```go
var name type = expression
```

-   语法糖 **:=**

<!--listend-->

```go
name := expression
```


### 使用var来定义 {#使用var来定义}

在使用var 来定义变量时 type 和 expression可以省略其中一个 但是不可以同时省略（同时省略的话，go将不清楚我们具体要定义什么）


#### 省略type(自动推倒类型) {#省略type--自动推倒类型}

省略type 的时候 golang将会根据expression自动推倒类型， 后面将会介绍的 := 就是这种写法的语法糖
比如

```go
var name = "jidibinlin" // name 的类型是string
var age = 12 // age 的类型是age
```


#### 省略expression {#省略expression}

若省略expression 变量将被默认值（0值)取代。

-   对于数值类型，默认值将是0, 对于字符串将会是""
-   对于bool型将会是false,对于引用类型(slice,pointer,map,channel,function) 将会是nil
-   对于array和struct来说，他们的元素将会是0值

<!--listend-->

```go
var s string // s = ""
var i int // i = 0
var b bool // b = false
var slc []int // slic = nil

var arr [3]int // arr[0]=0 arr[1]=0 arr[2]=0

type astruct struct {
    s string
    i int
    b bool
    slc []int
}

var stru astruct // stru.s = "" stru.i = 0 stru.b = false stru.slc = nil
```


#### 一次定义多个变量 {#一次定义多个变量}

```go
var i, j, k int //int, int, int
var b, f, s = true, 2.3, "fout" //bool,float64,string
```


#### 接受多返回值函数的返回值 {#接受多返回值函数的返回值}

go可以定义多返回值的函数。 使用var 就可以很便捷的申明多个变量用来接受函数的返回值.

```go
var f,err = os.Open(name) //os.Open 会返回一个file 和 一个error
```

{{< admonition type="note" title="note" open="true" >}}
注意，由于函数的返回值类型是不确定的。 而var 在定义多变量的时候只能是同一类型的

```go
var i, j ,k int
```

所以在接收多返回值函数的时候，建议使用自动类型推倒(强烈建议)
{{< /admonition >}}


### := 语法糖 {#语法糖}

**:=** 是用来定义和初始化变量的语法糖

```go
name := expression
```

乍一看似乎和自动推倒类型的var 形式很像，但是两者是有区别的. 两者都可以自动推倒变量的类型, 都可以同时申明多个variable

```go
var name = "" //name的类型为string
name1 := ""    // name的类型为string
name2, name3, name4 := "","",""
```

但是 **:=** 语法糖只能在函数的内部用来申明和初始化变量,而var 却可以用在函数外部

```go
package main

var i = 1 // right

j := 1 //wrong
func main() {
    k := 2 //right
    var g = 1 //right
}
```

**:=** 和 var 都会申明新的变量，但是:=却会考虑重用已有的变量

```go
var n1, n2 = "",""
var n2, n3 = "","" // wrong 这里会报n2重定义
```

```go
name1,name2 := "",""
name2,name3 := "","" //right := 会重用name2 (如果有已经申明的变量的话 会重用)
```

{{< admonition type="warning" title="warning" open="true" >}}
注意 **:=** 的左侧必须有未被申明的变量才能被执行

```go
name1,name2 :="",""
name1,name2 :="","" //wrong
```
{{< /admonition >}}


### Pointer指针 {#pointer指针}

如果对c系列的语言有了解的话，应该对指针是不陌生的. 在go 中指针的用法和c语言几乎相同 使用\* 声明/解引用，使用&amp;来取地址

```go
x := 1
p := &x //取地址 并声明一个p指针指向x
*p = 2 // 将2存进p指向的变量也就是x
```

有关go的指针不过多介绍


### new函数 {#new函数}

new也可以用来创建指针,但是new创建的是指针类型,使用的时候直接在传入一个类型即可

```go
p := new(int) //p指向一块int型的内存空间
*p = 2 //给p指向的空间赋值
```

new 和声明指针的区别在于，new会先分配内存空间再将这块空间的地址返回，而声明指针只是声明了一个指针，但是它并不指向任何地址。new 更像是先声明变量，再将变量的地址返回.


### 变量的生命周期 {#变量的生命周期}

除了包级别的变量外(声明在函数外的变量), 其余变量的生命周期都是动态的。
包级别的变量生命周期将会持续到整个程序被终止。
动态生命周期指变量从变量被声明的那一刻起，一直到它无法被访问到为止。当变量无法被访问时他的内存可能会被回收。
由于变量的生命周期是由其能否被访问到决定的，所以变量有可能会跳出循环或者函数继续存在。

{{< admonition type="note" title="note" open="true" >}}
一般来说局部变量的可以跳出代码块而存在的话，我们就说这是一个逃逸变量。相对于非逃逸变量来说，逃逸变量的内存将会被分配在堆上，而非逃逸变量是分配在栈上的。
c++ 程序员需要关心变量是分配在栈上好，还是分配在堆上好。但是对于go来说，这些都是编译器自动完成的，程序员无需关心变量的内存是分配在栈上还是堆上。

栈是及时回收的数据结构，所有在栈上声明的变量在函数结束的那一刻都会被回收，所以如果变量在函数外还可以被使用，就应该分配在堆上。c++中堆是程序员自己释放的，而在go中，go有自己的回收方案。
{{< /admonition >}}

```go
var global *int

func f() {
    var x int //x将被分配在堆上，因为global指向它，他逃逸出了f函数
    x = 1
    global = &x
}

func g() {
    y := new(int) //y将被分配在栈上，因为他不是逃逸变量，出了g函数后，是没法被访问的
    *y = 1
}
```


## 变量赋值 {#变量赋值}

赋值可以更新变量存储的value,但是左边和右边的类型必须是匹配的（比如左右都是同类型的,都实现了同一个接口）

```go
x = 1
*p = true
person.name = "bob"
count[x] = count[x]*scale
```

c语言支持的赋值操作go都支持包括++,--,+=,-=, 移位等等


### 元组赋值(tuple assignment) {#元组赋值--tuple-assignment}

元组赋值允许多个变量同时被赋值，所有右边的变量都会先被运算然后再赋值给左边。也就是说你可以这样交换两个变量的值

```go
x, y = y, x
```

前面有提到过，go的返回值可以有多个。go习惯用第二个返回值表示函数执行的状态，第二个返回值可以是error,或者一个bool值ok,这样就可以知道在取出变量和调用函数的时候是否有错误和异常出现。

```go
f, err = os.Open("foo.text") //如果err 不为nil的话表示打开文件失败
```

这一点在go中经常被用到

```go
v,ok = m[key] //从map中取值
v,ok = x.(T) //类型断言
v,ok = <-ch   //从channel中接受
```

如果你并不需要用到某些返回值，你可以用 "_" 占位来忽略它

```go
_, err = io.Copy(dst,src) //discard byte count
_, ok = x.(T)            //check type but discard result
```

但是捏，不要这样用

```go
v, _ = io.Copy(dst,src)
```

这样用和

```go
v = io.Copy(dst,src)
```

是没有区别的，但是上面的形式不推荐。因为元祖赋值是按顺序的，如果左边变量数量不足，那么右边的多余返回值会被自动忽略

{{< admonition type="note" title="note" open="true" >}}
在go中函数经常以返回一个err或者bool 值的形式来告诉函数在调用的时候是否出现问题。这是go处理异常的方式。
{{< /admonition >}}


## Type定义 {#type定义}

variable和expression的类型定义了这个variable的特征，例如这个值的大小，内部是如何表示的，可以对它做哪些操作，和它相关的函数有哪些。
type 可以定义一个新的类型（基于已有类型，并且具有他的一切特性）

```go
type name underlying-type
```

type定义一般写在包级别的作用域中，这样对整个包中的代码都是可见的。如果这个类型被导出了，也可以在包外使用.
我门先看一段简短的代码

```go
package tempconv

import "fmt"

type Celsius float64
type Fahrenheit float64

const (
    AbsoluteZeroC Celsius = -273.15
    FreezingC     Celsius = 0
    BoilingC      Celsius = 100
)

func CToF (c Celsius) Fahrenheit {
    return Fahrenheit(c*9/5 + 32)
}

func FToC (f Fahrenheit) Celsius {
    return Celsius((f-32) * 5 /9)
}
```

这段代码定义了温度的两种表示方法Celsius(摄氏度)和Fahrenheit(华氏度),两者的基本类型都是Float64。Celsius和Fahrenheit是两种不同的类型，所以两者不能做数学运算和逻辑运算。这样做是为了防止因为表示温度的单位不同而导致错误。Celsius(t)和Fahrenheit(t)是转型器，并不是函数调用。type定义显示的改变了类型的意义。

对于任意的类型T都有对应的转型操作T(x)用来将其他类型的值x转型成T类型的值. 当两种类型的基本类型是相同的时候，转型才被允许。如果是指针类型，被指向的值必须具有相同的基本类型才能够被转型。这些转型只会转换type但是值的表示是不会改变的。

转型也能发生在数值类型，string,和一些slice类型之间，这些转型可能会改变值的表示，比如将float类型转型成integer类型会丢失小数点。将string类型转成[]byte slice.

基本类型决定了 named type的结构和表示，以及所支持的操作，就像是基本类型被直接使用一样。不过，我们可以为named type 额外定义一些操作。

```go
func (c Celsius) string() string {
    return fmt.Sprintf("%g*c"，c)
}
```

这段代码会返回Celsius的string类型并以\*c结尾

{{< admonition type="note" title="note" open="true" >}}
许多类型都定义了string操作，这样在使用fmt打印的时候，就可以更加的好看
{{< /admonition >}}


## packages 和 Files {#packages-和-files}

go中的package就像其他语言中的libraries 和 modules. 提供modularity(模块化),encapsulation(封装),separate compilation(隔离编译)和reuse(重用)特性。代码可以写在package中多个.go结尾的文件中。
每个包都有一个独立的命名空间，当我们要在外部使用这个包中的东西的时候，必须要加上包名作为前缀。例如要是用image包中的Decode我们需要 image.Docode,要使用utf16包中的Decode我们需要utf16.Decode.
要想导出package中的变量，定义，函数，我们必须在定义他们的时候以大写字母开头。我们举一个例子

```go
//Package tempconv performs Celsius and Fahrenheit conversions.
package tempconv

import "fmt"
type Celsius float64
type Fahrenheit float64

const (
    AbsoluteZeroC Celsius = -273.15
    FreezingC Celsius = 0
    BoilingC celsius = 100
)

func (c Celsius) String() string {
    return fmt.Sprintf("%g*c",c)
}

func (f Fahrenheit) String() string {
    return fmt.Sprintf("%g*F",f)
}
```

```go
package tempconv

// CToF converts a Celsius temperature to Fahrenheit.
func CToF (c Celsius) Fahrenheit {
    return Fahrenheit(c*9/5 + 32)
}

//FToC converts a Fahrenheit temperature to Celsius.
func FToC (f Fahrenheit) Celsius {
    return Celsius((f-32) *5 / 9)
}
```

在包中，所有文件都应以包定义开头，这里是 package tempconv. 当包被导入后，我们就可以使用tempconv.CToF 这样的形式使用包中暴露出来的成员(以大写字母开头)。


### imports {#imports}

在go中，所有的包都被一个唯一的import path标识(string 类型) 比如"foo/tempconv"。import path 表明了包含包中的文件的路径. 除了import path 每个包还有一个package name, 相对import path要短很多. 通常一个包的名字通常是import path 的最后一个分割段 比如foot/tempconv 的包名就是tempconv.

```go
// Cf converts its numeric argument to Celsius and Fahrenheit.
package main

import (
    "fmt"
    "os"
    "strconv"

    "foo/tempconv"
)

func main() {
    for _, arg := range os.Args[1:] {
        t, err := strconv.ParseFloat(arg, 64)
        if err != nil {
            fmt.Fprintf(os.Stderr, "cf: %v\n", err)
            os.Exit(1)
        }
        f := tempconv.Fahrenheit(t)
        c := tempconv.Celsius(t)
        fmt.Printf("%s = %s, %s = %s\n",
            f, tempconv.FToC(f), c, tempconv.CToF(c))
    }
}
```

我们应直接使用tempconv(包名)而不是import path,我们也可以在import 的时候定义包的别名以防止冲突.

```go
package main

import (
    "fmt"
    "os"
    "strconv"

    conv "foo/tempconv"
)


func main () {
    for _, arg := range os.Args[1:] {
        t,err := strconv.ParseFloat(arg,64)
        if err != nil {
            fmt.Fprintf(os.Stderr,"cf:%v\n",err)
            os.Exit(1)
        }
        f := conv.Fahrenheit(t)
        c := conv.Celsius(t)
        fmt.Printf("%s = %s, %s = %s\n", f,conv.FToC(f),c,conv.CToF(c))
    }
}
```

导入未使用的包是不被允许的，在编译前必须删除多余的导入，推荐使用golang.org/x/tools/cmd/goimports 工具。 它会自动导入和删除不必要的导入。


### 包初始化 {#包初始化}

package会按定义的顺序先初始化包级别的变量（但是会先解决依赖）

```go
var a = b + c // a initialized third, to 3
var b = f() // b initialized second, to 2, by calling f
var c = 1 // c initialized first, to 1

func f() int { return c + 1 }
```

为了使用导入的程序包，必须首先对其进行初始化，而包的初始化过程可以由这张图表示出来

{{< figure src="/ox-hugo/2022-06-20_20-28-37_ejLR69f443.png.png" >}}

流程是

1.  先导入包（递归导入）
2.  初始化const
3.  初始化var
4.  调用init()函数

{{< admonition type="warning" title="warning" open="true" >}}
在golang中 init()函数会在初始化时被自动调用，但是init()函数不能够被手动调用。同一个包中可以出现多个init()函数，但是init()的调用顺序是不被保证的。所以尽量只写一个init()函数
{{< /admonition >}}

{{< admonition type="warning" title="warning" open="true" >}}
需要注意的是，golang中不允许导入了，但未被使用的包存在，也不允许定义了但未使用的变量存在
{{< /admonition >}}

