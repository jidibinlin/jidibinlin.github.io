# golang反射


<!--more-->

会想起来写这篇博客是因为，最近总是在想怎么让golang的代码写的更动态一点。再加上最近在关注架构上面的事情，无意中让我看到了gonet这个框架中对golang反射的使用。让我一时间有种惊讶的感觉（这东西还能这么用？😂）, 于是感觉还是先把这上面的只是恶补一下，也许对后面写架构有好处也不一定呢。


## 引言 {#引言}

在讨论反射之前，先说一下反射的利弊。反射可以用来做非常灵活的工作，但是反射的使用往往和代码的可读性走向对立面。这唤起了我在第一家公司的记忆。

第一家公司对于游戏的理解非常透彻，主程的水平非常的高，对游戏中各个元素的抽象非常透彻。他对代码的优化，以及简洁性也有一定的偏执。
项目解耦非常彻底，这应该是一个对项目里所有人都非常友好的框架。但是对我不是，我经常被模块里面的各种抽象和继承弄得烦不胜烦。

因为lua不像java那样有非常标准的面向对象概念，lua是函数式语言，lua的table数据结构可以用来实现很多数据结构，链表，队列，甚至用它实现一套面向对象。但是，不是语言原生支持的东西就总是有别扭的地方。项目没有任何文档，我在完全不了解框架的情况下开始了工作。印象最深的是，我经常担心我用的对象里面是没有那个字段的，于是逼迫着我去仔细梳理一遍代码的来龙去脉。这种体验让我感觉非常糟糕。加上没有类型检查，没有debug调试器（把我喜欢用调试器的毛病给纠正过来了，现在我只习惯打log😂）

而反射的使用，就会带来可读性的问题。应用反射的最主要目的是为了提高代码的灵活性，简洁性，复用率。但是反射会模糊程序内部逻辑，还会有性能问题。反射代码会比普通的代码要更加难读。所以反射的使用应该慎重。反射是一把双刃剑。


## 基本概念 {#基本概念}

golang 的reflect包提供了反射支持，golang中有两个重要类型Type和Value,任意的接口值都由reflect.Type 和 reflect.Value组成


### 接口值 {#接口值}

简单说接口值就是由reflect.Type 和 reflect.Value组成的值. Type部分保存了他的动态类型描述符，value部分保存了他的动态值。
接口值的作用是用来描述。反射的基本理念就是让程序能够自我描述，那么接口值的定义就是为了能够让程序自我描述。

{{< admonition type="note" title="动态类型描述符" open="true" >}}
对于go语言这种静态类型语言，类型只是编译期的概念。类型不是值，类型信息被称为类型描述符。所以接口值中的类型部分代表类型的描述符
{{< /admonition >}}

看下面的代码

```go
var w io.Writer //w是一个io.Writer类型的空接口值
w = os.Stdout //将*os.File类型的值赋值给变量w
w = new(bytes.Buffer) // 将*bytes.Buffer类型的值赋值给变量w
w = nil // 将nil赋值给接口值
```

1.  由浅入深 先看第一行代码
    ```go
    var w io.Writer
    ```
    定义了一个空的接口值, 这里表明w只能接收实现了io.Writer的接口的接口值。但是对于w本身来说他的内部是这样的。这里表明，他没有描述任何具体类型的值

    {{< figure src="/ox-hugo/2022-07-09_11-44-22_ch7-01.png" >}}

    golang中只有值传递，而接口值是用来描述存储在接口值用值的类型描述符和它的真实值的

2.  再看第二行，我们给接口值一个值
    ```go
    w = os.Stdout
    ```
    此时 w的内部接口是这样的

    {{< figure src="/ox-hugo/2022-07-09_11-51-43_ch7-02.png" >}}

    前面说过接口值的作用是用来描述，这里接口值传达的信息是，我描述了一个\*os.File类型的值, 到这里的时候，是不是一下就清晰了。
3.  第三行
    ```go
    w = new(bytes.Buffer)
    ```
    这里w被用来描述\*bytes.Buffer返回的值

    {{< figure src="/ox-hugo/2022-07-09_12-01-38_ch7-03.png" >}}

4.  第四行 再将w内部制空，让它不在描述任何值
    ```go
    w = nil
    ```

接口值可以用==和!=来比较，但是只有当接口值都为nil或者它们的动态类型相同且动态值也可以用==去判断相等。
如果两个接口值动态类型相同，但是动态类型不可比较（切片就属于这种）是会导致panic的（注意，因为反射的代码没有类型检查，所以可以被编译通过，只有运行的时候才会报错, 所以要尤为注意）

```go
var x interface{} = []int{1, 2, 3}
fmt.Println(x == x) // panic: comparing uncomparable type []int
```

{{< admonition type="warning" title="一个包含nil指针的接口不是nil接口" open="true" >}}
一个不包含任何值的nil接口值，和一个刚好包含nil指针的接口值是不同的
空的接口值是还没有被赋值的接口值（golang中，都是值，空接口也是值），没有被赋值的接口长这样

{{< figure src="/ox-hugo/2022-07-09_11-44-22_ch7-01.png" >}}

而包含了nil指针的接口值长这样

{{< figure src="/ox-hugo/2022-07-09_14-19-06_ch7-05.png" >}}

空接口值是指它内部的type 和 value都为nil，这是 w == nil 时才回返回true
{{< /admonition >}}


### 反射的类型对象 reflect.Type {#反射的类型对象-reflect-dot-type}

reflect包提供reflect.TypeOf()获取任意值的类型对象(reflect.Type), 通过类型对象，我们就可以知道任意值的类型信息

```go
package main

import (
    "fmt"
    "reflect"
)

func main() {
    var a int
    typeOfA := reflect.TypeOf(a) //获取类型变量
    fmt.Println(typeOfA.Name(), typeOfA.Kind()) //获取类型名(int)，可类型种类(int)
}
```

```text
int int
```

Name(),Kind()是类型对象的成员函数分别用来获取类型名，和类型的种类


### 反射的类型 type 和 种类 Kind {#反射的类型-type-和-种类-kind}

Type指的是原生go 数据类型和使用type关键字定义的类型的集合,而Kind指的是种类要比Type更加范化。在需要区分大品种的时候，我们会用到Kind。


#### Kind的定义 {#kind的定义}

Kind在reflect包的定义中有如下

```go
type Kind uint

const (
    Invalid Kind = iota  // 非法类型
    Bool                 // 布尔型
    Int                  // 有符号整型
    Int8                 // 有符号8位整型
    Int16                // 有符号16位整型
    Int32                // 有符号32位整型
    Int64                // 有符号64位整型
    Uint                 // 无符号整型
    Uint8                // 无符号8位整型
    Uint16               // 无符号16位整型
    Uint32               // 无符号32位整型
    Uint64               // 无符号64位整型
    Uintptr              // 指针
    Float32              // 单精度浮点数
    Float64              // 双精度浮点数
    Complex64            // 64位复数类型
    Complex128           // 128位复数类型
    Array                // 数组
    Chan                 // 通道
    Func                 // 函数
    Interface            // 接口
    Map                  // 映射
    Ptr                  // 指针
    Slice                // 切片
    String               // 字符串
    Struct               // 结构体
    UnsafePointer        // 底层指针
)
```

Map, Slice, Chan属于引用类型，但是属于独立的种类. type A struct{} 数据Struct 种类, type Enum int 是Emum类型 是int种类


## 从类型对象中获取类型名称和种类 {#从类型对象中获取类型名称和种类}

reflect.Type对象的Name()可以用来获取类型名称，而Kind()就可以帮我们获取种类。

```go
package main

import (
    "fmt"
    "reflect"
)

type Enum int

const (
    Zero Enum = 0
)

func main() {
    // 声明一个空结构体
    type cat struct {
    }
    // 获取结构体实例的反射类型对象
    typeOfCat := reflect.TypeOf(cat{})
    // 显示反射类型对象的名称和种类
    fmt.Println(typeOfCat.Name(), typeOfCat.Kind())
    // 获取Zero常量的反射类型对象
    typeOfA := reflect.TypeOf(Zero)
    // 显示反射类型对象的名称和种类
    fmt.Println(typeOfA.Name(), typeOfA.Kind())
}
```

```text
cat struct
Enum int
```


## 使用反射获取结构体成员的类型 {#使用反射获取结构体成员的类型}

如果类型是结构体，在使用reflect.TypeOf()获取反射对象类型信息后，可以通过 NumField()和Field()方法获得结构体成员的详细信息。

| 方法                                                        | 说明                                                                |
|-----------------------------------------------------------|-------------------------------------------------------------------|
| Field(i int) StructField                                    | 根据索引返回索引对应的结构体字段的信息，当值不是结构体或索引超界时发生宕机 |
| NumField() int                                              | 返回结构体成员字段数量，当类型不是结构体或索引超界时发生宕机        |
| FieldByName(name string) (StructField, bool)                | 根据给定字符串返回字符串对应的结构体字段的信息，没有找到时 bool 返回 false，当类型不是结构体或索引超界时发生宕机 |
| FieldByIndex(index []int) StructField                       | 多层成员访问时，根据 []int 提供的每个结构体的字段索引，返回字段的信息，没有找到时返回零值。当类型不是结构体或索引超界时发生宕机 |
| FieldByNameFunc(match func(string) bool) (StructField,bool) | 根据匹配函数匹配需要的字段，当值不是结构体或索引超界时发生宕机      |


#### 结构体字段类型 {#结构体字段类型}

reflect.Type的Field()会返回StructField结构。这个结构描述了结构体成员的信息

```go
type StructField struct {
    Name string          // 字段名
    PkgPath string       // 字段路径
    Type      Type       // 字段反射类型对象
    Tag       StructTag  // 字段的结构体标签
    Offset    uintptr    // 字段在结构体中的相对偏移
    Index     []int      // Type.FieldByIndex中的返回的索引值
    Anonymous bool       // 是否为匿名字段
}
```


#### 获取成员信息 {#获取成员信息}

通过reflect.Type对象的FieldByName()方法可以直接查找接口体中指定名称的字段。通过NumField()可以获取结构体中的字段数量，而通过Field()则可以获取对应索引的字段信息。

```go
package main
import (
    "fmt"
    "reflect"
)
func main() {
    // 声明一个空结构体
    type cat struct {
        Name string
        // 带有结构体tag的字段
        Type int `json:"type" id:"100"`
    }
    // 创建cat的实例
    ins := cat{Name: "mimi", Type: 1}
    // 获取结构体实例的反射类型对象
    typeOfCat := reflect.TypeOf(ins)
    // 遍历结构体所有成员
    for i := 0; i < typeOfCat.NumField(); i++ {
        // 获取每个成员的结构体字段类型
        fieldType := typeOfCat.Field(i)
        // 输出成员名和tag
        fmt.Printf("name: %v  tag: '%v'\n", fieldType.Name, fieldType.Tag)
    }
    // 通过字段名, 找到字段类型信息
    if catType, ok := typeOfCat.FieldByName("Type"); ok {
        // 从tag中取出需要的tag
        fmt.Println(catType.Tag.Get("json"), catType.Tag.Get("id"))
    }
}
```

```text
name: Name  tag: ''
name: Type  tag: 'json:"type" id:"100"'
type 100
```


#### 结构体标签 {#结构体标签}

你可能注意到了上面有这样的写法

```go
type cat struct {
    Type int `json:"type" id:"100"`
}
```

对于Type int 后面跟的那一串键值对，golang把它称作结构体标签。 结构体标签是对结构体字段信息的额外补充，很多ORM系统都会用到这样的标签

<!--list-separator-->

-  结构体标签的格式

    ```go
    `key1:"value1" key2:"value2"`
    ```

    结构体可以由一个或多个键值对组成；键与值之间用冒号分割，值用双引号括起来，键值对与键值对之间使用空格分割。

<!--list-separator-->

-  获取结构体标签中的值

    reflect.Type.Tag 的 Get(key string)string可以根据结构体标签中的键获取对应的值, Lookup(key string)(value string, ok bool)可以根据结构体标签中的键，查询值是否存在


## 指针与指针指向的元素 {#指针与指针指向的元素}

reflect.Elem()专用于获取指针指向的元素的类型, 因为当我们对一个指针使用reflect.TypeOf时，我们只能得到这个指针的接口值的类型信息(也就是interface值的type部分), 所以如果想要进一步获取它指向的指的类型信息，我们必须得先将指针解引用。而reflect.Elem()可以帮我们完成这一步的操作。

```go
package main
import (
    "fmt"
    "reflect"
)
func main() {
    // 声明一个空结构体
    type cat struct {
    }
    // 创建cat的实例
    ins := &cat{}
    // 获取结构体实例的反射类型对象
    typeOfCat := reflect.TypeOf(ins)
    // 显示反射类型对象的名称和种类
    fmt.Printf("name:'%v' kind:'%v'\n", typeOfCat.Name(), typeOfCat.Kind())
    // 取类型的元素
    typeOfCat = typeOfCat.Elem()
    // 显示反射类型对象的名称和种类
    fmt.Printf("element name: '%v', element kind: '%v'\n", typeOfCat.Name(), typeOfCat.Kind())
}
```

```text
name:'' kind:'ptr'
element name: 'cat', element kind: 'struct'
```


## 使用反射值对象包装任意值 {#使用反射值对象包装任意值}

反射不仅可以获取值的类型信息，还能动态获取或设置变量的值。Go语言中使用reflect.Value可以通过包装和拆包相互转化。

{{< admonition type="note" title="包装与拆包" open="true" >}}
所谓包装 就是将原值转换成reflect.Value类型的值。而拆包就是将reflect.Value类型的值转化成原值。
在包装的时候，原值被转换成reflect.Value, 而reflect.Value中封装了原值的各种信息，就像食品加工完后在外面套一层包装来告诉顾客成分信息一样，所以这一步我们通常叫做包装
拆包，就像顾客拆掉食品包装，见到真正的食物，所以由reflect.Value转化成原值这一步叫拆包。
{{< /admonition >}}

```go
value := refelct.ValueOf(rawValue)
```

reflect.ValueOf返回reflect.Value类型，包含有rawValue的值信息。


## 从reflect.Value类型的对象中获取被包装的值的方法 {#从reflect-dot-value类型的对象中获取被包装的值的方法}

| 方法名                   | 说  明                                             |
|-----------------------|--------------------------------------------------|
| Interface() interface {} | 将值以 interface{} 类型返回，可以通过类型断言转换为指定类型 |
| Int() int64              | 将值以 int 类型返回，所有有符号整型均可以此方式返回 |
| Uint() uint64            | 将值以 uint 类型返回，所有无符号整型均可以此方式返回 |
| Float() float64          | 将值以双精度（float64）类型返回，所有浮点数（float32、float64）均可以此方式返回 |
| Bool() bool              | 将值以 bool 类型返回                               |
| Bytes() []bytes          | 将值以字节数组 []bytes 类型返回                    |
| String() string          | 将值以字符串类型返回                               |

```go
package main
import (
    "fmt"
    "reflect"
)
func main() {
    // 声明整型变量a并赋初值
    var a int = 1024
    // 获取变量a的反射值对象
    valueOfA := reflect.ValueOf(a)
    // 获取interface{}类型的值, 通过类型断言转换
    var getA int = valueOfA.Interface().(int)
    // 获取64位的值, 强制类型转换为int类型
    var getA2 int = int(valueOfA.Int())
    fmt.Println(getA, getA2)
}
```

```text
1024 1024
```


## 使用IsNil() 和 IsValid() -- 判断反射值的空和有效性 {#使用isnil-和-isvalid-判断反射值的空和有效性}

反射对象(reflect.Value)提供了零值和空的判断

| 方 法          | 说 明                                                                    |
|--------------|------------------------------------------------------------------------|
| IsNil() bool   | 返回值是否为 nil。如果值类型不是通道（channel）、函数、接口、map、指针或 切片时发生 panic，类似于语言层的v== nil操作 |
| IsValid() bool | 判断值是否有效。 当值本身非法时，返回 false，例如 reflect Value不包含任何值，值为 nil 等。 |

```go
package main
import (
    "fmt"
    "reflect"
)
func main() {
    // *int的空指针
    var a *int
    fmt.Println("var a *int:", reflect.ValueOf(a).IsNil())
    // nil值
    fmt.Println("nil:", reflect.ValueOf(nil).IsValid())
    // *int类型的空指针
    fmt.Println("(*int)(nil):", reflect.ValueOf((*int)(nil)).Elem().IsValid())
    // 实例化一个结构体
    s := struct{}{}
    // 尝试从结构体中查找一个不存在的字段
    fmt.Println("不存在的结构体成员:", reflect.ValueOf(s).FieldByName("").IsValid())
    // 尝试从结构体中查找一个不存在的方法
    fmt.Println("不存在的结构体方法:", reflect.ValueOf(s).MethodByName("").IsValid())
    // 实例化一个map
    m := map[int]int{}
    // 尝试从map中查找一个不存在的键
    fmt.Println("不存在的键：", reflect.ValueOf(m).MapIndex(reflect.ValueOf(3)).IsValid())
}
```

```text
var a *int: true
nil: false
(*int)(nil): false
不存在的结构体成员: false
不存在的结构体方法: false
不存在的键： false
```


## 使用反射修改变量的值 {#使用反射修改变量的值}

使用reflect.Value对包装的值进行修改的时候，必须先遵循一些规则，否则有可能会导致程序宕机。


### 原则一：可被寻址 {#原则一-可被寻址}

先说一说什么是可被寻值。先上代码

```go
x := 2 // value type variable?
a := reflect.ValueOf(2) // 2 int 不可寻址
b := reflect.ValueOf(x) // 2 int 不可寻址
c := reflect.ValueOf(&x) // &x *int 不可寻址
d := c.Elem() // 2 int yes (x) 可被寻址
```

上面a b c 都不可被寻址。因为值在被传入reflect.ValueOf()的时候，会被自动拷贝一份出来，这时 a b c 都指向的是原值的副本。 而反射的目的是对原值描述，对原值修改。如果允许修改副本，违背了反射的初衷，所以如果之间将值传入，无法修改原值。因为a b c 这三个变量已经没有办法找到原值了，所以我们说不可被寻址。
再看d, c中存储的是x的指针，而之前说过Elem()函数可以获取到指针指向值，所以这里d描述的就是x,由于可被寻址，所以他可以被修改。

简单的说所有reflec.Value都是不可取地址的，只有当reflect.Value是指针并且调用了 Elem()的时候才能取地址，比如 reflect.ValueOf(&amp;s).Elem()

```go
package main

import (
    "fmt"
    "reflect"
)

func main() {
    x := 2

    c := reflect.ValueOf(&x)
    c.Elem().SetInt(6)
    d := c.Elem()
    d.SetInt(0)


    fmt.Println(x)
}
```

```text
0
```

使用reflect.Value取元素，取地址，判断是否可取地址可以修改的api

| 方法名         | 备  注                                                   |
|-------------|--------------------------------------------------------|
| Elem() Value   | 取值指向的元素值，类似于语言层\*操作。当值类型不是指针或接口时发生宕 机，空指针时返回 nil 的 Value |
| Addr() Value   | 对可寻址的值返回其地址，类似于语言层&amp;操作。当值不可寻址时发生宕机 |
| CanAddr() bool | 表示值是否可寻址                                         |
| CanSet() bool  | 返回值能否被修改。要求值可寻址且是导出的字段             |


### 原则二：被导出 {#原则二-被导出}

结构体成员中，如果字段没有被导出，即便不使用反射也可以被访问，但不能通过反射修改。

```go
package main
import (
    "reflect"
)
func main() {
    type dog struct {
            legCount int
    }
    // 获取dog实例的反射值对象
    valueOfDog := reflect.ValueOf(dog{})
    // 获取legCount字段的值
    vLegCount := valueOfDog.FieldByName("legCount")
    // 尝试设置legCount的值(这里会发生崩溃)
    vLegCount.SetInt(4)
}
```

:panic: reflect: reflect.Value.SetInt using value obtained using unexported field

为了能修改这个值，需要将该字段导出。将 dog 中的 legCount 的成员首字母大写，导出 LegCount 让反射可以访问，修改后的代码如下：

```go
type dog struct {
    LegCount int
}
```

然后根据字段名获取字段的值时，将字符串的字段首字母大写，修改后的代码如下：

```go
vLegCount := valueOfDog.FieldByName("LegCount")
```

再次运行程序，发现仍然报错：

```text
panic: reflect: reflect.Value.SetInt using unaddressable value
```

这个错误表示第 13 行构造的 valueOfDog 这个结构体实例不能被寻址，因此其字段也不能被修改。修改代码，取结构体的指针，再通过 reflect.Value 的 Elem() 方法取到值的反射值对象。修改后的完整代码如下：

```go
package main
import (
    "reflect"
    "fmt"
)
func main() {
    type dog struct {
            LegCount int
    }
    // 获取dog实例地址的反射值对象
    valueOfDog := reflect.ValueOf(&dog{})
    // 取出dog实例地址的元素
    valueOfDog = valueOfDog.Elem()
    // 获取legCount字段的值
    vLegCount := valueOfDog.FieldByName("LegCount")
    // 尝试设置legCount的值(这里会发生崩溃)
    vLegCount.SetInt(4)
    fmt.Println(vLegCount.Int())
}
```

```text
4
```


### 值修改相关api {#值修改相关api}

| Set(x Value)        | 将值设置为传入的反射值对象的值                               |
|---------------------|-----------------------------------------------|
| Setlnt(x int64)     | 使用 int64 设置值。当值的类型不是 int、int8、int16、 int32、int64 时会发生宕机 |
| SetUint(x uint64)   | 使用 uint64 设置值。当值的类型不是 uint、uint8、uint16、uint32、uint64 时会发生宕机 |
| SetFloat(x float64) | 使用 float64 设置值。当值的类型不是 float32、float64 时会发生宕机 |
| SetBool(x bool)     | 使用 bool 设置值。当值的类型不是 bod 时会发生宕机            |
| SetBytes(x []byte)  | 设置字节数组 []bytes值。当值的类型不是 []byte 时会发生宕机   |
| SetString(x string) | 设置字符串值。当值的类型不是 string 时会发生宕机             |

如果CanSet返回的是false, 荏苒调用上面的方法就会导致宕机


## 通过类型信息创建实例 {#通过类型信息创建实例}

当已知reflect.Type时，可以动态地创建这个类型的实例，实例的类型为指针。用例代码如下

```go
package main
import (
    "fmt"
    "reflect"
)
func main() {
    var a int
    // 取变量a的反射类型对象
    typeOfA := reflect.TypeOf(a)
    // 根据反射类型对象创建类型实例
    aIns := reflect.New(typeOfA)
    // 输出Value的类型和种类
    fmt.Println(aIns.Type(), aIns.Kind())
}
```

```text
*int ptr
```


## 通过反射调用函数 {#通过反射调用函数}

反射还能调用函数，是不是觉得挺🐮🍺的。
如果反射值对象(reflect.Value)中值的类型为函数时，可以通过reflect.Value调用该函数。使用反射调用函数，需要将参数用[]reflect.Value构造后传入Call(). 调用完成函数的返回值会通过[]reflect.Value返回。

```go
package main

import (
    "fmt"
    "reflect"
)

func add (a,b int) int {
    return a + b
}

func main(){
    funcValue := reflect.ValueOf(add)

    paramList := []reflect.Value{reflect.ValueOf(10),reflect.ValueOf(20)}

    retList := funcValue.Call(paramList)

    fmt.Println(retList[0].Int())
}
```

```text
30
```

反射调用函数的过程需要构造大量的 reflect.Value 和中间变量，对函数参数值进行逐一检查，还需要将调用参数复制到调用函数的参数内存中。调用完毕后，还需要将返回值转换为 reflect.Value，用户还需要从中取出调用值。因此，反射调用函数的性能问题尤为突出，不建议大量使用反射函数调用。

