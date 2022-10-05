# golang plugin


<!--more-->

golang 的plugin特性 是已经出了好几年了。不过直到我写这篇博客的时候，golang的plugin 也仅支持linux/unix平台。而且plugin的使用有诸多限制，让它成为了一个冷门特性。


## golang plugin {#golang-plugin}

golang的plugin为golang 提供了类似动态链接库的能力。通过plugin，我们可以将Go包编译成共享库，这样就可以动态加载模块。
但是他有一些限制

1.  插件的实现和主应用程序必须用完全相同的Go工具链版本构建。这种特性一般只会用于类似算法模块的更新，因为算法模块是纯函数。程序分为数据和算法，数据结构是一定不能动的，因为静态语言会有类型检查，这会导致很多的问题。作为数据结构不单会被主程序引用，plugin也得引用。两者版本对不上，会是非常头疼的问题。
2.  不同so文件定义的结构体不能使用类型断言进行转换
3.  每个so不能单独保存数据，so是没法被关闭的。可能多个so引用同一个变量，gc没有办法释放。
4.  使用plugin的主程序只能够使用动态链接，go以静态编译著称，编译的程序不需要别的依赖就可以跑起来。但是使用了plugin的主程序就需要依赖系统的动态链接库了。

plugin的使用很简单，分为三个步骤。

1.  先编写plugin程序，
2.  然后使用-buildmode=plugin 编译出.so文件
3.  编写主程序 使用plugin包的api去调用plugin中的函数

举个例子
先看项目结构

{{< figure src="/ox-hugo/2022-07-05_19-29-34_screenshot.png" >}}

plugin目录下的plug.go是plug.so的源码文件， 主程序是test目录下的test.go

1.  编写plugin程序 plug.go
    ```go
    package main

    import (
        "fmt"
    )

    func PrintHahaha() {
        fmt.Println("Hahaha")
    }
    func CallInDirect() {
        i:= 1
        i++
    }
    ```

{{< admonition type="note" title="note" open="true" >}}
必须要注意，plugin程序的包定义必须是package main 可以没有main函数。同一个plugin程序的init函数只会被调用一次，重复加载后也不会再被调用(就是说 plugin在第一次被打开时，会调用包里面的init函数，但是后面再次打开init是不会再被调用的)
{{< /admonition >}}

1.  使用-buildmode=plugin参数编译
    ```sh
    cd plugin
    go build -buildmode=plugin -o plug.so ./plug.go
    ```
2.  编写调用的主程序test.go
    ```go
    package main

    import (
        "fmt"
        "plugin"
    )

    func main() {
        plug, err := plugin.Open("../plugin/plug.so")

        if err != nil {
            fmt.Println(err)
        }

        foo, err := plug.Lookup("PrintHahaha")

        if err != nil {
            fmt.Println(err)
        }

        PrintHahaha, ok := foo.(func())

        if !ok {
            fmt.Println("unexpected type from module symbol")
        }

        PrintHahaha()
    }
    ```

这里先介绍一下基本使用流程，方便下面的性能测试


## 性能测试 {#性能测试}

先说结论，plugin的函数调用要比正常的直接函数调用慢很多，大概慢3倍左右。但是你大可不必感到失望，因为实际使用中并不一定就会有很大的性能差异。
这里会分为两个部分，一个是调用plugin中函数的调用速度与直接调用函数速度的对比，另一个则是测试plugin LookUp的性能


### directCall vs indirectCall {#directcall-vs-indirectcall}

先在宿主程序中添加一个CallDirect函数作为直接调用的素材

```go
func CallDirect() {
    // for i := 0; i < 1000; i++ {

    // }
    i := 10
    i++
}
```

再在plugin中添加CallInDirect函数作为调用plugin中函数的素材

```go
func CallInDirect() {
    // for i := 0; i < 1000; i++ {

    // }
    i := 10
    i++
}
```

接下来编写benchmark

```go

func BenchmarkCallDirect(b *testing.B) {
    for i := 0; i < b.N; i++ {
        CallDirect()
    }
}

func BenchmarkCallInDirect(b *testing.B) {
    plug, err := plugin.Open("../plugin/plug.so")

    if err != nil {
        fmt.Println(err)
    }

    foo, err := plug.Lookup("CallInDirect")

    if err != nil {
        fmt.Println(err)
    }

    CallInDirect, ok := foo.(func())

    if !ok {
        return
    }

    for i := 0; i < b.N; i++ {
        CallInDirect()
    }
}
```

下面是跑分结果

-   BenchmarkCallDirect
    ```sh
    ~/test/testPlugin/test
    ❯ go test -bench="CallDirect$" -benchtime=5s -count=3 .
    goos: darwin
    goarch: arm64
    pkg: testplugin
    BenchmarkCallDirect-8   	1000000000	         0.3317 ns/op
    BenchmarkCallDirect-8   	1000000000	         0.3271 ns/op
    BenchmarkCallDirect-8   	1000000000	         0.3228 ns/op
    PASS
    ok  	testplugin	1.346s
    ```
-   BenchmarkCallInDirect
    ```sh
    ~/test/testPlugin/test
    ❯ go test -bench="CallInDirect$" -benchtime=5s -count=3 .
    goos: darwin
    goarch: arm64
    pkg: testplugin
    BenchmarkCallInDirect-8   	1000000000	         0.9653 ns/op
    BenchmarkCallInDirect-8   	1000000000	         0.9513 ns/op
    BenchmarkCallInDirect-8   	1000000000	         0.9542 ns/op
    PASS
    ok  	testplugin	3.801s
    ```

差不多是三倍的差距，但是这里，测试素材比较简单。如果让测试素材里面跑一些比较耗时的逻辑呢。

-   CallDirect
    ```go
    func CallDirect() {
        for i := 0; i < 1000; i++ {

        }
        // i := 10
        // i++
    }
    ```
-   CallInDirect
    ```go
    func CallInDirect() {
        for i := 0; i < 1000; i++ {

        }
        // i := 10
        // i++
    }
    ```

跑分结果

-   BenchmarkCallDirect
    ```sh
    ~/test/testPlugin/test 13s
    ❯ go test -bench="CallDirect$" -benchtime=5s -count=3 .
    goos: darwin
    goarch: arm64
    pkg: testplugin
    BenchmarkCallDirect-8   	16496887	       327.1 ns/op
    BenchmarkCallDirect-8   	17459262	       328.3 ns/op
    BenchmarkCallDirect-8   	18355296	       332.5 ns/op
    PASS
    ok  	testplugin	21.329s
    ```
-   BenchmarkCallInDirect
    ```sh
    ❯ go test -bench="CallInDirect$" -benchtime=5s -count=3 .
    goos: darwin
    goarch: arm64
    pkg: testplugin
    BenchmarkCallInDirect-8   	16783506	       331.3 ns/op
    BenchmarkCallInDirect-8   	18161929	       333.9 ns/op
    BenchmarkCallInDirect-8   	18247104	       328.9 ns/op
    PASS
    ok  	testplugin	18.826s
    ```

{{< admonition type="note" title="summary" open="true" >}}
差距是无限缩小的，大概能够得出的结论是。如果你不是不要命的频繁的去调用函数，那么plugin调用带来的消耗几乎可以忽略不计，尤其是当你的函数执行越耗时，这种差异就会越小。
{{< /admonition >}}


### LookUp的性能 {#lookup的性能}

跑分代码

```go
func BenchmarkLookUp(b *testing.B) {
    plug, err := plugin.Open("../plugin/plug.so")

    if err != nil {
        fmt.Println(err)
    }

    for i := 0; i < b.N; i++ {
        plug.Lookup("CallInDirect")
    }
}
```

跑分结果

```sh
~/test/testPlugin/test 20s
❯ go test -bench="LookUp$" -benchtime=5s -count=3 .
goos: darwin
goarch: arm64
pkg: testplugin
BenchmarkLookUp-8   	1000000000	         4.785 ns/op
BenchmarkLookUp-8   	1000000000	         4.791 ns/op
BenchmarkLookUp-8   	1000000000	         4.787 ns/op
PASS
ok  	testplugin	16.576s

```

性能也还不错哦


## 结论 {#结论}

正常使用过程中你并不需要特别关心plugin所带来的消耗，因为相比函数内部逻辑带来的消耗，plugin 所带来的消耗实在是太小了。在了解plugin特性的时候，我还了解到golang 有一个 [go-plugin](https://github.com/hashicorp/go-plugin) 的包，使用grpc来实现组件的拔插，截止到现在已经有3.7k的收藏。所以性能并不由调用函数的速度决定，性能取决于函数内部的逻辑是如何实现的。

