# GoGameActor-103 对比golang 的两种actor实现———protoactor-go 和 gonet


<!--more-->


## introduction {#introduction}

actor 模型的介绍我就不赘述了可以去看我之前的博客[GoGameActor-101 actor intruduction](https://jidibinlin.github.io/actorintroduction/)这里说一下我最近在看的两个go语言的实现 protoactor-go 和 gonet


### protoactor-go {#protoactor-go}

[protoactor-go](https://github.com/asynkron/protoactor-go) 由瑞士团队Asynkron出品，虽然api还不稳定，但是已经有团队在生产环境使用了。看protoactor-go给我的感觉是，代码解耦非常彻底，所以略有些吃力。因为还没完全理清抽象的概念，所以经常被interface{}搞得晕头转向。

而且protoactor-go有许多非常有意思的特性比如deadletter,persistence,behaviour,middleware,futures,等等等等。由于特性很多，所以实现也比较复杂。protoactor-go只是Asynckron的protoactor的go语言版本，实际上还有c#版本。

没在大型团队待过，所以不知道有这些特性是为什么，虽然花时间把[文档](https://proto.actor/docs/)看了个七七八八。但是奈何没有经验，作为nerd实在没发评价。不过我感觉挺diao的, 专业团队设计的东西就是要设计成我不太能看懂的样子😄。但是protoactor-go的文档非常全面专业，真正使用的时候即使照着c#版本的文档，也不应该有无法理解的地方。


### gonet {#gonet}

相比之下，[gonet](https://github.com/bobohume/gonet)我就比较能看懂了。但是特性也比较简单，是为mmo游戏设计的一个框架。这里我只讨论他的actor实现部分。不过gonet的文档并不全面，能拿来说的地方并不多。


## 有关actor模型实现 {#有关actor模型实现}

protoactor-go 和 gonet关于actor模型的实现走的是两种不同的路线。gonet需要actor自己实现receive方法，以及和actor方法相对应的参数类型.actor的receive方法根据参数类型调用不同的函数。

```go
type HelloActor struct{} //先定义具体actor
type Hello struct{ Who string } //再定义消息

func (state *HelloActor) Receive(context actor.Context) { //receive函数负责消息分发和调用
    switch msg := context.Message().(type) {
    case Hello:
        fmt.Printf("Hello %v\n", msg.Who) //这里没有调用HelloActor自己的函数，而是做一个打印
    }
}

func main() {
    context := actor.EmptyRootContext
    props := actor.PropsFromProducer(func() actor.Actor { return &HelloActor{} })
    pid, err := context.Spawn(props) //到这你可以理解为都是为了将一个actor注册到系统中
    if err != nil {
        panic(err)
    }
    context.Send(pid, Hello{Who: "Roger"}) //向这个actor发送消息
    console.ReadLine()
}
```

而gonet就比较简单了,由于gonet内部使用反射实现，他和lua+c 实现的skynet是非常相似的。你不需要实现消息分发，gonet会自动帮你解决。但是缺点是，过度依赖golang 反射的函数调用，性能会差很多，不如去用动态语言（对比对象是common lisp）这里比较复杂，我贴一下有关反射函数调用那边的代码，你们自己看吧。

```go
func (a *Actor) call(io CallIO) {
    rpcPacket := io.RpcPacket
    head := io.RpcHead
    funcName := rpcPacket.FuncName
    if !a.HasRpc(funcName) {
        log.Printf("func [%s] has no method", funcName)
        return
    }
    m, _ := a.rType.MethodByName(funcName)
    rpcPacket.RpcHead.SocketId = io.SocketId
    params := rpc.UnmarshalBody(rpcPacket, m.Type)
    if len(params) >= 1 {
        in := make([]reflect.Value, len(params))
        in[0] = a.rVal
        for i, param := range params {
            if i == 0 {
                continue
            }
            in[i] = reflect.ValueOf(param) //收集需要调用的函数的参数
        }

        a.Trace(funcName)
        ret := m.Func.Call(in) //反射函数调用 并收集返回值
        a.Trace("")
        if ret != nil && head.Reply != "" {
            ret = append([]reflect.Value{reflect.ValueOf(&head)}, ret...)
            rpc.MGR.Call(ret)
        }
    } else {
        log.Printf("func [%s] params at least one context", funcName)
        //f.Call([]reflect.Value{reflect.ValueOf(ctx)})
    }
}
```


## 性能 {#性能}

这里我不直接使用两个框架，但是我用两者实现actor的基本方法模拟下测试,再和golang直接调用以及common lisp直接调用add函数做对比，因为我不是特别关心这两个框架的使用，或者实际使用的时候性能的好坏（我想写一个自己的嘛😁）所以简化一下哈哈哈。

```go
func add(a, b int) int {
    return a + b
}
```

我会分别用两种方式去调用这个add方法，以期对比他们的性能。


### protoactor的实现思路的性能测试 {#protoactor的实现思路的性能测试}

测试代码

```go
type HelloActor struct {
}

type CallAdd struct {
    word string
}

// Receive ...
func (this *HelloActor) Receive(param interface{}) {
    switch param.(type) {
    case CallAdd:
        add(0, 1000)
    }
}

func BenchmarkActor(b *testing.B) {
    actor := HelloActor{}
    for i := 0; i < b.N; i++ {
        actor.Receive(CallAdd{word: "hello"})
    }
}
```

结果

```go
❯ go test -bench="Actor" .
goos: darwin
goarch: arm64
pkg: test
BenchmarkActor-8   	1000000000	         0.3178 ns/op
PASS
ok  	test	2.150s
```

这个速度很快了哦，接近直接调用的性能了


### gonent基于反射实现思路的性能 {#gonent基于反射实现思路的性能}

测试代码

```go
func BenchmarkReflect(b *testing.B) {

    funcValue := reflect.ValueOf(add)
    paramList := []reflect.Value{reflect.ValueOf(0), reflect.ValueOf(1000)}
    for i := 0; i < b.N; i++ {

        funcValue.Call(paramList)
    }
}
```

结果

```sh
❯ go test -bench="Reflect" .
goos: darwin
goarch: arm64
pkg: test
BenchmarkReflect-8   	 5031619	       204.8 ns/op
PASS
ok  	test	2.458s
```

这个性能和上面的对比下，属实太拉胯的哈哈哈


### 直接调用add函数 {#直接调用add函数}

测试代码

```go
func BenchmarkDirect(b *testing.B) {
    for i := 0; i < b.N; i++ {
        add(0, 1000)
    }
}
```

结果

```sh
~/test/center
❯ go test -bench="Direct" .
goos: darwin
goarch: arm64
pkg: test
BenchmarkDirect-8   	1000000000	         0.3184 ns/op
PASS
ok  	test	1.836s
```


### common lisp 直接调用add函数 {#common-lisp-直接调用add函数}

测试代码(使用sbcl 解释器)

```lisp
(defun add (a b)
  (+ a b))

(defun test1000000000()
  (loop for i from 0 to 1000000000
        do (add 1 i)))

(time (test1000000000))
```

```text
Evaluation took:
  2.534 seconds of real time
  2.534737 seconds of total run time (2.532272 user, 0.002465 system)
  100.04% CPU
  0 bytes consed
```

性能也不错，虽然比不上静态语言的golang但是这个成绩已经很能打了，即使和静态语言对比也有一拼了。动态语言里面，怕是难逢敌手了。


## 总结 {#总结}

gonet胜过protoactor-go的地方可能就是他写起来会容易很多，但是性能和特性上，与protoactor比只能说是贫瘠了。即使和动态语言common lisp比，也属实拉胯了，当然common lisp是能和c++这种性能级语言battle一下的，性能上属于第一梯队的。

gonent会慢的主要原因是golang的反射函数调用拖了后腿。而protoactor的实现方式，让它几乎没有性能上的损失。当然写法上，会有些一点怪异吧。函数调用比较密集的话，并不推荐使用反射函数调用。

