# golang functional-options


<!--more-->


## Functional Options {#functional-options}

go语言的函数没有重载以及默认参数的功能，这个时候生成一个i对象会变得极其麻烦

```go
type Person struct{
    Name string
    Age  int
    Country string
    Gender  string
    Height  string
    Address string
}
func main(){
    person :=Person{
        Name:"张三",
        Age: -1，
        Country: "China",
        Gender: "Male",
        Height: "-1",
        Address: "unknown",
    }
}
```

我们可以使用函数式选项来解决这一问题。

```go
type Person struct {
    Name    string
    Age     int
    Country string
    Gender  string
    Height  string
    Address string
}

// 将func(*Person)这种类型的函数简化命名
type per func(*Person)

func Country(country string) per {
    return func(person *Person) {
        person.Country = country
    }
}

func Gender(gender string) per{
    return func(person *Person){
        person.Gender = gender
    }
}

func Gender(gender string) per{
    return func(person *Person){
        person.Gender = gender
    }
}

func Address(address string) per{
    return func(person *Person){
        person.Address = address
    }
}

// NewPerson ...
func NewPerson(name string,ops ...per) *Person {
    person := &Person{
        Name: name,
        Age: -1,
        Country: "China",
        Gender: "Male",
        Height: 0,
        Address: "unknown",
    }

    for _,op:= range ops {
        op(person)
    }
    return person
}
```


### 用法： {#用法}

```go
package main

import (
    "fmt"
    op "studygo/pattern/functionOptions"
)

// main ...
func main() {
    person1 := op.NewPerson("zhangsan")
    fmt.Println(person1)
    person2 := op.NewPerson("Marry", op.Gender("Female"), op.Country("Japan"))
    fmt.Println(person2)
}
```

