# shell脚本: 文本文件
** 在shell脚本的开头要指明调用的解释器**
* bash: #!/bin/bash
* python: #!/usr/bin/python
* perl: !/usr/bin/perl

# 运行脚本的方式
* 1、给予执行权限，通过具体文件的路径指定文件执行
        ./scriptname
* 2、直接运行解释器，将脚本作为解释器程序的参数运行
        bash scriptname

# 变量
** shell脚本是弱类型的语言 将所有存储的数据统统当作字符执行 不支持浮点数 **

# bash中的变量的种类
* 本地变量: 生效范围为当前shell进程，对当前shell之外的其他shell进程，包括当前shell的子进程均无效
* 环境变量: 生效范围为当前shell进程及其子进程
* 局部变量: 生效范围为当前shell进程中某代码片段(通常指函数)
* 位置变量: \$1,\$2.....来表示用于让脚本在代码中调用通过命令行传递给它的参数
* 特殊变量: \$?,\$0,\$*,\$@,\$#

# 变量引用:
* \${name},\$name
    * "": 弱引用，其中的变量引用会被替换为变量值
    * '': 强引用，其中的变量引用不会被替换为变量值, 而保持原字符串

# 变量赋值: 
* name='value'
* 使用引用
    * 变量引用: name="$username"
    * 命令引用: name=`COMMAND`, name=$(COMMAND)

# 显示和销毁变量
* 显示已定义的所有变量
    * set 
* 销毁变量
    * unset name 

# 环境变量
** 变量声明 **
* export name=value
* declare -x name=value

** 变量引用 **
* \$name
* \${name}

** 显示所有环境变量 **
* export 
* env
* printenv

** 销毁 **
* unset name

** bash中的内建环境变量 **
* PATH,SHELL,UID,HISTSIZE,HOME,PWD,OLD,HISTFILE,PS1

# 只读变量:
** 声明 ** 
* readonly name=value
* declare -r name=value

# 位置变量：

* $1,$2....分别对应调用第1、第2等参数
* 轮岗: shift [n]
* $0: 命令本身
* $*: 传递给脚本的所有参数(一个字符串)
* $@: 传递给脚本的所有参数(-个参数一个字符串)
* $#: 参数个数

# bash中的算术运算符
** +, -, \*, /, %, \*\*(次方) **
** 实现算术运算 **
* (1) let var=arithmetic expression
* (2) var=$[arithmetic expression]
* (3) var=$((arithmetic expression))
* (4) var=$(expr arg1 arg2 arg3....)  
> 乘法符号在有写场景需要转义  
> bash中的随即书生成器: $RANDOM

## 条件测试:
** 判断某需求是否满足, 需要由测试机制来实现 **
> Note: 专用的测试表达式需要由测试命令辅助完成测试过程

** 测试命令 **
* test EXPRESSION
* [ EXPRESSION ]
* [[ EXPRESSION ]]  
> Note: EXPRESSION前后必须有空白字符

** bash的测试类型 **
* 数值测试
    * -gt: 是否大于
    * -ge: 是否大于等于
    * -eq: 是否等于
    * -ne: 是否不等于
    * -lt: 是否小于
    * -le: 是否小于等于

* 字符串测试
    * ==: 是否等于
    * >: 是否大于
    * <: 是否小于
    * !=: 是否不等于
    * =~: 左侧字符串是否能够被右侧的PATTERN所匹配  
        > Note: 此表达式一般作用于[[  ]]中
    * -z "STRING": 测试字符传是否为空，空则为真，不空则为假
    * -n "STRING": 测试字符是否不空，不空则为真，空则为假  
    > Note: 用于字符串比较时的操作数都应该使用引号
* 文件测试

# bash中自定义退出状态码
** exit [n] **

# 选择执行
```
    if 判断条件; 
    then
        条件为真的分支代码
    fi

    if 判断条件; 
    then 
        条件为真的分支代码
    else 
        条件为假的分支代码
    fi
```

# 用户交互
## read
* **Description**
    * 从标准输入中读取数据
* **Synopsis**
        read [option]...[name...]
* **options**
    * -p 'PROMPT'
    * -t 'TIMEOUT'

# bash脚本的调试和检测
* bash -n /path/to/some-script: 检测语法错误
* bash -x /path/to/some-script: 调试
