# bash的配置文件
### 按生效范围划分，存在两类
** 1、全局配置**
* /etc/profile
    * /etc/profile.d/*.sh
* /etc/bashrc

** 2、个人配置**
* ~/.bash_profile
* ~/.bashrc

### 按功能划分, 存在两类
**1、profile类: 为交互式登录的shell提供配置**
**2、bashrc类: 为非交互式登录的shell提供配置**

# shell登录:
**交互式登录**
* 直接通过终端输入账号密码登录
* 使用"su - username" 或 "su -l username" 切换的用户
* 配置文件加载顺序: /etc/profile --> /etc/profile.d/*.sh --> ~/.bash_profile --> ~/.bashrc --> /etc/bashrc

**非交互式登录**
* su username
* 图形界面下打开的终端
* 执行脚本
* 配置文件加载顺序:~/.bzshrc --> /etc/bashrc --> /etc/profile.d/*.sh

# bash中的算术运算符
**+, -, \*, /, %, \*\*(次方)**
**实现算术运算**
* (1) let var=arithmetic expression
* (2) var=$[arithmetic expression]
* (3) var=$((arithmetic expression))
* (4) var=$(expr arg1 arg2 arg3....)  
> 乘法符号在有写场景需要转义  
> bash中的随即书生成器: $RANDOM

## 条件测试:
**判断某需求是否满足, 需要由测试机制来实现**
> Note: 专用的测试表达式需要由测试命令辅助完成测试过程

**测试命令**
* test EXPRESSION
* [ EXPRESSION ]
* [[ EXPRESSION ]]  
> Note: EXPRESSION前后必须有空白字符

**bash的测试类型**
* **数值测试**
    * -gt: 是否大于
    * -ge: 是否大于等于
    * -eq: 是否等于
    * -ne: 是否不等于
    * -lt: 是否小于
    * -le: 是否小于等于

* **字符串测试**
    * ==: 是否等于
    * >: 是否大于
    * <: 是否小于
    * !=: 是否不等于
    * =~: 左侧字符串是否能够被右侧的PATTERN所匹配  
        > Note: 此表达式一般作用于[[  ]]中
    * -z "STRING": 测试字符传是否为空，空则为真，不空则为假
    * -n "STRING": 测试字符是否不空，不空则为真，空则为假  
    > Note: 用于字符串比较时的操作数都应该使用引号
* **文件测试**
    * 存在性测试
        * -a File: 文件存在则为真
        * -e File: 文件存在则为真
    * 存在性及类别测试
        * -b File: 是否存在且为块设备文件
        * -c File: 是否存在且为字符设备文件
        * -d File: 是否存在且为目录文件
        * -f File: 是否存在且为普通文件
        * -h File 或 -L File: 存在且为符号链接文件
        * -p File: 是否存在且为命名管道文件
        * -S File: 是否存在且为套接字文件
    * 文件特殊权限测试
        * -r File: 是否存在且可读
        * -w File: 是否存在且可写
        * -x File: 是否存在且可执行
    * 文件大小测试
        * -s File: 是否存在且非空
    * 文件是否打开
        * -t fd: fd表示文件描述符 是否已经打开且与某终端相关
        * -N File: 文件子上一次被读取之后是否被修改过
        * -O File: 当前有效用户是否为文件属性
        * -G File: 当前有效用户是否为文件属组
    * 双目测试:
        * FILE1 -ef FILE2: FILE1与FILE2是否指向同一个设备上的相同inode
        * FILE1 -nt FILE2: FILE1是否新于FILE2
        * FILE1 -ot FILE2: FILE1是否旧于FILE2

### 组合测试条件
    逻辑运算: 
* **第一种方式**
    * COMMAND1 && COMMAND2
    * COMMAND1 || COMMAND2
    * !COMMAND  
```
    [-e FILE] && [-r FILE]
```
* **第二种方式**
    * EXPRESSION1 -a EXPRESSION2
    * EXPRESSION1 -o EXPRESSION2
    * !EXPRESSION
```
[-z "$hostName" -o "$hostName"=="localhost.localdomain"]
```


# bash中自定义退出状态码
**exit [n]**
