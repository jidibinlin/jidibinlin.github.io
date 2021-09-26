# grep: 
**description**    
* Global search Regular expression and Print out the line.

**function** 
* 文本搜索工具，根据用户指定的"模式"对目标文本进行匹配检查；打印匹配到的行;
**parttern** 
*   由正则表达式字符及文本字符所编写的过滤条件
    * REGXP: 由一类特殊字符及文本所编写的某事，其中有些字符不表达字符字面意义，而表示控制或通配功能
        * 基本正则表达式: BRE(grep 默认支持)
        * 扩展正则表达式: ERE (grep -E,egrep)
    * 正则表达式引擎:

**synopsis**  

        grep [OPTION...] PATTERNS [FILE...]
        grep [OPTION...] -e PATTERNS ... [FILE...]
        grep [OPTION...] -f PATTERN_FILE ... [FILE...]

**options**

* --color=auto: 对匹配到的文本着色显示
* -v: 显示没有被pattern匹配到的行
* -i: 匹配时忽略大小写
* -o: 仅显示匹配到的字符串
* -q: 不输出任何信息
* -A #: after, 后#行
* -B #: befire, 前#行
* -C #: context, 前后各#行
* -E: 使用ERE

# 基本正则表达式原字符
**字符匹配**
*   .: 匹配任意单个字符
*   [0-9...]: 匹配指定范围内的任意单个字符
*   [^]: 匹配指定范围外的任意单个字符
*   [:digit:] 任意数字,相当于0-9(实际用的时候要[[:digit:]]这样用)
*   [:lower:] 任意小写字符
*   [:upper:] 任意大写字符
*   [:alpha:] 任意大小写字符
*   [:alnum:] 任意数字或字母
*   [:space:] 任意空格字符
*   [:punct:] 标点符号
*   [:alnum:]  [:alpha:]  [:blank:]  [:cntrl:]
*   [:digit:]  [:graph:]  [:lower:]  [:print:]
*   [:punct:]  [:space:]  [:upper:]  [:xdigit:]

**匹配次数**: 用在要指定次数的字符后面，用于指定前面的字符要出现的次数
* *:匹配前面的字符任意次
    * 例如: grep "x*y"能匹配到"abxy","xay","ay","xxxxxxy" 
* . *: 任意长度的任意字符
* \?: 匹配其前面的字符0次或1次,即可有可无
* \+: 匹配其前面的字符至少一次
* \{m\}: 匹配前面的字符m次
* \{m,n\}: 匹配前面的字符至少m次，至多n次
    * \{0,n\}: 匹配前面的字符至多n次
    * \{m,\}: 匹配前面的字符至少m次

**位置锚定**
* ^: 行首锚定: 用于模式最左侧
* $: 行尾锚定: 用于模式最右侧
* ^PATTERN$: 用于模式匹配整行
    * ^$: 空行
    * ^[[:space;]]*$
* \< 或 \b: 词首锚定: 用于单词模式的左侧
* \> 或 \b: 词尾锚定: 用于单词模式的右侧
* \<PATTERN\>: 匹配整个单词

**分组** 
* \(\): 将一个或多个字符捆绑在一起，当作一个整体处理 例如 \(xy\)*ab
    > Note 分组括号中的模式匹配到的内容会被正则表达式引擎记录于内部的变量中，这些变量的命名方式为: \1,\2,\3,.....
* 后向引用: 引用前面的分组括号中的模式所匹配的字符串，(而非模式本身)

**练习**
* 显示/proc/meminfo文件中以大小s开头的行(要求: 使用两种方式)
        grep -i '^s' /proc/meminfo
        grep '^[Ss]' /proc/meminfo
* 显示/etc/passwd文件中ID号最大的用户的用户名
        sort -t : -k 3 /etc/passwd | tail -1 | cut -d : -f 1 
* 显示/etc/passwd文件中不以/bin/bash结尾的行
        grep -v "/bin/bash$" /etc/passwd
* 如果root用户存在, 显示其默认的shell程序
        grep '^root\>' /etc/passwd &>/dev/null && grep '^root\>' /etc/passwd | cut -d : -f 7
        id root &> /dev/null && grep '^root\>' /etc/passwd | cut -d : -f 7
* 找出/etc/passwd中的两位或三位数
        grep -o '[[:digit:]]\{2,3\}\>' /etc/passwd
* 显示/etc/rc.d/rc.sysinit文件中, 至少以一个空白字符开头且后面存在非空白字符的行
        grep '^[[:space:]]\+^[["space"]]\+' /etc/grub2.cfg
* 找出"netstat -tan" 命令的结果中以'LISTEN'后跟0、1或多个空白字符结尾的行
        netstat -tan | grep '\<LISTEN[[:space:]]$*'
* 添加用户bash、testbash、basher、以及nologin(其shell为/sbin/nologin): 而后找出/etc/passwd文件中用户名同shell名的行
        grep '\(^[[:alnum:]]*\>\).*\1$' /etc/passwd
        
# egrep
**egrep = grep -E**

**synopsis**

    egrep [OPTIONS] PATTERN [FILE...]

# PATTERNB扩展正则表达式
**字符匹配** 
* .
* [fdsafd]
* [^]

**次数匹配**
* *
* ?
* +
* {m}
* {m,n}

**锚定**
* ^
* $
* \<,\b
* \>,\b

**分组**
* ()
> 后向引用: \1,\2

**或者**
* a|b

**练习**
* 显示/proc/meminfo文件中以大小s开头的行(要求: 使用两种方式)
        egrep -i '^s' /proc/meminfo
        egrep '^[Ss]' /proc/meminfo
* 显示/etc/passwd文件中ID号最大的用户的用户名
        sort -t : -k 3 /etc/passwd | tail -1 | cut -d : -f 1 
* 显示/etc/passwd文件中不以/bin/bash结尾的行
        egrep -v "/bin/bash$" /etc/passwd
* 如果root用户存在, 显示其默认的shell程序
        egrep '^root\>' /etc/passwd &>/dev/null && egrep '^root\>' /etc/passwd | cut -d : -f 7
        id root &> /dev/null && egrep '^root\>' /etc/passwd | cut -d : -f 7
* 找出/etc/passwd中的两位或三位数
        egrep -o '[[:digit:]]{2,3}\>' /etc/passwd
* 显示/etc/grub2.cfg文件中, 至少以一个空白字符开头且后面存在非空白字符的行
        egrep '^[[:space:]]+^[["space"]]+' /etc/grub2.cfg
* 找出"netstat -tan" 命令的结果中以'LISTEN'后跟0、1或多个空白字符结尾的行
        netstat -tan | egrep '\<LISTEN[[:space:]]$*'
* 添加用户bash、testbash、basher、以及nologin(其shell为/sbin/nologin): 而后找出/etc/passwd文件中用户名同shell名的行
        egrep '(^[[:alnum:]]*\>).*\1$' /etc/passwd
* 显示当前系统root、centos或user1用户的默认shell和UID
        egrep '^(root|centos|user1)\>' /etc/passwd | cut -d : -f 3,7
* 找出/etc/rc.d/init.d/functions文件(centos6)中某单词后面跟一个小括号的行
        egrep -o '[_[:alpha:]]+\(\)' /etc/rc.d/init.d/functions
* 找出ifconfig命令结果中1-255之间的数值
        ifconfig | egrep -o '\<([1-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\>'
* 找出ifconfig命令结果中的ip地址
        ifconfig | egrep -o '(\<([0-9]|[0-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\>\.){3}\<([0-9]|1[0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\>'

# fgrep: 不是用引擎 直接匹配字符串

