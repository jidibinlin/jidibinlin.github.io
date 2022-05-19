# Common Lisp Files and Directories


<!--more-->

本节主要介绍UIOP中的一些有关文件和文件操作方面的使用。获取你也应该直接看一看UIOP的官方文档.

-   [UIOP](https://asdf.common-lisp.dev/uiop.html#Top)

当然，也别忘了

-   [Files and File I/O in Practicial Common Lisp](http://gigamonkeys.com/book/files-and-file-io.html)


## 测试文件是否存在 {#测试文件是否存在}

probe-file 专门用来测试文件是否存在。如果返回Nil文件不存在，返回truename则文件存在

{{< admonition type="tip" title="truename" open="true" >}}
Many file systems permit more than one filename to designate a particular file.

Even where multiple names are possible, most file systems have a convention for generating a canonical filename in such situations. Such a canonical filename (or the pathname representing such a filename) is called a truename.
{{< /admonition >}}

为了可移植性，可以使用返回pathname的uiop:probe-file\* 或者uiop:file-exists-p（如果文件存在的话）

```lisp
$ ln -s /etc/passwd foo

(probe-file "/etc/passwd")
#p"/etc/passwd"

(probe-file "foo")
#p"/etc/passwd"

(probe-file "bar")
NIL
```


## 展开~（家目录环境变量） {#展开-家目录环境变量}

我们使用uiop:native-namestring:

```lisp
(uiop:native-namestring "~/.emacs.d/")
```

如果文件不存在，他也会被展开

```lisp
(uiop:native-namestring "~/foo987.txt")
;; "/home/me/foo987.txt"
```

在许多lisp实现上(CCL,ABCL,ECL,CLISP,LispWorks),namestring 很像。在 SBCL上，如果文件不存在，或者目录不存在，namestring不会展开path 而是直接返回参数

要测试文件是否存在也可以使用truename,但是在SBCL上，如果文件不存在，会直接返回错误。


## 创建文件夹 {#创建文件夹}

```lisp
(ensure-directories-exist "fbbboo/bar/baz/")
```

这样会创建fbbboo bar baz 不要忘记在尾部加上斜杠


## 删除文件夹 {#删除文件夹}

uiop:delete-directory-tree 和一个pathname(#p),一个尾部斜杠 和:validate

```lisp
(uiop:delete-directory-tree #p"dirtest/" :validate t)
```

也可以使用pathname 函数创建一个pathname

```lisp
(defun rmdir (path)
  (uiop:delete-directory-tree (pathname path) :validate t))
```

uiop 还有一个delete-empty-directory 用来删除空的文件夹


## 合并文件和文件夹 {#合并文件和文件夹}

merge-pathnames 专用来合并路径，如果你想在后面添加一个文件夹，第二个参数必须带上末尾斜杠

```lisp
(merge-pathnames "otherpath" "/home/vince/projects/")
;; => #p"/home/vince/projects/otherpath" 这里otherpath被当做文件
```

```lisp
(merge-pathnames "otherpath" "/home/vince/projects")
;; #P"/home/vince/otherpath"
;;               ^^ no "projects", because it was seen as a file.
```

```lisp
(merge-pathnames "otherpath/" "/home/vince/projects")
;; #P"/home/vince/otherpath/projects"
;;                ^^ inserted here
```


## 获取当前目录(CWD) {#获取当前目录--cwd}

使用uiop/os:getcwd:

```lisp
(uiop/os:getcwd)
;; #P"/home/vince/projects/cl-cookbook/"
;;                                    ^ with a trailing slash, useful for merge-pathnames
```


## 获取以lisp工程为根目录的相对路径的绝对路径 {#获取以lisp工程为根目录的相对路径的绝对路径}

使用asdf:system-relative-pathname system path.

```lisp
(asdf:system-relative-pathname "mysystem" "src/web")
;; => #P"/home/vince/projects/mysystem/src/web/"
```


## opening a file {#opening-a-file}

打开文件其实之前有写过，这里不赘述。直接给出cookbook 的连接（主要，我自己已经看过了,不想多写哈哈哈哈）
[opening a file](https://lispcookbook.github.io/cl-cookbook/files.html#opening-a-file)
也可以去看看我之前写的Input-Output


## Reading files {#reading-files}


### 将文件读入string 或 行list中 {#将文件读入string-或-行list中}

-   read-file-string

    ```lisp
    (uiop:read-file-string "file.txt")
    ```
-   read-file-lines

    ```lisp
    (uiop:read-file-lines "file.txt")
    ```


### read-line or read-char {#read-line-or-read-char}

这两个函数的性能不高，可以加入缓冲区解决这个问题

```lisp
(with-output-to-string (out)
  (with-open-file (in "/path/to/big/file")
    (loop with buffer = (make-array 8192 :element-type 'character)
          for n-characters = (read-sequence buffer in)
          while (< 0 n-characters)
          do (write-sequence buffer out :start 0 :end n-characters))))
```


### 以utf-8的格式读取 {#以utf-8的格式读取}

```lisp
(with-open-file (in "/path/to/big/file"
                    :external-format :utf-8)
  )
```


### 将SBCL的默认字符集设为utf-8 {#将sbcl的默认字符集设为utf-8}

在~/.sbclrc中加入

```lisp
(setf sb-impl::*default-external-format* :utf-8)

或者

(setf sb-alien::*default-c-string-external-format* :utf-8)
```


### 向文件中写入内容 {#向文件中写入内容}

```lisp
(with-open-file (f <pathname> :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
  (write-sequence s f))
```

如果文件存在，你也可以使用:append 来追加内容


#### 使用库 {#使用库}

Alexandria 有一个函数叫write-string-into-file

```lisp
(alexandria:write-string-into-file content "file.txt")
```


## 获取文件后缀 {#获取文件后缀}

```lisp
(pathname-type "~/foo.org")  ;; => "org"
```


## 获取文件属性(size,access time,....) {#获取文件属性--size-access-time-dot-dot-dot-dot}

Osicat(in quicklisp)是一个轻量级操作系统接口。使用Osicat 可以获取环境变量，操作files directories pathnames

```lisp
(ql:quickload "osicat")

(let ((stat (osicat-posix:stat #P"./files.md")))
  (osicat-posix:stat-size stat))  ;; => 10629
```

可以使用以下函数获取更多的属性

```lisp
osicat-posix:stat-dev
osicat-posix:stat-gid
osicat-posix:stat-ino
osicat-posix:stat-uid
osicat-posix:stat-mode
osicat-posix:stat-rdev
osicat-posix:stat-size
osicat-posix:stat-atime
osicat-posix:stat-ctime
osicat-posix:stat-mtime
osicat-posix:stat-nlink
osicat-posix:stat-blocks
osicat-posix:stat-blksize
```


## 列出所有的文件和文件夹 {#列出所有的文件和文件夹}

有些函数可以返回pathnames

```lisp
(namestring #p"/foo/bar/baz.txt")           ==> "/foo/bar/baz.txt"
(directory-namestring #p"/foo/bar/baz.txt") ==> "/foo/bar/"
(file-namestring #p"/foo/bar/baz.txt")      ==> "baz.txt"
```


### 返回文件夹中的文件(不包括文件夹) {#返回文件夹中的文件--不包括文件夹}

```lisp
(uiop:directory-files "./")
```

返回一连串的pathnames:

```lisp
(#P"/home/vince/projects/cl-cookbook/.emacs"
 #P"/home/vince/projects/cl-cookbook/.gitignore"
 #P"/home/vince/projects/cl-cookbook/AppendixA.jpg"
 #P"/home/vince/projects/cl-cookbook/AppendixB.jpg"
 #P"/home/vince/projects/cl-cookbook/AppendixC.jpg"
 #P"/home/vince/projects/cl-cookbook/CHANGELOG"
 #P"/home/vince/projects/cl-cookbook/CONTRIBUTING.md"
 […]
```


### 返回所有的子文件夹 {#返回所有的子文件夹}

```lisp
(uiop:subdirectories "/Users/qibinyang/test")
```

```lisp

(#P"/Users/qibinyang/test/assertTest/" #P"/Users/qibinyang/test/center/"
#P"/Users/qibinyang/test/client1/" #P"/Users/qibinyang/test/client2/"
#P"/Users/qibinyang/test/test/")
```

