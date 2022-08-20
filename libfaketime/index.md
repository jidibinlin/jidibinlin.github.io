# libfaketime + roswell + emacs 时间测试尝试


<!--more-->

如果你写的程序经常依赖时间去做一些逻辑控制，那么你可能对调时间这件事比较敏感，你可能说不上讨厌，但是一定不喜欢。电脑频繁调时间会导致一些意想不到的问题，比如在我使用的mac m1 mini 上就会有交换内存暴涨40个g的问题。还会导致梯子失效无法访问外网。再比如你以为才上午12点，实际上已经晚上都已经下班了,你对时间没有概念了。如果往回调时间我的emacs还会卡死😮‍💨。 总之调时间好像是一件小事，但是影响却很大。

所以我一直在尝试如何在不调时间的情况下，给指定程序一个假的时间。我曾试过用容器来解决这个问题，不过容器内的时间会定时和宿主机同步，并不能解决时间问题。后来我发现了libfaketime这个工具，不过弄了半天一直没能在我的mac m1 上正常使用。最近我又遇到了要频繁调时间的情况，所以下定决心要解决这个问题。

那么在这篇blog中，你将看到如何使用libfaketime,如何使用roswell脚本制作指定程序的libfaketime 环境（先初始化时间环境，再启动指定程序），最后是如何使用emacs一键启动。


## libfaketime 安装 {#libfaketime-安装}

对于不同的环境，libfaketime的安装方法也不相同。

-   对于mac如果你使用homebrew(homebrew如何安装我就不赘述了)

    > brew install libfaketime

-   对于linux 你可以研究下自己发行版的包管理器，比如ubuntu

    > sudo apt-get install libfaketime

-   windows嘛，我讨厌windows抵制程序员使用windows。所以就不介绍了（不过我建议你用wsl,把windows当作linux的子系统😝）

-   还有一种源码编译的安装方式，你们可以看下libfaketime 的自述文件[libfaketime-readme](https://github.com/wolfcw/libfaketime/blob/master/README)


## 如何使用libfaketime {#如何使用libfaketime}

安装完成后，你有两件事情要做。

-   第一个读一遍libfaketime的readme。对于mac来说你应该读一遍[libfaketime-mac-readme](https://github.com/wolfcw/libfaketime/blob/master/README.OSX)
-   第二个，你需要知道libfaketime的动态链接库被发到了哪个目录下面(brew安装的是 /opt/homebrew/lib/faketime/libfaketime.1.dylib)

libfaketime大概的原理就是，让程序链接libfaketime的库达到改时间的目的（允许我这么潦草😄，毕竟这个库不是我写的）。所以我们应该让程序自己链接libfaketime。
在unix环境下，我们可以通过设置环境变量的方式来达到这个目的

-   mac下你需要
    ```sh
    export DYLD_FORCE_FLAT_NAMESPACE=1
    export DYLD_INSERT_LIBRARIES=/path/to/libfaketime.1.dylib
    ```
-   linux下你需要
    ```sh
    export LDPRELOAD=/path/to/libfaketime.1.so
    ```

-   在你正确设置了上面的环境变量后（mac 下最好通过export 设置环境变量，我试过直接在程序前面设置但是没成功）,你可以通过在程序启动前设置 "faketime" 这个环境变量来达到控制程序时间的目的。

    faketime有很多中设置格式，我只介绍如何设置指定时间，并让时间自然流逝的方法。libfaketime还有很多用法，都可以在官方的自述文件里面看到。

    现在直接上命令
    ```sh
    export faketime_dont_reset=1
    export faketime="@2022-08-20 00:00:00"
    ```
    "@"字符很关键，没有""@"时间将不会自动流逝,另外faketime_dont_reset这个变量有时候也不用设置，但是如果你发现程序时间和你预期的不一样，你应该把faketime_dont_reset设置为1(具体原因你可以去看自述文件)

-   最后一步就是启动程序，不需要我教你吧😊


## 如何使用roswell 初始化一个libfaketime环境 {#如何使用roswell-初始化一个libfaketime环境}

先介绍一下roswell。roswell 是一个common lisp 的实现管理工具，但是他自身有挺多的附带功能。其中一个就是可以作为bash 脚本的替代品，用common lisp 去写脚本。这样的好处是，common lisp要比shell脚本好写一点（主要是不像shell那么容易出错shell脚本一个空格不对都不行😒）。再者就是，利用repl,你可以非常方便的调试roswell脚本（动态语言的repl简直不要太香，开发效率真的要比go那种静态语言高，这也是我不选go的原因）。

如果你不喜欢common lisp, 你也可以使用python这样的脚本语言，也能达到一样甚至更好的效果。我简单介绍一下我的思路，其实就一句话，使用脚本语言去设置libfaketime的一系列环境变量，然后启动程序。你可以给脚本语言加上几个参数 比如程序的路径，你想要设置的时间等等去控制脚本的行为这都取决于你自己了

使用roswell作为脚本，你就需要知道如何使用common lisp的一些与系统交互的库，比如如何解析命令行的参数可以使用 unix-opts这个库。看代码喽(看不懂也没关系，你可以用你喜欢的语言甚至shell脚本自己研究下，很好写的)

这段脚本的行为是， 接受一个-e参数 用来指定程序的路径,以及一个-t参数用来指定时间

```lisp
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp(ql:quickload '() :silent t)
  )

(defpackage :ros.script.luna_time_tast.3869869288
  (:use :cl))
(in-package :ros.script.luna_time_tast.3869869288)


(ql:quickload "unix-opts")

(ql:quickload "uiop")

;; 定义命令行参数
(opts:define-opts
  (:name :bin-path
   :description "binary path to be executed"
   :short #\e
   :long "bin_path"
   :arg-parser #'uiop:native-namestring)
  (:name :time
   :description "time to be used"
   :short #\t
   :long "time"
   :arg-parser #'string)
  )

(defun main (&rest argv)
  (declare (ignorable argv))
  ;; 解析命令行参数
  (multiple-value-bind (options)
      (opts:get-opts argv)
    (let* ((binary-path (getf options :bin-path))
           ;;找到程序的父目录
           (dir (uiop:pathname-directory-pathname binary-path))
           (time (getf options :time)))
      ;; 设置libfaketime环境变量
      (sb-posix:setenv "DYLD_FORCE_FLAT_NAMESPACE" "1" 1)
      (sb-posix:setenv "DYLD_INSERT_LIBRARIES" "/opt/homebrew/lib/faketime/libfaketime.1.dylib" 1)
      (sb-posix:setenv "FAKETIME_DONT_RESET" "1" 1)
      (sb-posix:setenv "FAKETIME" time 1)

      ;; 切换到程序目录
      (uiop:chdir dir)
      ;;执行程序
      (uiop:run-program (list "./game"))
      )
    )
  )
```


## emacs集成 {#emacs集成}

其实我会用common lisp当作脚本语言是因为我比较喜欢用emacs, hack emacs需要用到lisp 这样的语言。emacs 需要做的是封装interactive函数去异步调用上述的脚本。

```lisp
;; 一个时间变量，后面会带入脚本
(setq luna_time "@2022-08-23 23:59:20")
;; 脚本的路径
(setq luna_run "/Users/qibinyang/Data/luna_time_test/luna_time_tast.ros")
(defun cycle-rank-test()
  "test luna game"
  (interactive)
  ;; 异步调用脚本执行我想要调时间的程序 我的工作是游戏开发，最近在做跨服业务所以会开三个
  (start-process "run_game" (get-buffer "*Messages*") luna_run "-e" "/opt/cycle_test/game/game" "-t" luna_time)

  (start-process "run_game" (get-buffer "*Messages*") luna_run "-e" "/opt/cycle_test/game2/game" "-t" luna_time)

  (start-process "run_game" (get-buffer "*Messages*") luna_run "-e" "/opt/cycle_test/game_center/game" "-t" luna_time)
  )
```

这样我就可以在emacs 中 M-x cycle-rank-test 将"20200-08-23 23:59:20" 这个时间带入到程序中。如果我想换一个时间，只需要修改luna_time这个变量，然后重启程序即可。


## 总结 {#总结}

毕业到工作有一年的时间了，经历了挺多的事情的。印象最深的是失恋以后，又慢慢的重整旗鼓，健身改造自己，和自己对话慢慢的走出以前的漩涡。保持积极的心态，慢慢的接受新的思想，新的理念。锻炼身体，也锻炼自己的内心。慢慢接受变化，向着不惧怕变化前进。这些改变确实给我带来了我没有想到的好处。

最近有断时间没写博客了，所以今天补上一篇工作日记。从高中的时候就接触到linux，在linux和编辑器上浪费了大量的时间。现在逐渐稳定下来，未来的半年应该把重点放在算法和后端架构的理解上，兼顾开发一些小工具，提升自己的效率。最近对自己的编码风格也不是很满意，写的程序有点乱还得注意一下。

最怕懒在床上，困在手机里，被低级趣味消磨意志。

