# Linux磁盘管理

## linux 的设备管理
* I/O Ports: I/O设备地址
* 一切皆文件
    * 块设备: block: 存取单位"块", 磁盘
    * 字符设备: character, 存取单位"字符", 磁盘
    * 设备文件: 关联至一个设备驱动程序, 进而能够与之对应硬件设备进行通信
* 设备号码
    * 主设备号: major number, 标识设备类型
    * 次设备号: minor number

## 硬盘接口类型
* 并行
    * IDE: 133MB/s
    * SCSI: 640MB/s
* 串口
    * SATA: 6Gbps
    * SAS: 6Gbps
    * USB: 480MB/s

## 磁盘设备的设备文件命名
* IDE: /dev/hd
* SCSI, SATA, SAS, USB: /dev/sd
    * 不同设备:a-z
        * /dev/sda, /dev/sdb,......
    * 同一设备上的不同分区:1,2,3....
        * /dev/sda1, /dev/sda5 

## 机械式硬盘
    * track: 磁道
    * cylinder: 柱面
    * sector: 扇区
        * 512bytes
    * 如何分区：
        * 按柱面
    * 0磁道0柱面: 512bytes
        * MBR: Master Boot Record
            * 446bytes: boot loader
            * 64bytes: 分区表
                * 16bytes: 标识一个分区
            * 55AA
            * 4 个主分区:
                * 3个主分区+1个拓展(N个逻辑分区)

## 分区管理工具: fdisk, parted, sfdisk
* **fdisk:** 对于一块硬盘来说, 最多只能管理15个分区
    * Synopsis
            fdisk [options] device
            fdisk -l [device...]
    * fdisk device的子命令
        * p: print, 显示已有分区
        * n: new, 创建
        * d: delete, 删除
        * w: write, 写入磁盘并推出
        * q: quit, 放弃更新并退出
        * m: 获取帮助
        * l: 列出所有支持的分区id
        * t: 调整分区id
* **查看内核是否已经设别新的分区**
        # cat /proc/partations
* **partx:** 通知内核重新读取硬盘分区表
    * Synopsis
            partx [-a|-d|-P|-r|-s|-u] [-t type] [-n M:N] [-] disk
            partx [-a|-d|-P|-r|-s|-u] [-t type] partition [disk]
    * options
        * -a，--add: 通知内核添加分区
        * -n, --nr M:N: 指定分区的范围
* **kpartx:** 通知内核重新读取硬盘分区表
    * Synopsis 
            kpartx -a /dev/DEVICE
    * options
        * -a: 添加
        * -f: 强制
* **partprobe：** Centos5专用的通知内核重新读取硬盘分区表的工具(全局扫描)
    * Synopsis
            partprobe [/dev/DEVICE] 

