# Linux文件系统管理基础
* linux的文件系统有:ext2,ext3,ext4,xfs,btrfs,reiserfs,jfs,swap
    * swap: 交换分区'
    * 光盘: iso
* Windows: fat32, ntfs
* Unix: FFS,UFS,JFS2
* 网络文件系统: NFS,CIFS
* 集群文件系统: GFS2, OCFS2
* 分布式文件系统: ceph, moosefs,mogilefs,GlusterFs,Lustre
* 根据是否支持"Journal" 功能:
    * 日志型文件系统: ext3,ext4,xfs,.....
    * 非日志型文件系统: ext2,vfat
* Linux的虚拟文件系统:VFS

## 创建文件系统
* **mkfs**
    * Synopsis
            # mkfs.FS_TYPE /dev/DEVICE
        > Note: FS_TYPE可以为ext4,xfs,btrfs,vfat......

            # mkfs -t FS_TYPE /dev/DEVICE
    * options
        * -t FS_TYPE: 指定文件系统
        * -L 'LABEL': 设定卷标
* **mke2fs**
    * description
        * ext系列文件系统专用的管理工具
    * Synopsis
    		mke2fs  [  -c | -l filename ] [ -b block-size ] [ -C cluster-size ] [ -d root-directory ] [ -D ] [ -g blocks-per-group ] [ -G number-of-groups ] [ -i bytes-per-inode ] [ -I inode-size ] [ -j ] [ -J journal-options ] [ -N number-of-inodes ] [ -n ] [ -m reserved-blocks-percentage ] [ -o creator-os ] [ -O [^]feature[,...]  ] [ -q ] [ -r fs-revision-level ]  [-E  extended-options  ]  [  -v  ]  [  -F ] [ -L volume-label ] [ -M last-mounted-directory ] [ -S ] [ -t fs-type ] [ -T usage-type ] [ -U UUID ] [ -V ] [ -e errors-behavior ] [ -zundo_file ] device [ fs-size ]
			mke2fs -O journal_dev [ -b block-size ] [ -L volume-label ] [ -n ] [ -q ] [ -v ] external-journal [ fs-size ]
    * Options
        * -t {ext2|ext3|ext4}: 指明文件系统的类型
        * -b {1024|2048|4096}: 指明块大小
        * -L 'LABEL': 指明卷标
        * -j: 相当于-t ext3 | mkfs -t ext3
        * -i #: 为数据空间每多少个字节创建一个inode, 此大小不应小于block的大小
        * -N #: 为数据空间创建多少个inode
        * -m #: 为管理人员预留的空间占据的百分比
        * -O FEATURE[,......]: 开启特性
        * -O ^FEATURE: 关闭特性
* **blkid** 
    * Description
        * 块设备属性信息查看
    * Synopsis
            blkid  --label label | --uuid uuid
            blkid   [--no-encoding  --garbage-collect  --list-one --cache-file file][--output format] [--match-tag tag]  [--match-token  NAME=value][device ...]
            blkid   --probe   [--offset  offset]  [--output  format]  [--size  size][--match-tag tag] [--match-types list]  [--usages  list]  device...
            blkid   --info [--output format] [--match-tag tag] device ...
    * Options
        * -U UUID: 根据指定的UUID来查看对应的设备
        * -L LABEL: 根据指定的LABEL来查看对应的设备
* **e2label**
    * Description
        * 管理ext系列文件系统的LABEL
    * Synopsis
            e2label DEVICE [LABEL] 
* **tune2fs** 
    * Description
        * 重新设定ext系列文件可调整参数的值
    * Synopsis
            tune2fs [ -l ] [ -c max-mount-counts ] [ -e errors-behavior ] [ -f ] [-i interval-between-checks ] [ -I new_inode_size ] [ -j ] [ -J journal-options ] [ -m reserved-blocks-percentage ] [ -o [^]mount-options[,...]]  [  -r  reserved-blocks-count  ] [ -u user ] [ -g group ] [ -C mount-count ] [ -E extended-options ] [ -L volume-label ] [ -M  last-mounted-directory  ]  [  -O [^]feature[,...]  ] [ -Q quota-options ] [ -T time-last-checked ] [ -U UUID ] [ -z undo_file ] device
    * Options
        * -l: 查看指定文件系统超级块信息 super block
        * -L 'LABEL': 修改卷标
        * -m #: 修改预留给管理员的空间百分比
        * -j: 将ext2升级成ext3
        * -O: 文件系统属性的开启或禁用
        * -o: 调整文件系统的默认挂载选项
        * -U UUID: 修改UUID号
* **dump2fs**
    * -h: 查看超级块信息

## 文件系统检测
* **fsck**
    * Description 
        * 检测文件系统 File System Check
    * Synopsis
            fsck.FS_TYPE
            fsck -t FSTYPE
        > Note FS_TYPE一定要与分区上已有的文件系统类型相同
    * Options
        * -a: 自动修复错误
        * -r: 交互式修复错误

     
* **exfsck**
    * Description
        * ext系列文件专用的检测修复工具
    * Options
        * -y: 自动回答yes
        * -f: 强制修复

## 文件系统的挂载
* **定义**
    * 挂载: 将额外文件系统与根文件系统某现存的目录建立起关联关系, 进而使得此目录作为其它文件访问入口的行为称之为挂载
    * 卸载: 解除此关联关系的过程称之为卸载
* **mount**
    * Description
        * 用来挂载文件系统
    * Synopsis
            mount [-l|-h|-V]
            mount -a [-fFnrsvw] [-t fstype] [-O optlist]
            mount [-fnrsvw] [-o options] device|dir
            mount [-fnrsvw] [-t fstype] [-o options] device dir
        * device:
            * (1) 设备文件: 例如/dev/sda5
            * (2) 卷标: -L 'LABEL'
            * (3) UUID: -U 'UUID'
            * (4) 伪文件系统名称: proc, sysfs, devtmpfs, configfs
        * dir: 挂载点
            * 事先存在: 建议使用空目录
    * Options
        * -t vsftype: 指明要挂载的设备上的文件系统类型
        * -r: readonly: 只读挂载
        * -w: read and write: 读写挂载
        * -n: 不更新/etc/mtab:
        * -a: 自动挂载所有支持自动挂载的设备(定义在了/etc/fstab文件中, 且挂载选项中有“自动挂载功能”)
        * -L 'LABEL': 以卷标指定挂载设备
        * -U "UUID": 以UUID指定要挂载的设备
        * -B, --bind: 绑定目录到另一个目录上
    > Note: 查看内核追踪到的已挂载的所有设备: cat /proc/mounts
        * -o options(挂载文件系统的选项)
            * async: 异步模式
            * sync: 同步模式
            * atime/noatime: 包含目录和文件
            * diratime/nodiratime: 目录的访问时间戳
            * auto/noauto: 是否支持自动挂载
            * exec/noexec: 是否支持将文件系统上应用程序运行为进程
            * dev/nodev: 是否支持在此文件系统上使用设备文件
            * suid/nosuid:
            * remount: 重新挂载
            * ro:
            * rw:
            * user/nouser: 是否允许普通用户挂载此设备
            * acl: 启用此文件系统上的acl功能
    > Note: 上述选项可多个同时使用, 彼此间用逗号分割  
    >   默认挂载选项:default(rw,suid,dev,exec,auto,nouser,async) 
* **umount**
    * Description
        * 用来卸载文件系统
    * Synopsis
            umount -a [-dflnrv] [-t fstype] [-O option...]
            umount [-dflnrv] {directory|device}...
            umount -h|-V
* **fuser**
    * Description
        * 查看和终止正在访问文件系统的进程
    * Synopsis
            # fuser -v MOUNT_POINT: 查看
            # fuser -km MOUNT_POINT: 终止进程
* **swapon**
    * Description
        * 启用交换分区
    * Synopsis
            swapon [options] [device]
    * options: 
        * -a: 激活所有交换分区
        * -p priority: 指定优先级
* **swapoff**
    * Description
        * 禁用交换分区
    * Synopsis
            swappoff [options] [device]
* **free**
    * Description
        * 查看内存空间的使用状态
    * Synopsis
            free [options]
    * Options
        * -m: 以MB作为单位
        * -g: 以GB作为单位
* **df**
    * Description
        * 文件系统空间占用等信息的查看工具
    * Synopsis
            df [OPTION]... [FILE]...
    * Option
        * -h: human-readable
        * -i: inodes istead of blocks
        * -P: 以Posxi兼容的格式输出
* **du**
    * Description
        * 查看目录总体空间占用状况
    * Synopsis
            du [OPTION]... [FILE]...
            du [OPTION]... --files0-from=F
    * Option
        * -h: human-readable
        * -s: summary

## 文件系统挂载的配置文件: /etc/fstab
* **Description**
    * 每行定义一个要挂载的文件系统:
            要挂载的设备或伪文件系统                    挂载点      文件系统类型    挂载选项    转储频率    自检次序 
            UUID=73bdfacf-5727-42c7-9475-a8c8f97ddb8b	/       ext4    rw,relatime	0           1
        * 设备文件: 可以是label(LABEL=""), UUID(UUID=""), 伪文件系统名称(proc,sysfs)
        * 挂载选项: defaults
        * 转储频率
            * 0: 不做备份
            * 1: 每天转储
            * 2: 每隔一天转储
        * 自检次序:
            * 0: 不自检
            * 1: 首先自检: 一般只有rootfs才用1
            * ......

## 文件系统上的其他概念:
* ### Inode
    * Inode: Index Node, 索引节点
        * 地址指针
            * 直接指针
            * 间接指针
            * 三级指针
        * inode bitmap: 对位标识每个inode空闲与否的状态信息
* ### 链接文件
    * 硬链接: 指向同一个inode的多个不同路径, 创建文件的硬链接即为为inode创建新的引用路径, 英此会增加其引用计数
        * 不能够对目录进行
        * 不能跨分区进行
    * 符号链接: 指向的是另一个文件的路径, 其大小为指向路径字符串的长度, 不增加或减少目标文件inode的引用计数
        * 可以对目录进行
        * 不能跨分区进行
    * **ln**
        * Description
            * 创建链接
        * Synopsis
                ln [OPTION]... [-T] TARGET LINK_NAME
                ln [OPTION]... TARGET
                ln [OPTION]... TARGET... DIRECTORY
                ln [OPTION]... -t DIRECTORY TARGET...
        * options
            * -s: symbolic link
            * -v: verbose

## 挂载光盘设备:
* **光盘设备文件**
    * IDE:
        * /dev/hdc
    * SATA: /dev/sr0
    * 符号链接文件:
        * /dev/cdrom
        * /dev/cdrw
        * /dev/dvd
        * /dev/dvdrw
* **dd**: convert and copy a file 多用于磁盘的拷贝
    * Synopsis
            dd if=/PATH/FROM/SRC of=/PATH/TO/DEST 
    * options 
        * bs=#: block size, 复制单元大小
        * count=#: 复制#个bs
    * 磁盘拷贝:
            dd if=/dev/sda of=/dev/sdb
    * 备份MBR
            dd if=/dev/sda of=/tmp/mbr.bak bs=512 count=1 
