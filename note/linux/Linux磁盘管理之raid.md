# Linux磁盘管理之raid
#### 什么是raid:
raid的英文名称叫做Redundant Arrays of Inexpensive(Independent) Disks独立(廉价)冗余磁盘阵列
#### 功能:
* 提高io能力
    * 磁盘并行读写
* 提高耐用性
    * 磁盘冗余来实现 

#### 实现方法 
* **外接式RAID**: 通过拓展卡提供适配能力
* **内接式RAID**: 主板集成RAID控制器
* **软件方式实现**
    * mdadm: 用来实现raid的工具
        * Synopsis
                mdadm [mode] <raiddevice> [options] <component-device>
        * mode:
            * -C: 创建
                * -n #: 使用#个块设备来创建此RAID
                * -l #: 指明要创建的RAID的级别
                * -a (yes|no): 自动创建目标RAID设备的设备文件
                * -c CHUNK_SIZE: 指明块大小
                * -x #: 指明空闲磁盘的个数
            * -A: 装配
            * —F: 监控
            * 管理:
                * -f: 标记指定磁盘为损坏
                * -a: 添加磁盘
                * -r: 移除磁盘
                * -D: 显示raid的详细信息
                * -S: 停止raid
        * <raiddevice>: /dev/md#
        * <component-devices>: 任意块设备
        * 观察raid的状态
               watch cat /proc/mdstat 
            * watch命令
                * -n #:刷新间隔 


#### 级别
* **raid0(条带卷 strip)**
    * 读、写性能提升
    * 可用空间:N\*min(S1,S2.....)
    * 无冗余能力
    * 最少磁盘数:2
* **raid1(镜像卷 mirror)**
    * 读性能提升、写性能略有下降
    * 可用空间: 1\*min(S1,S1....)
    * 有冗余能力
    * 最少磁盘数:2
* **raid4**
    * 读、写性能提升
    * 可用空间: (N-1)*min(S1,S2...)
    * 有冗余能力
    * 最少磁盘数:3
* **raid5**
    * 读、写性能提升
    * 可用空间: (N-1)*min(S1,S2...)
    * 有冗余能力
    * 最少磁盘数:3
* **raid6**
    * 读、写性能提升
    * 可用空间:(N-2)*min(S1,S2...)
    * 有冗余能力
    * 最少磁盘数:4
* **raid10**
    * 读、写性能提升
    * 可用空间:N*min(S1,S2...)/2
    * 有冗余能力: 每组镜像最多坏一块
    * 最少磁盘数:4
* **raid01**
* **raid50**
* **JBOO**
    * 功能: 将多块磁盘的空间合并一个大的连续空间使用
    * 可用空间: sum(S1,S2...)


