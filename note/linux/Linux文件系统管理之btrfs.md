# btrfs文件系统
Btrfs(B-tree, Butter FS, Better FS)由Oracle公司研发遵循GPL协定
* **核心特性**:
    * 多物理卷支持: btrfs可由多个底层物理卷组成, 支持RAID, 以联机"添加","移除","修改"
    * 写时复制更新机制(CoW): 复制、更新及替换指针, 而非"就地"更新
    * 数据及元数据的校验码: checksum
    * 子卷: sub_volume
    * 快照: 支持快照的快照
    * 透明压缩: 

### 文件系统创建
* #### mkfs.btrfs
    * **Synopsis**
            mkfs.btrfs [options] <device> [<device>...]
    * **Options**
        * -L 'LABEL': 指明卷标
        * -d type: raid0, raid1, raid5, raid6, raid10 数据的组织方式
        * -m profile: raid0, raid1, raid5, raid6, raid10, single, dup 元数据的组织方式
        * -O feature: 要启用的特性

### btrfs文件系统的属性查看
        btrfs filesystem show

### 挂载文件系统
* #### 普通挂载
        mount -t btrfs /dev/sdb(如果文件系统由多个物理设备组成 这里可以是任意的其中的一个设备) MOUNT_POINT
* #### 挂载时指明使用透明压缩机制
        mount -o compress={lzo|zlib} DEVICE MOUNT_POINT

### 调整文件系统空间大小
        btrfs filesystem resize [+|-]size /MOUNT_POINT
### 向btrfs中新增和移除设备
* **增加**
        btrfs device add /dev/物理设备 /需要添加的挂载点
* **移除**
        btrfs device delete /dev/物理设备 /需要移除的挂载点 

### 均衡物理设备空间
* **开始均衡**
        btrfs balance start /MOUNT_POINT
* **暂停**
        btrfs balance pause /MOUNT_POINT 
* **继续**
        btrfs balance resume /MOUNT_POINT 

### 修改raid级别
* **元数据**
        btrfs balance -mconvert={raid0, raid1, raid5, raid6, raid10, single, dup} 
* **数据**
        btrfs balance -dconvert={raid0, raid1, raid5, raid6, raid10}

### 子卷
* **创建**
        btrfs subvolume create /btrfs挂载点/要创建的子卷名称
* **列出所有子卷**
        btrfs subvolume list /btrfs挂载点
* **挂载子卷**
        mount -o subvol=子卷名 /dev/父卷 /挂载点
        mount -o subvolid=子卷id /dev/父卷 /挂载点
* **删除子卷**
        btrfs subvolume delete /子卷目录
* **创建子卷快照**
        btrfs subvolume snapshot /父卷/子卷 /父卷/快照卷
* **创建文件快照**
        cp -relink 源文件 快照文件


