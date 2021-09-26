# LVM2:
LVM2: Logical Volume Manager Version: 2
* **dm**: device mapperL 将一个或多个底层块设备组织成一个逻辑设备的模块
    * /dev/dm-# 
* **VG**: 由多个pv组合成的单个逻辑设备
    * lvm的设备路径
        * /dev/mapper/VG-NAME-LVNAME 
        * /dev/VGNAME/LV_NAME

#### pv管理工具
* ##### pvs: 简要pv信息显示
* ##### pvdisplay: 显示pv的详细信息
* ##### pvmove: 移动pv上的数据
* ##### pvcreate: 创建pv
    * Synopsis
            pvcreate /dev/DEVICE 

#### vg管理工具
* ##### vgs: 简要vg信息显示
* ##### vgdisplay: 显示vg的详细信息
* ##### vgcreate: 创建vg
    * Synopsis 
            vgcreate [-s #[kKmMgGtTpPeE]] VolumeGroupName PhysicalDevicePath [PhysicalDevicePath...]

* ##### vgextend: 拓展vg
    * Synopsis
            vgextend VolumeGroupName PhysicalDevicePath [PhysicalDevicePath...] 

* ##### vgreduce: 缩小vg(先作pvmove)
    * Synopsis
            vgreduce VolumeGroupName PhysicalDevicePath [PhysicalDevicePath...] 
#### lv管理工具
* ##### lvs: 简要lv信息显示
* ##### lvdisplay: 显示lv的详细信息
* ##### lvcreate: 创建lv
    * Synopsis
            lvcreate -L #[mMgGtT] -n NAME VolumeGroup
* ##### lvextend: 拓展逻辑卷(之后要使用resize2fs等命令拓展文件系统的大小)
    * Synopsis
            lvextend -L [+]#[mMgGtT] /dev/VGName/LV_NAME
* ##### 缩减逻辑卷
    * 1.umount /dev/VG_NAME/LV_NAME
    * 2.e2fsck -f /dev/VG_NAME/LV_NAME
    * 3.resize2fs /dev/VG_NAME/LV_NAME #[mMgGtT]
    * 4.lvreduce -L [-]#[mMgGtT] /dev/VG_NAME/LV_NAME
    * 5.mount
* ##### 快照
    * Synopsis
            lvcreate -L #[mMgGtT] -p -r -s -n snapshot_lv_name original_lv_name 




