#bool型变量
-    1个字节变量缺省赋值为true=1或者false=0
#引用型变量
-   类似于c语言中的指针变量
##使用方法及与c语言的区别
-   类似于c语言中的指针变量
    * c语言中我们使用
    ```
        int i;//声明一个变量
        int *p=&i;//让指针指向i的地址
        printf("%d",*p);//用*p来使用i的值
        *p=100;//用*p来对i惊醒赋值
    ```
    * c++中使用
        ```
            int i=10;
            INT &J=I;//让J指向I的地址
            j=-1;//不需要加*号就可以对i进行赋值和使用

        ```
##特点
    1. 引用型变量：让申请的变量挂在原有**同类型**变量
    2. 引用型变量必须初始化，必须挂在一个现有的同类型变量上
    3. 引用型变量的地址与来源变量的地址一定是在同一个内存地址上
    4. 引用型变量与来源变量任何一个数值变化则另一个一定改变
    5. 引用型变量做参数，经常用来代替来源变量赋值或取值
        ```
            #include<stdio.h>
            void Test(double &j);//声明一个函数j为引用型变量
            int main(void)
            {
                int i=0;
                Test(i);//调用Test函数
                printf("%d",i);//程序将输出100而不是0
            }
            void Test(double &j)
            {
                j=100;//对j赋值就是对i赋值
            }
        ```
---
#命名空间
-   为了防止不同程序员编写的代码函数或变量**命名冲突**
##使用方法
###命名
    ```
#include<stdio.h>   
    namespace name//name是命名空间的名字
    {
        int i=0;//代码体
    }

    int main(){

    }

    ```
###使用
    ```
#include<stdio.h>   
    namespace name//name是命名空间的名字
    {
        int i=0;//代码体
    }

    int main(){
        printf("%d",name::i);//调用name空间中的i
    }
    ```
###一次性包含(不必再使用name::)
    ```
#include<stdio.h>
    using namespace name;
    int main(void)
    {
        printf("%d",i);
    }
    
    ```
