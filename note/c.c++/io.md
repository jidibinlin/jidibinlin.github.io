#c++输入输出
-   c++输入输出类包含在iostream头文件的namespace std命名空间中
###count输出函数
####使用格式
``` 
    std::cout<<字符串或变量<<子符串或变量<<.......;//在<<中间包含想要输出的字符串或变量
    cout<<字符串或变量<<endl;//endl是换行
    int i=100;
    cout<<字符串或变量<<i<<endl;//i是变量 在c++中不需要指定**%**格式控制
```
###cin输入函数
####使用格式
```
    int i,j;
    cin>>j>>i;//给i和j进行输入赋值不需要想c那样取地址
```
---
#c++重载函数
-   函数名相同
-   函数的参数类型或个数不同
```
    void max(int x,int y);
    void max(int x,double y);//这两个函数就是一组重载函数
```
---
#c++申请堆空间
###new申请堆函数
####申请堆变量
#####c语言
```
    int *p=(int *)malloc(sizeof(int));//申请一个堆变量
```
#####c++
```
    int *i=new int;
```
####申请堆数组
#####c语言
```
    int *p=(int *)malloc(sizeof(int)*10);//申请长度为10的堆数组
```
#####c++
```
    int *p=new int[10];
```
####申请结构体
#####c语言
```
    struct SData
    {
        int data;
        char chara;
    };
    int main()
    {
        struct SData *s1=(struct SData *)malloc(sizeof(SData));
    }
```
#####c++
```
    struct SData
    {
        int data;
        char chara;
    };
    int main()
    {
        SData *s1=new SData;
    }
```
#c++释放堆空间
###delete释放堆空间函数
#####非数组释放
```
    int *i=new int;//申请堆变量i
    delete i;//释放堆变量i
```
#####数组释放
```
    struct SData{
        int i;
        char chara;
    };
    int main()
    {
        SData *p=new SData[10];//申请堆结构体数组
        delete []p;//释放堆结构体数组
    }
```
***
#内连函数inline
* 类似于c语言中的宏替换防止重复进如函数消耗资源
####c语言
```
    #define max(a,b) a>b? a:b//使用define max(a,b) 来替换
    int main()
    {
        max(1,2);
    }
```
####c++
```
    inline int max(a,b);
    int main()
    {
        max(1,2);
    }
    inline int max(int a,int b)//使用inline函数
    {
        return a>b? a:b;
    }
```

