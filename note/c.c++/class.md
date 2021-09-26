#c++结构体与c结构体的区别
####c语言
```
struct today
{
    int hour;
    int min;
    int sec;//不可以定义成员函数没有权限控制
};
```
####c++
```
struct today
{
private://有权限控制
    int hour;
    int min;
    int sec;
public:
    void SetTime(int h,int m,int s)//可以有成员函数设置的是本对象中的变量
    {
        hour=h;
        min=m;
        sec=s;
    }
    void display()
    {
        cout<<hour<<":"<min<<":"<<"s"<<endl;
    }
};
```
#c++类的使用
```
    #include<iostream>
    struct today
    {
    private://只能被本类中的函数调用，无法从外部直接调用
        int hour;
        int min;
        int sec;
    public://可以直接从外部调用
        void SetTime(int h,int m,int s)//可以有成员函数设置的是本对象中的变量
        {
            hour=h;
            min=m;
            sec=s;
        }
        void display()
        {
            cout<<hour<<":"<min<<":"<<"s"<<endl;
        }
    };

    using namespace std;
    int main()
    {
        today t1;
        t1.SetTime(10,20,40);//通过对象.成员函数/成员变量来调用
      //t1.hour=10是错误的写法 因为hour被private 关键字所修饰
    }
```
#类的权限管理
- 对类的成员的访问做限制
####private:私有权限
```
    struct SData{
    private://此时i和j只能在本类的内部被使用
        int i;
        int j;
    };
```
####public:公有权限
```
    struct SData{
    public://此时i和j可以被类的外部使用
        int i;
        int j;
    };
```
#struct和private的区别
####struct:主要用于纯的结构体，其成员默认都是public
```
struct SData{
    int i;//公有成员
    int j;
}
```
####class:由于有功能的开发，其成员默认都是private
```
class SData{
   int i;私有成员
   int j;
};
```
#this指针
- 用于指向本类中的成员
```
struct SData{
    int i;
    int j;
    int add(int i,int j)
    {
        this.i=i;//this指向本对象
        this.j=j;
        return i+j 
    }
}
```
***
#c++默认参数
- 当函数有多个参数时通过在定义时对后面的参数进行赋值来减少调用时的参数个数
```
    #include<iostream>
    int add(int x,int y,int z=0);//add函数有三个形式参数其中最后一个被赋值了
    int main()
    {
        add(10,20);//此时不用指定最后一个实参，因为定义时已经赋值；定义时已赋值的参数的位置若有实参将会被实参覆盖
    }
    int add(int x,int y,int z=0)
    {
        return x+y+z;
    }
```
- 形式参数的赋值必须从最后一个开始，否则程序会报错，且调用是实参无法缺省

