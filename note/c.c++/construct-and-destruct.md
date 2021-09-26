#构造函数
- 构造函数可用于为某些成员变量设置初始值
    1. 构造函数名与类名相同
    2. 构造函数没有返回值类型
    3. 当新建一个类是构造函数会被自动执行
    ```
    #include<iostream>
    using namespace std
    class Constructor{
        int i;
        int j;
    public:
        Constructor()
        {
            i=0;
            j=0;
            cout<<"构造函数已被调用"<<endl;
        }
    };
```
#析构函数
- 用来自动释放对象以外的堆空间
    1. 析构函数名为**~类名**
    2. 程序结束是析构函数会被自动调用
    3. 析构函数没有返回值类型
    ```
    #include<iostream>
    using namespace std;
    class Destructor{
    public:
        int i;
        Destructor *Next;
        Destructor(){
            i=0;
        }

        ~Destructor(){
            Destructor *p=this->Next;
            Destructor *k=this->Next;
            while(p!=NULL)
            {
                k=p;
                p=p->Next;
                delete k;
            }
        }

        Destructor *MakeNode(int e){
            Destructor *ins=new Destructor;
            ins->i=e;
            return ins;
        }

        void insfir(Destructor *L,Destructor *ins){
            ins->Next=L->Next;
            L->Next=ins;
        }
    };
    int main()
    {
        Destructor *D1=new Destructor;
        for(int i=0;i<5;i++)
        {
            D1->insfir(D1,D1->MakeNode(i));
        }
        delete D1;
    }
    ```
