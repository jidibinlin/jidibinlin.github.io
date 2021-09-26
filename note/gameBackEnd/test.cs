using System;
using System.Net;
using System.Net.Sockets;

namespace EchoServer
{
    class MainClass{
        public static void Main(string[] args){
            Console.WriteLine("Hello World!");
            //Socket
            Socket listenfd = new Socket(AddressFamily.InterNetwork,SocketType.Stream,ProtocolType.Tcp);

            //Bind
            IPAddress ipAdr = IPAddress.Parse("127.0.0.1"); // 指定ip地址
            IPEndPoint ipEp = new IPEndPoint(ipAdr,8888); // 指定ip和端口
            listenfd.Bind(ipEp); //这里绑定

            //Listen
            listenfd.Listen(0);//listenfd.Listen(backlog)开启监听。
                                //参数backlog指定队列中最多可容纳等待接受的连接数，0表示不限制
            Console.WriteLine("[服务器]启动成功");
            while(true){
                //Accept
                Socket connfd = listenfd.Accept();  // 接收客户端连接 ,客户端没有连接时，服务程序会卡在这里
                Console.WriteLine("[服务器]Accept");
                //Receive
                byte[] readBuff = new byte[1024];
                int count = countfd.Receive(readBuff);
                string readStr = System.Text.Encoding.Default.GetString(readBuff,0,count);
                Console.WriteLine("[服务器接收]"+readStr);

                byte[] sendBytes = System.Text.Encoding.Default.GetBytes(readStr);
                connfd.Send(sendBytes)
            }
        }
    }
}
