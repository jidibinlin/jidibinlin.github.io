using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Net.Sockets;
using UnityEngine.UI;
using System;

public class Echo : MonoBehaviour{
    Socket socket;
    public InputField InputFeld;
    public Text text;
    byte readBuff = new byte[1024];
    string recvStr = "";
    //点击链接按钮
    public void Connections(){

        //Socket
        socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream,ProtocolType.Tcp);
        // begin connect
        socket.BeginConnect("127.0.0.1",8888,ConnectCallback,socket);
    }
    //connect 回调
    public void ConnectCallback(IAsyncResult ar){
        try{
            Socket socket = (Socket) ar.AsyncState;
            socket.EndConnect(ar);
            Debug.Log("Socket Connect Succ");
            socket.BeginReceive (readBuff,0,1024,0,ReceiveCallback,socket);
        }catch(SocketException ex){
            Debug.Log("Socket Connect fail" + ex.ToString());
        }
    }

    //Receive 回调
    public void ReceiveCallback(IAsyncResult ar){
        try{
            Socket socket = (Socket) ar.AsyncState;
            int count = socket.EndReceive(ar);
            recvStr = System.Text.EnCoding.Default.GetString(readBuff,0,count);

            socket.BeginReceive(readBuff,0,1024,0,ReceiveCallback.socket);
        }catch(SocketException ex){
            Debug.Log("Socket Receive fail" + ex.ToString());
        }
    }

    public void Send(){
        //Send
        string sendStr = InputField.text;
        byte[] sendBytes = System.Text.Encoding.Default.GetBytes(sendStr);
        socket.Send(sendBytes);
    }

    public void Upate(){
        text.text = recvStr;
    }

}
