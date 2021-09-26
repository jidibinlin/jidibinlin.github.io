// 点击发送按钮
public void Send(){
    //Send
    string sendStr = InputFeld.text;
    bytep[] sendBytes = System.Text.Encoding.Default.GetBytes(sendStr);
    socket.BeginSend(sendBytes,0,sendBytes.Length,0,SendCallback,socket);
}

//Send回调
public void SendCallback(IAsyncResult ar){
    try{
        Socket socket = (Socket) ar.AsyncState;
        int count = socket.EndSend(ar);
        Debug.Log("Socket Send succ"+count);
    }catch(SocketException ex){
        Debug.Log("Socket Send fail"+ex.ToString());
    }
}
