Dim targetHost As String = "tw100104373"
        Dim targetPort As Integer = 80

        Dim proxyClient As New TcpClient(targetHost, targetPort)
        Dim proxyStream As NetworkStream = proxyClient.GetStream()

        ' Connect to target server through proxy
        Dim connectCommand As String = "GET /agv/ajax/AddAGVCmd_PLC.php?bs001=123&bs002=1&bs003=5&bs004=5&bs005=5 HTTP/1.0" & vbCrLf & vbCrLf
                               
        Dim connectBytes As Byte() = System.Text.Encoding.ASCII.GetBytes(connectCommand)
        proxyStream.Write(connectBytes, 0, connectBytes.Length)

        ' Read response from proxy
        Dim responseBuffer(1024) As Byte
        Dim bytesRead As Integer = proxyStream.Read(responseBuffer, 0, responseBuffer.Length)
        Dim response As String = System.Text.Encoding.ASCII.GetString(responseBuffer, 0, bytesRead)
        TextBox1.Text = response
        Threading.Thread.Sleep(500)
        ' Check if connection to target server was successful
        If response.StartsWith("HTTP/1.0 200") Or response.StartsWith("HTTP/1.1 200") Then
            ' Connection succeeded, you can now read/write data to target server
            Dim targetStream As NetworkStream = proxyClient.GetStream()
            ' Perform read/write operations on targetStream
            Dim dataBuffer(1024) As Byte
            Dim dataBytesRead As Integer = targetStream.Read(dataBuffer, 0, dataBuffer.Length)
            Dim responseData As String = System.Text.Encoding.ASCII.GetString(dataBuffer, 0, dataBytesRead)

            ' Display response data in TextBox
            TextBox2.Text = responseData
        Else
            ' Connection failed, handle error
            Console.WriteLine("Connection to target server failed")
        End If

        proxyStream.Close()
        proxyClient.Close()
