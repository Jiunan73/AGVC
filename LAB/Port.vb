Imports System.Net.Sockets
Imports System.Threading
Module Port
    Public Structure EQP_Modus
        Dim flag As Boolean

        Dim EQ_ID As String
        Dim ReadOK As Boolean

        Dim ipadress As String
        Dim port As Integer
        Dim tcp_client As TcpClient
        Dim TCPstream As NetworkStream
        Dim startDI As Integer
        Dim HoldingReg As Integer
        Dim Pre_Val As Integer
        ' Dim down_sensor As Integer

        Public DI() As Byte
        Public DI_val As Integer

        Dim retry As Integer
        Dim serialNo As Integer

        Public HoldingResponse() As Integer


        Dim tag_id As Integer
        Dim AXIS_X As Integer
        Dim AXIS_Y As Integer

        Dim Pre_State As Integer

        Dim WriteDList() As Integer
        Dim PreWriteDList() As Integer
        Dim BMS1() As Integer
        Dim BMS2() As Integer
        Dim SN1 As String
        Dim SN2 As String
        Dim BMSAlarm1() As Integer
        Dim BMSAlarm2() As Integer
        Public Sub init(ByVal ip As String, ByVal iport As Integer, ByVal startdi As Integer)
            ReDim DI(15)
            ReDim HoldingResponse(59)
            ReDim WriteDList(15)
            ReDim PreWriteDList(15)
            ipadress = ip
            serialNo = 1
            port = iport
            ReDim BMS1(60)
            ReDim BMS2(60)
            ReDim BMSAlarm1(20)
            ReDim BMSAlarm2(20)
        End Sub

        Function Read_DI() As Boolean
            Read_DI = False
            Dim Wbyte(11) As Byte
            Dim Rbyte(100) As Byte
            Dim cnt As Integer = 0
            ' While True
            If retry > 10 Or tcp_client Is Nothing Then
                '斷線處理
                Try
                    TCPstream.Close()
                    TCPstream.Dispose()
                Catch ex As Exception
                End Try
                Try
                    tcp_client.Close()
                Catch ex As Exception
                End Try

                If Not ipadress = "" Then
                    tcp_client = New TcpClient
                    Try
                        tcp_client.Connect(ipadress, port)
                    Catch ex As Exception
                    End Try
                    If tcp_client.Connected Then
                        TCPstream = tcp_client.GetStream
                        TCPstream.WriteByte(80)
                        TCPstream.ReadTimeout = 1000
                        retry = 0
                    End If
                End If
                DI_val = 0
                For i As Integer = 0 To 15
                    DI(i) = 0
                Next
            ElseIf tcp_client.Connected Then
                Try
                    Wbyte(0) = 0
                    Wbyte(1) = 0
                    Wbyte(2) = 0
                    Wbyte(3) = 0
                    Wbyte(4) = 0
                    Wbyte(5) = 6
                    Wbyte(6) = 1
                    Wbyte(7) = 2
                    Wbyte(8) = 0
                    Wbyte(9) = startDI
                    Wbyte(10) = 0
                    Wbyte(11) = 16
                    While TCPstream.DataAvailable
                        TCPstream.ReadByte()
                    End While
                    TCPstream.Write(Wbyte, 0, 12)
                    ' Thread.Sleep(100)
                    TCPstream.ReadTimeout = 100
                    cnt = TCPstream.Read(Rbyte, 0, 100)
                    If cnt = 11 And Rbyte(5) = 5 And Rbyte(7) = 2 And Rbyte(8) = 2 Then
                        Dim temp As Integer
                        DI_val = Rbyte(9) + (Rbyte(10) * 256)
                        temp = DI_val
                        For i As Integer = 0 To 15
                            DI(i) = temp Mod 2
                            temp = temp >> 1
                        Next
                    End If
                    retry = 0
                    Read_DI = True
                Catch ex As Exception
                    retry += 1
                End Try
            Else
                retry += 1

            End If


        End Function

        Function Read_HoldingReg(ByVal addr As Integer, ByVal iSize As Integer, ByRef Response() As Integer) As Boolean
            Read_HoldingReg = False
            Dim Wbyte(11) As Byte
            Dim Rbyte(200) As Byte
            Dim cnt As Byte
            serialNo += 1
            If serialNo > 255 Then
                serialNo = 1
            End If
            ' While True
            If retry > 2 Or tcp_client Is Nothing Then
                '斷線處理
                Try
                    TCPstream.Close()
                    TCPstream.Dispose()
                Catch ex As Exception
                End Try
                Try
                    tcp_client.Close()
                Catch ex As Exception

                End Try

                If Not ipadress = "" Then
                    tcp_client = New TcpClient
                    Try
                        tcp_client.Connect(ipadress, port)
                    Catch ex As Exception
                    End Try
                    If tcp_client.Connected Then
                        TCPstream = tcp_client.GetStream                    
                        TCPstream.ReadTimeout = 300
                        retry = 0
                    End If
                End If

            ElseIf tcp_client.Connected Then
                Try
                    Wbyte(0) = serialNo \ 256
                    Wbyte(1) = serialNo Mod 256 ' 流水號
                    Wbyte(2) = 0
                    Wbyte(3) = 0
                    Wbyte(4) = 0
                    Wbyte(5) = 6 '後面有幾個字元
                    Wbyte(6) = 1 'ID
                    Wbyte(7) = 3 'Fn
                    Wbyte(8) = addr \ 256  'Addr
                    Wbyte(9) = addr Mod 256 'Addr
                    Wbyte(10) = iSize \ 256 'Count
                    Wbyte(11) = iSize Mod 256 'Count

                    TCPstream.Write(Wbyte, 0, 12)
                    Thread.Sleep(100)
                    cnt = TCPstream.Read(Rbyte, 0, 200)
                    If cnt > 13 Then
                        If Rbyte(5) = Rbyte(8) + 3 Then
                            For i As Integer = 0 To Rbyte(8) / 2 - 1
                                Response(i) = Rbyte(9 + i * 2) * 256 + Rbyte(10 + i * 2)
                            Next
                            Return True
                        End If
                    End If
                    retry = 0
                    Read_HoldingReg = True
                Catch ex As Exception
                    retry += 1
                End Try
            Else
                retry += 1
            End If
        End Function
        Function Write_HoldingReg(ByVal addr As Integer, ByVal iSize As Integer, ByVal val() As Integer) As Boolean

            Write_HoldingReg = False
            Dim Wbyte(500) As Byte
            Dim Rbyte(500) As Byte
            Dim cnt As Byte
            serialNo += 1
            If serialNo > 255 Then
                serialNo = 1
            End If
            ' While True
            If retry > 2 Or tcp_client Is Nothing Then
                '斷線處理
                Try
                    TCPstream.Close()
                    TCPstream.Dispose()
                Catch ex As Exception
                End Try
                Try
                    tcp_client.Close()
                Catch ex As Exception

                End Try

                If Not ipadress = "" Then
                    tcp_client = New TcpClient
                    Try
                        tcp_client.Connect(ipadress, port)
                    Catch ex As Exception
                    End Try
                    If tcp_client.Connected Then
                        TCPstream = tcp_client.GetStream
                        TCPstream.ReadTimeout = 300
                        retry = 0
                    End If
                End If

            ElseIf tcp_client.Connected Then
                Try
                    Wbyte(0) = serialNo \ 256
                    Wbyte(1) = serialNo Mod 256 ' 流水號
                    Wbyte(2) = 0
                    Wbyte(3) = 0
                    Wbyte(4) = 0
                    Wbyte(5) = 7 + iSize * 2 '後面有幾個字元
                    Wbyte(6) = 1 'ID
                    Wbyte(7) = 16 'Fn
                    Wbyte(8) = addr \ 256  'Addr
                    Wbyte(9) = addr Mod 256 'Addr
                    Wbyte(10) = iSize \ 256 'Count
                    Wbyte(11) = iSize Mod 256 'Count
                    Wbyte(12) = iSize * 2 'Count

                    For i As Integer = 0 To iSize - 1
                        Wbyte(13 + i * 2) = Val(i) \ 256
                        Wbyte(13 + i * 2 + 1) = Val(i) Mod 256
                    Next

                    TCPstream.Write(Wbyte, 0, 13 + iSize * 2)
                    Thread.Sleep(100)
                    cnt = TCPstream.Read(Rbyte, 0, 200)
                    If Rbyte(7) = 16 Then
                        retry = 0
                        Write_HoldingReg = True
                    End If
                Catch ex As Exception
                    retry += 1
                End Try
            Else
                retry += 1
            End If
        End Function
    End Structure


End Module
