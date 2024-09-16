Imports System.IO
Imports System.Drawing
Imports System.Math
Imports System.Text
Imports System.Net.Sockets
Imports System.Net
Imports MySql.Data.MySqlClient
Imports System.Threading
Imports System.Configuration
Imports System.Xml
Imports DijkstraPath
Imports System.Diagnostics

Public Class Form1


    Dim car_no As Integer = 50 '變更數量 dowork要改
    Dim Car(car_no) As Car_point
    Dim shelf_car_total_no As Integer = 410
    Dim shelf_car(shelf_car_total_no) As shelf_car_point
    Dim path(100, 2) As String
    Dim Tag_point_list(5000) As Tag_Point
    Dim Tag_point_Dictionary As New Dictionary(Of Integer, Tag_Point)()

    Delegate Sub settextcallback(ByVal logout As String, ByVal append As Boolean)
    Delegate Sub connectlogcallback(ByVal logout As String, ByVal append As Boolean)
    Dim Door_List(40) As Door_point
    Dim LFT_List(6) As LFT_point
    Dim path_S(300) As SPath
    Dim path_base(5000) As Path
    Dim path_fork_base(5000) As Path


    Dim Path_base_Dictionary As New Dictionary(Of Integer, Path)()
    Dim Path_fork_Dictionary As New Dictionary(Of Integer, Path)()


    Dim Dijkstra_list(20) As Dijkstra.Dijkstra_ary
    Dim AgvTimeoutVal As Integer = 99999

    Dim Dijkstra_fn As Dijkstra.Dijkstra_ary
    Dim Tag_ID_List(5000) As Integer
    Dim Tag_ID_Fork_List(5000) As Integer
    '  Dim Read_modbus(500) As Integer
    Dim log_filename As String = ".\log\" + Now.ToString("yyyyMMdd") + ".log"
    Dim log As StreamWriter

    Dim connectlogtxt_filename As String = ".\log\" + "connect" + Now.ToString("yyyyMMdd") + ".log"
    Dim connectlogtxt As StreamWriter
    Dim view_car_idx As Integer = 0
    Dim Mysql_str As String = "charset=utf8 ;Database=agv; Data Source=127.0.0.1;User id=agvc;Password=agv; Allow Zero Datetime=True;"

    Dim offset_x As Integer = 0
    Dim offset_y As Integer = 0
    Dim group_path(600) As String
    Dim MyDb As String = "agv"
    Dim IP_adress As String = "127.0.0.1"
    Dim user As String = "agvc"
    Dim password As String = "FA$admin01"

    Dim PathLen As Integer = 7
    Dim ReMapLen As Integer = 4
    Dim DoorSetLen As Integer = 2
    Dim RetreatPath As Integer = 10
    Dim BmsAlertIdx As Integer = (1) + (1 << 2) + (1 << 3) + (1 << 5) + (1 << 7) + (1 << 9) + (1 << 13) + (1 << 15) ' 41645 '1010 0010 1010 1101
    Dim BmsWarmIdx As Integer = (1 << 14) + (1 << 12) + (1 << 11) + (1 << 10) + (1 << 8) + (1 << 4) + (1 << 1)  '1010 0010 1010 1101

    Dim alarm As ALM
    Dim testval As Integer = 0
    'Dim Rotate_list(500) As Integer
    'Dim Fork_point As String
    'Dim ALL_Loading_check As Boolean = True

    '  Dim thread_idx As Long = 0
    Public Sub ListenCmd()
        Dim myTCPlistenter As TcpListener
        Dim icount As Integer = 0
        Dim iport As Integer = CInt(LPort.Text) + 1000
        myTCPlistenter = New TcpListener(IPAddress.Any, iport)
        Dim Clientsocket As Socket
        myTCPlistenter.Start()
        Do
            Clientsocket = myTCPlistenter.AcceptSocket

            If (Clientsocket.Connected) Then
                ' Dim myobj As New Car_point
                'myobj.idx = icount

                settext("Port:" + iport.ToString + "有一CMD連線" + IPAddress.Parse(CType(Clientsocket.RemoteEndPoint, IPEndPoint).Address.ToString()).ToString + vbCrLf, True)
                Dim ReceiveThread As New Thread(AddressOf ReceiveCmd)
                ReceiveThread.IsBackground = True
                ReceiveThread.Start(Clientsocket)
                icount += 1
            End If
        Loop
    End Sub
    Public Sub ReceiveCmd(ByVal Client_socket As Object)
        Dim socket_Client As Socket = CType(Client_socket, Socket)
        Dim myReceiveBytes(1023) As Byte
        Dim cnt As Integer
        Dim wByte(1023) As Byte
        Dim cmd As String = ""
        Dim cmd_stream = New NetworkStream(socket_Client)
        Dim cmd_list() As String
        Dim rt_cmd As String = ""
        Dim idx As Integer
        Try
            While 1
                cnt = cmd_stream.read(wByte, 0, 100)

                cmd = System.Text.Encoding.Unicode.GetString(wByte)

                ' MsgBox(cmd)
                If cmd.StartsWith("$") Then
                    cmd_list = cmd.Substring(1).TrimEnd(vbCrLf).Split(",")
                    If cmd_list.Length = 2 Then
                        '判斷是否存在於資料庫內
                        idx = 999

                        For i As Integer = 0 To Car.Length
                            If Car(i).device_no = CInt(cmd_list(0)) Then
                                idx = i
                                Exit For

                            End If
                        Next
                        If idx = 999 Then
                            rt_cmd = "$" + cmd_list(0) + "," + "999" + vbCrLf
                        ElseIf cmd_list(1).StartsWith("?") Then
                            rt_cmd = "$" + cmd_list(0) + "," + Car(idx).device_status(18).ToString + vbCrLf
                        ElseIf cmd_list(1).StartsWith("1") Then
                            '異常
                            If Car(idx).device_status(18) = 0 Then
                                Try
                                    Car(idx).To_AGV(20) = CInt(cmd_list(1))
                                Catch ex As Exception
                                    Car(idx).To_AGV(20) = 1000
                                End Try

                                rt_cmd = "$" + cmd_list(0) + "," + "1" + vbCrLf
                            Else
                                rt_cmd = "$" + cmd_list(0) + "," + "0" + vbCrLf

                            End If
                        ElseIf cmd_list(1).StartsWith("0") Then
                            '復歸
                            If Car(idx).device_status(18) = 1000 Then
                                Car(idx).To_AGV(20) = 0
                                rt_cmd = "$" + cmd_list(0) + "," + "1" + vbCrLf
                            Else
                                rt_cmd = "$" + cmd_list(0) + "," + "0" + vbCrLf
                            End If
                        End If



                        cnt = System.Text.Encoding.Unicode.GetBytes(rt_cmd).Length
                        cmd_stream.write(System.Text.Encoding.Unicode.GetBytes(rt_cmd), 0, cnt)


                    End If
                End If
            End While
        Catch ex As Exception

        End Try


        Try
            'socket_Client.Disconnect()
            socket_Client.Close()
            socket_Client.Dispose()
        Catch ex As Exception

        End Try





    End Sub
    Public Sub StartListen()
        Dim myTCPlistenter As TcpListener
        Dim icount As Integer = 0
        Dim iport As Integer = CInt(LPort.Text)
        myTCPlistenter = New TcpListener(IPAddress.Any, iport)
        Dim Clientsocket As Socket
        myTCPlistenter.Start()
        Do
            Clientsocket = myTCPlistenter.AcceptSocket

            If (Clientsocket.Connected) Then
                ' Dim myobj As New Car_point
                'myobj.idx = icount

                settext("Port:" + iport.ToString + "有一新連線" + IPAddress.Parse(CType(Clientsocket.RemoteEndPoint, IPEndPoint).Address.ToString()).ToString + vbCrLf, True)

                Dim ReceiveThread As New Thread(AddressOf ReceiveData)
                ReceiveThread.IsBackground = True
                ReceiveThread.Start(Clientsocket)
                icount += 1
            End If
        Loop
    End Sub
    Public Sub StartListen1()
        Dim myTCPlistenter As TcpListener
        Dim icount As Integer = 0
        Dim iport As Integer = CInt(LPort.Text)
        myTCPlistenter = New TcpListener(IPAddress.Any, iport + 1)
        Dim Clientsocket As Socket
        myTCPlistenter.Start()
        Do
            Clientsocket = myTCPlistenter.AcceptSocket

            If (Clientsocket.Connected) Then
                ' Dim myobj As New Car_point
                'myobj.idx = icount
                settext("Port:" + (iport + 1).ToString + "有一新連線" + IPAddress.Parse(CType(Clientsocket.RemoteEndPoint, IPEndPoint).Address.ToString()).ToString + vbCrLf, True)
                Dim ReceiveThread As New Thread(AddressOf ReceiveData)
                ReceiveThread.IsBackground = True
                ReceiveThread.Start(Clientsocket)
                icount += 1
            End If
        Loop
    End Sub
    Public Sub ReceiveData(ByVal Client_socket As Object)
        Dim socket_Client As Socket = CType(Client_socket, Socket)
        Dim myReceiveBytes(1023) As Byte
        Dim i As Integer = 0
        Dim wByte(1023) As Byte
        Dim R_str As String = ""
        Dim Temp_str As String = ""
        Dim Car_pic As PictureBox = New PictureBox
        Dim stste As Car_point = New Car_point(100, 100, Car_pic)
        Dim status(50) As Byte
        Dim read_int(1) As Integer
        Dim flag As Boolean = False
        Dim idx As Integer = 0
        Dim this_thread_idx As String = ""
        Dim bms1(15), bms2(15) As Integer
        read_int(0) = 0
        read_int(1) = 0
        stste.econ_Socket = socket_Client
        stste.econ_stream = New NetworkStream(socket_Client)
        stste.flag = False
        stste.econ_stream.ReadTimeout = 400
        stste.econ_stream.WriteTimeout = 400
        '搜尋離線的車
        For i = 0 To car_no
            If Car(i).status = -2 And Car(i).device_no > 0 Then

                settext("讀取" + Car(i).device_no.ToString + "號車" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString(), True)
                If modbus_read(stste.econ_stream, Car(i).device_no, 200, 1, read_int) Then
                    stste.flag = True
                    stste.connected = True
                    stste.device_no = Car(i).device_no
                    stste.device_status = Car(i).device_status
                    stste.Car_type = Car(i).Car_type
                    stste.Chrage_Point = Car(i).Chrage_Point
                    stste.wait_point = Car(i).wait_point
                    stste.Block_Point = Car(i).Block_Point
                    stste.RollData = Car(i).RollData
                    stste.Chrage_volt = Car(i).Chrage_volt
                    stste.SafeSensor = Car(i).SafeSensor
                    stste.Compass = Car(i).Compass

                    stste.AutoCharge = Car(i).AutoCharge
                    stste.AutoChargeVal = Car(i).AutoChargeVal
                    stste.Site = Car(i).Site
                    stste.Recharge_SOC = Car(i).Recharge_SOC
                    stste.Recharge_Point = Car(i).Recharge_Point
                    stste.Lock_user = Car(i).Lock_user
                    stste.slam = Car(i).slam
                    ' MsgBox(i)
                    stste.step_i = 999

                    stste.ipadress = IPAddress.Parse(CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString()).ToString
                    modbus_read(stste.econ_stream, Car(i).device_no, 3053, 16, bms1)
                    Thread.Sleep(1000)
                    modbus_read(stste.econ_stream, Car(i).device_no, 3153, 16, bms2)
                    Thread.Sleep(1000)
                    modbus_read(stste.econ_stream, Car(i).device_no, 3053, 16, bms1)
                    stste.BMS_fw = int2bytestr(bms1, 0, 16) + " " + int2bytestr(bms2, 0, 16)
                    settext(Car(i).device_no.ToString + "號車，電池版本" + stste.BMS_fw)
                    settext("AGV" + stste.device_no.ToString + " BMS1:" + int2str(bms1, 0, 16))
                    settext("AGV" + stste.device_no.ToString + " BMS2:" + int2str(bms2, 0, 16))

                    If Car(i).flag = True Then
                        '斷線重連  
                        Car(i).econ_stream = stste.econ_stream
                        Car(i).econ_Socket = stste.econ_Socket
                        Car(i).ipadress = stste.ipadress
                        settext(stste.device_no.ToString + "號車斷線重連" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString(), True)
                    Else
                        '異常復歸或是初次上線  
                        settext(stste.device_no.ToString + "號車上線" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString(), True)
                        stste.step_i = 999
                        ' stste.set_tagid(Tag_ID_List)
                        stste.online = Car(i).online
                        stste.Car_Picbox = Car(i).Car_Picbox
                        Car(i) = stste


                    End If
                    Car(i).bat_sn(0) = int2bytestr(bms1, 0, 16)
                    Car(i).bat_sn(1) = int2bytestr(bms2, 0, 16)
                    ReDim Car(i).BMS1(52)
                    ReDim Car(i).BMS2(52)
                    ReDim Car(i).BMS3(52)
                    Car(i).BMS_fw = stste.BMS_fw
                    Car(i).Read_Err_Count = 0
                    Car(i).thread_idx = Now.ToString("yyyyMMddHHmmssfff")
                    Car(i).Misstagid_Flag = False
                    Car(i).Tag_Point_dict = Tag_point_Dictionary

                    this_thread_idx = Car(i).thread_idx
                    settext("thread_idx=" + this_thread_idx)

                    idx = i
                    flag = True
                    Exit For
                Else
                    settext("讀取" + Car(i).device_no.ToString + "號車 未回應" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString(), True)
                End If
            End If

        Next
        '全部車子搜尋
        If flag = False Then
            settext("已上線車輛讀取失敗，全部車輛重新讀取" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString(), True)

            For i = 0 To car_no
                If Car(i).device_no > 0 Then

                    settext("讀取" + Car(i).device_no.ToString + "號車" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString(), True)
                    If modbus_read(stste.econ_stream, Car(i).device_no, 200, 1, read_int) Then
                        stste.flag = True
                        stste.connected = True
                        stste.device_no = Car(i).device_no
                        stste.Car_type = Car(i).Car_type
                        stste.Chrage_Point = Car(i).Chrage_Point
                        stste.wait_point = Car(i).wait_point
                        stste.Block_Point = Car(i).Block_Point
                        stste.RollData = Car(i).RollData
                        stste.Chrage_volt = Car(i).Chrage_volt
                        stste.SafeSensor = Car(i).SafeSensor
                        stste.Compass = Car(i).Compass

                        stste.AutoCharge = Car(i).AutoCharge
                        stste.AutoChargeVal = Car(i).AutoChargeVal
                        stste.Site = Car(i).Site
                        stste.Recharge_SOC = Car(i).Recharge_SOC
                        stste.Recharge_Point = Car(i).Recharge_Point
                        stste.Lock_user = Car(i).Lock_user
                        stste.slam = Car(i).slam
                        ' MsgBox(i)
                        stste.step_i = 999
                        modbus_read(stste.econ_stream, Car(i).device_no, 3053, 16, bms1)
                        Thread.Sleep(1000)
                        modbus_read(stste.econ_stream, Car(i).device_no, 3153, 16, bms2)
                        Thread.Sleep(1000)
                        modbus_read(stste.econ_stream, Car(i).device_no, 3053, 16, bms1)
                        stste.BMS_fw = int2bytestr(bms1, 0, 16) + " " + int2bytestr(bms2, 0, 16)
                        settext("AGV" + stste.device_no.ToString + " BMS1:" + int2str(bms1, 0, 16))
                        settext("AGV" + stste.device_no.ToString + " BMS2:" + int2str(bms2, 0, 16))
                        settext(Car(i).device_no.ToString + "號車，電池版本" + stste.BMS_fw)
                        stste.ipadress = IPAddress.Parse(CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString()).ToString
                        If Car(i).flag = True Then
                            '斷線重連  
                            Car(i).econ_stream = stste.econ_stream
                            Car(i).econ_Socket = stste.econ_Socket
                            Car(i).ipadress = stste.ipadress
                            settext(stste.device_no.ToString + "號車斷線重連" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString(), True)
                        Else
                            '異常復歸或是初次上線  
                            settext(stste.device_no.ToString + "號車上線" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString(), True)
                            stste.step_i = 999
                            ' stste.set_tagid(Tag_ID_List)
                            stste.online = Car(i).online
                            stste.Car_Picbox = Car(i).Car_Picbox
                            Car(i) = stste
                        End If
                        Car(i).bat_sn(0) = int2bytestr(bms1, 0, 16)
                        Car(i).bat_sn(1) = int2bytestr(bms2, 0, 16)
                        ReDim Car(i).BMS1(52)
                        ReDim Car(i).BMS2(52)
                        ReDim Car(i).BMS3(52)
                        Car(i).BMS_fw = stste.BMS_fw
                        Car(i).Read_Err_Count = 0
                        Car(i).thread_idx = Now.ToString("yyyyMMddHHmmssfff")
                        Car(i).Misstagid_Flag = False
                        Car(i).Tag_Point_dict = Tag_point_Dictionary

                        this_thread_idx = Car(i).thread_idx
                        settext("thread_idx=" + this_thread_idx)
                        idx = i
                        flag = True
                        Exit For
                    Else
                        settext("讀取" + Car(i).device_no.ToString + "號車 未回應" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString(), True)
                    End If
                End If
            Next
        End If
        If flag = True Then
            Car_dowork(Car(idx), Mysql_str)
            Dim oConn As MySqlConnection
            Dim sqlCommand As New MySqlCommand
            Dim Query As String = ""
            ''---------------------上線紀錄
            Try
                oConn = New MySqlConnection(Mysql_str)
                oConn.Open()
                Try
                    sqlCommand.Connection = oConn
                    Query = "INSERT INTO `agv_event` (`Car_No` ,`Event` ,`Event_Time` ,`Tag_ID` ,`IP_Addr`,cmd_idx,Battery) VALUES ('" + Car(idx).device_no.ToString + "', 'ONLINE', now(), " + Car(idx).get_tagId.ToString + ", '" + Car(idx).ipadress.ToString + "', " + Car(idx).cmd_sql_idx.ToString + ",'" + Car(idx).BMS_fw.ToString + "');"
                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()

                    Query = "UPDATE  `agv`.`agv_list` SET  `bat_SN1` =  '" + Car(idx).bat_sn(0) + "' , `bat_SN2` =  '" + Car(idx).bat_sn(1) + "'  WHERE  `agv_list`.`AGVNo` =  '" + Car(idx).device_no.ToString + "';"
                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()

                Catch ex As Exception

                End Try

                oConn.Close()
                oConn.Dispose()
            Catch ex As Exception

            End Try
            ''---------------------上線紀錄
            While Car(idx).Read_Err_Count < 100
                If this_thread_idx = Car(idx).thread_idx Then


                    Dim doworklog As String = ""

                    doworklog = Car_dowork(Car(idx), Mysql_str)
                    If Not doworklog = "" Then
                        settext(Car(idx).device_no.ToString + ":" + doworklog, True, Car(idx).device_no)
                    End If

                    Thread.Sleep(200)
                Else
                    settext("thread_idx=:" + this_thread_idx.ToString + "->" + Car(idx).thread_idx)
                    Try
                        settext("結束執行緒:" + stste.device_no.ToString + ":" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString())
                        Client_socket.Close()

                    Catch ex As Exception

                    End Try
                    Exit While
                End If
            End While
            settext("結束執行緒END")

        Else
            Try
                stste.econ_stream.Close()
                stste.econ_Socket.Close()
                settext("未找到設備 關閉連線:" + stste.device_no.ToString + ":" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString())
            Catch ex As Exception

            End Try
        End If
    End Sub
    Dim car_comm_idx As Integer = 0
    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MainBG_timer.Tick

        TextBox5.Text = BackString
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim Query = ""
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn

        Me.txtFrom.Text = Car(view_car_idx).from_pos.ToString
        Me.txtTo.Text = Car(view_car_idx).To_pos.ToString
        txtTemp_to.Text = Car(view_car_idx).To_temp_pos.ToString
        Dim i As Integer


    
      
        ' 取得資料X,Y
        For i = 0 To car_no
            If Car(i).flag Then
                status_log(i)
            End If
        Next


        If Car(view_car_idx).flag = True Then

            For i = 0 To 25
                Dim TextBox As TextBox = Me.Controls.Find("Econ_" + i.ToString(), True)(0)
                If Not Car(view_car_idx).device_status Is Nothing Then
                    If Car(view_car_idx).device_status(i) > 42000 Then
                        TextBox.Text = (Car(view_car_idx).device_status(i) - 65536).ToString
                    Else
                        TextBox.Text = Car(view_car_idx).device_status(i).ToString
                    End If

                End If
            Next
            For i = 0 To 20
                Dim TextBox As TextBox = Me.Controls.Find("txtToAGV" + i.ToString(), True)(0)
                If Not Car(view_car_idx).To_AGV Is Nothing Then
                    TextBox.Text = Car(view_car_idx).To_AGV(i).ToString
                End If
            Next

            If Car(view_car_idx).BMS1(7) > 1200 And Car(view_car_idx).BMS1(7) < 7000 Then


                TextBox4.Text = ""
                For i = 0 To 52
                    TextBox4.Text += Car(view_car_idx).BMS1(i).ToString + " "
                    If i Mod 9 = 0 Then
                        TextBox4.Text += vbCrLf
                    End If

                Next
                TextBox4.Text += vbCrLf
                TextBox4.Text += "SOC:" + Car(view_car_idx).BMS1(14).ToString.ToString
                TextBox4.Text += "電壓:" + Car(view_car_idx).BMS1(7).ToString.ToString
                If Car(view_car_idx).BMS1(8) > 32765 Then
                    TextBox4.Text += "放電:" + (65535 - Car(view_car_idx).BMS1(8)).ToString.ToString
                Else
                    TextBox4.Text += "充電:" + Car(view_car_idx).BMS1(8).ToString.ToString
                End If
                TextBox4.Text += "溫度:" + Car(view_car_idx).BMS1(10).ToString.ToString + "," + Car(view_car_idx).BMS1(11).ToString.ToString + "," + Car(view_car_idx).BMS1(12).ToString.ToString + vbCrLf

                'If Not Car(view_car_idx).device_status Is Nothing Then
                '    Econ_20.Text = Err_code(Car(view_car_idx).device_status(20))
                'End If
            End If
            If Car(view_car_idx).BMS2(7) > 1200 And Car(view_car_idx).BMS2(7) < 7000 Then
                For i = 0 To 52
                    TextBox4.Text += Car(view_car_idx).BMS2(i).ToString + " "
                    If i Mod 9 = 0 Then
                        TextBox4.Text += vbCrLf
                    End If

                Next
                TextBox4.Text += vbCrLf
                TextBox4.Text += "SOC:" + Car(view_car_idx).BMS2(14).ToString.ToString

                TextBox4.Text += "電壓:" + Car(view_car_idx).BMS2(7).ToString.ToString
                If Car(view_car_idx).BMS2(8) > 32765 Then
                    TextBox4.Text += "放電:" + (65535 - Car(view_car_idx).BMS2(8)).ToString.ToString
                Else
                    TextBox4.Text += "充電:" + Car(view_car_idx).BMS2(8).ToString.ToString
                End If
                TextBox4.Text += "溫度:" + Car(view_car_idx).BMS2(10).ToString.ToString + "," + Car(view_car_idx).BMS2(11).ToString.ToString + "," + Car(view_car_idx).BMS2(12).ToString.ToString + vbCrLf
            End If
            'If Not Car(view_car_idx).device_status Is Nothing Then
            '    Econ_20.Text = Err_code(Car(view_car_idx).device_status(20))
            'End If
        End If

        'Me.AxActEasyIF1.State
        For j As Integer = 0 To car_no
            If Not Car(j).get_tagId = 0 Then
                If Tag_point_Dictionary.ContainsKey(Car(j).get_tagId Mod 10000) Then
                    Car(j).Car_Picbox.Left = Tag_point_Dictionary(Car(j).get_tagId Mod 10000).X - 15 - offset_x
                    Car(j).Car_Picbox.Top = Tag_point_Dictionary(Car(j).get_tagId Mod 10000).Y - 15 - offset_y
                End If
                If Car(j).Car_type = "PIN" Then
                    If Car(j).get_Shelf_Car_No > 0 And Agvc_shelfcheck.Checked = True Then
                        For i = 0 To shelf_car.Length - 1
                            If shelf_car(i).Shelf_Car_No = Car(j).get_Shelf_Car_No Then
                                shelf_car(i).LOCATION = Car(j).get_tagId
                                shelf_car(i).car.Left = Car(j).Car_Picbox.Left
                                shelf_car(i).car.Top = Car(j).Car_Picbox.Top + 15
                            End If
                        Next
                    End If
                ElseIf Car(j).Car_type = "ROLL" Then

                    If Car(j).Shelf_Car_No > 0 Then
                        For i = 0 To shelf_car.Length - 1
                            If shelf_car(i).Shelf_Car_No = Car(j).get_Shelf_Car_No Then
                                shelf_car(i).LOCATION = Car(j).get_tagId
                                shelf_car(i).car.Left = Car(j).Car_Picbox.Left
                                shelf_car(i).car.Top = Car(j).Car_Picbox.Top + 15
                            End If
                        Next
                    End If
                End If

            End If
        Next
        For i = 0 To Door_List.Length - 1
            If Tag_point_Dictionary.ContainsKey(Door_List(i).tagid) And Door_List(i).tagid > 0 Then
                Door_List(i).Door_Pic.Left = Tag_point_Dictionary(Door_List(i).tagid).X - 15 - offset_x
                Door_List(i).Door_Pic.Top = Tag_point_Dictionary(Door_List(i).tagid).Y - 15 - offset_y
            End If
        Next
        For i = 0 To LFT_List.Length - 1
            If Tag_point_Dictionary.ContainsKey(LFT_List(i).tagid) And LFT_List(i).tagid > 0 Then
                LFT_List(i).LFT_Pic.Left = Tag_point_Dictionary(LFT_List(i).tagid).X - 15 - offset_x
                LFT_List(i).LFT_Pic.Top = Tag_point_Dictionary(LFT_List(i).tagid).Y - 15 - offset_y
            End If
        Next
        'OFFLINE 灰
        'OK 綠
        'ERROR 紅
        'RUN 黃
        For i = 0 To car_no
            Dim load_str As String = ""
            If Car(i).get_loading = 3 And (Car(i).get_pin = 2 Or Car(i).get_pin = 6 Or Car(i).get_pin = 10 Or Car(i).Car_type = "LFT") Then
                load_str = "_load"
            End If
            If (Car(i).flag = False Or Car(i).status = -2) Then
                Car(i).Car_Picbox.Image = Image.FromFile("gray" + load_str + ".png")
                Car(i).status = -2
            ElseIf Car(i).status = 3 Then
                '手動
                Car(i).Car_Picbox.Image = Image.FromFile("gray" + load_str + ".png")
                Car(i).subcmd = Car(i).get_tagId.ToString

            ElseIf (Car(i).status = 2 Or Car(i).status = 0) Then
                Car(i).Car_Picbox.Image = Image.FromFile("green" + load_str + ".png")
            ElseIf (Car(i).status = 4 Or Car(i).status = 1) Then
                Car(i).Car_Picbox.Image = Image.FromFile("yellow" + load_str + ".png")
            ElseIf (Car(i).status = -1) Then
                Car(i).Car_Picbox.Image = Image.FromFile("red" + load_str + ".png")
            End If

            Dim bmp1 = New Bitmap(Car(i).Car_Picbox.Width, Car(i).Car_Picbox.Height)   '產生圖像
            bmp1 = Car(i).Car_Picbox.Image
            Dim g1 = Graphics.FromImage(bmp1) '產生畫布
            Dim mycolor1 As New SolidBrush(Color.FromArgb(255, 255, 255))   '定義字體顏色
            g1.DrawString(Car(i).device_no.ToString, New Font("Microsoft JhengHei", 12, FontStyle.Regular), mycolor1, 5, 7) '畫布寫上字串
            Car(i).Car_Picbox.BackgroundImage = bmp1    'PictureBox1.Image指定該圖像


            'If Car(i).Read_Err_Count > 1000 Then
            '    Car(i).flag = False
            'End If
            If Not Car(i).get_Err = Car(i).Pre_Error_Code Then
                Car(i).Pre_Error_Code = Car(i).get_Err
                If Car(i).get_Err > 0 Then


                    Try
                        Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`) "
                        Query += "VALUES ('" + Car(i).cmd_sql_idx.ToString + "', now() ,'', '" + Car(i).get_Err.ToString + "', '" + Car(i).device_no.ToString + "', '', '', '" + Car(i).get_tagId.ToString + "', '', '" + Car(i).Shelf_Car_No.ToString + "') ;"
                        sqlCommand.CommandText = Query
                        sqlCommand.ExecuteNonQuery()
                        settext(Query)
                    Catch ex As Exception
                        settext(Query + ex.Message)
                    End Try
                Else
                    Try
                        Query = "update  `alarm` set  `CLEAR_DATE`=now() where  ALM_DEVICE='" + Car(i).device_no.ToString + "' and CLEAR_DATE < '2019-01-01' "
                        sqlCommand.CommandText = Query
                        sqlCommand.ExecuteNonQuery()
                        settext(Query)
                    Catch ex As Exception
                        settext(Query + ex.Message)
                    End Try
                End If

            End If
        Next


        If Car(view_car_idx).get_Err > 0 Then
            Me.Err_lb.Text = Car(view_car_idx).get_Err.ToString + vbCrLf + alarm.Query_ALM_TXT(Car(view_car_idx).get_Err, Car(view_car_idx).Car_type)
        Else
            Me.Err_lb.Text = ""
        End If
        oConn.Close()
        oConn.Dispose()
        '
        'Dim a As Image = Image.FromFile("shelf.png")

    End Sub

    Private Sub Form1_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        If MessageBox.Show("確定關閉視窗?", "警告", MessageBoxButtons.YesNo) = MsgBoxResult.No Then

            e.Cancel = True

        End If
    End Sub



  

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        'Dim cf1 As Configuration = ConfigurationManager.OpenExeConfiguration(ConfigurationUserLevel.None)

       
        Dim xdoc As XmlDocument = New XmlDocument
        Dim a As StreamReader = New StreamReader("setting.ini")
        Dim str As String = a.ReadToEnd
        '判斷資料夾有無存在
        If Not Directory.Exists("log") Then
            Directory.CreateDirectory("log")
        End If
        a.Close()
        Try
            xdoc.LoadXml(str)

            Me.Agvc_shelfcheck.Checked = CBool(xdoc.GetElementsByTagName("ShelfCar_Check").Item(0).InnerXml)
            Me.Loading_Check.Checked = CBool(xdoc.GetElementsByTagName("Loading_Check").Item(0).InnerXml)
            Me.ALL_Loading_check.Checked = CBool(xdoc.GetElementsByTagName("ALL_Loading_check").Item(0).InnerXml)
            Me.IR_check.Checked = CBool(xdoc.GetElementsByTagName("IR_sensor").Item(0).InnerXml)

            Me.chrage_exception.Text = xdoc.GetElementsByTagName("chrage_exception").Item(0).InnerXml '不充電清單
            Me.MyDB_txt.Text = xdoc.GetElementsByTagName("MyDB").Item(0).InnerXml '
            Me.IP.Text = xdoc.GetElementsByTagName("IP").Item(0).InnerXml
            Me.LPort.Text = xdoc.GetElementsByTagName("LPort").Item(0).InnerXml
            MyDb = Me.MyDB_txt.Text
            IP_adress = Me.IP.Text
            user_txt.Text = xdoc.GetElementsByTagName("user").Item(0).InnerXml
            password_txt.Text = xdoc.GetElementsByTagName("password").Item(0).InnerXml
            user = user_txt.Text
            password = password_txt.Text


            PathLenTxt.Text = xdoc.GetElementsByTagName("PathLen").Item(0).InnerXml
            RemapLenTxt.Text = (xdoc.GetElementsByTagName("RemapLen").Item(0).InnerXml)
            PathLen = CInt(PathLenTxt.Text)
            ReMapLen = CInt(RemapLenTxt.Text)

            DebugMode.Checked = CBool(xdoc.GetElementsByTagName("DebugMode").Item(0).InnerXml)

            DoorSetLenTxt.Text = CInt((xdoc.GetElementsByTagName("DoorSetLen").Item(0).InnerXml))
            DoorSetLen = CInt(DoorSetLenTxt.Text)

            AgvTimeout.Text = CInt((xdoc.GetElementsByTagName("AgvTimeout").Item(0).InnerXml))
            AgvTimeoutVal = CInt(AgvTimeout.Text)
            RetreatPathTxt.Text = (xdoc.GetElementsByTagName("RetreatPath").Item(0).InnerXml)
            RetreatPath = CInt(RetreatPathTxt.Text)

        Catch ex As Exception
            MsgBox("讀取設定有異常，請重新存檔並重啟")
        End Try
        'MsgBox(My.Settings.MyDB)
        Mysql_str = "charset=utf8 ;Database=" + MyDB_txt.Text + "; Data Source=" + IP.Text + ";User id=" + user + ";Password=" + password + "; Allow Zero Datetime=True;"
        Load_Car_info()


        log = New StreamWriter(log_filename, True, Encoding.Default)
        Dim ListenThread As New Thread(AddressOf StartListen)
        ListenThread.IsBackground = True
        ListenThread.Start()

        Dim ListenThread1 As New Thread(AddressOf StartListen1)
        ListenThread1.IsBackground = True
        ListenThread1.Start()

        Dim ListenThread2 As New Thread(AddressOf ListenCmd)
        ListenThread2.IsBackground = True
        ListenThread2.Start()

        Dim i As Integer
        Dim tagid_len As Integer
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        oConn = New MySqlConnection(Mysql_str)
        Dim Query As String = "SELECT Tag_ID,X,Y,Retreat_Flag,Tag_name,floor,floor_no,th,slam FROM `point` where Tag_ID between 0 and 19999  order by Tag_ID ASC"
        Dim mReader As MySqlDataReader
        oConn.Open()
        sqlCommand.Connection = oConn
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        Tag_point_Dictionary.Clear()
        While (mReader.Read)
            Tag_ID_List(i) = CInt(mReader.Item(0))
            Tag_point_list(i).TagId = CInt(mReader.Item(0))
            Tag_point_list(i).X = CInt(mReader.Item(1))
            Tag_point_list(i).Y = CInt(mReader.Item(2))
            Tag_point_list(i).th = CInt(mReader.Item(7))
            Tag_point_list(i).Retreat_Flag = CInt(mReader.Item(3))
            Tag_point_list(i).name = mReader.Item(4).ToString
            Tag_point_list(i).floor = mReader.Item(5).ToString
            Tag_point_list(i).floor_no = CInt(mReader.Item(6))
            Tag_point_list(i).slam = CInt(mReader.Item(8))
            Tag_point_Dictionary.Add(Tag_point_list(i).TagId, Tag_point_list(i))
            i += 1
            tagid_len = i
        End While
        mReader.Close()
        Query = "update  `alarm` set  `CLEAR_DATE`=now() where   CLEAR_DATE < '2019-01-01' "
        sqlCommand.CommandText = Query
        sqlCommand.ExecuteNonQuery()
        Array.Resize(Tag_point_list, tagid_len)


   

        For j As Integer = 0 To tagid_len - 1
            Tag_ID_Fork_List(j) = Tag_ID_List(j)
            Tag_ID_Fork_List(j + tagid_len) = Tag_ID_List(j) + 10000
        Next
        Array.Resize(Tag_ID_Fork_List, tagid_len * 2)
        Array.Resize(Tag_ID_List, tagid_len)

        Load_Path_base()
        Load_Path_fork_base()


        For k As Integer = 0 To Dijkstra_list.Length - 1
            If Dijkstra_list(k).CarType = "9" Then
                'FORK
                Load_Path_fork(Dijkstra_list(k).name, Tag_ID_Fork_List, Dijkstra_list(k).ary) '載入路徑資料
            Else
                Load_Path(Dijkstra_list(k).name, Tag_ID_List, Dijkstra_list(k).ary) '載入路徑資料
            End If

            car_type.Items.Add(Dijkstra_list(k).name)
        Next

        ' Load_Shelf_Car() '載入路徑資料

        MainBG_timer.Start()
        For i = 0 To Tag_point_list.Length - 1
            Me.From_cb.Items.Add(Tag_point_list(i).TagId)
            Me.To_cb.Items.Add(Tag_point_list(i).TagId)
        Next
        ' cmd 顯示命令
        ListView1_ReNew()
        ListView1.ContextMenuStrip = ContextMenuStrip1
        'Me.Car1.ContextMenuStrip = ContextMenuStrip2
        Me.cmd_timer.Start()

        alarm.init()
        Query = " SELECT ErrorCode,Description,CarType FROM `error_code`"
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        alarm.ALM_ID(0) = 0
        alarm.ALM_TXT(0) = "未知異常"
        alarm.ALM_RPT_ID(0) = 3
        alarm.ALM_ENG_TXT(0) = "unknow"
        i = 1

        While mReader.Read
            alarm.ALM_ID(i) = CInt(mReader.Item(0))
            alarm.ALM_TXT(i) = mReader.Item(1).ToString
            alarm.CarType(i) = mReader.Item(2)
            alarm.ALM_ENG_TXT(i) = mReader.Item(1).ToString
            i += 1
        End While


        Array.Resize(alarm.ALM_ID, i)
        Array.Resize(alarm.ALM_TXT, i)
        Array.Resize(alarm.ALM_RPT_ID, i)
        Array.Resize(alarm.ALM_ENG_TXT, i)

        oConn.Close()
        oConn.Dispose()
        If DebugMode.Checked = False Then
            door_check_timer.Start()
            LFT_timer.Start()
            Load_Door_Data()
            EQ_Timer.Start()
        End If
        VerLog.Text += "3.128 重新載入地圖" + vbCrLf
        VerLog.Text += "3.102 修正陀螺儀機型" + vbCrLf
        VerLog.Text += "3.99 新增POWER2車型" + vbCrLf
        VerLog.Text += "3.96 修正LOWCAR的AGVC重啟問題" + vbCrLf
 
        ' Door_List(0).connect()
    End Sub
    Sub Load_Car_info()
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim Query As String = ""
        Dim mReader As MySqlDataReader
        ' Dim path_i As Integer = 0
        Dim i As Integer

        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        ' Query = "SELECT AGVNo,Position,Loading,owner,Car_type,Chrage_Point,block_point,wait_point,Chrage_volt,SafeSensor,Compass FROM `agv_list` where flag=1 order by AGVNo"
        Query = "SELECT AGVNo,Position,Loading,owner,Car_type,Chrage_Point,block_point,wait_point,Chrage_volt,SafeSensor,Compass,AutoCharge,AutoChargeVal,car_site,Recharge_SOC,Recharge_Point,lock_user,slam FROM `agv_list` where flag=1 order by AGVNo"
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        For i = 0 To car_no
            Dim pic As PictureBox = Me.Controls.Find("Car" + i.ToString(), True)(0)
            pic.ContextMenuStrip = ContextMenuStrip2
            Car(i) = New Car_point(300 + 25 * i, 430, pic)
            Car(i).status = 0
            If (mReader.Read) Then
                Try
                    ' MsgBox(mReader.Item(0).ToString)
                    Car(i).device_no = CInt(mReader.Item(0))
                    Car(i).set_tagId(CInt(mReader.Item(1))) '初始值
                    Car(i).set_loading(CInt(mReader.Item(2))) '初始值

                    Car(i).Car_type = mReader.Item(4).ToString
                    Car(i).Chrage_Point = CInt(mReader.Item(5))
                    Car(i).wait_point = CInt(mReader.Item(7))
                    Car(i).Chrage_volt = CInt(mReader.Item(8))
                    ' MsgBox(Car(i).device_no)
                    txtCar.Items.Add(Car(i).device_no)
                    txtCar.Text = txtCar.Items(0).ToString
                    Car(i).Block_Point = mReader.Item(6).ToString
                    Car(i).SafeSensor = CInt(mReader.Item(9))
                    Car(i).Compass = CInt(mReader.Item(10))
                    Car(i).AutoCharge = CInt(mReader.Item(11))
                    Car(i).AutoChargeVal = CInt(mReader.Item(12))
                    Car(i).Site = (mReader.Item(13)).ToString
                    Car(i).Recharge_SOC = CInt(mReader.Item(14))
                    Car(i).Recharge_Point = (mReader.Item(15)).ToString
                    Car(i).Lock_user = (mReader.Item(16)).ToString
                    Car(i).slam = CInt(mReader.Item(17))
                Catch ex As Exception
                    MsgBox(Car(i).device_no.ToString + "設定錯誤")
                End Try

            Else
                Car(i).device_no = 0
                Car(i) = New Car_point(0, 430, pic)
                pic.Hide()

            End If
        Next

        mReader.Close()

        Query = " select A.IPAdr,A.IPPort,A.StartDI,A.Adr,B.tag_id,B.X+A.AXIS_X ,B.Y+AXIS_Y,PORT_ID  FROM `charger` A left join point B  on A.Tag_ID=B.Tag_ID  WHERE 1 group by IPAdr,IPPort,StartDI"
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        While mReader.Read
            ChargerClient(i).init(mReader.Item(0).ToString, CInt(mReader.Item(1)), CInt(mReader.Item(2)))
            ChargerClient(i).HoldingReg = CInt(mReader.Item(3))
            ChargerClient(i).tag_id = CInt(mReader.Item(4))
            ChargerClient(i).AXIS_X = CInt(mReader.Item(5))
            ChargerClient(i).AXIS_Y = CInt(mReader.Item(6))
            ChargerClient(i).EQ_ID = mReader.Item(7).ToString
            i += 1
        End While
        Array.Resize(ChargerClient, i)
        mReader.Close()
        oConn.Close()
        oConn.Dispose()

    End Sub
    Sub Load_Door_Data()
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim Query As String = ""
        Dim mReader As MySqlDataReader
        ' Dim path_i As Integer = 0
        Dim i As Integer
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        Query = "SELECT A.EQ_ID,A.Door_type,A.Tagid,A.StartDO,A.StartDI,A.IP,A.port,B.X,B.Y FROM `door` A left join  `point` B on A.Tagid=B.Tag_ID where A.flag=1"
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        For i = 0 To Door_List.Length - 1
            If (mReader.Read) Then
                ' MsgBox(mReader.Item(0).ToString)
                Door_List(i) = New Door_point(True)
                Door_List(i).EQ_ID = mReader.Item(0).ToString
                Door_List(i).Door_type = mReader.Item(1).ToString
                Door_List(i).tagid = CInt(mReader.Item(2))
                Door_List(i).startDO = CInt(mReader.Item(3))
                Door_List(i).startDI = CInt(mReader.Item(4))
                Door_List(i).ipadress = mReader.Item(5).ToString
                Door_List(i).port = CInt(mReader.Item(6))
                Door_List(i).Door_Pic = Me.Controls.Find("Door_" + i.ToString(), True)(0)
                Door_List(i).Door_Pic.Text = Door_List(i).EQ_ID

                Try
                    Door_List(i).Door_Pic.Left = CInt(mReader.Item(7)) - offset_x
                    Door_List(i).Door_Pic.Top = CInt(mReader.Item(8)) - offset_y
                Catch ex As Exception
                    Door_List(i).Door_Pic.Left = offset_x
                    Door_List(i).Door_Pic.Top = offset_y
                End Try

                If Door_List(i).connect() Then
                    settext(i.ToString + "->" + Door_List(i).EQ_ID + ":door OK")
                    ' MsgBox(Door_List(i).Door_Pic.Text + "OK")
                Else
                    settext(i.ToString + "->" + Door_List(i).EQ_ID + ":door NG")
                    ' MsgBox(Door_List(i).Door_Pic.Text + "NG")
                End If
            Else
                '無資料設為false

                Door_List(i) = New Door_point(False)

                Door_List(i).Door_Pic = Me.Controls.Find("Door_" + i.ToString(), True)(0)
                Door_List(i).Door_Pic.Left = 10
                Door_List(i).Door_Pic.Top = 10
                Door_List(i).Door_Pic.BackColor = Color.Gray
            End If
        Next
        mReader.Close()

        Query = "SELECT A.EQ_ID,A.Door_type,A.Tagid,A.StartDO,A.StartDI,A.IP,A.port,B.X,B.Y FROM `LFT` A left join  `point` B on A.Tagid=B.Tag_ID where A.flag=1"
        sqlCommand.CommandText = Query
       
        mReader = sqlCommand.ExecuteReader()
        i = 0
        For i = 0 To LFT_List.Length - 1
            If (mReader.Read) Then
                ' MsgBox(mReader.Item(0).ToString)
                LFT_List(i).init(True)
                LFT_List(i).EQ_ID = mReader.Item(0).ToString
                LFT_List(i).tagid = CInt(mReader.Item(2))
                LFT_List(i).startDO = CInt(mReader.Item(3))
                LFT_List(i).startDI = CInt(mReader.Item(4))
                LFT_List(i).ipadress = mReader.Item(5).ToString
                LFT_List(i).port = CInt(mReader.Item(6))
                LFT_List(i).LFT_Pic = Me.Controls.Find("LFT" + i.ToString(), True)(0)
                LFT_List(i).LFT_Pic.Text = LFT_List(i).EQ_ID
                LFT_List(i).LFT_Pic.Left = CInt(mReader.Item(7)) - offset_x
                LFT_List(i).LFT_Pic.Top = CInt(mReader.Item(8)) - offset_y
                If LFT_List(i).connect() Then
                    settext(i.ToString + "->" + LFT_List(i).EQ_ID + ":LFT OK")
                    ' MsgBox(Door_List(i).Door_Pic.Text + "OK")
                Else
                    settext(i.ToString + "->" + LFT_List(i).EQ_ID + ":LFT NG")
                    ' MsgBox(Door_List(i).Door_Pic.Text + "NG")
                End If
            Else
                '無資料設為false
                LFT_List(i).init(False)
                LFT_List(i).LFT_Pic = Me.Controls.Find("LFT" + i.ToString(), True)(0)
                LFT_List(i).LFT_Pic.Left = 10
                LFT_List(i).LFT_Pic.Top = 10
                LFT_List(i).LFT_Pic.BackColor = Color.Gray
            End If
            LFT_List(i).IR_sensor = Me.IR_check.Checked
        Next


        mReader.Close()
        oConn.Close()
    End Sub
    Dim cmd_timer_IsBusy As Boolean = False

    Private Sub cmd_timer_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmd_timer.Tick
        '更新資料庫()
        Dim i As Integer = 0
        Dim Query As String = ""
        Dim temptagid(Tag_ID_List.Length - 1) As Integer
        For i = 0 To Tag_ID_List.Length - 1
            temptagid(i) = Tag_ID_List(i)
        Next
        Me.car_info.Text = "select_idx:" + Me.view_car_idx.ToString
        Me.car_info.Text += " " + Car(view_car_idx).get_info
        Dim car_idx As Integer = 0
        If cmd_timer_IsBusy = False Then
            settext("cmdT")
            cmd_timer_IsBusy = True

            WorkList.Text = Car(view_car_idx).subcmd
            Dim oConn As MySqlConnection
            Dim sqlCommand As New MySqlCommand


            oConn = New MySqlConnection(Mysql_str)
            oConn.Open()
            sqlCommand.Connection = oConn

            For i = 0 To car_no
                'If Car(i).flag Then

                If Car(i).cmd_idx = -2 And Not Car(i).get_status = 4 Then
                    Car(i).subcmd = Car(i).get_tagId.ToString
                ElseIf Car(i).subcmd = "" Then
                    Car(i).subcmd = Car(i).get_tagId.ToString
                Else
                    If Car(i).subcmd.IndexOf(Car(i).get_tagId.ToString + ",") > -1 Then
                        Car(i).subcmd = Car(i).subcmd.Remove(0, Car(i).subcmd.IndexOf(Car(i).get_tagId.ToString + ","))
                    End If

                End If

                If Car(i).get_auto = 1 Then
                    Car(i).path_error_count = 0
                    Car(i).Pre_TagID_time = Now()
                    For j As Integer = 6 To 20
                        Car(i).To_AGV(j) = 0
                    Next
                End If
                If Car(i).flag = True Then


                    If Car(i).device_status(23) = 0 And Car(i).device_status(24) = 0 Then
                        If Tag_point_Dictionary.ContainsKey(Car(i).get_tagId) Then
                            Car(i).AXIS_X = Tag_point_Dictionary(Car(i).get_tagId).X
                            Car(i).AXIS_Y = Tag_point_Dictionary(Car(i).get_tagId).Y
                            Car(i).AXIS_Z = Tag_point_Dictionary(Car(i).get_tagId).th

                        End If
                    Else
                        Dim offsetTh As Integer = 0
                        If Car(i).device_status(25) > 65535 / 2 Then
                            Car(i).AXIS_Z = (Car(i).device_status(25) - 65535) / 100
                        Else
                            Car(i).AXIS_Z = Car(i).device_status(25) / 100
                        End If
                        If (Car(i).ReverseXY = 0) Then
                            If Car(i).offset_X >= 0 Then

                                Car(i).AXIS_X = (Car(i).device_status(23) + Car(i).offset_X)
                            Else

                                Car(i).AXIS_X = -Car(i).offset_X - Car(i).device_status(23)
                            End If
                            If Car(i).offset_Y >= 0 Then

                                Car(i).AXIS_Y = Car(i).device_status(24) + Car(i).offset_Y
                            Else
                                Car(i).AXIS_Y = -Car(i).device_status(24) - Car(i).offset_Y
                            End If

                            Car(i).AXIS_Z = Car(i).AXIS_Z * (-1)
                        Else
                            If Car(i).offset_X >= 0 Then
                                Car(i).AXIS_X = (Car(i).device_status(24) + Car(i).offset_X)
                            Else
                                Car(i).AXIS_X = -Car(i).device_status(24) - Car(i).offset_X
                            End If
                            If Car(i).offset_Y >= 0 Then
                                Car(i).AXIS_Y = Car(i).device_status(23) + Car(i).offset_Y
                            Else
                                Car(i).AXIS_Y = -Car(i).device_status(23) - Car(i).offset_Y
                            End If
                            offsetTh = -90
                            Car(i).AXIS_Z += offsetTh
                        End If
                    End If
                End If
                If Car(i).get_auto = 0 And Car(i).get_status = 0 And Car(i).Lock_user = "" Then
                    If Car(i).Recharge_Point = Car(i).get_tagId().ToString And (BmsWarmIdx And Car(i).BMS1(16)) Then
                        Car(i).To_AGV(20) = 30000 + InttoBitidx(Car(i).BMS1(16))
                    End If
                    If Car(i).Recharge_Point = Car(i).get_tagId.ToString And (BmsWarmIdx And Car(i).BMS2(16)) Then
                        Car(i).To_AGV(20) = 30100 + InttoBitidx(Car(i).BMS2(16))
                    End If
                End If
                ' End If
            Next
            '---------------排序
            Dim temp0 As String = ""
            Dim temp1 As String = ""
            Dim car_idx_list(car_no, 1) As String
            For car_idx = 0 To car_no
                car_idx_list(car_idx, 0) = car_idx.ToString
                If Car(car_idx).RequestTime = "" Then
                    car_idx_list(car_idx, 1) = "9999-99-99 00:00:00"
                Else
                    car_idx_list(car_idx, 1) = Car(car_idx).RequestTime

                End If
            Next
            For i = 0 To car_no - 1
                For j As Integer = i + 1 To car_no
                    If String.Compare(car_idx_list(i, 1), car_idx_list(j, 1)) > 0 Then
                        temp0 = car_idx_list(i, 0)
                        temp1 = car_idx_list(i, 1)
                        car_idx_list(i, 0) = car_idx_list(j, 0)
                        car_idx_list(i, 1) = car_idx_list(j, 1)
                        car_idx_list(j, 0) = temp0
                        car_idx_list(j, 1) = temp1
                    End If
                Next
            Next
            'Debug.Text = ""
            'For i = 0 To car_no
            'Debug.Text += car_idx_list(i, 0) + ":" + car_idx_list(i, 1) + vbCrLf
            ' Next
            '''''''''''''''
            'Reserve_list = strReserve.Split(",")
            Try

                For car_idx_i As Integer = 0 To car_no


                    car_idx = 0
                    car_idx = CInt(car_idx_list(car_idx_i, 0))

                    If (Car(car_idx).flag And Car(car_idx).online) Then
                        If Car(car_idx).get_Err > 0 Then
                            Car(car_idx).Error_time += cmd_timer.Interval / 1000
                        ElseIf Car(car_idx).get_loading = 3 And Car(car_idx).get_pin = 10 And Car(car_idx).Car_type = "PIN" Then
                            Car(car_idx).Load_time += cmd_timer.Interval / 1000
                        ElseIf Car(car_idx).get_loading = 3 And Car(car_idx).Car_type = "ROLL" Then
                            Car(car_idx).Load_time += cmd_timer.Interval / 1000
                        Else
                            Car(car_idx).empty_time += cmd_timer.Interval / 1000
                        End If

                        If Car(car_idx).get_auto = 0 And Not Car(car_idx).get_status = 4 And Car(car_idx).get_Err = 0 And Car(car_idx).step_i >= 999 Then
                            '命令執行 step_i > 999

                            If (Car(car_idx).get_status = 12 Or Car(car_idx).get_status = 8) And Not Car(car_idx).rate_point = Car(car_idx).get_tagId() Then
                                Car(car_idx).rate_point = Car(car_idx).get_tagId()
                                settext(Car(car_idx).device_no.ToString + ":設定旋轉點位" + Car(car_idx).rate_point.ToString + "，無重新規劃")
                            End If
                            If Car(car_idx).cmd_idx = -2 Then '沒有待執行命令
                                '先選擇命令
                                Car(car_idx).cmd_sql_idx = 0
                                Car(car_idx).Cmd_From = 0
                                Car(car_idx).Cmd_To = 0
                                Car(car_idx).RequestTime = ""
                                Car(car_idx).Requestor = ""
                                Car(car_idx).cmd_Shelf_Car_No = 0
                                Car(car_idx).cmd_Shelf_Car_Type = ""
                                Car(car_idx).cmd_Shelf_Car_size = ""
                                Car(car_idx).Cmd_RollData = ""
                                Car(car_idx).ext_cmd = ""
                                Car(car_idx).empty_time = 0
                                Car(car_idx).Load_time = 0
                                Car(car_idx).Error_time = 0
                                Car(car_idx).path_error_count = 0
                                Car(car_idx).path_error_tagid = 0
                                Dim select_idx As Integer = -1
                                ' Dim temp_subcmd As String = ""
                                Try
                                    For i = 0 To Me.ListView1.Items.Count - 1
                                        If Not Me.ListView1.Items(i).SubItems(5).Text.StartsWith("ERROR") And CInt(Me.ListView1.Items(i).SubItems(4).Text) > 0 And CInt(Me.ListView1.Items(i).SubItems(1).Text) = Car(car_idx).device_no And (Car(car_idx).Lock_user = "" Or Car(car_idx).Lock_user = Me.ListView1.Items(i).SubItems(7).Text Or CInt(Me.ListView1.Items(i).SubItems(3).Text) = 8) Then

                                            select_idx = i
                                            Exit For
                                        End If
                                    Next
                                Catch ex As Exception

                                End Try



                                If (((CInt(Car(car_idx).get_Volt) < Car(car_idx).Chrage_volt Or (CInt(Car(car_idx).get_SOC) <= Car(car_idx).Recharge_SOC And BMSinfoCheck(Car(car_idx).BMS1))) And Car(car_idx).Lock_user = "" And CInt(Car(car_idx).get_Volt) > 20) Or (CInt(Car(car_idx).get_Volt) = 0 And Car(car_idx).Lock_user = "" And Car(car_idx).device_status(20) = 8)) And Not Car(car_idx).get_pin = 10 And Car(car_idx).get_tagId > 0 And Car(car_idx).Chrage_Point > 0 And Not In_Subcmd(chrage_exception.Text, Car(car_idx).get_tagId().ToString) Then
                                    '強制充電
                                    ' 
                                    settext("Car" + Car(car_idx).device_no.ToString + "強制充電1")
                                    If Not (Car(car_idx).get_tagId >= Car(car_idx).Chrage_Point And Car(car_idx).get_tagId <= Car(car_idx).Chrage_Point + 4) Then
                                        settext("Car" + Car(car_idx).device_no.ToString + "強制充電2")

                                        select_idx = -1
                                        For i = 0 To Me.ListView1.Items.Count - 1
                                            If Not Me.ListView1.Items(i).SubItems(5).Text.StartsWith("ERROR") And CInt(Me.ListView1.Items(i).SubItems(3).Text) >= Car(car_idx).Chrage_Point And CInt(Me.ListView1.Items(i).SubItems(3).Text) <= Car(car_idx).Chrage_Point + 4 And CInt(Me.ListView1.Items(i).SubItems(4).Text) > 0 And CInt(Me.ListView1.Items(i).SubItems(1).Text) = Car(car_idx).device_no Then
                                                select_idx = i
                                                Exit For
                                            End If
                                        Next
                                        If select_idx = -1 Then
                                            If Car(car_idx).AutoCharge = 0 Then
                                                For change_i As Integer = 0 To 4
                                                    If Send_CMD(Car(car_idx).device_no, 4, Car(car_idx).Chrage_Point + change_i) = 1 Then
                                                        change_i = 5
                                                    End If
                                                Next
                                            Else

                                                Dim chargerlist() As String = Car(car_idx).Recharge_Point.Split(",")
                                                For change_i As Integer = 0 To chargerlist.Length - 1
                                                    Dim flag As Boolean = False
                                                    '判斷充電站有沒有車子充電
                                                    For j As Integer = 0 To Me.ListView1.Items.Count - 1
                                                        If (Me.ListView1.Items(j).SubItems(2).Text) = 4 And Me.ListView1.Items(j).SubItems(3).Text = chargerlist(change_i) Then
                                                            flag = True
                                                        End If
                                                    Next
                                                    For j As Integer = 0 To ChargerClient.Length - 1
                                                        '判斷 該充電站沒有異常或是自動狀態
                                                        If ChargerClient(i).tag_id = chargerlist(change_i) And (ChargerClient(i).HoldingResponse(19) > 0 Or ChargerClient(i).HoldingResponse(18) = 0) Then
                                                            flag = True
                                                        End If

                                                    Next

                                                    If flag = False Then
                                                        '充電站IDLE
                                                        If Send_CMD(Car(car_idx).device_no, 4, chargerlist(change_i)) Then
                                                            Exit For
                                                        End If
                                                    End If


                                                Next
                                            End If

                                        Else
                                            Car(car_idx).cmd_sql_idx = CInt(Me.ListView1.Items(select_idx).SubItems(0).Text)
                                            Car(car_idx).Cmd_From = CInt(Me.ListView1.Items(select_idx).SubItems(2).Text)
                                            Car(car_idx).Cmd_To = CInt(Me.ListView1.Items(select_idx).SubItems(3).Text)
                                            Car(car_idx).RequestTime = (Me.ListView1.Items(select_idx).SubItems(6).Text)
                                            Car(car_idx).Requestor = (Me.ListView1.Items(select_idx).SubItems(7).Text)
                                            Car(car_idx).cmd_Shelf_Car_No = CInt(Me.ListView1.Items(select_idx).SubItems(8).Text)
                                            Car(car_idx).cmd_Shelf_Car_Type = Me.ListView1.Items(select_idx).SubItems(9).Text
                                            Car(car_idx).cmd_Shelf_Car_size = Me.ListView1.Items(select_idx).SubItems(10).Text
                                            Car(car_idx).Cmd_RollData = Me.ListView1.Items(select_idx).SubItems(11).Text
                                            Car(car_idx).ext_cmd = Me.ListView1.Items(select_idx).SubItems(12).Text
                                            Car(car_idx).Sql2Cmdlist()
                                            '選擇命令
                                            If Car(car_idx).cmd_idx = 0 Then
                                                '紀錄開始執行時間
                                                Query = ""
                                                Try
                                                    Query = "INSERT INTO `agv_cmd_history` (`CmdKey`, `AGVNo`,`CmdFrom`, `CmdTo`, `RequestTime`, `Requestor`, `Start_Time`,Shelf_Car_No,Shelf_Car_type,Shelf_Car_Size,start_distance,ext_cmd) "
                                                    Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "','" + Car(car_idx).device_no.ToString + "','" + Car(car_idx).Cmd_From.ToString + "', '" + Car(car_idx).Cmd_To.ToString + "', '" + Car(car_idx).RequestTime + "', '" + Car(car_idx).Requestor + "',now()," + Car(car_idx).cmd_Shelf_Car_No.ToString + ",'" + Car(car_idx).cmd_Shelf_Car_Type + "','" + Car(car_idx).cmd_Shelf_Car_size + "'," + Car(car_idx).get_distance.ToString + ",'" + Car(car_idx).ext_cmd + "');"
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                    Query = "update agv_cmd_list set CMD_Status='" + Car(car_idx).cmd_list(Car(car_idx).cmd_idx) + "',Pri_Wt=999 where CmdKey =" + Car(car_idx).cmd_sql_idx.ToString
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                Catch ex As Exception
                                                    settext(Query + ":" + ex.Message)
                                                End Try
                                            End If
                                        End If
                                    Else
                                        Car(car_idx).To_AGV(20) = 105 '電壓低下

                                    End If


                                ElseIf Not select_idx = -1 Then
                                    Car(car_idx).cmd_sql_idx = CInt(Me.ListView1.Items(select_idx).SubItems(0).Text)
                                    Car(car_idx).Cmd_From = CInt(Me.ListView1.Items(select_idx).SubItems(2).Text)
                                    Car(car_idx).Cmd_To = CInt(Me.ListView1.Items(select_idx).SubItems(3).Text)
                                    Car(car_idx).RequestTime = (Me.ListView1.Items(select_idx).SubItems(6).Text)
                                    Car(car_idx).Requestor = (Me.ListView1.Items(select_idx).SubItems(7).Text)
                                    Car(car_idx).cmd_Shelf_Car_No = CInt(Me.ListView1.Items(select_idx).SubItems(8).Text)
                                    Car(car_idx).cmd_Shelf_Car_Type = Me.ListView1.Items(select_idx).SubItems(9).Text
                                    Car(car_idx).cmd_Shelf_Car_size = Me.ListView1.Items(select_idx).SubItems(10).Text
                                    Car(car_idx).Cmd_RollData = Me.ListView1.Items(select_idx).SubItems(11).Text
                                    Car(car_idx).ext_cmd = Me.ListView1.Items(select_idx).SubItems(12).Text
                                    Car(car_idx).Sql2Cmdlist()
                                    '選擇命令
                                    If Car(car_idx).cmd_idx = 0 Then
                                        '紀錄開始執行時間
                                        Query = ""
                                        Try
                                            Query = "INSERT INTO `agv_cmd_history` (`CmdKey`, `AGVNo`,`CmdFrom`, `CmdTo`, `RequestTime`, `Requestor`, `Start_Time`,Shelf_Car_No,Shelf_Car_type,Shelf_Car_Size,start_distance,ext_cmd) "
                                            Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "','" + Car(car_idx).device_no.ToString + "','" + Car(car_idx).Cmd_From.ToString + "', '" + Car(car_idx).Cmd_To.ToString + "', '" + Car(car_idx).RequestTime + "', '" + Car(car_idx).Requestor + "',now()," + Car(car_idx).cmd_Shelf_Car_No.ToString + ",'" + Car(car_idx).cmd_Shelf_Car_Type + "','" + Car(car_idx).cmd_Shelf_Car_size + "'," + Car(car_idx).get_distance.ToString + ",'" + Car(car_idx).ext_cmd + "');"
                                            sqlCommand.CommandText = Query
                                            sqlCommand.ExecuteNonQuery()
                                            Query = "update agv_cmd_list set CMD_Status='" + Car(car_idx).cmd_list(Car(car_idx).cmd_idx) + "',Pri_Wt=999 where CmdKey =" + Car(car_idx).cmd_sql_idx.ToString
                                            sqlCommand.CommandText = Query
                                            sqlCommand.ExecuteNonQuery()
                                        Catch ex As Exception
                                            settext(Query + ":" + ex.Message)
                                        End Try
                                    End If

                                End If

                            End If

                            If Not Car(car_idx).cmd_idx = -2 Then '沒有待執行命令
                                ' 開始執行命令
                                '確認命令未被刪除
                                Dim check_idx As Boolean = Check_SQL_idx(Car(car_idx).cmd_sql_idx)
                                settext(Car(car_idx).device_no.ToString + ":CheckSQL:" + check_idx.ToString + ":" + Car(car_idx).cmd_sql_idx.ToString)
                                ' If check_idx Or (My.Settings.chrage_flag And Car(car_idx).Cmd_To = Car(car_idx).Chrage_Point) Then
                                '-----------------
                                If (DateDiff("s", Car(car_idx).Pre_TagID_time, Now) > AgvTimeoutVal) Then
                                    Car(car_idx).To_AGV(20) = 115 '對峙TimeOut

                                End If
                                If check_idx Then
                                    Select Case Car(car_idx).cmd_list(Car(car_idx).cmd_idx)
                                        Case ""
                                            Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "FINSH"
                                            settext("cmd_list empty,cmd_idx:" + Car(car_idx).cmd_idx.ToString + " cmd_sql_idx" + Car(car_idx).cmd_sql_idx.ToString)
                                        Case "TagID->0"
                                            Car(car_idx).subcmd = "1"
                                            Car(car_idx).force_tagId(1)
                                        Case ("STOP")
                                            Car(car_idx).step_i = 902
                                        Case "CheckLifter"
                                            Car(car_idx).cmd_idx += 1
                                        Case "Manual_Forward"
                                            Car(car_idx).step_i = 21
                                            Car(car_idx).cmd_idx += 1
                                        Case "Manual_Backward"
                                            Car(car_idx).step_i = 31
                                            Car(car_idx).cmd_idx += 1
                                        Case "KeepManual"
                                            If Car(car_idx).step_i = 999 Then
                                                Car(car_idx).cmd_idx += 1
                                            End If
                                        Case "CHECK_START"
                                            If Car(car_idx).Car_type = "PIN" Or Car(car_idx).Car_type = "POWER" Or Car(car_idx).Car_type = "POWER2" Or Car(car_idx).Car_type = "LOWCAR" Then
                                                If Car(car_idx).get_pin = 5 Then
                                                    Car(car_idx).To_AGV(6) = 0
                                                    Car(car_idx).cmd_idx += 1
                                                ElseIf Car(car_idx).device_no = 6 And Car(car_idx).get_loading = 3 And Car(car_idx).get_pin = 10 And Car(car_idx).get_tagId() = 9000 Then

                                                    Car(car_idx).cmd_idx = 8

                                                ElseIf Car(car_idx).get_loading = 3 And Car(car_idx).get_pin = 10 And (Car(car_idx).get_Shelf_Car_No = Car(car_idx).cmd_Shelf_Car_No Or Car(car_idx).get_Shelf_Car_No = 0) Then
                                                    '載物與物品一致
                                                    If Car(car_idx).Car_type = "LOWCAR" Then
                                                        Car(car_idx).cmd_idx = 8
                                                    Else
                                                        Car(car_idx).cmd_idx = 6
                                                    End If

                                                ElseIf Car(car_idx).get_loading = 3 And Car(car_idx).get_pin = 10 Then

                                                    Car(car_idx).To_AGV(20) = 104 '有料無帳

                                                Else
                                                    Car(car_idx).To_AGV(6) = 4 'PIN 下降
                                                End If

                                            ElseIf Car(car_idx).Car_type = "ROLL" Then
                                                If Car(car_idx).get_loading = 3 Then
                                                    Car(car_idx).cmd_idx = 7

                                                    'ElseIf Car(car_idx).get_loading = 3 Then

                                                    '    Car(car_idx).To_AGV(20) = 104 '有料無帳
                                                Else
                                                    Car(car_idx).cmd_idx += 1
                                                End If
                                            ElseIf Car(car_idx).Car_type = "LFT" Then
                                                If Car(car_idx).get_tagId() Mod 10 = 0 Or Car(car_idx).get_loading = 3 Then
                                                    Car(car_idx).To_AGV(20) = 104 '有料無帳
                                                Else
                                                    Car(car_idx).cmd_idx += 1
                                                End If
                                            ElseIf Car(car_idx).Car_type = "FORK" Then
                                                If Car(car_idx).get_loading = 3 And Car(car_idx).get_pin = 10 Then
                                                    Car(car_idx).cmd_idx = 8
                                                Else
                                                    Car(car_idx).cmd_idx += 1
                                                End If

                                            End If

                                        Case "Check_Loading"

                                            settext("Check_Loading:" + Agvc_shelfcheck.Checked.ToString)
                                            If Agvc_shelfcheck.Checked = False And Loading_Check.Checked = False Then
                                                'bypass 在荷檢查
                                                Car(car_idx).cmd_idx += 1
                                            ElseIf Car(car_idx).get_loading = 3 And Loading_Check.Checked Then
                                                '更新資料庫
                                                If Car(car_idx).Car_type = "PIN" Then
                                                    If Car(car_idx).get_Shelf_Car_No() = Car(car_idx).cmd_Shelf_Car_No Then
                                                        Car(car_idx).cmd_idx += 1
                                                    Else
                                                        Car(car_idx).To_AGV(20) = 101
                                                        Car(car_idx).cmd_idx = -2
                                                        Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString
                                                    End If
                                                ElseIf Car(car_idx).Car_type = "ROLL" Then
                                                    Car(car_idx).cmd_idx += 1
                                                End If
                                            Else
                                                'Car(car_idx).cmd_idx = -2
                                                'Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString
                                                'Car(car_idx).To_AGV(20) = 100
                                            End If
                                        Case "CHECK_LOAD"
                                            '確認狀態 
                                            If Car(car_idx).Car_type = "LFT" Then
                                                If Car(car_idx).get_loading = 0 Or Car(car_idx).get_tagId() Mod 10 = 0 Then
                                                    Car(car_idx).To_AGV(20) = 104 '有料無帳
                                                Else
                                                    Car(car_idx).cmd_idx += 1
                                                End If
                                            ElseIf Car(car_idx).Car_type = "ROLL" Then
                                                Dim checkeq As Integer = 0
                                                Query = "select count(*) FROM `eqp` where Tag_ID=" + Car(car_idx).get_tagId.ToString + " and load_req=1"
                                                sqlCommand.CommandText = Query
                                                checkeq = CInt(sqlCommand.ExecuteScalar())

                                                Car(car_idx).cmd_idx += 1
                                                If checkeq = 0 Then
                                                    Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "FINSH"
                                                End If
                                            Else
                                                Car(car_idx).cmd_idx += 1
                                            End If

                                        Case "CHECK_UNLOAD"
                                            Dim checkeq As Integer = 0
                                            Query = "select count(*) FROM `eqp` where Tag_ID=" + Car(car_idx).get_tagId.ToString + " and unload_sensor=1"
                                            sqlCommand.CommandText = Query
                                            checkeq = CInt(sqlCommand.ExecuteScalar())
                                            Car(car_idx).cmd_idx += 1
                                            If checkeq = 0 Then
                                                Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "FINSH"
                                            End If
                                        Case "CHECK_UNLOCK"
                                            'If Car(car_idx).get_Shelf_Car_No > 0 Then
                                            For ii As Integer = 0 To shelf_car.Length - 1
                                                If shelf_car(ii).Shelf_Car_No = Car(car_idx).cmd_Shelf_Car_No And shelf_car(ii).UNLOCK = 1 And shelf_car(ii).step_i = 3 Then
                                                    '對象解鎖
                                                    Car(car_idx).cmd_idx += 1
                                                End If
                                            Next
                                            'End If
                                        Case "LOCK"
                                            Car(car_idx).Lock_user = Car(car_idx).Requestor
                                            Query = "UPDATE `agv_list` SET `lock_user` = '" + Car(car_idx).Lock_user + "' WHERE `AGVNo` = " + Car(car_idx).device_no.ToString + ";"
                                            sqlCommand.CommandText = Query
                                            sqlCommand.ExecuteNonQuery()
                                            Car(car_idx).cmd_idx += 1
                                        Case "UNLOCK"
                                            Car(car_idx).Lock_user = ""
                                            Query = "UPDATE `agv_list` SET `lock_user` = '" + Car(car_idx).Lock_user + "' WHERE `AGVNo` = " + Car(car_idx).device_no.ToString + ";"
                                            sqlCommand.CommandText = Query
                                            sqlCommand.ExecuteNonQuery()

                                            Car(car_idx).cmd_idx += 1
                                        Case "Forward_TagID"
                                            Car(car_idx).tagId(0) = Car(car_idx).get_tagId()
                                            Car(car_idx).action(0) = &H110
                                            Car(car_idx).action(1) = 3
                                            For i = 1 To 29
                                                Car(car_idx).tagId(i) = 0
                                                Car(car_idx).action(i * 2) = 0
                                                Car(car_idx).action(i * 2 + 1) = 0
                                            Next
                                            Car(car_idx).step_i = 1
                                            Car(car_idx).cmd_idx += 1
                                        Case "Backward_TagID"
                                            Car(car_idx).tagId(0) = Car(car_idx).get_tagId()
                                            Car(car_idx).action(0) = &H111
                                            Car(car_idx).action(1) = 3
                                            For i = 1 To 240 - 1
                                                Car(car_idx).tagId(i) = 0
                                                Car(car_idx).action(i * 2) = 0
                                                Car(car_idx).action(i * 2 + 1) = 0
                                            Next
                                            Car(car_idx).step_i = 1
                                            Car(car_idx).cmd_idx += 1

                                        Case "PINUP"
                                            settext("PINUP")
                                            If Car(car_idx).get_pin = 10 Then
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).cmd_idx += 1
                                                If Car(car_idx).Car_type = "FORK" Then
                                                    Query = " update `agv_list` A,`agv_buffer` B  set A.RollData=B.Buf_Type "
                                                    Query += " where A.AGVNo=" + Car(car_idx).device_no.ToString + " and B.TagID=" + Car(car_idx).get_tagId().ToString + " "
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()

                                                    'buffer
                                                    Query = "update `agv_buffer`  set Buf_Layer=Buf_Layer-1,Buf_Type=if(Buf_Layer=1,'',Buf_Type),LM_Time=now(),LM_User='AGVC_AGVC_PINUP' "
                                                    Query += " where  TagID=" + Car(car_idx).get_tagId().ToString + " and Buf_Layer > 0 "
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                    'add
                                                    Query = "update `agv_buffer`  set Buf_Type='',LM_Time=now(),LM_User='AGVC_TAKE_UP' "
                                                    Query += " where  Buf_Layer = 0 "
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()

                                                End If
                                            Else
                                                Car(car_idx).To_AGV(6) = 2
                                            End If
                                        Case "PINDOWNFORK"
                                            settext("PINDOWNFORK") 'just pindowm
                                            If Car(car_idx).get_pin = 5 Then
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).cmd_idx += 1
                                            Else
                                                Car(car_idx).To_AGV(6) = 4
                                            End If
                                        Case "PINDOWN"
                                            settext("PINDOWN")
                                            If Car(car_idx).get_pin = 5 Then
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).cmd_idx += 1
                                                If Car(car_idx).Car_type = "FORK" Then
                                                    '過帳BUFFER
                                                    '過帳到buffer
                                                    Query = "update `shelf_car` set LOCATION=" + Car(car_idx).get_tagId.ToString + ",updateTime=now(),updateName='PinDownByFork' where Shelf_Car_No=" + Car(car_idx).cmd_Shelf_Car_No.ToString
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()

                                                    Query = "update `agv_buffer` A ,agv_list B  set A.Buf_Layer=if(B.RollData='pallet',A.Buf_Layer+1,1),A.Buf_Type=B.RollData ,A.LM_Time=now(),A.LM_User='AGVC_PINDOWN' "
                                                    Query += " where  TagID=" + Car(car_idx).get_tagId().ToString + " and B.AGVNo=" + Car(car_idx).device_no.ToString
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                    '除帳
                                                    Query = " update `agv_list` A  set A.RollData='' "
                                                    Query += " where A.AGVNo=" + Car(car_idx).device_no.ToString
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                    Car(car_idx).cmd_Shelf_Car_No = 0
                                                    Car(car_idx).To_AGV(6) = 0
                                                End If
                                            Else

                                                If Car(car_idx).get_Shelf_Car_No > 0 And Agvc_shelfcheck.Checked = True Then
                                                    Query = "update `shelf_car` set LOCATION=0,updateTime=now(),updateName='AGVC" + Car(car_idx).cmd_idx.ToString + "' where LOCATION=" + Car(car_idx).get_tagId.ToString + " and not  Shelf_Car_No=" + Car(car_idx).get_Shelf_Car_No.ToString
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()

                                                    Query = "update `shelf_car` set LOCATION=" + Car(car_idx).get_tagId.ToString + ",updateTime=now(),updateName='PINDOWN' where Shelf_Car_No=" + Car(car_idx).get_Shelf_Car_No.ToString
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                End If
                                                Car(car_idx).To_AGV(6) = 4
                                            End If
                                        Case "ROLLIN"
                                            settext("ROLLIN")
                                            ' Car(car_idx).cmd_idx += 1
                                            'If Car(car_idx).get_loading = 0 Then
                                            Car(car_idx).To_AGV(6) = &H20
                                            If Car(car_idx).get_pin >= 8 And Car(car_idx).get_action = &H20 And Car(car_idx).get_Err = 0 Then
                                                '異常結束
                                                settext(Car(car_idx).device_no.ToString + ":異常結束")
                                                Car(car_idx).To_AGV(6) = 0
                                                If Car(car_idx).get_loading = 3 Then


                                                    Car(car_idx).cmd_idx += 1
                                                    Car(car_idx).RollData = Car(car_idx).Cmd_RollData
                                                    Try
                                                        If Car(car_idx).get_Shelf_Car_No > 0 Then
                                                            Query = "INSERT INTO `roll_data` (cmdkey,`Car_no`, `Rolltype`, `RollData`,`From_Pos`,`To_Pos`, `LM_time`) VALUES (" + Car(car_idx).cmd_sql_idx.ToString + ",'" + Car(car_idx).device_no.ToString + "', 'IN', '" + Car(car_idx).RollData + "','" + Car(car_idx).Cmd_From.ToString + "','" + Car(car_idx).Cmd_To.ToString + "', now());"

                                                        Else
                                                            Query = "INSERT INTO `roll_data` (cmdkey,`Car_no`, `Rolltype`, `RollData`,`From_Pos`,`To_Pos`, `LM_time`) VALUES (" + Car(car_idx).cmd_sql_idx.ToString + ",'" + Car(car_idx).device_no.ToString + "', 'IN', '" + Car(car_idx).get_Shelf_Car_No.ToString + "','" + Car(car_idx).Cmd_From.ToString + "','" + Car(car_idx).Cmd_To.ToString + "', now());"

                                                        End If
                                                        sqlCommand.CommandText = Query
                                                        sqlCommand.ExecuteNonQuery()
                                                    Catch ex As Exception
                                                        settext("ex:" + ex.Message)
                                                        settext("mysqlerror:" + Query)
                                                    End Try


                                                    Query = "UPDATE `agv_list` SET `RollData` = '" + Car(car_idx).RollData + "' WHERE `AGVNo` = '" + Car(car_idx).device_no.ToString + "';"
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                    Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`) "
                                                    Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "',now(),now(), '201', '" + Car(car_idx).device_no.ToString + "', '', '', '" + Car(car_idx).get_tagId.ToString + "', '', '" + Car(car_idx).Shelf_Car_No.ToString + "') ;"
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                Else
                                                    Car(car_idx).cmd_idx += 1
                                                    Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "FINSH"
                                                    Try
                                                        If Car(car_idx).get_Shelf_Car_No > 0 Then
                                                            Query = "INSERT INTO `roll_data` (cmdkey,`Car_no`, `Rolltype`, `RollData`,`From_Pos`,`To_Pos`, `LM_time`) VALUES (" + Car(car_idx).cmd_sql_idx.ToString + ",'" + Car(car_idx).device_no.ToString + "', 'IN', '" + Car(car_idx).get_Shelf_Car_No.ToString + "','" + Car(car_idx).Cmd_From.ToString + "','" + Car(car_idx).Cmd_To.ToString + "', now());"
                                                        Else
                                                            Query = "INSERT INTO `roll_data` (cmdkey,`Car_no`, `Rolltype`, `RollData`,`From_Pos`,`To_Pos`, `LM_time`) VALUES (" + Car(car_idx).cmd_sql_idx.ToString + ",'" + Car(car_idx).device_no.ToString + "', 'IN', '" + Car(car_idx).RollData + "','" + Car(car_idx).Cmd_From.ToString + "','" + Car(car_idx).Cmd_To.ToString + "', now());"

                                                        End If

                                                        sqlCommand.CommandText = Query
                                                        sqlCommand.ExecuteNonQuery()
                                                    Catch ex As Exception
                                                        settext("ex:" + ex.Message)
                                                        settext("mysqlerror:" + Query)
                                                    End Try
                                                End If
                                            ElseIf Car(car_idx).get_pin = 2 And Car(car_idx).get_action = &H20 And Car(car_idx).get_Err = 0 Then
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).cmd_idx += 1
                                                Car(car_idx).RollData = Car(car_idx).Cmd_RollData
                                                Try
                                                    If Car(car_idx).get_Shelf_Car_No > 0 Then
                                                        Query = "INSERT INTO `roll_data` (cmdkey,`Car_no`, `Rolltype`, `RollData`,`From_Pos`,`To_Pos`, `LM_time`) VALUES (" + Car(car_idx).cmd_sql_idx.ToString + ",'" + Car(car_idx).device_no.ToString + "', 'IN', '" + Car(car_idx).get_Shelf_Car_No.ToString + "','" + Car(car_idx).Cmd_From.ToString + "','" + Car(car_idx).Cmd_To.ToString + "', now());"

                                                    Else
                                                        Query = "INSERT INTO `roll_data` (cmdkey,`Car_no`, `Rolltype`, `RollData`,`From_Pos`,`To_Pos`, `LM_time`) VALUES (" + Car(car_idx).cmd_sql_idx.ToString + ",'" + Car(car_idx).device_no.ToString + "', 'IN', '" + Car(car_idx).RollData + "','" + Car(car_idx).Cmd_From.ToString + "','" + Car(car_idx).Cmd_To.ToString + "', now());"

                                                    End If
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                Catch ex As Exception
                                                    settext("ex:" + ex.Message)
                                                    settext("mysqlerror:" + Query)
                                                End Try


                                                Query = "UPDATE `agv_list` SET `RollData` = '" + Car(car_idx).RollData + "' WHERE `AGVNo` = '" + Car(car_idx).device_no.ToString + "';"
                                                sqlCommand.CommandText = Query
                                                sqlCommand.ExecuteNonQuery()
                                            End If

                                            'End If
                                        Case "ROLLOUT"
                                            settext("ROLLOUT")
                                            If Car(car_idx).To_AGV(6) = 0 Then
                                                Try
                                                    If Car(car_idx).get_Shelf_Car_No > 0 Then
                                                        Query = "INSERT INTO `roll_data` (cmdkey,`Car_no`, `Rolltype`, `RollData`,`From_Pos`,`To_Pos`, `LM_time`) VALUES (" + Car(car_idx).cmd_sql_idx.ToString + ",'" + Car(car_idx).device_no.ToString + "', 'OUT', '" + Car(car_idx).get_Shelf_Car_No.ToString + "','" + Car(car_idx).Cmd_From.ToString + "','" + Car(car_idx).Cmd_To.ToString + "', now());"
                                                    Else
                                                        Query = "INSERT INTO `roll_data` (cmdkey,`Car_no`, `Rolltype`, `RollData`,`From_Pos`,`To_Pos`, `LM_time`) VALUES (" + Car(car_idx).cmd_sql_idx.ToString + ",'" + Car(car_idx).device_no.ToString + "', 'OUT', '" + Car(car_idx).RollData + "','" + Car(car_idx).Cmd_From.ToString + "','" + Car(car_idx).Cmd_To.ToString + "', now());"

                                                    End If


                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                Catch ex As Exception
                                                    settext("ex:" + ex.Message)
                                                    settext("mysqlerror:" + Query)
                                                End Try
                                            End If

                                            If Car(car_idx).To_AGV(6) = &H20 Then
                                                Car(car_idx).To_AGV(6) = &H20
                                            Else
                                                Car(car_idx).To_AGV(6) = &H10
                                            End If


                                            If Car(car_idx).get_pin >= 8 And (Car(car_idx).get_action = &H10 Or Car(car_idx).get_action = &H20) And Car(car_idx).get_Err = 0 Then
                                                '異常結束
                                                settext(Car(car_idx).device_no.ToString + ":異常結束")
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).cmd_idx += 1
                                                '---------
                                                'Try
                                                '    Query = "INSERT INTO `roll_data` (cmdkey,`Car_no`, `Rolltype`, `RollData`,`From_Pos`,`To_Pos`, `LM_time`) VALUES (" + Car(car_idx).cmd_sql_idx.ToString + ",'" + Car(car_idx).device_no.ToString + "', 'OUT', '" + Car(car_idx).RollData + "','" + Car(car_idx).Cmd_From.ToString + "','" + Car(car_idx).Cmd_To.ToString + "', now());"
                                                '    sqlCommand.CommandText = Query
                                                '    sqlCommand.ExecuteNonQuery()
                                                'Catch ex As Exception
                                                '    settext("ex:" + ex.Message)
                                                '    settext("mysqlerror:" + Query)
                                                'End Try
                                                '------------
                                                'Car(car_idx).RollData = "" '車子除帳
                                                'Query = "UPDATE `agv_list` SET `RollData` = '" + Car(car_idx).RollData + "' WHERE `AGVNo` = '" + Car(car_idx).device_no.ToString + "';"
                                                'sqlCommand.CommandText = Query
                                                'sqlCommand.ExecuteNonQuery()
                                                Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`) "
                                                Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "',now(),now(), '201', '" + Car(car_idx).device_no.ToString + "', '', '', '" + Car(car_idx).get_tagId.ToString + "', '', '" + Car(car_idx).Shelf_Car_No.ToString + "') ;"
                                                sqlCommand.CommandText = Query
                                                sqlCommand.ExecuteNonQuery()
                                            ElseIf Car(car_idx).get_pin = 2 And Car(car_idx).get_action = &H10 And Car(car_idx).get_Err = 0 Then
                                                ' 只有ROLLOUT
                                                settext(Car(car_idx).device_no.ToString + ":只有ROLLOUT")
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).cmd_idx += 1
                                                '   '---------
                                                'Try
                                                '    Query = "INSERT INTO `roll_data` (cmdkey,`Car_no`, `Rolltype`, `RollData`,`From_Pos`,`To_Pos`, `LM_time`) VALUES (" + Car(car_idx).cmd_sql_idx.ToString + ",'" + Car(car_idx).device_no.ToString + "', 'OUT', '" + Car(car_idx).RollData + "','" + Car(car_idx).Cmd_From.ToString + "','" + Car(car_idx).Cmd_To.ToString + "', now());"
                                                '    sqlCommand.CommandText = Query
                                                '    sqlCommand.ExecuteNonQuery()
                                                'Catch ex As Exception
                                                '    settext("ex:" + ex.Message)
                                                '    settext("mysqlerror:" + Query)
                                                'End Try
                                                '---------
                                                Car(car_idx).RollData = "" '車子除帳
                                                Query = "UPDATE `agv_list` SET `RollData` = '" + Car(car_idx).RollData + "' WHERE `AGVNo` = '" + Car(car_idx).device_no.ToString + "';"
                                                sqlCommand.CommandText = Query
                                                sqlCommand.ExecuteNonQuery()
                                            ElseIf (Car(car_idx).get_pin = 6 Or Car(car_idx).get_pin = 2) And Car(car_idx).get_action = &H20 And Car(car_idx).get_Err = 0 Then
                                                settext("Exchange模式結束")
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).cmd_idx += 1
                                                '---------
                                                'Try
                                                '    Query = "INSERT INTO `roll_data` (cmdkey,`Car_no`, `Rolltype`, `RollData`,`From_Pos`,`To_Pos`, `LM_time`) VALUES (" + Car(car_idx).cmd_sql_idx.ToString + ",'" + Car(car_idx).device_no.ToString + "', 'OUT', '" + Car(car_idx).RollData + "','" + Car(car_idx).Cmd_From.ToString + "','" + Car(car_idx).Cmd_To.ToString + "', now());"
                                                '    sqlCommand.CommandText = Query
                                                '    sqlCommand.ExecuteNonQuery()
                                                'Catch ex As Exception
                                                '    settext("ex:" + ex.Message)
                                                '    settext("mysqlerror:" + Query)
                                                'End Try
                                                Car(car_idx).RollData = "" '車子除帳
                                                Query = "UPDATE `agv_list` SET `RollData` = '" + Car(car_idx).RollData + "' WHERE `AGVNo` = '" + Car(car_idx).device_no.ToString + "';"
                                                sqlCommand.CommandText = Query
                                                sqlCommand.ExecuteNonQuery()
                                            ElseIf Car(car_idx).get_pin = 6 And Car(car_idx).get_action = &H10 And Car(car_idx).get_Err = 0 Then
                                                settext("Exchange:收箱")
                                                Car(car_idx).To_AGV(6) = &H20
                                            End If
                                        Case "ROBOT"


                                            Dim ext_cmd_list() As String = Car(car_idx).ext_cmd.Split(",")

                                            If ext_cmd_list.Length = 3 Then
                                                If IsNumeric(ext_cmd_list(0)) And IsNumeric(ext_cmd_list(1)) Then

                                                    Car(car_idx).To_AGV(6) = CInt(ext_cmd_list(0))
                                                    Car(car_idx).To_AGV(7) = CInt(ext_cmd_list(1))
                                                    Car(car_idx).To_AGV(8) = CInt(ext_cmd_list(2))
                                                    If Car(car_idx).get_pin = 2 And Car(car_idx).get_action = Car(car_idx).To_AGV(6) And Car(car_idx).get_Err = 0 Then
                                                        Car(car_idx).To_AGV(6) = 0
                                                        Car(car_idx).To_AGV(7) = 0
                                                        Car(car_idx).To_AGV(8) = 0
                                                        Car(car_idx).cmd_idx += 1
                                                    End If
                                                End If
                                            End If
                                        Case "ACTION"
                                            Dim ext_cmd_list() As String = Car(car_idx).ext_cmd.Split(",")
                                            If ext_cmd_list.Length = 3 Then
                                                If IsNumeric(ext_cmd_list(0)) And IsNumeric(ext_cmd_list(1)) And IsNumeric(ext_cmd_list(2)) Then

                                                    Car(car_idx).To_AGV(6) = CInt(ext_cmd_list(0))
                                                    Car(car_idx).To_AGV(7) = CInt(ext_cmd_list(1))
                                                    Car(car_idx).To_AGV(8) = CInt(ext_cmd_list(2))

                                                    If Car(car_idx).get_interlock = 2 And Car(car_idx).get_action = Car(car_idx).To_AGV(6) And Car(car_idx).get_Err = 0 Then
                                                        Car(car_idx).To_AGV(6) = 0
                                                        Car(car_idx).To_AGV(7) = 0
                                                        Car(car_idx).To_AGV(8) = 0
                                                        Car(car_idx).cmd_idx += 1
                                                    End If
                                                Else
                                                    Car(car_idx).cmd_idx += 1
                                                End If
                                            End If
                                        Case "ADDPOINT"
                                            '樓層 ID 2DX 2DY 0 0 0 0
                                            Dim ext_cmd_list() As String = Car(car_idx).ext_cmd.Split(",")
                                            If ext_cmd_list.Length = 7 Then

                                                If IsNumeric(ext_cmd_list(0)) And IsNumeric(ext_cmd_list(1)) Then
                                                    Car(car_idx).To_AGV(13) = 2

                                                    Car(car_idx).To_AGV(21) = CInt(ext_cmd_list(0))
                                                    Car(car_idx).To_AGV(22) = CInt(ext_cmd_list(1))
                                                    Car(car_idx).To_AGV(23) = CInt(ext_cmd_list(2)) >> 16 'Z
                                                    Car(car_idx).To_AGV(24) = CInt(ext_cmd_list(2)) Mod 65536

                                                    Car(car_idx).To_AGV(25) = CInt(ext_cmd_list(3)) >> 16 'Y
                                                    Car(car_idx).To_AGV(26) = CInt(ext_cmd_list(3)) Mod 65536

                                                    Car(car_idx).To_AGV(27) = CInt(ext_cmd_list(4)) >> 16 'Z
                                                    Car(car_idx).To_AGV(28) = CInt(ext_cmd_list(4)) Mod 65536
                                                    Car(car_idx).To_AGV(29) = CInt(ext_cmd_list(5)) 'AGV Val
                                                    Car(car_idx).To_AGV(30) = CInt(ext_cmd_list(6))

                                                    If Car(car_idx).device_status(13) = 2 Then '檢查bit 1
                                                        '要建立點位
                                                        Car(car_idx).To_AGV(13) = 0

                                                        Car(car_idx).cmd_idx += 1
                                                    End If
                                                End If
                                            End If

                                        Case "CHARGE"

                                            Car(car_idx).To_AGV(6) = 48
                                            If Car(car_idx).device_status(9) And Car(car_idx).get_action = 48 And Car(car_idx).get_Err = 0 Then
                                                Car(car_idx).Counter_timer = Now()
                                                Car(car_idx).cmd_idx += 1
                                            End If
                                        Case "CHARGING"
                                            Car(car_idx).To_AGV(6) = 48
                                            If Car(car_idx).AutoCharge = 1 Then
                                                '1百分比
                                                If Car(car_idx).get_SOC > Car(car_idx).AutoChargeVal Then
                                                    Car(car_idx).To_AGV(6) = 0
                                                    Car(car_idx).cmd_idx += 1
                                                    settext(Car(car_idx).device_no.ToString + "CHARGINGSOC高於設定值，終止充電")
                                                End If
                                            ElseIf Car(car_idx).AutoCharge = 2 Then
                                                ' 2絕對時間
                                                If (DateDiff("n", Car(car_idx).Counter_timer, Now) > Car(car_idx).AutoChargeVal) Then
                                                    Car(car_idx).To_AGV(6) = 0
                                                    Car(car_idx).cmd_idx += 1
                                                    settext(Car(car_idx).device_no.ToString + "CHARGING定時到，終止充電")
                                                End If

                                            ElseIf Car(car_idx).AutoCharge = 3 Then
                                                '最低時間命令優先
                                                If (DateDiff("n", Car(car_idx).Counter_timer, Now) > Car(car_idx).AutoChargeVal) Then
                                                    For k As Integer = 0 To ListView1.Items.Count - 1
                                                        If Not ListView1.Items(k).SubItems(2).Text = "4" And ListView1.Items(k).SubItems(1).Text = Car(car_idx).device_no.ToString Then
                                                            settext(Car(car_idx).device_no.ToString + ":CHARGING other cmd" + ListView1.Items(k).SubItems(0).Text)
                                                            Car(car_idx).To_AGV(6) = 0
                                                            Car(car_idx).cmd_idx += 1
                                                            Exit For
                                                        End If
                                                    Next
                                                ElseIf (DateDiff("n", Car(car_idx).Counter_timer, Now) > Car(car_idx).AutoChargeVal + 120) Then
                                                    settext(Car(car_idx).device_no.ToString + "CHARGING定時到120分鐘，終止充電")
                                                    Car(car_idx).To_AGV(6) = 0
                                                    Car(car_idx).cmd_idx += 1
                                                End If

                                            Else
                                                settext(Car(car_idx).device_no.ToString + "無設定充電方式，終止充電" + Car(car_idx).AutoCharge.ToString)
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).cmd_idx += 1
                                            End If

                                            If Car(car_idx).device_status(9) = 2 Or Car(car_idx).device_status(8) = 2 Then
                                                settext(Car(car_idx).device_no.ToString + "CHARGING  AGV完成充電命令")
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).cmd_idx += 1
                                            End If
                                        Case "CounterStart"
                                            settext(Car(car_idx).device_no.ToString + ":Counter  " + Now().ToString)
                                            Car(car_idx).To_AGV(6) = 50
                                            If Car(car_idx).get_action = 50 And Car(car_idx).get_Err = 0 Then
                                                Car(car_idx).cmd_idx += 1
                                                Car(car_idx).Counter_timer = Now()
                                            End If
                                        Case "Waiting"
                                            If (DateDiff("s", Car(car_idx).Counter_timer, Now) > 250) Then
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).cmd_idx += 1
                                            End If
                                        Case "FORCE_OUT"
                                            settext("FORCE_OUT")
                                            Car(car_idx).To_AGV(6) = &H11
                                            If Car(car_idx).get_pin = 2 And Car(car_idx).get_action = &H11 And Car(car_idx).get_Err = 0 Then
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).cmd_idx += 1
                                                Car(car_idx).RollData = "" '車子除帳
                                                Query = "UPDATE `agv_list` SET `RollData` = '" + Car(car_idx).RollData + "' WHERE `AGVNo` = '" + Car(car_idx).device_no.ToString + "';"
                                                sqlCommand.CommandText = Query
                                                sqlCommand.ExecuteNonQuery()

                                            End If
                                        Case "TAKE_DOWN"
                                            Dim fn_idx As Integer = 0
                                            Dim ext_cmd_list() As String = Car(car_idx).ext_cmd.Split(",")

                                            If (ext_cmd_list.Length = 2 Or ext_cmd_list.Length = 4) And IsNumeric(ext_cmd_list(fn_idx)) Then
                                                Car(car_idx).To_AGV(6) = CInt(ext_cmd_list(fn_idx))
                                                If Car(car_idx).get_lft_action = CInt(ext_cmd_list(fn_idx)) Then

                                                    Car(car_idx).To_AGV(6) = 0
                                                    Car(car_idx).cmd_idx += 1
                                                End If
                                            End If
                                        Case "TAKE_UP"
                                            Dim fn_idx As Integer = 0
                                            Dim ext_cmd_list() As String = Car(car_idx).ext_cmd.Split(",")

                                            If (ext_cmd_list.Length = 2 Or ext_cmd_list.Length = 4) And IsNumeric(ext_cmd_list(fn_idx)) Then
                                                Car(car_idx).To_AGV(6) = CInt(ext_cmd_list(fn_idx)) + 1
                                                If Car(car_idx).get_lft_action = CInt(ext_cmd_list(fn_idx)) + 1 Then
                                                    Dim dbreader As MySqlDataReader
                                                    Dim Buf_Type As String = ""
                                                    Dim Buf_Layer As Integer = 0
                                                    Dim ans As Integer = 0
                                                    Query = "SELECT Buf_Type,Buf_Layer FROM `agv_buffer` where TagID=" + Car(car_idx).get_tagId().ToString
                                                    sqlCommand.CommandText = Query
                                                    dbreader = sqlCommand.ExecuteReader()

                                                    If dbreader.Read Then
                                                        Buf_Type = dbreader.Item(0)
                                                        Buf_Layer = CInt(dbreader.Item(1))
                                                        dbreader.Close()
                                                        settext("Car" + Car(car_idx).device_no.ToString + "(" + Car(car_idx).get_tagId().ToString + "):" + Buf_Type + ":" + Buf_Layer.ToString)
                                                        If Buf_Layer > 0 Then
                                                            Query = " update `agv_list` set RollData='" + Buf_Type + "' "
                                                            Query += " where AGVNo=" + Car(car_idx).device_no.ToString + ""
                                                            sqlCommand.CommandText = Query
                                                            ans = sqlCommand.ExecuteNonQuery()
                                                            settext(Query + ":" + ans.ToString)
                                                            Buf_Layer = Buf_Layer - 1

                                                            If Buf_Layer = 0 Then
                                                                Buf_Type = ""
                                                            End If

                                                            Query = "update `agv_buffer`  set Buf_Type='" + Buf_Type + "',Buf_Layer=" + Buf_Layer.ToString + ",LM_Time=now(),LM_User='AGVC_TAKE_UP' "
                                                            Query += " where  TagID=" + Car(car_idx).get_tagId().ToString
                                                            sqlCommand.CommandText = Query
                                                            ans = sqlCommand.ExecuteNonQuery()
                                                            settext(Query + ":" + ans.ToString)

                                                        End If
                                                    End If
                                                    If dbreader.IsClosed = False Then
                                                        dbreader.Close()
                                                    End If
                                                    Car(car_idx).To_AGV(6) = 0
                                                    Car(car_idx).cmd_idx += 1
                                                End If
                                            End If
                                        Case "ONCAR_CMD"
                                            Dim fn_idx As Integer = 0
                                            Dim ext_cmd_list() As String = Car(car_idx).ext_cmd.Split(",")

                                            If ext_cmd_list.Length = 1 And IsNumeric(ext_cmd_list(fn_idx)) Then
                                                Car(car_idx).To_AGV(6) = CInt(ext_cmd_list(fn_idx))
                                                If Car(car_idx).get_lft_action = CInt(ext_cmd_list(fn_idx)) Then
                                                    Car(car_idx).To_AGV(6) = 0
                                                    Car(car_idx).cmd_idx += 1
                                                End If
                                            End If
                                        Case "LFT_CTRL"
                                            Dim ext_cmd As String = Car(car_idx).ext_cmd
                                            If IsNumeric(ext_cmd) Then
                                                Car(car_idx).To_AGV(6) = CInt(ext_cmd)
                                                If Car(car_idx).get_lft_action = CInt(ext_cmd) Then
                                                    Car(car_idx).To_AGV(6) = 0
                                                    Car(car_idx).cmd_idx += 1
                                                End If
                                            End If
                                        Case "PUT_UP"
                                            Dim fn_idx As Integer = 1
                                            Dim ext_cmd_list() As String = Car(car_idx).ext_cmd.Split(",")

                                            If (ext_cmd_list.Length = 2 Or ext_cmd_list.Length = 4) And IsNumeric(ext_cmd_list(fn_idx)) Then
                                                Car(car_idx).To_AGV(6) = CInt(ext_cmd_list(fn_idx)) + 1
                                                If Car(car_idx).get_lft_action = CInt(ext_cmd_list(fn_idx)) + 1 Then
                                                    Car(car_idx).To_AGV(6) = 0
                                                    Car(car_idx).cmd_idx += 1
                                                End If
                                            End If
                                        Case "PUT_DOWN"
                                            Dim fn_idx As Integer = 1
                                            Dim ext_cmd_list() As String = Car(car_idx).ext_cmd.Split(",")

                                            If (ext_cmd_list.Length = 2 Or ext_cmd_list.Length = 4) And IsNumeric(ext_cmd_list(fn_idx)) Then
                                                Car(car_idx).To_AGV(6) = CInt(ext_cmd_list(fn_idx))
                                                If Car(car_idx).get_lft_action = ext_cmd_list(fn_idx) Then
                                                    '過帳到buffer
                                                    Query = "update `agv_buffer` A ,agv_list B  set A.Buf_Layer=if(B.RollData='pallet',A.Buf_Layer+1,1),A.Buf_Type=B.RollData ,A.LM_Time=now(),A.LM_User='AGVC_PUT_DOWN' "
                                                    Query += " where  TagID=" + Car(car_idx).get_tagId().ToString + " and B.AGVNo=" + Car(car_idx).device_no.ToString
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                    '除帳
                                                    Query = " update `agv_list` A  set A.RollData='' "
                                                    Query += " where A.AGVNo=" + Car(car_idx).device_no.ToString
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                    'Car(car_idx).cmd_Shelf_Car_No = 0
                                                    Car(car_idx).To_AGV(6) = 0
                                                    Car(car_idx).cmd_idx += 1
                                                End If
                                            End If
                                        Case "LFT_DOWN1"
                                            Car(car_idx).To_AGV(6) = 34
                                            If Car(car_idx).get_lft_action = 34 Then
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).cmd_idx += 1
                                            End If
                                        Case "LFT_DOWN2"

                                            Car(car_idx).To_AGV(6) = 30
                                            If Car(car_idx).get_lft_action = 30 Then
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).cmd_idx += 1
                                            End If
                                        Case "CHECK_SHELF_LOAD_ON"
                                            If Car(car_idx).get_shelf_loading = 1 Then
                                                Car(car_idx).cmd_idx += 1
                                            Else
                                                Car(car_idx).To_AGV(20) = 106 ' 無載荷
                                            End If
                                        Case "CHECK_SHELF_LOAD_OFF"
                                            If Car(car_idx).get_shelf_loading = 0 Then
                                                Car(car_idx).cmd_idx += 1
                                            Else
                                                Car(car_idx).To_AGV(20) = 107 ' 有載荷
                                            End If
                                        Case "FINSH"
                                            Try
                                                Query = "update  agv_cmd_history set End_TIme=now(),end_distance=" + Car(car_idx).get_distance.ToString + ",empty_time=" + Car(car_idx).empty_time.ToString + ",Load_time=" + Car(car_idx).Load_time.ToString + ",Error_time=" + Car(car_idx).Error_time.ToString + " where Cmdkey = " + Car(car_idx).cmd_sql_idx.ToString + " and Start_Time >'" + Now().AddHours(-2).ToString("yyyy-MM-dd HH:mm:ss") + "'"
                                                sqlCommand.CommandText = Query
                                                sqlCommand.ExecuteNonQuery()
                                                Query = "delete from  agv_cmd_list where CmdKey =" + Car(car_idx).cmd_sql_idx.ToString
                                                sqlCommand.CommandText = Query
                                                sqlCommand.ExecuteNonQuery()
                                                If Car(car_idx).get_Shelf_Car_No > 0 And Agvc_shelfcheck.Checked = True And Car(car_idx).Car_type = "PIN" Then
                                                    Query = "update   `shelf_car` set LOCATION=" + Car(car_idx).get_tagId.ToString + ",updateTime=now(),updateName='FINSH_GET' where Shelf_Car_No=" + Car(car_idx).get_Shelf_Car_No.ToString
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                    Query = "insert into shelf_car_history select * from shelf_car where shelf_car_no =" + Car(car_idx).get_Shelf_Car_No.ToString
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                ElseIf Car(car_idx).cmd_Shelf_Car_No > 0 And Car(car_idx).Car_type = "PIN" Then
                                                    Query = "update   `shelf_car` set LOCATION=" + Car(car_idx).get_tagId.ToString + ",updateTime=now(),updateName='FINSH_CMD' where Shelf_Car_No=" + Car(car_idx).cmd_Shelf_Car_No.ToString
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                    Query = "insert into shelf_car_history select * from shelf_car where shelf_car_no =" + Car(car_idx).cmd_Shelf_Car_No.ToString
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                End If
                                            Catch ex As Exception
                                                settext("FINSH SQL ERROR:" + Query)
                                            End Try
                                            For ii As Integer = 4 To 20
                                                Car(car_idx).To_AGV(ii) = 0
                                            Next
                                            Car(car_idx).cmd_sql_idx = 0
                                            Car(car_idx).Cmd_From = 0
                                            Car(car_idx).Cmd_To = 0
                                            Car(car_idx).RequestTime = ""
                                            Car(car_idx).Requestor = ""
                                            Car(car_idx).cmd_Shelf_Car_No = 0
                                            Car(car_idx).cmd_Shelf_Car_Type = ""
                                            Car(car_idx).cmd_Shelf_Car_size = 0
                                            Car(car_idx).cmd_idx = -2 '恢復可接受命令狀態
                                            Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString
                                        Case "FINSH01"
                                            Try
                                                Query = "update  agv_cmd_history set End_TIme=now(),end_distance=" + Car(car_idx).get_distance.ToString + ",empty_time=" + Car(car_idx).empty_time.ToString + ",Load_time=" + Car(car_idx).Load_time.ToString + ",Error_time=" + Car(car_idx).Error_time.ToString + " where Cmdkey = " + Car(car_idx).cmd_sql_idx.ToString + " and Start_Time >'" + Now().AddHours(-2).ToString("yyyy-MM-dd HH:mm:ss") + "'"
                                                sqlCommand.CommandText = Query
                                                sqlCommand.ExecuteNonQuery()
                                                Query = "delete from  agv_cmd_list where CmdKey =" + Car(car_idx).cmd_sql_idx.ToString
                                                sqlCommand.CommandText = Query
                                                sqlCommand.ExecuteNonQuery()
                                            Catch ex As Exception
                                                settext("FINSH01 SQL ERROR" + Query)
                                            End Try
                                            For ii As Integer = 4 To 20
                                                Car(car_idx).To_AGV(ii) = 0
                                            Next
                                            Car(car_idx).cmd_sql_idx = 0
                                            Car(car_idx).Cmd_From = 0
                                            Car(car_idx).Cmd_To = 0
                                            Car(car_idx).RequestTime = ""
                                            Car(car_idx).Requestor = ""
                                            Car(car_idx).cmd_Shelf_Car_No = 0
                                            Car(car_idx).cmd_Shelf_Car_Type = ""
                                            Car(car_idx).cmd_Shelf_Car_size = 0
                                            Car(car_idx).cmd_idx = -2 '恢復可接受命令狀態
                                            Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString
                                        Case "GoingNext"
                                            '用於搜尋下個點位
                                            If Car(car_idx).step_i = 999 And Car(car_idx).get_status = 0 Then
                                                Car(car_idx).cmd_idx += 1
                                            End If
                                        Case "NEXT"
                                            '用於搜尋下個點位
                                            ' If Car(car_idx).step_i = 999 And Car(car_idx).get_status = 0 Then
                                            Car(car_idx).cmd_idx += 1
                                            ' End If
                                        Case "NEXT2"
                                            '用於搜尋下個點位
                                            ' If Car(car_idx).step_i = 999 And Car(car_idx).get_status = 0 Then
                                            Car(car_idx).cmd_idx += 2
                                        Case "Going"

                                            If Car(car_idx).get_tagId = Car(car_idx).To_pos Then
                                                Car(car_idx).cmd_idx += 1
                                                Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString

                                            ElseIf (Car(car_idx).get_status = 0 Or Car(car_idx).get_status = 2) And Car(car_idx).get_Err = 0 Then
                                                '判斷時間
                                                Dim wait_setting As Integer = 1
                                                If Car(car_idx).get_tagId = Car(car_idx).To_temp_pos Then
                                                    Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString
                                                    wait_setting = 2
                                                End If
                                                Car(car_idx).Wait_count += 1
                                                If Car(car_idx).Wait_count > wait_setting Then
                                                    Car(car_idx).cmd_idx -= 1
                                                    Car(car_idx).sflag = 0
                                                End If
                                                settext("Wait_count:" + Car(car_idx).Wait_count.ToString)
                                                settext("get_tagId:" + Car(car_idx).get_tagId.ToString)
                                                settext("To_pos:" + Car(car_idx).To_pos.ToString)
                                                settext("To_temp_pos:" + Car(car_idx).To_temp_pos.ToString)
                                            ElseIf Car(car_idx).get_status = 4 Then
                                                Car(car_idx).Wait_count = 0
                                            End If
                                            If Car(car_idx).subcmd.IndexOf(Car(car_idx).get_tagId.ToString + ",") > -1 Then
                                                Car(car_idx).subcmd = Car(car_idx).subcmd.Remove(0, Car(car_idx).subcmd.IndexOf(Car(car_idx).get_tagId.ToString + ","))
                                            End If
                                        Case "GoingEmpty"
                                            '判斷出發狀態 ，不可以有載 也不可以頂PIN
                                            If Car(car_idx).get_tagId = Car(car_idx).To_pos Then
                                                Car(car_idx).cmd_idx += 1
                                                Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString
                                            ElseIf Car(car_idx).get_tagId = Car(car_idx).To_temp_pos Then
                                                Car(car_idx).cmd_idx -= 1
                                            ElseIf (Car(car_idx).get_status = 0 Or Car(car_idx).get_status = 2) And Car(car_idx).get_Err = 0 Then
                                                '判斷時間
                                                Car(car_idx).Wait_count += 1
                                                If Car(car_idx).Wait_count > 1 Then
                                                    Car(car_idx).cmd_idx -= 1
                                                End If
                                                settext("Wait_count:" + Car(car_idx).Wait_count.ToString)
                                                settext("get_tagId:" + Car(car_idx).get_tagId.ToString)
                                                settext("To_pos:" + Car(car_idx).To_pos.ToString)
                                                settext("To_temp_pos:" + Car(car_idx).To_temp_pos.ToString)

                                            ElseIf Car(car_idx).get_status = 4 Then
                                                Car(car_idx).Wait_count = 0
                                            End If
                                            If Car(car_idx).subcmd.IndexOf(Car(car_idx).get_tagId.ToString + ",") > -1 Then
                                                Car(car_idx).subcmd = Car(car_idx).subcmd.Remove(0, Car(car_idx).subcmd.IndexOf(Car(car_idx).get_tagId.ToString + ","))
                                            End If
                                        Case "Going_Check"
                                            If Car(car_idx).get_tagId = Car(car_idx).To_pos Then
                                                '到達目的，跳到下一個
                                                Car(car_idx).cmd_idx += 1
                                                Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString
                                                Car(car_idx).To_pos = 0

                                            ElseIf (Car(car_idx).get_status = 0 Or Car(car_idx).get_status = 2) And Car(car_idx).get_Err = 0 Then
                                                '判斷時間
                                                '判斷時間
                                                Dim wait_setting As Integer = 1
                                                If Car(car_idx).get_tagId = Car(car_idx).To_temp_pos Then
                                                    Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString
                                                    wait_setting = 1
                                                End If
                                                Car(car_idx).Wait_count += 1
                                                If Car(car_idx).Wait_count > wait_setting Then
                                                    Car(car_idx).cmd_idx -= 1
                                                    Car(car_idx).sflag = 0
                                                End If
                                                settext(Car(car_idx).device_no.ToString + ":Wait_count:" + Car(car_idx).Wait_count.ToString)
                                                settext(Car(car_idx).device_no.ToString + ":get_tagId:" + Car(car_idx).get_tagId.ToString)
                                                settext(Car(car_idx).device_no.ToString + ":To_pos:" + Car(car_idx).To_pos.ToString)
                                                settext(Car(car_idx).device_no.ToString + ":To_temp_pos:" + Car(car_idx).To_temp_pos.ToString)
                                            ElseIf DateDiff("s", Car(car_idx).Run_time, Now) > 240 And (Car(car_idx).get_tagId = Car(car_idx).from_pos Or Car(car_idx).get_tagId = Car(car_idx).from_pos + 1) Then
                                                ' 超過40秒(未前進到下個點位)
                                                'Car(car_idx).cmd_idx = -2
                                                'Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString
                                                'Car(car_idx).To_AGV(20) = 102 '走行超時

                                            ElseIf Car(car_idx).get_status = 4 Then
                                                Car(car_idx).Wait_count = 0
                                            End If


                                            If Car(car_idx).subcmd.IndexOf(Car(car_idx).get_tagId.ToString + ",") > -1 Then
                                                Car(car_idx).subcmd = Car(car_idx).subcmd.Remove(0, Car(car_idx).subcmd.IndexOf(Car(car_idx).get_tagId.ToString + ","))
                                            End If

                                        Case Else
                                            '執行路徑命令
                                            'Dim From_to_ary() As String
                                            ' Dim path_str As String

                                            Dim R_subcmd As String = ""
                                            If IsNumeric(Car(car_idx).cmd_list(Car(car_idx).cmd_idx)) Then

                                                Car(car_idx).from_pos = Car(car_idx).get_tagId
                                                Car(car_idx).To_pos = CInt(Car(car_idx).cmd_list(Car(car_idx).cmd_idx))
                                                Car(car_idx).sflag = 0
                                                If Car(car_idx).from_pos = Car(car_idx).To_pos Then
                                                    Car(car_idx).cmd_idx += 1
                                                Else
                                                    R_subcmd = Send2AGV(car_idx, 1)
                                                    If Not R_subcmd = "" And Not R_subcmd = Car(car_idx).get_tagId.ToString Then
                                                        Car(car_idx).cmd_idx += 1
                                                    Else
                                                        settext(Car(car_idx).device_no.ToString + ":無預約路徑", True, Car(car_idx).device_no)
                                                    End If

                                                End If
                                            Else
                                                settext(Car(car_idx).cmd_list(Car(car_idx).cmd_idx) + ":非數字", True, Car(car_idx).device_no)
                                                Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "FINSH"
                                            End If

                                    End Select
                                    '-----------------
                                    If Car(car_idx).cmd_idx >= 0 Then
                                        Query = "update agv_cmd_list set step_i=" + Car(car_idx).cmd_idx.ToString + ",CMD_status='" + Car(car_idx).cmd_list(Car(car_idx).cmd_idx).ToString + "',SubCmd='" + Car(car_idx).subcmd + "',cmd_cnt='" + Car(car_idx).main_subcmd.Split(",").Length.ToString + "' where CmdKey =" + Car(car_idx).cmd_sql_idx.ToString
                                        sqlCommand.CommandText = Query
                                        sqlCommand.ExecuteNonQuery()
                                    End If

                                Else
                                    '表列找不到SQL
                                    Query = "update  agv_cmd_history set Requestor=concat(Requestor,'(X)'),End_TIme=now(),end_distance=" + Car(car_idx).get_distance.ToString + ",empty_time=" + Car(car_idx).empty_time.ToString + ",Load_time=" + Car(car_idx).Load_time.ToString + ",Error_time=" + Car(car_idx).Error_time.ToString + " where Cmdkey = " + Car(car_idx).cmd_sql_idx.ToString + " and Start_Time >'" + Now().AddHours(-2).ToString("yyyy-MM-dd HH:mm:ss") + "'"
                                    sqlCommand.CommandText = Query
                                    sqlCommand.ExecuteNonQuery()
                                    Car(car_idx).cmd_idx = -2 '重置命令
                                    '先選擇命令
                                    Car(car_idx).cmd_sql_idx = 0
                                    Car(car_idx).Cmd_From = 0
                                    Car(car_idx).Cmd_To = 0
                                    Car(car_idx).RequestTime = ""
                                    Car(car_idx).Requestor = ""
                                    Car(car_idx).cmd_Shelf_Car_No = 0
                                    Car(car_idx).cmd_Shelf_Car_Type = ""
                                    Car(car_idx).cmd_Shelf_Car_size = ""
                                    Car(car_idx).Cmd_RollData = ""
                                    'Car(car_idx).To_AGV(20) = 0
                                    For ii As Integer = 4 To 20
                                        Car(car_idx).To_AGV(ii) = 0
                                    Next
                                End If

                            End If '選擇命令結束 Car(car_idx).cmd_idx = -2
                        Else
                            '離線時須判斷的程式
                            If Not Car(car_idx).cmd_idx = -2 Then
                                If Car(car_idx).get_status = 4 Then
                                    Car(car_idx).Wait_count = 0
                                End If
                                Car(car_idx).Wait_count = 0

                                If Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "Going_Check" And Car(car_idx).get_pin = 10 And (Car(car_idx).Car_type = "PIN" Or Car(car_idx).Car_type = "FORK") Then
                                    '判斷目前位置不等於出發或是出發地-1
                                    If Car(car_idx).get_loading = 3 Then
                                        Car(car_idx).Loading_err_cnt = 0
                                    End If
                                    If Car(car_idx).get_tagId = Car(car_idx).Cmd_From + 2 Then
                                        settext(Car(car_idx).device_no.ToString + ":判斷" + ALL_Loading_check.Checked.ToString + Loading_Check.Checked.ToString + "," + Car(car_idx).get_pin.ToString + "," + Car(car_idx).get_loading.ToString)
                                    End If

                                    Dim a As Integer = Car(car_idx).get_Shelf_Car_No
                                    If Not (Car(car_idx).get_tagId = Car(car_idx).from_pos Or Car(car_idx).get_tagId = Car(car_idx).from_pos - 1) Then



                                        If (Not Car(car_idx).device_status(17) = Car(car_idx).cmd_Shelf_Car_No) And Car(car_idx).cmd_Shelf_Car_No > 0 And Not Car(car_idx).device_no = 6 And Agvc_shelfcheck.Checked = True And Not Car(car_idx).Car_type = "FORK" Then
                                            'Car(car_idx).cmd_idx = -2
                                            settext(Car(car_idx).device_no.ToString + ":帳料不符")
                                            Car(car_idx).To_AGV(20) = 1000 '預比對

                                        ElseIf Car(car_idx).get_loading < 3 And Loading_Check.Checked = True And ALL_Loading_check.Checked = True And Car(car_idx).get_pin = 10 And Not Car(car_idx).Car_type = "FORK" Then
                                            '全時監控
                                            Car(car_idx).Loading_err_cnt += 1
                                            If Car(car_idx).Loading_err_cnt > 3 Then
                                                settext(Car(car_idx).device_no.ToString + ":未取到架台異常")
                                                If Car(car_idx).get_loading = 1 Then
                                                    Car(car_idx).To_AGV(20) = 1001 '  前感應未取到架台
                                                ElseIf Car(car_idx).get_loading = 2 Then
                                                    Car(car_idx).To_AGV(20) = 1002 '  後感應未取到架台
                                                Else
                                                    Car(car_idx).To_AGV(20) = 100 '  未取到架台
                                                End If

                                            End If
                                        ElseIf Car(car_idx).get_loading < 3 And Loading_Check.Checked = True And Car(car_idx).get_pin = 10 And Car(car_idx).get_tagId = Car(car_idx).Cmd_From + 2 And Not Car(car_idx).Car_type = "FORK" Then
                                            '非全時監控
                                            Car(car_idx).Loading_err_cnt += 1
                                            settext(Car(car_idx).device_no.ToString + ":未取到架台異常")
                                            If Car(car_idx).get_loading = 1 Then
                                                Car(car_idx).To_AGV(20) = 1001 '  未取到架台
                                            ElseIf Car(car_idx).get_loading = 2 Then
                                                Car(car_idx).To_AGV(20) = 1002 '  未取到架台
                                            Else
                                                Car(car_idx).To_AGV(20) = 100 '  未取到架台
                                            End If

                                        ElseIf (Not Car(car_idx).get_Shelf_Car_No = Car(car_idx).cmd_Shelf_Car_No) And Car(car_idx).cmd_Shelf_Car_No > 0 And Not Car(car_idx).device_no = 6 And Agvc_shelfcheck.Checked = True And Not Car(car_idx).Car_type = "FORK" Then
                                            'Car(car_idx).cmd_idx = -2
                                            settext(Car(car_idx).device_no.ToString + ":帳料不符")
                                            Car(car_idx).To_AGV(20) = 101 ' 帳料不符
                                        End If

                                        If Not Car(car_idx).get_tagId = Car(car_idx).To_pos + 1 And Not Car(car_idx).get_tagId = Car(car_idx).To_pos + 2 And Not Car(car_idx).get_tagId = Car(car_idx).To_pos + 3 And Not Car(car_idx).get_tagId = Car(car_idx).To_pos Then
                                            For ii As Integer = 0 To shelf_car_total_no
                                                If shelf_car(ii).LOCATION = Car(car_idx).To_pos And shelf_car(ii).LOCATION Mod 10 = 0 Then
                                                    ' Car(car_idx).cmd_idx = -2

                                                    Car(car_idx).To_AGV(20) = 103 ' 目的端有架台
                                                    settext(Car(car_idx).device_no.ToString + ":目的端有架台To_pos " + Car(car_idx).To_pos.ToString + "shelf_car" + shelf_car(ii).Shelf_Car_No.ToString + "LOCATION" + shelf_car(ii).LOCATION.ToString)

                                                End If
                                            Next

                                        End If

                                    End If
                                ElseIf Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "Going_Check" And Loading_Check.Checked = True And Car(car_idx).get_loading < 3 Then
                                    Car(car_idx).To_AGV(20) = 100 ' 未取到架台

                                ElseIf Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "GoingEmpty" And Car(car_idx).get_tagId Mod 10 > 3 And (Car(car_idx).get_loading = 1 Or Car(car_idx).get_loading = 2 Or Car(car_idx).get_loading = 3 Or Car(car_idx).get_pin = 10) Then
                                    Car(car_idx).To_AGV(20) = 112 '在席異常
                                ElseIf (Car(car_idx).Car_type = "LFT" Or Car(car_idx).Car_type = "FORK") And Car(car_idx).path_error_count >= 3 Then
                                    Car(car_idx).To_AGV(20) = 114 '地圖錯誤
                                ElseIf Car(car_idx).Car_type = "POWER2" And Car(car_idx).get_pin = 10 And Car(car_idx).get_tagId Mod 10 > 2 And Car(car_idx).cmd_list(Car(car_idx).cmd_idx).StartsWith("Going") Then
                                    Car(car_idx).To_AGV(20) = 112 '在席異常
                                End If
                                If Car(car_idx).get_Err = 100 And Car(car_idx).get_loading = 3 Then
                                    '自動復歸
                                    Car(car_idx).To_AGV(20) = 0

                                End If

                                '  settext(Car(car_idx).device_no.ToString + ":車子狀態不等於4，無重新規劃")
                                If Car(car_idx).get_status = 12 Or Car(car_idx).get_status = 8 Then
                                    Car(car_idx).rate_point = Car(car_idx).get_tagId()
                                    settext(Car(car_idx).device_no.ToString + ":設定旋轉點位" + Car(car_idx).rate_point.ToString + "，無重新規劃")
                                End If


                                If Car(car_idx).sflag = 1 Then
                                    ' settext(Car(car_idx).device_no.ToString + ":退避中，無重新規劃")
                                ElseIf Not Car(car_idx).cmd_list(Car(car_idx).cmd_idx).StartsWith("Going") Then
                                    '  settext(Car(car_idx).device_no.ToString + ":狀態->" + Car(car_idx).cmd_list(Car(car_idx).cmd_idx) + "，無重新規劃")
                                ElseIf Car(car_idx).subcmd.Split(",").Length > ReMapLen Then
                                    ' settext(Car(car_idx).device_no.ToString + ":長度大於3，無重新規劃")
                                ElseIf Car(car_idx).subcmd.Split(",").Length <= 2 Then
                                    '  settext(Car(car_idx).device_no.ToString + ":長度小於2，無重新規劃")

                                ElseIf Not Car(car_idx).get_status = 4 Then
                                    '  settext(Car(car_idx).device_no.ToString + ":車子狀態不等於4，無重新規劃")
                                    If Car(car_idx).get_status = 12 Or Car(car_idx).get_status = 8 Then
                                        Car(car_idx).rate_point = Car(car_idx).get_tagId()
                                        settext(Car(car_idx).device_no.ToString + ":設定旋轉點位" + Car(car_idx).rate_point.ToString + "，無重新規劃")
                                    End If
                                ElseIf Car(car_idx).get_tagId = Car(car_idx).To_temp_pos Then
                                    '  settext(Car(car_idx).device_no.ToString + ":車子到達暫時停車點")
                                ElseIf Car(car_idx).subcmd.EndsWith(Car(car_idx).To_pos.ToString) Then
                                    '   settext(Car(car_idx).device_no.ToString + ":已規劃到目的地" + Car(car_idx).subcmd + "，無重新規劃")
                                ElseIf Car(car_idx).step_i < 999 Then
                                    settext(Car(car_idx).device_no.ToString + ":地圖未傳送完畢，無重新規劃")
                                ElseIf Car(car_idx).rate_point = Car(car_idx).get_tagId() Then
                                    settext(Car(car_idx).device_no.ToString + ":目前旋轉中" + Car(car_idx).subcmd + "，無重新規劃")
                                Else
                                    Dim R_subcmd As String = Send2AGV(car_idx, 11)
                                    settext(Car(car_idx).device_no.ToString + ":重新規劃路徑" + R_subcmd, True, Car(car_idx).device_no)
                                End If
                                'If Car(car_idx).step_i = 999 And Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "KeepManual" Then
                                '    Car(car_idx).cmd_idx += 1
                                'End If
                            End If
                            '離線移除TAGID的命令
                            Try


                                For i = 0 To Me.ListView1.Items.Count - 1
                                    If Not Me.ListView1.Items(i).SubItems(5).Text.StartsWith("ERROR") And CInt(Me.ListView1.Items(i).SubItems(1).Text) = Car(car_idx).device_no And CInt(Me.ListView1.Items(i).SubItems(3).Text) = -2 Then
                                        Car(car_idx).subcmd = "1"
                                        Car(car_idx).force_tagId(1) '建立離線

                                        Query = "delete from  agv_cmd_list where CmdKey =" + CInt(Me.ListView1.Items(i).SubItems(0).Text).ToString
                                        sqlCommand.CommandText = Query
                                        sqlCommand.ExecuteNonQuery()
                                        Query = "update  agv_cmd_history set End_TIme=now() where Cmdkey = " + CInt(Me.ListView1.Items(i).SubItems(0).Text).ToString
                                        sqlCommand.CommandText = Query
                                        sqlCommand.ExecuteNonQuery()
                                        Car(car_idx).cmd_sql_idx = 0
                                        Car(car_idx).cmd_idx = -2 '恢復可接受命令狀態

                                    End If
                                Next
                            Catch ex As Exception
                                Car(car_idx).cmd_sql_idx = 0
                                Car(car_idx).cmd_idx = -2 '恢復可接受命令狀態
                            End Try

                            Dim check_idx As Boolean = Check_SQL_idx(Car(car_idx).cmd_sql_idx)
                            If Car(car_idx).To_AGV(20) >= 100 And Car(car_idx).To_AGV(20) < 1000 And Not (Car(car_idx).To_AGV(20) = 105 And (Car(car_idx).get_Volt < Car(car_idx).Chrage_volt Or Car(car_idx).device_status(20) = 8)) And check_idx = False Then
                                '電壓恢復
                                Car(car_idx).cmd_idx = -2 '重置命令
                                Car(car_idx).To_AGV(20) = 0
                            End If
                            '
                        End If

                        '電池1 狀態異常
                        If Car(car_idx).BMS1(16) > 0 And BMSinfoCheck(Car(car_idx).BMS1) Then
                            If (BmsAlertIdx And Car(car_idx).BMS1(16)) > 0 Then
                                Car(car_idx).To_AGV(20) = 30000 + InttoBitidx(Car(car_idx).BMS1(16))
                                settext("AGV" + Car(car_idx).device_no.ToString + "bmsErr:" + int2str(Car(car_idx).BMS1, 0, 50))
                            ElseIf (BmsWarmIdx And Car(car_idx).BMS1(16)) > 0 Then
                                '警報處理機制
                                '鎖車->派到充電站

                                Dim chargerlist(0) As String
                                chargerlist(0) = Car(car_idx).Recharge_Point
                                For change_i As Integer = 0 To chargerlist.Length - 1
                                    Dim flag As Boolean = False
                                    '判斷充電站有沒有車子充電
                                    For j As Integer = 0 To Me.ListView1.Items.Count - 1
                                        If (Me.ListView1.Items(j).SubItems(2).Text) = 4 And Me.ListView1.Items(j).SubItems(3).Text = chargerlist(change_i) Then
                                            flag = True
                                        End If
                                    Next
                                    If flag = False Then
                                        '充電站IDLE
                                        If Send_CMD(Car(car_idx).device_no, 1, chargerlist(change_i)) Then
                                            Exit For
                                        End If
                                    End If
                                Next

                            End If
                        End If




                        If Car(car_idx).get_auto = 0 And Car(car_idx).get_Err = 0 Then
                            Dim bms1check, bms2check As Integer
                            If BMSinfoCheck(Car(car_idx).BMS1) Then
                                bms1check = Car(car_idx).CheckBms(Car(car_idx).BMS1, Car(car_idx).BMSAlarm1)
                                '電池1  BMS硬體異常
                                If Car(car_idx).BMS1(17) >= 64 And BMSinfoCheck(Car(car_idx).BMS1) Then
                                    Car(car_idx).To_AGV(20) = 30016 + InttoBitidx(Car(car_idx).BMS1(17))
                                End If
                                If Car(car_idx).BMSAlarm1(17) > 600 Then
                                    Car(car_idx).To_AGV(20) = 30065 '正常連線且心跳異常
                                End If
                                If Car(car_idx).BMSAlarm1(17) = 0 Then
                                    If bms1check > 0 Then
                                        If (BmsAlertIdx And bms1check) > 0 Then
                                            Car(car_idx).To_AGV(20) = 30032 + InttoBitidx(bms1check)
                                            settext("AGV" + Car(car_idx).device_no.ToString + "bms1check" + bms1check.ToString)
                                            settext("AGV" + Car(car_idx).device_no.ToString + "bms:" + int2str(Car(car_idx).BMS1, 0, 53))
                                        End If
                                    End If
                                End If
                            End If
                            If BMSinfoCheck(Car(car_idx).BMS2) Then
                                bms2check = Car(car_idx).CheckBms(Car(car_idx).BMS2, Car(car_idx).BMSAlarm2)
                                '電池2 狀態異常
                                If Car(car_idx).BMS2(16) > 0 And BMSinfoCheck(Car(car_idx).BMS2) Then
                                    If (BmsAlertIdx And Car(car_idx).BMS2(16)) > 0 Then
                                        Car(car_idx).To_AGV(20) = 30100 + InttoBitidx(Car(car_idx).BMS2(16))
                                    End If
                                End If
                                '電池2 BMS硬體異常
                                If Car(car_idx).BMS2(17) >= 64 And BMSinfoCheck(Car(car_idx).BMS2) Then
                                    Car(car_idx).To_AGV(20) = 30116 + InttoBitidx(Car(car_idx).BMS2(17))
                                End If
                                If Car(car_idx).BMSAlarm2(17) > 300 Then
                                    Car(car_idx).To_AGV(20) = 30165 '正常連線且心跳異常
                                End If
                                '電池二 上位偵測異常
                                If Car(car_idx).BMSAlarm2(17) = 0 Then
                                    If bms2check > 0 Then
                                        If (BmsAlertIdx And bms1check) > 0 Then
                                            Car(car_idx).To_AGV(20) = 30132 + InttoBitidx(bms2check)
                                            settext("AGV" + Car(car_idx).device_no.ToString + "bms2check" + bms1check.ToString)
                                            settext("AGV" + Car(car_idx).device_no.ToString + "bms:" + int2str(Car(car_idx).BMS2, 0, 53))
                                        End If
                                    End If
                                End If
                            End If
                        End If
                        If Car(car_idx).To_AGV(20) >= 30000 Then
                            settext("AGV" + Car(car_idx).device_no.ToString + "Errbms1:" + int2str(Car(car_idx).BMS1, 0, 20))
                            settext("AGV" + Car(car_idx).device_no.ToString + "Errbms2:" + int2str(Car(car_idx).BMS2, 0, 20))
                        End If

                    Else
                        For i = 0 To Me.ListView1.Items.Count - 1
                            Try


                                If CInt(Me.ListView1.Items(i).SubItems(3).Text) = -2 And CInt(Me.ListView1.Items(i).SubItems(1).Text) = Car(car_idx).device_no Then
                                    'Tag強制設為0
                                    Car(car_idx).subcmd = "1"
                                    Car(car_idx).force_tagId(1)
                                    Query = "delete from  agv_cmd_list where CmdKey =" + CInt(Me.ListView1.Items(i).SubItems(0).Text).ToString
                                    sqlCommand.CommandText = Query
                                    sqlCommand.ExecuteNonQuery()

                                End If
                            Catch ex As Exception

                            End Try
                        Next
                    End If '(Car(car_idx).flag And Car(car_idx).online)
                Next


                oConn.Close()
                oConn.Dispose()
                ListView1_ReNew()
                Me.PictureBox1.Invalidate()
                For i = 0 To Car.Length - 1


                    If Car(i).BMS1(7) > 2000 And Car(i).BMS1(7) < 7000 Then
                        Try
                            Dim file_str As String = ".\log\" + Now.ToString("yyyyMMdd") + "_BMS" + Car(i).device_no.ToString + ".log"
                            Dim filestream As StreamWriter = New StreamWriter(file_str, True)
                            Dim bms As String = int2str(Car(i).BMS1, 0, 52)
                            filestream.WriteLine(Now.ToString("yyyy-MM-dd HH:mm:ss") + ":" + bms)
                            If Car(i).BMS2(7) > 2000 And Car(i).BMS1(7) < 7000 Then
                                bms = int2str(Car(i).BMS2, 0, 52)
                                filestream.WriteLine(Now.ToString("yyyy-MM-dd HH:mm:ss") + ":" + bms)
                            End If

                            filestream.Flush()
                            filestream.Close()
                        Catch ex As Exception

                        End Try

                    End If

                    If Not Car(i).Lock_user = "" Then
                        '手動
                        Car(i).agv_status = "PM"

                    ElseIf (Car(i).flag = False Or Car(i).status = -2) Then
                        Car(i).agv_status = "OffLine"
                    ElseIf Car(i).status = 3 Then
                        Car(i).agv_status = "Manual"

                    ElseIf Car(i).device_status(6) = 0 And (Car(i).status = 2 Or Car(i).status = 0) Then
                        If Car(i).cmd_idx = -2 Then
                            Car(i).agv_status = "Idle"
                        Else
                            Car(i).agv_status = "Run"
                        End If

                    ElseIf Car(i).status = 5 Then
                        If Car(i).device_status(6) = 48 Then
                            Car(i).agv_status = "Charge"
                        Else
                            Car(i).agv_status = "Run"
                        End If
                    ElseIf Car(i).status = 4 Or Car(i).status = 1 Or Car(i).status = 4 Or Car(i).status = 12 Or Car(i).status = 8 Then
                        If Car(i).device_status(44) = 1 Then
                            Car(i).agv_status = "Mapping"
                        ElseIf Car(i).sflag = 1 Then
                            Car(i).agv_status = "Retreat"
                        Else
                            Car(i).agv_status = "Run"
                        End If

                    ElseIf (Car(i).status = -1) Then
                        Car(i).agv_status = "Down"
                    Else
                        Car(i).agv_status = "OffLine"
                    End If
                    If Not Car(i).agv_status = Car(i).Pre_agv_status And Car(i).device_no > 0 Then
                        Update_SQL("INSERT INTO `agv_status_history` (`AGVNo`, `Status`, `updatetime`, `PreStatus`, `Preupdatetime`) VALUES ('" + Car(i).device_no.ToString + "', '" + Car(i).agv_status + "', CURRENT_TIMESTAMP, '" + Car(i).Pre_agv_status + "', '" + Car(i).agv_status_time + "');")
                        Car(i).Pre_agv_status = Car(i).agv_status
                        Car(i).agv_status_time = Now.ToString("yyyy-MM-dd HH:mm:ss")
                    End If
                Next
            Catch ex As Exception
                ' MsgBox(ex.StackTrace.ToString)
                settext(ex.Message + " cmd_timer:" + ex.StackTrace.ToString)
                If car_idx >= 0 And car_idx <= car_no Then
                    settext("ErrCarinfo:" + Car(car_idx).device_no.ToString + "," + Car(car_idx).get_info)
                End If
            End Try
            cmd_timer_IsBusy = False
        End If
    End Sub
    Function Update_SQL(ByVal Query As String)
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        sqlCommand.CommandText = Query
        Try
            Update_SQL = sqlCommand.ExecuteNonQuery()
        Catch ex As Exception
            Update_SQL = 0
        End Try

        oConn.Close()
        oConn.Dispose()

    End Function

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Car(Cidx(CInt(txtCar.Text))).step_i = 902
    End Sub

    Sub writelog(ByVal str As String, ByVal device_no As Integer)
        Try
            Dim sw As StreamWriter = New StreamWriter(".\log\" + Now().ToString("yyyyMMdd") + "_toPC_" + device_no.ToString + ".log", True, Encoding.Default)
            sw.Write(Now.ToString + ":" + str + vbCrLf)
            sw.Flush()
            sw.Close()
        Catch ex As Exception

        End Try

    End Sub


    Sub writedoorlog(ByVal str As String)
        Try
            Dim sw As StreamWriter = New StreamWriter(".\log\" + Now().ToString("yyyyMMdd") + "_todoorPC.log", True, Encoding.Default)
            sw.Write(Now.ToString + ":" + str + vbCrLf)
            sw.Flush()
            sw.Close()
        Catch ex As Exception

        End Try

    End Sub



    Private Sub Button3_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        Dim Query As String = "INSERT INTO `agv_cmd_list` ( `CmdFrom`, `CmdTo`, `Pri_Wt`, `Requestor`) VALUES ('" + Me.From_cb.Text + "', '" + Me.To_cb.Text + "', '50', 'AGVC');"
        sqlCommand.CommandText = Query
        sqlCommand.ExecuteNonQuery()
        oConn.Close()
        oConn.Dispose()

    End Sub

    Private Sub Button13_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        If (Not Me.To_cb.Text = "") Then
            Dim Query As String = "INSERT INTO `agv_cmd_list` ( `CmdFrom`, `CmdTo`, `Pri_Wt`, `Requestor`) VALUES ('" + Me.From_cb.Text + "', '" + Me.To_cb.Text + "', '50', 'AGVC');"
            sqlCommand.CommandText = Query
            sqlCommand.ExecuteNonQuery()
            ListView1_ReNew()
        Else
            MsgBox("目的地不可為零")
        End If

        oConn.Close()
        oConn.Dispose()
    End Sub
    Dim Log_txt_cnt As Integer = 0
    Sub settext(ByVal logout As String, Optional ByVal append As Boolean = True, Optional ByVal car_no As Integer = 0)
        Try
            If Not logout = "" Then


                Dim file_str As String = ".\log\" + Now.ToString("yyyyMMdd") + ".log"
                If Not file_str = log_filename Then
                    log.Flush()
                    log.Close()
                    log_filename = file_str
                    log = New StreamWriter(log_filename, True, Encoding.Default)
                End If

                If Me.Log_txt.InvokeRequired Then
                    Dim d As New settextcallback(AddressOf settext)
                    Me.Invoke(d, New Object() {logout, append})
                ElseIf append = True Then
                    If settext_filter.Checked = True Then
                        If car_no = CInt(txtCar.Text) Or car_no = 0 Then
                            Me.Log_txt.AppendText(logout + vbCrLf)
                        End If
                    Else
                        Me.Log_txt.AppendText(logout + vbCrLf)
                    End If
                    Log_txt_cnt += 1
                    If Log_txt_cnt > 300 Then
                        Log_txt_cnt = 1
                        Me.Log_txt.Text = ""
                    End If
                    log.WriteLine(Now.ToString("yyyy-MM-dd HH:mm:ss") + ":" + logout)
                Else
                    If settext_filter.Checked = True Then
                        If car_no = CInt(txtCar.Text) Or car_no = 0 Then
                            Me.Log_txt.Text = logout + vbCrLf
                        End If
                    Else
                        Me.Log_txt.Text = logout + vbCrLf
                    End If


                    log.WriteLine(Now.ToString("yyyy-MM-dd HH:mm:ss") + ":" + logout)
                End If

                log.Flush()
            End If
        Catch ex As Exception

        End Try
    End Sub
   
   


  






    Private Sub Button12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button12.Click
        Log_txt.Text = ""
    End Sub

    Sub ListView1_ReNew()
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        oConn = New MySqlConnection(Mysql_str)
        'Dim Query As String = "SELECT CmdKey,AGVno,CmdFrom,CmdTo,Pri_Wt,CMD_Status,RequestTime,Requestor FROM `agv_cmd_list` where 1 order by Pri_Wt DESC "
        Dim mReader As MySqlDataReader
        Dim Query As String = ""
        Dim i As Integer
        Dim select_idx As String = ""
        If ListView1.SelectedItems.Count = 1 Then
            select_idx = Me.ListView1.SelectedItems(0).SubItems(0).Text
        End If
        oConn.Open()
        sqlCommand.Connection = oConn
        Try




            Query = "SELECT CmdKey,AGVno,CmdFrom,CmdTo,Pri_Wt,CMD_Status,date_format(RequestTime,'%Y-%m-%d %H:%i:%s'),Requestor,Shelf_Car_No,Shelf_Car_type,Shelf_Car_Size,RollData,ext_cmd  "
            Query += "FROM `agv_cmd_list`   where 1 order by Pri_Wt DESC,RequestTime ASC "
            ListView1.Items.Clear()
            sqlCommand.CommandText = Query
            mReader = sqlCommand.ExecuteReader()
            While (mReader.Read)
                Dim item As New ListViewItem()
                item.Text = mReader.Item(0)
                For i = 1 To 12
                    item.SubItems.Add(mReader.Item(i).ToString)
                Next
                ListView1.Items.Add(item)
            End While
            mReader.Close()
            For i = 0 To ListView1.Items.Count - 1
                If ListView1.Items(i).SubItems(0).Text = select_idx Then
                    ListView1.Items(i).Selected = True

                End If
            Next

        Catch ex As Exception
            settext("Mysql 異常")
        End Try
        ' MsgBox(My.Settings.MyDB)
        Query = "SELECT A.Shelf_Car_No,A.Shelf_Car_type,A.Shelf_Car_Size,A.LOCATION,A.flag,if (B.X is null,0, B.X),if (B.Y is null ,0,B.Y),A.step_i,A.UNLOCK_FLAG,A.offset_sensor "
        Query += " FROM `shelf_car` A left join point B on A.LOCATION=B.Tag_ID where A.flag=1 and A.LOCATION between 0 and 19999 "
        sqlCommand.CommandText = Query

        mReader = sqlCommand.ExecuteReader()



        i = 0
        While (mReader.Read)
            'Dim g As Graphics
            Try
                shelf_car(i).Shelf_Car_No = CInt(mReader.Item(0)) 'from
            Catch ex As Exception
                MsgBox(i)
            End Try

            shelf_car(i).Shelf_Car_type = mReader.Item(1)
            shelf_car(i).Shelf_Car_Size = mReader.Item(2) '
            shelf_car(i).LOCATION = CInt(mReader.Item(3)) ' 
            shelf_car(i).flag = mReader.Item(4) '
            shelf_car(i).X = mReader.Item(5) '
            shelf_car(i).Y = mReader.Item(6) '
            shelf_car(i).step_i = CInt(mReader.Item(7))
            shelf_car(i).UNLOCK = CInt(mReader.Item(8))
            shelf_car(i).offset_sensor = CInt(mReader.Item(9))
            Try
                shelf_car(i).car = Me.Controls.Find("Shelf_Car_Pic" + i.ToString(), True)(0)
            Catch ex As Exception
                MsgBox(i)
            End Try

            shelf_car(i).car.Visible = True
            shelf_car(i).car.Top = shelf_car(i).Y - offset_y
            shelf_car(i).car.Left = shelf_car(i).X - 15 - offset_x
            shelf_car(i).car.Text = shelf_car(i).Shelf_Car_No
            i += 1
        End While
        ' Array.Resize(shelf_car, i)
        mReader.Close()

        Try
            For i = 0 To car_no
                If Car(i).flag = True Then
                    Dim temp_Shelf_Car_No As String = ""

                    If Car(i).get_Shelf_Car_No > 0 And Car(i).Car_type = "ROLL" Then
                        temp_Shelf_Car_No = Car(i).get_Shelf_Car_No.ToString
                    ElseIf Car(i).get_Shelf_Car_No > 0 And Agvc_shelfcheck.Checked = True Then


                        temp_Shelf_Car_No = Car(i).get_Shelf_Car_No.ToString
                    ElseIf Car(i).cmd_Shelf_Car_No > 0 Then
                        temp_Shelf_Car_No = Car(i).cmd_Shelf_Car_No.ToString
                    Else
                        temp_Shelf_Car_No = " 0 "
                    End If
                    Dim VC1_MAX, VC1_MIN As Integer
                    Dim VC2_MAX, VC2_MIN As Integer
                    VC1_MAX = VC2_MAX = 0
                    VC1_MIN = 9999
                    VC2_MIN = 9999
                    For j As Integer = 18 To 32
                        If Car(i).BMS1(j) > VC1_MAX Then
                            VC1_MAX = Car(i).BMS1(j)
                        End If
                        If Car(i).BMS1(j) < VC1_MIN And Car(i).BMS1(j) > 0 Then
                            VC1_MIN = Car(i).BMS1(j)
                        End If
                        If Car(i).BMS2(j) > VC2_MAX Then
                            VC2_MAX = Car(i).BMS2(j)
                        End If
                        If Car(i).BMS2(j) < VC2_MIN And Car(i).BMS2(j) > 0 Then
                            VC2_MIN = Car(i).BMS2(j)
                        End If
                    Next
                    Query = "update  `agv_list` set CmdKey=" + Car(i).cmd_sql_idx.ToString + ",carWork=" + Car(i).device_status(6).ToString + ",AGVAction='" + Car(i).get_pin().ToString + "',"
                    Query += "Status='" + Car(i).status.ToString + "',Position=" + Car(i).get_tagId().ToString + ",ErrorCode='" + Car(i).get_Err().ToString + "',"
                    Query += "Speed=" + Car(i).get_Speed().ToString + ",BatteryVoltage=" + Car(i).get_Volt().ToString + ",Shelf_Car_No=" + temp_Shelf_Car_No + ",Loading='"
                    Query += Car(i).get_loading.ToString + "',distance=" + Car(i).get_distance.ToString + ",Temp=" + Car(i).device_status(21).ToString + ",tag_change_time='"
                    Query += Car(i).Pre_TagID_time.ToString("yyyy-MM-dd HH:mm:ss") + "',AGV_X=" + Car(i).AXIS_X.ToString + " ,AGV_Y=" + Car(i).AXIS_Y.ToString + ",AGV_TH=" + Car(i).AXIS_Z.ToString + ",AGV_Z=" + Car(i).device_status(22).ToString
                    Query += ",VB1=" + Car(i).BMS1(7).ToString + ",IB1=" + Car(i).BMS1(8).ToString + " ,BT1=" + Car(i).BMS1(10).ToString + ",SOC1=" + Car(i).BMS1(14).ToString + ",SOH1=" + Car(i).BMS1(15).ToString
                    Query += ",PROT1=" + Car(i).BMS1(16).ToString + ",STAT1=" + Car(i).BMS1(17).ToString + " ,CHG_AH1=" + (Car(i).BMS1(37) * 65536 + Car(i).BMS1(38)).ToString + ",DSG_AH1=" + (Car(i).BMS1(39) * 65536 + Car(i).BMS1(40)).ToString + ",CYCLE1=" + Car(i).BMS1(41).ToString
                    Query += ",VB2=" + Car(i).BMS2(7).ToString + ",IB2=" + Car(i).BMS2(8).ToString + " ,BT2=" + Car(i).BMS2(10).ToString + ",SOC2=" + Car(i).BMS2(14).ToString + ",SOH2=" + Car(i).BMS2(15).ToString
                    Query += ",PROT2=" + Car(i).BMS2(16).ToString + ",STAT2=" + Car(i).BMS2(17).ToString + " ,CHG_AH2=" + (Car(i).BMS2(37) * 65536 + Car(i).BMS2(38)).ToString + ",DSG_AH2=" + (Car(i).BMS2(39) * 65536 + Car(i).BMS2(40)).ToString + ",CYCLE2=" + Car(i).BMS2(41).ToString
                    Query += ",VC1_MAX=" + VC1_MAX.ToString + ",VC1_MIN=" + VC1_MIN.ToString + " ,VC2_MAX=" + VC2_MAX.ToString + ",VC2_MIN=" + VC2_MIN.ToString + ",BT1_2=" + Car(i).BMS1(11).ToString + ",BT2_2=" + Car(i).BMS2(11).ToString + ",car_site='" + Car(i).Site.ToString + "'"
                    Query += "  where AGVNo=" + Car(i).device_no.ToString
                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()

                    For k As Integer = 1 To 3
                        Dim BMS(53) As Integer
                        If k = 1 Then
                            BMS = Car(i).BMS1
                        ElseIf k = 2 Then
                            BMS = Car(i).BMS2
                        ElseIf k = 3 Then
                            BMS = Car(i).BMS3
                        End If
                        If BMSinfoCheck(BMS) Then
                            Query = "UPDATE `agv_bat_info` SET "
                            For j As Integer = 0 To 41
                                Query += " Val_" + j.ToString + "=" + BMS(j).ToString + " , "
                            Next
                            Query += " LM_TIME=now() where AGVNo=" + Car(i).device_no.ToString + " and slot=" + k.ToString
                            sqlCommand.CommandText = Query
                            sqlCommand.ExecuteNonQuery()
                        End If
                    Next

                End If

                ' If Car(i).Car_type = "FORK" And Car(i).cmd_Shelf_Car_No > 0 And Car(i).cmd_idx > 0 And Car(i).get_pin = 10 And Car(i).get_loading = 3 Then
                If Car(i).Car_type = "FORK" And Car(i).cmd_Shelf_Car_No > 0 And Car(i).get_pin = 10 And Car(i).get_loading = 3 Then
                    If Car(i).cmd_list(Car(i).cmd_idx) = "Going_Check" Or Car(i).cmd_list(Car(i).cmd_idx) = "PINDOWN" Then
                        Query = "update `shelf_car` set LOCATION=" + Car(i).get_tagId.ToString + ",updateTime=now(),updateName='List_ReNew' where Shelf_Car_No=" + Car(i).cmd_Shelf_Car_No.ToString
                        sqlCommand.CommandText = Query
                        sqlCommand.ExecuteNonQuery()
                    End If

                ElseIf Car(i).get_Shelf_Car_No > 0 And Car(i).cmd_idx > 0 And Car(i).get_pin = 10 And Agvc_shelfcheck.Checked = True And Not Car(i).Car_type = "FORK" Then
                    Query = "update `shelf_car` set LOCATION=" + Car(i).get_tagId.ToString + ",updateTime=now(),updateName='List_ReNew' where Shelf_Car_No=" + Car(i).get_Shelf_Car_No.ToString
                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()

                ElseIf Car(i).cmd_Shelf_Car_No > 0 And Car(i).cmd_idx > 0 And Car(i).get_pin = 10 And Car(i).step_i > 1 And Not Car(i).Car_type = "FORK" Then
                    Query = "update `shelf_car`   set LOCATION=" + Car(i).get_tagId.ToString + ",updateTime=now(),updateName='List_ReNew' where Shelf_Car_No=" + Car(i).cmd_Shelf_Car_No.ToString

                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()
                End If
            Next

            ' settext("Update SQL:" + Query)
        Catch ex As Exception
            settext("Update SQL:" + Query)
            settext("Update SQL ERROR")
        End Try
        Query = "update `system_info` set updatetime=now() where SYSTEM_TYPE='AGVC'"

        sqlCommand.CommandText = Query
        sqlCommand.ExecuteNonQuery()
        oConn.Close()
        oConn.Dispose()
    End Sub




    Private Sub Button4_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Car(view_car_idx).flag Then
            Car(view_car_idx).To_AGV(6) = 10
        Else
            MsgBox(view_car_idx.ToString + "號車未啟用")

        End If

    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Car(view_car_idx).flag Then
            Car(view_car_idx).To_AGV(6) = 20
        Else
            MsgBox(view_car_idx.ToString + "號車未啟用")
        End If

    End Sub

    Private Sub Button14_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Car(view_car_idx).flag Then
            Car(view_car_idx).To_AGV(13) = 0
        Else
            MsgBox(view_car_idx.ToString + "號車未啟用")
        End If

    End Sub

    Private Sub Button10_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Car(view_car_idx).flag Then
            Car(view_car_idx).To_AGV(13) = 1
        Else
            MsgBox(view_car_idx.ToString + "號車未啟用")
        End If

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim Action(240 - 1) As Integer
        Dim temptagid(240 - 1) As Integer
        For i As Integer = 0 To Tag_ID_List.Length - 1
            temptagid(i) = Tag_ID_List(i)
        Next
        If Car(view_car_idx).get_auto = 0 Then
            Car(view_car_idx).tagId = Tag_ID_List
            'Car(0).tagIdSize = Tag_ID_List.Length
            For i As Integer = 0 To 240 - 1
                If Tag_ID_List(i) Mod 10 = 1 Then
                    Action(i * 2) = &H110
                    Action(i * 2 + 1) = 3
                Else
                    Action(i * 2) = &H111
                    Action(i * 2 + 1) = 3
                End If
            Next
            Car(view_car_idx).action = Action
            Car(view_car_idx).step_i = 1
        Else
            MsgBox("目前AGV為手動狀態")
        End If
    End Sub



    Private Sub Button16_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Me.From_cb.Text = Me.Econ_16.Text
    End Sub

    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        ' MsgBox(Dijkstra_fn(Me.Dijkstra, Tag_ID_List, CInt(Me.From_cb.Text), (Me.To_cb.Text)))
    End Sub

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click

        Car(Cidx(CInt(txtCar.Text))).online = Not Car(Cidx(CInt(txtCar.Text))).online


        If Car(Cidx(CInt(txtCar.Text))).online Then
            Button7.Text = "ONLINE"
        Else
            Button7.Text = "OFFLINE"
        End If
    End Sub



  

   




    Private Sub ToolStripMenuItem1_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem1.Click
        ' MsgBox(ToolStripMenuItem1.Text)
        Dim Query As String
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        If Me.ListView1.SelectedItems.Count = 1 Then
            ' MsgBox(Me.ListView1.SelectedItems(0).SubItems(0).Text)
            Try
                Query = "delete from  agv_cmd_list where CmdKey =" + Me.ListView1.SelectedItems(0).SubItems(0).Text
                sqlCommand.CommandText = Query
                sqlCommand.ExecuteNonQuery()
                ListView1_ReNew()

            Catch ex As Exception

            End Try
        Else
            MsgBox("請先選擇命令")
        End If

        oConn.Close()
        oConn.Dispose()
    End Sub

    Private Sub ToolStripMenuItem2_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem2.Click
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        Dim Query As String
        Try

            Query = "update  agv_cmd_list set Pri_Wt=Pri_Wt+10 where CmdKey =" + Me.ListView1.SelectedItems(0).SubItems(0).Text
            sqlCommand.CommandText = Query
            sqlCommand.ExecuteNonQuery()
            ListView1_ReNew()
        Catch ex As Exception

        End Try

        oConn.Close()
        oConn.Dispose()
    End Sub
    Sub status_log(ByVal idx As Integer)
        Dim flag As Boolean = False
        For i As Integer = 0 To 20
            If Not Car(idx).device_status(i) = Car(idx).Pre_device_status(i) Then
                Car(idx).Pre_device_status(i) = Car(idx).device_status(i)
                flag = True
            End If
        Next
        If flag Then
            writelog(int2str(Car(idx).device_status, 0, 30), Car(idx).device_no)
        End If
    End Sub


    Private Sub Button9_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button9.Click
        ' Car(Cidx(CInt(txtCar.Text))).cmd_idx = -2
        Car(Cidx(CInt(txtCar.Text))).subcmd = Car(Cidx(CInt(txtCar.Text))).get_tagId
        Car(Cidx(CInt(txtCar.Text))).To_AGV(20) = 0
        Car(Cidx(CInt(txtCar.Text))).step_i = 905
        Car(Cidx(CInt(txtCar.Text))).path_error_count = 0
        Car(Cidx(CInt(txtCar.Text))).path_error_tagid = 0
        Car(Cidx(CInt(txtCar.Text))).Pre_TagID_time = Now()
    End Sub
    Sub Load_Path_base()
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim Query As String = ""
        Dim mReader As MySqlDataReader
        ' Dim path_i As Integer = 0
        Dim i As Integer

        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        Query = "SELECT A.From_Point,A.To_Point,A.Forward_Sensor,A.Forward_Speed,A.backward_Sensor,A.backward_Speed,B.X, B.Y, C.X, C.Y,A.distance"
        Query += " FROM `path` A"
        Query += " LEFT JOIN `point` B ON A.`From_Point` = B.Tag_ID"
        Query += " LEFT JOIN `point` C ON A.`To_Point` = C.Tag_ID where   A.active=1 "
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        ReDim path_base(10000)
        Dim action As String = ""
        Dim idx As Integer = 0
        While (mReader.Read)
            Try
                path_base(i).From_Point = CInt(mReader.Item(0)) 'from
                path_base(i).To_Point = CInt(mReader.Item(1)) 'To
                For j As Integer = 0 To i
                    If path_base(i).From_Point = path_base(j).To_Point And path_base(i).To_Point = path_base(j).From_Point Then
                        MsgBox("路徑重複:" + path_base(i).From_Point.ToString + "->" + path_base(i).To_Point.ToString)
                        Continue While
                    End If
                Next
                'path_base(path_i).direction = 0
                action = mReader.Item(2)
                If IsNumeric(action.Substring(1, 1)) Then
                    idx = 1
                Else
                    idx = 2
                End If
                path_base(i).action0 = action.Substring(0, idx)  '
                path_base(i).Sensor0 = action.Substring(idx) '
                path_base(i).speed0 = CInt(mReader.Item(3)) '  
                action = mReader.Item(4)
                If IsNumeric(action.Substring(1, 1)) Then
                    idx = 1
                Else
                    idx = 2
                End If
                path_base(i).action1 = action.Substring(0, idx)  '
                path_base(i).Sensor1 = action.Substring(idx) '
                path_base(i).speed1 = CInt(mReader.Item(5)) ' 
                Try
                    path_base(i).X1 = CInt(mReader.Item(6)) ' 
                    path_base(i).Y1 = CInt(mReader.Item(7)) ' 
                    path_base(i).X2 = CInt(mReader.Item(8)) ' 
                    path_base(i).Y2 = CInt(mReader.Item(9)) ' 
                Catch ex As Exception
                    MsgBox(path_base(i).From_Point.ToString + "->" + path_base(i).To_Point.ToString)
                    ' MsgBox(path_base(i).To_Point)
                End Try
                path_base(i).distance = Round(((path_base(i).X1 - path_base(i).X2) ^ 2 + (path_base(i).Y1 - path_base(i).Y2) ^ 2) ^ 0.5, 0)
                path_base(i).offsetdistance = CInt(mReader.Item(10)) ' 
             
                Path_base_Dictionary.Add(path_base(i).From_Point * 100000 + path_base(i).To_Point, path_base(i))
                i += 1
            Catch ex As Exception
                MsgBox("error" + path_base(i).From_Point.ToString + "," + path_base(i).To_Point.ToString + ex.Message)
            End Try

        End While

        Array.Resize(path_base, i)
        mReader.Close()
        Query = "SELECT From_Point,M_Point,To_Point,direction0,Sensor0,Speed0,direction1,Sensor1,Speed1,M2_Point"
        Query += " FROM `path_special` A"
        Query += " where  A.active=1 "
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        ReDim path_S(500)
        While (mReader.Read)
           
            Try

 
            path_S(i).From_Point = CInt(mReader.Item(0)) 'from
            path_S(i).M_Point = CInt(mReader.Item(1)) 'To
            path_S(i).To_Point = CInt(mReader.Item(2)) 'To
            'path_base(path_i).direction = 0
            path_S(i).direction0 = CInt(mReader.Item(3)) '

            action = mReader.Item(4)
            If IsNumeric(action.Substring(1, 1)) Then
                idx = 1
            Else
                idx = 2
            End If
            path_S(i).action0 = action.Substring(0, idx)  '
            path_S(i).Sensor0 = action.Substring(idx) '


            path_S(i).speed0 = CInt(mReader.Item(5)) ' 
            path_S(i).direction1 = CInt(mReader.Item(6)) '

            action = mReader.Item(7)
            If IsNumeric(action.Substring(1, 1)) Then
                idx = 1
            Else
                idx = 2
            End If
            path_S(i).action1 = action.Substring(0, idx)  '
            path_S(i).Sensor1 = action.Substring(idx) '

                path_S(i).speed1 = CInt(mReader.Item(8)) ' 
                path_S(i).M2_Point = mReader.Item(9).ToString  ' 


                i += 1

            Catch ex As Exception
                MsgBox(ex.Message)
            End Try
        End While
        Array.Resize(path_S, i)
        mReader.Close()

        Query = "SELECT TagID_LIST FROM `group_path`"
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        ReDim group_path(400)
        i = 0
        Group_path_text.Text = ""
        While (mReader.Read)
            group_path(i) = mReader.Item(0) '
            Group_path_text.Text += group_path(i) + vbCrLf
            i += 1
        End While
        mReader.Close()
        Array.Resize(group_path, i)

      

        Query = "SELECT Shelf_Car_type,max(Shelf_Car_Size)  FROM `shelf_car` where 1 group by Shelf_Car_type "
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()

        ReDim Dijkstra_list(20)
        Dijkstra_list(0).name = ""
        Dijkstra_list(0).CarType = ""
        Dijkstra_list(1).name = ""
        Dijkstra_list(1).CarType = "9"

        i = 2
        While (mReader.Read)
            Dijkstra_list(i).name = mReader.Item(0).ToString  '
            Dijkstra_list(i).CarType = mReader.Item(1).ToString  '
            i += 1
        End While
        mReader.Close()
        Array.Resize(Dijkstra_list, i)

        oConn.Close()
        oConn.Dispose()
    End Sub
    Sub Load_Path(ByVal path_type As String, ByVal tag_id() As Integer, ByRef L1(,) As Integer)
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim Query As String = ""
        Dim mReader As MySqlDataReader
        ' Dim path_i As Integer = 0
        Dim tagid_len As Integer = tag_id.Length
        ReDim L1(tagid_len - 1, tagid_len - 1)
        Dim i, j As Integer

        Dim path(5000) As Path
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
       
        sqlCommand.Connection = oConn
   

        Query = "SELECT A.From_Point,A.To_Point,A.Forward_Sensor,A.Forward_Speed,A.backward_Sensor,A.backward_Speed,B.X, B.Y, C.X, C.Y ,A.FORK_BACK,A.distance	"
        Query += " FROM `path` A"
        Query += " LEFT JOIN `point` B ON A.`From_Point` = B.Tag_ID"
        Query += " LEFT JOIN `point` C ON A.`To_Point` = C.Tag_ID where A.path_type like '%" + path_type + "%' and  A.active=1"


        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        While (mReader.Read)
            path(i).From_Point = CInt(mReader.Item(0)) 'from
            path(i).To_Point = CInt(mReader.Item(1)) 'To
            'path_base(path_i).direction = 0
            path(i).Sensor0 = mReader.Item(2) '
            path(i).speed0 = CInt(mReader.Item(3)) ' 
            path(i).Fork_back = CInt(mReader.Item(10)) ' FORK 可不可後退  
            path(i).Sensor1 = mReader.Item(4) '
            path(i).speed1 = CInt(mReader.Item(5))


            path(i).X1 = CInt(mReader.Item(6)) ' 
            path(i).Y1 = CInt(mReader.Item(7)) ' 
            path(i).X2 = CInt(mReader.Item(8)) ' 
            path(i).Y2 = CInt(mReader.Item(9)) ' 

            path(i).distance = Round(((path(i).X1 - path(i).X2) ^ 2 + (path(i).Y1 - path(i).Y2) ^ 2) ^ 0.5, 0)
            path(i).offsetdistance = CInt(mReader.Item(11)) ' 
            i += 1
        End While
        mReader.Close()
        Dim i_cnt As Integer = i
        Array.Resize(path, i_cnt)
        For i = 0 To tagid_len - 1
            For j = 0 To tagid_len - 1
                If (i = j) Then
                    L1(i, j) = 0
                Else
                    L1(i, j) = 99999
                End If
            Next
        Next
        For i = 0 To path.Length - 1
            Dim idx1 As Integer = Array.IndexOf(Tag_ID_List, path(i).From_Point)
            Dim idx2 As Integer = Array.IndexOf(Tag_ID_List, path(i).To_Point)



            If (path(i).speed0 > 0) Then
                L1(idx1, idx2) = CInt(path(i).distance / path(i).speed0)  '紀錄距離 正走
            End If

            If (path(i).speed1 > 0) Then
                L1(idx2, idx1) = CInt(path(i).distance / path(i).speed1)  '紀錄距離 逆走
            End If


        Next

        oConn.Close()
        oConn.Dispose()
    End Sub
    Sub Load_Path_fork_base()
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim Query As String = ""
        Dim mReader As MySqlDataReader
        ' Dim path_i As Integer = 0

        Dim i As Integer



        Dim idx As Integer = 0
        Dim action As String
        ReDim path_fork_base(10000)
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn

        Query = "SELECT A.From_Point,A.To_Point,A.Forward_Sensor,A.Forward_Speed,A.backward_Sensor,A.backward_Speed,B.X, B.Y, C.X, C.Y ,A.FORK_BACK,A.distance	"
        Query += " FROM `path` A"
        Query += " LEFT JOIN `point` B ON A.`From_Point` = B.Tag_ID"
        Query += " LEFT JOIN `point` C ON A.`To_Point` = C.Tag_ID where A.path_type like '%' and  A.active=1 "

        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        While (mReader.Read)
            Try


                path_fork_base(i).From_Point = CInt(mReader.Item(0)) 'from
                path_fork_base(i).To_Point = CInt(mReader.Item(1)) 'To
                'path_base(path_i).direction = 0
                action = mReader.Item(2)

                If IsNumeric(action.Substring(1, 1)) Then
                    idx = 1
                Else
                    idx = 2
                End If
                path_fork_base(i).action0 = action.Substring(0, idx)  '
                path_fork_base(i).Sensor0 = action.Substring(idx)
                path_fork_base(i).speed0 = CInt(mReader.Item(3)) ' 
                path_fork_base(i).Fork_back = CInt(mReader.Item(10)) ' FORK 可不可後退  
                action = mReader.Item(4)
                If IsNumeric(action.Substring(1, 1)) Then
                    idx = 1
                Else
                    idx = 2
                End If
                path_fork_base(i).action1 = action.Substring(0, idx)  '
                path_fork_base(i).Sensor1 = action.Substring(idx)

                If path_fork_base(i).Fork_back = 0 Then
                    path_fork_base(i).speed1 = 0
                Else
                    path_fork_base(i).speed1 = CInt(mReader.Item(5))
                End If
                path_fork_base(i).X1 = CInt(mReader.Item(6)) ' 
                path_fork_base(i).Y1 = CInt(mReader.Item(7)) ' 
                path_fork_base(i).X2 = CInt(mReader.Item(8)) ' 
                path_fork_base(i).Y2 = CInt(mReader.Item(9)) ' 
                path_fork_base(i).distance = Round(((path_fork_base(i).X1 - path_fork_base(i).X2) ^ 2 + (path_fork_base(i).Y1 - path_fork_base(i).Y2) ^ 2) ^ 0.5, 0)
                path_fork_base(i).offsetdistance = CInt(mReader.Item(11))
                Path_fork_Dictionary.Add(path_fork_base(i).From_Point * 100000 + path_fork_base(i).To_Point, path_fork_base(i))
                i += 1

                '逆地圖產生
                path_fork_base(i).From_Point = CInt(mReader.Item(1)) + 10000 'from
                path_fork_base(i).To_Point = CInt(mReader.Item(0)) + 10000 'To
                'path_base(path_i).direction = 0
                action = mReader.Item(2)
                If IsNumeric(action.Substring(1, 1)) Then
                    idx = 1
                Else
                    idx = 2
                End If
                path_fork_base(i).action0 = action.Substring(0, idx)  '
                path_fork_base(i).Sensor0 = action.Substring(idx)
                path_fork_base(i).speed0 = CInt(mReader.Item(3)) ' 

                If path_fork_base(i).action0 = "FR" Then
                    path_fork_base(i).action0 = "FL"
                ElseIf path_fork_base(i).action0 = "FL" Then
                    path_fork_base(i).action0 = "FR"
                End If
                path_fork_base(i).Fork_back = CInt(mReader.Item(10)) ' FORK 可不可後退  
                action = mReader.Item(4)
                '決定方向長度
                If IsNumeric(action.Substring(1, 1)) Then
                    idx = 1
                Else
                    idx = 2
                End If

                path_fork_base(i).action1 = action.Substring(0, idx)  '
                path_fork_base(i).Sensor1 = action.Substring(idx)
                If path_fork_base(i).action1 = "FR" Then
                    path_fork_base(i).action1 = "FL"
                ElseIf path_fork_base(i).action1 = "FL" Then
                    path_fork_base(i).action1 = "FR"
                End If
                If path_fork_base(i).Fork_back = 0 Then
                    path_fork_base(i).speed1 = 0
                Else
                    path_fork_base(i).speed1 = CInt(mReader.Item(5))
                End If

                path_fork_base(i).X1 = CInt(mReader.Item(8)) ' 
                path_fork_base(i).Y1 = CInt(mReader.Item(9)) ' 
                path_fork_base(i).X2 = CInt(mReader.Item(6)) ' 
                path_fork_base(i).Y2 = CInt(mReader.Item(7)) ' 
                path_fork_base(i).distance = Round(((path_fork_base(i).X1 - path_fork_base(i).X2) ^ 2 + (path_fork_base(i).Y1 - path_fork_base(i).Y2) ^ 2) ^ 0.5, 0)
                path_fork_base(i).offsetdistance = CInt(mReader.Item(11))
                Path_fork_Dictionary.Add(path_fork_base(i).From_Point * 100000 + path_fork_base(i).To_Point, path_fork_base(i))
                i += 1
            Catch ex As Exception
                MsgBox(ex.Message)
            End Try
        End While
        mReader.Close()
        Dim len As Integer = i
        Dim FromPoint As Integer = 0
        Dim ToPoint As Integer = 0
        Dim flag As Integer = 0
        Dim path_fork_base_idx As Integer = 0
        Dim key As Integer = 0
        Query = "SELECT A.From_Point,A.To_Point,A.Forward_Sensor,A.Forward_Speed,A.backward_Sensor,A.backward_Speed,B.X, B.Y, C.X, C.Y ,A.FORK_BACK	"
        Query += " FROM `path_fork` A"
        Query += " LEFT JOIN `point` B ON A.`From_Point` mod 10000 = B.Tag_ID"
        Query += " LEFT JOIN `point` C ON A.`To_Point` mod 10000 = C.Tag_ID where A.active=1 and not B.X is NULL and not C.X is NULL"

        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        While (mReader.Read)
            FromPoint = CInt(mReader.Item(0)) 'from
            ToPoint = CInt(mReader.Item(1)) 'To
            flag = 0

            For ii As Integer = 0 To len - 1

                If (path_fork_base(ii).From_Point = FromPoint And path_fork_base(ii).To_Point = ToPoint) Then
                    flag = 1 '正走的路線
                    path_fork_base_idx = ii
                    key = FromPoint * 100000 + ToPoint
                    Exit For
                ElseIf (path_fork_base(ii).From_Point = ToPoint And path_fork_base(ii).To_Point = FromPoint) Then                   
                    flag = 2 '逆地圖的路線
                    path_fork_base_idx = ii
                    key = ToPoint * 100000 + FromPoint

                    Exit For
                End If
            Next
            If flag = 1 Then
                If CInt(mReader.Item(3)) > 0 Then '前進速度超過0才覆蓋
                    action = mReader.Item(2)
                    '判斷障礙物是1個字元或是2個字元
                    If IsNumeric(action.Substring(1, 1)) Then
                        idx = 1
                    Else
                        idx = 2
                    End If
                    '取代舊的資料
                    path_fork_base(path_fork_base_idx).action0 = action.Substring(0, idx)  '
                    path_fork_base(path_fork_base_idx).Sensor0 = action.Substring(idx)
                    path_fork_base(path_fork_base_idx).speed0 = CInt(mReader.Item(3)) '

                End If
                If CInt(mReader.Item(5)) > 0 Then '後退速度超過0才覆蓋
                    action = mReader.Item(4)
                    '判斷障礙物是1個字元或是2個字元
                    If IsNumeric(action.Substring(1, 1)) Then
                        idx = 1
                    Else
                        idx = 2
                    End If

                    '取代舊的資料
                    path_fork_base(path_fork_base_idx).action1 = action.Substring(0, idx)  '
                    path_fork_base(path_fork_base_idx).Sensor1 = action.Substring(idx)
                    path_fork_base(path_fork_base_idx).speed1 = CInt(mReader.Item(5))

                   
                End If
                Path_fork_Dictionary(key) = path_fork_base(path_fork_base_idx)
            ElseIf flag = 2 Then
                '符合逆地圖
                If CInt(mReader.Item(5)) > 0 Then '後退速度超過0才覆蓋
                    action = mReader.Item(4)
                    If IsNumeric(action.Substring(1, 1)) Then
                        idx = 1
                    Else
                        idx = 2
                    End If
                    '取代舊的資料
                    path_fork_base(path_fork_base_idx).action0 = action.Substring(0, idx)  '
                    path_fork_base(path_fork_base_idx).Sensor0 = action.Substring(idx)
                    path_fork_base(path_fork_base_idx).speed0 = CInt(mReader.Item(5)) '

                End If

                If CInt(mReader.Item(3)) > 0 Then '前進速度超過0才覆蓋
                    action = mReader.Item(2)
                    If IsNumeric(action.Substring(1, 1)) Then
                        idx = 1
                    Else
                        idx = 2
                    End If
                    '取代舊的資料
                    path_fork_base(path_fork_base_idx).action1 = action.Substring(0, idx)  '
                    path_fork_base(path_fork_base_idx).Sensor1 = action.Substring(idx)
                    path_fork_base(path_fork_base_idx).speed1 = CInt(mReader.Item(3))

                End If
                Path_fork_Dictionary(key) = path_fork_base(path_fork_base_idx)

            Else
                '原始地圖沒有，直接新增
                path_fork_base(i).From_Point = CInt(mReader.Item(0)) 'from
                path_fork_base(i).To_Point = CInt(mReader.Item(1)) 'To
                action = mReader.Item(2)
                If IsNumeric(action.Substring(1, 1)) Then
                    idx = 1
                Else
                    idx = 2
                End If
                path_fork_base(i).action0 = action.Substring(0, idx)  '
                path_fork_base(i).Sensor0 = action.Substring(idx)
                path_fork_base(i).speed0 = CInt(mReader.Item(3)) '
                action = mReader.Item(4)
                If IsNumeric(action.Substring(1, 1)) Then
                    idx = 1
                Else
                    idx = 2
                End If
                path_fork_base(i).action1 = action.Substring(0, idx)  '
                path_fork_base(i).Sensor1 = action.Substring(idx)
                path_fork_base(i).speed1 = CInt(mReader.Item(5))
                path_fork_base(i).X1 = CInt(mReader.Item(6)) ' 
                path_fork_base(i).Y1 = CInt(mReader.Item(7)) ' 
                path_fork_base(i).X2 = CInt(mReader.Item(8)) ' 
                path_fork_base(i).Y2 = CInt(mReader.Item(9)) ' 
                path_fork_base(i).distance = Round(((path_fork_base(i).X1 - path_fork_base(i).X2) ^ 2 + (path_fork_base(i).Y1 - path_fork_base(i).Y2) ^ 2) ^ 0.5, 0)
                Path_fork_Dictionary.Add(path_fork_base(i).From_Point * 100000 + path_fork_base(i).To_Point, path_fork_base(i))

                i += 1
            End If

        End While
        mReader.Close()
        Dim i_cnt As Integer = i
        Array.Resize(path_fork_base, i_cnt)


       
        oConn.Close()
        oConn.Dispose()
    End Sub
    Sub Load_Path_fork(ByVal path_type As String, ByVal tag_id() As Integer, ByRef L1(,) As Integer)
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim Query As String = ""
        Dim mReader As MySqlDataReader
        Dim path_i As Integer = 0
        Dim path(5000) As Path
        Dim tagid_len As Integer = tag_id.Length
        ReDim L1(tagid_len - 1, tagid_len - 1)
        Dim i, j As Integer



        Dim idx As Integer = 0
        Dim action As String
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn

        Query = "SELECT A.From_Point,A.To_Point,A.Forward_Sensor,A.Forward_Speed,A.backward_Sensor,A.backward_Speed,B.X, B.Y, C.X, C.Y ,A.FORK_BACK,A.distance	"
        Query += " FROM `path` A"
        Query += " LEFT JOIN `point` B ON A.`From_Point` = B.Tag_ID"
        Query += " LEFT JOIN `point` C ON A.`To_Point` = C.Tag_ID where A.path_type like '%" + path_type + "%' and  A.active=1 "

        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        While (mReader.Read)

            path(i).From_Point = CInt(mReader.Item(0)) 'from
            path(i).To_Point = CInt(mReader.Item(1)) 'To
            'path_base(path_i).direction = 0
            action = mReader.Item(2)
            If IsNumeric(action.Substring(1, 1)) Then
                idx = 1
            Else
                idx = 2
            End If
            path(i).action0 = action.Substring(0, idx)  '
            path(i).Sensor0 = action.Substring(idx)
            path(i).speed0 = CInt(mReader.Item(3)) ' 
            path(i).Fork_back = CInt(mReader.Item(10)) ' FORK 可不可後退  
            action = mReader.Item(4)
            If IsNumeric(action.Substring(1, 1)) Then
                idx = 1
            Else
                idx = 2
            End If
            path(i).action1 = action.Substring(0, idx)  '
            path(i).Sensor1 = action.Substring(idx)

            If path(i).Fork_back = 0 Then
                path(i).speed1 = 0
            Else
                path(i).speed1 = CInt(mReader.Item(5))
            End If
            path(i).X1 = CInt(mReader.Item(6)) ' 
            path(i).Y1 = CInt(mReader.Item(7)) ' 
            path(i).X2 = CInt(mReader.Item(8)) ' 
            path(i).Y2 = CInt(mReader.Item(9)) ' 
            path(i).distance = Round(((path(i).X1 - path(i).X2) ^ 2 + (path(i).Y1 - path(i).Y2) ^ 2) ^ 0.5, 0)
            path(i).offsetdistance = CInt(mReader.Item(11))
            i += 1

            '逆地圖產生
            path(i).From_Point = CInt(mReader.Item(1)) + 10000 'from
            path(i).To_Point = CInt(mReader.Item(0)) + 10000 'To
            'path_base(path_i).direction = 0
            action = mReader.Item(2)
            If IsNumeric(action.Substring(1, 1)) Then
                idx = 1
            Else
                idx = 2
            End If
            path(i).action0 = action.Substring(0, idx)  '
            path(i).Sensor0 = action.Substring(idx)
            path(i).speed0 = CInt(mReader.Item(3)) ' 

            If path(i).action0 = "FR" Then
                path(i).action0 = "FL"
            ElseIf path(i).action0 = "FL" Then
                path(i).action0 = "FR"
            End If
            path(i).Fork_back = CInt(mReader.Item(10)) ' FORK 可不可後退  
            action = mReader.Item(4)
            If IsNumeric(action.Substring(1, 1)) Then
                idx = 1
            Else
                idx = 2
            End If

            path(i).action1 = action.Substring(0, idx)  '
            path(i).Sensor1 = action.Substring(idx)
            If path(i).action1 = "FR" Then
                path(i).action1 = "FL"
            ElseIf path(i).action1 = "FL" Then
                path(i).action1 = "FR"
            End If
            If path(i).Fork_back = 0 Then
                path(i).speed1 = 0
            Else
                path(i).speed1 = CInt(mReader.Item(5))
            End If

            path(i).X1 = CInt(mReader.Item(8)) ' 
            path(i).Y1 = CInt(mReader.Item(9)) ' 
            path(i).X2 = CInt(mReader.Item(6)) ' 
            path(i).Y2 = CInt(mReader.Item(7)) ' 
            path(i).distance = Round(((path(i).X1 - path(i).X2) ^ 2 + (path(i).Y1 - path(i).Y2) ^ 2) ^ 0.5, 0)
            path(i).offsetdistance = CInt(mReader.Item(11))

            i += 1

        End While
        mReader.Close()
        Dim len As Integer = i
        Dim FromPoint As Integer = 0
        Dim ToPoint As Integer = 0
        Dim flag As Integer = 0
        Dim path_idx As Integer = 0
        Query = "SELECT A.From_Point,A.To_Point,A.Forward_Sensor,A.Forward_Speed,A.backward_Sensor,A.backward_Speed,B.X, B.Y, C.X, C.Y ,A.FORK_BACK	"
        Query += " FROM `path_fork` A"
        Query += " LEFT JOIN `point` B ON A.`From_Point` mod 10000 = B.Tag_ID"
        Query += " LEFT JOIN `point` C ON A.`To_Point` mod 10000 = C.Tag_ID where A.active=1 and not B.X is NULL and not C.X is NULL"

        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        While (mReader.Read)
            FromPoint = CInt(mReader.Item(0)) 'from
            ToPoint = CInt(mReader.Item(1)) 'To
            flag = 0
            For ii As Integer = 0 To len - 1
                If path(ii).From_Point = FromPoint And path(ii).To_Point = ToPoint Then
                    flag = 1
                    path_idx = ii
                    Exit For
                End If
            Next
            If flag = 1 Then
                If CInt(mReader.Item(3)) > 0 Then


                    action = mReader.Item(2)
                    If IsNumeric(action.Substring(1, 1)) Then
                        idx = 1
                    Else
                        idx = 2
                    End If
                    path(path_idx).action0 = action.Substring(0, idx)  '
                    path(path_idx).Sensor0 = action.Substring(idx)
                    path(path_idx).speed0 = CInt(mReader.Item(3)) '
                End If
                If CInt(mReader.Item(5)) > 0 Then


                    action = mReader.Item(4)
                    If IsNumeric(action.Substring(1, 1)) Then
                        idx = 1
                    Else
                        idx = 2
                    End If
                    path(path_idx).action1 = action.Substring(0, idx)  '
                    path(path_idx).Sensor1 = action.Substring(idx)
                    path(path_idx).speed1 = CInt(mReader.Item(5))
                End If
               
            ElseIf flag = 2 Then

                If CInt(mReader.Item(5)) > 0 Then
                    action = mReader.Item(4)
                    If IsNumeric(action.Substring(1, 1)) Then
                        idx = 1
                    Else
                        idx = 2
                    End If
                    path(path_idx).action0 = action.Substring(0, idx)  '
                    path(path_idx).Sensor0 = action.Substring(idx)
                    path(path_idx).speed0 = CInt(mReader.Item(5)) '
                End If
                If CInt(mReader.Item(3)) > 0 Then
                    action = mReader.Item(2)
                    If IsNumeric(action.Substring(1, 1)) Then
                        idx = 1
                    Else
                        idx = 2
                    End If
                    path(path_idx).action1 = action.Substring(0, idx)  '
                    path(path_idx).Sensor1 = action.Substring(idx)
                    path(path_idx).speed1 = CInt(mReader.Item(3))
                End If
                
            Else
            path(i).From_Point = CInt(mReader.Item(0)) 'from
            path(i).To_Point = CInt(mReader.Item(1)) 'To
            action = mReader.Item(2)
            If IsNumeric(action.Substring(1, 1)) Then
                idx = 1
            Else
                idx = 2
            End If
            path(i).action0 = action.Substring(0, idx)  '
            path(i).Sensor0 = action.Substring(idx)
            path(i).speed0 = CInt(mReader.Item(3)) '
            action = mReader.Item(4)
            If IsNumeric(action.Substring(1, 1)) Then
                idx = 1
            Else
                idx = 2
            End If
            path(i).action1 = action.Substring(0, idx)  '
            path(i).Sensor1 = action.Substring(idx)
            path(i).speed1 = CInt(mReader.Item(5))
            path(i).X1 = CInt(mReader.Item(6)) ' 
            path(i).Y1 = CInt(mReader.Item(7)) ' 
            path(i).X2 = CInt(mReader.Item(8)) ' 
            path(i).Y2 = CInt(mReader.Item(9)) ' 
            path(i).distance = Round(((path(i).X1 - path(i).X2) ^ 2 + (path(i).Y1 - path(i).Y2) ^ 2) ^ 0.5, 0)
            i += 1
            End If

        End While
        mReader.Close()



        Dim i_cnt As Integer = i
        Array.Resize(path, i_cnt)
        For i = 0 To tagid_len - 1
            For j = 0 To tagid_len - 1
                If (i = j) Then
                    L1(i, j) = 0
                Else
                    L1(i, j) = 99999
                End If
            Next
        Next
        Try


            For i = 0 To path.Length - 1
                '搜尋
                Dim idx1 As Integer = 0
                Dim idx2 As Integer = 0
                idx1 = Array.IndexOf(tag_id, path(i).From_Point)
                idx2 = Array.IndexOf(tag_id, path(i).To_Point)
                If (path(i).speed0 > 0) Then
                    L1(idx1, idx2) = CInt(path(i).distance / path(i).speed0)  '紀錄距離 正走
                End If
                If (path(i).speed1 > 0) Then
                    L1(idx2, idx1) = CInt(path(i).distance / path(i).speed1) '紀錄距離 逆走
                End If
            Next
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try
        oConn.Close()
        oConn.Dispose()
    End Sub
    Private Sub Button13_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs)
        'Load_Path("", Tag_ID_List)
    End Sub

    Private Sub Button20_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button20.Click
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim ans As Integer = 0
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn

        Try
            If Not ext_cmd_txt.Text = "" Then
                Dim Query As String = "INSERT INTO `agv_cmd_list` ( AGVNo,`CmdFrom`, `CmdTo`, `Pri_Wt`,RequestTime, `Requestor`,ext_cmd) VALUES ('" + txtCar.Text + "','" + Me.From_cb.Text + "', '" + Me.To_cb.Text + "', '50',now(), 'AGVC','" + ext_cmd_txt.Text + "');"
                sqlCommand.CommandText = Query
                ans = sqlCommand.ExecuteNonQuery()
            ElseIf (CInt(Me.From_cb.Text)) < 10 Then
                Dim Query As String = "INSERT INTO `agv_cmd_list` ( AGVNo,`CmdFrom`, `CmdTo`, `Pri_Wt`,RequestTime, `Requestor`) VALUES ('" + txtCar.Text + "','" + Me.From_cb.Text + "', '" + Me.To_cb.Text + "', '50',now(), 'AGVC');"
                sqlCommand.CommandText = Query
                ans = sqlCommand.ExecuteNonQuery()

            Else
                Dim Query As String = "insert into  `agv_cmd_list`(`AGVNo`,`CmdFrom`,`CmdTo`,`Pri_Wt`,`Requestor`,`Shelf_Car_No`,`Shelf_Car_type`,`Shelf_Car_Size`) select '" + txtCar.Text + "' as AGVNo,'" + Me.From_cb.Text + "','" + Me.To_cb.Text + "',50,'AGVC',Shelf_Car_No,`Shelf_Car_type`,`Shelf_Car_Size` from `shelf_car` where LOCATION='" + Me.From_cb.Text + "' "
                sqlCommand.CommandText = Query
                ans = sqlCommand.ExecuteNonQuery()
            End If

        Catch ex As Exception
            MsgBox("派貨失敗" + ex.Message)
        End Try
        If (Not ans = 1) Then
            MsgBox("派貨失敗:來源無架台或目的端有架台")
        End If
        oConn.Close()
        oConn.Dispose()
    End Sub



    Private Sub Button11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Car(view_car_idx).step_i = 999 Then

            Car(view_car_idx).step_i = 903
        Else
            MsgBox("有命令執行中")
        End If
    End Sub


    Private Sub Button6_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        '手動前進
        If Car(view_car_idx).step_i = 999 Then
            Car(view_car_idx).step_i = 21
        Else
            MsgBox("有命令執行中")
        End If
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        '手動後退
        If Car(view_car_idx).step_i = 999 Then
            Car(view_car_idx).step_i = 31
        Else
            MsgBox("有命令執行中")
        End If
    End Sub

    Private Sub Button21_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button21.Click
        '自動轉手動
        If Car(view_car_idx).step_i = 999 Then
            Car(view_car_idx).step_i = 110
        End If
    End Sub
    Private Sub shelf1_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs)
        Dim g As Graphics = e.Graphics
        g.DrawString(Car(view_car_idx).get_Shelf_Car_No.ToString, New Font("Tahoma", 8), Brushes.Black, 2, 15)

        'me.shelf1.g =g.
        'g.DrawString(Tag_point_list(i).TagId.ToString, New Font("Tahoma", 8), Brushes.Black, Tag_point_list(i).X, Tag_point_list(i).Y)
        'Me.shelf1.Image = Image.FromFile("shelf.png")
        'g.DrawImage(a, 0, 0)
        '
        ' Me.shelf1.Image
    End Sub

    Private Sub Button22_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Car(view_car_idx).step_i = 999 Then
            Car(view_car_idx).step_i = 904
        End If
    End Sub



    Private Sub CheckBox3_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBox3.CheckedChanged
        Me.PictureBox1.Invalidate()
    End Sub
    Dim ChargerImg As Image = Image.FromFile("charger.png")
    Private Sub PictureBox1_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles PictureBox1.Paint
        Dim g As Graphics = e.Graphics
        Dim p As Pen = New Pen(Color.Yellow, 2)
        Dim r As Pen = New Pen(Color.Red, 2)
        Dim g_pen As Pen = New Pen(Color.Green, 2)
        'Dim G_Pen As Brush = New Brush()
        Dim drawFormat As New StringFormat
        Dim img As Image = Image.FromFile("shelf.png")
        offset_x = 0
        Try
            offset_y = CInt(Floor_Map.Text)
        Catch ex As Exception
            offset_y = 0
            Floor_Map.Text = "0"
        End Try




        drawFormat.Alignment = StringAlignment.Center
        For i As Integer = 0 To path_base.Length - 1
            g.DrawLine(p, path_base(i).X1 - offset_x, path_base(i).Y1 - offset_y, path_base(i).X2 - offset_x, path_base(i).Y2 - offset_y)
            ' dfh()
        Next

        For j As Integer = 0 To car_no


            Try


                If (Not Car(j).subcmd = "") Then
                    'Car(j).subcmd = Car(j).subcmd.Remove(0, Car(j).subcmd.IndexOf(Car(j).get_tagId.ToString + ","))
                    Dim subcmd_list() As String = Car(j).subcmd.Split(",")
                    For i As Integer = 0 To subcmd_list.Length - 1
                        If subcmd_list(i).Length > 4 Then
                            subcmd_list(i) = subcmd_list(i).Substring(1)
                        End If
                    Next


                    For i As Integer = 0 To path_base.Length - 1
                        For k As Integer = 0 To subcmd_list.Length - 2
                            If path_base(i).From_Point = CInt(subcmd_list(k)) And path_base(i).To_Point = CInt(subcmd_list(k + 1)) Then
                                g.DrawLine(g_pen, path_base(i).X1 - offset_x, path_base(i).Y1 - offset_y, path_base(i).X2 - offset_x, path_base(i).Y2 - offset_y)
                            ElseIf path_base(i).To_Point = CInt(subcmd_list(k)) And path_base(i).From_Point = CInt(subcmd_list(k + 1)) Then
                                g.DrawLine(r, path_base(i).X1 - offset_x, path_base(i).Y1 - offset_y, path_base(i).X2 - offset_x, path_base(i).Y2 - offset_y)
                            End If
                        Next

                    Next

                End If
            Catch ex As Exception
                settext(ex.Message)
            End Try
        Next



        For i As Integer = 0 To Tag_point_list.Length - 1

            If CheckBox3.Checked = True Then
                If Not Tag_point_list(i).name = "NA" Then
                    g.DrawString(Tag_point_list(i).name.ToString, New Font("Tahoma", 8), Brushes.Black, Tag_point_list(i).X - offset_x, Tag_point_list(i).Y - offset_y)
                End If

            Else
                g.DrawString(Tag_point_list(i).TagId.ToString, New Font("Tahoma", 8), Brushes.Black, Tag_point_list(i).X - offset_x, Tag_point_list(i).Y - offset_y)
            End If

        Next
        Dim AGVratio As Double = 0.15
        Dim myBrushR As New System.Drawing.SolidBrush(System.Drawing.Color.Red)
        Dim myBrushG As New System.Drawing.SolidBrush(System.Drawing.Color.Green)
        Dim myBrushB As New System.Drawing.SolidBrush(System.Drawing.Color.Blue)
        For i As Integer = 0 To ChargerClient.Length - 1
            If ChargerClient(i).HoldingResponse(19) > 0 Then
                e.Graphics.FillRectangle(myBrushR, CInt(ChargerClient(i).AXIS_X) - offset_x, CInt(ChargerClient(i).AXIS_Y) - offset_y, CInt(AGVratio * 150), CInt(AGVratio * 150))

            ElseIf ((ChargerClient(i).HoldingResponse(21) << 16) + ChargerClient(i).HoldingResponse(20)) > 0 Then
                '
                e.Graphics.FillRectangle(myBrushB, CInt(ChargerClient(i).AXIS_X) - offset_x, CInt(ChargerClient(i).AXIS_Y) - offset_y, CInt(AGVratio * 150), CInt(AGVratio * 150))
            Else
                e.Graphics.FillRectangle(myBrushG, CInt(ChargerClient(i).AXIS_X) - offset_x, CInt(ChargerClient(i).AXIS_Y) - offset_y, CInt(AGVratio * 150), CInt(AGVratio * 150))
            End If

            g.DrawImage(ChargerImg, CInt(ChargerClient(i).AXIS_X) - offset_x, CInt(ChargerClient(i).AXIS_Y) - offset_y, CInt(AGVratio * 150), CInt(AGVratio * 150))
        Next
        'For i As Integer = 0 To shelf_car.Length - 1

        '    ' g.DrawImage(img, shelf_car(i).X, shelf_car(i).Y)
        '    g.FillRectangle(Brushes.LightGreen, shelf_car(i).X - 24, shelf_car(i).Y - 32, 35, 45)
        'Next
    End Sub

    


    Private Sub Shelf_Car_Pic0_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs)

    End Sub


 

   

    Private Sub Car1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car1.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 1
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try

    End Sub





    Private Sub door_check_timer_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles door_check_timer.Tick
       
        If Me.door_check.IsBusy = False Then
            door_check.RunWorkerAsync()
        End If

        Door_Data_txt.Text = "EQ_Name:" + Door_List(CInt(Door_idx.Text)).EQ_ID.ToString + vbCrLf
        Door_Data_txt.Text += "flag:" + Door_List(CInt(Door_idx.Text)).flag.ToString + vbCrLf
        Door_Data_txt.Text += "Connect:" + Door_List(CInt(Door_idx.Text)).Connect_flag.ToString + vbCrLf
        Door_Data_txt.Text += "<-up_point(wait in):" + Door_List(CInt(Door_idx.Text)).up_point.ToString + vbCrLf
        Door_Data_txt.Text += "<-stop_point(Going):" + Door_List(CInt(Door_idx.Text)).stop_point.ToString + vbCrLf
        Door_Data_txt.Text += "<-down_point:" + Door_List(CInt(Door_idx.Text)).down_point.ToString + vbCrLf
        Door_Data_txt.Text += "->write_up(wait in):" + Door_List(CInt(Door_idx.Text)).write_up.ToString + vbCrLf
        Door_Data_txt.Text += "->write_stop(OK in):" + Door_List(CInt(Door_idx.Text)).write_stop.ToString + vbCrLf
        Door_Data_txt.Text += "->write_down(Release):" + Door_List(CInt(Door_idx.Text)).write_down.ToString + vbCrLf
        Door_Data_txt.Text += "<-up_sensor(Ready):" + Door_List(CInt(Door_idx.Text)).up_sensor.ToString + vbCrLf
        If Door_List(CInt(Door_idx.Text)).control_flag >= 0 Then
            Door_Data_txt.Text += "<-control_flag(car):" + Car(Door_List(CInt(Door_idx.Text)).control_flag).device_no.ToString + vbCrLf
        Else
            Door_Data_txt.Text += "<-control_flag(car):" + Door_List(CInt(Door_idx.Text)).control_flag.ToString + vbCrLf
        End If

        Door_Data_txt.Text += "subcmd:" + Door_List(CInt(Door_idx.Text)).subcmd.ToString + vbCrLf
        Door_Data_txt.Text += "retry:" + Door_List(CInt(Door_idx.Text)).retry.ToString + vbCrLf
        For i As Integer = 0 To Door_List.Length - 1
            If Not Door_List(i).EQ_ID.ToString = "" Then
                Dim log As String = ""
                log = "EQ_Name:" + Door_List(i).EQ_ID.ToString + vbCrLf
                log += "flag:" + Door_List(i).flag.ToString + vbCrLf
                log += "Connect:" + Door_List(i).Connect_flag.ToString + vbCrLf
                log += "<-up_point(wait in):" + Door_List(i).up_point.ToString + vbCrLf
                log += "<-stop_point(Going):" + Door_List(i).stop_point.ToString + vbCrLf
                log += "<-down_point:" + Door_List(i).down_point.ToString + vbCrLf
                log += "->write_up(wait in):" + Door_List(i).write_up.ToString + vbCrLf
                log += "->write_stop(OK in):" + Door_List(i).write_stop.ToString + vbCrLf
                log += "->write_down(Release):" + Door_List(i).write_down.ToString + vbCrLf
                log += "<-up_sensor(Ready):" + Door_List(i).up_sensor.ToString + vbCrLf
                log += "<-control_flag(car):" + Door_List(i).control_flag.ToString + vbCrLf
                log += "subcmd:" + Door_List(i).subcmd.ToString + vbCrLf
                log += "retry:" + Door_List(i).retry.ToString + vbCrLf
                writedoorlog(log)
            End If
        Next
        For i As Integer = 0 To Door_List.Length - 1
            If (Door_List(i).up_sensor = 1) Then
                Door_List(i).Door_Pic.BackColor = Color.Green
            Else
                Door_List(i).Door_Pic.BackColor = Color.Red
            End If
            If Not Door_List(i).Pre_up_sensor = Door_List(i).up_sensor Then
                Dim oConn As MySqlConnection
                Dim sqlCommand As New MySqlCommand
                Dim Query As String = ""
                ' Dim path_i As Integer = 0
                oConn = New MySqlConnection(Mysql_str)
                oConn.Open()
                sqlCommand.Connection = oConn
                Query = "update door set Door_Open=" + Door_List(i).up_sensor.ToString + " where EQ_ID='" + Door_List(i).EQ_ID + "'"
                Try

                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()
                Catch ex As Exception
                    settext(Query + ":異常")
                End Try
                oConn.Close()
                Door_List(i).Pre_up_sensor = Door_List(i).up_sensor
            End If
        Next


    End Sub
    Dim doorcheck As Integer = 0
    Private Sub door_check_DoWork(ByVal sender As System.Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles door_check.DoWork
        settext("DoorCheck")
        doorcheck += 1
        If doorcheck > 100 Then
            doorcheck = 0
        End If
        For j As Integer = 0 To Door_List.Length - 1
            'If Door_List(j).flag = True And Door_List(j).Connect_flag = False Then
            '    'door.flag = True
            '    If Door_List(j).connect() Then
            '        settext(Door_List(j).EQ_ID + ":door OK")
            '    Else
            '        settext(Door_List(j).EQ_ID + ":Door Connect NG")
            '    End If
            'End If
            If doorcheck = 0 Then
                settext("DoorCheck:" + Door_List(j).EQ_ID)
            End If


            If Door_List(j).flag = True Then
                If Door_List(j).retry > 20 Then
                    settext(Door_List(j).EQ_ID + "重新連線")
                    If Door_List(j).connect() = False Then
                        settext(Door_List(j).EQ_ID + "重新連線失敗")
                    End If
                End If

                ' Door_List(j).Write_DO()
                Door_List(j).Read_DI()
                Thread.Sleep(20)
                Door_List(j).Read_DO()
                Thread.Sleep(20)

                For i As Integer = 0 To car_no
                    If Car(i).subcmd = "" Then
                        Car(i).subcmd = Car(i).get_tagId().ToString
                    End If
                    Dim tagid_list() As String = Car(i).subcmd.Split(",")
                    If Not Door_List(j).tagid = 0 And Door_List(j).flag Then
                        If (Car(i).get_tagId = Door_List(j).tagid) Then
                            Door_List(j).DoorUp()
                            settext("DoorUp")
                            Door_List(j).control_flag = i
                        ElseIf Door_In_Array(tagid_list, (Door_List(j).tagid).ToString, DoorSetLen, 3) Or Door_In_Array(tagid_list, (Door_List(j).tagid + 10000).ToString, DoorSetLen, 3) Then
                            '車子走路預計上升的位置，門持續上升
                            Door_List(j).DoorUp()
                            settext("DoorUp")
                            Door_List(j).control_flag = i
                            '開門並取得門的控制權
                        Else
                            If Door_List(j).control_flag = i Then
                                '如果控制權的車子未在門下，關門並釋放控制權
                                Door_List(j).DoorDown()
                                If Door_List(j).up_sensor = 0 Then
                                    '釋放門的控制權
                                    Door_List(j).release()
                                    'door.control_flag = -1
                                End If
                            End If
                        End If
                    End If
                Next

            End If
        Next


    End Sub


 

  



    

   


   


  
 
    Private Sub Label31_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label31.Click

    End Sub

 

    Private Sub Car2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car2.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 2
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try

    End Sub

    Private Sub Car3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car3.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 3
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car4.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 4
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try

    End Sub

    Private Sub Car5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car5.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 5
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try

    End Sub

    Private Sub Car6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car6.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 6
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

   
    Private Sub Car0_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car0.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 0
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try

    End Sub

    Private Sub txtCar_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtCar.SelectedIndexChanged
        view_car_idx = Cidx(CInt(txtCar.Text))
        If Car(Cidx(CInt(txtCar.Text))).online Then
            Button7.Text = "ONLINE"
        Else
            Button7.Text = "OFFLINE"
        End If
    End Sub
    Function Check_SQL_idx(ByVal idx As Integer) As Boolean
        Check_SQL_idx = False

        For i As Integer = 0 To Me.ListView1.Items.Count - 1
            '檢查命令是否存在
            If idx = CInt(Me.ListView1.Items(i).SubItems(0).Text) Then
                Check_SQL_idx = True
            End If
        Next

    End Function

 

    Private Sub ToolStripMenuItem3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem3.Click
        add_cmd.txtCar.Text = Car(view_car_idx).device_no.ToString
        add_cmd.From_cb.Text = "0"
        add_cmd.sql_str.Text = Mysql_str

        add_cmd.Show()
    End Sub

  
  
    Private Sub ContextMenuStrip2_Opening(ByVal sender As System.Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles ContextMenuStrip2.Opening
        Dim CM As ContextMenuStrip = sender
        Dim picbox_no As String = CM.SourceControl.Name.Substring(3)

        If IsNumeric(picbox_no) Then
            view_car_idx = CInt(picbox_no)
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End If
        ' MsgBox(a.SourceControl.Name)
    End Sub

    Private Sub Button1_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Door_List(CInt(Door_idx.Text)).DoorUp()

    End Sub






    Private Sub Button15_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button15.Click

        '如果控制權的車子未在門下，關門並釋放控制權
        Door_List(CInt(Door_idx.Text)).DoorDown()
        If Door_List(CInt(Door_idx.Text)).up_point = 0 Then
            '釋放門的控制權
            Door_List(CInt(Door_idx.Text)).release()
            'door.control_flag = -1
        End If

    End Sub

   

    Private Sub Button11_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button11.Click
        For i As Integer = 0 To car_no
            Car(i).online = True
        Next
        Button7.Text = "ONLINE"
    End Sub

    Private Sub Button8_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        For i As Integer = 0 To car_no
            Car(i).online = False
        Next
        Button7.Text = "OFFLINE"
    End Sub
    Function Cidx(ByVal device_no As Integer) As Integer
        Cidx = 0
        For i As Integer = 0 To car_no
            If Car(i).device_no = device_no Then
                cidx = i
            End If
        Next
        Return Cidx
    End Function

    Private Sub Button16_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs)
      
    End Sub

  

   
    Private Sub Floor_Map_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Floor_Map.SelectedIndexChanged
        Me.PictureBox1.Invalidate()
    End Sub



  
    Private Sub Car7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car7.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 7
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try

    End Sub

    Private Sub Car8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car8.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 8
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try

    End Sub

    Private Sub Car9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car9.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 9
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car10.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 10
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car11.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 11
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try

    End Sub

  


  

    Private Sub LFT_timer_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles LFT_timer.Tick
        If Me.LFT_bgwork.IsBusy = False Then
            LFT_bgwork.RunWorkerAsync()
        End If
        For i As Integer = 0 To 15
            Dim lbR As Label = Me.Controls.Find("LFT_R" + i.ToString(), True)(0)
            Dim lbW As Label = Me.Controls.Find("LFT_W" + i.ToString(), True)(0)
            Dim distr As String = ""
            Dim dostr As String = ""
            If LFT_List(CInt(LFT_idx.Text)).From_LFT_DI(i) = 1 Then
                lbR.BackColor = Color.Green
                distr += "1"
            Else
                lbR.BackColor = Color.Gray
                distr += "0"
            End If
            If LFT_List(CInt(LFT_idx.Text)).From_LFT_DO(i) = 1 Then
                dostr += "1"
                lbW.BackColor = Color.Green
            Else
                lbW.BackColor = Color.Gray
                dostr += "0"
            End If
            '   LFT_FLOOR.Text = LFT_List(CInt(LFT_idx.Text)).From_LFT_DI(12) + LFT_List(CInt(LFT_idx.Text)).From_LFT_DI(13) * 2 + LFT_List(CInt(LFT_idx.Text)).From_LFT_DI(14) * 4 + LFT_List(CInt(LFT_idx.Text)).From_LFT_DI(15) * 8
        Next
        For i As Integer = 0 To LFT_List.Length - 1
            If LFT_List(i).flag = True Then
                Dim floor_no As Integer = 0
                floor_no = (LFT_List(i).From_LFT_DI(12) + LFT_List(i).From_LFT_DI(13) * 2 + LFT_List(i).From_LFT_DI(14) * 4 + LFT_List(i).From_LFT_DI(15) * 8)
                LFT_List(i).LFT_Pic.Text = floor_no
                If (LFT_List(i).From_LFT_DI(7) = 1 And LFT_List(i).From_LFT_DI(9) = 1) Then
                    LFT_List(i).LFT_Pic.BackColor = Color.Green
                Else
                    LFT_List(i).LFT_Pic.BackColor = Color.Red
                End If
                If LFT_List(i).Pre_floor_no = floor_no And LFT_List(i).Pre_open_sensor = LFT_List(i).open_sensor Then
                Else
                    LFT_List(i).Pre_open_sensor = LFT_List(i).open_sensor
                    LFT_List(i).Pre_floor_no = floor_no
                    Dim oConn As MySqlConnection
                    Dim sqlCommand As New MySqlCommand
                    Dim Query As String = ""
                    ' Dim path_i As Integer = 0
                    oConn = New MySqlConnection(Mysql_str)
                    oConn.Open()
                    sqlCommand.Connection = oConn
                    Dim carno As String = "-1"
                    If LFT_List(i).control_flag > -1 Then
                        carno = Car(LFT_List(i).control_flag).device_no.ToString
                    End If
                    Query = "update lft set Door_Open=" + LFT_List(i).open_sensor.ToString + ",floor_no=" + floor_no.ToString + ",DI='" + LFT_List(i).sFrom_LFT_DI + "',DO='" + LFT_List(i).sFrom_LFT_DO + "',Stepi=" + LFT_List(i).LFT_STEP_I.ToString + ",ControlCar=" + carno + " where EQ_ID='" + LFT_List(i).EQ_ID + "'"
                    Try

                        sqlCommand.CommandText = Query
                        sqlCommand.ExecuteNonQuery()
                    Catch ex As Exception
                        settext(Query + ":異常")
                    End Try
                    oConn.Close()

                End If
            End If
        Next
        LFT_FLOOR.Text = (LFT_List(CInt(LFT_idx.Text)).From_LFT_DI(12) + LFT_List(CInt(LFT_idx.Text)).From_LFT_DI(13) * 2 + LFT_List(CInt(LFT_idx.Text)).From_LFT_DI(14) * 4 + LFT_List(CInt(LFT_idx.Text)).From_LFT_DI(15) * 8).ToString
        Try

            LFT_name.Text = LFT_List(CInt(LFT_idx.Text)).EQ_ID + ":" + LFT_List(CInt(LFT_idx.Text)).tagid.ToString
            opendata.Text = "open:" + LFT_List(CInt(LFT_idx.Text)).open_sensor.ToString + LFT_List(CInt(LFT_idx.Text)).EQ_ID.ToString
            step_text.Text = "step:" + LFT_List(CInt(LFT_idx.Text)).LFT_STEP_I.ToString

            If LFT_List(CInt(LFT_idx.Text)).control_flag >= 0 Then
                Car_text.Text = "Car:" + Car(LFT_List(CInt(LFT_idx.Text)).control_flag).device_no.ToString
            Else
                Car_text.Text = "Car:" + LFT_List(CInt(LFT_idx.Text)).control_flag.ToString
            End If
        Catch ex As Exception
            settext("LFT info ERROR")
        End Try
    End Sub

    Sub writeLFTlog(ByVal str As String)
        Try
            Dim sw As StreamWriter = New StreamWriter(".\log\" + Now().ToString("yyyyMMdd") + "_toLFT.log", True, Encoding.Default)
            sw.Write(Now.ToString + ":" + str + vbCrLf)
            sw.Flush()
            sw.Close()
        Catch ex As Exception

        End Try

    End Sub
    Private Sub LFT_bgwork_DoWork(ByVal sender As System.Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles LFT_bgwork.DoWork
        Dim debuglog As String = ""
        For j As Integer = 0 To LFT_List.Length - 1

            If LFT_List(j).flag = True And LFT_List(j).Connect_flag = True Then
                ' LFT_List(j).LFT_Main(0, 0, 0)
                ' LFT_List(j).control_flag = -1


                For i As Integer = 0 To car_no
                    If (select_LFT(LFT_List(j).tagid, Car(i).subcmd) Or select_LFT(LFT_List(j).tagid, Car(i).get_tagId.ToString)) And (LFT_List(j).control_flag = i Or LFT_List(j).control_flag = -1) Then
                        'If (select_LFT(LFT_List(j).tagid, Car(i).get_tagId.ToString)) And Car(i).get_auto = 0 Then
                        '點位被預約就接管電梯控制權，並控制電梯

                        'select_floor(Car(i).main_subcmd, LFT_List(j).tagid)

                        debuglog = LFT_List(j).LFT_Main(select_from_floor(Car(i).main_subcmd, LFT_List(j).tagid), select_to_floor(Car(i).main_subcmd, LFT_List(j).tagid), Car(i).get_tagId)

                        LFT_List(j).control_flag = i
                        Dim LFTDI As String = LFT_List(j).tagid.ToString + ",DI:"
                        Dim LFTDO As String = LFT_List(j).tagid.ToString + ",DO:"
                        For jj As Integer = 0 To 15
                            LFTDI += LFT_List(j).From_LFT_DI(jj).ToString
                            LFTDO += LFT_List(j).From_LFT_DO(jj).ToString
                        Next
                        If LFT_List(j).control_flag > -1 Then
                            writeLFTlog(LFT_List(j).tagid.ToString + ",STEP:" + LFT_List(j).LFT_STEP_I.ToString + ",control_car:" + Car(LFT_List(j).control_flag).device_no.ToString + ",DoWork")
                        Else
                            writeLFTlog(LFT_List(j).tagid.ToString + ",STEP:" + LFT_List(j).LFT_STEP_I.ToString + ",control_car:" + LFT_List(j).control_flag.ToString + ",DoWork")
                        End If
                        LFT_List(j).sFrom_LFT_DI = LFTDI
                        LFT_List(j).sFrom_LFT_DO = LFTDO
                        writeLFTlog(LFTDO)
                        writeLFTlog(LFTDI)



                        ' settext("LFT:" + Query_Floor(Car(i).from_pos).ToString + "," + Query_Floor(Car(i).To_pos).ToString + "," + Car(i).get_tagId.ToString + vbCrLf + "control_flag:" + i.ToString)
                        settext(debuglog)
                    Else
                        '釋放控制權
                        If LFT_List(j).control_flag = i Then
                            '如果控制權的車子未在LFT點位，釋放控制權
                            '釋放門的控制權
                            LFT_List(j).release()

                        End If
                    End If

                Next
                If LFT_List(j).control_flag = -1 Then
                    debuglog = LFT_List(j).LFT_Main(0, 0, 0)
                    settext(debuglog)
                    If LFT_List(j).From_LFT_DI(0) = 1 Then
                        LFT_List(j).release()
                    End If
                End If
            ElseIf LFT_List(j).flag = True Then
                settext(LFT_List(j).tagid.ToString + "LFT Connect_flag")
                If LFT_List(j).retry > 20 Then
                    settext(LFT_List(j).tagid.ToString + "LFT ReConnect")
                    LFT_List(j).connect()
                    LFT_List(j).retry = 0
                End If
            End If
        Next
    End Sub
    Function select_from_floor(ByVal subcmd As String, ByVal lft_tagid As Integer)

        select_from_floor = 0
        If Not subcmd = "" Then


            Dim subcmd_list() As String = subcmd.Split(",")
            For i As Integer = 1 To subcmd_list.Length - 1
                If CInt(subcmd_list(i)) = lft_tagid Or CInt(subcmd_list(i)) = lft_tagid + 10000 Then
                    select_from_floor = Query_Floor(CInt(subcmd_list(i - 1)))
                End If
            Next
        End If
        'start_cmd = subcmd.Remove(0, subcmd.IndexOf(tagid + ","))
        ' start_cmd = subcmd.Remove(0, subcmd.IndexOf(lft_tagid + ","))
    End Function
    Function select_to_floor(ByVal subcmd As String, ByVal lft_tagid As Integer)

        select_to_floor = 0

        If Not subcmd = "" Then
            Dim subcmd_list() As String = subcmd.Split(",")
            For i As Integer = 0 To subcmd_list.Length - 2
                If CInt(subcmd_list(i)) = lft_tagid Or CInt(subcmd_list(i)) = lft_tagid + 10000 Then
                    select_to_floor = Query_Floor(CInt(subcmd_list(i + 1)))
                End If
            Next
        End If
        'start_cmd = subcmd.Remove(0, subcmd.IndexOf(tagid + ","))
        ' start_cmd = subcmd.Remove(0, subcmd.IndexOf(lft_tagid + ","))
    End Function
  

    Private Sub Button23_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button23.Click
        'LFT_List(0).release()
        'LFT_List(0).write_do()
        LFT_List(CInt(LFT_idx.Text)).LFT_STEP_I = 0
        LFT_List(CInt(LFT_idx.Text)).control_flag = -1
        LFT_List(CInt(LFT_idx.Text)).release()

    End Sub
    Function Query_Floor(ByVal tagid As Integer) As Integer
        Query_Floor = 0
        tagid = tagid Mod 10000
        If Tag_point_Dictionary.ContainsKey(tagid) And tagid > 0 Then
            Return Tag_point_Dictionary(tagid).floor_no
        End If
    End Function
    Function select_LFT(ByVal LFT_tagid As Integer, ByVal subcmd As String)
        Dim path_list() As String = subcmd.Split(",")
        select_LFT = False
        ' For Each Val As String In path_list
        For i As Integer = 0 To 7
            If In_Array(path_list, (LFT_tagid + i).ToString, 3) Then
                Return True
            End If
            '逆地圖也可以成立
            If In_Array(path_list, (LFT_tagid + i + 10000).ToString, 3) Then
                Return True
            End If
        Next
        ' Next


    End Function

   
   
    Function Send2AGV(ByVal car_idx As Integer, ByVal cmdtype As Integer)

        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim Query As String = ""
        Dim i As Integer = 0
        Send2AGV = ""
        Dim old_subcmd As String = Car(car_idx).subcmd
        If Not Car(car_idx).cmd_idx = -2 And Not Car(car_idx).To_pos = 0 Then

            oConn = New MySqlConnection(Mysql_str)
            oConn.Open()
            sqlCommand.Connection = oConn

            If Car(car_idx).sflag = 0 Then
                Dim path_ary() As String
                Dim path_output As String = ""
                Dim last_direction As Integer
                Dim cmd_to_point As Integer = 0
                cmd_to_point = Car(car_idx).To_pos
                Car(car_idx).subcmd_req = ""
                If Car(car_idx).get_pin = 10 And Car(car_idx).get_Shelf_Car_No > 0 And Car(car_idx).cmd_Shelf_Car_No = 0 Then
                    Dim mysql_Object As Object
                    Dim temp_Shelf_Car_type As String = ""
                    Try
                        Query = "SELECT Shelf_Car_type FROM `shelf_car` where Shelf_Car_No=" + Car(car_idx).get_Shelf_Car_No.ToString
                        sqlCommand.CommandText = Query
                        mysql_Object = sqlCommand.ExecuteScalar()
                        temp_Shelf_Car_type = mysql_Object.ToString
                        Car(car_idx).cmd_Shelf_Car_No = Car(car_idx).get_Shelf_Car_No
                        Car(car_idx).cmd_Shelf_Car_Type = temp_Shelf_Car_type
                    Catch ex As Exception
                        settext(Query + ":" + ex.Message)
                    End Try
                    Try

                        Query = "update  `agv_cmd_list` set Shelf_Car_No=" + Car(car_idx).cmd_Shelf_Car_No.ToString + ",Shelf_Car_type='" + Car(car_idx).cmd_Shelf_Car_Type + "' where CmdKey=" + Car(car_idx).cmd_sql_idx.ToString
                        sqlCommand.CommandText = Query
                        sqlCommand.ExecuteNonQuery()
                    Catch ex As Exception
                        settext(Query + ":" + ex.Message)
                    End Try
                End If
                '選擇路徑->塞住選擇第二路徑->塞住->選擇退避點
                '選擇路徑->塞住選擇第二路徑->塞住->選擇退避點


                If Car(car_idx).get_pin = 10 And Not Car(car_idx).cmd_Shelf_Car_Type = "" Then
                    For ii As Integer = 1 To Dijkstra_list.Length - 1
                        '判斷載貨的物品，使用該物品的路徑
                        If Car(car_idx).cmd_Shelf_Car_Type = Dijkstra_list(ii).name Then
                            If (Car(car_idx).Car_type = "FORK" Or Car(car_idx).Car_type = "LOWCAR") And Car(car_idx).get_loading() = 3 Then

                                Car(car_idx).subcmd = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(ii).ary, Tag_ID_Fork_List, Car(car_idx).get_tagId, cmd_to_point.ToString, Car(car_idx).Block_Point)
                            ElseIf Car(car_idx).Car_type = "FORK" Or Car(car_idx).Car_type = "LOWCAR" Then
                                Car(car_idx).subcmd = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(1).ary, Tag_ID_Fork_List, Car(car_idx).get_tagId, cmd_to_point.ToString, Car(car_idx).Block_Point)
                            Else
                                Car(car_idx).subcmd = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(ii).ary, Tag_ID_List, Car(car_idx).get_tagId, cmd_to_point.ToString, Car(car_idx).Block_Point)
                            End If
                        End If
                    Next
                Else
                    '空車用原始路徑
                    If Car(car_idx).Car_type = "FORK" Or Car(car_idx).Car_type = "LOWCAR" Then
                        Car(car_idx).subcmd = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(1).ary, Tag_ID_Fork_List, Car(car_idx).get_tagId, cmd_to_point.ToString, Car(car_idx).Block_Point)
                    Else
                        Car(car_idx).subcmd = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, Car(car_idx).get_tagId, cmd_to_point.ToString, Car(car_idx).Block_Point)
                    End If

                End If


                If Not Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString() Then
                    Dim car_subcmd As String = ""
                    Car(car_idx).main_subcmd = Car(car_idx).subcmd
                    settext(Car(car_idx).device_no.ToString + ":main_subcmd" + Car(car_idx).main_subcmd, True, Car(car_idx).device_no)
                    For k As Integer = 0 To car_no
                        If (Not k = car_idx) Then
                            If Car(k).subcmd = "" Then
                                Car(k).subcmd = Car(k).get_tagId
                            End If
                            If Car(k).subcmd = "" Then
                                Car(k).subcmd = Car(k).get_tagId().ToString
                            End If
                            car_subcmd = Get_group_path(Car(k).subcmd) '包含原本的subcmd
                            Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, car_subcmd)
                        End If
                        If Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString Then '判斷被k截斷了
                            Dim temp As String = Cut_before_Path(Car(k).main_subcmd, Car(k).get_tagId.ToString)
                            settext(Car(car_idx).device_no.ToString + ":main_cmd=" + Car(car_idx).main_subcmd, True, Car(car_idx).device_no)
                            settext(Car(car_idx).device_no.ToString + ":目的地被" + Car(k).device_no.ToString + "擋住:" + Car(k).subcmd + "(" + Car(k).get_tagId.ToString + ")grouppath=" + car_subcmd, True, Car(car_idx).device_no)
                            Car(car_idx).subcmd_req = Car(k).device_no.ToString
                            If Car(k).subcmd = "" Or Car(k).subcmd = Car(k).get_tagId.ToString Or (In_Subcmd(temp, Car(car_idx).get_tagId().ToString) And Car(k).status = 4) Then 'k車也沒有動作 要排除 電梯與滑升門 不然等待滑升門的時候會亂跑
                                '判斷為對峙或是前方車輛無法通行
                                Dim LftFlag As Boolean = CheckCarInLft(Car(k).get_tagId(), Car(k).main_subcmd)
                                Dim DoorFlag As Boolean = CheckCarInDoor(Car(k).get_tagId())
                                If In_Subcmd(temp, Car(car_idx).get_tagId().ToString) Then
                                    settext(Car(car_idx).device_no.ToString + "(" + Car(car_idx).get_tagId().ToString + "):" + "k車:" + Car(k).main_subcmd + " autotemp:" + temp)
                                End If

                                settext(Car(car_idx).device_no.ToString + ":" + "判斷前車無動作", True, Car(car_idx).device_no)
                                If Car(k).cmd_sql_idx = 0 Then
                                    Dim ans As Integer
                                    ' ans = Send_CMD(Car(k).device_no, 0, 240)
                                    If Car(k).wait_point > 0 Then
                                        If Car(car_idx).Car_type = "LFT" Then
                                            ans = Send_CMD(Car(k).device_no, 0, Car(k).wait_point, "30")
                                        Else
                                            ans = Send_CMD(Car(k).device_no, 1, Car(k).wait_point)
                                        End If

                                    End If
                                ElseIf LftFlag Then
                                    settext(Car(car_idx).device_no.ToString + ":" + "前車在電梯程序", True, Car(car_idx).device_no)
                                    '乖乖停好步要動
                                ElseIf DoorFlag Then
                                    settext(Car(car_idx).device_no.ToString + ":" + "前車在開門程序", True, Car(car_idx).device_no)
                                    '乖乖停好步要動
                                Else

                                    If Car(car_idx).Car_type = "FORK" Or Car(car_idx).Car_type = "LOWCAR" Then ' FORK 不能退避

                                        Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString  ' Dijkstra_fn_ary(Dijkstra_list(1).ary, Tag_ID_Fork_List, Car(car_idx).get_tagId, cmd_to_point, Car(k).get_tagId.ToString + "," + Car(car_idx).Block_Point)
                                        settext(Car(car_idx).device_no.ToString + ":" + "不能選第二條", True, Car(car_idx).device_no)
                                    ElseIf Car(car_idx).get_pin = 10 And Not Car(car_idx).cmd_Shelf_Car_Type = "" Then
                                        For ii As Integer = 1 To Dijkstra_list.Length - 1
                                            If Car(car_idx).cmd_Shelf_Car_Type = Dijkstra_list(ii).name And Car(car_idx).get_pin = 10 Then
                                                Car(car_idx).subcmd = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(ii).ary, Tag_ID_List, Car(car_idx).get_tagId, cmd_to_point, (Car(k).get_tagId Mod 10000).ToString + "," + Car(car_idx).Block_Point)
                                            End If
                                        Next
                                    Else
                                        Car(car_idx).subcmd = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, Car(car_idx).get_tagId, cmd_to_point, (Car(k).get_tagId Mod 10000).ToString + "," + Car(car_idx).Block_Point)
                                    End If


                                    settext(Car(car_idx).device_no.ToString + ":" + "選擇第二路徑" + Car(car_idx).subcmd, True, Car(car_idx).device_no)
                                    If Not Car(car_idx).subcmd = "" Then
                                        Car(car_idx).sflag = 1
                                    End If

                                    '沒有第二條路徑，退到退避點
                                    For ii As Integer = 0 To car_no
                                        If (Not ii = car_idx) Then
                                            If Car(ii).subcmd = "" Then
                                                Car(ii).subcmd = Car(ii).get_tagId().ToString
                                            End If
                                            car_subcmd = Get_group_path(Car(ii).subcmd) '包含原本的subcmd
                                            Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, car_subcmd)
                                        End If
                                    Next
                                    If Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString Then

                                        Dim temp_point As String = ""
                                        Query = "SELECT  group_concat(Tag_ID) "
                                        Query += " from (SELECT A.Tag_ID,( A.X - B.X ) * ( A.X - B.X ) + ( A.Y - B.Y ) * ( A.Y - B.Y ) AS dist "
                                        Query += " FROM `point` A , `point` B  where A.Retreat_Flag=1 and B.Tag_ID=" + Car(car_idx).get_tagId.ToString
                                        Query += "  and A.floor_no=B.floor_no and not A.`Tag_ID` in (select CmdTo FROM `agv_cmd_list` )  and not A.`Tag_ID`  in (select Position FROM `agv_list` )"
                                        Query += " order by dist ASC limit 0,10 ) C"
                                        sqlCommand.CommandText = Query
                                        temp_point = sqlCommand.ExecuteScalar.ToString
                                        car_subcmd = Get_group_path(Car(k).subcmd) '包含原本的subcmd
                                        If Car(car_idx).Car_type = "FORK" Or Car(car_idx).Car_type = "LOWCAR" Then

                                            Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString  ' Dijkstra_fn_ary(Dijkstra_list(1).ary, Tag_ID_Fork_List, Car(car_idx).get_tagId, cmd_to_point, Car(k).get_tagId.ToString + "," + Car(car_idx).Block_Point)
                                            settext(Car(car_idx).device_no.ToString + ":" + "不能選退避", True, Car(car_idx).device_no)
                                            ' Car(car_idx).subcmd = Dijkstra_fn_ary(Dijkstra_list(1).ary, Tag_ID_Fork_List, Car(car_idx).get_tagId, temp_point, Car(k).get_tagId.ToString + "," + Car(car_idx).Block_Point.ToString)
                                        ElseIf Car(car_idx).get_pin = 10 And Not Car(car_idx).cmd_Shelf_Car_Type = "" Then
                                            For ii As Integer = 1 To Dijkstra_list.Length - 1
                                                If Car(car_idx).cmd_Shelf_Car_Type = Dijkstra_list(ii).name And Car(car_idx).get_pin = 10 Then
                                                    Car(car_idx).subcmd = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(ii).ary, Tag_ID_List, Car(car_idx).get_tagId, temp_point, (Car(k).get_tagId Mod 10000).ToString + "," + car_subcmd + "," + Car(car_idx).Block_Point.ToString)
                                                End If
                                            Next
                                        Else
                                            Car(car_idx).subcmd = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, Car(car_idx).get_tagId, temp_point, (Car(k).get_tagId Mod 10000).ToString + "," + car_subcmd + "," + Car(car_idx).Block_Point.ToString)
                                        End If

                                        settext(Car(car_idx).device_no.ToString + ":" + "回到退避點" + Car(car_idx).subcmd, True, Car(car_idx).device_no)
                                        Car(car_idx).sflag = 1
                                    End If
                                End If
                            Else
                                'k車還有命令 那就先等待  目前還沒有規劃其他工作
                                '預計可以提早退避
                                If In_Subcmd(Car(k).main_subcmd, Car(car_idx).get_tagId().ToString) Then
                                    '預計可以提早退避 k車的main_subcmd 有包含現在的車
                                    settext(Car(k).device_no.ToString + ":可以提早退避" + Car(k).subcmd, True, Car(car_idx).device_no)
                                End If


                                'settext(Car(car_idx).device_no.ToString + ":" + Car(car_idx).main_subcmd, True, Car(car_idx).device_no)


                            End If
                            Car(car_idx).main_subcmd = Car(car_idx).subcmd
                            '重新過濾其他車子的路徑
                            For ii As Integer = 0 To car_no
                                If (Not ii = car_idx) Then
                                    If Car(ii).subcmd = "" Then
                                        Car(ii).subcmd = Car(ii).get_tagId().ToString
                                    End If
                                    car_subcmd = Get_group_path(Car(ii).subcmd) '包含原本的subcmd
                                    Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, car_subcmd)
                                End If
                            Next

                            Exit For
                        End If

                    Next
                    If (Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString) Then
                        settext(Car(car_idx).device_no.ToString + ":無路徑:其他車子", True, Car(car_idx).device_no)
                        Car(car_idx).subcmd_req += "車擋住"
                    End If

                Else


                    Car(car_idx).To_AGV(20) = 111
                    settext(Car(car_idx).device_no.ToString + ":" + Car(car_idx).get_tagId.ToString + "TO DEST " + cmd_to_point.ToString)
                    '無到達路徑
                End If


                If Not (Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString()) Then


                    For k As Integer = 0 To Door_List.Length - 1
                        If Door_List(k).flag Then
                            If (Door_List(k).up_sensor = 1 And Door_List(k).write_down = 0 And Door_List(k).control_flag = car_idx) Or Door_List(k).tagid = Car(car_idx).get_tagId Then
                                ' da()
                            Else

                                Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, Door_List(k).tagid.ToString)

                            End If
                        End If

                    Next
                    If Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString() Then
                        settext(Car(car_idx).device_no.ToString + ":無路徑door", True, Car(car_idx).device_no)
                        Car(car_idx).subcmd_req = "等開門"
                    End If
                End If
                '處理充電站
                'ChargerClient(i).HoldingResponse(7)

                If Not (Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString()) Then


                    For k As Integer = 0 To ChargerClient.Length - 1
                        If ChargerClient(k).flag Then
                            If ChargerClient(i).HoldingResponse(7) = 0 Then
                                ' da()

                            Else

                                Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, ChargerClient(k).tag_id.ToString)
                                Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, (ChargerClient(k).tag_id + 1).ToString)
                                Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, (ChargerClient(k).tag_id - 1).ToString)
                            End If
                        End If

                    Next
                    If Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString() Then
                        settext(Car(car_idx).device_no.ToString + ":充電站電極伸出", True, Car(car_idx).device_no)
                        Car(car_idx).subcmd_req = "電極伸出"
                    End If
                End If

                '處理電梯
                If Not (Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString()) Then
                    For k As Integer = 0 To LFT_List.Length - 1
                        If LFT_List(k).flag Then
                            '如果沒有人預約 就只擋1000
                            '如果有人預約就擋除了OPEN的樓層
                            If LFT_List(k).control_flag = -1 Or LFT_List(k).open_sensor = 0 Then
                                '電梯沒被車子預約或是沒開門
                                Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, LFT_List(k).tagid.ToString)
                            Else
                                '電梯被車子預約
                                If LFT_List(k).control_flag = car_idx Then
                                    '預約電梯的車子
                                    If LFT_List(k).open_sensor = Query_Floor(Car(car_idx).get_tagId) Or Car(car_idx).get_tagId = LFT_List(k).tagid Or Car(car_idx).get_tagId = LFT_List(k).tagid + 10000 Then
                                        '相同樓層且開門 1000 不擋
                                    Else
                                        Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, LFT_List(k).tagid.ToString)
                                    End If
                                    ' Dim debug As Integer = Query_Floor(Car(car_idx).To_pos)

                                    For ii As Integer = 1 To 7
                                        If Query_Floor(Car(car_idx).get_tagId) = ii Then
                                            settext((ii + LFT_List(k).tagid).ToString + "不擋1", True, Car(car_idx).device_no)
                                        ElseIf (Car(car_idx).get_tagId = LFT_List(k).tagid Or Car(car_idx).get_tagId = LFT_List(k).tagid + 10000) And select_to_floor(Car(car_idx).main_subcmd, LFT_List(k).tagid) = LFT_List(k).open_sensor Then
                                            '如果車子在電梯內且開門的樓層是出口的樓層就不擋
                                            settext((ii + LFT_List(k).tagid).ToString + "不擋2", True, Car(car_idx).device_no)
                                        Else
                                            Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, (ii + LFT_List(k).tagid).ToString)
                                        End If
                                    Next
                                Else
                                    '  Dim lft_path As String = ""
                                    For ii As Integer = LFT_List(k).tagid To LFT_List(k).tagid + 7
                                        Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, ii.ToString)
                                    Next
                                End If
                            End If

                            'If (LFT_List(k).open_sensor > 0 And LFT_List(k).control_flag = car_idx) Or LFT_List(k).tagid = Car(car_idx).get_tagId Then
                            '    ' da()
                            'Else
                            '    Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, LFT_List(k).tagid.ToString)

                            'End If
                        End If

                    Next
                    If Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString Then
                        Car(car_idx).subcmd_req = "等待電梯"
                        settext(Car(car_idx).device_no.ToString + ":無路徑LFT", True, Car(car_idx).device_no)
                    End If
                End If
                'If Car(car_idx).subcmd.Length > 4 * 5 Then
                '    Car(car_idx).subcmd = Car(car_idx).subcmd.Remove(Car(car_idx).subcmd.IndexOf(",", CInt(Car(car_idx).subcmd.Length / 2)))
                'End If
                If Not (Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString) Then

                    If Car(car_idx).subcmd.Split(",").Length > PathLen And Car(car_idx).sflag = 0 Then
                        Dim path_temp() As String = Car(car_idx).subcmd.Split(",")
                        Car(car_idx).subcmd = Car(car_idx).subcmd.Remove(Car(car_idx).subcmd.IndexOf("," + path_temp(PathLen - 1) + ",")) ' 移除
                    ElseIf Car(car_idx).subcmd.Split(",").Length > RetreatPath And Car(car_idx).sflag = 1 Then
                        Dim path_temp() As String = Car(car_idx).subcmd.Split(",")
                        Car(car_idx).subcmd = Car(car_idx).subcmd.Remove(Car(car_idx).subcmd.IndexOf("," + path_temp(RetreatPath - 1) + ",")) ' 移除
                    End If


                    If Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString Then
                        settext(Car(car_idx).device_no.ToString + ":無路徑temp_path 5", True, Car(car_idx).device_no)
                    End If
                End If
                '決定暫時路徑
                Send2AGV = Car(car_idx).subcmd
                If Car(car_idx).get_tagId = cmd_to_point Then
                    '目的地跟現在地一致
                    ' Car(car_idx).cmd_idx += 1
                    Car(car_idx).move_flag = True
                    Car(car_idx).Wait_count = 0
                    Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString

                    settext(Car(car_idx).device_no.ToString + ":無路徑6" + "-cmd_to_point" + cmd_to_point.ToString, True, Car(car_idx).device_no)

                ElseIf Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString Then
                    If Car(car_idx).get_status = 4 Then
                        Car(car_idx).step_i = 902 '如國沒有路徑，就強停
                    End If
                    Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString

                    settext(Car(car_idx).device_no.ToString + ":無路徑7", True, Car(car_idx).device_no)

                ElseIf Not Car(car_idx).subcmd = "" Then
                    path_ary = Car(car_idx).subcmd.Split(",")
                    Dim sensor_offset As Integer = 0

                    If Car(car_idx).get_pin = 10 And Car(car_idx).cmd_Shelf_Car_No And Car(car_idx).get_loading = 3 And Car(car_idx).Car_type = "FORK" Then
                        sensor_offset = Query_shelf_sensoroffset(Car(car_idx).cmd_Shelf_Car_No)
                    ElseIf Car(car_idx).get_pin = 10 And Car(car_idx).cmd_Shelf_Car_No And Not Car(car_idx).Car_type = "FORK" Then
                        sensor_offset = Query_shelf_sensoroffset(Car(car_idx).cmd_Shelf_Car_No)
                    End If


                    If sensor_offset > 0 Then
                        settext(Car(car_idx).device_no.ToString + ":sensor_offset=" + sensor_offset.ToString, True, Car(car_idx).device_no)
                    End If

                    If Not path_ary.Length = 1 Then
                        '超過兩個點位    
                        For i = 0 To path_ary.Length - 2
                            Dim flag As Boolean = False
                            If i <= (path_ary.Length - 3) Then
                                For j As Integer = 0 To path_S.Length - 1
                                    If path_ary(i) = path_S(j).From_Point And path_ary(i + 1) = path_S(j).M_Point And path_ary(i + 2) = path_S(j).To_Point Then
                                        flag = True

                                        If path_S(j).M2_Point = "" Then
                                            path_output += "@" + path_S(j).From_Point.ToString + "," + path_S(j).direction0.ToString + "," + path_S(j).action0 + (CInt(path_S(j).Sensor0) + sensor_offset).ToString + "," + path_S(j).speed0.ToString
                                            path_output += "@" + path_S(j).M_Point.ToString + "," + path_S(j).direction1.ToString + "," + path_S(j).action1 + (CInt(path_S(j).Sensor1) + sensor_offset).ToString + "," + path_S(j).speed1.ToString
                                            settext(Car(car_idx).device_no.ToString + ":特殊路徑換", True, Car(car_idx).device_no)
                                        Else

                                            '如果有M2就全部用M2取代
                                            settext(Car(car_idx).device_no.ToString + ":特殊路徑取代", True, Car(car_idx).device_no)
                                            path_output += path_S(j).M2_Point
                                        End If
                                        i += 1
                                        Exit For
                                    End If
                                Next
                            End If

                            If flag = False And i <= (path_ary.Length - 2) Then
                                If Car(car_idx).Car_type = "FORK" Or Car(car_idx).Car_type = "LOWCAR" Then
                                    'For j As Integer = 0 To path_fork_base.Length - 1
                                    '    If path_fork_base(j).From_Point = path_ary(i) And path_fork_base(j).To_Point = path_ary(i + 1) And Not path_fork_base(j).speed0 = 0 Then
                                    '        path_output += "@" + path_ary(i).ToString + ",0," + path_fork_base(j).action0 + (CInt(path_fork_base(j).Sensor0) + sensor_offset).ToString + "," + (path_fork_base(j).speed0 + path_fork_base(j).offsetdistance * 256).ToString
                                    '        last_direction = 0
                                    '    ElseIf path_fork_base(j).To_Point = path_ary(i) And path_fork_base(j).From_Point = path_ary(i + 1) And Not path_fork_base(j).speed1 = 0 Then
                                    '        path_output += "@" + path_ary(i).ToString + ",1," + path_fork_base(j).action1 + (CInt(path_fork_base(j).Sensor1) + sensor_offset).ToString + "," + (path_fork_base(j).speed1 + path_fork_base(j).offsetdistance * 256).ToString
                                    '        last_direction = 1
                                    '    End If
                                    'Next
                                    Dim key1 As Integer = path_ary(i) * 100000 + CInt(path_ary(i + 1))
                                    Dim key2 As Integer = path_ary(i + 1) * 100000 + CInt(path_ary(i))
                                    If Path_fork_Dictionary.ContainsKey(path_ary(i) * 100000 + path_ary(i + 1)) Then

                                        path_output += "@" + path_ary(i).ToString + ",0," + Path_fork_Dictionary(key1).action0 + (CInt(Path_fork_Dictionary(key1).Sensor0) + sensor_offset).ToString + "," + Path_fork_Dictionary(key1).speed0.ToString
                                        last_direction = 0

                                    ElseIf Path_fork_Dictionary.ContainsKey(path_ary(i + 1) * 100000 + path_ary(i)) Then

                                        path_output += "@" + path_ary(i).ToString + ",1," + Path_fork_Dictionary(key2).action1 + (CInt(Path_fork_Dictionary(key2).Sensor1) + sensor_offset).ToString + "," + Path_fork_Dictionary(key2).speed1.ToString
                                        last_direction = 1


                                    Else
                                        settext("錯誤，無法置信的錯誤fork")
                                    End If
                                Else
                                    'For j As Integer = 0 To path_base.Length - 1
                                    '    If path_base(j).From_Point = path_ary(i) And path_base(j).To_Point = path_ary(i + 1) Then
                                    '        path_output += "@" + path_ary(i).ToString + ",0," + path_base(j).action0 + (CInt(path_base(j).Sensor0) + sensor_offset).ToString + "," + path_base(j).speed0.ToString
                                    '        last_direction = 0
                                    '    ElseIf path_base(j).To_Point = path_ary(i) And path_base(j).From_Point = path_ary(i + 1) Then
                                    '        path_output += "@" + path_ary(i).ToString + ",1," + path_base(j).action1 + (CInt(path_base(j).Sensor1) + sensor_offset).ToString + "," + path_base(j).speed1.ToString
                                    '        last_direction = 1
                                    '    End If
                                    'Next
                                    Dim key1 As Integer = path_ary(i) * 100000 + CInt(path_ary(i + 1))
                                    Dim key2 As Integer = path_ary(i + 1) * 100000 + CInt(path_ary(i))
                                    If Path_base_Dictionary.ContainsKey(path_ary(i) * 100000 + path_ary(i + 1)) Then

                                        path_output += "@" + path_ary(i).ToString + ",0," + Path_base_Dictionary(key1).action0 + (CInt(Path_base_Dictionary(key1).Sensor0) + sensor_offset).ToString + "," + Path_base_Dictionary(key1).speed0.ToString
                                        last_direction = 0

                                    ElseIf Path_base_Dictionary.ContainsKey(path_ary(i + 1) * 100000 + path_ary(i)) Then

                                        path_output += "@" + path_ary(i).ToString + ",1," + Path_base_Dictionary(key2).action1 + (CInt(Path_base_Dictionary(key2).Sensor1) + sensor_offset).ToString + "," + Path_base_Dictionary(key2).speed1.ToString
                                        last_direction = 1


                                    Else
                                        settext("錯誤，無法置信的錯誤base")
                                    End If
                                End If
                            End If
                        Next

                    End If
                    '新增停止TAG
                    If last_direction = 0 Then
                        path_output += "@" + path_ary(path_ary.Length - 1) + ",1,S0,5"
                    Else
                        path_output += "@" + path_ary(path_ary.Length - 1) + ",1,S0,5"
                        'path_output += "@" + path_ary(path_ary.Length - 1) + ",0,S0,5"
                    End If
                    Car(car_idx).To_temp_pos = CInt(path_ary(path_ary.Length - 1))

                    '字串轉成命令點位傳入AGV，並啟動AGV
                    If cmdtype = 11 Then
                        '動態變更()
                        If Not Car(car_idx).subcmd = old_subcmd Then
                            settext(Car(car_idx).device_no.ToString + ":規劃路徑" + path_output.Substring(1), True, Car(car_idx).device_no)
                            Car(car_idx).cmd2Car(path_output.Substring(1), cmdtype)
                        End If
                    Else
                        If path_ary.Length > 1 Then

                            If Query_Floor(path_ary(1)) > 0 Then
                                Car(car_idx).To_AGV(3) = Query_Floor(path_ary(1))
                            End If

                        End If
                        settext(Car(car_idx).device_no.ToString + ":規劃路徑" + path_output.Substring(1), True, Car(car_idx).device_no)

                        Car(car_idx).cmd2Car(path_output.Substring(1), cmdtype)
                    End If

                End If

            End If
            oConn.Close()
            oConn.Dispose()

        End If



    End Function

    Function Get_group_path(ByVal subcmd As String) As String
        Get_group_path = subcmd
        For i As Integer = 0 To group_path.Length - 1
            If In_Subcmd(subcmd, group_path(i)) = True Then
                Get_group_path += "," + group_path(i)
            End If
        Next


    End Function
    Function CheckCarInDoor(ByVal CarTagID As Integer)
        CheckCarInDoor = False
        For i As Integer = 0 To Door_List.Length - 1
            If Door_List(i).flag Then
                'door 25  car tagid 位於 23 27之間
                If CarTagID >= Door_List(i).tagid - 2 And CarTagID <= Door_List(i).tagid + 2 Then

                    CheckCarInDoor = True
                    Exit For
                End If
            End If
        Next
    End Function
    Function CheckCarInLft(ByVal CarTagID As Integer, ByVal maincmd As String)
        CheckCarInLft = False
        For i As Integer = 0 To LFT_List.Length - 1
            If LFT_List(i).flag Then
                'LFT 1000  car tagid 位於 1000 1009之間
                If CarTagID >= LFT_List(i).tagid And CarTagID <= LFT_List(i).tagid + 9 Then
                    '判斷是否已經完成電梯
                    If maincmd.IndexOf(CarTagID.ToString + "," + LFT_List(i).tagid.ToString) > 0 Then
                        '未完成電梯，電梯程序
                        CheckCarInLft = True
                    Else
                        '完成電梯，那就不是電梯程序
                        CheckCarInLft = False
                    End If

                    Exit For
                End If
            End If
        Next
    End Function
    Function Query_shelf_sensoroffset(ByVal shelf_car_no As Integer)
        Query_shelf_sensoroffset = 0
        Try


            For i As Integer = 0 To shelf_car.Length - 1
                If shelf_car(i).Shelf_Car_No = shelf_car_no Then
                    Query_shelf_sensoroffset = shelf_car(i).offset_sensor
                    Exit Function
                End If

            Next
        Catch ex As Exception

        End Try
    End Function
    Function In_Array(ByVal str_ary() As String, ByVal val As String, Optional ByVal check_count As Integer = 99)
        Dim i As Integer = 0
        For Each temp As String In str_ary
            i += 1
            If val = temp Then
                Return True
            End If

            If i > check_count Then
                Return False
            End If

        Next
        Return False
    End Function

    Function Door_In_Array(ByVal str_ary() As String, ByVal val As String, ByVal offset As Integer, Optional ByVal check_count As Integer = 99)
        Dim i As Integer = 0
        For j As Integer = CInt(val) - offset To CInt(val) + offset
            i = 0
            For Each temp As String In str_ary

                i += 1
                If j.ToString = temp Then
                    Return True
                End If
                If i > check_count Then
                    Exit For
                End If
            Next
        Next
        Return False
    End Function


    Private Sub Button13_Click_2(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Send2AGV(view_car_idx, 11)
    End Sub
    Function Send_CMD(ByVal car_no As Integer, ByVal From_Point As Integer, ByVal To_Point As Integer, Optional ByVal ext_cmd As String = "")
        Send_CMD = 0
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand

        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        Dim Query As String = ""
        Dim mysql_data As Object
        Try



            If From_Point < 10 Then

                Query = "INSERT INTO `agv_cmd_list` ( AGVNo,`CmdFrom`, `CmdTo`, `Pri_Wt`,RequestTime, `Requestor`) VALUES ('" + car_no.ToString + "','" + From_Point.ToString + "', '" + To_Point.ToString + "', '50',now(), 'AGVC');"

                Query = " insert into agv_cmd_list(AGVNo,CmdFrom,	CmdTo,Pri_Wt,RequestTime,Requestor,RequestName,ext_cmd)					"
                Query += " SELECT A.`AGVNo` , " + From_Point.ToString + " AS from_point, " + To_Point.ToString + ",'50',now(), 'AGVC_S','AGVC_S' ,'" + ext_cmd + "'"
                Query += " FROM `agv_list` A LEFT JOIN agv_cmd_list B ON A.`AGVNo` = B.`AGVNo`"
                Query += " WHERE A.`AGVNo` =" + car_no.ToString
                Query += " and not " + To_Point.ToString + " in (select CmdTo FROM `agv_cmd_list` ) "
                Query += " and " + To_Point.ToString + " in (select Tag_ID FROM `point` ) "
                Query += " and not " + To_Point.ToString + " in (select Position FROM `agv_list` ) limit 0,1"
                sqlCommand.CommandText = Query
                'Cmd_status.AppendText("Query=" + Query)
                Send_CMD = sqlCommand.ExecuteNonQuery()


                ' Cmd_status.AppendText(Query + ":" + Send_CMD.ToString + vbCrLf)
            Else
                Dim to_cnt, from_cnt As Integer
                Query = "select count(*) FROM `shelf_car` where LOCATION=" + From_Point.ToString
                sqlCommand.CommandText = Query
                mysql_data = sqlCommand.ExecuteScalar()
                from_cnt = CInt(mysql_data)
                'Cmd_status.AppendText(Query + ":" + from_cnt.ToString + vbCrLf)
                Query = "select count(*) FROM `shelf_car` where LOCATION=" + To_Point.ToString
                sqlCommand.CommandText = Query
                mysql_data = sqlCommand.ExecuteScalar()
                to_cnt = CInt(mysql_data)
                'Cmd_status.AppendText(Query + ":" + to_cnt.ToString + vbCrLf)
                If from_cnt = 1 And to_cnt = 0 Then
                    'TO 點位沒有架台
                    Query = "insert into  `agv_cmd_list`(`AGVNo`,`CmdFrom`,`CmdTo`,`Pri_Wt`,`Requestor`,`Shelf_Car_No`,`Shelf_Car_type`,`Shelf_Car_Size`) select '" + car_no.ToString + "' as AGVNo,'" + From_Point.ToString + "','" + To_Point.ToString + "',50,'AGVC',Shelf_Car_No,`Shelf_Car_type`,`Shelf_Car_Size` from `shelf_car` where LOCATION='" + From_Point.ToString + "' "
                    sqlCommand.CommandText = Query
                    Send_CMD = sqlCommand.ExecuteNonQuery()
                    '  Cmd_status.AppendText(Query + ":" + Send_CMD.ToString + vbCrLf)
                ElseIf from_cnt = 0 Then
                    Send_CMD = 0
                    '   Cmd_status.AppendText("來源端無架台" + vbCrLf)
                ElseIf to_cnt = 1 Then
                    Send_CMD = 0
                    '  Cmd_status.AppendText("目的端架台" + vbCrLf)
                End If
            End If

        Catch ex As Exception
            Send_CMD = 0
            'Cmd_status.AppendText("Query=" + Query + ":" + ex.Message)
        End Try

        oConn.Close()
        oConn.Dispose()
    End Function

    Private Sub Button16_Click_2(ByVal sender As System.Object, ByVal e As System.EventArgs)
        MsgBox(Send_CMD(CInt(txtCar.Text), CInt(From_cb.Text), CInt(To_cb.Text)))
    End Sub


    Private Sub Button17_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand

        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        Dim Query As String = ""
        Dim temp_point As String = "814,808,803"
        Query = "SELECT group_concat( `Tag_ID` ) FROM `point` WHERE `Retreat_Flag` =1 and not `Tag_ID` in (select CmdTo FROM `agv_cmd_list` )  and not `Tag_ID`  in (select Position FROM `agv_list` )"
        sqlCommand.CommandText = Query
        temp_point = sqlCommand.ExecuteScalar.ToString



        'MsgBox(Dijkstra_fn_ary(Dijkstra, Tag_ID_List, CInt(From_cb.Text), temp_point, ""))

        oConn.Close()
        oConn.Dispose()
    End Sub



    Private Sub Button13_Click_3(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button13.Click
        Car(view_car_idx).To_AGV(3) = Query_Floor(Car(view_car_idx).get_tagId)
    End Sub



    Private Sub Button16_Click_3(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Car(view_car_idx).flag Then
            Car(view_car_idx).To_AGV(6) = 0
        Else
            MsgBox(view_car_idx.ToString + "號車未啟用")

        End If
    End Sub

    Private Sub PictureBox3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox3.Click
        Floor_Map.Text = Car(view_car_idx).Car_Picbox.Top + offset_y + 15 - 250
    End Sub

    Private Sub Car12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car12.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 12
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car13_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car13.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 13
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car15.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 15
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car14_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car14.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 14
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub


    Private Sub Car16_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car16.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 16
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car17_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car17.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 17
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car18.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 18
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car19_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car19.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 19
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car20_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car20.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 20
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car21_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car21.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 21
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car22_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car22.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 22
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car23_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car23.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 23
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car24_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car24.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 24
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car25_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car25.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 25
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car26_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car26.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 26
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car27_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car27.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 27
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car28_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car28.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 28
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car29_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car29.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 29
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car30_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Car30.Click
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 30
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub



    Private Sub Button19_Click_3(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button19.Click
        Dim watch As Stopwatch = Stopwatch.StartNew()
        Dim a As String = ""
        If CInt(From_cb.Text) = 0 Then
            a = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(1).ary, Tag_ID_Fork_List, Car(view_car_idx).get_tagId, To_cb.Text, TextBox3.Text)
        ElseIf Not car_type.Text = "" Then
            For i As Integer = 0 To Dijkstra_list.Length - 1
                If Dijkstra_list(i).name = car_type.Text Then
                    a = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(i).ary, Tag_ID_Fork_List, CInt(From_cb.Text), To_cb.Text, TextBox3.Text)
                End If
            Next
        Else

            a = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(1).ary, Tag_ID_Fork_List, CInt(From_cb.Text), To_cb.Text, TextBox3.Text)
        End If
        Dim path_ary() As String
        path_ary = a.Split(",")
        Dim sensor_offset As Integer = 0
        Dim path_output As String = ""
        Dim last_direction As Integer = 0
        'Dim path_output_list(50) As Integer
        'Dim path_output_list_idx As Integer = 0
        Dim path_ary_Len As Integer = path_ary.Length
        If Not path_ary_Len = 1 Then
            '超過兩個點位
            For i As Integer = 0 To path_ary_Len - 2
                Dim flag As Boolean = False
                '特殊路徑
                If i <= (path_ary_Len - 3) Then
                    For j As Integer = 0 To path_S.Length - 1
                        If path_ary(i) = path_S(j).From_Point And path_ary(i + 1) = path_S(j).M_Point And path_ary(i + 2) = path_S(j).To_Point Then
                            flag = True

                            If path_S(j).M2_Point = "" Then
                                path_output += "@" + path_S(j).From_Point.ToString + "," + path_S(j).direction0.ToString + "," + path_S(j).action0 + (CInt(path_S(j).Sensor0) + sensor_offset).ToString + "," + path_S(j).speed0.ToString
                                path_output += "@" + path_S(j).M_Point.ToString + "," + path_S(j).direction1.ToString + "," + path_S(j).action1 + (CInt(path_S(j).Sensor1) + sensor_offset).ToString + "," + path_S(j).speed1.ToString
                            Else
                                '如果有M2就全部用M2取代
                                path_output += path_S(j).M2_Point
                            End If
                            i += 1
                            Exit For
                        End If
                    Next

                End If

                If i <= (path_ary_Len - 2) And flag = False Then

                    Dim key1 As Integer = path_ary(i) * 100000 + CInt(path_ary(i + 1))
                    Dim key2 As Integer = path_ary(i + 1) * 100000 + CInt(path_ary(i))
                    If Path_fork_Dictionary.ContainsKey(key1) Then

                        path_output += "@" + path_ary(i).ToString + ",0," + Path_fork_Dictionary(key1).action0 + (CInt(Path_fork_Dictionary(key1).Sensor0) + sensor_offset).ToString + "," + Path_fork_Dictionary(key1).speed0.ToString
                        last_direction = 0

                    ElseIf Path_fork_Dictionary.ContainsKey(key2) Then

                        path_output += "@" + path_ary(i).ToString + ",1," + Path_fork_Dictionary(key2).action1 + (CInt(Path_fork_Dictionary(key2).Sensor1) + sensor_offset).ToString + "," + Path_fork_Dictionary(key2).speed1.ToString
                        last_direction = 1


                    Else
                        settext("錯誤，無法置信的錯誤fork")
                    End If
                End If


            Next
            '新增停止TAG
            If last_direction = 0 Then
                path_output += "@" + path_ary(path_ary.Length - 1) + ",1,S0,5"
            Else
                path_output += "@" + path_ary(path_ary.Length - 1) + ",0,S0,5"
            End If

            '字串轉成命令點位傳入AGV，並啟動AGV

        End If

        watch.Stop()
        Dim elapsedMs As Long = watch.ElapsedMilliseconds

        MsgBox(elapsedMs.ToString() & " ms")
        MsgBox(a)
        settext(path_output)
        MsgBox(path_output)
    End Sub

    Private Sub CheckBox1_CheckedChanged_3(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles settext_filter.CheckedChanged

    End Sub


    Private Sub Button17_Click_2(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button17.Click
        Dim writer As New XmlTextWriter("setting.ini", System.Text.Encoding.UTF8)
        Try


            writer.WriteStartDocument(True)
            writer.Formatting = Formatting.Indented
            writer.Indentation = 2
            writer.WriteStartElement("setting")

            writer.WriteStartElement("ShelfCar_Check")
            writer.WriteString(Me.Agvc_shelfcheck.Checked.ToString)
            writer.WriteEndElement()

            writer.WriteStartElement("Loading_Check")
            writer.WriteString(Loading_Check.Checked.ToString)
            writer.WriteEndElement()

            writer.WriteStartElement("IR_sensor")
            writer.WriteString(IR_check.Checked.ToString)
            writer.WriteEndElement()

            writer.WriteStartElement("ALL_Loading_check")
            writer.WriteString(ALL_Loading_check.Checked.ToString)
            writer.WriteEndElement()

            writer.WriteStartElement("Loading_Check")
            writer.WriteString(Loading_Check.Checked.ToString)
            writer.WriteEndElement()

            writer.WriteStartElement("chrage_exception")
            writer.WriteString(chrage_exception.Text)
            writer.WriteEndElement()

            writer.WriteStartElement("MyDB")
            writer.WriteString(MyDB_txt.Text)
            writer.WriteEndElement()

            writer.WriteStartElement("IP")
            writer.WriteString(IP.Text)
            writer.WriteEndElement()
            writer.WriteStartElement("LPort")
            writer.WriteString(LPort.Text)
            writer.WriteEndElement()


            writer.WriteStartElement("user")
            writer.WriteString(user_txt.Text)
            writer.WriteEndElement()

            writer.WriteStartElement("password")
            writer.WriteString(password_txt.Text)
            writer.WriteEndElement()

            writer.WriteStartElement("PathLen")
            PathLen = CInt(PathLenTxt.Text.ToString)
            writer.WriteString(PathLenTxt.Text.ToString)
            writer.WriteEndElement()

            writer.WriteStartElement("RemapLen")
            ReMapLen = CInt(RemapLenTxt.Text.ToString)
            writer.WriteString(RemapLenTxt.Text.ToString)
            writer.WriteEndElement()


            writer.WriteStartElement("DebugMode")
            writer.WriteString(DebugMode.Checked.ToString)
            writer.WriteEndElement()

            writer.WriteStartElement("DoorSetLen")
            writer.WriteString(DoorSetLenTxt.Text)
            DoorSetLen = CInt(DoorSetLenTxt.Text)
            writer.WriteEndElement()

            writer.WriteStartElement("AgvTimeout")
            writer.WriteString(AgvTimeout.Text)
            AgvTimeoutVal = CInt(AgvTimeout.Text)
            writer.WriteEndElement()

            writer.WriteStartElement("RetreatPath")
            writer.WriteString(RetreatPathTxt.Text)
            RetreatPath = CInt(RetreatPathTxt.Text)
            writer.WriteEndElement()

            writer.WriteEndElement()
            writer.WriteEndDocument()
        Catch ex As Exception
            MsgBox("存檔失敗")
        End Try
        writer.Close()

        For i As Integer = 0 To LFT_List.Length - 1
            LFT_List(i).IR_sensor = Me.IR_check.Checked
        Next
    End Sub

    Private Sub ContextMenuStrip1_Opening(ByVal sender As System.Object, ByVal e As System.ComponentModel.CancelEventArgs) Handles ContextMenuStrip1.Opening

    End Sub

    Private Sub Button18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button18.Click
        Dim watch As Stopwatch = Stopwatch.StartNew()


        Dim a As String = ""
        If CInt(From_cb.Text) = 0 Then
            a = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, Car(view_car_idx).get_tagId, To_cb.Text, TextBox3.Text)
        ElseIf Not car_type.Text = "" Then
            For i As Integer = 0 To Dijkstra_list.Length - 1
                If Dijkstra_list(i).name = car_type.Text Then
                    a = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(i).ary, Tag_ID_List, CInt(From_cb.Text), To_cb.Text, TextBox3.Text)
                End If
            Next
        Else
            a = Dijkstra_fn.Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, CInt(From_cb.Text), To_cb.Text, TextBox3.Text)
        End If

        Dim path_ary() As String
        path_ary = a.Split(",")
        Dim sensor_offset As Integer = 0
        Dim path_output As String = ""
        Dim last_direction As Integer = 0
        Dim path_ary_Len As Integer = path_ary.Length
        'Dim path_output_list(50) As Integer
        'Dim path_output_list_idx As Integer = 0

        If Not path_ary_Len = 1 Then
            '超過兩個點位
            For i As Integer = 0 To path_ary_Len - 2
                Dim flag As Boolean = False
                '特殊路徑
                If i <= (path_ary_Len - 3) Then
                    For j As Integer = 0 To path_S.Length - 1
                        If path_ary(i) = path_S(j).From_Point And path_ary(i + 1) = path_S(j).M_Point And path_ary(i + 2) = path_S(j).To_Point Then
                            flag = True

                            If path_S(j).M2_Point = "" Then
                                path_output += "@" + path_S(j).From_Point.ToString + "," + path_S(j).direction0.ToString + "," + path_S(j).action0 + (CInt(path_S(j).Sensor0) + sensor_offset).ToString + "," + path_S(j).speed0.ToString
                                path_output += "@" + path_S(j).M_Point.ToString + "," + path_S(j).direction1.ToString + "," + path_S(j).action1 + (CInt(path_S(j).Sensor1) + sensor_offset).ToString + "," + path_S(j).speed1.ToString
                            Else
                                '如果有M2就全部用M2取代
                                path_output += path_S(j).M2_Point
                            End If
                            i += 1
                            Exit For

                        End If
                    Next

                End If
                '正常
                If i <= (path_ary_Len - 2) And flag = False Then
                    'For j As Integer = 0 To path_base.Length - 1
                    '    If path_base(j).From_Point = path_ary(i) And path_base(j).To_Point = path_ary(i + 1) And Not path_base(j).speed0 = 0 Then
                    '        path_output += "@" + path_ary(i).ToString + ",0," + path_base(j).action0 + (CInt(path_base(j).Sensor0) + sensor_offset).ToString + "," + path_base(j).speed0.ToString
                    '        last_direction = 0
                    '    ElseIf path_base(j).To_Point = path_ary(i) And path_base(j).From_Point = path_ary(i + 1) And Not path_base(j).speed1 = 0 Then
                    '        path_output += "@" + path_ary(i).ToString + ",1," + path_base(j).action1 + (CInt(path_base(j).Sensor1) + sensor_offset).ToString + "," + path_base(j).speed1.ToString
                    '        last_direction = 1
                    '    End If
                    'Next
                    ' -------------
                    ' 找出
                    Dim key1 As Integer = path_ary(i) * 100000 + CInt(path_ary(i + 1))
                    Dim key2 As Integer = path_ary(i + 1) * 100000 + CInt(path_ary(i))
                    If Path_base_Dictionary.ContainsKey(path_ary(i) * 100000 + path_ary(i + 1)) Then

                        path_output += "@" + path_ary(i).ToString + ",0," + Path_base_Dictionary(key1).action0 + (CInt(Path_base_Dictionary(key1).Sensor0) + sensor_offset).ToString + "," + Path_base_Dictionary(key1).speed0.ToString
                        last_direction = 0

                    ElseIf Path_base_Dictionary.ContainsKey(path_ary(i + 1) * 100000 + path_ary(i)) Then

                        path_output += "@" + path_ary(i).ToString + ",1," + Path_base_Dictionary(key2).action1 + (CInt(Path_base_Dictionary(key2).Sensor1) + sensor_offset).ToString + "," + Path_base_Dictionary(key2).speed1.ToString
                        last_direction = 1


                    Else
                        settext("錯誤，無法置信的錯誤")
                    End If
                    ' --------------
                End If

            Next
            '新增停止TAG
            If last_direction = 0 Then
                path_output += "@" + path_ary(path_ary.Length - 1) + ",1,S0,5"
            Else
                path_output += "@" + path_ary(path_ary.Length - 1) + ",0,S0,5"
            End If

            '字串轉成命令點位傳入AGV，並啟動AGV

        End If


        watch.Stop()
        Dim elapsedMs As Long = watch.ElapsedMilliseconds

        MsgBox(elapsedMs.ToString() & " ms")
        MsgBox(a)
        settext(path_output)
        MsgBox(path_output)
    End Sub

  
   
    Private Sub Button18_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button18.Click

    End Sub

    Private Sub Button22_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button22.Click
        LFT_List(CInt(LFT_idx.Text)).reconnect()

    End Sub

    Private Sub Err_lb_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Err_lb.Click
        'MsgBox("explorer HTTP://" + IP.Text + "/alarm/index.php")
        Shell("explorer ""HTTP://" + IP.Text + "/alarm/index.php?alm=" + Err_lb.Text + """ ")


    End Sub
    Dim ChargerClient(10) As EQP_Modus
    Private Sub EQ_BG_DoWork(ByVal sender As System.Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles EQ_BG.DoWork
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim Query As String = ""
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        For i As Integer = 0 To ChargerClient.Length - 1
            Try

        
            If My.Computer.Network.Ping(ChargerClient(i).ipadress) Then
                'Dim readflag As Boolean = False
                'setCommtext("Ping->" + ChargerClient(i).EQ_ID + ":" + ChargerClient(i).ipadress)
                ChargerClient(i).ReadOK = ChargerClient(i).Read_HoldingReg(ChargerClient(i).HoldingReg, 60, ChargerClient(i).HoldingResponse)
                Dim Status As String = "Auto"
                Dim AutoStatus As Integer = 1
                If ChargerClient(i).HoldingResponse(19) > 0 Then
                    If Not ChargerClient(i).HoldingResponse(19) = ChargerClient(i).Pre_State Then
                        Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`) "
                        Query += "VALUES ('',now(),now(), '" + (3000 + ChargerClient(i).HoldingResponse(19)).ToString + "', '" + ChargerClient(i).tag_id.ToString + "', '', '', '" + ChargerClient(i).tag_id.ToString + "', '', '') ;"
                        sqlCommand.CommandText = Query
                        sqlCommand.ExecuteNonQuery()
                        ChargerClient(i).Pre_State = ChargerClient(i).HoldingResponse(19)
                    End If
                    Status = "DOWN"
                    AutoStatus = -1
                ElseIf ChargerClient(i).HoldingResponse(18) = 0 Then
                    Status = "Manual"
                    AutoStatus = 0
                ElseIf ((ChargerClient(i).HoldingResponse(23) << 16) + ChargerClient(i).HoldingResponse(22)) > 0 Then
                    Status = "RUN"
                    AutoStatus = 2
                    '充電中
                ElseIf ChargerClient(i).HoldingResponse(18) = 1 Then
                    Status = "IDLE"
                    AutoStatus = 1
                End If
                Dim Amp As Integer = 0
                Dim Volt As Integer = 0
                For j As Integer = 0 To Car.Length - 1
                    If Car(j).get_tagId = ChargerClient(i).tag_id And Car(j).device_status(6) = 48 Then
                        Amp = Car(j).get_AMP
                        Volt = Car(j).get_BMSVolt
                    End If
                Next
                Try
                    Dim step_time As String = ChargerClient(i).HoldingResponse(34).ToString + ChargerClient(i).HoldingResponse(35).ToString + ChargerClient(i).HoldingResponse(36).ToString
                    Dim totaltime As String = ChargerClient(i).HoldingResponse(37).ToString + ChargerClient(i).HoldingResponse(38).ToString + ChargerClient(i).HoldingResponse(39).ToString
                    Query = "INSERT ignore INTO `agv`.`charger_history` (`tagid`, `X`, `Y`, `T1`, `ForkLocation`,LineT, `AutoStatus`, `Err`, `OUT_V`, `OUT_A`, `OUT_Watt`,OUT_T, " + _
                            "`OUT_mAh`, `OUT_Wh`, `steptime`, `totaltime`, `STEPIndex`, `IN_V`, `IN_A`, `IN_Watt`, `IN_kWh`, `IN_totaltime`,OUT_T2) " + _
                            "VALUES ('" + ChargerClient(i).tag_id.ToString + "', '" + ChargerClient(i).HoldingResponse(1).ToString + "', '" + ChargerClient(i).HoldingResponse(2).ToString + "', '" + ChargerClient(i).HoldingResponse(3).ToString + "'," + _
                            " '" + ChargerClient(i).HoldingResponse(7).ToString + "', '" + ChargerClient(i).HoldingResponse(8).ToString + "', '" + AutoStatus.ToString + "', '" + ChargerClient(i).HoldingResponse(19).ToString + "', '" + ((ChargerClient(i).HoldingResponse(21) << 16) + ChargerClient(i).HoldingResponse(20)).ToString + "'," + _
                            " '" + ((ChargerClient(i).HoldingResponse(23) << 16) + ChargerClient(i).HoldingResponse(22)).ToString + "', '" + ((ChargerClient(i).HoldingResponse(25) << 16) + ChargerClient(i).HoldingResponse(24)).ToString + "', '" + ((ChargerClient(i).HoldingResponse(27) << 16) + ChargerClient(i).HoldingResponse(26)).ToString + "', '" + ((ChargerClient(i).HoldingResponse(29) << 16) + ChargerClient(i).HoldingResponse(28)).ToString + "', " + _
                            " '" + ((ChargerClient(i).HoldingResponse(31) << 16) + ChargerClient(i).HoldingResponse(30)).ToString + "', '" + step_time + "', " + _
                            " '" + totaltime + "', '" + ChargerClient(i).HoldingResponse(40).ToString + "', '" + ChargerClient(i).HoldingResponse(50).ToString + "' " + _
                            ", '" + ChargerClient(i).HoldingResponse(52).ToString + "', '" + ChargerClient(i).HoldingResponse(24).ToString + "', '" + ChargerClient(i).HoldingResponse(56).ToString + "', '" + ChargerClient(i).HoldingResponse(58).ToString + "', '" + ChargerClient(i).HoldingResponse(9).ToString + "');"
                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()
                    Query = "update  `charger`  set Status ='" + Status + "',err=" + ChargerClient(i).HoldingResponse(19).ToString + ",Amp=" + Amp.ToString + ",Volt=" + Volt.ToString + " where PORT_ID='" + ChargerClient(i).EQ_ID + "'"
                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()
                    Query = "update  `charger_status`  set X ='" + ChargerClient(i).HoldingResponse(1).ToString + "',Y=" + ChargerClient(i).HoldingResponse(2).ToString + ",T1=" + ChargerClient(i).HoldingResponse(3).ToString + ",ForkLocation=" + ChargerClient(i).HoldingResponse(7).ToString + ",LineT=" + ChargerClient(i).HoldingResponse(8).ToString + _
                        ",AutoStatus ='" + AutoStatus.ToString + "',Err=" + ChargerClient(i).HoldingResponse(19).ToString + ",OUT_V=" + ((ChargerClient(i).HoldingResponse(21) << 16) + ChargerClient(i).HoldingResponse(20)).ToString + ",OUT_A=" + ((ChargerClient(i).HoldingResponse(23) << 16) + ChargerClient(i).HoldingResponse(22)).ToString + _
                        ",OUT_Watt ='" + ((ChargerClient(i).HoldingResponse(25) << 16) + ChargerClient(i).HoldingResponse(24)).ToString + "',OUT_T=" + ((ChargerClient(i).HoldingResponse(27) << 16) + ChargerClient(i).HoldingResponse(26)).ToString + ",OUT_mAh=" + ((ChargerClient(i).HoldingResponse(29) << 16) + ChargerClient(i).HoldingResponse(28)).ToString + ",OUT_Wh=" + ((ChargerClient(i).HoldingResponse(31) << 16) + ChargerClient(i).HoldingResponse(30)).ToString + _
                        ",steptime ='" + step_time + "',totaltime=" + totaltime + ",STEPIndex=" + ChargerClient(i).HoldingResponse(40).ToString + ",IN_V=" + ChargerClient(i).HoldingResponse(50).ToString + _
                         ",IN_A ='" + ChargerClient(i).HoldingResponse(52).ToString + "',IN_Watt=" + ChargerClient(i).HoldingResponse(54).ToString + ",IN_kWh=" + ChargerClient(i).HoldingResponse(56).ToString + ",IN_totaltime=" + ChargerClient(i).HoldingResponse(58).ToString + _
                          ",cur_time =now(),OUT_T2 ='" + ChargerClient(i).HoldingResponse(9).ToString + "'" + _
                        " where tagid=" + ChargerClient(i).tag_id.ToString
                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()
                Catch ex As Exception
                        settext("EQ_BG " + Query)
                End Try
            Else
                ChargerClient(i).ReadOK = False
                End If
            Catch ex As Exception
                settext("EQ2_BG" + Query)
            End Try

        Next


        oConn.Close()
        oConn.Dispose()
        For i As Integer = 0 To Car.Length - 1
            Dim flag As Boolean = False
            For j As Integer = 0 To 52
                If Not Car(i).BMS1(j) = Car(i).Pre_BMS1(j) Then
                    Car(i).Pre_BMS1(j) = Car(i).BMS1(j)
                    flag = True
                End If
                If Not Car(i).BMS2(j) = Car(i).Pre_BMS2(j) Then
                    Car(i).Pre_BMS2(j) = Car(i).BMS2(j)
                    flag = True
                End If
            Next

            If Car(i).BMS1(7) > 1500 And Car(i).BMS1(7) < 7000 And flag = True Then

                Dim file_str As String = ".\log\" + Now.ToString("yyyyMMdd") + "_BMS" + Car(i).device_no.ToString + ".log"
                Dim filestream As StreamWriter = New StreamWriter(file_str, True)
                Dim bms As String = int2str(Car(i).BMS1, 0, 53)
                filestream.WriteLine(Now.ToString("yyyy-MM-dd HH:mm:ss") + ":" + bms)
                If Car(i).BMS2(7) > 1500 And Car(i).BMS2(7) < 7000 Then
                    bms = int2str(Car(i).BMS2, 0, 53)
                    filestream.WriteLine(Now.ToString("yyyy-MM-dd HH:mm:ss") + ":" + bms)
                End If

                filestream.Flush()
                filestream.Close()
            End If
        Next
    End Sub

    Private Sub EQ_Timer_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles EQ_Timer.Tick
        If Me.EQ_BG.IsBusy = False Then

            EQ_BG.RunWorkerAsync()

        End If
    End Sub
    Function InttoBitidx(ByVal IntVal As Integer)
        InttoBitidx = -1
        For code As Integer = 0 To 15
            If (IntVal >> code) = 1 Then
                Return code
            End If
        Next

    End Function
    Function int2bytestr(ByVal val() As Integer, ByVal startidx As Integer, ByVal len As Integer) As String
        int2bytestr = ""
        Dim strbyte(len * 2 - 1) As Byte
        If startidx + len > val.Length Then
            Return ""
        End If
        For i As Integer = 0 To len - 1
            strbyte(2 * i) = val(startidx + i) \ 256
            strbyte(2 * i + 1) = val(startidx + i) Mod 256
        Next
        int2bytestr = System.Text.Encoding.UTF8.GetString(strbyte, 0, len)
        int2bytestr = int2bytestr.TrimEnd("")
    End Function

 
    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim i As Integer = 0
        Dim tagid_len As Integer = 0
        oConn = New MySqlConnection(Mysql_str)
        Dim Query As String = "SELECT Tag_ID,X,Y,Retreat_Flag,Tag_name,floor,floor_no,th,slam FROM `point` where Tag_ID between 0 and 19999  order by Tag_ID ASC"
        Dim mReader As MySqlDataReader
        oConn.Open()
        sqlCommand.Connection = oConn
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        ReDim Tag_ID_Fork_List(10000)
        ReDim Tag_point_list(10000)
        ReDim Tag_ID_List(10000)
        Tag_point_Dictionary.Clear()


        While (mReader.Read)
            Tag_ID_List(i) = CInt(mReader.Item(0))
            Tag_point_list(i).TagId = CInt(mReader.Item(0))
            Tag_point_list(i).X = CInt(mReader.Item(1))
            Tag_point_list(i).Y = CInt(mReader.Item(2))
            Tag_point_list(i).th = CInt(mReader.Item(7))
            Tag_point_list(i).Retreat_Flag = CInt(mReader.Item(3))
            Tag_point_list(i).name = mReader.Item(4).ToString
            Tag_point_list(i).floor = mReader.Item(5).ToString
            Tag_point_list(i).floor_no = CInt(mReader.Item(6))
            Tag_point_list(i).slam = CInt(mReader.Item(8))
            Tag_point_Dictionary.Add(Tag_point_list(i).TagId, Tag_point_list(i))
            i += 1
            tagid_len = i
        End While
        mReader.Close()
        Array.Resize(Tag_point_list, tagid_len)

        '載入不影響設定車體
        Query = "SELECT AGVNo,Position,Loading,owner,Car_type,Chrage_Point,block_point,wait_point,Chrage_volt,SafeSensor,Compass,AutoCharge,AutoChargeVal,car_site,Recharge_SOC,Recharge_Point,lock_user FROM `agv_list` where flag=1 order by AGVNo"
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        While (mReader.Read)
            Try
                For j As Integer = 0 To Car.Length - 1
                    If Car(j).device_no = CInt(mReader.Item(0)) Then
                        Car(j).Car_type = mReader.Item(4).ToString
                        Car(j).Chrage_Point = CInt(mReader.Item(5))
                        Car(j).wait_point = CInt(mReader.Item(7))
                        Car(j).Chrage_volt = CInt(mReader.Item(8)) '
                        Car(i).Block_Point = mReader.Item(6).ToString
                        Car(i).SafeSensor = CInt(mReader.Item(9))
                        Car(i).Compass = CInt(mReader.Item(10))
                        Car(i).AutoCharge = CInt(mReader.Item(11))
                        Car(i).AutoChargeVal = CInt(mReader.Item(12))
                        Car(i).Site = (mReader.Item(13)).ToString
                        Car(i).Recharge_SOC = CInt(mReader.Item(14))
                        Car(i).Recharge_Point = (mReader.Item(15)).ToString
                        Car(i).Lock_user = (mReader.Item(16)).ToString
                        Exit For
                    End If
                Next
            Catch ex As Exception
                MsgBox(Car(i).device_no.ToString + "設定錯誤")
            End Try
        End While

        mReader.Close()


        For j As Integer = 0 To tagid_len - 1
            Tag_ID_Fork_List(j) = Tag_ID_List(j)
            Tag_ID_Fork_List(j + tagid_len) = Tag_ID_List(j) + 10000
        Next
        Array.Resize(Tag_ID_Fork_List, tagid_len * 2)
        Array.Resize(Tag_ID_List, tagid_len)
        Path_base_Dictionary.Clear()
        Load_Path_base()
        Path_fork_Dictionary.Clear()
        Load_Path_fork_base()
        For k As Integer = 0 To Dijkstra_list.Length - 1
            If Dijkstra_list(k).CarType = "9" Then
                'FORK
                Load_Path_fork(Dijkstra_list(k).name, Tag_ID_Fork_List, Dijkstra_list(k).ary) '載入路徑資料
            Else
                Load_Path(Dijkstra_list(k).name, Tag_ID_List, Dijkstra_list(k).ary) '載入路徑資料
            End If
            car_type.Items.Add(Dijkstra_list(k).name)
        Next
        For i = 0 To Car.Length - 1

            Car(i).Tag_Point_dict = Tag_point_Dictionary ' New Dictionary(Of Integer, Tag_Point)(myDictionary)
        Next

        MsgBox("更新完畢")
    End Sub

    Private Sub Button5_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        If Car(view_car_idx).Car_type = "SLAM" Then
            Car(view_car_idx).To_AGV(4) = CInt(To_cb.Text)
            Car(view_car_idx).To_AGV(5) = Query_Floor(CInt(To_cb.Text))
        End If



    End Sub
  
    Private Sub Button10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button10.Click
        Car(view_car_idx).To_AGV(3) = CInt(To_cb.Text)
    End Sub
    Dim BackString As String = ""
    Private Sub BackUp_DoWork(ByVal sender As System.Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles BackUp.DoWork

        Dim idx As Integer = view_car_idx
        If Car(view_car_idx).Car_type = "SLAM" Then
            For i As Integer = 0 To Tag_point_list.Length - 1
                '逐個詢問
                BackString = Tag_point_list(i).floor_no.ToString + ":" + Tag_point_list(i).TagId.ToString + "(" + Round(i / Tag_point_list.Length, 1).ToString + "%)"
                'If Tag_point_list(i).floor_no = 5 And (Tag_point_list(i).TagId = 1397 Or Tag_point_list(i).TagId = 1398 Or Tag_point_list(i).TagId = 1399) Then
                Car(idx).To_AGV(4) = Tag_point_list(i).TagId
                Car(idx).To_AGV(5) = Tag_point_list(i).floor_no

                For j As Integer = 0 To 30
                    BackString = Tag_point_list(i).floor_no.ToString + ":" + Tag_point_list(i).TagId.ToString + "(" + Round(i / Tag_point_list.Length, 1).ToString + "%) wait" + j.ToString
                    If Car(idx).SlamTag > 0 Then
                        Car(idx).SlamTag = 0
                        Exit For
                    End If
                    Thread.Sleep(100)
                Next
                'End If

            Next
        End If
        BackString = "完成"
    End Sub

    Private Sub Button14_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button14.Click
        If Not BackUp.IsBusy Then
            BackUp.RunWorkerAsync()
            BackString = "Start"
        End If
    End Sub

 
End Class
