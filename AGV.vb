Imports System.Net.Sockets
Imports System.IO.Ports
Imports System.IO
Imports Bilgic.Net
Imports System.Threading
Imports System.Text
Imports MySql.Data.MySqlClient

Module AGV
    Public Structure lableTxt
        Dim Txt As String
        Dim X As Integer
        Dim Y As Integer
        Dim isize As Integer
    End Structure
    Public Structure ALM
        Dim len As Integer
        Dim ALM_ID() As Integer
        Dim ALM_RPT_ID() As Integer
        Dim ALM_TXT() As String
        Dim ALM_ENG_TXT() As String
        Sub init()
            ReDim ALM_ID(2000)
            ReDim ALM_RPT_ID(2000)
            ReDim ALM_TXT(2000)
            ReDim ALM_ENG_TXT(2000)
        End Sub

        Function Query_idx(ByVal ID As Integer) As Integer
            Query_idx = 0 '設定為未知異常
            For i As Integer = 0 To ALM_ID.Length - 1
                If ALM_ID(i) = ID Then
                    Return i
                End If
            Next
        End Function
        Function Query_ALM_TXT(ByVal ID As Integer) As String
            Query_ALM_TXT = "unknow" '設定為未知異常
            For i As Integer = 0 To ALM_ID.Length - 1
                If ALM_ID(i) = ID Then
                    Return ALM_TXT(i)
                End If
            Next
        End Function
    End Structure
    Public Structure SPath
        Dim From_Point As Integer
        Dim M_Point As Integer
        Dim To_Point As Integer
        'Dim direction As Integer
        Dim direction0 As Integer
        Dim action0 As String
        Dim Sensor0 As String
        Dim speed0 As Integer

        Dim direction1 As Integer
        Dim action1 As String
        Dim Sensor1 As String
        Dim speed1 As Integer

        Dim M2_Point As String

    End Structure

    Public Structure shelf_car_point
        Dim flag As String
        Dim Shelf_Car_No As Integer
        Dim Shelf_Car_type As String
        Dim Shelf_Car_Size As String
        Dim LOCATION As Integer
        Dim X As Integer
        Dim Y As Integer
        Dim car As Label
        Dim step_i As Integer
        Dim UNLOCK As Integer
        Dim offset_sensor As Integer
    End Structure
    Public Structure Tag_Point
        '劃地圖專用
        Dim TagId As Integer
        Dim X As Integer
        Dim Y As Integer
        Dim th As Integer
        Dim Retreat_Flag As Integer
        Dim name As String
        Dim floor As String
        Dim floor_no As Integer
        Dim site As String
        Dim tagtype As Integer
        Dim ZONE_NAME As String
        Dim LOC As String
        Dim stkval As String
        Dim CarrierID As String
        Sub setval(ByVal set_ID As Integer, ByVal set_x As Integer, ByVal set_y As Integer, ByVal set_D As String, ByVal set_Len As Integer)
            TagId = set_ID
            X = set_x
            Y = set_y
        End Sub

    End Structure
    Function Tag_Point_ByTagid(ByVal Tag_Point_list() As Tag_Point, ByVal tagid As Integer) As Integer

        For i As Integer = 0 To Tag_Point_list.Length - 1
            If Tag_Point_list(i).TagId = tagid Then
                Return i

            End If
        Next
        Return -1
    End Function
    Public Structure Path
        Dim From_Point As Integer
        Dim To_Point As Integer
        'Dim direction As Integer
        Dim action0 As String
        Dim action1 As String

        Dim Sensor0 As String
        Dim speed0 As Integer
        Dim Sensor1 As String
        Dim speed1 As Integer
        Dim distance As Integer
        Dim X1 As Integer
        Dim Y1 As Integer
        Dim X2 As Integer
        Dim Y2 As Integer

        Dim Fork_back As Integer
    End Structure

    Public Structure Car_point
        Dim flag As Boolean '是否
        Dim connected As Boolean
        Dim online As Boolean
        Dim step_i As Integer

        Public ipadress As String
        Public status As Integer  'online offline run down 
        Public econ_Socket As Socket
        Public econ_stream As NetworkStream
        Public timeout As Integer
        'modbus rtu 
        Public device_no As Integer
        Public device_status() As Integer
        Public Pre_device_status() As Integer
        Public To_AGV() As Integer
        Public Pre_TagID As Integer
        Public Pre_TagID_time As Date
        Public Pre_Auto As Integer
        ' Public tagIdSize As Integer
        Public tagId() As Integer
        Public action() As Integer
        Public from_pos As Integer
        Public To_pos As Integer
        Public To_temp_pos As Integer

        Dim cmd_type_flag As Boolean
        Dim cmd_idx As Integer

        Dim cmd_list() As String
        Dim cmd_Shelf_Car_No As Integer
        Dim cmd_Shelf_Car_Type As String
        Dim cmd_Shelf_Car_size As String
        Dim subcmd As String
        Dim main_subcmd As String
        Dim fast_subcmd As String
        'SQL list
        Dim cmd_sql_idx As Integer
        Dim Cmd_From As Integer
        Dim Cmd_To As Integer
        Dim RequestTime As String
        Dim Requestor As String
        Dim Shelf_Car_No As Integer


        '偵測斷線機制
        Dim Read_Err_Count As Integer
        Dim Shelf_Car As Boolean

        Dim Manual_TIme As Date
        Dim move_flag As Boolean
        Dim Wait_count As Integer
        Dim Run_time As Date
        Dim heart_bit_count As Integer
        'Dim Car_Picbox As PictureBox
        Dim Car_type As String
        'Dim Recharge_Point As Integer
        Dim Recharge_Point_list As String
        Dim wait_point As Integer
        Dim Recharge_volt As Integer
        Dim Blocking_car_device_no As Integer
        Dim Loading_err_cnt As Integer
        '退避機制
        Dim sflag As Integer
        Dim Block_Point As String
        Dim Block_Path As String
        Dim Cmd_RollData As String
        Dim RollData As String
        Public Lock_user As String
        Dim Counter_timer As Date


        Dim path_error_tagid As Integer
        Dim path_error_count As Integer

        Dim empty_time As Integer
        Dim Load_time As Integer
        Dim Error_time As Integer
        Dim Particle_idx As Integer
        Dim Pre_Error_Code As Integer
        Dim thread_idx As String
        Dim ext_cmd As String
        Dim subcmd_req As String
        Dim step_retry As Integer
        Dim rate_point As Integer
        Dim SafeSensor As Integer
        'MCS
        Dim CommandID As String
        Dim Site As String
        Dim State As String
        Dim Pre_State As String
        Dim AXIS_X As Integer
        Dim AXIS_Y As Integer
        Dim AXIS_Z As Integer
        Dim width As Integer
        Dim height As Integer
        Dim th As Integer
        Dim ReverseXY As Integer
        Dim offset_X As Integer
        Dim offset_Y As Integer
        Dim Pre_BMS1() As Integer
        Dim BMS1() As Integer
        Dim Pre_BMS2() As Integer
        Dim BMS2() As Integer
        Dim warning() As Integer
        Dim T1, T2, T3, T4 As Integer
        Dim barcodeError0, barcodeError1 As Integer
        Dim IL300R0, IL300R1, IL300L0, IL300L1 As Integer
        Dim starttime As Date
        Dim Recharge_SOC As Integer

        Dim MaxPath As Integer
        Dim RePath As Integer
        Dim RetreatPath As Integer
        Dim agv_status As String
        Dim agv_status_time As String
        Dim Pre_agv_status As String



        Dim BMSAlarm1() As Integer
        Dim BMSAlarm2() As Integer
        Dim BMS_fw As String
        Dim bat_SN() As String
        Public Sub New(ByVal x_val As Integer, ByVal Y_val As Integer)
            flag = False
            'Car_Picbox = pic
            'Car_Picbox.Top = Y_val
            ' Car_Picbox.Left = x_val
            'Car_Picbox.Image = Image.FromFile("gray.png")
            status = -2
            online = False
            timeout = 0
            step_i = 999
            cmd_idx = -2
            cmd_type_flag = True
            ipadress = "0.0.0.0"
            subcmd = ""
            Shelf_Car = False
            Shelf_Car_No = 0
            cmd_Shelf_Car_Type = ""
            ReDim device_status(50)
            ReDim Pre_device_status(50)
            ReDim To_AGV(40)
            Read_Err_Count = 0
            Pre_TagID = 0
            move_flag = False
            Wait_count = 0
            heart_bit_count = 0
            Pre_TagID_time = Now
            Run_time = Now
            For i As Integer = 0 To device_status.Length - 1
                device_status(i) = 0
                Pre_device_status(i) = 0

            Next
            For i As Integer = 0 To To_AGV.Length - 1
  
                To_AGV(i) = 0
            Next
            ReDim tagId(240 - 1)
            ReDim action(240 * 2 - 1)
            For i As Integer = 0 To 240 - 1
                tagId(i) = 0

                action(i * 2) = 0
                action(i * 2 + 1) = 0
            Next
            Car_type = "PIN"
            Blocking_car_device_no = 0
            Loading_err_cnt = 0
            sflag = 0
            Block_Point = ""
            Block_Path = ""
            RollData = ""
            main_subcmd = ""
            Lock_user = ""
            empty_time = 0
            Load_time = 0
            Error_time = 0
            Pre_Error_Code = 0
            wait_point = 0
            Particle_idx = 0
            thread_idx = ""
            Pre_Auto = 0
            subcmd_req = ""
            Counter_timer = Now
            step_retry = 0
            rate_point = 0
            SafeSensor = 0
            CommandID = ""
            State = ""
            Pre_State = ""
            width = 54
            height = 26
            ReverseXY = 0
            ReDim BMS1(52)
            ReDim BMS2(52)
            ReDim Pre_BMS1(52)
            ReDim Pre_BMS2(52)
            ReDim warning(100)
            For i As Integer = 0 To 49
                warning(i) = 200 + i
            Next
            For i As Integer = 50 To 100
                warning(i) = 0
            Next

            warning(50) = 15200
            warning(51) = 15202
            warning(52) = 15205
            warning(53) = 15206
            warning(54) = 15400
            warning(55) = 15401
            warning(56) = 15402
            warning(57) = 15403
            warning(58) = 15404
            warning(59) = 15405
            warning(60) = 15407
            warning(61) = 15408
            warning(62) = 15412
            warning(63) = 15421
            warning(64) = 15413
            warning(65) = 15414
            warning(66) = 15415
            warning(67) = 15416
            warning(68) = 15417
            warning(69) = 15418
            warning(70) = 211
            agv_status_time = Now().ToString("yyyy-MM-dd HH:mm:ss")
            Pre_agv_status = "OffLine"

            ReDim BMSAlarm1(20)
            ReDim BMSAlarm2(20)
            ReDim bat_SN(1)
        End Sub
        Public Sub cmd_list_clear()
            ReDim cmd_list(50)
            For i As Integer = 0 To 50
                cmd_list(i) = ""
            Next
        End Sub
        Function get_hart_bit()
            Return device_status(0)
        End Function
        Function get_direction()
            Return device_status(1)
        End Function
        Function get_direction_RL()
            Return device_status(1)
        End Function
        Function get_Speed()
            Return device_status(3)
        End Function
        Function get_auto()
            Return device_status(5) Mod 2
        End Function
        Function get_isbusy()
            Return (device_status(5) >> 1) Mod 2
        End Function
        Function get_action()
            ' 0 未載 1 前載 2 後載 3 全載
            '1 有料 2 無料
            Return device_status(6)
        End Function
        Function get_loading()
            ' 0 未載 1 前載 2 後載 3 全載 

            Dim a As Integer = device_status(7) Mod 4
            '1 有料 2 無料
            Return a
        End Function
        Function get_SOC()
            'If BMS1(14) > 0 And BMS2(14) > 0 Then
            '    If BMS1(14) < BMS2(14) Then
            '        Return BMS1(14)
            '    Else
            '        Return BMS2(14)
            '    End If
            'End If
            If BMS1(14) > 0 Or BMS2(14) > 0 Then

                If BMS1(14) > BMS2(14) And BMS2(14) > 0 Then
                    Return BMS2(14)
                Else
                    Return BMS1(14)
                End If

            End If

            Return 99
        End Function
        Function GetIMAL(ByVal BMS() As Integer)
            GetIMAL = 0
            Dim VC_List(15) As Integer
            Dim maxVC As Integer
            Dim minVC As Integer
            Array.Copy(BMS, 18, VC_List, 0, 16)
            Array.Sort(VC_List)
            maxVC = VC_List(15)
            For i As Integer = 0 To 15
                If VC_List(i) > 0 Then
                    minVC = VC_List(i)
                    Exit For
                End If
            Next
            GetIMAL = maxVC - minVC
        End Function
        Function GetVcMax(ByVal BMS)
            Dim VC_List(15) As Integer
            Array.Copy(BMS, 18, VC_List, 0, 16)
            Array.Sort(VC_List)
            GetVcMax = VC_List(15)
        End Function
        Function GetVcMin(ByVal BMS)
            GetVcMin = 0
            Dim VC_List(15) As Integer
            Array.Copy(BMS, 18, VC_List, 0, 16)
            Array.Sort(VC_List)
            For i As Integer = 0 To 15
                If VC_List(i) > 0 Then
                    GetVcMin = VC_List(i)
                    Exit Function
                End If
            Next
            GetVcMin = VC_List(15)
        End Function
        Function CheckBms(ByVal BMS() As Integer, ByRef BMSAlarm() As Integer)
            CheckBms = 0
            Dim idx As Integer = 0
            If BMSAlarm(16) = BMS(6) And get_Err() = 0 Then
                '心跳重複
                BMSAlarm(17) += 1
            Else
                '心跳不重複
                BMSAlarm(16) = BMS(6)
                BMSAlarm(17) = 0
            End If

            'IMAL
            If GetIMAL(BMS) > 600 Then
                If BMSAlarm(0) > 1 Then
                    '異常
                    CheckBms = 1
                Else
                    BMSAlarm(0) += 1
                End If
            Else
                BMSAlarm(0) = 0
            End If

            idx = 1
            If BMS(10) > 48 Or BMS(11) > 48 Then
                If BMSAlarm(idx) > 1 Then
                    CheckBms += 1 << idx
                Else
                    BMSAlarm(idx) += 1
                End If
            Else
                BMSAlarm(idx) = 0
            End If

            idx = 2
            If BMS(10) > 50 Or BMS(11) > 50 Then
                If BMSAlarm(idx) > 1 Then
                    CheckBms += 1 << idx
                Else
                    BMSAlarm(idx) += 1
                End If
            Else
                BMSAlarm(idx) = 0
            End If

            idx = 4 '警告
            If BMS(10) < 2 Or BMS(11) < 2 Or BMS(10) > 36727 Or BMS(11) > 36727 Then
                If BMSAlarm(idx) > 1 Then
                    CheckBms += 1 << idx
                Else
                    BMSAlarm(idx) += 1
                End If
            Else
                BMSAlarm(idx) = 0
            End If

            idx = 5 '低溫保護
            If (BMS(10) > 36727 And BMS(10) < 65535 - 10) Or (BMS(11) > 36727 And BMS(11) < 65535 - 10) Then
                If BMSAlarm(idx) > 1 Then
                    CheckBms += 1 << idx
                Else
                    BMSAlarm(idx) += 1
                End If
            Else
                BMSAlarm(idx) = 0
            End If

            idx = 8 '過電壓警告
            If GetVcMax(BMS) > 3700 Then
                If BMSAlarm(idx) > 1 Then
                    CheckBms += 1 << idx
                Else
                    BMSAlarm(idx) += 1
                End If
            Else
                BMSAlarm(idx) = 0
            End If
            idx = 9 '過電壓保護
            If GetVcMax(BMS) > 3750 Then
                If BMSAlarm(idx) > 1 Then
                    CheckBms += 1 << idx
                Else
                    BMSAlarm(idx) += 1
                End If
            Else
                BMSAlarm(idx) = 0
            End If

            idx = 12 '充電過電流警告
            If BMS(8) > 450 And BMS(8) < 32767 Then
                If BMSAlarm(idx) > 1 Then
                    CheckBms += 1 << idx
                Else
                    BMSAlarm(idx) += 1
                End If
            Else
                BMSAlarm(idx) = 0
            End If

            idx = 13 '充電過電流保護
            If BMS(8) > 500 And BMS(8) < 32767 Then
                If BMSAlarm(idx) > 1 Then
                    CheckBms += 1 << idx
                Else
                    BMSAlarm(idx) += 1
                End If
            Else
                BMSAlarm(idx) = 0
            End If

            idx = 14 '放電過電流保護
            If BMS(8) > 36728 And BMS(8) < (65535 - 450) Then
                If BMSAlarm(idx) > 1 Then
                    CheckBms += 1 << idx
                Else
                    BMSAlarm(idx) += 1
                End If
            Else
                BMSAlarm(idx) = 0
            End If
            idx = 15 '放電過電流保護
            If BMS(8) > 36728 And BMS(8) < (65535 - 450) Then
                If BMSAlarm(idx) > 1 Then
                    CheckBms += 1 << idx
                Else
                    BMSAlarm(idx) += 1
                End If
            Else
                BMSAlarm(idx) = 0
            End If
        End Function
        Function get_AMP() As Integer
            If BMS1(8) > 32765 Then
                get_AMP = -(65535 - BMS1(8))
            Else
                get_AMP = BMS1(8) / 10
            End If
            If BMS2(8) > 32765 Then
                get_AMP += -(65535 - BMS2(8))
            Else
                get_AMP += BMS2(8)
            End If
        End Function
        Function get_BMSVolt() As Integer
            get_BMSVolt = CInt((BMS1(7) + BMS2(7)) / 2)
        End Function
        Function get_NS() As String
            Dim a As Integer = (device_status(7) >> 5) Mod 4
            If a = 1 Then
                get_NS = "S"
            Else
                get_NS = "N"
            End If
            'Return a
        End Function
        Function get_shelf_loading()
            Dim a As Integer = (device_status(7) >> 2) Mod 2
            Return a
        End Function
        Sub set_loading(ByVal loading As Integer)
            device_status(7) = loading
        End Sub
        Function get_pin()
            ' 上:10 下:5 待測試
            If Car_type = "CRANE" Then
                Return device_status(8)
            Else
                Return device_status(8)
            End If


        End Function
        Function get_interlock()
            Return device_status(9)
        End Function
        Function get_Volt()
            Return device_status(10)
        End Function
        Function get_distance()
            '  Return device_status(17)

            Return device_status(12) * 1000 + device_status(11)

        End Function
        Function get_map()
            Return device_status(13)
        End Function
        Function get_tagId() As Integer
            Return device_status(16)
        End Function
        Sub set_tagId(ByVal tagid As Integer)
            device_status(16) = tagid
        End Sub
        Sub force_tagId(ByRef forcetagid As Integer)
            device_status(16) = forcetagid
            device_status(23) = 99999
            device_status(24) = 99999
            device_status(25) = 0
        End Sub
        Function get_status()
            '啟動 1 停止2 走行中 4
            Return device_status(15)
        End Function
        Function get_Shelf_Car_No()
            '  Return device_status(17)
            If get_loading() = 3 Then
                Return device_status(17)
            Else
                Return 0
            End If
        End Function
        Function get_cstid() As String
            get_cstid = ""
            '  Return device_status(17)
            'If get_loading() = 3 Then
            Dim cst(7) As Byte
            For i As Integer = 0 To 3
                cst(2 * i) = device_status(26 + i) Mod 256
                cst(2 * i + 1) = device_status(26 + i) \ 256
            Next
            get_cstid = System.Text.Encoding.UTF8.GetString(cst, 0, 8)
            get_cstid = get_cstid.TrimEnd("")

            'End If
        End Function

        Function get_Err()
            get_Err = 0
            If Not device_status(9) = 8 Then


                If device_status(18) > 0 Then
                    Return device_status(18)
                ElseIf device_status(19) = 95 Or device_status(20) = 95 Then
                    If Not path_error_tagid = get_tagId() Then
                        path_error_tagid = get_tagId()
                        path_error_count += 1
                    End If
                    Return 0
                ElseIf device_status(19) > 0 Then
                    If Array.IndexOf(warning, device_status(19)) > -1 Then
                        Return 0
                    Else
                        Return device_status(19)
                    End If


                ElseIf Not device_status(9) = 8 Then
                    Return device_status(20)
                Else
                    Return 0
                End If
            End If
        End Function
       
        'Function get_CstID() As String
        '    get_CstID = ""
        '    get_CstID = Chr(device_status(26) Mod 256) + Chr(device_status(26) \ 256)
        '    get_CstID += Chr(device_status(27) Mod 256) + Chr(device_status(27) \ 256)
        '    get_CstID += Chr(device_status(28) Mod 256) + Chr(device_status(28) \ 256)
        '    get_CstID += Chr(device_status(29) Mod 256) + Chr(device_status(29) \ 256)
        '    get_CstID = get_CstID.TrimEnd()

        'End Function

        Function get_lft_action() As Integer
            Return device_status(24)
        End Function


        Sub cmd2Car(ByVal cmd As String, ByVal call_step As Integer)
            If cmd.StartsWith("@") Then
                cmd = cmd.Substring(1)
            End If
            Dim arycmdlist() As String = cmd.Split("@")
            Dim work_list(2, arycmdlist.Length - 1) As Integer
            Dim work_size As Integer = arycmdlist.Length
            For i As Integer = 0 To arycmdlist.Length - 1
                Dim step_list() As String
                step_list = arycmdlist(i).Split(",")
                '0 地圖碼  1  安全功能 走行速度 障礙物 走行方向  '
                '2 保留 SLAM 磁軌秒數 動作(置中,左分起,右分起,停止,原地旋轉 )
                work_list(0, i) = CInt(step_list(0)) 'TAGID

                step_list(2).Substring(1, 1)

                Dim idx As Integer = 0
                If IsNumeric(step_list(2).Substring(1, 1)) Then
                    idx = 1
                Else
                    idx = 2
                End If


                Dim sersor_val As Integer = CInt(step_list(2).Substring(idx))
                Dim work2 As String = step_list(2).Substring(0, idx)

                If Car_type = "FORK" Then
                    work_list(1, i) = 0 '方向 0 往前 1後左 2 後右2


                Else
                    work_list(1, i) = CInt(step_list(1)) '方向 0 往前 1後左 2 後右2
                End If

                Select Case work2
                    Case "M"
                        '置中
                        If Car_type = "PIN" Or Car_type = "ROLL" Or Car_type = "ROBOT" Or Car_type = "CRANE" Then
                            work_list(2, i) = 1 'PING 沒有致中
                        Else
                            work_list(2, i) = 3 * 16
                        End If
                    Case "L"
                        If Car_type = "POWER" Then
                            work_list(2, i) = 2 * 16 + 1
                        Else
                            work_list(2, i) = 1
                        End If
                    Case "R"
                        If Car_type = "POWER" Then
                            work_list(2, i) = 2 * 16 + 2
                        Else
                            work_list(2, i) = 2
                        End If
                    Case "O"
                        work_list(2, i) = 11
                    Case "S"
                        work_list(2, i) = 3
                    Case "X"
                        '右旋
                        If Car_type = "FORK" Then
                            work_list(1, i) = 0
                            work_list(2, i) = 6
                        Else
                            work_list(2, i) = 3 * 16 + 6 '忽視磁軌3秒
                        End If

                    Case "XL"
                        '右旋
                        If Car_type = "FORK" Then
                            work_list(1, i) = 1
                            work_list(2, i) = 6
                        Else
                            work_list(2, i) = 3 * 16 + 6 '忽視磁軌3秒
                        End If
                    Case "XR"
                        '右旋
                        If Car_type = "FORK" Then
                            work_list(1, i) = 2
                            work_list(2, i) = 6
                        Else
                            work_list(2, i) = 3 * 16 + 6 '忽視磁軌3秒
                        End If
                    Case "Y"
                        '左旋
                        If Car_type = "FORK" Then
                            work_list(1, i) = 0
                            work_list(2, i) = 5
                        Else
                            work_list(2, i) = 3 * 16 + 5 '忽視磁軌3秒
                        End If
                    Case "YL"
                        '左旋
                        If Car_type = "FORK" Then
                            work_list(1, i) = 1
                            work_list(2, i) = 5
                        Else
                            work_list(2, i) = 3 * 16 + 5 '忽視磁軌3秒
                        End If

                    Case "YR"
                        '左旋
                        If Car_type = "FORK" Then
                            work_list(1, i) = 2
                            work_list(2, i) = 5
                        Else
                            work_list(2, i) = 3 * 16 + 5 '忽視磁軌3秒
                        End If
                    Case "G"
                        '右旋 180
                        If Car_type = "FORK" Then
                            work_list(2, i) = 10
                        Else
                            work_list(2, i) = 3 * 16 + 9 '忽視磁軌3秒
                        End If
                    Case "GR"
                        '右旋 180
                        If Car_type = "FORK" Then
                            work_list(1, i) = 2 + 4096 * SafeSensor
                            work_list(2, i) = 10
                        Else
                            work_list(2, i) = 3 * 16 + 9 '忽視磁軌3秒
                        End If
                    Case "GL"
                        '右旋 180
                        If Car_type = "FORK" Then
                            work_list(1, i) = 1 + 4096 * SafeSensor
                            work_list(2, i) = 10
                        Else
                            work_list(2, i) = 3 * 16 + 9 '忽視磁軌3秒
                        End If

                    Case "H"
                        '左旋 180 
                        If Car_type = "FORK" Then
                            work_list(2, i) = 9
                        Else
                            work_list(2, i) = 3 * 16 + 8 '忽視磁軌3秒
                        End If

                    Case "HR"
                        '左旋 180 
                        If Car_type = "FORK" Then
                            work_list(1, i) = 2 + 4096 * SafeSensor
                            work_list(2, i) = 9
                        Else
                            work_list(2, i) = 3 * 16 + 8 '忽視磁軌3秒
                        End If

                    Case "HL"
                        '左旋 180 
                        If Car_type = "FORK" Then
                            work_list(1, i) = 1 + 4096 * SafeSensor
                            work_list(2, i) = 9
                        Else
                            work_list(2, i) = 3 * 16 + 8 '忽視磁軌3秒
                        End If

                    Case "FR"
                        If Car_type = "FORK" Then
                            work_list(1, i) = 2 + 4096 * SafeSensor '原本後退是1 ，靠右導航就再+1
                            work_list(2, i) = 4 * 16
                        Else
                            work_list(2, i) = 2
                        End If
                    Case "FL"
                        If Car_type = "FORK" Then
                            work_list(1, i) = 1 + 4096 * SafeSensor '原本後退是1 
                            work_list(2, i) = 4 * 16
                        Else
                            work_list(2, i) = 1
                        End If
                    Case "PR"
                        If Car_type = "FORK" Then
                            work_list(1, i) = 2 + 4096 * SafeSensor '原本後退是1 ，靠右導航就再+1
                            work_list(2, i) = &H10
                        Else
                            work_list(2, i) = 2
                        End If
                    Case "PL"
                        If Car_type = "FORK" Then
                            work_list(1, i) = 1 + 4096 * SafeSensor  '原本後退是1 
                            work_list(2, i) = &H10
                        Else
                            work_list(2, i) = 1
                        End If



                    Case Else
                        work_list(2, i) = 1
                End Select
                work_list(2, i) += &H100
                work_list(1, i) += (sersor_val \ 16) * (2 ^ 13)
                work_list(1, i) += (sersor_val Mod 16) * 2 ^ 4
                work_list(1, i) += Math.Ceiling(CInt(step_list(3)) / 5) * 256 ' 速度
            Next
            For i As Integer = 0 To work_size - 1
                tagId(i) = work_list(0, i)
                action(2 * i) = work_list(1, i)
                action(2 * i + 1) = work_list(2, i)
            Next
            For i As Integer = work_size To 239
                tagId(i) = 0
                action(2 * i) = 0
                action(2 * i + 1) = 0
            Next
            Dim a() As Integer = tagId
            Dim b() As Integer = action
            step_i = call_step '啟動發送程序

            move_flag = True
            Wait_count = 0
            Pre_TagID_time = Now
            Pre_TagID = get_tagId()
            Run_time = Now
        End Sub
        Function get_info() As String
            get_info = ""
            get_info = "  device:" + device_no.ToString + vbCrLf
            get_info += "idx:" + cmd_idx.ToString + "SQL_idx:" + cmd_sql_idx.ToString + vbCrLf
            If cmd_idx = -2 Then
                get_info += "cmd_list:" + vbCrLf

            Else
                get_info += "cmd_list:" + cmd_list(cmd_idx).ToString + vbCrLf
            End If
            Dim direction As String = "後退"
            Dim direction_RL As String = "R"
            If get_direction() = 0 Then
                direction = "前進"
            End If
            If get_direction_RL() = 1 Then
                direction_RL = "L"
            End If
            get_info += "Flag:" + flag.ToString + vbCrLf
            get_info += "Car_type:" + Car_type.ToString + vbCrLf
            get_info += "OnLine:" + online.ToString
            get_info += "Auto:" + get_auto.ToString + vbCrLf
            get_info += "status:" + get_status.ToString
            get_info += "setp:" + step_i.ToString + vbCrLf
            'get_info += "Read Err:" + Read_Err_Count.ToString + vbCrLf
            get_info += "tagId:" + get_tagId.ToString + "PIN:" + get_pin.ToString + vbCrLf
            get_info += "Speed:" + get_Speed.ToString + "方向:" + direction + direction_RL + vbCrLf
            get_info += "IP:" + ipadress.ToString + vbCrLf
            get_info += "Load:" + get_loading.ToString + "Data:" + RollData + vbCrLf
            get_info += "Read_Err_Count:" + Read_Err_Count.ToString + vbCrLf
            get_info += "req:" + subcmd_req + " NS:" + get_NS() + vbCrLf
            get_info += "tag_time:" + Pre_TagID_time.ToString + vbCrLf
            get_info += "from:" + Cmd_From.ToString + "To:" + Cmd_To.ToString + vbCrLf
            get_info += "pos from:" + from_pos.ToString + "To:" + To_pos.ToString + vbCrLf
        End Function

        Function Sql2Cmdlist()
            Dim Car_Now As Integer = get_tagId()
            cmd_list_clear()
            Dim ext_cmd_list() As String
            Dim From_wait As Integer = Cmd_From + 1
            Dim To_wait As Integer = Cmd_To + 1
            Dim step_idx As Integer = 0
            Pre_TagID_time = Now()
            starttime = Now()
            T1 = 0
            T2 = 0
            T3 = 0
            T4 = 0

            ext_cmd_list = ext_cmd.Split(",")
            If ext_cmd_list.Length = 2 And Car_type = "FORK" Then
                If IsNumeric(ext_cmd_list(0)) And IsNumeric(ext_cmd_list(1)) Then
                    From_wait = CInt(ext_cmd_list(0))
                    To_wait = CInt(ext_cmd_list(1))
                End If
            ElseIf ext_cmd_list.Length = 4 And Car_type = "LFT" Then
                If IsNumeric(ext_cmd_list(2)) And IsNumeric(ext_cmd_list(3)) Then
                    From_wait = CInt(ext_cmd_list(2))
                    To_wait = CInt(ext_cmd_list(3))
                End If
            End If
            If Car_Now = 0 And Not Cmd_From = 0 Then
                ' MsgBox("車子讀取位置異常")
            ElseIf Car_Now = Cmd_To And Cmd_From = 0 Then
                ''小命令的Now = To
                If Car_type = "PIN" Or Car_type = "POWER" Then
                    cmd_list(0) = "PINDOWN"
                    cmd_list(1) = "FINSH"
                ElseIf Car_type = "LFT" Then
                    cmd_list(0) = "LFT_CTRL"
                    cmd_list(1) = "FINSH"
                ElseIf Car_type = "FORK" Then
                    cmd_list(0) = "FINSH"
                Else
                    cmd_list(0) = "FINSH"
                End If


                cmd_idx = 0
            ElseIf Cmd_To = 1 Then
                ' 往前 下一個TAG ID 
                cmd_list(0) = "Forward_TagID"
                cmd_list(1) = "GoingNext"
                cmd_list(2) = "FINSH"
                cmd_idx = 0
            ElseIf Cmd_To = -2 Then

                cmd_list(0) = "TagID->0"
                cmd_list(1) = "FINSH"
                cmd_idx = 0
            ElseIf Cmd_To = -1 Then
                ' 往後 下一個TAG ID 
                cmd_list(0) = "Backward_TagID"
                cmd_list(1) = "GoingNext"
                cmd_list(2) = "FINSH"
                cmd_idx = 0
            ElseIf Cmd_To = -3 Then
                cmd_list(step_idx) = "CHECK_SHELF_LOAD_OFF"
                step_idx += 1
                cmd_list(step_idx) = "FINSH"
                cmd_idx = 0 '準備啟動
            ElseIf Cmd_To = -4 Then
                cmd_list(step_idx) = "CHECK_SHELF_LOAD_ON"
                step_idx += 1
                cmd_list(step_idx) = "FINSH"
                cmd_idx = 0 '準備啟動
            ElseIf Cmd_To = -5 Then
                '執行car_sitechange
                cmd_list(step_idx) = "SET_SITE"
                step_idx += 1
                cmd_list(step_idx) = "FINSH"
                cmd_idx = 0 '準備啟動
            ElseIf Cmd_To = 2 Then
                If Car_type = "PIN" Or Car_type = "POWER" Or Car_type = "LFT" Or Car_type = "FORK" Then
                    cmd_list(0) = "PINUP"
                ElseIf Car_type = "ROLL" Then
                    cmd_list(1) = "ROLLOUT"
                ElseIf Car_type = "CRANE" Then
                    cmd_list(1) = "FORKOUT"
                Else
                    cmd_list(0) = "NEXT"
                End If

                cmd_list(1) = "FINSH"
                cmd_idx = 0
            ElseIf Cmd_To = 3 Then
                If Car_type = "PIN" Or Car_type = "POWER" Or Car_type = "LFT" Or Car_type = "FORK" Then
                    cmd_list(0) = "PINDOWN"
                ElseIf Car_type = "ROLL" Then
                    cmd_list(0) = "ROLLIN"
                ElseIf Car_type = "CRANE" Then
                    cmd_list(1) = "FORKIN"
                Else
                    cmd_list(0) = "NEXT"
                End If

                cmd_list(1) = "FINSH"
                cmd_idx = 0

            ElseIf Cmd_To = 5 Then
                '使用ext_cmd 0,0,0 來控制 並使用 109來監控完成
                cmd_list(0) = "ACTION"
                cmd_list(1) = "FINSH"
                cmd_idx = 0
            ElseIf Cmd_To = 6 Then
                ' 收料
                cmd_list(0) = "ROLLOUT"
                cmd_list(1) = "FINSH"
                cmd_idx = 0
            ElseIf Cmd_To = 7 Then
                '鎖車
                cmd_list(0) = "LOCK"
                cmd_list(1) = "FINSH01"
                cmd_idx = 0
            ElseIf Cmd_To = 8 Then
                '解鎖
                cmd_list(0) = "UNLOCK"
                cmd_list(1) = "FINSH01"
                cmd_idx = 0
            ElseIf Cmd_To = -3 Then
                '解鎖
                cmd_list(0) = "WindStart"
                cmd_list(1) = "WindWaiting"
                cmd_list(2) = "WindEnd"
                cmd_list(3) = "FINSH01"

                cmd_idx = 0
            ElseIf Cmd_From = 0 Then
                '小命令
                '; Car(0).cmd_list(0) = "PINDOWN" 

                '    MsgBox(device_no.ToString)
                If Car_type = "PIN" Or Car_type = "POWER" Then
                    If Not Cmd_To = get_tagId() Then
                        cmd_list(step_idx) = Cmd_To.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going"
                        step_idx += 1
                    End If
                    cmd_list(step_idx) = "PINDOWN"   '空車目的地無所謂
                    step_idx += 1
                ElseIf Car_type = "LFT" Then
                    cmd_list(step_idx) = "LFT_DOWN1" '降到走行高度 32 
                    step_idx += 1
                    If Not Cmd_To = get_tagId() Then
                        cmd_list(step_idx) = Cmd_To.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going"
                        step_idx += 1
                    End If
                    cmd_list(step_idx) = "LFT_CTRL"
                    step_idx += 1
                ElseIf Car_type = "FORK" Then
                    cmd_list(step_idx) = (Cmd_To).ToString
                    step_idx += 1
                    cmd_list(step_idx) = "Going"
                    step_idx += 1
                    cmd_list(step_idx) = "PINDOWN"   '空車目的地無所謂
                    step_idx += 1

                Else
                    If Not Cmd_To = get_tagId() Then
                        cmd_list(step_idx) = Cmd_To.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going"
                        step_idx += 1
                    End If
                End If

                cmd_list(step_idx) = "FINSH"
                cmd_idx = 0 '準備啟動
            ElseIf Cmd_From = 1 Then
                '小命令
                '; Car(0).cmd_list(0) = "PINDOWN" 

                If Car_type = "LFT" Then
                    If Not ext_cmd = "" Then
                        cmd_list(step_idx) = "LFT_CTRL"
                        step_idx += 1

                    End If

                    cmd_list(step_idx) = Cmd_To.ToString
                    step_idx += 1
                    cmd_list(step_idx) = "Going"
                    step_idx += 1
                ElseIf Car_type = "FORK" Then
                    cmd_list(step_idx) = Cmd_To.ToString
                    step_idx += 1
                    cmd_list(step_idx) = "Going"
                    step_idx += 1
                ElseIf Not Cmd_To = get_tagId() Then
                    cmd_list(step_idx) = Cmd_To.ToString
                    step_idx += 1
                    cmd_list(step_idx) = "Going"
                    step_idx += 1

                End If
                cmd_list(step_idx) = "FINSH"
                cmd_idx = 0 '準備啟動
            ElseIf Cmd_From = 2 Then
                '小命令
                If Car_type = "PIN" Then

                    If Not Cmd_To = get_tagId() Then
                        cmd_list(step_idx) = Cmd_To.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going"
                        step_idx += 1
                    End If
                    cmd_list(step_idx) = "PINUP"
                    step_idx += 1
                ElseIf Car_type = "ROBOT" Then
                    If Not Cmd_To = get_tagId() Then
                        cmd_list(step_idx) = Cmd_To.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going"
                        step_idx += 1
                    End If
                    cmd_list(step_idx) = "ROBOT"
                    step_idx += 1
                ElseIf Car_type = "FORK" Then

                    cmd_list(step_idx) = Cmd_To.ToString
                    step_idx += 1
                    cmd_list(step_idx) = "Going"
                    step_idx += 1
                    cmd_list(step_idx) = "PINDOWN"
                    step_idx += 1
                    cmd_list(step_idx) = To_wait.ToString
                    step_idx += 1
                    cmd_list(step_idx) = "Going"
                    step_idx += 1
                    cmd_list(step_idx) = "PINUP"
                    step_idx += 1

                ElseIf Car_type = "ROLL" Then
                    If Not Cmd_To = get_tagId() Then
                        cmd_list(step_idx) = Cmd_To.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going"
                        step_idx += 1
                    End If
                    cmd_list(step_idx) = "ROLLOUT"
                    step_idx += 1
                ElseIf Car_type = "CRANE" Then
                    If Not Cmd_To = get_tagId() Then
                        cmd_list(step_idx) = Cmd_To.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going_Check"
                        step_idx += 1
                    End If
                    cmd_list(step_idx) = "FORKOUT"
                    step_idx += 1
                ElseIf Car_type = "LFT" Then
                    cmd_list(step_idx) = "CHECK_LOAD"   '空車目的地無所謂
                    step_idx += 1
                    cmd_list(step_idx) = "LFT_DOWN1" '降到走行高度 32 
                    step_idx += 1
                    If Car_Now = To_wait Then
                        '如果現地為頂PIN點，為確保地點先回到端點在回到頂PIN點
                        cmd_list(step_idx) = (To_wait + 1).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                        step_idx += 1
                        cmd_list(step_idx) = "Going_Check" '走行時保持下降， 如頂PIN不再下降位置則異常
                        step_idx += 1
                    Else
                        cmd_list(step_idx) = "NEXT"  'Car_From+1 是頂PIN位置固定為端點+1
                        step_idx += 1
                        cmd_list(step_idx) = "NEXT" '走行時保持下降， 如頂PIN不再下降位置則異常
                        step_idx += 1
                    End If
                    cmd_list(step_idx) = (To_wait).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                    step_idx += 1
                    cmd_list(step_idx) = "Going_Check" '走行時保持下降， 如頂PIN不再下降位置則異常
                    step_idx += 1
                    cmd_list(step_idx) = "PUT_UP"
                    step_idx += 1
                    cmd_list(step_idx) = (Cmd_To).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                    step_idx += 1
                    cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                    step_idx += 1
                    cmd_list(step_idx) = "PUT_DOWN"
                    step_idx += 1
                    cmd_list(step_idx) = (To_wait + 1).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                    step_idx += 1
                    cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                    step_idx += 1
                    cmd_list(step_idx) = "LFT_DOWN2" '停止高度30
                    step_idx += 1

                End If
                cmd_list(step_idx) = "FINSH"
                cmd_idx = 0 '準備啟動
            ElseIf Cmd_From = 3 Then
                If Car_type = "PIN" Then
                    If Not Cmd_To = get_tagId() Then
                        cmd_list(step_idx) = Cmd_To.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going"
                        step_idx += 1

                    End If
                    cmd_list(step_idx) = "PINDOWN"
                    step_idx += 1
                ElseIf Car_type = "FORK" Then

                    If Not get_tagId() = To_wait Then
                        cmd_list(step_idx) = To_wait.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going"
                        step_idx += 1
                    End If
                    If get_pin() = 10 Then
                        cmd_list(step_idx) = "PINDOWNFORK"
                        step_idx += 1
                    End If

                    cmd_list(step_idx) = (Cmd_To).ToString
                    step_idx += 1
                    cmd_list(step_idx) = "Going"
                    step_idx += 1
                    cmd_list(step_idx) = "PINUP"
                    step_idx += 1
                    cmd_list(step_idx) = To_wait.ToString
                    step_idx += 1
                    cmd_list(step_idx) = "Going_Check"
                    step_idx += 1
                ElseIf Car_type = "ROLL" Then
                    If Not Cmd_To = get_tagId() Then
                        cmd_list(step_idx) = Cmd_To.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going"
                        step_idx += 1

                    End If
                    cmd_list(step_idx) = "ROLLIN"
                    step_idx += 1
                ElseIf Car_type = "CRANE" Then
                    If Not Cmd_To = get_tagId() Then
                        cmd_list(step_idx) = Cmd_To.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going"
                        step_idx += 1

                    End If
                    cmd_list(step_idx) = "FORKIN"
                    step_idx += 1
                ElseIf Car_type = "LFT" Then
                    cmd_list(step_idx) = "CHECK_START"   '空車目的地無所謂
                    step_idx += 1
                    cmd_list(step_idx) = "LFT_DOWN1"
                    step_idx += 1
                    If Car_Now = From_wait Then
                        '如果現地為頂PIN點，為確保地點先回到端點在回到頂PIN點
                        cmd_list(step_idx) = (From_wait + 1).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                        step_idx += 1
                        cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                        step_idx += 1
                    Else
                        cmd_list(step_idx) = "NEXT"  'Car_From+1 是頂PIN位置固定為端點+1
                        step_idx += 1
                        cmd_list(step_idx) = "NEXT" '走行時保持下降， 如頂PIN不再下降位置則異常
                        step_idx += 1
                    End If
                    cmd_list(step_idx) = (From_wait).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                    step_idx += 1
                    cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                    step_idx += 1
                    cmd_list(step_idx) = "TAKE_DOWN"
                    step_idx += 1
                    cmd_list(step_idx) = (Cmd_To).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                    step_idx += 1
                    cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                    step_idx += 1
                    cmd_list(step_idx) = "TAKE_UP"
                    step_idx += 1
                    cmd_list(step_idx) = (To_wait + 1).ToString
                    step_idx += 1
                    cmd_list(step_idx) = "Going_Check" '走行時保持下降， 如頂PIN不再下降位置則異常
                    step_idx += 1
                    cmd_list(step_idx) = "LFT_DOWN1"
                    step_idx += 1
                End If
                cmd_list(step_idx) = "FINSH"
                cmd_idx = 0 '準備啟動
            ElseIf Cmd_From = 4 Then
                If Car_type = "PIN" Then
                    cmd_list(step_idx) = "PINDOWN"
                    step_idx += 1
                ElseIf Car_type = "LFT" Then
                    cmd_list(step_idx) = "LFT_DOWN1"
                    step_idx += 1
                End If
                If Not Cmd_To = get_tagId() Then
                    cmd_list(step_idx) = Cmd_To.ToString
                    step_idx += 1
                    cmd_list(step_idx) = "Going"
                    step_idx += 1
                End If
                If Car_type = "LFT" Then
                    cmd_list(step_idx) = "LFT_DOWN2"
                    step_idx += 1
                End If
                If Car_type = "CRANE" Or Car_type = "ROLL" Or Car_type = "FORK" Then

                    cmd_list(step_idx) = "RECHARGE"
                    step_idx += 1
                End If
                cmd_list(step_idx) = "FINSH"
                cmd_idx = 0 '準備啟動
            ElseIf Cmd_From = 5 Then
                If Car_type = "ROLL" Then
                    cmd_list(step_idx) = Cmd_To.ToString
                    step_idx += 1
                    cmd_list(step_idx) = "Going"
                    step_idx += 1
                    cmd_list(step_idx) = "FORCE_OUT"
                    step_idx += 1
                Else
                    cmd_list(step_idx) = "FINSH"
                End If

                cmd_list(step_idx) = "FINSH"
                cmd_idx = 0 '準備啟動
            ElseIf Cmd_From = 6 Then
                If Car_type = "CRANE" Then
                    cmd_list(step_idx) = "CHECKCHARGER"
                    step_idx += 1
                    cmd_list(step_idx) = Cmd_To.ToString
                    step_idx += 1
                    cmd_list(step_idx) = "Going"
                    step_idx += 1
                    cmd_list(step_idx) = "RECHARGE"
                    step_idx += 1
                Else
                    cmd_list(step_idx) = "FINSH"
                End If

                cmd_list(step_idx) = "FINSH"
                cmd_idx = 0 '準備啟動
            ElseIf Cmd_From = 9 Then
                ' counter 量測
                If Not Cmd_To = get_tagId() Then
                    cmd_list(step_idx) = Cmd_To.ToString
                    step_idx += 1
                    cmd_list(step_idx) = "Going"
                    step_idx += 1
                End If
                cmd_list(step_idx) = "CounterStart"
                step_idx += 1
                cmd_list(step_idx) = "Waiting"
                step_idx += 1
                cmd_list(step_idx) = "FINSH"
                cmd_idx = 0
            Else
                Car_Now = get_tagId()
                If Car_Now > 0 Then
                    If Car_type = "PIN" Then
                        cmd_list(step_idx) = "CHECK_START"   '空車目的地無所謂
                        step_idx += 1
                        If Car_Now = Cmd_From + 1 Then
                            '如果現地為頂PIN點，為確保地點先回到端點在回到頂PIN點
                            cmd_list(step_idx) = Cmd_From.ToString  'Car_From+1 是頂PIN位置固定為端點+1
                            step_idx += 1
                            cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                            step_idx += 1
                        Else
                            cmd_list(step_idx) = "NEXT2"  'Car_From+1 是頂PIN位置固定為端點+1
                            step_idx += 1
                            cmd_list(step_idx) = "NEXT" '走行時保持下降， 如頂PIN不再下降位置則異常
                            step_idx += 1
                        End If
                        cmd_list(step_idx) = (Cmd_From + 1).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                        step_idx += 1
                        cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                        step_idx += 1
                        cmd_list(step_idx) = "PINUP"
                        step_idx += 1
                        cmd_list(step_idx) = Cmd_To.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going_Check"
                        step_idx += 1
                        If Cmd_To Mod 10 = 0 And Not Cmd_To = Cmd_From Then
                            cmd_list(step_idx) = "PINDOWN"
                            step_idx += 1
                        End If

                        cmd_list(step_idx) = "FINSH"
                    ElseIf Car_type = "POWER" Then
                        cmd_list(step_idx) = "CHECK_START"   '空車目的地無所謂
                        step_idx += 1
                        If Car_Now = Cmd_From Then
                            '如果現地為頂PIN點，為確保地點先回到端點在回到頂PIN點
                            cmd_list(step_idx) = (Cmd_From + 1).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                            step_idx += 1
                            cmd_list(step_idx) = "GoingEmpty" '走行時保持下降， 如頂PIN不再下降位置則異常
                            step_idx += 1

                        Else
                            cmd_list(step_idx) = "NEXT2"  'Car_From+1 是頂PIN位置固定為端點+1
                            step_idx += 1
                            cmd_list(step_idx) = "NEXT" '走行時保持下降， 如頂PIN不再下降位置則異常
                            step_idx += 1
                        End If
                        cmd_list(step_idx) = (Cmd_From).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                        step_idx += 1
                        cmd_list(step_idx) = "GoingEmpty" '走行時保持下降， 如頂PIN不再下降位置則異常
                        step_idx += 1
                        cmd_list(step_idx) = "PINUP"
                        step_idx += 1
                        cmd_list(step_idx) = Cmd_To.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going_Check"
                        step_idx += 1
                        If Cmd_To Mod 10 = 0 Then
                            cmd_list(step_idx) = "PINDOWN"
                            step_idx += 1
                        End If

                        cmd_list(step_idx) = "FINSH"
                    ElseIf Car_type = "ROLL" Then
                        'ROLL 的命令

                        cmd_list(step_idx) = "CHECK_START"   '空車目的地無所謂
                        step_idx += 1
                        'If Car_Now = Cmd_From Then
                        '    '如果現地為頂PIN點，為確保地點先回到端點在回到頂PIN點
                        '    cmd_list(step_idx) = (Cmd_From + 2).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                        '    step_idx += 1
                        '    cmd_list(step_idx) = "Going"
                        '    step_idx += 1
                        'Else
                        '    cmd_list(step_idx) = "NEXT2"  'Car_From+1 是頂PIN位置固定為端點+1
                        '    step_idx += 1
                        '    cmd_list(step_idx) = "NEXT" '走行時保持下降， 如頂PIN不再下降位置則異常
                        '    step_idx += 1
                        'End If
                        cmd_list(step_idx) = Cmd_From.ToString  'Car_From+1 是頂PIN位置固定為端點+1
                        step_idx += 1
                        cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                        step_idx += 1

                        'cmd_list(step_idx) = "CHECK_UNLOAD" '走行時保持下降， 如頂PIN不再下降位置則異常
                        ' step_idx += 1


                        cmd_list(step_idx) = "ROLLIN"
                        step_idx += 1
                        cmd_list(step_idx) = Cmd_To.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                        step_idx += 1
                        If Cmd_To Mod 10 = 0 Then

                            ' cmd_list(step_idx) = "CHECK_LOAD" '走行時保持下降， 如頂PIN不再下降位置則異常
                            ' step_idx += 1


                            cmd_list(step_idx) = "ROLLOUT"
                            step_idx += 1
                        End If

                        cmd_list(step_idx) = "FINSH"
                    ElseIf Car_type = "CRANE" Then
                        'CRANE 的命令   
                        cmd_list(step_idx) = "CHECK_START"   '空車目的地無所謂
                        step_idx += 1
                        If Not get_tagId() = Cmd_From Then
                            'If get_tagId() = 3010 Or get_tagId() = 3011 Then
                            '    cmd_list(step_idx) = "3007" '避免撞機台的暫時點位
                            '    step_idx += 1
                            '    cmd_list(step_idx) = "GoingEmpty"
                            '    step_idx += 1
                            'End If
                            cmd_list(step_idx) = Cmd_From.ToString  'Car_From+1 是頂PIN位置固定為端點+1
                            step_idx += 1
                            cmd_list(step_idx) = "GoingEmpty" '走行時保持下降， 如頂PIN不再下降位置則異常
                            step_idx += 1
                        End If

                        cmd_list(step_idx) = "FORKIN"
                        step_idx += 1
                        'If Cmd_From = 3010 Or Cmd_From = 3011 Then
                        '    cmd_list(step_idx) = "3007" '避免撞機台的暫時點位
                        '    step_idx += 1
                        '    cmd_list(step_idx) = "Going_Check"
                        '    step_idx += 1
                        'End If
                        cmd_list(step_idx) = Cmd_To.ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going_Check" '走行時保持下降， 如頂PIN不再下降位置則異常
                        step_idx += 1
                        cmd_list(step_idx) = "FORKOUT"
                        step_idx += 1
                        cmd_list(step_idx) = "FINSH"
                    ElseIf Car_type = "LFT" Then
                        'ROLL 的命令

                        'cmd_list(step_idx) = "CHECK_START"   '空車目的地無所謂
                        'step_idx += 1
                        cmd_list(step_idx) = "CHECK_START"   '空車目的地無所謂
                        step_idx += 1
                        cmd_list(step_idx) = "LFT_DOWN1"
                        step_idx += 1
                        If Car_Now = From_wait Then
                            '如果現地為頂PIN點，為確保地點先回到端點在回到頂PIN點
                            cmd_list(step_idx) = (From_wait + 1).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                            step_idx += 1
                            cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                            step_idx += 1
                        Else
                            cmd_list(step_idx) = "NEXT"  'Car_From+1 是頂PIN位置固定為端點+1
                            step_idx += 1
                            cmd_list(step_idx) = "NEXT" '走行時保持下降， 如頂PIN不再下降位置則異常
                            step_idx += 1
                        End If
                        cmd_list(step_idx) = (From_wait).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                        step_idx += 1
                        cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                        step_idx += 1
                        cmd_list(step_idx) = "TAKE_DOWN"
                        step_idx += 1
                        cmd_list(step_idx) = (Cmd_From).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                        step_idx += 1
                        cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                        step_idx += 1
                        cmd_list(step_idx) = "TAKE_UP" '建帳
                        step_idx += 1
                        cmd_list(step_idx) = (From_wait).ToString
                        step_idx += 1
                        cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                        step_idx += 1
                        cmd_list(step_idx) = "LFT_DOWN1"
                        step_idx += 1
                        If Cmd_To Mod 10 = 0 Then
                            cmd_list(step_idx) = (To_wait).ToString
                            step_idx += 1
                            cmd_list(step_idx) = "Going_Check" '走行時保持下降， 如頂PIN不再下降位置則異常
                            step_idx += 1
                            cmd_list(step_idx) = "PUT_UP"
                            step_idx += 1
                            cmd_list(step_idx) = (Cmd_To).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                            step_idx += 1
                            cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                            step_idx += 1
                            cmd_list(step_idx) = "PUT_DOWN" '移帳
                            step_idx += 1
                            cmd_list(step_idx) = (To_wait + 1).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                            step_idx += 1
                            cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                            step_idx += 1
                            cmd_list(step_idx) = "LFT_DOWN2"
                            step_idx += 1
                        Else
                            cmd_list(step_idx) = (Cmd_To).ToString
                            step_idx += 1
                            cmd_list(step_idx) = "Going_Check" '走行時保持下降， 如頂PIN不再下降位置則異常
                            step_idx += 1

                        End If

                        cmd_list(step_idx) = "FINSH"
                        '大命令的Now =預計取貨的位置
                    ElseIf Car_type = "FORK" Then

                        'MsgBox(ext_cmd)
                        cmd_list(step_idx) = "CHECK_START"   '空車目的地無所謂
                        step_idx += 1
                        If Not get_tagId() Mod 10 And get_pin() = 5 And Not get_tagId() = From_wait Then
                            cmd_list(step_idx) = "PINUP"
                            step_idx += 1
                        Else
                            cmd_list(step_idx) = "NEXT" '走行時保持下降， 如頂PIN不再下降位置則異常
                            step_idx += 1
                        End If
                        cmd_list(step_idx) = From_wait.ToString  'Car_From+1 是頂PIN位置固定為端點+1
                        step_idx += 1
                        cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                        step_idx += 1
                        cmd_list(step_idx) = "PINDOWNFORK"
                        step_idx += 1
                        cmd_list(step_idx) = (Cmd_From).ToString  'Car_From+1 是頂PIN位置固定為端點+1
                        step_idx += 1
                        cmd_list(step_idx) = "Going" '走行時保持下降， 如頂PIN不再下降位置則異常
                        step_idx += 1
                        cmd_list(step_idx) = "PINUP"
                        step_idx += 1
                        cmd_list(step_idx) = Cmd_To.ToString '這邊
                        step_idx += 1
                        cmd_list(step_idx) = "Going_Check"
                        step_idx += 1
                        If Cmd_To Mod 10 = 0 And Not Cmd_To = Cmd_From Then
                            cmd_list(step_idx) = "PINDOWN"
                            step_idx += 1
                            cmd_list(step_idx) = To_wait.ToString
                            step_idx += 1
                            cmd_list(step_idx) = "Going"
                            step_idx += 1
                            cmd_list(step_idx) = "PINUP"
                            step_idx += 1
                        End If

                        cmd_list(step_idx) = "FINSH"
                    Else
                        cmd_list(step_idx) = "FINSH"
                    End If

                    cmd_idx = 0 '準備啟動
                End If
            End If
            Return cmd_idx
        End Function
        Function Get2Derror()
            Get2Derror = int2Toint32(device_status(40), device_status(41))
        End Function
        Function GetILerror()
            If device_status(42) > 32768 Then
                GetILerror = device_status(42) - 65536
            Else
                GetILerror = device_status(42)
            End If
        End Function
    End Structure


    'Function Err_code(ByVal code As Integer)

    '    Select Case code
    '        Case 0
    '            Return ""
    '        Case 1
    '            Return "走行出軌"
    '        Case 4
    '            Return "緊急停止"
    '        Case 5
    '            Return "防撞桿碰撞"
    '        Case 7
    '            Return "電池電壓不足"
    '        Case 8
    '            Return "電池電壓低下"
    '        Case 16
    '            Return "前轉向角感應器故障"
    '        Case 17
    '            Return "後轉向角感應器故障"
    '        Case 32
    '            Return "前右馬達過載"
    '        Case 33
    '            Return "前左馬達過載"
    '        Case 34
    '            Return "後右馬達過載"

    '        Case 35
    '            Return "後左馬達過載"

    '        Case 39
    '            Return "AGV超速運行"

    '        Case 56
    '            Return "AGV遇到障礙物停止"

    '        Case 82
    '            Return "AGV控制參數設定異常"
    '        Case 83
    '            Return "AGV控制器通訊中斷"
    '        Case 100
    '            Return "AGV未取到架台"
    '        Case 101
    '            Return "AGV架錯誤"
    '        Case 102
    '            Return "取架台逾時"
    '        Case 103
    '            Return "目的端有架台"
    '        Case 104
    '            Return "AGV有料無帳"
    '        Case 105
    '            Return "AGV電壓低下"

    '        Case 110
    '            Return "收箱載荷異常"
    '        Case 111
    '            Return "無到達路徑"
    '        Case 112
    '            Return "在席異常"
    '        Case 113
    '            Return "同GROUP"
    '        Case 114
    '            Return "跳探戈"
    '        Case 115
    '            Return "未取到CST"

    '        Case 120
    '            Return "T1光通訊交握 Time Out"
    '        Case 121
    '            Return "T3光通訊交握 Time Out"
    '        Case 122
    '            Return "T5光通訊交握 Time Out"
    '        Case 123
    '            Return "T6光通訊交握 Time Out"
    '        Case 124
    '            Return "T8光通訊交握 Time Out"
    '        Case 125
    '            Return "馬達Alarm"
    '        Case 126
    '            Return "RS-485通訊異常"
    '        Case 127
    '            Return "防撞檢知異常"
    '        Case 130
    '            Return "動力台車斷線"
    '        Case Else
    '            Return "未知異常:" + code.ToString
    '    End Select



    'End Function

  
    Function int2Toint32(ByVal input1 As Integer, ByVal input2 As Integer) As Integer
        Dim a(3) As Byte
        a(0) = input1 Mod 256
        a(1) = input1 \ 256
        a(2) = input2 Mod 256
        a(3) = input2 \ 256
        int2Toint32 = BitConverter.ToInt32(a, 0)
    End Function
    Function byte2str(ByVal cmd() As Byte, ByVal start As Integer, ByVal isize As Integer) As String
        byte2str = ""
        For i As Integer = start To isize - 1
            byte2str += Hex(cmd(i)) + " "
        Next
    End Function

    Function int2hex(ByVal cmd() As Integer, ByVal start As Integer, ByVal isize As Integer) As String
        int2hex = ""
        For i As Integer = start To isize - 1
            int2hex += String.Format("{0:0000}", Hex(cmd(i))) + " "
        Next
    End Function
    Function int2str(ByVal cmd() As Integer, ByVal start As Integer, ByVal isize As Integer) As String
        int2str = ""
        For i As Integer = start To isize - 1
            int2str += String.Format("{0:0000}", cmd(i)) + " "
        Next
    End Function
    Function int2bool(ByVal int As Integer, ByVal start As Integer) As Boolean
        int2bool = (int >> start) Mod 2
    End Function
    Function modbus_write(ByVal netstream As NetworkStream, ByVal device_no As Integer, ByVal start_addr As Integer, ByVal iSize As Integer, ByVal val() As Integer) As Boolean
        Dim Wbyte(500) As Byte
        Dim Rbyte(500) As Byte
        Dim crc As New Bilgic.Net.CRC
        Dim Response_i As Integer
        Dim c As Long

        'heart bit
        Wbyte(0) = device_no
        Wbyte(1) = 16
        Wbyte(2) = start_addr \ 256
        Wbyte(3) = start_addr Mod 256
        Wbyte(4) = iSize \ 256
        Wbyte(5) = iSize Mod 256
        Wbyte(6) = iSize * 2

        For i As Integer = 0 To iSize - 1
            Wbyte(7 + i * 2) = val(i) \ 256
            Wbyte(7 + i * 2 + 1) = val(i) Mod 256
        Next


        c = crc.CRC16(Wbyte, 7 + iSize * 2)
        Wbyte(7 + iSize * 2) = c \ 256
        Wbyte(7 + iSize * 2 + 1) = c Mod 256
        While (netstream.DataAvailable)
            netstream.ReadByte()
        End While
        Try
            netstream.Write(Wbyte, 0, 7 + iSize * 2 + 2)
        Catch ex As Exception

            Return False
        End Try


        For k As Integer = 0 To 150
            If netstream.DataAvailable Then


                Try
                    Response_i += netstream.Read(Rbyte, Response_i, 200 - Response_i)
                Catch ex As Exception

                End Try
                If crc_check(Rbyte, Response_i) Then

                    modbus_write = True

                    Exit For
                End If
            End If
            Thread.Sleep(10)
        Next

    End Function
    Function modbus_read(ByRef netstream As NetworkStream, ByVal device_no As Integer, ByVal addr As Integer, ByVal iSize As Integer, ByRef Response() As Integer) As Boolean
        Dim Wbyte(500) As Byte
        Dim Rbyte(500) As Byte
        Dim crc As New Bilgic.Net.CRC
        Dim c As Long
        Dim Response_i As Integer = 0

        modbus_read = False
        Wbyte(0) = device_no
        Wbyte(1) = 3
        Wbyte(2) = addr \ 256
        Wbyte(3) = addr Mod 256
        Wbyte(4) = iSize \ 256
        Wbyte(5) = iSize Mod 256
        c = crc.CRC16(Wbyte, 6)
        Wbyte(6) = c \ 256
        Wbyte(7) = c Mod 256
        While (netstream.DataAvailable)
            netstream.ReadByte()
        End While
        Try
            netstream.Write(Wbyte, 0, 8)
        Catch ex As Exception

            Return False
        End Try



        For k As Integer = 0 To 150

            If netstream.DataAvailable Then
                Try
                    Response_i += netstream.Read(Rbyte, Response_i, 200 - Response_i)
                Catch ex As Exception
                End Try
                If crc_check(Rbyte, Response_i) And Rbyte(0) = device_no Then
                    For i As Integer = 0 To iSize - 1
                        Response(i) = Rbyte(i * 2 + 3) * 256 + Rbyte(i * 2 + 4)
                    Next
                    modbus_read = True
                    Exit For
                End If
            End If
            Thread.Sleep(10)
        Next



    End Function
    Function crc_check(ByVal cmd() As Byte, ByVal len As Integer)
        Dim crc As New Bilgic.Net.CRC
        Dim c As Long
        If len > 3 Then
            c = crc.CRC16(cmd, len - 2)
            If (c = cmd(len - 2) * 256 + cmd(len - 1)) Then
                Return True
            Else
                If len = 47 Then
                    ' MsgBox(byte2str(cmd, 0, len) + ":" + Hex(c))
                End If
                Return False
            End If
        End If
        Return False
    End Function
    Function Car_dowork(ByRef car As Car_point, ByVal Mysql_str As String) As String
        Dim Wbyte(100) As Byte
        Dim Rbyte(100) As Byte
        Dim status1 As String = ""
        Dim status2 As String = ""
        Dim crc As New Bilgic.Net.CRC
        Dim c As Integer = 0
        Dim write_flag As Boolean = False
        Dim ReadVal(200) As Integer
        Dim status(car.device_status.Length - 1) As Integer
        Dim set_list(20) As Integer
        Dim tagid(240 - 1) As Integer
        Dim action(240 * 2 - 1) As Integer
        Dim ReadTag(240) As Integer
        Dim ReadAction(480) As Integer
        Dim Read_bit As Integer = 100
        Dim tag_bit As Integer = 1000
        Dim action_bit As Integer = 1240
        Car_dowork = ""
        '  Car_dowork +=car.step_i, True)s
        For i As Integer = 0 To 239
            action(i) = 0
            action(i * 2) = 0
            action(i * 2 + 1) = 0
        Next
        If car.flag = True Then
            car.cmd_type_flag = Not car.cmd_type_flag 'true 下命令 flase 讀取狀態 
            If car.cmd_type_flag = True And car.online Then
                '  Car_dowork +="step_i:" + car.step_i.ToString)
                If car.get_isbusy = 0 And car.status >= 0 Then


                    Select Case car.step_i
                        '1~8 初始化
                        Case 1
                            '寫入TagID
                            Dim temp_buffer(59) As Integer

                            Dim i As Integer = 0
                            ' Dim temp_i As Integer = 0
                            write_flag = True
                            Car_dowork += car.device_no.ToString + ":寫地圖"
                            For i = 0 To 29
                                temp_buffer(i) = car.tagId(i)
                            Next
                            write_flag *= modbus_write(car.econ_stream, car.device_no, tag_bit, 30, temp_buffer)
                            writemaplog(int2str(temp_buffer, 0, 30) + ":" + write_flag.ToString, car.device_no)

                            For i = 0 To 59
                                temp_buffer(i) = car.action(i)
                            Next
                            write_flag *= modbus_write(car.econ_stream, car.device_no, action_bit, 60, temp_buffer)
                            writemaplog(int2hex(temp_buffer, 0, 60) + ":" + write_flag.ToString, car.device_no)
                            If (write_flag) Then
                                Car_dowork += car.device_no.ToString + ":S1地圖ON"
                                If car.get_map = 1 Then
                                    car.To_AGV(13) = 0
                                Else
                                    car.To_AGV(13) = 1

                                End If

                                car.To_AGV(15) = 0
                                car.step_i = 3
                                car.step_retry = 0
                            Else
                                car.step_retry += 1
                            End If

                        Case 3
                            ' 地圖寫入HIGH
                            car.To_AGV(13) = 1
                            If car.get_map = 1 Then
                                car.To_AGV(13) = 0
                                Dim flag As Boolean = False
                                For i As Integer = 0 To 29
                                    If car.tagId(i) = car.get_tagId And car.action(i * 2 + 1) = 3 Then
                                        flag = True
                                    End If
                                Next
                                If flag = True Then
                                    Car_dowork += "車子TAG應為停止，不能啟動"
                                    car.To_AGV(15) = 0
                                    car.step_i = 999
                                    car.step_retry = 0
                                Else
                                    Car_dowork += "啟動ON"
                                    car.To_AGV(15) = 1
                                    car.step_i = 4
                                    car.step_retry = 0

                                End If

                            Else
                                Car_dowork += "S3地圖ON"
                                car.step_retry += 1
                            End If
                        Case 4
                            ' 地圖寫入LOW
                            car.To_AGV(13) = 0
                            car.To_AGV(15) = 1
                            If car.get_status Mod 2 = 1 Or car.get_status = 4 Then
                                Car_dowork += car.device_no.ToString + ":啟動OFF"
                                car.To_AGV(15) = 0
                                car.step_i = 999
                                car.step_retry = 0
                            Else
                                Car_dowork += car.device_no.ToString + ":啟動ON:get_map=" + car.get_map.ToString + ",get_status=" + car.get_status.ToString + ",get_tagId=" + car.get_tagId.ToString + ",car.To_pos=" + car.To_pos.ToString
                                car.step_retry += 1
                            End If




                        Case 11
                            '寫入TagID
                            Dim temp_buffer(59) As Integer
                            Dim i As Integer = 0
                            car.To_AGV(13) = 0
                            car.To_AGV(15) = 0
                            ' Dim temp_i As Integer = 0
                            write_flag = True

                            Dim flag As Boolean = False
                            For i = 0 To 29
                                If i < 2 And action(i * 2 + 1) Mod 16 > 4 Then
                                    flag = True
                                    Exit For
                                End If
                                temp_buffer(i) = car.tagId(i)
                            Next
                            If flag = True Then
                                Car_dowork += "未來3點有特殊動作 不更新"
                                car.To_AGV(13) = 0
                                car.To_AGV(15) = 0
                                car.step_i = 999
                                car.step_retry = 0
                            Else
                                Car_dowork += car.device_no.ToString + ":寫地圖" + vbCrLf
                                write_flag *= modbus_write(car.econ_stream, car.device_no, tag_bit, 30, temp_buffer)
                                Car_dowork += car.device_no.ToString + ":寫動作" + vbCrLf
                                writemaplog(int2str(temp_buffer, 0, 30) + ":" + write_flag.ToString, car.device_no)
                                For i = 0 To 59
                                    temp_buffer(i) = car.action(i)
                                Next
                                write_flag *= modbus_write(car.econ_stream, car.device_no, action_bit, 60, temp_buffer)
                                writemaplog(int2hex(temp_buffer, 0, 60) + ":" + write_flag.ToString, car.device_no)
                                If (write_flag) Then
                                    Car_dowork += car.device_no.ToString + ":11地圖ON"
                                    car.To_AGV(13) = 1
                                    car.To_AGV(15) = 0
                                    car.step_i = 12
                                    car.step_retry = 0
                                Else
                                    car.step_retry += 1


                                End If
                            End If


                        Case 12
                            ' 地圖寫入HIGH
                            car.To_AGV(13) = 1
                            car.To_AGV(15) = 0
                            If car.get_map = 1 Then
                                Car_dowork += car.device_no.ToString + ":12地圖ON"
                                car.To_AGV(13) = 0
                                car.To_AGV(15) = 0
                                car.step_i = 999
                                car.step_retry = 0
                            Else
                                Car_dowork += car.device_no.ToString + ":12地圖OFF" + car.step_retry.ToString
                                car.step_retry += 1
                            End If


                        Case 21

                            '切換成手動前進
                            '停止
                            car.To_AGV(15) = 2
                            car.To_AGV(18) = 0
                            car.To_AGV(19) = 0
                            If car.get_status = 0 Or car.get_status = 2 Then
                                car.To_AGV(15) = 0
                                car.step_i += 1
                            End If

                        Case 22
                            car.To_AGV(5) = 1
                            If car.get_auto = 1 Then
                                car.To_AGV(15) = 0
                                car.step_i += 1
                            End If

                        Case 23
                            car.To_AGV(18) = 1
                            car.To_AGV(19) = 50
                            car.step_i += 1
                            car.Manual_TIme = Now
                        Case 24
                            '計時
                            'If Manual_TIme Then
                            If (DateDiff("s", car.Manual_TIme, Now) > 5) Then
                                car.To_AGV(15) = 2
                                car.To_AGV(18) = 0
                                car.To_AGV(19) = 0
                                car.step_i += 1
                            End If
                        Case 25
                            '停止
                            car.To_AGV(15) = 2
                            car.To_AGV(18) = 0
                            car.To_AGV(19) = 0
                            If car.get_status = 0 Or car.get_status = 2 Then
                                car.step_i += 1
                            End If
                        Case 26
                            '手動切換成自動
                            car.To_AGV(5) = 0
                            car.step_i = 999
                        Case 31

                            '切換成手動前進
                            '停止
                            car.To_AGV(15) = 2
                            car.To_AGV(18) = 0
                            car.To_AGV(19) = 0
                            If car.get_status = 0 Or car.get_status = 2 Then
                                car.To_AGV(15) = 0
                                car.step_i += 1
                            End If

                        Case 32
                            car.To_AGV(5) = 1
                            If car.get_auto = 1 Then
                                car.To_AGV(15) = 0
                                car.step_i += 1
                            End If

                        Case 33
                            car.To_AGV(18) = 2
                            car.To_AGV(19) = 50
                            car.step_i += 1
                            car.Manual_TIme = Now
                        Case 34
                            '計時
                            'If Manual_TIme Then
                            If (DateDiff("s", car.Manual_TIme, Now) > 5) Then
                                car.To_AGV(15) = 2
                                car.To_AGV(18) = 0
                                car.To_AGV(19) = 0
                                car.step_i += 1
                            End If
                        Case 35

                            '停止
                            car.To_AGV(15) = 2
                            car.To_AGV(18) = 0
                            car.To_AGV(19) = 0
                            If car.get_status = 0 Or car.get_status = 2 Then
                                car.step_i += 1
                            End If
                        Case 36
                            '手動切換成自動
                            car.To_AGV(5) = 0
                            car.step_i = 999
                        Case 101
                            '切換成手動前進
                            '停止
                            car.To_AGV(15) = 2
                            car.To_AGV(18) = 0
                            car.To_AGV(19) = 0
                            If car.get_status = 0 Or car.get_status = 2 Then
                                car.To_AGV(15) = 0
                                car.step_i += 1
                            End If
                        Case 102
                            car.To_AGV(5) = 1
                            If car.get_auto = 1 Then
                                car.To_AGV(15) = 0
                                car.step_i += 1
                            End If
                        Case 103
                            car.To_AGV(18) = 1
                            car.step_i = 999
                        Case 106
                            '切換成手動後退
                            '停止
                            car.To_AGV(15) = 2
                            car.To_AGV(18) = 0
                            car.To_AGV(19) = 0
                            If car.get_status = 0 Or car.get_status = 2 Then
                                car.step_i += 1
                            End If

                        Case 107
                            car.To_AGV(5) = 1
                            If car.get_auto = 1 Then
                                car.step_i += 1
                            End If
                        Case 108
                            car.To_AGV(19) = 1
                            car.step_i = 999

                        Case 110
                            '手動切換成自動
                            '停止
                            car.To_AGV(15) = 2
                            car.To_AGV(18) = 0
                            car.To_AGV(19) = 0
                            If car.get_status = 0 Then
                                car.step_i += 1
                            End If
                        Case 111
                            car.To_AGV(5) = 0
                            car.step_i = 999
                        Case 900
                            car.To_AGV(15) = 1
                            car.step_i += 1
                        Case 901
                            car.To_AGV(15) = 0
                            car.step_i = 999
                        Case 902
                            car.To_AGV(15) = 2
                            If car.get_status = 2 Then
                                car.To_AGV(15) = 0
                                car.step_i = 999
                            End If

                        Case 904
                            car.To_AGV(20) = 100
                            car.step_i = 999
                        Case 905

                            car.To_AGV(15) = 0
                            car.To_AGV(20) = 0
                            car.step_i = 999
                        Case 903
                            ' Dim ReadTag(240) As Integer
                            ' Dim ReadAction(480) As Integer
                            Dim j As Integer = 0
                            Dim temp_i As Integer = 0
                            write_flag = True

                            For i As Integer = 0 To 240 - 1
                                j += 1
                                If j = 60 Then
                                    write_flag *= modbus_read(car.econ_stream, car.device_no, 1000 + temp_i, j, ReadTag)
                                    Car_dowork += "讀取TagID:" + temp_i.ToString + "," + write_flag.ToString
                                    j = 0
                                    temp_i += 60
                                End If
                            Next
                            If Not j = 0 Then
                                write_flag *= modbus_read(car.econ_stream, car.device_no, 1000 + temp_i, j, ReadTag)
                                Car_dowork += "讀取TagID:" + temp_i.ToString + "," + write_flag.ToString
                            End If
                            j = 0
                            temp_i = 0
                            For i As Integer = 0 To 240 * 2 - 1
                                j += 1
                                If j = 60 Then
                                    write_flag *= modbus_read(car.econ_stream, car.device_no, 1000 + temp_i, j, ReadAction)
                                    Car_dowork += "讀取Action:" + temp_i.ToString + "," + write_flag.ToString
                                    j = 0
                                    temp_i += 60
                                End If
                            Next
                            If Not j = 0 Then
                                write_flag *= modbus_read(car.econ_stream, car.device_no, 1000 + temp_i, j, ReadAction)
                                Car_dowork += "讀取Action:" + temp_i.ToString + "," + write_flag.ToString
                            End If


                            For i As Integer = 0 To 240 - 1
                                ' If ReadTag(i) > 0 Then
                                Car_dowork += ReadTag(i).ToString + "," + ReadAction(i * 2).ToString + "," + ReadAction(i * 2 + 1).ToString + vbCrLf
                                'End If
                            Next
                            car.step_i = 999

                        Case Else
                            car.step_i = 999
                    End Select
                ElseIf Not car.step_i = 999 Then
                    car.step_retry += 1
                End If
                'heart bit
                If car.step_retry > 20 Then
                    car.step_retry = 0
                    car.step_i = 999
                    car.To_AGV(13) = 0
                    car.To_AGV(15) = 0
                    Car_dowork += "啟動失敗"
                End If
                If car.heart_bit_count > 4 Then
                    car.heart_bit_count = 0
                    If car.To_AGV(0) = 1 Then
                        car.To_AGV(0) = 0
                    Else
                        car.To_AGV(0) = 1
                    End If


                End If
                car.heart_bit_count += 1

                write_flag = modbus_write(car.econ_stream, car.device_no, 200, 21, car.To_AGV)


            Else
                If car.cmd_type_flag = True And car.online And car.get_isbusy And car.step_i < 999 Then
                    Car_dowork += "Car Not Ready 105.1 ON"
                End If
                Dim bms1(52), bms2(52) As Integer
                Dim bms1flag, bms2flag As Boolean
                bms2flag = True
                write_flag = modbus_read(car.econ_stream, car.device_no, Read_bit, 50, ReadVal)
                bms1flag = modbus_read(car.econ_stream, car.device_no, 3000, 53, bms1)

                bms2flag = modbus_read(car.econ_stream, car.device_no, 3100, 53, bms2)
                If bms1flag And bms1(0) = 6 Then

                    car.BMS1 = bms1.Clone


                ElseIf (bms1flag = False) Then
                    '消除BMS 不更新計時器
                    car.BMSAlarm1(17) = 0
                ElseIf (bms2flag = False) Then
                    car.BMSAlarm2(17) = 0
                End If

                If bms2flag And bms2(0) = 6 Then

                    car.BMS2 = bms2.Clone
                ElseIf (bms2flag = False) Then
                    car.BMSAlarm2(17) = 0
                End If

                If (write_flag And ReadVal(0) < 60 And ReadVal(5) < 5 And ReadVal(13) < 5 And ReadVal(14) < 5) Then

                    car.Read_Err_Count = 0
                    'car.timeout = 0
                    For i As Integer = 0 To car.device_status.Length - 1
                        'tagid <10 不要更新
                        If i = 16 And ReadVal(i) < 10 Then
                            status(i) = ReadVal(i)
                        Else
                            status(i) = ReadVal(i)
                        End If

                    Next
                    car.device_status = status
                    If Not car.get_Err = 0 Then
                        car.status = -1
                    ElseIf car.get_auto = 1 Then
                        car.status = 3 '手動狀態
                    ElseIf car.device_status(6) > 0 Then
                        car.status = 5 '車上狀態
                    Else
                        car.status = car.get_status
                    End If
                    '清除timeout計數
                    If (Not car.Pre_TagID = car.get_tagId) Then
                        Car_dowork += car.device_no.ToString + ":TagID" + car.Pre_TagID.ToString + "->" + car.get_tagId.ToString
                        If car.subcmd.IndexOf(car.get_tagId.ToString + ",") > -1 Then
                            car.subcmd = car.subcmd.Remove(0, car.subcmd.IndexOf(car.get_tagId().ToString + ","))
                        End If
                        Dim oConn As MySqlConnection
                        Dim sqlCommand As New MySqlCommand
                        Dim Query As String = ""
                        Dim nDay As TimeSpan = Now.Subtract(car.Pre_TagID_time)

                        Dim keep_time As Integer = CInt(nDay.TotalSeconds)
                        If keep_time > 10000 Then
                            keep_time = 0
                        End If
                        car.Pre_TagID_time = Now
                        oConn = New MySqlConnection(Mysql_str)
                        oConn.Open()
                        sqlCommand.Connection = oConn
                        Try

                            Dim VC1_MAX, VC1_MIN As Integer
                            Dim VC2_MAX, VC2_MIN As Integer
                            VC1_MAX = VC2_MAX = 0
                            VC1_MIN = 9999
                            VC2_MIN = 9999

                            For j As Integer = 18 To 32
                                If car.BMS1(j) > VC1_MAX Then
                                    VC1_MAX = car.BMS1(j)
                                End If
                                If car.BMS1(j) < VC1_MIN Then
                                    VC1_MIN = car.BMS1(j)
                                End If
                                If car.BMS2(j) > VC2_MAX Then
                                    VC2_MAX = car.BMS2(j)
                                End If
                                If car.BMS1(j) < VC2_MIN Then
                                    VC2_MIN = car.BMS2(j)
                                End If
                            Next
                            Query = "INSERT INTO `agv_tagid_history` (`AGV_No` ,`Pre_TagID` ,`TagID` ,`RecordTime`,keep_time,cmd_idx,speed,Volt,Loading,Shelf_Car,distance,Temp,Humidity,direction,Auto_Info,AGV_X,AGV_Y,AGV_TH,`VB1`, `IB1`, `BT1`, `SOC1`, `SOH1`, `PROT1`, `STAT1`, `CHG_AH1`, `DSG_AH1`, `CYCLE1`, `VB2`, `IB2`, `BT2`, `SOC2`, `SOH2`, `PROT2`, `STAT2`, `CHG_AH2`, `DSG_AH2`, `CYCLE2`,`VC1_MIN`,`VC1_MAX`,`BT1_2`,`VC2_MIN`,`VC2_MAX`,`BT2_2`,subcmd,bat_SN1,bat_SN2) " + _
                         " VALUES ('" + car.device_no.ToString + "', '" + car.Pre_TagID.ToString + "', '" + car.get_tagId.ToString + "', '" + Now().ToString("yyyy-MM-dd HH:mm:ss") + "','" + keep_time.ToString + "'," + car.cmd_sql_idx.ToString + "," + car.get_Speed.ToString + "," + car.get_Volt.ToString + "," + car.device_status(7).ToString + "," + car.get_Shelf_Car_No.ToString + "," + car.get_distance.ToString + "," + car.device_status(21).ToString + "," + car.device_status(22).ToString + "," + car.get_direction.ToString + "," + car.status.ToString + "," + car.AXIS_X.ToString + "," + car.AXIS_Y.ToString + "," + car.AXIS_Z.ToString + _
                         "," + car.BMS1(7).ToString + "," + car.BMS1(8).ToString + "," + car.BMS1(10).ToString + "," + car.BMS1(14).ToString + "," + car.BMS1(15).ToString + "," + car.BMS1(16).ToString + "," + car.BMS1(17).ToString + "," + (car.BMS1(37) * 65536 + car.BMS1(38)).ToString + "," + (car.BMS1(39) * 65536 + car.BMS1(40)).ToString + "," + car.BMS1(41).ToString + _
                         "," + car.BMS2(7).ToString + "," + car.BMS2(8).ToString + "," + car.BMS2(10).ToString + "," + car.BMS2(14).ToString + "," + car.BMS2(15).ToString + "," + car.BMS2(16).ToString + "," + car.BMS2(17).ToString + "," + (car.BMS2(37) * 65536 + car.BMS2(38)).ToString + "," + (car.BMS2(39) * 65536 + car.BMS2(40)).ToString + "," + car.BMS2(41).ToString + _
                         "," + VC1_MIN.ToString + "," + VC1_MAX.ToString + "," + car.BMS1(11).ToString + "," + VC2_MIN.ToString + "," + VC2_MAX.ToString + "," + car.BMS2(11).ToString + _
                         ",'" + car.subcmd + "','" + car.bat_SN(0) + "','" + car.bat_SN(1) + "');"
                            sqlCommand.CommandText = Query
                            sqlCommand.ExecuteNonQuery()
                            car.Pre_TagID = car.get_tagId

                        Catch ex As Exception
                            Car_dowork += "SQL:" + Query + ":ERROR" + vbCrLf
                        End Try
                        oConn.Close()
                        oConn.Dispose()

                    End If

                Else
                    car.status = -2
                    car.Read_Err_Count += 1
                    If car.Read_Err_Count = 15 Then
                        Dim oConn As MySqlConnection
                        Dim sqlCommand As New MySqlCommand
                        Dim Query As String = ""
                        oConn = New MySqlConnection(Mysql_str)
                        oConn.Open()
                        Try

                            Try
                                sqlCommand.Connection = oConn
                                Query = "INSERT INTO `agv_event` (`Car_No` ,`Event` ,`Event_Time` ,`Tag_ID` ,`IP_Addr`,cmd_idx)VALUES ('" + car.device_no.ToString + "', 'OFFLINE', now(), " + car.get_tagId.ToString + ", '" + car.ipadress.ToString + "', " + car.cmd_sql_idx.ToString + ");"
                                sqlCommand.CommandText = Query
                                sqlCommand.ExecuteNonQuery()
                            Catch ex As Exception
                                Car_dowork += "SQL:" + Query + ":ERROR" + vbCrLf
                            End Try

                        Catch ex As Exception
                            Car_dowork += "SQL:" + Query + ":ERROR" + vbCrLf
                        End Try
                        oConn.Close()
                        oConn.Dispose()
                    End If
                    ' Car_dowork += "狀態讀取:" + write_flag.ToString + "(" + int2str(ReadVal, 0, 21) + ")"
                End If
            End If
        End If
    End Function
    Sub writeAGVLog(ByVal str As String)
        Dim sw As StreamWriter = New StreamWriter(Now().ToString("yyyyMMdd") + "_toAGV.log", True, Encoding.Default)
        sw.Write(Now.ToString + ":" + str + vbCrLf)
        sw.Flush()
        sw.Close()
    End Sub

    Sub Dec2Bin(ByVal k As Integer, ByRef bool() As Integer)
        Dim n As Integer = k
        For i As Integer = 0 To bool.Length - 1
            bool(i) = (n Mod 2 ^ (i + 1)) \ 2 ^ (i)
            n = n - (n Mod 2 ^ (i + 1))
        Next
    End Sub
    'Function Cheak_Path2(ByVal car_subcmd As String, ByVal Path_Used                                Car_dowork += "SQL:" + Query + ":ERROR" + vbCrLf  '    For j As Integer = 0 To a.Length - 1
    '        If car_subcmd.IndexOf("," + a(j) + ",") > 0 Then
    '            car_subcmd = car_subcmd.Substring(0, car_subcmd.IndexOf("," + a(j) + ","))
    '        ElseIf car_subcmd.EndsWith(("," + a(j))) Then
    '            car_subcmd = car_subcmd.Substring(0, car_subcmd.IndexOf("," + a(j)))
    '        ElseIf car_subcmd.StartsWith((a(j) + ",")) Then
    '            car_subcmd = car_subcmd.Substring(0, car_subcmd.IndexOf(a(j) + ","))
    '        End If
    '    Next
    '    Return car_subcmd
    'End Function
    Function Check_Path_group(ByVal car_subcmd As String, ByVal Path_Used As String)

        Dim a() As String
        Dim subcmd_list() As String = car_subcmd.Split(",")
        Dim subcmd_len As Integer = subcmd_list.Length
        a = Path_Used.Split(",")


        For j As Integer = 0 To a.Length - 1
            For k As Integer = 0 To subcmd_len - 1
                If a(j) = subcmd_list(k) Then
                    subcmd_len = k
                    k = 99999999
                    If subcmd_len <= 1 Then
                        Return subcmd_list(0)
                    End If
                    Array.Resize(subcmd_list, subcmd_len)
                End If
            Next
        Next
        car_subcmd = String.Join(",", subcmd_list)
        Return car_subcmd
    End Function
    Function Check_Path(ByVal cmd1 As String, ByVal cmd2 As String, ByVal width As Integer, ByVal height As Integer, ByVal Tag_point_list As Object, Optional ByVal PointX As Integer = 0, Optional ByVal PointY As Integer = 999999, Optional ByVal PointTh As Integer = 999999)
        Dim point1() As String = cmd1.Split(",")
        Dim point2() As String = cmd2.Split(",")
        Dim idx1(point1.Length - 1) As Integer
        Dim idx2(point2.Length - 1) As Integer
        'Dim ii As Integer = 0
        ' Dim jj As Integer = 0

        For j As Integer = 0 To idx1.Length - 1
            idx1(j) = -1
            For i As Integer = 0 To Tag_point_list.Length - 1
                If point1(j).ToString = Tag_point_list(i).TagId.ToString Then
                    idx1(j) = i

                End If
            Next
            If idx1(j) = -1 Then
                Return ""
            End If
        Next
        For j As Integer = 0 To idx2.Length - 1
            idx2(j) = -1
            For i As Integer = 0 To Tag_point_list.Length - 1
                If point2(j) = Tag_point_list(i).TagId.ToString Then
                    idx2(j) = i
                End If
            Next
        Next
        Dim minX1, maxX1, minY1, maxY1 As Integer
        Dim minX2, maxX2, minY2, maxY2 As Integer
        Dim len As Integer = point1.Length

        For i As Integer = 1 To idx1.Length - 1
            If idx1(i) > -1 Then
                Dim k As Integer = i - 1
                '先計算 cmd1 的
                If Tag_point_list(idx1(i)).th = Tag_point_list(idx1(k)).th Then
                    If Tag_point_list(idx1(i)).th = 0 Or Tag_point_list(idx1(i)).th = 180 Then
                        minX1 = Tag_point_list(idx1(i)).X - width / 2
                        maxX1 = Tag_point_list(idx1(i)).X + width / 2
                        minY1 = Tag_point_list(idx1(i)).Y - height / 2
                        maxY1 = Tag_point_list(idx1(i)).Y + height / 2
                    ElseIf Tag_point_list(idx1(i)).th = 90 Or Tag_point_list(idx1(i)).th = -90 Then
                        minX1 = Tag_point_list(idx1(i)).X - height / 2
                        maxX1 = Tag_point_list(idx1(i)).X + height / 2
                        minY1 = Tag_point_list(idx1(i)).Y - width / 2
                        maxY1 = Tag_point_list(idx1(i)).Y + width / 2
                    End If
                Else
                    '下一個旋轉的話就 展開成正方形
                    Dim MaxVal As Integer = Math.Max(height, width) - 70
                    minX1 = Tag_point_list(idx1(i)).X - MaxVal / 2
                    maxX1 = Tag_point_list(idx1(i)).X + MaxVal / 2
                    minY1 = Tag_point_list(idx1(i)).Y - MaxVal / 2
                    maxY1 = Tag_point_list(idx1(i)).Y + MaxVal / 2
                End If

                '先跟K車的旋轉矩形 比對
                Dim X(3) As Integer
                Dim Y(3) As Integer
                X(0) = (width / 2) * Math.Cos(PointTh * Math.PI / 180) + (height / 2) * Math.Sin(PointTh * Math.PI / 180) + PointX
                Y(0) = -(width / 2) * Math.Sin(PointTh * Math.PI / 180) + (height / 2) * Math.Cos(PointTh * Math.PI / 180) + PointY
                X(1) = (width / 2) * Math.Cos(PointTh * Math.PI / 180) + (-height / 2) * Math.Sin(PointTh * Math.PI / 180) + PointX
                Y(1) = -(width / 2) * Math.Sin(PointTh * Math.PI / 180) + (-height / 2) * Math.Cos(PointTh * Math.PI / 180) + PointY
                X(3) = (-width / 2) * Math.Cos(PointTh * Math.PI / 180) + (height / 2) * Math.Sin(PointTh * Math.PI / 180) + PointX
                Y(3) = -(-width / 2) * Math.Sin(PointTh * Math.PI / 180) + (height / 2) * Math.Cos(PointTh * Math.PI / 180) + PointY
                X(2) = (-width / 2) * Math.Cos(PointTh * Math.PI / 180) + (-height / 2) * Math.Sin(PointTh * Math.PI / 180) + PointX
                Y(2) = -(-width / 2) * Math.Sin(PointTh * Math.PI / 180) + (-height / 2) * Math.Cos(PointTh * Math.PI / 180) + PointY
                Array.Sort(X)
                Array.Sort(Y)

                minX2 = X(0)
                maxX2 = X(3)




                minY2 = Y(0)
                maxY2 = Y(3)
                If (maxX1 > minX2 And maxX2 > minX1 And maxY1 > minY2 And maxY2 > minY1) Then
                    len = i
                    Exit For
                End If
                For j As Integer = 0 To idx2.Length - 1

                    If idx2(j) > -1 Then

                        Dim k2 As Integer = j + 1

                        If j = idx2.Length - 1 Then
                            k2 = j '如果是最後一個 代表不用檢查
                        End If


                        If Tag_point_list(idx2(j)).th = Tag_point_list(idx2(k2)).th Then
                            If Tag_point_list(idx2(j)).th = 0 Or Tag_point_list(idx2(j)).th = 180 Then
                                minX2 = Tag_point_list(idx2(j)).X - width / 2
                                maxX2 = Tag_point_list(idx2(j)).X + width / 2
                                minY2 = Tag_point_list(idx2(j)).Y - height / 2
                                maxY2 = Tag_point_list(idx2(j)).Y + height / 2
                            ElseIf Tag_point_list(idx2(j)).th = 90 Or Tag_point_list(idx2(j)).th = -90 Then
                                minX2 = Tag_point_list(idx2(j)).X - height / 2
                                maxX2 = Tag_point_list(idx2(j)).X + height / 2
                                minY2 = Tag_point_list(idx2(j)).Y - width / 2
                                maxY2 = Tag_point_list(idx2(j)).Y + width / 2
                            End If

                        Else
                            Dim MaxVal As Integer = Math.Max(height, width)
                            minX2 = Tag_point_list(idx2(j)).X - MaxVal / 2
                            maxX2 = Tag_point_list(idx2(j)).X + MaxVal / 2
                            minY2 = Tag_point_list(idx2(j)).Y - MaxVal / 2
                            maxY2 = Tag_point_list(idx2(j)).Y + MaxVal / 2
                        End If
                        If (maxX1 > minX2 And maxX2 > minX1 And maxY1 > minY2 And maxY2 > minY1) Then
                            len = i
                            Exit For
                        End If

                    End If
                Next
                If Not len = point1.Length Then
                    Exit For
                End If
            End If
        Next

        Return String.Join(",", point1, 0, len)
    End Function
    Function Cut_before_Path(ByVal car_subcmd As String, ByVal Path_Used As String)

        Dim subcmd_list() As String = car_subcmd.Split(",")
        Dim subcmd_len As Integer = subcmd_list.Length
        Dim flag As Boolean = False
        Dim subcmd(subcmd_len - 1) As String
        Dim i As Integer = 0
        For k As Integer = 0 To subcmd_len - 1
            If Path_Used = subcmd_list(k) Or flag = True Then
                flag = True
                subcmd(i) = subcmd_list(k)
                i += 1
            End If
        Next
        Array.Resize(subcmd, i)
        car_subcmd = String.Join(",", subcmd)
        Return car_subcmd
    End Function
    Function In_Subcmd(ByVal car_subcmd As String, ByVal Point As String)
        Dim subcmd_list() As String = car_subcmd.Split(",")
        Dim Point_list() As String = Point.Split(",")
        'For i As Integer = 0 To subcmd_list.Length - 1
        '    If subcmd_list(i).Length = 5 Then
        '        subcmd_list(i) = subcmd_list(i).Substring(1)
        '    End If
        'Next
        For i As Integer = 0 To Point_list.Length - 1
            'If Point_list(i).Length = 5 Then
            '    Point_list(i) = Point_list(i).Substring(1)
            'End If
            If Array.IndexOf(subcmd_list, Point_list(i)) >= 0 Then
                Return True
            End If

        Next
        Return False
    End Function
    Sub writemaplog(ByVal str As String, ByVal device_no As Integer)
        Try
            Dim sw As StreamWriter = New StreamWriter(".\log\" + Now().ToString("yyyyMMdd") + "_map_" + device_no.ToString + ".log", True, Encoding.Default)
            sw.Write(Now.ToString("HH:mm:ss") + " " + str + vbCrLf)
            sw.Flush()
            sw.Close()
        Catch ex As Exception
        End Try

    End Sub
End Module
