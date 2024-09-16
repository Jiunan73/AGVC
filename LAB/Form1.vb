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

Public Class Form1


    Dim car_no As Integer = 30 '變更數量 dowork要改
    Dim Car(car_no) As Car_point
    Dim shelf_car_total_no As Integer = 280
    Dim shelf_car(shelf_car_total_no) As shelf_car_point
    Dim path(100, 2) As String
    Dim Tag_point_list(5000) As Tag_Point
    Delegate Sub settextcallback(ByVal logout As String, ByVal append As Boolean)
    Delegate Sub setcommtextcallback(ByVal logout As String, ByVal append As Boolean)
    Delegate Sub connectlogcallback(ByVal logout As String, ByVal append As Boolean)
    Dim Door_List(30) As Door_point
    Dim LFT_List(6) As LFT_point
    Dim path_S(200) As SPath
    Dim path_base(5000) As Path
    Dim path_fork_base(5000) As Path
    Dim Dijkstra_list(20) As Dijkstra_ary
    Dim AgvTimeoutVal As Integer = 99999

    Dim AGVratio As Double
    Dim Tag_ID_List(5000) As Integer
    Dim Tag_ID_Fork_List(5000) As Integer
    '  Dim Read_modbus(500) As Integer
    Dim log_filename As String = ".\log\" + Now.ToString("yyyyMMdd") + ".log"
    Dim log As StreamWriter

    Dim connectlogtxt_filename As String = "connect" + Now.ToString("yyyyMMdd") + ".log"
    Dim connectlogtxt As StreamWriter
    Dim view_car_idx As Integer = 0
    Public Mysql_str As String = "charset=utf8 ;Database=agv; Data Source=127.0.0.1;User id=agvc;Password=agv; Allow Zero Datetime=True;"

    Dim offset_x As Integer = 0
    Dim offset_y As Integer = 0
    Dim group_path(600) As String
    Dim MyDb As String = "agv"
    Dim IP_adress As String = "127.0.0.1"
    Dim user As String = "agvc"
    Dim password As String = "FA$admin01"

    ' Dim PathLen As Integer = 7
    ' Dim ReMapLen As Integer = 4
    Dim DoorSetLen As Integer = 2
    'Dim Rotate_list(500) As Integer
    'Dim Fork_point As String
    'Dim ALL_Loading_check As Boolean = True

    '  Dim thread_idx As Long = 0
    Dim ConnectionState As Integer = 0
    Dim Pre_ControlState As Integer = 2
    Dim Pre_SCState As Integer = 0
    Private WithEvents comQSWrapper As New SECSComm
    Public WithEvents comQGWrapper As New GEM
    Dim eqp_client(50) As EQP_Modus
    Dim ChargerClient(10) As EQP_Modus

    Dim alarm As ALM
    Dim start_flag As Boolean = False
    Dim McsPort As Integer = 5001
    Dim AutoReset(100) As Integer
    Dim SOC As Integer = 100
    Dim AllBlockPoint As String = ""
    Dim AllBlockPointList() As String
    Dim lablelist(200) As lableTxt
    Dim BmsAlertIdx As Integer = (1) + (1 << 2) + (1 << 3) + (1 << 5) + (1 << 7) + (1 << 9) + (1 << 13) + (1 << 15) ' 41645 '1010 0010 1010 1101
    Dim BmsWarmIdx As Integer = (1 << 14) + (1 << 12) + (1 << 11) + (1 << 10) + (1 << 8) + (1 << 4) + (1 << 1)  '1010 0010 1010 1101
    Dim testval As Integer = 0
    Public Declare Function SetLocalTime Lib "Coredll.dll" (ByRef lpSystemTime As SYSTEMTIME) As Integer
    Public Structure SYSTEMTIME
        Public Year As Short
        Public Month As Short
        Public DayOfWeek As Short
        Public Day As Short
        Public Hour As Short
        Public Minute As Short
        Public Second As Short
        Public Milliseconds As Short
    End Structure
    Public Sub QSEvent(ByVal lID As Integer, ByVal lMsgID As Integer, ByVal S As Integer, ByVal F As Integer, ByVal W_Bit As Integer, ByVal ulSystemBytes As UInteger, ByVal RawData As Object, ByVal Head As Object, ByVal pEventText As String) Handles comQSWrapper.QSEvent

        Dim lOffset As Integer = 0
        Dim lItemNum As Integer = 0
        Dim ItemData As Object = New Object
        Dim OutputRawData(1000) As Byte
        Dim DataItemOutLen As Integer = 0

        lOffset = 0
        If lID = 2 And lMsgID = 3 Then
            setCommtext("MCS 建立連線:" + ulSystemBytes.ToString)
        ElseIf lID = 2 And lMsgID = 15 Then
            setCommtext("MCS Link Test:" + ulSystemBytes.ToString)
        ElseIf lID = 2 And lMsgID = 5 Then
            Dim data() As Byte = CType(RawData, Byte())
            setCommtext("MCS Send:" + ulSystemBytes.ToString + byte2str(data, 0, ulSystemBytes).ToString)
        ElseIf lID = 2 And lMsgID = 6 Then
            Dim data() As Byte = CType(RawData, Byte())
            setCommtext("MCS Send Err:" + ulSystemBytes.ToString + byte2str(data, 0, ulSystemBytes).ToString)
        ElseIf lID = 1 And lMsgID = 16 Then
            Dim data() As Byte = CType(RawData, Byte())
            setCommtext("MCS ReviceLen>0:""剩下S" + S.ToString + "F" + F.ToString + ":" + byte2str(data, 0, data.Length).ToString)
        ElseIf lID = 3 And lMsgID = 1 Then
            Dim data() As Byte = CType(RawData, Byte())
            setCommtext("MCS recv(" + ulSystemBytes.ToString + "):" + byte2str(data, 0, ulSystemBytes).ToString)
        ElseIf lID = 3 And lMsgID = 2 Then
            Dim data() As Byte = CType(RawData, Byte())
            setCommtext("MCS-cut1(" + ulSystemBytes.ToString + "):" + byte2str(data, 0, ulSystemBytes).ToString)
        ElseIf lID = 4 And lMsgID = 1 Then
            '紀錄LOG
            Dim data() As Byte = CType(RawData, Byte())
            setCommtext("MCS-showErr(" + ulSystemBytes.ToString + "):" + byte2str(data, 0, ulSystemBytes).ToString)
        ElseIf lID = 4 And lMsgID = 2 Then
            'ERR MSG
            Try
                setCommtext("ErrMsg:" + ulSystemBytes.ToString + " " + pEventText)
                Dim data() As Byte = CType(RawData, Byte())
                setCommtext("MCS-showErr(" + ulSystemBytes.ToString + "):" + byte2str(data, 0, 500).ToString)
            Catch ex As Exception

            End Try

            'Else
            '    setCommtext("UNKNOWN:" + S.ToString + "," + F.ToString + ulSystemBytes.ToString + " " + pEventText.ToString)
        End If

        If S > 0 And F > 0 Then
            setCommtext("system:" + ulSystemBytes.ToString + "收到S" + S.ToString + "F" + F.ToString)
            ShowSECSIIMessage(RawData)
            If S = 1 And F = 17 Then
                '判斷系統有沒有AUTO 
                Dim SC_State As Integer
                'PAUSE 切換
                comQGWrapper.GetSV(GEM_SC_STATE, 1, SC_State)
                If SC_State = GEM.SC_AUTO Then
                    'comQGWrapper.UpdateSV(GEM_CONTROL_STATE, GEM.OnlineLoaclval)
                End If



                comQGWrapper.EventReportSend(2)




            ElseIf S = 2 And F = 41 And W_Bit = 1 Then
                'RCMD ABORT CANCEL PAUSE  RESUME REMOVE PRIORITYUPDATE處理命令 
                'INSTALL 上報CARRIERID  CARRIERTYPESET
                'PORTMODECHANGE
                'SCAN
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing)
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, ItemData)
                If ItemData = "CANCEL" Then
                ElseIf ItemData = "ABORT" Then
                ElseIf ItemData = "PAUSE" Then
                    comQGWrapper.UpdateSV(GEM_SC_STATE, GEM.SC_PAUSING)
                ElseIf ItemData = "RESUME" Then
                    comQGWrapper.UpdateSV(GEM_SC_STATE, GEM.SC_AUTO)
                    'comQGWrapper.EventReportSend(53)
                    start_flag = True
                ElseIf ItemData = "INSTALL" Then

                    'ItemData
                    Dim loc As String = ""
                    Dim cstid As String = ""
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing) 'L3
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing) 'L2
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) '"CARRIERID"
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) 'CARRIERID
                    cstid = ItemData
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing) 'L2
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) '"CARRIERLOC"
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) 'CARRIERLOC
                    loc = ItemData
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing) 'L2
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) '"CARRIERTYPE"
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) 'CARRIERTYPE
                    Dim locPoint As New Tag_Point
                    Dim Query As String
                    locPoint = Seach_Tagid(loc)
                    If Not locPoint.LOC = "" Then

                        comQGWrapper.CST_Add(cstid, locPoint.ZONE_NAME, locPoint.LOC)
                        Query = "insert ignore into `carrier` (STK_NAME,CARRIER_ID,LOT_ID,LOC_NAME,LOC_TYPE,SUB_LOC,CARRIER_STATUS,STORED_TIME,REQ_TIME,UPDATE_DATE,UPDATE_BY,CREATE_DATE,CREATE_BY) " + _
                         "VALUES ('" + comQGWrapper.Eqpname + "', '" + cstid + "', '" + cstid + "','" + locPoint.ZONE_NAME + "',1,'" + locPoint.LOC + "', '4', now(), '', now(), 'AGVC', now(), 'AGVC');"
                        Update_SQL(Query)
                    End If


                ElseIf ItemData = "REMOVE" Then

                    Dim cstid As String = ""
                    Dim idx As Integer = -1
                    Dim Query As String
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing) 'L3
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing) 'L2
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) '"CARRIERID"
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) 'CARRIERID
                    cstid = ItemData
                    comQGWrapper.CST_REMOVE(cstid)
                    Query = "delete from `carrier` where CARRIER_ID='" + cstid + "';"
                    Update_SQL(Query)
                ElseIf ItemData = "PORTTYPCHG" Then
                ElseIf ItemData = "SCAN" Then
                ElseIf ItemData = "CARRIERTYPESET" Then
                    Dim CSTID As String = ""
                    Dim CarrierType As String = ""
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing) 'L2
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing) 'L2
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) '"CSTID"
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) 'CSTID
                    CSTID = ItemData
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing) 'L2
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) '"Priority"
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) 'CarrierType
                    CarrierType = ItemData
                    For i As Integer = 0 To comQGWrapper.CST.Length - 1
                        If comQGWrapper.CST(i).CarrierID = CSTID Then
                            comQGWrapper.CST(i).CarrierType = CarrierType
                            Update_SQL("UPDATE `carrier` SET `CARRIER_TYPE` = '" + CarrierType + "' WHERE `carrier`.`CARRIER_ID` = '" + comQGWrapper.CST(i).CarrierID + "'")
                            Exit For
                        End If

                    Next
                ElseIf ItemData = "PRIORITYUPDATE" Then
                    Dim cmdid As String = ""
                    Dim Priority As UShort
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing) 'L2
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing) 'L2
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) '"Commandid"
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) 'Commandid
                    cmdid = ItemData
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing) 'L2
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) '"Priority"
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) 'Priority
                    Priority = ItemData(0)
                    For i As Integer = 0 To comQGWrapper.CST.Length - 1
                        If comQGWrapper.CST(i).CommandID = cmdid Then
                            comQGWrapper.CST(i).PRIORITY = Priority
                            Update_SQL("update mcs_cmd_list set PRIORITY=" + comQGWrapper.CST(i).PRIORITY.ToString + " where CARRIERID='" + comQGWrapper.CST(i).CarrierID + "'")
                        End If

                    Next

                End If


                ' comQSWrapper.SendSECSIIMessage(S, F + 1, 0, ulSystemBytes, OutputRawData)
            ElseIf S = 2 And F = 49 Then
                Dim temp_carrier As GEM.Carrier = New GEM.Carrier
                lOffset = 0
                'ItemData
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing)
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.UINT_2_TYPE, lItemNum, ItemData) 'Data ID = 0 (Fixed) 
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) 'Not Used 
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) '
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing)
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing)
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) '
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing)
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing)
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) '
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) '“COMMANDID” 
                temp_carrier.CommandID = ItemData
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing)
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData)
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.UINT_2_TYPE, lItemNum, ItemData) '“PRIORITY” 
                temp_carrier.PRIORITY = ItemData(0)
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing)
                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData)

                lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing) 'L7
                Dim len As Integer = lItemNum
                Dim ItemData2 As Object = New Object
                For i As Integer = 0 To len - 1
                    'CARRIERID SOURCE DEST NEXT CARRIERTYPE EMPTYFLAG PROCESSID

                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing)
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData) '
                    lOffset = comQSWrapper.DataItemIn(RawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData2) '
                    temp_carrier.SetValByName(ItemData, ItemData2)

                Next
                Dim flag As Boolean = False
                Dim ACKCodes As Byte
                Dim CST_flag As Integer = -1
                If Not temp_carrier.CarrierID.Length = 6 Then
                    ACKCodes = 68
                End If
                If temp_carrier.PRIORITY > 99 Then
                    ACKCodes = 74
                End If

                For i As Integer = 0 To comQGWrapper.CST.Length - 1
                    If comQGWrapper.CST(i).CarrierID = temp_carrier.CarrierID Then
                        CST_flag = i
                    End If
                    If comQGWrapper.CST(i).CommandID = temp_carrier.CommandID Then
                        'Cmd ID 重複
                        ' ACKCodes = 65
                    End If
                    If comQGWrapper.CST(i).CarrierID = temp_carrier.CarrierID And Not comQGWrapper.CST(i).CommandID = "" Then
                        'CST 重複
                        '  ACKCodes = 66
                    End If

                    'EQ 才算 棚位不算
                    If comQGWrapper.CST(i).SOURCE = temp_carrier.SOURCE And Not temp_carrier.SOURCE.StartsWith(comQGWrapper.Eqpname) Then
                        '  ACKCodes = 75 'EQ來源重複
                    End If
                Next

                flag = False
                For i As Integer = 0 To comQGWrapper.EqPort.Length - 1
                    '找尋目的或是來源是否有存在EQ
                    If temp_carrier.DEST = comQGWrapper.EqPort(i).PortID And comQGWrapper.EqPort(i).LoadAvail = 1 Then
                        flag = True
                    End If
                Next
                If flag Then
                    'ACKCodes = 69
                End If
                flag = False
                For i As Integer = 0 To comQGWrapper.EqPort.Length - 1
                    '找尋目的或是來源是否有存在EQ
                    If temp_carrier.SOURCE = comQGWrapper.EqPort(i).PortID And comQGWrapper.EqPort(i).UnLoadAvail = 1 Then
                        flag = True
                    End If
                Next
                If flag Then
                    'ACKCodes = 70
                End If

                If ACKCodes = 0 Then
                    temp_carrier.TransferState = GEM.TransferState_Queued
                End If




                Dim str As String = "COMMANDINFO"
                If ACKCodes = 0 Then
                    '沒有問題，可以開始複製Cmd ID 
                    Dim Query As String
                    If CST_flag = -1 Then
                        If Not temp_carrier.SOURCE.StartsWith(comQGWrapper.Eqpname) Then
                            CST_flag = comQGWrapper.CST_Add(temp_carrier.CarrierID, temp_carrier.SOURCE, temp_carrier.SOURCE)
                            Query = "insert ignore into `carrier` (STK_NAME,CARRIER_ID,LOT_ID,LOC_NAME,LOC_TYPE,SUB_LOC,CARRIER_STATUS,STORED_TIME,REQ_TIME,UPDATE_DATE,UPDATE_BY,CREATE_DATE,CREATE_BY) " + _
                         "VALUES ('" + comQGWrapper.Eqpname + "', '" + comQGWrapper.CST(CST_flag).CarrierID + "', '" + comQGWrapper.CST(CST_flag).CarrierLoc + "','" + comQGWrapper.CST(CST_flag).CarrierLoc + "',1,'" + comQGWrapper.CST(CST_flag).CarrierZoneName + "', '4', now(), '', now(), 'AGVC', now(), 'AGVC');"
                            Update_SQL(Query)
                        End If
                    End If

                    If CST_flag > -1 Then

                        comQGWrapper.CST(CST_flag).CommandID = temp_carrier.CommandID
                        comQGWrapper.CST(CST_flag).PRIORITY = temp_carrier.PRIORITY
                        comQGWrapper.CST(CST_flag).SOURCE = temp_carrier.SOURCE
                        comQGWrapper.CST(CST_flag).DEST = temp_carrier.DEST
                        comQGWrapper.CST(CST_flag).NEXTDest = temp_carrier.NEXTDest
                        comQGWrapper.CST(CST_flag).CarrierType = temp_carrier.CarrierType
                        comQGWrapper.CST(CST_flag).EmptyCarrier = temp_carrier.EmptyCarrier
                        comQGWrapper.CST(CST_flag).PROCESSID = temp_carrier.PROCESSID
                        comQGWrapper.CST(CST_flag).TransferState = GEM.TransferState_Queued
                        comQGWrapper.CST(CST_flag).note = "Waitting"
                        comQGWrapper.CST(CST_flag).EQ_Retry = 0
                        comQGWrapper.CST(CST_flag).mcstime = CLng(Now.ToString("yyyyMMddHHmmss"))
                        Query = "INSERT INTO `agv`.`mcs_cmd_history` ( `COMMANDID` ,`CARRIERID` ,`SOURCE` ,`DEST` ,`PROCESSID` ,`REQUEST_TIME`,PRIORITY )" + _
                            "VALUES ( '" + comQGWrapper.CST(CST_flag).CommandID + "', '" + comQGWrapper.CST(CST_flag).CarrierID + "', '" + comQGWrapper.CST(CST_flag).SOURCE + "', '" + comQGWrapper.CST(CST_flag).DEST + "', '" + comQGWrapper.CST(CST_flag).PROCESSID + "', CURRENT_TIMESTAMP()," + comQGWrapper.CST(CST_flag).PRIORITY.ToString + ");"
                        Update_SQL(Query)
                        Query = "INSERT ignore INTO `agv`.`mcs_cmd_list` ( `COMMANDID` ,`CARRIERID` ,`SOURCE` ,`DEST` ,`PROCESSID` ,`REQUEST_TIME`,PRIORITY )" + _
                        "VALUES ( '" + comQGWrapper.CST(CST_flag).CommandID + "', '" + comQGWrapper.CST(CST_flag).CarrierID + "', '" + comQGWrapper.CST(CST_flag).SOURCE + "', '" + comQGWrapper.CST(CST_flag).DEST + "', '" + comQGWrapper.CST(CST_flag).PROCESSID + "', CURRENT_TIMESTAMP()," + comQGWrapper.CST(CST_flag).PRIORITY.ToString + ");"
                        Update_SQL(Query)
                        Query = "update mcs_cmd_list set `COMMANDID`='" + comQGWrapper.CST(CST_flag).CommandID + "' ,SOURCE='" + comQGWrapper.CST(CST_flag).SOURCE + "',DEST='" + comQGWrapper.CST(CST_flag).DEST + "',PROCESSID='" + comQGWrapper.CST(CST_flag).PROCESSID + "',PRIORITY=" + comQGWrapper.CST(CST_flag).PRIORITY.ToString + " where CARRIERID='" + comQGWrapper.CST(CST_flag).CarrierID + "'"
                        Update_SQL(Query)
                        Query = "update `carrier` set `RROCESS_ID`='" + comQGWrapper.CST(CST_flag).PROCESSID + "' where CARRIER_ID='" + comQGWrapper.CST(CST_flag).CarrierID + "'"

                        Update_SQL(Query)
                      
                    End If





                Else
                    setCommtext("CST tranfer" + temp_carrier.CarrierID + "ACK=" + ACKCodes.ToString)
                End If

                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 2, SECSComm.LIST_TYPE, Nothing, DataItemOutLen)
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 2, SECSComm.LIST_TYPE, Nothing, DataItemOutLen)
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, str.Length, SECSComm.ASCII_TYPE, str, DataItemOutLen)
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 2, SECSComm.LIST_TYPE, Nothing, DataItemOutLen)
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 2, SECSComm.LIST_TYPE, Nothing, DataItemOutLen)
                str = "COMMANDID"
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, str.Length, SECSComm.ASCII_TYPE, str, DataItemOutLen)
                '0:          Normal()
                '1:          Undefined Parameter X
                '2 : Parameter is outside the specified values. X
                '3 : Parameter is outside the specified types. X
                '4:          Normal()
                '5~63 : Reserved 
                '64 : The career number is abnormal. V
                '65 : Command ID duplicate. V
                '66 : Carrier ID duplicate. X
                '67 : Command ID is abnormal. X
                '68 : Carrier ID is abnormal. V
                '69 : Load saving equip name undefinition. V
                '70 : Unloading equip name undefinition. 
                '71 : The equip name is abnormal. 
                '72 : The transportation table is full. 
                '73 : It is the unloading device name <it is> the same.become 
                'empty the load. 
                '74 : The priority is abnormal. V
                '75 : SOURCE duplicate.  V
                '76 : Additionally, it is abnormal. 
                '82 : Transportation is former abnormal. (group name specification) 
                '<Not Use> 
                '83 : The station does not exist in the group at the transportation 
                'destination. 
                '84 : The or load lower destination at the load saving destination is 
                'different. (for 2 batch) <Not Use> 
                '85 : Cassette type error. MCS should set the command to error. 
                '86 : Specified shelf is a block shelf. 
                ACKCodes = 0
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 1, SECSComm.BINARY_TYPE, ACKCodes, DataItemOutLen)
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 2, SECSComm.LIST_TYPE, Nothing, DataItemOutLen)
                str = "PRIORITY"
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, str.Length, SECSComm.ASCII_TYPE, str, DataItemOutLen)
                ACKCodes = 0
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 1, SECSComm.BINARY_TYPE, ACKCodes, DataItemOutLen)
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 1, SECSComm.LIST_TYPE, Nothing, DataItemOutLen)
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 2, SECSComm.LIST_TYPE, Nothing, DataItemOutLen)
                str = "TRANSFERINFO"
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, str.Length, SECSComm.ASCII_TYPE, str, DataItemOutLen)
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 3, SECSComm.LIST_TYPE, Nothing, DataItemOutLen)
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 2, SECSComm.LIST_TYPE, Nothing, DataItemOutLen)
                str = "CARRIERID"
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, str.Length, SECSComm.ASCII_TYPE, str, DataItemOutLen)
                ACKCodes = 0
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 1, SECSComm.BINARY_TYPE, ACKCodes, DataItemOutLen)
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 2, SECSComm.LIST_TYPE, Nothing, DataItemOutLen)
                str = "SOURCEPORT"
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, str.Length, SECSComm.ASCII_TYPE, str, DataItemOutLen)
                ACKCodes = 0
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 1, SECSComm.BINARY_TYPE, ACKCodes, DataItemOutLen)
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 2, SECSComm.LIST_TYPE, Nothing, DataItemOutLen)
                str = "DESTPORT"
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, str.Length, SECSComm.ASCII_TYPE, str, DataItemOutLen)
                ACKCodes = 0
                DataItemOutLen = comQSWrapper.DataItemOut(OutputRawData, 1, SECSComm.BINARY_TYPE, ACKCodes, DataItemOutLen)
                '  comQSWrapper.SendSECSIIMessage(S, F + 1, 0, ulSystemBytes, OutputRawData)
                'MsgBox("CST tranfer")
                setCommtext("CST tranfer" + temp_carrier.CarrierID + " from " + temp_carrier.SOURCE + " To " + temp_carrier.DEST.ToString)
                Install_CMD(temp_carrier)


                ElseIf S = 2 And F = 31 Then

                    setCommtext("Systemtime:" + comQGWrapper.gettime)
                End If
        End If

        If S = 2 And F = 49 Then
            '傳送命令在這邊發送
            setCommtext("S2F49 Start:" + ulSystemBytes.ToString)
            comQGWrapper_QGEvent(1, S, F + 1, 0, ulSystemBytes, OutputRawData, DataItemOutLen)
            setCommtext("S2F49 End:" + ulSystemBytes.ToString)
        ElseIf lMsgID = QS_EVENT_RECV_MSG Then
            comQGWrapper.ProcessMessage(lMsgID, S, F, W_Bit, ulSystemBytes, RawData, Head, pEventText)
        ElseIf lMsgID = QS_EVENT_CONNECTED Then
            'Ver:4.10 
            Dim CONTROL_STATE As UInt16 = 0
            comQGWrapper.GetSV(GEM_CONTROL_STATE, SECSComm.UINT_2_TYPE, CONTROL_STATE)
            ConnectionState = 1
            Pre_ControlState = CONTROL_STATE

            Dim SCSTATE As UInt16 = 0
            comQGWrapper.GetSV(GEM_SC_STATE, SECSComm.UINT_2_TYPE, SCSTATE)
            Pre_SCState = SCSTATE
            'Me.lbl_SECSConnectState.Text = "Connection"
            ' Me.lbl_SECSConnectState.BackColor = Color.GreenYellow
        ElseIf lMsgID = QS_EVENT_DISCONNECTED Then
            'Ver:4.10
            ' lbl_SECSConnectState.Text = "Disconnection(Stop)"
            'lbl_SECSConnectState.BackColor = Color.Red
            ConnectionState = 0
            setCommtext("MCS斷線")
        ElseIf lMsgID = 15 Then
            settext("Link test")
        ElseIf lMsgID = QS_EVENT_REPLY_TIMEOUT Then

        End If

    End Sub
    Private Sub comQGWrapper_QGEvent(ByVal lID As Integer, ByVal S As Integer, ByVal F As Integer, ByVal W_Bit As Integer, ByVal SystemBytes As UInteger, ByVal RawData As Object, ByVal Length As Integer) Handles comQGWrapper.QGEvent
        'ShowSECSIIMessage RawData



        If lID = 1 Then
            If Not RawData Is Nothing Then
                Try
                    ShowSECSIIMessage(RawData)
                Catch ex As Exception
                    setCommtext(SystemBytes.ToString + "送出失敗")
                    Exit Sub

                End Try



            End If

        ElseIf lID = 2 Then
            'transfer event

        End If

        comQSWrapper.SendSECSIIMessage(S, F, W_Bit, SystemBytes, RawData, Length)
        setCommtext(SystemBytes.ToString + "送出S" + S.ToString + "F" + F.ToString + " len:" + Length.ToString)
    End Sub
    Function Seach_Tagid(ByVal Loc As String)
        Dim point As New Tag_Point
        For i As Integer = 0 To Tag_point_list.Length - 1
            If Tag_point_list(i).LOC = Loc Then
                point = Tag_point_list(i)

            End If
        Next
        Return point
    End Function
    Public Sub ListenCmd()
        Dim myTCPlistenter As TcpListener
        Dim icount As Integer = 0
        Dim iport As Integer = CInt(LPort.Text) + 2000
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
                        ElseIf cmd_list(1).StartsWith("1000") Then
                            '異常
                            If Car(idx).device_status(18) = 0 Then
                                Car(idx).To_AGV(20) = 1000
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
        Dim stste As Car_point = New Car_point(100, 100)
        Dim status(50) As Byte
        Dim read_int(1) As Integer
        Dim flag As Boolean = False
        Dim idx As Integer = 0
        Dim this_thread_idx As String = ""
        Dim bms1flag As Boolean = False
        Dim bms1(15), bms2(15) As Integer
        read_int(0) = 0
        read_int(1) = 0
        stste.econ_Socket = socket_Client
        stste.econ_stream = New NetworkStream(socket_Client)
        stste.flag = False
        stste.econ_stream.ReadTimeout = 400
        stste.econ_stream.WriteTimeout = 400
        Thread.Sleep(2000)
        '搜尋離線的車
        For i = 0 To car_no - 1
            If Car(i).status = -2 And Car(i).device_no > 0 Then
                settext("讀取" + Car(i).device_no.ToString + "號車" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString(), True)
                If modbus_read(stste.econ_stream, Car(i).device_no, 200, 1, read_int) Then
                    stste.flag = True
                    stste.connected = True
                    stste.device_no = Car(i).device_no
                    stste.subcmd = Car(i).subcmd
                    stste.device_status = Car(i).device_status.Clone
                    stste.Car_type = Car(i).Car_type
                    stste.Recharge_Point_list = Car(i).Recharge_Point_list
                    stste.wait_point = Car(i).wait_point
                    stste.Block_Point = Car(i).Block_Point
                    stste.Block_Path = Car(i).Block_Path
                    stste.RollData = Car(i).RollData
                    stste.Recharge_volt = Car(i).Recharge_volt
                    stste.Recharge_SOC = Car(i).Recharge_SOC
                    stste.SafeSensor = Car(i).SafeSensor
                    stste.Site = Car(i).Site
                    stste.width = Car(i).width
                    stste.height = Car(i).height
                    stste.ReverseXY = Car(i).ReverseXY
                    stste.offset_X = Car(i).offset_X
                    stste.offset_Y = Car(i).offset_Y
                    stste.Lock_user = Car(i).Lock_user
                    For j As Integer = 0 To 49
                        stste.warning(j) = AutoReset(j)
                    Next
                    stste.MaxPath = Car(i).MaxPath

                    stste.RePath = Car(i).RePath
                    stste.RetreatPath = Car(i).RetreatPath
                    ' MsgBox(i)
                    stste.step_i = 999
                    modbus_read(stste.econ_stream, Car(i).device_no, 3053, 16, bms1)
                    modbus_read(stste.econ_stream, Car(i).device_no, 3153, 16, bms2)
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
                        ' stste.Car_Picbox = Car(i).Car_Picbox
                        Car(i) = stste
                    End If
                    Car(i).BMS_fw = stste.BMS_fw
                    Car(i).bat_SN(0) = int2bytestr(bms1, 0, 16)
                    Car(i).bat_SN(1) = int2bytestr(bms2, 0, 16)
                    Car(i).Read_Err_Count = 0
                    Car(i).thread_idx = Now.ToString("yyyyMMddHHmmssfff")
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

            For i = 0 To car_no - 1
                If Car(i).device_no > 0 Then

                    settext("讀取" + Car(i).device_no.ToString + "號車" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString(), True)
                    For k As Integer = 0 To 2


                        If modbus_read(stste.econ_stream, Car(i).device_no, 200, 1, read_int) Then
                            stste.flag = True
                            stste.connected = True
                            stste.device_no = Car(i).device_no
                            stste.subcmd = Car(i).subcmd
                            stste.device_status = Car(i).device_status.Clone
                            stste.Car_type = Car(i).Car_type
                            stste.Recharge_Point_list = Car(i).Recharge_Point_list
                            stste.wait_point = Car(i).wait_point
                            stste.Block_Point = Car(i).Block_Point
                            stste.Block_Path = Car(i).Block_Path
                            stste.RollData = Car(i).RollData
                            stste.Recharge_volt = Car(i).Recharge_volt
                            stste.Recharge_SOC = Car(i).Recharge_SOC
                            stste.SafeSensor = Car(i).SafeSensor
                            stste.Site = Car(i).Site
                            stste.width = Car(i).width
                            stste.height = Car(i).height
                            stste.ReverseXY = Car(i).ReverseXY
                            stste.offset_X = Car(i).offset_X
                            stste.offset_Y = Car(i).offset_Y
                            stste.Lock_user = Car(i).Lock_user
                            For j As Integer = 0 To 49
                                stste.warning(j) = AutoReset(j)
                            Next
                            stste.MaxPath = Car(i).MaxPath
                            stste.RePath = Car(i).RePath
                            stste.RetreatPath = Car(i).RetreatPath
                            ' MsgBox(i)

                            stste.step_i = 999
                            modbus_read(stste.econ_stream, Car(i).device_no, 3053, 16, bms1)
                            modbus_read(stste.econ_stream, Car(i).device_no, 3153, 16, bms2)
                            stste.BMS_fw = int2bytestr(bms1, 0, 16) + " " + int2bytestr(bms2, 0, 16)
                            settext(Car(i).device_no.ToString + "號車，電池版本" + stste.BMS_fw)
                            settext("AGV" + stste.device_no.ToString + " BMS1:" + int2str(bms1, 0, 16))
                            settext("AGV" + stste.device_no.ToString + " BMS2:" + int2str(bms2, 0, 16))
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
                                ' stste.Car_Picbox = Car(i).Car_Picbox
                                Car(i) = stste
                            End If
                            Car(i).Read_Err_Count = 0
                            Car(i).thread_idx = Now.ToString("yyyyMMddHHmmssfff")
                            Car(i).BMS_fw = stste.BMS_fw
                            Car(i).bat_SN(0) = int2bytestr(bms1, 0, 16)
                            Car(i).bat_SN(1) = int2bytestr(bms2, 0, 16)
                            this_thread_idx = Car(i).thread_idx
                            settext("thread_idx=" + this_thread_idx)
                            idx = i
                            flag = True
                            Exit For
                        Else
                            settext("讀取" + Car(i).device_no.ToString + "號車 未回應" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString(), True)
                        End If
                    Next
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
                    Query = "INSERT INTO `agv_event` (`Car_No` ,`Event` ,`Event_Time` ,`Tag_ID` ,`IP_Addr`,cmd_idx,Battery)VALUES ('" + Car(idx).device_no.ToString + "', 'ONLINE', now(), " + Car(idx).get_tagId.ToString + ", '" + Car(idx).ipadress.ToString + "', " + Car(idx).cmd_sql_idx.ToString + ",'" + Car(idx).BMS_fw.ToString + "');"
                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()


                    Query = "update agv_list set bat_SN1='" + Car(idx).bat_SN(0) + "',bat_SN2='" + Car(idx).bat_SN(1) + "' where AGVNo='" + Car(idx).device_no.ToString + "' ;"
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

                    Thread.Sleep(100)
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
            Try
                stste.econ_stream.Close()
                stste.econ_Socket.Close()
                settext("未找到設備 關閉連線:" + stste.device_no.ToString + ":" + CType(socket_Client.RemoteEndPoint, IPEndPoint).Address.ToString())
            Catch ex As Exception

            End Try
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
    ' Dim bms_cnt As Integer = 0
    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MainBG_timer.Tick


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
        For i = 0 To car_no - 1
            If Car(i).flag Then
                status_log(i)
            End If
        Next


        If Car(view_car_idx).flag = True Then



            For i = 0 To 49
                Dim TextBox As TextBox = Me.Controls.Find("Econ_" + i.ToString(), True)(0)
                If Not Car(view_car_idx).device_status Is Nothing Then
                    TextBox.Text = Car(view_car_idx).device_status(i).ToString
                End If
            Next
            For i = 1 To 4
                For j As Integer = 0 To 15
                    Dim lab As Label = Me.Controls.Find("AGVIO" + i.ToString + "_" + j.ToString.PadLeft(2, "0"), True)(0)
                    Dim val As Integer = (Car(view_car_idx).device_status(29 + i) >> j) Mod 2
                    If val = 1 Then
                        lab.BackColor = Color.LightGreen
                    Else
                        lab.BackColor = Color.Gray
                    End If


                Next
            Next

            CstID.Text = Car(view_car_idx).get_cstid

            For i = 0 To 20
                Dim TextBox As TextBox = Me.Controls.Find("txtToAGV" + i.ToString(), True)(0)
                If Not Car(view_car_idx).To_AGV Is Nothing Then
                    TextBox.Text = Car(view_car_idx).To_AGV(i).ToString
                End If
            Next
            If Car(view_car_idx).BMS1(7) > 100 And Car(view_car_idx).BMS1(7) < 7000 Then


                TextBox11.Text = "電池版本" + Car(view_car_idx).BMS_fw + vbCrLf

                For i = 0 To 52
                    TextBox11.Text += Car(view_car_idx).BMS1(i).ToString + " "
                    If i = 6 Or i = 17 Or i = 34 Or i = 42 Then
                        TextBox11.Text += vbCrLf
                    End If

                Next
                TextBox11.Text += vbCrLf
                TextBox11.Text += "電壓:" + Car(view_car_idx).BMS1(7).ToString.ToString
                If Car(view_car_idx).BMS1(8) > 32765 Then
                    TextBox11.Text += "放電:" + (65535 - Car(view_car_idx).BMS1(8)).ToString.ToString
                Else
                    TextBox11.Text += "充電:" + Car(view_car_idx).BMS1(8).ToString.ToString
                End If
                TextBox11.Text += "溫度:" + Car(view_car_idx).BMS1(10).ToString.ToString + "," + Car(view_car_idx).BMS1(11).ToString.ToString + "," + Car(view_car_idx).BMS1(12).ToString.ToString + vbCrLf
                TextBox11.Text += "心跳檢測:" + Car(view_car_idx).BMSAlarm1(17).ToString + vbCrLf
                For i = 0 To 52
                    TextBox11.Text += Car(view_car_idx).BMS2(i).ToString + " "
                    If i = 6 Or i = 17 Or i = 34 Or i = 42 Then
                        TextBox11.Text += vbCrLf
                    End If

                Next
                TextBox11.Text += vbCrLf
                TextBox11.Text += "電壓:" + Car(view_car_idx).BMS2(7).ToString.ToString
                If Car(view_car_idx).BMS2(8) > 32765 Then
                    TextBox11.Text += "放電:" + (65535 - Car(view_car_idx).BMS2(8)).ToString.ToString
                Else
                    TextBox11.Text += "充電:" + Car(view_car_idx).BMS2(8).ToString.ToString
                End If
                TextBox11.Text += "溫度:" + Car(view_car_idx).BMS2(10).ToString.ToString + "," + Car(view_car_idx).BMS2(11).ToString.ToString + "," + Car(view_car_idx).BMS2(12).ToString.ToString + vbCrLf
                TextBox11.Text += "心跳檢測:" + Car(view_car_idx).BMSAlarm2(17).ToString + vbCrLf
                'If Not Car(view_car_idx).device_status Is Nothing Then
                '    Econ_20.Text = Err_code(Car(view_car_idx).device_status(20))
                'End If
            End If
            If ChargerClient(view_charger_idx).BMS1(7) > 100 And ChargerClient(view_charger_idx).BMS1(7) < 7000 Then


                TextBox11.Text = "電池版本" + ChargerClient(view_charger_idx).SN1 + ChargerClient(view_charger_idx).SN2 + vbCrLf

                For i = 0 To 49
                    TextBox11.Text += ChargerClient(view_charger_idx).BMS1(i).ToString + " "
                    If i = 6 Or i = 17 Or i = 34 Or i = 42 Then
                        TextBox11.Text += vbCrLf
                    End If

                Next
                TextBox11.Text += vbCrLf
                TextBox11.Text += "電壓:" + ChargerClient(view_charger_idx).BMS1(7).ToString.ToString
                If ChargerClient(view_charger_idx).BMS1(8) > 32765 Then
                    TextBox11.Text += "放電:" + (65535 - ChargerClient(view_charger_idx).BMS1(8)).ToString.ToString
                Else
                    TextBox11.Text += "充電:" + ChargerClient(view_charger_idx).BMS1(8).ToString.ToString
                End If
                TextBox11.Text += "溫度:" + ChargerClient(view_charger_idx).BMS1(10).ToString.ToString + "," + ChargerClient(view_charger_idx).BMS1(11).ToString.ToString + "," + ChargerClient(view_charger_idx).BMS1(12).ToString.ToString + vbCrLf

                For i = 0 To 49
                    TextBox11.Text += ChargerClient(view_charger_idx).BMS2(i).ToString + " "
                    If i = 6 Or i = 17 Or i = 34 Or i = 42 Then
                        TextBox11.Text += vbCrLf
                    End If

                Next
                TextBox11.Text += vbCrLf
                TextBox11.Text += "電壓:" + ChargerClient(view_charger_idx).BMS2(7).ToString.ToString
                If Car(view_car_idx).BMS2(8) > 32765 Then
                    TextBox11.Text += "放電:" + (65535 - ChargerClient(view_charger_idx).BMS2(8)).ToString.ToString
                Else
                    TextBox11.Text += "充電:" + ChargerClient(view_charger_idx).BMS2(8).ToString.ToString
                End If
                TextBox11.Text += "溫度:" + ChargerClient(view_charger_idx).BMS2(10).ToString.ToString + "," + ChargerClient(view_charger_idx).BMS2(11).ToString.ToString + "," + ChargerClient(view_charger_idx).BMS2(12).ToString.ToString + vbCrLf

                'If Not Car(view_car_idx).device_status Is Nothing Then
                '    Econ_20.Text = Err_code(Car(view_car_idx).device_status(20))
                'End If
            End If
        End If

        'Me.AxActEasyIF1.State
        For j As Integer = 0 To car_no - 1
            If Not Car(j).get_tagId = 0 Then
                For i = 0 To Tag_point_list.Length - 1
                    If Car(j).get_tagId = Me.Tag_point_list(i).TagId Or Car(j).get_tagId - 10000 = Me.Tag_point_list(i).TagId Then
                        If Car(j).Car_type = "CRANE" Then
                        Else
                            ' Car(j).AXIS_X = Me.Tag_point_list(i).X - 15 - offset_x
                            ' Car(j).AXIS_Y = Me.Tag_point_list(i).Y - 15 - offset_y
                        End If
                    End If

                Next
                If Car(j).Car_type = "PIN" Then
                    If Car(j).get_Shelf_Car_No > 0 And Agvc_shelfcheck.Checked = True Then
                        For i = 0 To shelf_car.Length - 1
                            If shelf_car(i).Shelf_Car_No = Car(j).get_Shelf_Car_No Then
                                shelf_car(i).LOCATION = Car(j).get_tagId
                                shelf_car(i).car.Left = Car(j).AXIS_X
                                shelf_car(i).car.Top = Car(j).AXIS_Y + 15
                            End If
                        Next
                    End If
                ElseIf Car(j).Car_type = "ROLL" Then

                    If Car(j).Shelf_Car_No > 0 Then
                        For i = 0 To shelf_car.Length - 1
                            If shelf_car(i).Shelf_Car_No = Car(j).get_Shelf_Car_No Then
                                shelf_car(i).LOCATION = Car(j).get_tagId
                                shelf_car(i).car.Left = Car(j).AXIS_X
                                shelf_car(i).car.Top = Car(j).AXIS_Y + 15
                            End If
                        Next
                    End If
                End If

            End If
        Next
        For i = 0 To Door_List.Length - 1
            For j As Integer = 0 To Tag_point_list.Length - 1
                If Door_List(i).tagid = Me.Tag_point_list(j).TagId And Door_List(i).tagid > 0 Then
                    Door_List(i).Door_Pic.Left = Me.Tag_point_list(j).X - 15 - offset_x
                    Door_List(i).Door_Pic.Top = Me.Tag_point_list(j).Y - 15 - offset_y
                End If
            Next

        Next
        For i = 0 To LFT_List.Length - 1
            For j As Integer = 0 To Tag_point_list.Length - 1
                If LFT_List(i).tagid = Me.Tag_point_list(j).TagId And LFT_List(i).tagid > 0 Then
                    LFT_List(i).LFT_Pic.Left = Me.Tag_point_list(j).X - 15 - offset_x
                    LFT_List(i).LFT_Pic.Top = Me.Tag_point_list(j).Y - 15 - offset_y
                End If
            Next

        Next
        'OFFLINE 灰
        'OK 綠
        'ERROR 紅
        'RUN 黃
        For i = 0 To car_no - 1
            'Dim load_str As String = ""
            'If Car(i).get_loading = 3 And (Car(i).get_pin = 2 Or Car(i).get_pin = 6 Or Car(i).get_pin = 10 Or Car(i).Car_type = "LFT") Then
            '    load_str = "_load"
            'End If
            'If (Car(i).flag = False Or Car(i).status = -2) Then
            '    'Car(i).Car_Picbox.Image = Image.FromFile("gray" + load_str + ".png")
            '    Car(i).status = -2
            'ElseIf Car(i).status = 3 Then
            '    '手動
            '    Car(i).Car_Picbox.Image = Image.FromFile("gray" + load_str + ".png")
            '    Car(i).subcmd = Car(i).get_tagId.ToString

            'ElseIf (Car(i).status = 2 Or Car(i).status = 0) Then
            '    Car(i).Car_Picbox.Image = Image.FromFile("green" + load_str + ".png")
            'ElseIf (Car(i).status = 4 Or Car(i).status = 1) Then
            '    Car(i).Car_Picbox.Image = Image.FromFile("yellow" + load_str + ".png")
            'ElseIf (Car(i).status = -1) Then
            '    Car(i).Car_Picbox.Image = Image.FromFile("red" + load_str + ".png")
            'End If

            'Dim bmp1 = New Bitmap(Car(i).Car_Picbox.Width, Car(i).Car_Picbox.Height)   '產生圖像
            'bmp1 = Car(i).Car_Picbox.Image
            'Dim g1 = Graphics.FromImage(bmp1) '產生畫布
            'Dim mycolor1 As New SolidBrush(Color.FromArgb(255, 255, 255))   '定義字體顏色
            'g1.DrawString(Car(i).device_no.ToString, New Font("Microsoft JhengHei", 12, FontStyle.Regular), mycolor1, 5, 7) '畫布寫上字串
            'Car(i).Car_Picbox.BackgroundImage = bmp1    'PictureBox1.Image指定該圖像


            'If Car(i).Read_Err_Count > 1000 Then
            '    Car(i).flag = False
            'End If
            If Not Car(i).get_Err = Car(i).Pre_Error_Code Then
                Dim Pre_Error_Code = Car(i).Pre_Error_Code
                Car(i).Pre_Error_Code = Car(i).get_Err
                If Car(i).get_Err > 0 Then
                    Try
                        Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`) "
                        Query += "VALUES ('" + Car(i).cmd_sql_idx.ToString + "', now() ,'', '" + Car(i).get_Err.ToString + "', '" + Car(i).device_no.ToString + "', '', '', '" + Car(i).get_tagId.ToString + "', '', '" + Car(i).Shelf_Car_No.ToString + "') ;"
                        sqlCommand.CommandText = Query
                        sqlCommand.ExecuteNonQuery()
                        Load_Error()
                        settext(Query)
                        Dim idx As Integer = alarm.Query_idx(Car(i).get_Err)
                        'comQGWrapper.EventReportSendOb(GEM.EVENT_UnitAlarmSet, comQGWrapper.Eqpname + "C" + Car(i).device_no.ToString + "," + alarm.ALM_ID(idx).ToString + "," + alarm.ALM_ENG_TXT(idx))
                    Catch ex As Exception
                        settext(Query + ex.Message)
                    End Try
                Else
                    Try
                        Query = "update  `alarm` set  `CLEAR_DATE`=now() where  ALM_DEVICE='" + Car(i).device_no.ToString + "' and CLEAR_DATE < '2019-01-01' "
                        sqlCommand.CommandText = Query
                        sqlCommand.ExecuteNonQuery()
                        Load_Error()
                        settext(Query)
                        Dim idx As Integer = alarm.Query_idx(Pre_Error_Code)
                        '  comQGWrapper.EventReportSendOb(GEM.EVENT_UnitAlarmCleared, comQGWrapper.Eqpname + "C" + Car(i).device_no.ToString + "," + alarm.ALM_ID(idx).ToString + "," + alarm.ALM_ENG_TXT(idx))
                    Catch ex As Exception
                        settext(Query + ex.Message)
                    End Try
                End If

            End If
        Next


        If Car(view_car_idx).get_Err > 0 Then
            Me.Err_lb.Text = Car(view_car_idx).get_Err.ToString + vbCrLf + alarm.Query_ALM_TXT(Car(view_car_idx).get_Err)
        Else
            Me.Err_lb.Text = ""
        End If
        oConn.Close()
        oConn.Dispose()
        '
        'Dim a As Image = Image.FromFile("shelf.png")

    End Sub

    Private Sub Form1_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        If MessageBox.Show("確定關閉IMBS?", "警告", MessageBoxButtons.YesNo) = MsgBoxResult.No Then
            e.Cancel = True
        End If
    End Sub





    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        'Dim cf1 As Configuration = ConfigurationManager.OpenExeConfiguration(ConfigurationUserLevel.None
        'ReDim eqptest.DI(15)
        ' comQGWrapper = New GEM
        Dim xdoc As XmlDocument = New XmlDocument
        Dim a As StreamReader = New StreamReader("setting.ini")
        Dim str As String = a.ReadToEnd

        a.Close()

        Try
            xdoc.LoadXml(str)

            Me.Agvc_shelfcheck.Checked = CBool(xdoc.GetElementsByTagName("ShelfCar_Check").Item(0).InnerXml)
            Me.Loading_Check.Checked = CBool(xdoc.GetElementsByTagName("Loading_Check").Item(0).InnerXml)
            Me.ALL_Loading_check.Checked = CBool(xdoc.GetElementsByTagName("ALL_Loading_check").Item(0).InnerXml)
            Me.IR_check.Checked = CBool(xdoc.GetElementsByTagName("IR_sensor").Item(0).InnerXml)


            Me.MyDB_txt.Text = xdoc.GetElementsByTagName("MyDB").Item(0).InnerXml '
            Me.IP.Text = xdoc.GetElementsByTagName("IP").Item(0).InnerXml
            Me.LPort.Text = xdoc.GetElementsByTagName("LPort").Item(0).InnerXml
            MyDb = Me.MyDB_txt.Text
            IP_adress = Me.IP.Text
            user_txt.Text = xdoc.GetElementsByTagName("user").Item(0).InnerXml
            password_txt.Text = xdoc.GetElementsByTagName("password").Item(0).InnerXml
            user = user_txt.Text
            password = password_txt.Text


        

            DebugMode.Checked = CBool(xdoc.GetElementsByTagName("DebugMode").Item(0).InnerXml)

            DoorSetLenTxt.Text = CInt((xdoc.GetElementsByTagName("DoorSetLen").Item(0).InnerXml))
            DoorSetLen = CInt(DoorSetLenTxt.Text)

            AgvTimeout.Text = CInt((xdoc.GetElementsByTagName("AgvTimeout").Item(0).InnerXml)).ToString
            AgvTimeoutVal = CInt(AgvTimeout.Text)
            ratio.Text = xdoc.GetElementsByTagName("Ratio").Item(0).InnerXml
            AGVratio = CDbl(xdoc.GetElementsByTagName("Ratio").Item(0).InnerXml)

            McsPort = CInt(xdoc.GetElementsByTagName("McsPort").Item(0).InnerXml)
            McsPortTxt.Text = xdoc.GetElementsByTagName("McsPort").Item(0).InnerXml


            SOC = CInt(xdoc.GetElementsByTagName("SOC").Item(0).InnerXml)
            SOCTxt.Text = xdoc.GetElementsByTagName("SOC").Item(0).InnerXml

            Try
                map_offset_X = CInt(xdoc.GetElementsByTagName("MapX").Item(0).InnerXml)
                MapX.Text = xdoc.GetElementsByTagName("MapX").Item(0).InnerXml
                map_offset_Y = CInt(xdoc.GetElementsByTagName("MapY").Item(0).InnerXml)
                MapY.Text = xdoc.GetElementsByTagName("MapY").Item(0).InnerXml
            Catch ex As Exception
                map_offset_X = 0
                map_offset_Y = 0
            End Try

            For k As Integer = 200 To 249

                If xdoc.GetElementsByTagName("Err" + k.ToString).Count = 1 Then
                  
                        Dim chb As CheckBox = Me.Controls.Find("Err" + k.ToString(), True)(0)
                        chb.Checked = CBool(xdoc.GetElementsByTagName("Err" + k.ToString).Item(0).InnerXml)
                    If CBool(xdoc.GetElementsByTagName("Err" + k.ToString).Item(0).InnerXml) = False Then
                        AutoReset(k - 200) = 0


                    Else

                        AutoReset(k - 200) = k
                    End If
                Else
                    AutoReset(k - 200) = k
                End If


            Next
            BlockPoint.Text = xdoc.GetElementsByTagName("BlockPoint").Item(0).InnerXml
            AllBlockPoint = xdoc.GetElementsByTagName("BlockPoint").Item(0).InnerXml
            AllBlockPointList = AllBlockPoint.Split(",")
        Catch ex As Exception
            MsgBox("讀取設定有異常，請重新存檔並重啟" + ex.Message)
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
        Dim Query As String = "SELECT A.Tag_ID, X, Y, Retreat_Flag, Tag_name, floor, floor_no, site, IF( B.`Tag_ID` IS NULL , IF( C.`Tag_ID` IS NULL , 0, 3 ) , 1 ) , "
        Query += " IF( B.`Tag_ID` IS NULL , IF( C.`Tag_ID` IS NULL , '', C.ZONE_NAME ) , B.ZONE_NAME	 )  as ZONENAME, IF( B.`Tag_ID` IS NULL , IF( C.`Tag_ID` IS NULL , '', C.PORT_ID) , B.SHELF_LOC	 )  as LOC , IF( B.`Tag_ID` IS NULL , IF( C.`Tag_ID` IS NULL , '0', PORT_STN_NO ) , B.SHELF_STN_NO )   as stkval ,if (D.CARRIER_ID is null,'',D.CARRIER_ID ),th "
        Query += " FROM `point` A LEFT JOIN shelf B ON A.`Tag_ID` = B.`Tag_ID` LEFT JOIN port C ON A.`Tag_ID` = C.`Tag_ID` LEFT JOIN `carrier` D on A.Tag_name=D.SUB_LOC  WHERE  a.Tag_ID   BETWEEN 0 AND 19999 "
        Dim mReader As MySqlDataReader
        oConn.Open()
        sqlCommand.Connection = oConn
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        While (mReader.Read)
            Tag_ID_List(i) = CInt(mReader.Item(0))
            Tag_point_list(i).TagId = CInt(mReader.Item(0))
            Tag_point_list(i).X = CInt(mReader.Item(1))
            Tag_point_list(i).Y = CInt(mReader.Item(2))
            Tag_point_list(i).Retreat_Flag = CInt(mReader.Item(3))
            Tag_point_list(i).name = mReader.Item(4).ToString
            Tag_point_list(i).floor = mReader.Item(5).ToString
            Tag_point_list(i).floor_no = CInt(mReader.Item(6))
            Tag_point_list(i).site = mReader.Item(7)
            Tag_point_list(i).tagtype = CInt(mReader.Item(8))
            Tag_point_list(i).ZONE_NAME = mReader.Item(9)
            Tag_point_list(i).LOC = mReader.Item(10)
            Tag_point_list(i).stkval = CInt(mReader.Item(11))
            Tag_point_list(i).CarrierID = (mReader.Item(12))
            Tag_point_list(i).th = (mReader.Item(13))
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
        Load_Error()

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


        'For i = 0 To Tag_point_list.Length - 1
        '    Me.From_cb.Items.Add(Tag_point_list(i).TagId)
        '    Me.To_cb.Items.Add(Tag_point_list(i).TagId)
        'Next
        ' cmd 顯示命令
        ListView1_ReNew()
        ListView1.ContextMenuStrip = ContextMenuStrip1
        ListView2.ContextMenuStrip = ContextMenuStrip3
        'Me.Car1.ContextMenuStrip = ContextMenuStrip2
        Me.cmd_timer.Start()






        oConn.Close()
        oConn.Dispose()
        If DebugMode.Checked = False Then
            door_check_timer.Start()
            LFT_timer.Start()
            Load_Door_Data()
        End If

        VerLog.Text += "3.68 退避改成最近10個點" + vbCrLf
        VerLog.Text += "3.67 修正電梯異常，避免搶電梯，修改LOG紀錄方式" + vbCrLf
        VerLog.Text += "3.66 門口時，AGVC重啟異常" + vbCrLf
        VerLog.Text += "3.63 修正相鄰點位無法導航" + vbCrLf
        VerLog.Text += "3.62 加入AgvTimeout機制" + vbCrLf
        VerLog.Text += "3.61 加入重複路徑判斷" + vbCrLf
        VerLog.Text += "3.60 修正重啟AGVC無法清除異常的問題" + vbCrLf
        VerLog.Text += "3.59 修正退避邏輯 D演算法有問題，測試路徑的BUG修正" + vbCrLf


        ' Door_List(0).connect()

        Dim path As String




      
        path = System.Environment.CurrentDirectory '& "\.."
        comQSWrapper.Initialize()
        comQSWrapper.Start(McsPort)

        comQGWrapper.Initialize(path)

        Load_mcs_info()

        MCS.Start()
        Me.From_cb.Items.Add(0)
        Me.From_cb.Items.Add(1)
        Me.From_cb.Items.Add(2)
        Me.From_cb.Items.Add(3)
        For i = 0 To Tag_point_list.Length - 1
            Dim idx As Integer = 0

            If Not Tag_point_list(i).LOC = "" Then
                idx = comQGWrapper.CST_SearchByLoc(Tag_point_list(i).LOC)
                If idx > -1 Then
                    Me.From_cb.Items.Add(Tag_point_list(i).TagId)
                Else
                    Me.To_cb.Items.Add(Tag_point_list(i).TagId)
                End If
            End If



        Next
        agv_info.BackColor = Color.FromArgb(0, 147, 166)
        Update_SQL("update port set EQ_State ='OFFLINE' where 1")
        ' PictureBox1.BackColor = Color.FromArgb(92, 175, 149)
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
        Query = "SELECT AGVNo,Position,Loading,owner,Car_type,Recharge_Point,block_point,wait_point,Recharge_volt,SafeSensor,car_site,width,height,offset_x,offset_y,Recharge_soc,ReverseXY,MaxPath,RePath,RetreatPath,block_path,lock_user FROM `agv_list` where flag=1 order by AGVNo"
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        For i = 0 To car_no - 1
            ' Dim pic As PictureBox = Me.Controls.Find("Car" + i.ToString(), True)(0)
            'pic.ContextMenuStrip = ContextMenuStrip2
            Car(i) = New Car_point(300 + 25 * i, 430)
            Car(i).status = -2
            If (mReader.Read) Then
                ' MsgBox(mReader.Item(0).ToString)
                Car(i).flag = True
                Car(i).device_no = CInt(mReader.Item(0))
                Car(i).set_tagId(CInt(mReader.Item(1))) '初始值
                Car(i).set_loading(CInt(mReader.Item(2))) '初始值

                Car(i).Car_type = mReader.Item(4).ToString
                Car(i).Recharge_Point_list = mReader.Item(5).ToString
                Car(i).wait_point = CInt(mReader.Item(7))
                Car(i).Recharge_volt = CInt(mReader.Item(8))
                ' MsgBox(Car(i).device_no)
                txtCar.Items.Add(Car(i).device_no)
                AGV_SetNo.Items.Add(Car(i).device_no)
                txtCar.Text = txtCar.Items(0).ToString
                Car(i).Block_Point = mReader.Item(6).ToString
                Car(i).SafeSensor = CInt(mReader.Item(9))
                Car(i).Site = mReader.Item(10).ToString
                Car(i).width = CInt(mReader.Item(11))
                Car(i).height = CInt(mReader.Item(12))
                Car(i).offset_X = CInt(mReader.Item(13))
                Car(i).offset_Y = CInt(mReader.Item(14))
                Car(i).Recharge_SOC = CInt(mReader.Item(15))
                Car(i).ReverseXY = CInt(mReader.Item(16))
                Car(i).MaxPath = CInt(mReader.Item(17))
                Car(i).RePath = CInt(mReader.Item(18))
                Car(i).RetreatPath = CInt(mReader.Item(19))
                Car(i).Block_Path = mReader.Item(20)
                Car(i).Lock_user = mReader.Item(21)


                'Car(i).force_tagId(CInt(mReader.Item(1)))
            Else

                car_no = i

                Exit For

                'pic.Hide()

            End If
        Next

        mReader.Close()
        Array.Resize(Car, car_no)
        For i = 0 To car_no - 1
            Query = "  SELECT  `Status`,date_format(updatetime,'%Y-%m-%d %H:%i:%s')  FROM  `agv_status_history` WHERE  `AGVNo` =" + Car(i).device_no.ToString + " ORDER BY  `agv_status_history`.`updatetime` DESC limit 0,1 "
            sqlCommand.CommandText = Query
            mReader = sqlCommand.ExecuteReader
            If mReader.Read Then
                Car(i).agv_status = mReader.Item(0)
                Car(i).Pre_agv_status = mReader.Item(0)
                Car(i).agv_status_time = mReader.Item(1).ToString
            Else
                Car(i).agv_status = "OffLine"
                Car(i).Pre_agv_status = "OffLine"
                Car(i).agv_status_time = Now().ToString("yyyy-MM-dd HH:mm:ss")
            End If


            mReader.Close()
        Next
        Query = "SELECT  `Label_txt`, `X`, `Y` from `label` where 1"
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        While (mReader.Read)
            lablelist(i).Txt = mReader.Item(0).ToString
            lablelist(i).X = CInt(mReader.Item(1))
            lablelist(i).Y = CInt(mReader.Item(2))

            lablelist(i).isize = 40
            i += 1
        End While
        mReader.Close()
        Array.Resize(lablelist, i)

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

                Door_List(i).Door_Pic.Left = CInt(mReader.Item(7)) - offset_x
                Door_List(i).Door_Pic.Top = CInt(mReader.Item(8)) - offset_y
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
    Sub Load_mcs_info()
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim Query As String = ""
        Dim mReader As MySqlDataReader
        ' Dim path_i As Integer = 0
        Dim i As Integer


        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn

        alarm.init()
        Query = " SELECT ErrorCode,Description,ALM_RPT_ID,ALM_TXT FROM `error_code`"
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        alarm.ALM_ID(0) = 0
        alarm.ALM_TXT(0) = "未知異常"
        alarm.ALM_RPT_ID(0) = 3
        alarm.ALM_ENG_TXT(0) = "UNKNOWN"
        i = 1
        While mReader.Read
            alarm.ALM_ID(i) = CInt(mReader.Item(0))
            alarm.ALM_TXT(i) = mReader.Item(1).ToString
            alarm.ALM_RPT_ID(i) = CInt(mReader.Item(2))
            alarm.ALM_ENG_TXT(i) = mReader.Item(3).ToString
            i += 1
        End While
        Array.Resize(alarm.ALM_ID, i)
        Array.Resize(alarm.ALM_TXT, i)
        Array.Resize(alarm.ALM_RPT_ID, i)
        Array.Resize(alarm.ALM_ENG_TXT, i)


        mReader.Close()

  

        Query = " SELECT `ZONE_NAME` , COUNT( * ) FROM `shelf` WHERE 1 GROUP BY `ZONE_NAME`"
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        While mReader.Read
            comQGWrapper.Zone(i).ZoneName = mReader.Item(0).ToString
            comQGWrapper.Zone(i).ZoneSize = CInt(mReader.Item(1).ToString)
            comQGWrapper.Zone(i).DisabledLocations = ""
            ToLocList.Items.Add(comQGWrapper.Zone(i).ZoneName)
            i += 1
        End While
        Array.Resize(comQGWrapper.Zone, i)
        mReader.Close()
        Query = "SELECT `STK_NAME`, `CARRIER_ID`, `LOT_ID`, `CARRIER_TYPE`, `LOC_NAME`, `LOC_TYPE`,  `SUB_LOC`, `CARRIER_STATUS`, `RROCESS_ID`,"
        Query += " `STORED_TIME`, `REQ_TIME`, `UPDATE_DATE`, `UPDATE_BY`, `CREATE_DATE`, `CREATE_BY`, `BLOCK_FLAG`, `INV_FLAG` FROM `carrier` where not LOC_NAME='' "
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        While mReader.Read
            If CInt(mReader.Item(5)) = 1 Then
                'shelf   
                comQGWrapper.CST_Add(mReader.Item(1).ToString, mReader.Item(4).ToString, mReader.Item(6).ToString, mReader.Item(7))
            ElseIf CInt(mReader.Item(5)) = 3 Then
                'EQ
                comQGWrapper.CST_Add(mReader.Item(1).ToString, mReader.Item(4).ToString, mReader.Item(4).ToString, mReader.Item(7))
            End If
            CSTList.Items.Add(mReader.Item(1).ToString)

            i += 1
        End While

        mReader.Close()
        Query = " SELECT A.Tag_ID,STK_NAME,SHELF_LOC,CarrierID,A.AXIS_X+B.`X`,AXIS_Y+B.`Y`,ZONE_NAME,SHELF_STATUS,UPDATE_REASON,SHELF_STN_NO FROM `shelf` A left join point B on A.Tag_ID=B.Tag_ID WHERE not B.Tag_ID is null "
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        While mReader.Read
            Dim idx As Integer = -1
            comQGWrapper.ShelfData(i).tag_id = mReader.Item(0).ToString
            comQGWrapper.ShelfData(i).STK_Name = mReader.Item(1).ToString
            comQGWrapper.ShelfData(i).Shelf_Loc = mReader.Item(2).ToString

            idx = comQGWrapper.CST_SearchByLoc(comQGWrapper.ShelfData(i).Shelf_Loc)
            If idx > -1 Then
                comQGWrapper.ShelfData(i).CarrierID = comQGWrapper.CST(idx).CarrierID
            End If


            comQGWrapper.ShelfData(i).AXIS_X = CInt(mReader.Item(4))
            comQGWrapper.ShelfData(i).AXIS_Y = CInt(mReader.Item(5))
            comQGWrapper.ShelfData(i).Zone_Name = mReader.Item(6).ToString
            comQGWrapper.ShelfData(i).Shelf_Status = mReader.Item(7).ToString
            comQGWrapper.ShelfData(i).UPDATE_REASON = mReader.Item(8).ToString
            comQGWrapper.ShelfData(i).SHELF_STN_NO = mReader.Item(9).ToString

            ' ToLocList.Items.Add(comQGWrapper.ShelfData(i).Shelf_Loc)
            i += 1
        End While
        mReader.Close()
        Array.Resize(comQGWrapper.ShelfData, i)
    
        For j As Integer = 0 To comQGWrapper.Zone.Length - 1
            comQGWrapper.Zone(j).ZoneCapacity = comQGWrapper.Zone(j).ZoneSize - comQGWrapper.Zone(j).CST_count
            comQGWrapper.Zone(j).pre_ZoneCapacity = comQGWrapper.Zone(j).ZoneCapacity
        Next
        Query = " select A.`Tag_ID`, `STK_NAME`, `PORT_ID`, `ZONE_NAME`, `LOC_TYPE`, `PORT_TYPE`, `PORT_ORDER`, `TRANSFER_MODE`, `PORT_STATE`,"
        Query += " `STATE_DESC`, `PORT_PRIORITY`, `PORT_STN_NO`, `EQ_NAME`, A.`AXIS_X`+B.`X`, `AXIS_Y`+B.`Y`, `AXIS_Z`, `UPDATE_TIME`, `UPDATE_USER`,IPAdr,IPPort,StartDI,Adr  FROM `port` A left join point B on A.Tag_ID=B.Tag_ID  WHERE not  B.Tag_ID is null "

        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        While mReader.Read
            comQGWrapper.EqPort(i).tag_id = CInt(mReader.Item(0))
            comQGWrapper.EqPort(i).PortID = mReader.Item(2).ToString
            comQGWrapper.EqPort(i).PortTransferState = 0
            comQGWrapper.EqPort(i).LoadAvail = 1
            comQGWrapper.EqPort(i).UnLoadAvail = 1
            comQGWrapper.EqPort(i).ip = mReader.Item(18).ToString
            comQGWrapper.EqPort(i).port = CInt(mReader.Item(19))
            comQGWrapper.EqPort(i).AXIS_X = CInt(mReader.Item(13))
            comQGWrapper.EqPort(i).AXIS_Y = CInt(mReader.Item(14))
            comQGWrapper.EqPort(i).adr = CInt(mReader.Item(21))
            ToLocList.Items.Add(comQGWrapper.EqPort(i).PortID)
            i += 1

        End While
        Array.Resize(comQGWrapper.EqPort, i)

        mReader.Close()



        Query = " select IPAdr,IPPort,StartDI  FROM `port` WHERE 1 group by IPAdr,IPPort,StartDI"

        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        While mReader.Read
            eqp_client(i).init(mReader.Item(0).ToString, CInt(mReader.Item(1)), CInt(mReader.Item(2)))

            i += 1
        End While
        Array.Resize(eqp_client, i)
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
        Query = "SELECT COMMANDID,CARRIERID,SOURCE,DEST,PROCESSID,date_format(`REQUEST_TIME`,'%Y%m%d%H%i%s') FROM `mcs_cmd_list` "


        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        While mReader.Read
            Dim cstid As String = mReader.Item(1).ToString
            For j As Integer = 0 To comQGWrapper.CST.Length - 1
                If comQGWrapper.CST(j).CarrierID = cstid Then
                    comQGWrapper.CST(j).CommandID = mReader.Item(0).ToString
                    comQGWrapper.CST(j).DEST = mReader.Item(3).ToString
                    comQGWrapper.CST(j).PROCESSID = mReader.Item(4).ToString
                    comQGWrapper.CST(j).TransferState = 1
                    comQGWrapper.CST(j).PRIORITY = 50
                    comQGWrapper.CST(j).mcstime = CLng(mReader.Item(5).ToString)

                    Exit For
                End If
            Next
        End While



        mReader.Close()

        oConn.Close()
        oConn.Dispose()

    End Sub
    Sub Load_Error()
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim Query As String = ""
        Dim mReader As MySqlDataReader

        oConn = New MySqlConnection(Mysql_str)
    
        Try
            oConn.Open()
            sqlCommand.Connection = oConn
            Query = "SELECT `cmd_idx`,`ALM_DEVICE`,`HAPPEN_DATE`,`CLEAR_DATE`,`ALM_ID`,B.`Description`,`SUB_LOC`,`CST_ID`,C.CmdFrom,C.CmdTo FROM " + _
            " `alarm` A left join error_code B on A.`ALM_ID`=B.ErrorCode left join `agv_cmd_history` C on A.cmd_idx=C.CmdKey WHERE 1 ORDER BY `A`.`HAPPEN_DATE` DESC limit 0,30 "

            sqlCommand.CommandText = Query
            mReader = sqlCommand.ExecuteReader()
            alarm_list.Items.Clear()
            While mReader.Read
                Dim item As New ListViewItem()
                item.Text = mReader.Item(0)
                For i As Integer = 1 To 9
                    item.SubItems.Add(mReader.Item(i).ToString)
                Next
                alarm_list.Items.Add(item)
            End While
            mReader.Close()

        Catch ex As Exception
            settext("Load_Error")
        End Try


        Try
            oConn.Close()
            oConn.Dispose()
        Catch ex As Exception

        End Try


    End Sub
    Dim cmd_timer_isbusy As Boolean = False
    Private Sub cmd_timer_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmd_timer.Tick
        '更新資料庫()
        If cmd_timer_isbusy = False Then
            cmd_timer_isbusy = True
    
        Dim i As Integer = 0
        Dim Query As String = ""
        'Dim timestart As Long = Now.Ticks
        Dim temptagid(Tag_ID_List.Length - 1) As Integer
        For i = 0 To Tag_ID_List.Length - 1
            temptagid(i) = Tag_ID_List(i)
        Next
        Me.car_info.Text = "select_idx:" + Me.view_car_idx.ToString
        Me.car_info.Text += " " + Car(view_car_idx).get_info


        WorkList.Text = Car(view_car_idx).subcmd

        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand


        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
            Car(1).BMS1(16) += testval
        For i = 0 To car_no - 1
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
                For j As Integer = 0 To 20
                    Car(i).To_AGV(j) = 0
                Next
            End If
             If Car(i).device_status(23) = 0 And Car(i).device_status(24) = 0 Then
                For j As Integer = 0 To Tag_point_list.Length - 1
                    If Car(i).get_tagId = Tag_point_list(j).TagId Then
                        Car(i).AXIS_X = Tag_point_list(j).X
                        Car(i).AXIS_Y = Tag_point_list(j).Y
                        Car(i).AXIS_Z = Tag_point_list(j).th
                    End If
                Next

            Else

                Dim offsetTh As Integer = 0
                'ReverseXY 
                'bit0(反轉XY)
                'bit1 反轉X
                'bit2 反轉Y
                '
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
                ElseIf (Car(i).ReverseXY = 2) Then
                    '用真實數字
                    Car(i).AXIS_X = (Car(i).device_status(23) + Car(i).offset_X)
                    Car(i).AXIS_Y = Car(i).device_status(24) + Car(i).offset_Y
                ElseIf (Car(i).ReverseXY = 3) Then
                    '反向X ，Y真實
                    If Car(i).offset_X >= 0 Then
                        Car(i).AXIS_X = (Car(i).device_status(23) + Car(i).offset_X)
                    Else
                        Car(i).AXIS_X = -Car(i).offset_X - Car(i).device_status(23)
                    End If
                    Car(i).AXIS_Y = Car(i).device_status(24) + Car(i).offset_Y
                Else
                    'XY反轉
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
                        offsetTh = 90
                End If

                If Car(i).device_status(25) > 65535 / 2 Then
                        Car(i).AXIS_Z = (Car(i).device_status(25) - 65535) / 100 + offsetTh
                Else
                        Car(i).AXIS_Z = (Car(i).device_status(25) / 100 + offsetTh)
                End If
            End If
            ' End If
            If Car(i).get_SOC > 0 And Car(i).get_SOC < (Car(i).Recharge_SOC - 8) And Car(i).Lock_user = "" And DateDiff("s", Car(i).Pre_TagID_time, Now) > 60 Then
                Car(i).To_AGV(20) = 105
            End If

            If Car(i).get_auto = 0 And Car(i).get_status = 0 And Car(i).Lock_user = "" And DateDiff("s", Car(i).Pre_TagID_time, Now) > 180 Then
                If In_String(Car(i).Recharge_Point_list, Car(i).get_tagId.ToString) And (BmsWarmIdx And Car(i).BMS1(16)) Then
                    Car(i).To_AGV(20) = 30000 + InttoBitidx(Car(i).BMS1(16))
                End If
                If In_String(Car(i).Recharge_Point_list, Car(i).get_tagId.ToString) And (BmsWarmIdx And Car(i).BMS2(16)) Then
                    Car(i).To_AGV(20) = 30100 + InttoBitidx(Car(i).BMS2(16))
                End If
            End If

        Next

        '---------------排序
        Dim temp0 As String = ""
        Dim temp1 As String = ""
        Dim car_idx_list(car_no, 1) As String
        For car_idx As Integer = 0 To car_no - 1
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
        For car_idx_i As Integer = 0 To car_no - 1

            Dim car_idx As Integer = 0
            ' car_idx = CInt(car_idx_list(car_idx_i, 0)) '排序
            car_idx = car_no - 1 - car_idx_i

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

                If Car(car_idx).get_auto = 0 And Not Car(car_idx).get_status = 4 And Not Car(car_idx).get_status = 8 And Not Car(car_idx).get_status = 12 And Car(car_idx).get_Err = 0 And Car(car_idx).step_i >= 999 Then
                    '命令執行 step_i > 999
                
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
                        Car(car_idx).CommandID = ""
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


                        If (((CInt(Car(car_idx).get_Volt) < Car(car_idx).Recharge_volt Or CInt(Car(car_idx).get_SOC) <= Car(car_idx).Recharge_SOC) And CInt(Car(car_idx).get_Volt) > 20) Or (CInt(Car(car_idx).get_Volt) = 0 And Car(car_idx).device_status(20) = 8) Or Car(car_idx).device_status(19) = 95 Or Car(car_idx).device_status(19) = 96) And Car(car_idx).Lock_user = "" And Not Car(car_idx).get_pin = 10 And Car(car_idx).get_tagId > 0 And Car(car_idx).get_loading = 0 And Not Car(car_idx).Recharge_Point_list = "" Then
                            'If (((CInt(Car(car_idx).get_Volt) < Car(car_idx).Recharge_volt Or CInt(Car(car_idx).get_SOC) <= Car(car_idx).Recharge_SOC)) Or (CInt(Car(car_idx).get_Volt) = 0 And Car(car_idx).device_status(20) = 8) Or Car(car_idx).device_status(19) = 95 Or Car(car_idx).device_status(19) = 96) And Car(car_idx).Lock_user = "" And Not Car(car_idx).get_pin = 10 And Car(car_idx).get_tagId > 0 And Car(car_idx).get_loading = 0 And Not Car(car_idx).Recharge_Point_list = "" Then

                            '強制充電
                            ' 
                            settext("Car" + Car(car_idx).device_no.ToString + "強制充電1")
                            If Not (In_String(Car(car_idx).Recharge_Point_list, Car(car_idx).get_tagId.ToString)) Or (Car(car_idx).Car_type = "CRANE" And Car(car_idx).device_status(6) = 0) Then
                                settext("Car" + Car(car_idx).device_no.ToString + "強制充電2")

                                select_idx = -1
                                For i = 0 To Me.ListView1.Items.Count - 1
                                    If In_String(Car(car_idx).Recharge_Point_list, Me.ListView1.Items(i).SubItems(3).Text) And CInt(Me.ListView1.Items(i).SubItems(4).Text) > 0 And CInt(Me.ListView1.Items(i).SubItems(1).Text) = Car(car_idx).device_no Then
                                        select_idx = i
                                        Exit For
                                    End If
                                Next
                                If select_idx = -1 Then
                                    Dim chargerlist() As String = Car(car_idx).Recharge_Point_list.Split(",")
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
                                            If Send_CMD(Car(car_idx).device_no, 4, chargerlist(change_i)) Then
                                                Exit For
                                            End If
                                        End If


                                    Next

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
                                    Car(car_idx).CommandID = Me.ListView1.Items(select_idx).SubItems(13).Text

                                    comQGWrapper.EventReportSendOb(GEM.EVENT_CraneActive, Car(car_idx).CommandID + "," + comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString)

                                    Car(car_idx).Sql2Cmdlist()

                                    '選擇命令
                                    If Car(car_idx).cmd_idx = 0 Then
                                        '紀錄開始執行時間
                                        Query = ""
                                        Try
                                            Query = "INSERT INTO `agv_cmd_history` (`CmdKey`, `AGVNo`,`CmdFrom`, `CmdTo`, `RequestTime`, `Requestor`, `Start_Time`,Shelf_Car_No,Shelf_Car_type,Shelf_Car_Size,start_distance,ext_cmd,McsCmdKey,RollData,cmdstart) "
                                            Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "','" + Car(car_idx).device_no.ToString + "','" + Car(car_idx).Cmd_From.ToString + "', '" + Car(car_idx).Cmd_To.ToString + "', '" + Car(car_idx).RequestTime + "', '" + Car(car_idx).Requestor + "',now()," + Car(car_idx).cmd_Shelf_Car_No.ToString + _
                                                ",'" + Car(car_idx).cmd_Shelf_Car_Type + "','" + Car(car_idx).cmd_Shelf_Car_size + "'," + Car(car_idx).get_distance.ToString + ",'" + _
                                                Car(car_idx).ext_cmd + "','" + Car(car_idx).CommandID + "','" + Car(car_idx).Cmd_RollData + "'," + Car(car_idx).get_tagId.ToString + ");"
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
                                If Not Car(car_idx).Car_type = "CRANE" Then


                                    Car(car_idx).To_AGV(20) = 105 '電壓低下
                                End If


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
                            Car(car_idx).CommandID = Me.ListView1.Items(select_idx).SubItems(13).Text
                            comQGWrapper.EventReportSendOb(GEM.EVENT_CraneActive, Car(car_idx).CommandID + "," + comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString)
                            Car(car_idx).Sql2Cmdlist()
                            '選擇命令
                            If Car(car_idx).cmd_idx = 0 Then
                                '紀錄開始執行時間
                                Query = ""
                                Try
                                    Query = "INSERT INTO `agv_cmd_history` (`CmdKey`, `AGVNo`,`CmdFrom`, `CmdTo`, `RequestTime`, `Requestor`, `Start_Time`,Shelf_Car_No,Shelf_Car_type,Shelf_Car_Size,start_distance,ext_cmd,RollData,McsCmdKey,cmdstart) "
                                    Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "','" + Car(car_idx).device_no.ToString + "','" + Car(car_idx).Cmd_From.ToString + "', '" + Car(car_idx).Cmd_To.ToString + "', '" + Car(car_idx).RequestTime + "', '" + Car(car_idx).Requestor + "',now()," + Car(car_idx).cmd_Shelf_Car_No.ToString + ",'" + Car(car_idx).cmd_Shelf_Car_Type + _
                                        "','" + Car(car_idx).cmd_Shelf_Car_size + "'," + Car(car_idx).get_distance.ToString + ",'" + Car(car_idx).ext_cmd + "','" + Car(car_idx).Cmd_RollData + "','" + Car(car_idx).CommandID + "'," + Car(car_idx).get_tagId.ToString + ");"
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
                            settext(Car(car_idx).device_no.ToString + ":ChSQL:" + check_idx.ToString + ":" + Car(car_idx).cmd_sql_idx.ToString + ":" + Car(car_idx).cmd_list(Car(car_idx).cmd_idx).ToString)
                        ' If check_idx Or (My.Settings.chrage_flag And Car(car_idx).Cmd_To = Car(car_idx).Chrage_Point) Then
                        '-----------------
                        If (DateDiff("s", Car(car_idx).Pre_TagID_time, Now) > AgvTimeoutVal) Then
                            Car(car_idx).To_AGV(20) = 115 '對峙TimeOut

                        End If
                        If check_idx Then
                            Select Case Car(car_idx).cmd_list(Car(car_idx).cmd_idx)
                                Case "TagID->0"
                                    Car(car_idx).subcmd = "1"
                                    Car(car_idx).force_tagId(1)

                                    Car(car_idx).cmd_idx += 1
                                Case "STOP"
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
                                    ' Car(car_idx).starttime = Now()
                                    If Car(car_idx).Car_type = "PIN" Or Car(car_idx).Car_type = "POWER" Then
                                        If Car(car_idx).get_pin = 5 Then
                                            Car(car_idx).To_AGV(6) = 0
                                            Car(car_idx).cmd_idx += 1
                                        ElseIf Car(car_idx).get_loading = 3 And Car(car_idx).get_pin = 10 And (Car(car_idx).get_Shelf_Car_No = Car(car_idx).cmd_Shelf_Car_No Or Car(car_idx).get_Shelf_Car_No = 0) Then
                                            '載物與物品一致
                                            Car(car_idx).cmd_idx = 6
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
                                    ElseIf Car(car_idx).Car_type = "CRANE" Then
                                        Car(car_idx).State = "ACTIVE"
                                        If Car(car_idx).get_loading = 3 Then
                                            If Car(car_idx).get_cstid = Car(car_idx).Cmd_RollData Or Car(car_idx).Cmd_RollData = "" Then
                                                Car(car_idx).cmd_idx = 4
                                            Else
                                                Car(car_idx).To_AGV(20) = 104 '有料無帳
                                                '找最近的 放置

                                            End If
                                        Else
                                            '確認FROM TO的EQ 狀態
                                            '-------------以下模擬程式 
                                            Dim cst(7) As Byte
                                            cst = System.Text.Encoding.UTF8.GetBytes(Car(car_idx).Cmd_RollData)
                                            Array.Resize(cst, 8)
                                            Car(car_idx).To_AGV(9) = cst(1) * 256 + cst(0)
                                            Car(car_idx).To_AGV(10) = cst(3) * 256 + cst(2)
                                            Car(car_idx).To_AGV(11) = cst(5) * 256 + cst(4)
                                            Car(car_idx).To_AGV(12) = cst(7) * 256 + cst(6)
                                            '---通知模擬程式 命令的CST ID
                                            Dim From_idx As Integer = Tag_Point_ByTagid(Tag_point_list, Car(car_idx).Cmd_From)
                                            Dim To_idx As Integer = Tag_Point_ByTagid(Tag_point_list, Car(car_idx).Cmd_To)
                                            'Dim To_idx As Integer = Tag_Point_ByTagid(Tag_point_list, Car(car_idx).Cmd_From)
                                            Try
                                                From_idx = comQGWrapper.CST_SearchByLoc(Tag_point_list(From_idx).LOC)

                                                To_idx = comQGWrapper.CST_SearchByLoc(Tag_point_list(To_idx).LOC)
                                            Catch ex As Exception
                                                From_idx = -1
                                                To_idx = -1
                                            End Try

                                            For j As Integer = 0 To comQGWrapper.EqPort.Length - 1
                                                If comQGWrapper.EqPort(j).UnLoadAvail = 1 And comQGWrapper.EqPort(j).tag_id = Car(car_idx).Cmd_From Then
                                                    'EQ 沒有unload req
                                                    settext(comQGWrapper.EqPort(j).PortID + "EQ do not unload req")
                                                    Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "FINSH"
                                                End If
                                                If comQGWrapper.EqPort(j).LoadAvail = 1 And comQGWrapper.EqPort(j).tag_id = Car(car_idx).Cmd_To Then
                                                    'EQ 沒有load req
                                                    settext(comQGWrapper.EqPort(j).PortID + " do not load req")
                                                    Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "FINSH"
                                                End If
                                            Next

                                            If From_idx = -1 Then
                                                '起始沒有CST 也可以下命令
                                                settext(Car(car_idx).Cmd_From.ToString + " No CST")
                                                ' Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "FINSH"
                                            ElseIf To_idx > -1 And Not Car(car_idx).Cmd_From = Car(car_idx).Cmd_To Then
                                                settext(Tag_point_list(To_idx).LOC + " had CST")
                                                'Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 1) = "FINSH"

                                            End If

                                            Car(car_idx).cmd_idx += 1



                                        End If
                                        '搜尋空棚位

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
                                    Query = "UPDATE `agv_list` SET `lock_user` = '" + Car(car_idx).Lock_user + "' WHERE `AGVNo` = '" + Car(car_idx).device_no.ToString + "';"
                                    sqlCommand.CommandText = Query
                                    sqlCommand.ExecuteNonQuery()
                                    Car(car_idx).cmd_idx += 1
                                Case "UNLOCK"
                                    Car(car_idx).Lock_user = ""
                                    Query = "UPDATE `agv_list` SET `lock_user` = '" + Car(car_idx).Lock_user + "' WHERE `AGVNo` = '" + Car(car_idx).device_no.ToString + "';"
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
                                    If Car(car_idx).get_interlock >= 8 And Car(car_idx).get_action = &H20 And Car(car_idx).get_Err = 0 Then
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
                                            Load_Error()

                                            ' comQGWrapper.EventReportSendOb(GEM.EVENT_UnitAlarmSet, "")
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
                                    ElseIf Car(car_idx).get_interlock = 2 And Car(car_idx).get_action = &H20 And Car(car_idx).get_Err = 0 Then
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


                                    If Car(car_idx).get_interlock >= 8 And (Car(car_idx).get_action = &H10 Or Car(car_idx).get_action = &H20) And Car(car_idx).get_Err = 0 Then
                                        '異常結束
                                        settext(Car(car_idx).device_no.ToString + ":異常結束")
                                        Car(car_idx).To_AGV(6) = 0
                                        Car(car_idx).cmd_idx += 1
                                        Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`) "
                                        Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "',now(),now(), '201', '" + Car(car_idx).device_no.ToString + "', '', '', '" + Car(car_idx).get_tagId.ToString + "', '', '" + Car(car_idx).Shelf_Car_No.ToString + "') ;"
                                        sqlCommand.CommandText = Query
                                        sqlCommand.ExecuteNonQuery()
                                        Load_Error()
                                    ElseIf Car(car_idx).get_interlock = 2 And Car(car_idx).get_action = &H10 And Car(car_idx).get_Err = 0 Then
                                        ' 只有ROLLOUT
                                        settext(Car(car_idx).device_no.ToString + ":只有ROLLOUT")
                                        Car(car_idx).To_AGV(6) = 0
                                        Car(car_idx).cmd_idx += 1
                                        Car(car_idx).RollData = "" '車子除帳
                                        Query = "UPDATE `agv_list` SET `RollData` = '" + Car(car_idx).RollData + "' WHERE `AGVNo` = '" + Car(car_idx).device_no.ToString + "';"
                                        sqlCommand.CommandText = Query
                                        sqlCommand.ExecuteNonQuery()
                                    ElseIf (Car(car_idx).get_interlock = 6 Or Car(car_idx).get_interlock = 2) And Car(car_idx).get_action = &H20 And Car(car_idx).get_Err = 0 Then
                                        settext("Exchange模式結束")
                                        Car(car_idx).To_AGV(6) = 0
                                        Car(car_idx).cmd_idx += 1
                                        Car(car_idx).RollData = "" '車子除帳
                                        Query = "UPDATE `agv_list` SET `RollData` = '" + Car(car_idx).RollData + "' WHERE `AGVNo` = '" + Car(car_idx).device_no.ToString + "';"
                                        sqlCommand.CommandText = Query
                                        sqlCommand.ExecuteNonQuery()
                                    ElseIf Car(car_idx).get_interlock = 6 And Car(car_idx).get_action = &H10 And Car(car_idx).get_Err = 0 Then
                                        settext("Exchange:收箱")
                                        Car(car_idx).To_AGV(6) = &H20
                                    End If
                                Case "ACTION"
                                    Dim ext_cmd_list() As String = Car(car_idx).ext_cmd.Split(",")
                                    If ext_cmd_list.Length = 3 Then
                                            If IsNumeric(ext_cmd_list(0)) And IsNumeric(ext_cmd_list(1)) Then
                                                If Car(car_idx).get_action() = CInt(ext_cmd_list(0)) Or Car(car_idx).get_action = 0 Then
                                                    Car(car_idx).To_AGV(6) = CInt(ext_cmd_list(0))
                                                    Car(car_idx).To_AGV(7) = CInt(ext_cmd_list(1))
                                                End If

                                                If Car(car_idx).get_interlock = 2 And Car(car_idx).get_action = Car(car_idx).To_AGV(6) And Car(car_idx).get_Err = 0 Then
                                                    Car(car_idx).To_AGV(6) = 0
                                                    Car(car_idx).To_AGV(7) = 0
                                                    Car(car_idx).To_AGV(8) = 0
                                                    Car(car_idx).cmd_idx += 1
                                                End If
                                            End If
                                        End If

                                    Case "FORKIN"
                                        Dim ForkType As String = ""
                                        Dim tagid As Tag_Point = New Tag_Point
                                        settext(Car(car_idx).device_no.ToString + ":FORKIN")
                                        Dim span As TimeSpan = Now.Subtract(Car(car_idx).starttime)

                                        Car(car_idx).T2 = span.TotalSeconds - Car(car_idx).T1

                                        For j As Integer = 0 To Tag_point_list.Length - 1
                                            If Tag_point_list(j).TagId = Car(car_idx).get_tagId() Then
                                                If Tag_point_list(j).tagtype = 1 Then
                                                    Car(car_idx).To_AGV(6) = &H20
                                                    Car(car_idx).To_AGV(7) = Tag_point_list(j).stkval
                                                    tagid = Tag_point_list(j)
                                                    ForkType = "SHELF"
                                                    Exit For
                                                ElseIf Tag_point_list(j).tagtype = 3 Then
                                                    Car(car_idx).To_AGV(6) = &H21
                                                    ForkType = "EQ"
                                                    tagid = Tag_point_list(j)

                                                    Car(car_idx).To_AGV(7) = Tag_point_list(j).stkval
                                                    Exit For
                                                End If
                                            End If
                                        Next
                                        If ForkType = "" Then

                                            Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`,BarCodeERR,IL300R,IL300L) "
                                            Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "',now(),now(), '98', '" + Car(car_idx).device_no.ToString + "', '', '', '" + Car(car_idx).get_tagId.ToString + "', '', '" + Car(car_idx).get_cstid.ToString + "'" + _
                                                "," + Car(car_idx).barcodeError1.ToString + "," + Car(car_idx).IL300L1.ToString + "," + Car(car_idx).IL300R1.ToString + ") ;"
                                            sqlCommand.CommandText = Query
                                            sqlCommand.ExecuteNonQuery()
                                            settext(Car(car_idx).device_no.ToString + ":FORKIN地點異常" + Car(car_idx).get_tagId().ToString)
                                            Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "FINSH"
                                        ElseIf Car(car_idx).get_pin = 0 And Car(car_idx).get_action = 0 And Car(car_idx).get_loading = 3 Then
                                            settext(Car(car_idx).device_no.ToString + ":載荷異常" + Car(car_idx).get_tagId().ToString)
                                            Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "FINSH"

                                        ElseIf Car(car_idx).get_pin >= 8 And (Car(car_idx).get_action = &H20 Or Car(car_idx).get_action = &H21) And Car(car_idx).get_Err = 0 Then
                                            '異常結束
                                            settext(Car(car_idx).device_no.ToString + ":異常結束 2DCode" + Car(car_idx).Get2Derror.ToString + "IL:" + Car(car_idx).GetILerror.ToString)
                                            '目前自動復歸


                                            ' Car(car_idx).cmd_idx += 1
                                            Car(car_idx).barcodeError0 = Car(car_idx).Get2Derror
                                            Car(car_idx).IL300L0 = int2Toint32(Car(car_idx).device_status(36), Car(car_idx).device_status(37))
                                            Car(car_idx).IL300R0 = int2Toint32(Car(car_idx).device_status(38), Car(car_idx).device_status(39))
                                            Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "FINSH"
                                            Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`,BarCodeERR,IL300R,IL300L) "
                                            Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "',now(),now(), '" + Car(car_idx).device_status(19).ToString + "', '" + Car(car_idx).device_no.ToString + "', '', '', '" + Car(car_idx).get_tagId.ToString + "', '', '" + Car(car_idx).get_cstid.ToString + "'" + _
                                                "," + Car(car_idx).barcodeError0.ToString + "," + Car(car_idx).IL300L0.ToString + "," + Car(car_idx).IL300R0.ToString + ") ;"
                                            sqlCommand.CommandText = Query
                                            sqlCommand.ExecuteNonQuery()


                                            '判斷沒有命令，就產生 移動到別位置的命令
                                            Dim cmd_cnt As Integer = 0
                                            For ii As Integer = 0 To ListView1.Items.Count - 1
                                                Try
                                                    If Car(car_idx).device_no = CInt(Me.ListView1.Items(ii).SubItems(1).Text) Then
                                                        cmd_cnt += 1
                                                    End If
                                                Catch ex As Exception

                                                End Try
                                            Next
                                            If cmd_cnt <= 1 Then
                                                Dim cmd As String = Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, Car(car_idx).get_tagId(), Car(car_idx).wait_point, AllBlockPoint + "," + Car(car_idx).Block_Point, Car(car_idx).Block_Path)
                                                Dim cmd_list() As String = cmd.Split(",")
                                                Dim to_point As Integer = Car(car_idx).wait_point
                                                If cmd_list.Length > 5 Then
                                                    to_point = CInt(cmd_list(4))
                                                End If

                                                Send_CMD(Car(car_idx).device_no, 1, to_point, "ForkinRetry")
                                            End If


                                            ' Send_CMD(Car(car_idx).device_no, 1, Car(car_idx).wait_point)
                                            Load_Error()
                                            If Car(car_idx).device_status(19) = 15404 Or Car(car_idx).device_status(19) = 217 Then

                                                Dim idx As Integer = -1
                                                idx = comQGWrapper.CST_SearchByLoc(tagid.LOC)
                                                If idx > -1 Then
                                                    Dim ans As Integer
                                                    '  comQGWrapper.CST(idx).CarrierState = GEM.CS_BLOCKED
                                                    comQGWrapper.EventReportSendOb(GEM.EVENT_TransferAbortInitiated, comQGWrapper.CST(idx))
                                                    'new 12/02
                                                    comQGWrapper.EventReportSendOb(GEM.EVENT_TransferAbortCompleted, comQGWrapper.CST(idx))
                                                    Query = "Delete from mcs_cmd_list where  CARRIERID='" + comQGWrapper.CST(idx).CarrierID + "' or REQUEST_TIME < '" + Now.AddDays(-1).ToString("yyyy-MM-dd HH:mm:ss") + "'"
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                    Query = "update mcs_cmd_history  set End_time=now() where  COMMANDID='" + comQGWrapper.CST(idx).CommandID + "'"
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                    comQGWrapper.CST(idx).DEST = ""
                                                    comQGWrapper.CST(idx).PRIORITY = 0
                                                    comQGWrapper.CST(idx).mcstime = 0
                                                    comQGWrapper.CST(idx).CommandID = ""
                                                    For j As Integer = ListView2.Items.Count - 1 To 0 Step -1
                                                        Try
                                                            If ListView2.Items(j).SubItems(0).Text = comQGWrapper.CST(idx).CarrierID Then
                                                                ListView2.Items.RemoveAt(j)
                                                            End If
                                                        Catch ex As Exception
                                                        End Try
                                                    Next
                                                    'new 12/02

                                                    comQGWrapper.CST_Change(comQGWrapper.CST(idx).CarrierID, tagid.ZONE_NAME, tagid.LOC, GEM.CS_BLOCKED)
                                                    Query = "update `carrier`  set CARRIER_STATUS=" + GEM.CS_BLOCKED.ToString + " "
                                                    Query += " where  CARRIER_ID='" + comQGWrapper.CST(idx).CarrierID + "'"
                                                    sqlCommand.CommandText = Query
                                                    ans = sqlCommand.ExecuteNonQuery()
                                                    comQGWrapper.EventReportSendOb(GEM.EVENT_TransferAbortCompleted, comQGWrapper.CST(idx))
                                                    settext(Query + ":" + ans.ToString)

                                                End If

                                            End If
                                            Car(car_idx).To_AGV(6) = 0
                                            Car(car_idx).To_AGV(7) = 0

                                        ElseIf Car(car_idx).get_pin = 2 And (Car(car_idx).get_action = &H20 Or Car(car_idx).get_action = &H21) And Car(car_idx).get_Err = 0 Then
                                            If Car(car_idx).Car_type = "CRANE" And Car(car_idx).get_loading = 0 Then
                                                Car(car_idx).To_AGV(20) = 116 '未取到CST
                                            ElseIf Car(car_idx).Car_type = "CRANE" And Car(car_idx).get_loading = 3 And Not Car(car_idx).Cmd_RollData = Car(car_idx).get_cstid And Not Car(car_idx).Cmd_RollData = "" And Not Car(car_idx).Cmd_RollData.StartsWith("UNKNOWN") Then
                                                'Car(car_idx).To_AGV(20) = 101 '帳料不符
                                                For ii As Integer = 0 To 20
                                                    If comQGWrapper.CST_SearchByCSTID("UNKNOWN" + ii.ToString + "-" + Car(car_idx).get_cstid) = -1 Then
                                                        '建新帳
                                                        Dim new_cst_id As String = "UNKNOWN" + ii.ToString + "-" + Car(car_idx).get_cstid
                                                        '回傳index 
                                                        comQGWrapper.CST_Add("UNKNOWN" + ii.ToString + "-" + Car(car_idx).get_cstid, comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString, comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString, GEM.CS_TRANSFERRING)
                                                        '新增資料庫
                                                        Query = "INSERT ignore INTO `carrier` (`STK_NAME`, `CARRIER_ID`, `LOT_ID`, `CARRIER_TYPE`, `LOC_NAME`, `LOC_TYPE`, `SUB_LOC`, `CARRIER_STATUS`, `RROCESS_ID`, `STORED_TIME`, `REQ_TIME`, `UPDATE_DATE`, `UPDATE_BY`, `CREATE_DATE`, `CREATE_BY`, `BLOCK_FLAG`, `INV_FLAG`) " + _
                                                            "VALUES ('" + comQGWrapper.Eqpname + "', '" + new_cst_id + "', '" + new_cst_id + "', '', '" + comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString + "', '3', '" + comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString + "'," + _
                                                            " '" + GEM.CS_TRANSFERRING.ToString + "', '', now(),now(), now(), 'AGVC', now(), 'AGVC', '0', '1');"
                                                        sqlCommand.CommandText = Query
                                                        sqlCommand.ExecuteNonQuery()



                                                        Dim idx As Integer = -1
                                                        idx = comQGWrapper.CST_SearchByLoc(tagid.LOC)
                                                        If idx > -1 Then
                                                            comQGWrapper.EventReportSendOb(GEM.EVENT_TransferAbortInitiated, comQGWrapper.CST(idx))
                                                            comQGWrapper.EventReportSendOb(GEM.EVENT_TransferAbortCompleted, comQGWrapper.CST(idx))
                                                            Query = "Delete from mcs_cmd_list where  CARRIERID='" + comQGWrapper.CST(idx).CarrierID + "' or REQUEST_TIME < '" + Now.AddDays(-1).ToString("yyyy-MM-dd HH:mm:ss") + "'"
                                                            sqlCommand.CommandText = Query
                                                            sqlCommand.ExecuteNonQuery()
                                                            Query = "update mcs_cmd_history  set End_time=now() where  COMMANDID='" + comQGWrapper.CST(idx).CommandID + "'"
                                                            sqlCommand.CommandText = Query
                                                            sqlCommand.ExecuteNonQuery()
                                                            comQGWrapper.EventReportSendOb(GEM.EVENT_CarrierRemoved, comQGWrapper.CST(idx))
                                                            comQGWrapper.CST_REMOVE(comQGWrapper.CST(idx).CarrierID)

                                                            comQGWrapper.EventReportSendOb(GEM.EVENT_CarrierRemoveCompleted, comQGWrapper.CST(idx))
                                                        End If
                                                        If ForkType = "SHELF" Then
                                                            Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 1) = Car(car_idx).get_tagId.ToString
                                                        ElseIf ForkType = "EQ" Then
                                                            Dim idx1 As Integer = Tag_Point_ByTagid(Tag_point_list, Car(car_idx).get_tagId)
                                                            idx1 = Search_EMPTY_Shelf(Car(car_idx).get_tagId, "", AllBlockPoint + "," + Car(car_idx).Block_Point, Car(car_idx).Block_Path) '回傳的是tagid
                                                            If idx1 > 0 And Car(car_idx).cmd_idx < Car(car_idx).cmd_list.Length - 5 Then
                                                                Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 1) = idx1.ToString
                                                            Else
                                                                Car(car_idx).To_AGV(20) = 119 '找不到空棚
                                                            End If
                                                            ' Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 1) = Car(car_idx).get_tagId.ToString
                                                        End If

                                                        Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 2) = "Going_Check"
                                                        Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 3) = "FORKOUT"
                                                        Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 4) = "FINSH"
                                                        Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 5) = "FINSH"
                                                        Car(car_idx).cmd_idx += 1
                                                        '紀錄資料

                                                        Car(car_idx).barcodeError0 = Car(car_idx).Get2Derror
                                                        Car(car_idx).IL300L0 = int2Toint32(Car(car_idx).device_status(36), Car(car_idx).device_status(37))
                                                        Car(car_idx).IL300R0 = int2Toint32(Car(car_idx).device_status(38), Car(car_idx).device_status(39))
                                                        Car(car_idx).RollData = new_cst_id
                                                        Try
                                                            Query = "INSERT INTO `roll_data` (cmdkey,`Car_no`, `Rolltype`, `RollData`,`From_Pos`,`To_Pos`, `LM_time`) VALUES (" + Car(car_idx).cmd_sql_idx.ToString + ",'" + Car(car_idx).device_no.ToString + "', 'IN', '" + Car(car_idx).RollData.ToString + "','" + Car(car_idx).Cmd_From.ToString + "','" + Car(car_idx).Cmd_To.ToString + "', now());"
                                                            sqlCommand.CommandText = Query
                                                            sqlCommand.ExecuteNonQuery()
                                                        Catch ex As Exception
                                                            settext("ex:" + ex.Message)
                                                            settext("mysqlerror:" + Query)
                                                        End Try


                                                        Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`) "
                                                        Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "',now(),now(), '101', '" + Car(car_idx).device_no.ToString + "', '', '', '" + Car(car_idx).get_tagId.ToString + "', '', '" + Car(car_idx).Shelf_Car_No.ToString + "') ;"
                                                        sqlCommand.CommandText = Query
                                                        sqlCommand.ExecuteNonQuery()
                                                        Query = "UPDATE `agv_list` SET `RollData` = '" + Car(car_idx).RollData + "' WHERE `AGVNo` = '" + Car(car_idx).device_no.ToString + "';"
                                                        sqlCommand.CommandText = Query
                                                        sqlCommand.ExecuteNonQuery()
                                                        '鵬位除帳
                                                        Query = "update `shelf`  set CarrierID='' "
                                                        Query += " where  Tag_ID=" + Car(car_idx).get_tagId().ToString
                                                        sqlCommand.CommandText = Query
                                                        sqlCommand.ExecuteNonQuery()

                                                        Query = "update `carrier`  set LOC_NAME='" + comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString + "',SUB_LOC='" + comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString + "' "
                                                        Query += " where  CARRIER_ID='" + Car(car_idx).RollData + "'"
                                                        sqlCommand.CommandText = Query
                                                        sqlCommand.ExecuteNonQuery()

                                                        Query = "update `carrier`  set LOC_NAME='' ,SUB_LOC='' "
                                                        Query += " where  SUB_LOC='" + tagid.LOC + "'"
                                                        sqlCommand.CommandText = Query
                                                        sqlCommand.ExecuteNonQuery()
                                                        Exit For
                                                    End If
                                                Next
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).To_AGV(7) = 0
                                            Else
                                                '正常狀態

                                                Car(car_idx).cmd_idx += 1
                                                Car(car_idx).barcodeError0 = Car(car_idx).Get2Derror
                                                Car(car_idx).IL300L0 = int2Toint32(Car(car_idx).device_status(36), Car(car_idx).device_status(37))
                                                Car(car_idx).IL300R0 = int2Toint32(Car(car_idx).device_status(38), Car(car_idx).device_status(39))
                                                Car(car_idx).RollData = Car(car_idx).get_cstid ' 如果命令未知CST的話，就相信AGV barcode


                                                Try
                                                    Query = "INSERT INTO `roll_data` (cmdkey,`Car_no`, `Rolltype`, `RollData`,`From_Pos`,`To_Pos`, `LM_time`) VALUES (" + Car(car_idx).cmd_sql_idx.ToString + ",'" + Car(car_idx).device_no.ToString + "', 'IN', '" + Car(car_idx).RollData.ToString + "','" + Car(car_idx).Cmd_From.ToString + "','" + Car(car_idx).Cmd_To.ToString + "', now());"
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                Catch ex As Exception
                                                    settext("ex:" + ex.Message)
                                                    settext("mysqlerror:" + Query)
                                                End Try

                                                Query = "UPDATE `agv_list` SET `RollData` = '" + Car(car_idx).RollData + "' WHERE `AGVNo` = '" + Car(car_idx).device_no.ToString + "';"
                                                sqlCommand.CommandText = Query
                                                sqlCommand.ExecuteNonQuery()
                                                Dim idx As Integer = -1
                                                idx = comQGWrapper.CST_SearchByLoc(tagid.LOC)

                                                If idx > -1 Then
                                                    '有找到CST 如果命令是"" 或是unkonw 就新增 資料庫會卡相同ID
                                                    Dim cstidx As Integer = comQGWrapper.CST_SearchByCSTID(Car(car_idx).RollData)
                                                    If cstidx > -1 And Not idx = cstidx Then
                                                        '有CST 但是不相同
                                                        comQGWrapper.CST_REMOVE(comQGWrapper.CST(idx).CarrierID)
                                                        idx = cstidx
                                                    End If
                                                    comQGWrapper.CST_Change(comQGWrapper.CST(idx).CarrierID, comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString, comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString, "0", GEM.EVENT_CarrierTransferring)
                                                    If Car(car_idx).Cmd_RollData = "" Or Car(car_idx).Cmd_RollData.StartsWith("UNKNOWN") Then
                                                        Query = "insert ignore into `carrier` (STK_NAME,CARRIER_ID,LOT_ID,LOC_NAME,LOC_TYPE,SUB_LOC,CARRIER_STATUS,STORED_TIME,REQ_TIME,UPDATE_DATE,UPDATE_BY,CREATE_DATE,CREATE_BY) " + _
                                               "VALUES ('" + comQGWrapper.Eqpname + "', '" + Car(car_idx).RollData + "', '" + Car(car_idx).RollData + "','" + comQGWrapper.CST(idx).CarrierZoneName + "',1,'', '4', '', '', now(), 'AGVC', now(), 'AGVC');"
                                                        sqlCommand.CommandText = Query
                                                        sqlCommand.ExecuteNonQuery()
                                                    End If
                                                    comQGWrapper.EventReportSendOb(GEM.EVENT_CarrierIDRead, comQGWrapper.CST(idx).CarrierID + "," + comQGWrapper.CST(idx).CarrierLoc + "," + comQGWrapper.CST(idx).CarrierZoneName + "," + comQGWrapper.CST(idx).CarrierType + ",0")
                                                    'CarrierID

                                                End If
                                                If ForkType = "SHELF" Then
                                                    For j As Integer = 0 To comQGWrapper.ShelfData.Length - 1
                                                        Dim a As Integer = Car(car_idx).get_tagId()
                                                        If comQGWrapper.ShelfData(j).tag_id = Car(car_idx).get_tagId() Then
                                                            comQGWrapper.ShelfData(j).CarrierID = ""
                                                        End If
                                                    Next
                                                End If
                                                '棚位除帳


                                                Query = "update `shelf`  set CarrierID='' "
                                                Query += " where  Tag_ID=" + Car(car_idx).get_tagId().ToString
                                                sqlCommand.CommandText = Query
                                                sqlCommand.ExecuteNonQuery()
                                                If Car(car_idx).Cmd_RollData.StartsWith("UNKNOWN") Or Car(car_idx).Cmd_RollData = "" Then
                                                    Query = "delete from  `carrier` where LOC_NAME='" + tagid.ZONE_NAME + "' and SUB_LOC='" + tagid.LOC + "'"
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                End If
                                                Query = "update `carrier`  set LOC_NAME='" + comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString + "',SUB_LOC='" + comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString + "' "
                                                Query += " where  CARRIER_ID='" + Car(car_idx).RollData + "'"
                                                sqlCommand.CommandText = Query
                                                sqlCommand.ExecuteNonQuery()
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).To_AGV(7) = 0
                                            End If
                                            '要比對帳料
                                        End If
                                        'End If
                                    Case "FORKOUT"
                                        settext("FORKOUT")
                                        '10 shelf  11 EQ      
                                        Dim ForkType As String = ""
                                        Dim loc As Tag_Point = New Tag_Point
                                        If Car(car_idx).RollData = "" Then
                                            Car(car_idx).RollData = Car(car_idx).get_cstid
                                        End If
                                        Dim span As TimeSpan = Now.Subtract(Car(car_idx).starttime)
                                        Car(car_idx).T4 = span.TotalSeconds - Car(car_idx).T1 - Car(car_idx).T2 - Car(car_idx).T3

                                        For j As Integer = 0 To Tag_point_list.Length - 1
                                            If Tag_point_list(j).TagId = Car(car_idx).get_tagId Then
                                                If Tag_point_list(j).tagtype = 1 Then
                                                    Car(car_idx).To_AGV(6) = &H10
                                                    Car(car_idx).To_AGV(7) = Tag_point_list(j).stkval
                                                    loc = Tag_point_list(j)
                                                    ForkType = "SHELF"
                                                    Exit For
                                                ElseIf Tag_point_list(j).tagtype = 3 Then
                                                    Car(car_idx).To_AGV(6) = &H11
                                                    Car(car_idx).To_AGV(7) = Tag_point_list(j).stkval
                                                    loc = Tag_point_list(j)
                                                    ForkType = "EQ"
                                                    Exit For
                                                End If
                                            End If
                                        Next
                                        If ForkType = "" Then

                                            Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`,BarCodeERR,IL300R,IL300L) "
                                            Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "',now(),now(), '99', '" + Car(car_idx).device_no.ToString + "', '', '', '" + Car(car_idx).get_tagId.ToString + "', '', '" + Car(car_idx).get_cstid.ToString + "'" + _
                                                "," + Car(car_idx).barcodeError1.ToString + "," + Car(car_idx).IL300L1.ToString + "," + Car(car_idx).IL300R1.ToString + ") ;"
                                            sqlCommand.CommandText = Query
                                            sqlCommand.ExecuteNonQuery()
                                            settext(Car(car_idx).device_no.ToString + ":FORKOUT地點異常" + Car(car_idx).get_tagId.ToString)
                                            Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "FINSH"
                                        ElseIf Car(car_idx).get_pin = 0 And Car(car_idx).get_action = 0 And Car(car_idx).get_loading = 0 Then
                                            settext(Car(car_idx).device_no.ToString + ":載荷異常" + Car(car_idx).get_tagId().ToString)
                                            Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "FINSH"
                                        ElseIf Car(car_idx).get_pin >= 8 And (Car(car_idx).get_action = &H10 Or Car(car_idx).get_action = &H11) And Car(car_idx).get_Err = 0 Then

                                            '異常結束
                                            settext(Car(car_idx).device_no.ToString + ":異常結束")

                                            Car(car_idx).barcodeError1 = Car(car_idx).Get2Derror
                                            Car(car_idx).IL300L1 = int2Toint32(Car(car_idx).device_status(36), Car(car_idx).device_status(37))
                                            Car(car_idx).IL300R1 = int2Toint32(Car(car_idx).device_status(38), Car(car_idx).device_status(39))
                                            Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`,BarCodeERR,IL300R,IL300L) "
                                            Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "',now(),now(), '" + Car(car_idx).device_status(19).ToString + "', '" + Car(car_idx).device_no.ToString + "', '', '', '" + Car(car_idx).get_tagId.ToString + "', '', '" + Car(car_idx).get_cstid.ToString + "'" + _
                                                "," + Car(car_idx).barcodeError1.ToString + "," + Car(car_idx).IL300L1.ToString + "," + Car(car_idx).IL300R1.ToString + ") ;"
                                            sqlCommand.CommandText = Query
                                            sqlCommand.ExecuteNonQuery()
                                            Load_Error()
                                            If Car(car_idx).device_status(19) = 15400 Or Car(car_idx).device_status(19) = 218 Then
                                                '建立UNKNOWN 的帳
                                                '未找到CST 建立新帳
                                                Dim idx1 As Integer = -1
                                                idx1 = comQGWrapper.CST_SearchByLoc(Car(car_idx).get_tagId)
                                                If idx1 = -1 Then
                                                    comQGWrapper.CST_Add("UNKNOWN-" + loc.LOC, loc.ZONE_NAME, loc.LOC, 4)
                                                    Query = "insert ignore into `carrier` (STK_NAME,CARRIER_ID,LOT_ID,LOC_NAME,LOC_TYPE,SUB_LOC,CARRIER_STATUS,STORED_TIME,REQ_TIME,UPDATE_DATE,UPDATE_BY,CREATE_DATE,CREATE_BY) " + _
                                                   "VALUES ('" + comQGWrapper.Eqpname + "', '" + "UNKNOWN-" + loc.LOC + "', '" + "UNKNOWN-" + loc.LOC + "','" + loc.ZONE_NAME + "',1,'" + loc.LOC + "', '4', now(), '', now(), 'AGVC', now(), 'AGVC');"
                                                    Dim ans As Integer
                                                    sqlCommand.CommandText = Query
                                                    ans = sqlCommand.ExecuteNonQuery()
                                                    settext(Query + ":" + ans.ToString)
                                                End If


                                            End If

                                            Car(car_idx).To_AGV(6) = 0
                                            Car(car_idx).To_AGV(7) = 0
                                            '搜尋空白位置 ' 先找棚位 再找EQ 
                                            Dim totemp As Integer = 0

                                            Dim idx As Integer = Tag_Point_ByTagid(Tag_point_list, Car(car_idx).get_tagId)

                                            idx = Search_EMPTY_Shelf(Car(car_idx).get_tagId, "", AllBlockPoint + "," + Car(car_idx).Block_Point, Car(car_idx).Block_Path) '回傳的是tagid

                                            If idx > 0 And Car(car_idx).cmd_idx < Car(car_idx).cmd_list.Length - 5 Then
                                                Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 1) = idx.ToString
                                                Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 2) = "Going_Check"
                                                Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 3) = "FORKOUT"
                                                Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 4) = "FINSH"
                                                Car(car_idx).cmd_idx += 1
                                                Try
                                                    Dim cmd As String = Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, Car(car_idx).get_tagId(), Car(car_idx).wait_point, AllBlockPoint + "," + Car(car_idx).Block_Point, Car(car_idx).Block_Path)
                                                    Dim cmd_list() As String = cmd.Split(",")
                                                    Dim to_point As Integer = Car(car_idx).wait_point
                                                    If cmd_list.Length > 10 Then
                                                        to_point = CInt(cmd_list(9))
                                                    End If

                                                    'Send_CMD(Car(car_idx).device_no, 1, to_point, "ForkoutRetry")
                                                Catch ex As Exception

                                                End Try


                                            Else
                                                Car(car_idx).To_AGV(20) = 119 '找不到空棚
                                            End If
                                            ' Send_CMD_CST(Car(car_idx).device_no, 2, totemp, Car(car_idx).get_cstid, Now.Ticks.ToString, 9999)


                                        ElseIf Car(car_idx).get_pin = 2 And (Car(car_idx).get_action = &H10 Or Car(car_idx).get_action = &H11) And Car(car_idx).get_Err = 0 Then
                                            ' 只有ROLLOUT
                                            settext(Car(car_idx).device_no.ToString + ":FORKOUT")
                                            If Car(car_idx).Car_type = "CRANE" And Car(car_idx).get_loading = 3 Then
                                                Car(car_idx).To_AGV(20) = 112 '未取到CST
                                            Else
                                                Query = "update `shelf` A ,agv_list B  set A.CarrierID='" + Car(car_idx).RollData + "'  "
                                                Query += " where  Tag_ID=" + Car(car_idx).get_tagId().ToString + " and B.AGVNo=" + Car(car_idx).device_no.ToString
                                                sqlCommand.CommandText = Query
                                                sqlCommand.ExecuteNonQuery()
                                                Car(car_idx).barcodeError1 = Car(car_idx).Get2Derror
                                                Car(car_idx).IL300L1 = int2Toint32(Car(car_idx).device_status(36), Car(car_idx).device_status(37))
                                                Car(car_idx).IL300R1 = int2Toint32(Car(car_idx).device_status(38), Car(car_idx).device_status(39))
                                                Dim idx As Integer = -1
                                                Dim ans As Integer
                                                idx = comQGWrapper.CST_SearchByCSTID(Car(car_idx).RollData)
                                                settext(Car(car_idx).device_no.ToString + ":" + Car(car_idx).RollData + "=" + idx.ToString)
                                                If idx > -1 Then


                                                    If loc.site.IndexOf("common") > -1 Then
                                                        '暫存  
                                                        comQGWrapper.CST_Change(comQGWrapper.CST(idx).CarrierID, loc.ZONE_NAME, loc.LOC, comQGWrapper.CST(idx).CarrierState, GEM.EVENT_CarrierStoredAlt)
                                                        comQGWrapper.CST(idx).CarrierState = GEM.CS_STOREDALTERMATE
                                                    ElseIf loc.tagtype = 1 Then
                                                        comQGWrapper.CST_Change(comQGWrapper.CST(idx).CarrierID, loc.ZONE_NAME, loc.LOC, comQGWrapper.CST(idx).CarrierState)
                                                        comQGWrapper.CST(idx).CarrierState = GEM.CS_STOREDCOMPLETED
                                                    ElseIf loc.tagtype = 3 Then
                                                        comQGWrapper.CST_Change(comQGWrapper.CST(idx).CarrierID, loc.ZONE_NAME, loc.LOC, comQGWrapper.CST(idx).CarrierState)
                                                        comQGWrapper.EventReportSendOb(GEM.EVENT_CarrierWaitOut, comQGWrapper.CST(idx))
                                                        comQGWrapper.EventReportSendOb(GEM.EVENT_CarrierRemoved, comQGWrapper.CST(idx))
                                                        comQGWrapper.CST(idx).CarrierState = GEM.CS_NONE
                                                    Else
                                                        comQGWrapper.CST_Change(comQGWrapper.CST(idx).CarrierID, loc.ZONE_NAME, loc.LOC, comQGWrapper.CST(idx).CarrierState)
                                                    End If
                                                    'comQGWrapper.CST_Add(Car(car_idx).RollData, loc.ZONE_NAME, loc.LOC, 4)
                                                    Query = "insert ignore into `carrier` (STK_NAME,CARRIER_ID,LOT_ID,LOC_NAME,LOC_TYPE,SUB_LOC,CARRIER_STATUS,STORED_TIME,REQ_TIME,UPDATE_DATE,UPDATE_BY,CREATE_DATE,CREATE_BY) " + _
                                                   "VALUES ('" + comQGWrapper.Eqpname + "', '" + Car(car_idx).RollData + "', '" + Car(car_idx).RollData + "','" + loc.ZONE_NAME + "',1,'" + loc.LOC + "', '4', now(), '', now(), 'AGVC', now(), 'AGVC');"
                                                    sqlCommand.CommandText = Query
                                                    ans = sqlCommand.ExecuteNonQuery()
                                                    settext(Query + ":" + ans.ToString)


                                                    setCommtext("CarrierZoneName change:" + comQGWrapper.CST(idx).CarrierID + "->" + loc.ZONE_NAME)
                                                    Query = "update `carrier`  set LOC_NAME='" + loc.ZONE_NAME + "',LOC_TYPE=" + loc.tagtype.ToString + ",SUB_LOC='" + loc.LOC + "' ,CARRIER_STATUS	=" + comQGWrapper.CST(idx).CarrierState.ToString
                                                    Query += " where  CARRIER_ID='" + Car(car_idx).RollData + "'"
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                Else
                                                    '未找到CST 建立新帳
                                                    idx = comQGWrapper.CST_Add(Car(car_idx).RollData, loc.ZONE_NAME, loc.LOC, 4)
                                                    If loc.site.IndexOf("common") > -1 Then
                                                        '暫存  
                                                        comQGWrapper.CST_Change(comQGWrapper.CST(idx).CarrierID, loc.ZONE_NAME, loc.LOC, comQGWrapper.CST(idx).CarrierState, GEM.EVENT_CarrierStoredAlt)
                                                        comQGWrapper.CST(idx).CarrierState = GEM.CS_STOREDALTERMATE
                                                    ElseIf loc.tagtype = 1 Then
                                                        comQGWrapper.CST(idx).CarrierState = GEM.CS_STOREDCOMPLETED
                                                        comQGWrapper.CST_Change(comQGWrapper.CST(idx).CarrierID, loc.ZONE_NAME, loc.LOC, comQGWrapper.CST(idx).CarrierState)
                                                    ElseIf loc.tagtype = 3 Then
                                                        comQGWrapper.CST_Change(comQGWrapper.CST(idx).CarrierID, loc.LOC, loc.LOC, comQGWrapper.CST(idx).CarrierState)
                                                        comQGWrapper.EventReportSendOb(GEM.EVENT_CarrierWaitOut, comQGWrapper.CST(idx))
                                                        comQGWrapper.EventReportSendOb(GEM.EVENT_CarrierRemoved, comQGWrapper.CST(idx))
                                                        comQGWrapper.CST(idx).CarrierState = GEM.CS_NONE
                                                    Else
                                                        comQGWrapper.CST_Change(comQGWrapper.CST(idx).CarrierID, loc.ZONE_NAME, loc.LOC, comQGWrapper.CST(idx).CarrierState)
                                                    End If
                                                    Query = "insert ignore into `carrier` (STK_NAME,CARRIER_ID,LOT_ID,LOC_NAME,LOC_TYPE,SUB_LOC,CARRIER_STATUS,STORED_TIME,REQ_TIME,UPDATE_DATE,UPDATE_BY,CREATE_DATE,CREATE_BY) " + _
                                                   "VALUES ('" + comQGWrapper.Eqpname + "', '" + Car(car_idx).RollData + "', '" + Car(car_idx).RollData + "','" + loc.ZONE_NAME + "',1,'" + loc.LOC + "', '4', now(), '', now(), 'AGVC', now(), 'AGVC');"

                                                    sqlCommand.CommandText = Query
                                                    ans = sqlCommand.ExecuteNonQuery()
                                                    settext(Query + ":" + ans.ToString)
                                                End If

                                                Car(car_idx).RollData = "" '車子除帳   
                                                Query = "UPDATE `agv_list` SET `RollData` = '" + Car(car_idx).RollData + "' WHERE `AGVNo` = '" + Car(car_idx).device_no.ToString + "';"
                                                sqlCommand.CommandText = Query
                                                sqlCommand.ExecuteNonQuery()
                                                idx = comQGWrapper.CST_SearchByLoc(comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString)
                                                If (idx > -1) Then
                                                    Query = "delete from  `carrier`  where CARRIER_ID='" + comQGWrapper.CST(idx).CarrierID + "'"
                                                    sqlCommand.CommandText = Query
                                                    sqlCommand.ExecuteNonQuery()
                                                    comQGWrapper.CST_REMOVE(comQGWrapper.CST(idx).CarrierID)
                                                End If

                                                Car(car_idx).cmd_Shelf_Car_No = 0
                                                Car(car_idx).To_AGV(6) = 0
                                                Car(car_idx).To_AGV(7) = 0
                                                Car(car_idx).cmd_idx += 1

                                            End If


                                        End If
                                    Case "RECHARGE"
                                        Car(car_idx).To_AGV(6) = &H30
                                        If Car(car_idx).get_interlock >= 8 And (Car(car_idx).get_action = &H30) And Car(car_idx).get_Err = 0 Then
                                            '異常觸發
                                            settext(Car(car_idx).device_no.ToString + ":充電異常結束")


                                            Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`) "
                                            Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "',now(),now(), '" + Car(car_idx).device_status(19).ToString + "', '" + Car(car_idx).device_no.ToString + "', '', '', '" + Car(car_idx).get_tagId.ToString + "', '', '" + Car(car_idx).Shelf_Car_No.ToString + "') ;"
                                            sqlCommand.CommandText = Query
                                            sqlCommand.ExecuteNonQuery()
                                            Load_Error()


                                            Car(car_idx).To_AGV(6) = 0
                                            Car(car_idx).To_AGV(7) = 0

                                            If Car(car_idx).get_tagId() = 12002 And Car(car_idx).cmd_idx < 30 Then

                                                Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 1) = "12001"
                                                Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 2) = "Going"
                                                Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 3) = "12002"
                                                Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 4) = "Going"
                                                Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 5) = "RECHARGE"
                                                Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 6) = "FINSH"
                                            End If
                                            Car(car_idx).cmd_idx += 1





                                        ElseIf Car(car_idx).get_interlock = 1 And Car(car_idx).To_AGV(6) = Car(car_idx).get_action And (Car(car_idx).get_SOC >= SOC) Then
                                            '
                                            settext(Car(car_idx).device_no.ToString + ":SOC to limit:" + SOC.ToString + "%")
                                            Car(car_idx).To_AGV(6) = 0
                                            Car(car_idx).cmd_idx += 1
                                        ElseIf Car(car_idx).get_interlock = 2 And Car(car_idx).To_AGV(6) = Car(car_idx).get_action And Car(car_idx).get_Err = 0 Then
                                            Car(car_idx).To_AGV(6) = 0
                                            Car(car_idx).cmd_idx += 1
                                        ElseIf BitCheck(Car(car_idx).BMS1(16), 8) Or BitCheck(Car(car_idx).BMS1(16), 9) Or BitCheck(Car(car_idx).BMS1(16), 12) Or BitCheck(Car(car_idx).BMS1(16), 13) Then
                                            Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`) "
                                            Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "',now(),now(), '125', '" + Car(car_idx).device_no.ToString + "', '', '', '" + Car(car_idx).get_tagId.ToString + "', '', '" + Car(car_idx).Shelf_Car_No.ToString + "') ;"
                                            sqlCommand.CommandText = Query
                                            sqlCommand.ExecuteNonQuery()
                                            Dim bms As String = int2str(Car(car_idx).BMS1, 0, 53)
                                            settext(Car(car_idx).device_no.ToString + "BMS1:" + bms)
                                            bms = int2str(Car(car_idx).BMS2, 0, 53)
                                            settext(Car(car_idx).device_no.ToString + "BMS2:" + bms)
                                            Load_Error()
                                            Car(car_idx).To_AGV(6) = 0
                                            Car(car_idx).cmd_idx += 1
                                        ElseIf BitCheck(Car(car_idx).BMS2(16), 8) Or BitCheck(Car(car_idx).BMS2(16), 9) Or BitCheck(Car(car_idx).BMS2(16), 12) Or BitCheck(Car(car_idx).BMS2(16), 13) Then
                                            Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`) "
                                            Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "',now(),now(), '125', '" + Car(car_idx).device_no.ToString + "', '', '', '" + Car(car_idx).get_tagId.ToString + "', '', '" + Car(car_idx).Shelf_Car_No.ToString + "') ;"
                                            sqlCommand.CommandText = Query
                                            sqlCommand.ExecuteNonQuery()
                                            Dim bms As String = int2str(Car(car_idx).BMS1, 0, 53)
                                            settext(Car(car_idx).device_no.ToString + "BMS1:" + bms)
                                            bms = int2str(Car(car_idx).BMS2, 0, 53)
                                            settext(Car(car_idx).device_no.ToString + "BMS2:" + bms)
                                            Load_Error()
                                            Car(car_idx).To_AGV(6) = 0
                                            Car(car_idx).cmd_idx += 1

                                        ElseIf Car(car_idx).get_SOC >= 85 Then

                                            For k As Integer = 0 To ListView1.Items.Count - 1
                                                If Not ListView1.Items(k).SubItems(2).Text = "4" And ListView1.Items(k).SubItems(1).Text = Car(car_idx).device_no.ToString Then
                                                    settext(Car(car_idx).device_no.ToString + ":other cmd" + ListView1.Items(k).SubItems(0).Text)
                                                    Car(car_idx).To_AGV(6) = 0
                                                    Car(car_idx).cmd_idx += 1
                                                    Exit For
                                                End If
                                            Next
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

                                    Case "WindStart" 'Wind speed
                                        settext(Car(car_idx).device_no.ToString + ":Wind  " + Now().ToString)
                                        Car(car_idx).To_AGV(6) = 6
                                        Car(car_idx).To_AGV(7) = 1
                                        If Car(car_idx).get_action = 50 And Car(car_idx).get_interlock = 2 And Car(car_idx).get_Err = 0 Then
                                            Car(car_idx).cmd_idx += 1
                                            Car(car_idx).Counter_timer = Now()
                                        End If
                                    Case "WindWaiting"
                                        If (DateDiff("s", Car(car_idx).Counter_timer, Now) > 250) Then
                                            Car(car_idx).To_AGV(6) = 6
                                            Car(car_idx).To_AGV(7) = 0
                                            Car(car_idx).cmd_idx += 1
                                        End If
                                    Case "WindEnd"
                                        settext(Car(car_idx).device_no.ToString + ":Wind  " + Now().ToString)
                                        Car(car_idx).To_AGV(6) = 6
                                        Car(car_idx).To_AGV(7) = 0
                                        If Car(car_idx).get_action = 50 And Car(car_idx).get_interlock = 2 And Car(car_idx).get_Err = 0 Then
                                            Car(car_idx).To_AGV(6) = 6
                                            Car(car_idx).To_AGV(7) = 0
                                            Car(car_idx).cmd_idx += 1
                                            Car(car_idx).Counter_timer = Now()
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
                                    Case "SET_SITE"
                                        Car(car_idx).Site = Car(car_idx).ext_cmd
                                        Car(car_idx).cmd_idx += 1
                                    Case "FINSH"
                                        Try
                                            'Query = "update `point` set `X`=" + Car(car_idx).AXIS_X.ToString + ",`Y`=" + Car(car_idx).AXIS_Y.ToString + " where `Y` < 582 and tag_id=" + Car(car_idx).get_tagId().ToString
                                            ' Dim test As Integer = Update_SQL(Query)
                                            '  settext("Update_SQL:" + Query + "->" + test.ToString)
                                            Dim CmdTo As String = Car(car_idx).Cmd_To
                                            If Car(car_idx).Cmd_To >= 10 Then
                                                CmdTo = Car(car_idx).get_tagId().ToString
                                            End If
                                            If Car(car_idx).Cmd_From = 1 Then
                                                Car(car_idx).barcodeError0 = Car(car_idx).Get2Derror
                                                Car(car_idx).IL300L0 = int2Toint32(Car(car_idx).device_status(36), Car(car_idx).device_status(37))
                                                Car(car_idx).IL300R0 = int2Toint32(Car(car_idx).device_status(38), Car(car_idx).device_status(39))
                                            End If
                                            Query = "update  agv_cmd_history set CmdTo='" + CmdTo + "',End_TIme=now(),end_distance=" + Car(car_idx).get_distance.ToString + ",empty_time=" + Car(car_idx).empty_time.ToString + ",Load_time=" + Car(car_idx).Load_time.ToString + ",Error_time=" + Car(car_idx).Error_time.ToString + _
                                            " , T1=" + Car(car_idx).T1.ToString + " , T2=" + Car(car_idx).T2.ToString + " , T3=" + Car(car_idx).T3.ToString + " , T4=" + Car(car_idx).T4.ToString + _
                                            " , BarCodeERR0=" + Car(car_idx).barcodeError0.ToString + " , IL300L0=" + Car(car_idx).IL300L0.ToString + " , BarCodeERR1=" + Car(car_idx).barcodeError1.ToString + " , IL300R0=" + Car(car_idx).IL300R0.ToString + " , IL300R1=" + Car(car_idx).IL300R1.ToString + " , IL300L1=" + Car(car_idx).IL300L1.ToString + _
                                            " where Cmdkey = " + Car(car_idx).cmd_sql_idx.ToString + _
                                                " and Start_Time >'" + Now().AddHours(-2).ToString("yyyy-MM-dd HH:mm:ss") + "'"
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
                                        For ii As Integer = 0 To 20
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
                                        ' Car(car_idx).CommandID = ""
                                        Car(car_idx).cmd_idx = -2 '恢復可接受命令狀態
                                        comQGWrapper.EventReportSendOb(GEM.EVENT_CraneIdle, Car(car_idx).CommandID + "," + comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString)

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

                                        For ii As Integer = 0 To 20
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
                                        comQGWrapper.EventReportSendOb(GEM.EVENT_CraneIdle, Car(car_idx).CommandID + "," + comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString)
                                        ' Car(car_idx).CommandID = ""
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
                                        Car(car_idx).State = "ACTIVE"
                                        If Car(car_idx).get_tagId = Car(car_idx).To_pos Then

                                            Car(car_idx).cmd_idx += 1
                                            Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString
                                            Dim span As TimeSpan = Now.Subtract(Car(car_idx).starttime)
                                            If Car(car_idx).T1 = 0 Then
                                                Car(car_idx).T1 = span.TotalSeconds
                                            Else
                                                Car(car_idx).T3 = span.TotalSeconds - Car(car_idx).T1 - Car(car_idx).T2
                                            End If
                                        ElseIf (Car(car_idx).get_status = 0 Or Car(car_idx).get_status = 2) And Car(car_idx).get_Err = 0 Then
                                            '判斷時間
                                            Dim wait_setting As Integer = 2

                                            Car(car_idx).Wait_count += 1
                                            If Car(car_idx).Wait_count > wait_setting Then
                                                Car(car_idx).cmd_idx -= 1
                                                Car(car_idx).Wait_count = 0
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
                                        Car(car_idx).State = "ACTIVE"
                                        If Car(car_idx).get_tagId = Car(car_idx).To_pos Then
                                            Dim span As TimeSpan = Now.Subtract(Car(car_idx).starttime)
                                            Car(car_idx).cmd_idx += 1
                                            Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString
                                            Car(car_idx).T1 = span.TotalSeconds
                                            If Car(car_idx).T1 < 0 Then
                                                Car(car_idx).T1 = 0
                                            End If
                                        ElseIf Car(car_idx).get_tagId = Car(car_idx).To_temp_pos Then
                                            Car(car_idx).cmd_idx -= 1
                                            Car(car_idx).Wait_count = 0
                                        ElseIf (Car(car_idx).get_status = 0 Or Car(car_idx).get_status = 2) And Car(car_idx).get_Err = 0 Then
                                            '判斷時間
                                            Dim wait_setting As Integer = 2

                                            Car(car_idx).Wait_count += 1
                                            If Car(car_idx).Wait_count > wait_setting Then
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
                                        Car(car_idx).State = "ACTIVE"
                                        If Car(car_idx).get_tagId = Car(car_idx).To_pos Then
                                            '到達目的，跳到下一個
                                            Car(car_idx).cmd_idx += 1
                                            Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString
                                            Car(car_idx).To_pos = 0

                                            Dim span As TimeSpan = Now.Subtract(Car(car_idx).starttime)
                                            Car(car_idx).T3 = span.TotalSeconds - Car(car_idx).T1 - Car(car_idx).T2

                                        ElseIf (Car(car_idx).get_status = 0 Or Car(car_idx).get_status = 2) And Car(car_idx).get_Err = 0 Then
                                            '判斷時間
                                            '判斷時間
                                            Dim wait_setting As Integer = 2

                                            If Car(car_idx).get_tagId = Car(car_idx).To_temp_pos Then
                                                Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString
                                                wait_setting = 3
                                            End If
                                            Car(car_idx).Wait_count += 1
                                            If Car(car_idx).Wait_count > wait_setting Then
                                                Car(car_idx).cmd_idx -= 1
                                                Car(car_idx).Wait_count = 0
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

                                        Car(car_idx).from_pos = Car(car_idx).get_tagId
                                        Try
                                            Car(car_idx).To_pos = CInt(Car(car_idx).cmd_list(Car(car_idx).cmd_idx))
                                        Catch ex As Exception
                                            Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`,BarCodeERR,IL300R,IL300L) "
                                            Query += "VALUES ('" + Car(car_idx).cmd_sql_idx.ToString + "',now(),now(), '97', '" + Car(car_idx).device_no.ToString + "', '', '', '" + Car(car_idx).get_tagId.ToString + "', '', '" + Car(car_idx).get_cstid.ToString + "'" + _
                                                "," + Car(car_idx).barcodeError1.ToString + "," + Car(car_idx).IL300L1.ToString + "," + Car(car_idx).IL300R1.ToString + ") ;"
                                            sqlCommand.CommandText = Query
                                            sqlCommand.ExecuteNonQuery()
                                            settext(Car(car_idx).device_no.ToString + " Car(car_idx).To_pos Err:" + Car(car_idx).cmd_list(Car(car_idx).cmd_idx))
                                            Car(car_idx).To_pos = Car(car_idx).from_pos
                                            Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "FINSH"
                                            Car(car_idx).cmd_list(Car(car_idx).cmd_idx + 1) = "FINSH"
                                        End Try
                                        Dim wait_setting As Integer = 0
                                        If Car(car_idx).sflag = 1 Then
                                            wait_setting = 5
                                        End If
                                        Car(car_idx).Wait_count += 1
                                        If Car(car_idx).Wait_count > wait_setting Then
                                            Car(car_idx).Wait_count = 0
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
                                            Car(car_idx).subcmd = Car(car_idx).get_tagId().ToString
                                            settext("wait 退避" + Car(car_idx).Wait_count.ToString)
                                        End If

                                End Select
                            Car(car_idx).To_AGV(1) = Car(car_idx).To_pos
                            '-----------------
                            If Car(car_idx).cmd_idx >= 0 Then
                                Try
                                    Query = "update agv_cmd_list set step_i=" + Car(car_idx).cmd_idx.ToString + ",CMD_status='" + Car(car_idx).cmd_list(Car(car_idx).cmd_idx).ToString + "',SubCmd='" + Car(car_idx).subcmd + "',cmd_cnt='" + Car(car_idx).main_subcmd.Split(",").Length.ToString + "' where CmdKey =" + Car(car_idx).cmd_sql_idx.ToString
                                    sqlCommand.CommandText = Query
                                    sqlCommand.ExecuteNonQuery()
                                Catch ex As Exception
                                    Car(car_idx).To_AGV(20) = 119
                                    settext("Step 異常" + Car(car_idx).Wait_count.ToString)
                                End Try

                            End If

                        Else
                            '表列找不到SQL
                            Query = "update  agv_cmd_history set Requestor=concat(Requestor,'(X)'),End_TIme=now(),end_distance=" + Car(car_idx).get_distance.ToString + ",empty_time=" + Car(car_idx).empty_time.ToString + ",Load_time=" + Car(car_idx).Load_time.ToString + ",Error_time=" + Car(car_idx).Error_time.ToString + " where Cmdkey = " + Car(car_idx).cmd_sql_idx.ToString + " and Start_Time >'" + Now().AddHours(-2).ToString("yyyy-MM-dd HH:mm:ss") + "'"
                            sqlCommand.CommandText = Query
                            sqlCommand.ExecuteNonQuery()
                            Car(car_idx).cmd_idx = -2 '重置命令
                            comQGWrapper.EventReportSendOb(GEM.EVENT_CraneIdle, Car(car_idx).CommandID + "," + comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString)
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
                            ' Car(car_idx).CommandID = ""
                            'Car(car_idx).To_AGV(20) = 0
                            Car(car_idx).State = "IDLE"
                            For ii As Integer = 0 To 20
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

                        If Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "Going_Check" And Car(car_idx).get_pin = 10 And Car(car_idx).Car_type = "PIN" Then
                            '判斷目前位置不等於出發或是出發地-1
                            If Car(car_idx).get_loading = 3 Then
                                Car(car_idx).Loading_err_cnt = 0
                            End If
                            Dim a As Integer = Car(car_idx).get_Shelf_Car_No
                            If Not (Car(car_idx).get_tagId = Car(car_idx).from_pos Or Car(car_idx).get_tagId = Car(car_idx).from_pos - 1) Then

                                If Car(car_idx).get_loading < 3 And Loading_Check.Checked = True And ALL_Loading_check.Checked = True And Car(car_idx).get_pin = 10 Then
                                    ' Car(car_idx).cmd_idx = -2
                                    Car(car_idx).Loading_err_cnt += 1
                                    If Car(car_idx).Loading_err_cnt > 3 Then

                                        Car(car_idx).To_AGV(20) = 100 '  未取到架台
                                    End If
                                ElseIf Car(car_idx).get_loading < 3 And Loading_Check.Checked = True And Car(car_idx).get_pin = 10 And Car(car_idx).get_tagId = Car(car_idx).Cmd_From + 2 Then
                                    '非全時監控
                                    Car(car_idx).Loading_err_cnt += 1


                                    Car(car_idx).To_AGV(20) = 100 '  未取到架台

                                ElseIf (Not Car(car_idx).get_Shelf_Car_No = Car(car_idx).cmd_Shelf_Car_No) And Car(car_idx).cmd_Shelf_Car_No > 0 And Not Car(car_idx).device_no = 6 And Agvc_shelfcheck.Checked = True And Not Car(car_idx).Car_type = "FORK" Then
                                    'Car(car_idx).cmd_idx = -2

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
                        ElseIf (Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "Going_Check" Or Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "GoingEmpty" Or Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "Going") And Car(car_idx).get_Err = 0 And Car(car_idx).device_status(15) = 4 And Car(car_idx).Pre_TagID_time < Now.AddMinutes(-5) Then
                            Car(car_idx).To_AGV(20) = 102
                        ElseIf (Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "Going_Check" Or Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "GoingEmpty" Or Car(car_idx).cmd_list(Car(car_idx).cmd_idx) = "Going") And Car(car_idx).get_Err = 0 And (Car(car_idx).device_status(15) = 12 Or Car(car_idx).device_status(15) = 8) And Car(car_idx).Pre_TagID_time < Now.AddMinutes(-10) Then
                            Car(car_idx).To_AGV(20) = 102
                        End If
                        If Car(car_idx).get_Err = 100 And Car(car_idx).get_loading = 3 Then
                            '自動復歸
                            Car(car_idx).To_AGV(20) = 0

                        End If

                        '  settext(Car(car_idx).device_no.ToString + ":車子狀態不等於4，無重新規劃")
                        If Car(car_idx).get_status = 12 Or Car(car_idx).get_status = 8 Then
                            Car(car_idx).rate_point = Car(car_idx).get_tagId()
                            ' settext(Car(car_idx).device_no.ToString + ":設定旋轉點位" + Car(car_idx).rate_point.ToString + "，無重新規劃")
                        End If

                        If Car(car_idx).sflag = 1 Then
                            settext(Car(car_idx).device_no.ToString + ":退避中，無重新規劃")
                        ElseIf Not Car(car_idx).cmd_list(Car(car_idx).cmd_idx).StartsWith("Going") Then
                            '  settext(Car(car_idx).device_no.ToString + ":狀態->" + Car(car_idx).cmd_list(Car(car_idx).cmd_idx) + "，無重新規劃")
                        ElseIf Car(car_idx).subcmd.Split(",").Length > Car(car_idx).RePath Then
                            ' settext(Car(car_idx).device_no.ToString + ":長度大於3，無重新規劃")
                        ElseIf Car(car_idx).subcmd.Split(",").Length <= 2 Then
                            ' settext(Car(car_idx).device_no.ToString + ":長度小於2，無重新規劃")
                        ElseIf Not Car(car_idx).get_status = 4 Then
                            'settext(Car(car_idx).device_no.ToString + ":車子狀態不等於4，無重新規劃")
                            If Car(car_idx).get_status = 12 Or Car(car_idx).get_status = 8 Then
                                Car(car_idx).rate_point = Car(car_idx).get_tagId()
                                'settext(Car(car_idx).device_no.ToString + ":設定旋轉點位" + Car(car_idx).rate_point.ToString + "，無重新規劃")
                            End If
                        ElseIf Car(car_idx).get_tagId = Car(car_idx).To_temp_pos Then
                            ' settext(Car(car_idx).device_no.ToString + ":車子到達暫時停車點")
                        ElseIf Car(car_idx).subcmd.EndsWith(Car(car_idx).To_pos.ToString) Then
                            '  settext(Car(car_idx).device_no.ToString + ":已規劃到目的地" + Car(car_idx).subcmd + "，無重新規劃")
                        ElseIf Car(car_idx).rate_point = Car(car_idx).get_tagId() Then
                            ' settext(Car(car_idx).device_no.ToString + ":目前旋轉中" + Car(car_idx).subcmd + "，無重新規劃")
                        Else
                            Dim R_subcmd As String = Send2AGV(car_idx, 11)
                            settext(Car(car_idx).device_no.ToString + ":重新規劃路徑" + R_subcmd, True, Car(car_idx).device_no)
                            Query = "update agv_cmd_list set step_i=" + Car(car_idx).cmd_idx.ToString + ",CMD_status='" + Car(car_idx).cmd_list(Car(car_idx).cmd_idx).ToString + "',SubCmd='" + Car(car_idx).subcmd + "',cmd_cnt='" + Car(car_idx).main_subcmd.Split(",").Length.ToString + "' where CmdKey =" + Car(car_idx).cmd_sql_idx.ToString
                            sqlCommand.CommandText = Query
                            sqlCommand.ExecuteNonQuery()

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
                                Car(car_idx).force_tagId(1)
                                Query = "delete from  agv_cmd_list where CmdKey =" + CInt(Me.ListView1.Items(i).SubItems(0).Text).ToString
                                sqlCommand.CommandText = Query
                                sqlCommand.ExecuteNonQuery()
                                Query = "update  agv_cmd_history set End_TIme=now() where Cmdkey = " + CInt(Me.ListView1.Items(i).SubItems(0).Text).ToString
                                sqlCommand.CommandText = Query
                                sqlCommand.ExecuteNonQuery()
                                Car(car_idx).cmd_sql_idx = 0
                                '  Car(car_idx).CommandID = ""
                                Car(car_idx).State = "IDLE"
                                Car(car_idx).cmd_idx = -2 '恢復可接受命令狀態
                                comQGWrapper.EventReportSendOb(GEM.EVENT_CraneIdle, Car(car_idx).CommandID + "," + comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString)
                            End If
                        Next
                    Catch ex As Exception
                        Car(car_idx).cmd_sql_idx = 0
                        ' Car(car_idx).CommandID = ""
                        Car(car_idx).State = "IDLE"
                        Car(car_idx).cmd_idx = -2 '恢復可接受命令狀態
                        comQGWrapper.EventReportSendOb(GEM.EVENT_CraneIdle, Car(car_idx).CommandID + "," + comQGWrapper.Eqpname + "C" + Car(car_idx).device_no.ToString)
                    End Try

                    Dim check_idx As Boolean = Check_SQL_idx(Car(car_idx).cmd_sql_idx)
                    If Car(car_idx).To_AGV(20) >= 100 And Car(car_idx).To_AGV(20) < 1000 And Not (Car(car_idx).To_AGV(20) = 105 And (Car(car_idx).get_Volt < Car(car_idx).Recharge_volt Or Car(car_idx).get_SOC < Car(car_idx).Recharge_SOC Or Car(car_idx).device_status(20) = 8)) And check_idx = False Then
                        '電壓恢復
                        Car(car_idx).cmd_idx = -2 '重置命令
                        Car(car_idx).To_AGV(20) = 0
                    End If
                    '
                End If
                '第一階段上位異常 

                '電池1 狀態異常
                'If Car(car_idx).BMS1(16) > 0 Then
                '    If (BmsAlertIdx And Car(car_idx).BMS1(16)) > 0 Then
                '        Car(car_idx).To_AGV(20) = 30000 + InttoBitidx(Car(car_idx).BMS1(16))
                '    ElseIf (BmsWarmIdx And Car(car_idx).BMS1(16)) > 0 Then
                '        '警報處理機制
                '        '鎖車->派到充電站
                '        Send_CMD(Car(car_idx).device_no, 1, 7, "Bat1ErrLock" + Car(car_idx).BMS1(16).ToString)
                '        Dim chargerlist() As String = Car(car_idx).Recharge_Point_list.Split(",")
                '        For change_i As Integer = 0 To chargerlist.Length - 1
                '            Dim flag As Boolean = False
                '            '判斷充電站有沒有車子充電
                '            For j As Integer = 0 To Me.ListView1.Items.Count - 1
                '                If (Me.ListView1.Items(j).SubItems(2).Text) = 4 And Me.ListView1.Items(j).SubItems(3).Text = chargerlist(change_i) Then
                '                    flag = True
                '                End If
                '            Next
                '            If flag = False Then
                '                '充電站IDLE
                '                If Send_CMD(Car(car_idx).device_no, 1, chargerlist(change_i)) Then
                '                    Exit For
                '                End If
                '            End If
                '        Next

                '    End If
                'End If
                ''電池1  BMS硬體異常
                'If Car(car_idx).BMS1(17) >= 64 Then
                '    Car(car_idx).To_AGV(20) = 30016 + InttoBitidx(Car(car_idx).BMS1(17))
                'End If

                ''電池2 狀態異常
                'If Car(car_idx).BMS2(16) > 0 Then
                '    If (BmsAlertIdx And Car(car_idx).BMS2(16)) > 0 Then
                '        Car(car_idx).To_AGV(20) = 30100 + InttoBitidx(Car(car_idx).BMS2(16))
                '    End If
                'End If
                ''電池2 BMS硬體異常
                'If Car(car_idx).BMS2(17) >= 64 Then
                '    Car(car_idx).To_AGV(20) = 30116 + InttoBitidx(Car(car_idx).BMS2(17))
                'End If


                'If Car(car_idx).get_auto = 0 And Car(car_idx).get_Err = 0 And Car(car_idx).get_tagId > 1 And Car(car_idx).status >= 0 Then

                '    Dim bms1check, bms2check As Integer
                '    bms1check = Car(car_idx).CheckBms(Car(car_idx).BMS1, Car(car_idx).BMSAlarm1)
                '    bms2check = Car(car_idx).CheckBms(Car(car_idx).BMS2, Car(car_idx).BMSAlarm2)
                '    '要有心跳才偵測異常

                '    '電池一 上位偵測異常
                '    If Car(car_idx).BMSAlarm1(17) = 0 Then
                '        If bms1check > 0 Then
                '            If (BmsAlertIdx And bms1check) > 0 Then
                '                Car(car_idx).To_AGV(20) = 30032 + InttoBitidx(bms1check)
                '                settext("AGV" + Car(car_idx).device_no.ToString + "bms1check" + bms1check.ToString)
                '                settext("AGV" + Car(car_idx).device_no.ToString + "bms:" + int2str(Car(car_idx).BMS1, 0, 53))
                '            End If
                '        End If
                '    End If

                '    '電池二 上位偵測異常
                '    If Car(car_idx).BMSAlarm2(17) = 0 Then
                '        If bms2check > 0 Then
                '            If (BmsAlertIdx And bms1check) > 0 Then
                '                Car(car_idx).To_AGV(20) = 30132 + InttoBitidx(bms2check)
                '                settext("AGV" + Car(car_idx).device_no.ToString + "bms2check" + bms1check.ToString)
                '                settext("AGV" + Car(car_idx).device_no.ToString + "bms:" + int2str(Car(car_idx).BMS2, 0, 53))
                '            End If
                '        End If
                '    End If

                '    If Car(car_idx).BMSAlarm1(17) > 300 Then
                '        Car(car_idx).To_AGV(20) = 30065 '正常連線且心跳異常
                '        settext("AGV" + Car(car_idx).device_no.ToString + "bms1:" + int2str(Car(car_idx).BMS1, 0, 53))

                '    End If
                '    If Car(car_idx).BMSAlarm2(17) > 300 Then
                '        Car(car_idx).To_AGV(20) = 30165 '正常連線且心跳異常
                '        settext("AGV" + Car(car_idx).device_no.ToString + "bms2:" + int2str(Car(car_idx).BMS2, 0, 53))
                '    End If
                'End If
            Else
                For i = 0 To Me.ListView1.Items.Count - 1
                    Try
                        If CInt(Me.ListView1.Items(i).SubItems(2).Text) = 0 And CInt(Me.ListView1.Items(i).SubItems(3).Text) = -2 And CInt(Me.ListView1.Items(i).SubItems(1).Text) = Car(car_idx).device_no Then
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
            cmd_timer_isbusy = False
        End If
        ListView1_ReNew()
        Me.PictureBox1.Invalidate()
        'settext("Cmd_timer:" + (Now.Ticks - timestart).ToString)
    End Sub

    Function InttoBitidx(ByVal IntVal As Integer)
        InttoBitidx = -1
        For code As Integer = 0 To 15
            If (IntVal >> code) = 1 Then
                Return code
            End If
        Next

    End Function

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Car(Cidx(CInt(txtCar.Text))).step_i = 902
    End Sub

    Sub writelog(ByVal str As String, ByVal device_no As Integer)
        Try
            Dim sw As StreamWriter = New StreamWriter(".\log\" + Now().ToString("yyyyMMdd") + "_toPC_" + device_no.ToString + ".log", True, Encoding.Default)
            sw.Write(Now.ToString("HH:mm:ss fff") + " " + str + vbCrLf)
            sw.Flush()
            sw.Close()
        Catch ex As Exception

        End Try

    End Sub


    Sub writedoorlog(ByVal str As String)
        Try
            Dim sw As StreamWriter = New StreamWriter(Now().ToString(".\log\" + "yyyyMMdd") + "_todoorPC.log", True, Encoding.Default)
            sw.Write(Now.ToString("yyyy-MM-dd HH:mm:ss") + ":" + str + vbCrLf)
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

                    Me.Log_txt.AppendText(logout + vbCrLf)

                    Log_txt_cnt += 1
                    If Log_txt_cnt > 100 Then
                        Log_txt_cnt = 1
                        Me.Log_txt.Text = ""
                    End If
                    log.WriteLine(Now.ToString("HH:mm:ss fff") + " " + logout)
                Else

                    Me.Log_txt.Text = logout + vbCrLf



                    log.WriteLine(Now.ToString("HH:mm:ss") + " " + logout)
                End If

                log.Flush()
            End If
        Catch ex As Exception

        End Try
    End Sub

    Dim commlog As StreamWriter
    Dim commlog_filename As String = ""
    Dim commLog_txt_cnt As Integer = 0
    Sub setCommtext(ByVal logout As String, Optional ByVal append As Boolean = True, Optional ByVal car_no As Integer = 0)

        If Not logout = "" Then

            Try

                Dim file_str As String = ".\log\" + Now.ToString("yyyyMMdd") + "_comm.log"
                If commlog_filename = "" Then
                    commlog_filename = file_str
                    commlog = New StreamWriter(commlog_filename, True, Encoding.Default)
                ElseIf Not file_str = commlog_filename Then
                    commlog.Flush()
                    commlog.Close()
                    commlog_filename = file_str
                    commlog = New StreamWriter(commlog_filename, True, Encoding.Default)
                End If

                If Me.CommTxt.InvokeRequired Then
                    Dim d As New settextcallback(AddressOf setCommtext)
                    Me.Invoke(d, New Object() {logout, append})
                ElseIf append = True Then

                    Me.CommTxt.AppendText(logout + vbCrLf)

                    commLog_txt_cnt += 1
                    If commLog_txt_cnt > 299 Then
                        commLog_txt_cnt = 0
                        Me.CommTxt.Text = ""
                    End If
                    commlog.WriteLine(Now.ToString("HH:mm:ss") + " " + logout)
                Else

                    Me.CommTxt.Text = logout + vbCrLf



                    commlog.WriteLine(Now.ToString("HH:mm:ss") + " " + logout)
                End If

                commlog.Flush()

            Catch ex As Exception

            End Try
        End If

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
            Query = "SELECT CmdKey,AGVno,CmdFrom,CmdTo,Pri_Wt,CMD_Status,date_format(RequestTime,'%Y-%m-%d %H:%i:%s'),Requestor,Shelf_Car_No,Shelf_Car_type,Shelf_Car_Size,RollData,ext_cmd,McsCmdKey  "
            Query += "FROM `agv_cmd_list`   where 1 order by Pri_Wt DESC,RequestTime ASC "
            'ListView1.Items.Clear()
            sqlCommand.CommandText = Query
            mReader = sqlCommand.ExecuteReader()
            Dim keylist(500) As String
            Dim key_idx As Integer = 0
            While (mReader.Read)
                Dim idx As Integer = -1
                Dim item As New ListViewItem()
                keylist(key_idx) = mReader.Item(0).ToString
                key_idx += 1
                item.Text = mReader.Item(0)
                For i = 1 To 13
                    item.SubItems.Add(mReader.Item(i).ToString)
                Next

                For i = 0 To ListView1.Items.Count - 1
                    If ListView1.Items(i).SubItems(0).Text = mReader.Item(0).ToString Then
                        idx = i
                        Exit For
                    End If
                Next
                If idx >= 0 Then
                    '修改
                    Dim flag As Boolean = False

                    For i = 0 To item.SubItems.Count - 1
                        If Not item.SubItems(i).Text = ListView1.Items(idx).SubItems(i).Text Then
                            ListView1.Items(idx).SubItems(i).Text = item.SubItems(i).Text
                        End If
                    Next
                Else

                    ListView1.Items.Add(item)
                End If
            End While
            For i = 0 To ListView1.Items.Count - 1

                If Array.IndexOf(keylist, ListView1.Items(i).SubItems(0).Text) < 0 Then
                    ListView1.Items(i).Remove()
                    Exit For
                End If
            Next
            ListView1.ListViewItemSorter = New ListViewItemComparer(4)
            mReader.Close()
        Catch ex As Exception
            settext("List renew Mysql 異常")
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
        For i As Integer = 0 To 33
            If Not Car(idx).device_status(i) = Car(idx).Pre_device_status(i) Then
                Car(idx).Pre_device_status(i) = Car(idx).device_status(i)
                flag = True
            End If
        Next
        If flag Then
            writelog(int2str(Car(idx).device_status, 0, 50), Car(idx).device_no)
        End If
    End Sub


    Private Sub Button9_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button9.Click
        ' Car(Cidx(CInt(txtCar.Text))).cmd_idx = -2
        Car(Cidx(CInt(txtCar.Text))).subcmd = Car(Cidx(CInt(txtCar.Text))).get_tagId
        Car(Cidx(CInt(txtCar.Text))).To_AGV(5) = 0
        Car(Cidx(CInt(txtCar.Text))).To_AGV(6) = 0

        Car(Cidx(CInt(txtCar.Text))).To_AGV(20) = 0
        Car(Cidx(CInt(txtCar.Text))).step_i = 905
        Car(Cidx(CInt(txtCar.Text))).path_error_count = 0
        Car(Cidx(CInt(txtCar.Text))).path_error_tagid = 0
        Car(Cidx(CInt(txtCar.Text))).Pre_TagID_time = Now()
        For i As Integer = 0 To 17
            Car(Cidx(CInt(txtCar.Text))).BMSAlarm1(i) = 0
            Car(Cidx(CInt(txtCar.Text))).BMSAlarm2(i) = 0
        Next
        testval = 0
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
        Query = "SELECT A.From_Point,A.To_Point,A.Forward_Sensor,A.Forward_Speed,A.backward_Sensor,A.backward_Speed,B.X, B.Y, C.X, C.Y,A.Priwt"
        Query += " FROM `path` A"
        Query += " LEFT JOIN `point` B ON A.`From_Point` = B.Tag_ID"
        Query += " LEFT JOIN `point` C ON A.`To_Point` = C.Tag_ID where   A.active=1 "
        sqlCommand.CommandText = Query
        mReader = sqlCommand.ExecuteReader()
        i = 0
        ReDim path_base(5000)
        Dim action As String = ""
        Dim idx As Integer = 0
        Dim priwt As Double = 1
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
                priwt = CDbl(mReader.Item(10))
                path_base(i).distance = Round((((path_base(i).X1 - path_base(i).X2) ^ 2 + (path_base(i).Y1 - path_base(i).Y2) ^ 2) ^ 0.5) / priwt, 0)
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
        ReDim path_S(1000)
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

        Dijkstra_list(2).name = "ALL"
        Dijkstra_list(2).CarType = "8"
        i = 3
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
        Dim priwt As Double = 1
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()

        sqlCommand.Connection = oConn
        Dim temp As String = path_type
        If path_type = "ALL" Then
            temp = ""
        End If
        Query = "SELECT A.From_Point,A.To_Point,A.Forward_Sensor,A.Forward_Speed,A.backward_Sensor,A.backward_Speed,B.X, B.Y, C.X, C.Y ,A.FORK_BACK,A.priwt	"
        Query += " FROM `path` A"
        Query += " LEFT JOIN `point` B ON A.`From_Point` = B.Tag_ID"
        Query += " LEFT JOIN `point` C ON A.`To_Point` = C.Tag_ID where A.path_type like '%" + temp + "%' and  A.active=1"


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
            If path_type = "ALL" Then
                path(i).speed0 = 20
                path(i).speed1 = 20
            End If

            path(i).X1 = CInt(mReader.Item(6)) ' 
            path(i).Y1 = CInt(mReader.Item(7)) ' 
            path(i).X2 = CInt(mReader.Item(8)) ' 
            path(i).Y2 = CInt(mReader.Item(9)) ' 
            priwt = CDbl(mReader.Item(11))
            path(i).distance = Round((((path(i).X1 - path(i).X2) ^ 2 + (path(i).Y1 - path(i).Y2) ^ 2) ^ 0.5) / priwt, 0)
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
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn

        Query = "SELECT A.From_Point,A.To_Point,A.Forward_Sensor,A.Forward_Speed,A.backward_Sensor,A.backward_Speed,B.X, B.Y, C.X, C.Y ,A.FORK_BACK	"
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
                    flag = 1
                    path_fork_base_idx = ii
                    Exit For
                ElseIf (path_fork_base(ii).From_Point = ToPoint And path_fork_base(ii).To_Point = FromPoint) Then
                    flag = 2
                    path_fork_base_idx = ii
                    Exit For
                End If
            Next
            If flag = 1 Then
                If CInt(mReader.Item(3)) > 0 Then '速度超過0才覆蓋
                    action = mReader.Item(2)
                    If IsNumeric(action.Substring(1, 1)) Then
                        idx = 1
                    Else
                        idx = 2
                    End If
                    path_fork_base(path_fork_base_idx).action0 = action.Substring(0, idx)  '
                    path_fork_base(path_fork_base_idx).Sensor0 = action.Substring(idx)


                    path_fork_base(path_fork_base_idx).speed0 = CInt(mReader.Item(3)) '
                End If
                If CInt(mReader.Item(5)) > 0 Then '速度超過0才覆蓋
                    action = mReader.Item(4)
                    If IsNumeric(action.Substring(1, 1)) Then
                        idx = 1
                    Else
                        idx = 2
                    End If
                    path_fork_base(path_fork_base_idx).action1 = action.Substring(0, idx)  '
                    path_fork_base(path_fork_base_idx).Sensor1 = action.Substring(idx)

                    path_fork_base(path_fork_base_idx).speed1 = CInt(mReader.Item(5))
                End If
            ElseIf flag = 2 Then
                '後退符合
                If CInt(mReader.Item(5)) > 0 Then
                    action = mReader.Item(4)
                    If IsNumeric(action.Substring(1, 1)) Then
                        idx = 1
                    Else
                        idx = 2
                    End If
                    path_fork_base(path_fork_base_idx).action0 = action.Substring(0, idx)  '
                    path_fork_base(path_fork_base_idx).Sensor0 = action.Substring(idx)
                    path_fork_base(path_fork_base_idx).speed0 = CInt(mReader.Item(5)) '
                End If

                If CInt(mReader.Item(3)) > 0 Then
                    action = mReader.Item(2)
                    If IsNumeric(action.Substring(1, 1)) Then
                        idx = 1
                    Else
                        idx = 2
                    End If
                    path_fork_base(path_fork_base_idx).action1 = action.Substring(0, idx)  '
                    path_fork_base(path_fork_base_idx).Sensor1 = action.Substring(idx)
                    path_fork_base(path_fork_base_idx).speed1 = CInt(mReader.Item(3))
                End If


            Else
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

        Query = "SELECT A.From_Point,A.To_Point,A.Forward_Sensor,A.Forward_Speed,A.backward_Sensor,A.backward_Speed,B.X, B.Y, C.X, C.Y ,A.FORK_BACK	"
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

    Private Sub Button20_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SendBtn.Click
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim ans As Integer = 0
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn

        Try
            'If Not rolldateTxt.Text = "" Then
            Dim Query As String = "INSERT INTO `agv_cmd_list` ( AGVNo,`CmdFrom`, `CmdTo`, `Pri_Wt`,RequestTime, `Requestor`,RollData,ext_cmd) VALUES ('" + txtCar.Text + "','" + Me.From_cb.Text + "', '" + Me.To_cb.Text + "', '50',now(), 'AGVC','','" + rolldateTxt.Text + "');"
            sqlCommand.CommandText = Query
            ans = sqlCommand.ExecuteNonQuery()
            'ElseIf Not ext_cmd_txt.Text = "" Then
            '    Dim Query As String = "INSERT INTO `agv_cmd_list` ( AGVNo,`CmdFrom`, `CmdTo`, `Pri_Wt`,RequestTime, `Requestor`,ext_cmd) VALUES ('" + txtCar.Text + "','" + Me.From_cb.Text + "', '" + Me.To_cb.Text + "', '50',now(), 'AGVC','" + ext_cmd_txt.Text + "');"
            '    sqlCommand.CommandText = Query
            '    ans = sqlCommand.ExecuteNonQuery()
            'ElseIf (CInt(Me.From_cb.Text)) < 10 Then
            '    Dim Query As String = "INSERT INTO `agv_cmd_list` ( AGVNo,`CmdFrom`, `CmdTo`, `Pri_Wt`,RequestTime, `Requestor`) VALUES ('" + txtCar.Text + "','" + Me.From_cb.Text + "', '" + Me.To_cb.Text + "', '50',now(), 'AGVC');"
            '    sqlCommand.CommandText = Query
            '    ans = sqlCommand.ExecuteNonQuery()

            'Else
            '    Dim Query As String = "insert into  `agv_cmd_list`(`AGVNo`,`CmdFrom`,`CmdTo`,`Pri_Wt`,`Requestor`,`Shelf_Car_No`,`Shelf_Car_type`,`Shelf_Car_Size`) select '" + txtCar.Text + "' as AGVNo,'" + Me.From_cb.Text + "','" + Me.To_cb.Text + "',50,'AGVC',Shelf_Car_No,`Shelf_Car_type`,`Shelf_Car_Size` from `shelf_car` where LOCATION='" + Me.From_cb.Text + "' "
            '    sqlCommand.CommandText = Query
            '    ans = sqlCommand.ExecuteNonQuery()
            '  End If

        Catch ex As Exception
            MsgBox("派貨失敗" + ex.Message)
        End Try
        If (Not ans = 1) Then
            MsgBox("派貨失敗")
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
    Dim view_shelf_idx As Integer = 0
    Dim view_charger_idx As Integer = 0
    Private Sub PictureBox1_MouseClick(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBox1.MouseClick
        Dim shelf_size As Integer = 55
        For i As Integer = 0 To Car.Length - 1
            If e.X > CInt(AGVratio * Car(i).AXIS_X) - map_offset_X - 10 And e.X < CInt(AGVratio * Car(i).AXIS_X) - map_offset_X + 10 And e.Y > CInt(AGVratio * Car(i).AXIS_Y) - map_offset_Y - 10 And e.Y < CInt(AGVratio * Car(i).AXIS_Y) - map_offset_Y + 20 Then

                view_car_idx = i
                Button16.Text = "SetPoint"
                txtCar.SelectedIndex = i
                showType = 1
                agv_info.Show()

                Me.pic_close.Show()
                txtCar.Show()
                From_cb.Show()
                To_cb.Show()
                rolldateTxt.Show()
                SendBtn.Show()
                From_cb_TextChanged(sender, e)
                agv_info.Invalidate()
                Exit Sub
            End If
        Next
        For i As Integer = 0 To comQGWrapper.EqPort.Length - 1
            If e.X > CInt(AGVratio * comQGWrapper.EqPort(i).AXIS_X) - map_offset_X And e.X < CInt(AGVratio * comQGWrapper.EqPort(i).AXIS_X) - map_offset_X + CInt(AGVratio * shelf_size) And e.Y > CInt(AGVratio * comQGWrapper.EqPort(i).AXIS_Y) - map_offset_Y And e.Y < CInt(AGVratio * comQGWrapper.EqPort(i).AXIS_Y) - map_offset_Y + CInt(AGVratio * shelf_size) Then
                view_eq_idx = i
                showType = 2
                agv_info.Show()
                Me.pic_close.Show()
                rolldateTxt.Text = ""
                'From_cb.Text = comQGWrapper.EqPort(i).tag_id
                From_cb_TextChanged(sender, e)
                Dim idx As Integer = -1
                idx = comQGWrapper.CST_SearchByLoc(comQGWrapper.EqPort(i).PortID)
                If idx > -1 Then
                    From_cb.Text = comQGWrapper.EqPort(i).tag_id
                Else
                    To_cb.Text = comQGWrapper.EqPort(i).tag_id
                End If

                'Button16.Text = "Lock"
                'txtCar.Hide()
                ' From_cb.Hide()
                'To_cb.Hide()
                'rolldateTxt.Hide()

                'SendBtn.Hide()
                agv_info.Invalidate()
                Exit Sub

            End If
        Next
        For i As Integer = 0 To comQGWrapper.ShelfData.Length - 1
            If e.X > CInt(AGVratio * comQGWrapper.ShelfData(i).AXIS_X) - map_offset_X And e.X < CInt(AGVratio * comQGWrapper.ShelfData(i).AXIS_X) - map_offset_X + CInt(AGVratio * shelf_size) And e.Y > CInt(AGVratio * comQGWrapper.ShelfData(i).AXIS_Y) - map_offset_Y And e.Y < CInt(AGVratio * comQGWrapper.ShelfData(i).AXIS_Y) + CInt(AGVratio * shelf_size) - map_offset_Y Then
                view_shelf_idx = i
                showType = 3
                agv_info.Show()
                Me.pic_close.Show()
                rolldateTxt.Text = ""
                Dim idx As Integer = -1
                idx = comQGWrapper.CST_SearchByLoc(comQGWrapper.ShelfData(i).Shelf_Loc)
                If idx > -1 Then
                    From_cb.Text = comQGWrapper.ShelfData(i).tag_id
                Else
                    To_cb.Text = comQGWrapper.ShelfData(i).tag_id
                End If
                From_cb_TextChanged(sender, e)
                If comQGWrapper.ShelfData(i).Shelf_Status = "X" Then
                    Button16.Text = "Unlock"
                Else
                    Button16.Text = "Lock"
                End If
                ' txtCar.Hide()
                ' From_cb.Hide()
                ' To_cb.Hide()
                ' SendBtn.Hide()
                ' rolldateTxt.Hide()
                agv_info.Invalidate()
                Exit Sub
            End If
        Next
        For i As Integer = 0 To ChargerClient.Length - 1
            If e.X > CInt(AGVratio * ChargerClient(i).AXIS_X) - map_offset_X And e.X < CInt(AGVratio * ChargerClient(i).AXIS_X) + CInt(AGVratio * 150) - map_offset_X And e.Y > CInt(AGVratio * ChargerClient(i).AXIS_Y) - map_offset_Y And e.Y < CInt(AGVratio * ChargerClient(i).AXIS_Y) + CInt(AGVratio * 150) - map_offset_Y Then
                view_charger_idx = i
                showType = 4
                agv_info.Show()
                Me.pic_close.Show()
                rolldateTxt.Text = ""

                agv_info.Invalidate()
                Button16.Text = "Reset"
                Exit Sub

            End If
        Next

    End Sub

    Dim cst_img As Image = Image.FromFile("CST.png")
    Dim agv_img As Image = Image.FromFile("AGV.png")

    Dim AgvErr As Image = Image.FromFile("AGV_ERR.png")
    Dim AgvOffline As Image = Image.FromFile("AGV_Offline.png")
    Dim AgvManual As Image = Image.FromFile("AGV_Manual.png")
    Dim AgvRun As Image = Image.FromFile("AGV_RUN.png")
    Dim AgvIdle As Image = Image.FromFile("AGV_Idle.png")
    Dim Agvaction As Image = Image.FromFile("AGV_ACTION.png")
    Dim ChargerImg As Image = Image.FromFile("charger.png")

    Private Sub PictureBox1_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles PictureBox1.Paint
        Dim g As Graphics = e.Graphics
        Dim p As Pen = New Pen(Color.FromArgb(0, 78, 97), 2)
        Dim r As Pen = New Pen(Color.Red)
        Dim g_pen As Pen = New Pen(Color.Green, 2)
        Dim g_b As SolidBrush = New SolidBrush(Color.CadetBlue)
        'Dim G_Pen As Brush = New Brush()
        Dim drawFormat As New StringFormat
        Dim shelf_size As Integer = 50

        offset_x = map_offset_X
        Try
            ' offset_y = CInt(Floor_Map.Text) * AGVratio
            offset_y = map_offset_Y
        Catch ex As Exception
            offset_y = 0

        End Try
        Dim rect As New Rectangle(10, 10, 30, 30)
        Dim toLoc(ListView1.Items.Count - 1) As String
        For i As Integer = 0 To ListView1.Items.Count - 1
            toLoc(i) = ListView1.Items(i).SubItems(3).Text
        Next






        drawFormat.Alignment = StringAlignment.Center
        For i As Integer = 0 To path_base.Length - 1
            g.DrawLine(p, CInt(path_base(i).X1 * AGVratio) - offset_x, CInt(path_base(i).Y1 * AGVratio) - offset_y, CInt(path_base(i).X2 * AGVratio) - offset_x, CInt(path_base(i).Y2 * AGVratio) - offset_y)
            ' dfh()
        Next

        For j As Integer = 0 To car_no - 1
            Try

                If (Not Car(j).subcmd = "") Then
                    'Car(j).subcmd = Car(j).subcmd.Remove(0, Car(j).subcmd.IndexOf(Car(j).get_tagId.ToString + ","))
                    Dim subcmd_list() As String = Car(j).subcmd.Split(",")
                    For i As Integer = 0 To subcmd_list.Length - 1
                        If CInt(subcmd_list(i)) > 10000 Then
                            subcmd_list(i) = (CInt(subcmd_list(i)) Mod 10000).ToString
                        End If
                    Next


                    For i As Integer = 0 To path_base.Length - 1
                        For k As Integer = 0 To subcmd_list.Length - 2
                            If path_base(i).From_Point = CInt(subcmd_list(k)) And path_base(i).To_Point = CInt(subcmd_list(k + 1)) Then
                                g.DrawLine(g_pen, CInt(path_base(i).X1 * AGVratio) - offset_x, CInt(path_base(i).Y1 * AGVratio) - offset_y, CInt(path_base(i).X2 * AGVratio) - offset_x, CInt(path_base(i).Y2 * AGVratio) - offset_y)
                            ElseIf path_base(i).To_Point = CInt(subcmd_list(k)) And path_base(i).From_Point = CInt(subcmd_list(k + 1)) Then
                                g.DrawLine(r, CInt(path_base(i).X1 * AGVratio) - offset_x, CInt(path_base(i).Y1 * AGVratio) - offset_y, CInt(path_base(i).X2 * AGVratio) - offset_x, CInt(path_base(i).Y2 * AGVratio) - offset_y)
                            End If
                        Next

                    Next

                    For i As Integer = 0 To subcmd_list.Length - 1
                        Dim idx As Integer = Tag_Point_ByTagid(Tag_point_list, CInt(subcmd_list(i)))
                        Dim idx2 As Integer
                        If i = subcmd_list.Length - 1 Then
                            idx2 = idx
                        Else
                            idx2 = Tag_Point_ByTagid(Tag_point_list, CInt(subcmd_list(i + 1)))
                        End If

                        If idx > -1 And idx2 > -1 Then
                            If Tag_point_list(idx).th = Tag_point_list(idx2).th Then
                                If Tag_point_list(idx).th = 0 Then
                                    rect = New Rectangle(CInt((Tag_point_list(idx).X - Car(j).width / 2) * AGVratio) - offset_x, CInt((Tag_point_list(idx).Y - Car(j).height / 2) * AGVratio) - offset_y, CInt(AGVratio * Car(j).width), CInt(AGVratio * Car(j).height))
                                Else
                                    rect = New Rectangle(CInt((Tag_point_list(idx).X - Car(j).height / 2) * AGVratio) - offset_x, CInt((Tag_point_list(idx).Y - Car(j).width / 2) * AGVratio) - offset_y, CInt(AGVratio * Car(j).height), CInt(AGVratio * Car(j).width))
                                End If
                            Else
                                Dim MaxVal As Integer = Math.Max(Car(j).width, Car(j).height) - 70
                                rect = New Rectangle(CInt((Tag_point_list(idx).X - MaxVal / 2) * AGVratio) - offset_x, CInt((Tag_point_list(idx).Y - MaxVal / 2) * AGVratio) - offset_y, CInt(AGVratio * MaxVal), CInt(AGVratio * MaxVal))
                            End If
                            If j = 0 Then
                                e.Graphics.DrawRectangle(New Pen(Color.Red, 1), rect)
                            ElseIf j = 1 Then
                                e.Graphics.DrawRectangle(New Pen(Color.Orange, 1), rect)
                            ElseIf j = 2 Then
                                e.Graphics.DrawRectangle(New Pen(Color.Yellow, 1), rect)
                            ElseIf j = 3 Then
                                e.Graphics.DrawRectangle(New Pen(Color.Green, 1), rect)
                            ElseIf j = 4 Then
                                e.Graphics.DrawRectangle(New Pen(Color.Blue, 1), rect)
                            ElseIf j = 5 Then
                                e.Graphics.DrawRectangle(New Pen(Color.Indigo, 1), rect)
                            ElseIf j = 6 Then
                                e.Graphics.DrawRectangle(New Pen(Color.Purple, 1), rect)
                            Else
                                e.Graphics.DrawRectangle(New Pen(Color.Green, 1), rect)
                            End If

                        End If
                    Next


                End If
            Catch ex As Exception
                settext(ex.Message)
            End Try

        Next



        For i As Integer = 0 To Tag_point_list.Length - 1

            If Array.IndexOf(AllBlockPointList, Tag_point_list(i).TagId.ToString) > -1 Then
                rect = New Rectangle(CInt(Tag_point_list(i).X * AGVratio) - 5 - offset_x, CInt(AGVratio * Tag_point_list(i).Y) - 5 - offset_y, 10, 10)
                e.Graphics.FillEllipse(New SolidBrush(Color.Red), rect)
                e.Graphics.FillRectangle(New SolidBrush(Color.White), CInt(AGVratio * Tag_point_list(i).X) - 3 - offset_x, CInt(AGVratio * Tag_point_list(i).Y) - 2 - offset_y, 6, 3)
            ElseIf In_String(Car(view_car_idx).Block_Point, Tag_point_list(i).TagId.ToString) Then
                rect = New Rectangle(CInt(Tag_point_list(i).X * AGVratio) - 5 - offset_x, CInt(AGVratio * Tag_point_list(i).Y) - 5 - offset_y, 10, 10)
                e.Graphics.FillEllipse(New SolidBrush(Color.Orange), rect)
                e.Graphics.FillRectangle(New SolidBrush(Color.White), CInt(AGVratio * Tag_point_list(i).X) - 3 - offset_x, CInt(AGVratio * Tag_point_list(i).Y) - 2 - offset_y, 6, 3)
            ElseIf CheckBox3.Checked = True Then

            Else

                g.DrawString(Tag_point_list(i).TagId.ToString, New Font("Gill Sans MT", 8), Brushes.Black, CInt(AGVratio * Tag_point_list(i).X) - offset_x, CInt(AGVratio * Tag_point_list(i).Y) - offset_y)
            End If

        Next
        'shelf 
        For i As Integer = 0 To comQGWrapper.ShelfData.Length - 1
            rect = New Rectangle(CInt(comQGWrapper.ShelfData(i).AXIS_X * AGVratio) - offset_x, CInt(comQGWrapper.ShelfData(i).AXIS_Y * AGVratio) - offset_y, CInt(AGVratio * shelf_size), CInt(AGVratio * shelf_size))


            If comQGWrapper.ShelfData(i).Shelf_Status = "X" Then
                e.Graphics.FillRectangle(New SolidBrush(Color.Brown), rect)
            ElseIf Array.IndexOf(toLoc, comQGWrapper.ShelfData(i).tag_id.ToString) > -1 Then
                e.Graphics.FillRectangle(New SolidBrush(Color.FromArgb(0, 255, 0)), rect)
            ElseIf comQGWrapper.ShelfData(i).Shelf_Status = "B" Then
            e.Graphics.FillRectangle(New SolidBrush(Color.Orange), rect)
            ElseIf comQGWrapper.ShelfData(i).Zone_Name.EndsWith("04") Then
            e.Graphics.FillRectangle(New SolidBrush(Color.WhiteSmoke), rect)
            Else
            e.Graphics.FillRectangle(New SolidBrush(Color.LightCyan), rect)
            End If

            e.Graphics.DrawRectangle(New Pen(Color.Black, 1), rect)
            If Not comQGWrapper.ShelfData(i).CarrierID = "" Then
                Dim idx As Integer = -1

                idx = comQGWrapper.CST_SearchByLoc(comQGWrapper.ShelfData(i).Shelf_Loc)
                If idx > -1 Then
                    g.DrawImage(cst_img, CInt(comQGWrapper.ShelfData(i).AXIS_X * AGVratio) - offset_x, CInt(comQGWrapper.ShelfData(i).AXIS_Y * AGVratio) - offset_y, CInt(AGVratio * shelf_size), CInt(AGVratio * shelf_size))

                    If comQGWrapper.CST(idx).CarrierID.StartsWith("UNKNOWN") Then
                        g.DrawString("?", New Font("Tahoma", 10), Brushes.Red, CInt(comQGWrapper.ShelfData(i).AXIS_X * AGVratio) - offset_x, CInt(comQGWrapper.ShelfData(i).AXIS_Y * AGVratio) - offset_y)
                    End If
                Else
                    'comQGWrapper.ShelfData(i).CarrierID = ""
                End If


            End If

        Next
        For i As Integer = 0 To lablelist.Length - 1
            e.Graphics.DrawString(lablelist(i).Txt, New Font("Gill Sans MT", CInt(AGVratio * lablelist(i).isize)), Brushes.Black, CInt(AGVratio * (lablelist(i).X + 25)) - offset_x, CInt(AGVratio * (lablelist(i).Y - 50)) - offset_y)


        Next

        For i As Integer = 0 To comQGWrapper.EqPort.Length - 1
            rect = New Rectangle(CInt(comQGWrapper.EqPort(i).AXIS_X * AGVratio) - offset_x, CInt(comQGWrapper.EqPort(i).AXIS_Y * AGVratio) - offset_y, CInt(AGVratio * shelf_size), CInt(AGVratio * shelf_size))
            If comQGWrapper.EqPort(i).LoadAvail = 0 Then
                e.Graphics.FillRectangle(New SolidBrush(Color.Green), rect)
            ElseIf comQGWrapper.EqPort(i).UnLoadAvail = 0 Then
                e.Graphics.FillRectangle(New SolidBrush(Color.MediumBlue), rect)
            ElseIf comQGWrapper.EqPort(i).ONLINE = 1 Then
                e.Graphics.FillRectangle(New SolidBrush(Color.LightGreen), rect)
            ElseIf comQGWrapper.EqPort(i).ERR = 1 Then
                e.Graphics.FillRectangle(New SolidBrush(Color.Red), rect)
            Else
                e.Graphics.FillRectangle(New SolidBrush(Color.Gray), rect)
            End If


            e.Graphics.DrawRectangle(New Pen(Color.Black, 1), rect)



            Dim idx As Integer = comQGWrapper.CST_SearchByLoc(comQGWrapper.EqPort(i).PortID)

            If idx >= 0 Then
                If comQGWrapper.CST(idx).CarrierLoc = comQGWrapper.EqPort(i).PortID Then
                    g.DrawImage(cst_img, CInt(AGVratio * comQGWrapper.EqPort(i).AXIS_X) - offset_x, CInt(AGVratio * comQGWrapper.EqPort(i).AXIS_Y) - offset_y, CInt(AGVratio * shelf_size), CInt(AGVratio * shelf_size))
                End If

            End If


        Next

        For i As Integer = 0 To ChargerClient.Length - 1

            g.DrawImage(ChargerImg, CInt(ChargerClient(i).AXIS_X * AGVratio) - offset_x, CInt(ChargerClient(i).AXIS_Y * AGVratio) - offset_y, CInt(AGVratio * 150), CInt(AGVratio * 150))
        Next
        For i As Integer = 0 To car_no - 1
            ' If Car(i).flag = True Then
            'If Car(i).get_tagId() > 0 Then
            'If Car(i).device_status(23) > 0 Then
            If Car(i).device_status(23) = 0 And Car(i).device_status(24) = 0 Then
                For j As Integer = 0 To Tag_point_list.Length - 1
                    If Car(i).get_tagId = Tag_point_list(j).TagId Then
                        Car(i).AXIS_X = Tag_point_list(j).X
                        Car(i).AXIS_Y = Tag_point_list(j).Y
                        Car(i).AXIS_Z = Tag_point_list(j).th
                    End If
                Next

            Else
  
                Dim offsetTh As Integer = 0
                'ReverseXY 
                'bit0(反轉XY)
                'bit1 反轉X
                'bit2 反轉Y
                '
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
                ElseIf (Car(i).ReverseXY = 2) Then
                    '用真實數字
                    Car(i).AXIS_X = (Car(i).device_status(23) + Car(i).offset_X)
                    Car(i).AXIS_Y = Car(i).device_status(24) + Car(i).offset_Y
                ElseIf (Car(i).ReverseXY = 3) Then
                    '反向X ，Y真實
                    If Car(i).offset_X >= 0 Then
                        Car(i).AXIS_X = (Car(i).device_status(23) + Car(i).offset_X)
                    Else
                        Car(i).AXIS_X = -Car(i).offset_X - Car(i).device_status(23)
                    End If
                    Car(i).AXIS_Y = Car(i).device_status(24) + Car(i).offset_Y
                Else
                    'XY反轉
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
            End If

                If Car(i).device_status(25) > 65535 / 2 Then
                    '  Car(i).AXIS_Z = -1 * (Car(i).device_status(25) - 65535) / 100 + offsetTh
                Else
                    'Car(i).AXIS_Z = -1 * Car(i).device_status(25) / 100 + offsetTh
                End If
            End If

            Dim agv As Image

            If (Car(i).flag = False Or Car(i).status = -2) Then

                agv = AgvOffline.Clone
            ElseIf Car(i).status = 3 Then
                '手動
                agv = AgvManual.Clone
            ElseIf Car(i).device_status(6) = 0 And (Car(i).status = 2 Or Car(i).status = 0) Then
                agv = AgvIdle.Clone
            ElseIf Car(i).status = 5 Or Car(i).get_isbusy > 0 Then
                agv = Agvaction.Clone
            ElseIf Car(i).status = 4 Or Car(i).status = 1 Or Car(i).status = 4 Or Car(i).status = 12 Or Car(i).status = 8 Then
                agv = AgvRun.Clone
            ElseIf (Car(i).status = -1) Then
                agv = AgvErr.Clone
            Else
                agv = AgvOffline.Clone
            End If
            e.Graphics.TranslateTransform(CInt(AGVratio * Car(i).AXIS_X) - offset_x, CInt(AGVratio * Car(i).AXIS_Y) - offset_y)
            e.Graphics.RotateTransform(-Car(i).AXIS_Z)
            'agv.Rotatelip(System.Drawing.RotateFlipType.Rotate90FlipNone)

            e.Graphics.DrawImage(agv, CInt(AGVratio * -1 * Car(i).width / 2), CInt(AGVratio * -1 * Car(i).height / 2), CInt(AGVratio * Car(i).width), CInt(AGVratio * Car(i).height))

            e.Graphics.RotateTransform(Car(i).AXIS_Z)
            If Car(i).get_loading = 3 Or Not Car(i).RollData = "" Then
                ' g.DrawImage(cst_img, CInt(AGVratio * -1 * Car(i).width / 2), CInt(AGVratio * -1 * Car(i).height / 2))
                g.DrawImage(cst_img, CInt(AGVratio * -1 * cst_img.Width / 2), CInt(AGVratio * -1 * cst_img.Height))
            End If
            If Car(i).Lock_user = "" Then
                e.Graphics.DrawString(Car(i).device_no.ToString, New Font("Tahoma", CInt(AGVratio * 32)), Brushes.Red, CInt(AGVratio * (-15)), CInt(AGVratio * (-10)))
            Else
                e.Graphics.DrawString(Car(i).device_no.ToString + " Lock", New Font("Tahoma", CInt(AGVratio * 32)), Brushes.Red, CInt(AGVratio * (-15)), CInt(AGVratio * (-10)))
            End If


            e.Graphics.TranslateTransform(-CInt(AGVratio * Car(i).AXIS_X) + offset_x, -CInt(AGVratio * Car(i).AXIS_Y) + offset_y)

            Dim X(3) As Integer
            Dim Y(3) As Integer
            Dim minX2, maxX2, minY2, maxY2 As Integer

            X(0) = (CInt(AGVratio * Car(i).width) / 2) * Math.Cos(Car(i).AXIS_Z * Math.PI / 180) + (CInt(AGVratio * Car(i).height) / 2) * Math.Sin(Car(i).AXIS_Z * Math.PI / 180) + CInt(AGVratio * Car(i).AXIS_X)
            Y(0) = -(CInt(AGVratio * Car(i).width) / 2) * Math.Sin(Car(i).AXIS_Z * Math.PI / 180) + (CInt(AGVratio * Car(i).height) / 2) * Math.Cos(Car(i).AXIS_Z * Math.PI / 180) + CInt(AGVratio * Car(i).AXIS_Y)
            X(1) = (CInt(AGVratio * Car(i).width) / 2) * Math.Cos(Car(i).AXIS_Z * Math.PI / 180) + (-CInt(AGVratio * Car(i).height) / 2) * Math.Sin(Car(i).AXIS_Z * Math.PI / 180) + CInt(AGVratio * Car(i).AXIS_X)
            Y(1) = -(CInt(AGVratio * Car(i).width) / 2) * Math.Sin(Car(i).AXIS_Z * Math.PI / 180) + (-CInt(AGVratio * Car(i).height) / 2) * Math.Cos(Car(i).AXIS_Z * Math.PI / 180) + CInt(AGVratio * Car(i).AXIS_Y)
            X(3) = (-CInt(AGVratio * Car(i).width) / 2) * Math.Cos(Car(i).AXIS_Z * Math.PI / 180) + (CInt(AGVratio * Car(i).height) / 2) * Math.Sin(Car(i).AXIS_Z * Math.PI / 180) + CInt(AGVratio * Car(i).AXIS_X)
            Y(3) = -(-CInt(AGVratio * Car(i).width) / 2) * Math.Sin(Car(i).AXIS_Z * Math.PI / 180) + (CInt(AGVratio * Car(i).height) / 2) * Math.Cos(Car(i).AXIS_Z * Math.PI / 180) + CInt(AGVratio * Car(i).AXIS_Y)
            X(2) = (-CInt(AGVratio * Car(i).width) / 2) * Math.Cos(Car(i).AXIS_Z * Math.PI / 180) + (-CInt(AGVratio * Car(i).height) / 2) * Math.Sin(Car(i).AXIS_Z * Math.PI / 180) + CInt(AGVratio * Car(i).AXIS_X)
            Y(2) = -(-CInt(AGVratio * Car(i).width) / 2) * Math.Sin(Car(i).AXIS_Z * Math.PI / 180) + (-CInt(AGVratio * Car(i).height) / 2) * Math.Cos(Car(i).AXIS_Z * Math.PI / 180) + CInt(AGVratio * Car(i).AXIS_Y)
            Array.Sort(X)
            Array.Sort(Y)
            minX2 = X(0)
            maxX2 = X(3)
            minY2 = Y(0)
            maxY2 = Y(3)
            Dim car_width As Integer = maxX2 - minX2
            Dim car_height As Integer = maxY2 - minY2
            rect = New Rectangle(minX2 - offset_x, minY2 - offset_y, car_width, car_height)
            e.Graphics.DrawRectangle(New Pen(Color.Blue, 1), rect)
            ' If Car(i).get_c Then
            '   e.Graphics.DrawArc(New Pen(Color.Blue, 2S), CInt(AGVratio * (Car(i).AXIS_X - Car(i).width / 2)), CInt(AGVratio * (Car(i).AXIS_Y - Car(i).height)) - 5, CInt(AGVratio * Car(i).width), CInt(AGVratio * Car(i).width), 0, -1 * (Car(i).get_SOC / 100 * 360))
        Next
    End Sub




    Private Sub Shelf_Car_Pic0_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs)

    End Sub






    Private Sub Car1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
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
    Private Sub door_check_DoWork(ByVal sender As System.Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles door_check.DoWork
        For j As Integer = 0 To Door_List.Length - 1
            'If Door_List(j).flag = True And Door_List(j).Connect_flag = False Then
            '    'door.flag = True
            '    If Door_List(j).connect() Then
            '        settext(Door_List(j).EQ_ID + ":door OK")
            '    Else
            '        settext(Door_List(j).EQ_ID + ":Door Connect NG")
            '    End If
            'End If



            If Door_List(j).flag = True Then
                If Door_List(j).retry > 10 Then
                    settext(Door_List(j).EQ_ID + "重新連線")
                    If Door_List(j).connect() = False Then
                        settext(Door_List(j).EQ_ID + "重新連線失敗")
                    End If
                End If
                ' Door_List(j).Write_DO()
                Door_List(j).Read_DI()
                Thread.Sleep(100)
                Door_List(j).Read_DO()
                Thread.Sleep(100)

                For i As Integer = 0 To car_no - 1
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



    Private Sub Car2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 2
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try

    End Sub

    Private Sub Car3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 3
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 4
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try

    End Sub

    Private Sub Car5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 5
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try

    End Sub

    Private Sub Car6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 6
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub


    Private Sub Car0_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
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



    Private Sub Button11_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs)
        For i As Integer = 0 To car_no - 1
            Car(i).online = True
        Next
        Button7.Text = "ONLINE"
    End Sub

    Private Sub Button8_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        For i As Integer = 0 To car_no - 1
            Car(i).online = False
        Next
        Button7.Text = "OFFLINE"
    End Sub
    Function Cidx(ByVal device_no As Integer) As Integer
        Cidx = 0
        For i As Integer = 0 To car_no - 1
            If Car(i).device_no = device_no Then
                Cidx = i
            End If
        Next
        Return Cidx
    End Function

    Private Sub Button16_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub




    Private Sub Floor_Map_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Me.PictureBox1.Invalidate()
    End Sub




    Private Sub Car7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 7
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try

    End Sub

    Private Sub Car8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 8
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try

    End Sub

    Private Sub Car9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 9
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 10
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
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
                    Query = "update lft set Door_Open=" + LFT_List(i).open_sensor.ToString + ",floor_no=" + floor_no.ToString + " where EQ_ID='" + LFT_List(i).EQ_ID + "'"
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
            sw.Write(Now.ToString("yyyy-MM-dd HH:mm:ss") + ":" + str + vbCrLf)
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


                For i As Integer = 0 To car_no - 1
                    If (select_LFT(LFT_List(j).tagid, Car(i).subcmd) Or select_LFT(LFT_List(j).tagid, Car(i).get_tagId.ToString)) And Car(i).get_auto = 0 And (LFT_List(j).control_flag = i Or LFT_List(j).control_flag = -1) Then
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
                        writeLFTlog(LFT_List(j).tagid.ToString + ",STEP:" + LFT_List(j).LFT_STEP_I.ToString + ",control_flag:" + LFT_List(j).control_flag.ToString + ",DoWork")
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

    End Sub
    Function Query_Floor(ByVal tagid As Integer)
        Query_Floor = 0
        tagid = tagid Mod 10000
        For i As Integer = 0 To Tag_point_list.Length - 1
            If tagid = Tag_point_list(i).TagId Then
                Query_Floor = Tag_point_list(i).floor_no
            End If
        Next
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
                'Car(car_idx).To_AGV(1) = Car(car_idx).To_pos
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
                            If Car(car_idx).Car_type = "FORK" And Car(car_idx).get_loading() = 3 Then
                                Car(car_idx).subcmd = Dijkstra_fn_ary(Dijkstra_list(ii).ary, Tag_ID_Fork_List, Car(car_idx).get_tagId, cmd_to_point.ToString, Car(car_idx).Block_Point + "," + AllBlockPoint, Car(car_idx).Block_Path)
                            ElseIf Car(car_idx).Car_type = "FORK" Then
                                Car(car_idx).subcmd = Dijkstra_fn_ary(Dijkstra_list(1).ary, Tag_ID_Fork_List, Car(car_idx).get_tagId, cmd_to_point.ToString, Car(car_idx).Block_Point + "," + AllBlockPoint, Car(car_idx).Block_Path)
                            Else
                                Car(car_idx).subcmd = Dijkstra_fn_ary(Dijkstra_list(ii).ary, Tag_ID_List, Car(car_idx).get_tagId, cmd_to_point.ToString, Car(car_idx).Block_Point + "," + AllBlockPoint, Car(car_idx).Block_Path)
                            End If
                        End If
                    Next
                Else
                    '空車用原始路徑
                    If Car(car_idx).Car_type = "FORK" Then
                        Car(car_idx).subcmd = Dijkstra_fn_ary(Dijkstra_list(1).ary, Tag_ID_Fork_List, Car(car_idx).get_tagId, cmd_to_point.ToString, Car(car_idx).Block_Point + "," + AllBlockPoint, Car(car_idx).Block_Path)
                    Else
                        Car(car_idx).subcmd = Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, Car(car_idx).get_tagId, cmd_to_point.ToString, Car(car_idx).Block_Point + "," + AllBlockPoint, Car(car_idx).Block_Path)
                    End If

                End If


                If Not Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString() Then
                    'Start

                    Dim car_subcmd As String = ""

                    Car(car_idx).main_subcmd = Car(car_idx).subcmd
                    settext(Car(car_idx).device_no.ToString + ":main_subcmd" + Car(car_idx).main_subcmd, True, Car(car_idx).device_no)

                    '快速逆向
                    Car(car_idx).fast_subcmd = "" 'Dijkstra_fn_ary(Dijkstra_list(2).ary, Tag_ID_List, Car(car_idx).get_tagId, cmd_to_point.ToString, Car(car_idx).Block_Point + "," + AllBlockPoint, Car(car_idx).Block_Path)
                    '先取15個，判斷是否重疊 舊路徑

                    settext(Car(car_idx).device_no.ToString + ":fast_subcmd= " + Car(car_idx).fast_subcmd, True, Car(car_idx).device_no)


                    For ii As Integer = 0 To car_no - 1
                        'Dim fast_subcmd As String = ""
                        If (Not ii = car_idx) Then
                            If Car(ii).subcmd = "" Then
                                Car(ii).subcmd = Car(ii).get_tagId().ToString
                            End If
                            car_subcmd = Get_group_path(Car(ii).subcmd) '包含原本的subcmd
                            Car(car_idx).fast_subcmd = Check_Path(Car(car_idx).fast_subcmd, Car(ii).subcmd, Car(car_idx).width, Car(car_idx).height, Tag_point_list, Car(ii).AXIS_X, Car(ii).AXIS_Y, Car(ii).AXIS_Z)
                            Car(car_idx).fast_subcmd = Check_Path_group(Car(car_idx).fast_subcmd, car_subcmd)
                            settext(Car(ii).device_no.ToString + ":判fastcmd=" + Car(car_idx).fast_subcmd, True, Car(car_idx).device_no)
                        End If
                    Next

                    Car(car_idx).fast_subcmd = short_path(Car(car_idx).main_subcmd, Car(car_idx).fast_subcmd, Car(car_idx).MaxPath + 3)
                    If Not Car(car_idx).fast_subcmd = "" Then
                        Car(car_idx).subcmd = Car(car_idx).fast_subcmd               
                    End If
                '判斷條件後就 之就使用最短路徑

                For k As Integer = 0 To car_no - 1
                    Dim pre_subcmd As String = Car(car_idx).subcmd
                    If Not k = car_idx And Car(k).device_no > 0 Then
                        If Car(k).subcmd = "" Then
                            Car(k).subcmd = Car(k).get_tagId.ToString
                        End If
                        car_subcmd = Get_group_path(Car(k).subcmd) '包含原本的subcmd
                            settext(Car(car_idx).device_no.ToString + ":check_path" + Car(car_idx).subcmd + "X" + Car(k).device_no.ToString + ":" + car_subcmd)
                        Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, Car(k).subcmd, Car(car_idx).width, Car(car_idx).height, Tag_point_list, Car(k).AXIS_X, Car(k).AXIS_Y, Car(k).AXIS_Z) ' 使用車子面積來截斷

                        '使用Group來截斷
                        settext(Car(car_idx).device_no.ToString + ":" + Car(car_idx).subcmd)
                        Car(car_idx).subcmd = Check_Path_group(Car(car_idx).subcmd, car_subcmd)
                    End If
                    Dim ans As Integer
                    ' ans = Send_CMD(Car(k).device_no, 0, 240)
                    Dim cmd_cnt As Integer = 0

                    '預先趕車
                    Dim cmdlen As Integer = Car(car_idx).subcmd.Split(",").Length
                    If Not pre_subcmd = Car(car_idx).subcmd And Car(k).cmd_idx = -2 And Car(k).Lock_user = "" And cmdlen < 20 Then
                        For iii As Integer = 0 To ListView1.Items.Count - 1
                            If CInt(ListView1.Items(iii).SubItems(1).Text) = Car(k).device_no Then
                                cmd_cnt += 1
                            End If
                        Next
                        If Car(k).wait_point > 0 And (Car(k).get_status = 0 Or Car(k).get_status = 2) And Car(k).get_auto = 0 And cmd_cnt = 0 And DateDiff(DateInterval.Second, CDate(Car(k).Pre_TagID_time), CDate(Now())) > 6 Then

                            Dim to_point As String = Car(k).wait_point.ToString
                                settext(Car(car_idx).device_no.ToString + "提早趕車pre_subcmd" + pre_subcmd + ";" + "subcmd" + Car(car_idx).subcmd + ";", True, Car(car_idx).device_no)
                            If Car(k).get_tagId < 1999 Then
                                to_point = "2008,2030,2036,2056"

                            ElseIf Car(k).get_tagId >= 2000 And Car(k).get_tagId < 2999 Then
                                to_point = "1003,1013,1016,1022"
                            ElseIf Car(k).get_tagId >= 3000 And Car(k).get_tagId < 3999 Then
                                If Car(k).device_no > 2 Then
                                    to_point = "4019,2036,2056"
                                Else
                                    to_point = "4019,4031"
                                End If


                            ElseIf Car(k).get_tagId >= 4000 And Car(k).get_tagId < 4999 Then
                                If Car(k).device_no > 2 Then
                                    to_point = "1003,1013,1016,1022,2036,2056"
                                Else
                                    to_point = "3026"
                                End If
                            Else
                                to_point = "2008,2023,2036,2043,2055"
                                to_point += ",1008,1013,1018,1022,1032"
                                to_point += ",4016,4019,4028"
                                to_point += ",3011,3014,3025"
                            End If
                                Dim temp_point As String = ""
                                If Car(k).device_no > 3 Then
                                    Query = "SELECT  group_concat(Tag_ID) "
                                    Query += " from (SELECT A.Tag_ID,( A.X - B.X ) * ( A.X - B.X ) + ( A.Y - B.Y ) * ( A.Y - B.Y ) AS dist "
                                    Query += " FROM `point` A , `point` B  where (A.Tag_ID in (" + to_point + ") or A.Retreat_Flag=1 ) and not A.Tag_ID  =4028 and B.Tag_ID=" + Car(car_idx).get_tagId.ToString
                                    Query += "  and A.floor_no=B.floor_no and not A.`Tag_ID` in (select CmdTo FROM `agv_cmd_list` )  and not A.`Tag_ID`  in (select Position FROM `agv_list` )"

                                    Query += " order by dist ASC limit 0,10 ) C"
                                    sqlCommand.CommandText = Query
                                Else
                                    Query = "SELECT  group_concat(Tag_ID) "
                                    Query += " from (SELECT A.Tag_ID,( A.X - B.X ) * ( A.X - B.X ) + ( A.Y - B.Y ) * ( A.Y - B.Y ) AS dist "
                                    Query += " FROM `point` A , `point` B  where (A.Tag_ID in (" + to_point + ") or A.Retreat_Flag=1 ) and B.Tag_ID=" + Car(car_idx).get_tagId.ToString
                                    Query += "  and A.floor_no=B.floor_no and not A.`Tag_ID` in (select CmdTo FROM `agv_cmd_list` )  and not A.`Tag_ID`  in (select Position FROM `agv_list` )"

                                    Query += " order by dist ASC limit 0,10 ) C"
                                    sqlCommand.CommandText = Query
                                End If

                            to_point = sqlCommand.ExecuteScalar.ToString

                                Dim cmd As String = Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, Car(k).get_tagId(), to_point, AllBlockPoint + "," + Car(k).Block_Point + "," + Car(car_idx).subcmd.ToString, Car(k).Block_Path)
                                Application.DoEvents()
                            Dim cmd_list() As String = cmd.Split(",")
                            If cmd_list.Length > 14 Then
                                to_point = CInt(cmd_list(13))
                            Else
                                to_point = cmd_list(cmd_list.Length - 1)
                            End If
                            If Not to_point = "" Then
                                ans = Send_CMD(Car(k).device_no, 1, to_point, "Retreat" + Car(car_idx).device_no.ToString + Car(k).device_no.ToString)
                                cmd_cnt += 1
                                settext(Car(car_idx).device_no.ToString + "趕車->" + to_point, True, Car(car_idx).device_no)
                            End If

                        End If
                    End If
                    '----------------------------------------------

                    If Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString Then '判斷被k截斷了
                        Dim temp As String = Cut_before_Path(Car(k).main_subcmd, Car(k).get_tagId.ToString)
                            'settext(Car(car_idx).device_no.ToString + ":main_cmd=" + Car(car_idx).main_subcmd, True, Car(car_idx).device_no)
                        settext(Car(car_idx).device_no.ToString + ":目的地被" + Car(k).device_no.ToString + "擋住:" + Car(k).subcmd + "(" + Car(k).get_tagId.ToString + ")grouppath=" + car_subcmd, True, Car(car_idx).device_no)
                        settext(Car(car_idx).subcmd + "," + Car(k).subcmd + "," + Car(car_idx).width.ToString + "," + Car(car_idx).height.ToString + "," + Car(k).AXIS_X.ToString + "," + Car(k).AXIS_Y.ToString + "," + Car(k).AXIS_Z.ToString)
                        settext(Car(car_idx).device_no.ToString + " Group:" + Car(car_idx).subcmd + " car_subcmd:" + car_subcmd)
                        Car(car_idx).subcmd_req = Car(k).device_no.ToString
                            If Car(k).subcmd = "" Or Car(k).subcmd = Car(k).get_tagId.ToString Or (In_Subcmd(temp, Car(car_idx).get_tagId().ToString) And Car(k).status = 4) Then 'k車也沒有動作 要排除 或是前車取放電梯與滑升門 不然等待滑升門的時候會亂跑
                                '判斷為對峙或是前方車輛無法通行
                                Dim LftFlag As Boolean = CheckCarInLft(Car(k).get_tagId(), Car(k).main_subcmd)
                                Dim DoorFlag As Boolean = CheckCarInDoor(Car(k).get_tagId())
                                If In_Subcmd(temp, Car(car_idx).get_tagId().ToString) Then
                                    settext(Car(car_idx).device_no.ToString + "(" + Car(car_idx).get_tagId().ToString + "):" + "k車:" + Car(k).main_subcmd + " autotemp:" + temp)
                                End If
                                settext("k車:" + Car(k).get_info)

                                settext(Car(car_idx).device_no.ToString + ":" + "判斷前車無動作", True, Car(car_idx).device_no)
                                If Car(k).get_action > 0 And Car(k).get_auto = 0 And Car(k).get_Err = 0 Then
                                    settext(Car(car_idx).device_no.ToString + ":" + "前車在取放", True, Car(car_idx).device_no)
                                ElseIf Car(k).get_tagId = Car(k).Cmd_From And Car(k).Pre_TagID_time > Now.AddSeconds(-90) And Car(k).cmd_idx > -2 Then
                                    settext(Car(car_idx).device_no.ToString + ":" + "前車到來源端90秒內" + Car(k).Pre_TagID_time.ToString, True, Car(car_idx).device_no)
                                ElseIf Car(k).get_tagId = Car(k).Cmd_To And Car(k).Pre_TagID_time > Now.AddSeconds(-90) And Car(k).cmd_idx > -2 Then
                                    settext(Car(car_idx).device_no.ToString + ":" + "前車到目的端90秒內" + Car(k).Pre_TagID_time.ToString, True, Car(car_idx).device_no)
                                ElseIf LftFlag Then
                                    settext(Car(car_idx).device_no.ToString + ":" + "前車在電梯程序", True, Car(car_idx).device_no)
                                    '乖乖停好步要動
                                ElseIf DoorFlag Then
                                    settext(Car(car_idx).device_no.ToString + ":" + "前車在開門程序", True, Car(car_idx).device_no)
                                    '乖乖停好步要動
                                    'ElseIf DoorFlag Then
                                    '判斷前車是命令剛結束，準備下一道
                                Else
                                    settext(Car(car_idx).device_no.ToString + ":" + "進行退避", True, Car(car_idx).device_no)
                                    If Car(k).cmd_sql_idx = 0 And Car(k).flag = True And (Car(k).status = 0 Or Car(k).status = 2) And Car(k).Lock_user = "" Then
                                        For iii As Integer = 0 To ListView1.Items.Count - 1
                                            If CInt(ListView1.Items(iii).SubItems(1).Text) = Car(k).device_no Then
                                                cmd_cnt += 1
                                            End If
                                        Next
                                        '前車無命令，送出新的命令
                                        If Car(k).wait_point > 0 And (Car(k).get_status = 0 Or Car(k).get_status = 2) And Car(k).get_auto = 0 And cmd_cnt = 0 Then
                                            Dim to_point As String = Car(k).wait_point.ToString
                                            If Car(k).wait_point = Car(k).get_tagId Then
                                                '如果已經到達
                                                Dim temp_point As String = ""
                                                Query = "SELECT  group_concat(Tag_ID) "
                                                Query += " from (SELECT A.Tag_ID,( A.X - B.X ) * ( A.X - B.X ) + ( A.Y - B.Y ) * ( A.Y - B.Y ) AS dist "
                                                Query += " FROM `point` A , `point` B  where A.Retreat_Flag=1 and B.Tag_ID=" + Car(car_idx).get_tagId.ToString
                                                Query += "  and A.floor_no=B.floor_no and not A.`Tag_ID` in (select CmdTo FROM `agv_cmd_list` )  and not A.`Tag_ID`  in (select Position FROM `agv_list` )"
                                                Query += " order by dist ASC limit 0,10 ) C"
                                                sqlCommand.CommandText = Query
                                                to_point = sqlCommand.ExecuteScalar.ToString

                                            End If
                                            Dim cmd As String = Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, Car(k).get_tagId(), to_point, AllBlockPoint + "," + Car(k).Block_Point, Car(k).Block_Path)
                                            Dim cmd_list() As String = cmd.Split(",")

                                            If cmd_list.Length > 11 Then
                                                to_point = CInt(cmd_list(10))

                                            Else
                                                to_point = cmd_list(cmd_list.Length - 1)

                                            End If
                                            If Not to_point = "" Then
                                                cmd_cnt += 1
                                                ans = Send_CMD(Car(k).device_no, 1, to_point, "Retreat2")
                                            End If

                                        End If

                                    Else
                                        Dim Car_main As String = ""

                                        If Car(car_idx).main_subcmd.Split(",").Length > 1 Then
                                            Car_main = Car(car_idx).main_subcmd.Split(",")(1)
                                        End If

                                        If Car(car_idx).Car_type = "FORK" Then ' FORK 不能退避

                                            Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString  ' Dijkstra_fn_ary(Dijkstra_list(1).ary, Tag_ID_Fork_List, Car(car_idx).get_tagId, cmd_to_point, Car(k).get_tagId.ToString + "," + Car(car_idx).Block_Point)
                                            settext(Car(car_idx).device_no.ToString + ":" + "不能選第二條", True, Car(car_idx).device_no)
                                        ElseIf Car(car_idx).get_pin = 10 And Not Car(car_idx).cmd_Shelf_Car_Type = "" Then
                                            For ii As Integer = 1 To Dijkstra_list.Length - 1
                                                If Car(car_idx).cmd_Shelf_Car_Type = Dijkstra_list(ii).name And Car(car_idx).get_pin = 10 Then
                                                    Car(car_idx).subcmd = Dijkstra_fn_ary(Dijkstra_list(ii).ary, Tag_ID_List, Car(car_idx).get_tagId, cmd_to_point, Car_main + "," + (Car(k).get_tagId Mod 10000).ToString + "," + Car(car_idx).Block_Point + "," + AllBlockPoint, Car(car_idx).Block_Path)
                                                End If
                                            Next
                                        Else
                                            Dim cark As String = GetCarPoint(Car(k).get_tagId, Car(k).AXIS_Z, Car(k).width * 2, Car(k).height)
                                            'Car(car_idx).subcmd = Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, Car(car_idx).get_tagId, cmd_to_point, Car_main + "," + (Car(k).get_tagId Mod 10000).ToString + "," + Car(car_idx).Block_Point + "," + AllBlockPoint, Car(car_idx).Block_Path)
                                            Car(car_idx).subcmd = Dijkstra_fn_ary(Dijkstra_list(2).ary, Tag_ID_List, Car(car_idx).get_tagId, cmd_to_point, Car_main + "," + cark + "," + Car(car_idx).Block_Point + "," + AllBlockPoint, Car(car_idx).Block_Path)
                                        End If


                                        settext(Car(car_idx).device_no.ToString + ":" + "選擇第二路徑:" + Car(car_idx).subcmd, True, Car(car_idx).device_no)

                                        If Not Car(car_idx).subcmd = "" Then
                                            Car(car_idx).sflag = 1
                                        End If


                                        For ii As Integer = 0 To car_no - 1
                                            If (Not ii = car_idx) Then
                                                If Car(ii).subcmd = "" Then
                                                    Car(ii).subcmd = Car(ii).get_tagId().ToString
                                                End If
                                                car_subcmd = Get_group_path(Car(ii).subcmd) '包含原本的subcmd
                                                Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, Car(ii).subcmd, Car(car_idx).width, Car(car_idx).height, Tag_point_list, Car(ii).AXIS_X, Car(ii).AXIS_Y, Car(ii).AXIS_Z)
                                                Car(car_idx).subcmd = Check_Path_group(Car(car_idx).subcmd, car_subcmd)
                                            End If
                                        Next
                                        If Car(car_idx).subcmd.Split(",").Length < 6 And Not Car(car_idx).subcmd = "" Then
                                            settext(Car(car_idx).device_no.ToString + ":" + "第二路徑太短" + Car(car_idx).subcmd, True, Car(car_idx).device_no)
                                            Car(car_idx).subcmd = ""
                                        End If
                                        If Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString Then
                                            '沒有第二條路徑，退到退避點
                                            Dim temp_point As String = ""
                                            Query = "SELECT  group_concat(Tag_ID) "
                                            Query += " from (SELECT A.Tag_ID,( A.X - B.X ) * ( A.X - B.X ) + ( A.Y - B.Y ) * ( A.Y - B.Y ) AS dist "
                                            Query += " FROM `point` A , `point` B  where A.Retreat_Flag=1 and B.Tag_ID=" + Car(car_idx).get_tagId.ToString
                                            Query += "  and A.floor_no=B.floor_no "
                                            Query += "  and not A.Tag_ID = " + Car(car_idx).get_tagId.ToString
                                            Query += " order by dist ASC limit 0,10 ) C"
                                            sqlCommand.CommandText = Query
                                            temp_point = sqlCommand.ExecuteScalar.ToString
                                            car_subcmd = Get_group_path(Car(k).subcmd) '包含原本的subcmd
                                            If Car(car_idx).Car_type = "FORK" Then

                                                Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString  ' Dijkstra_fn_ary(Dijkstra_list(1).ary, Tag_ID_Fork_List, Car(car_idx).get_tagId, cmd_to_point, Car(k).get_tagId.ToString + "," + Car(car_idx).Block_Point)
                                                settext(Car(car_idx).device_no.ToString + ":" + "不能選退避", True, Car(car_idx).device_no)
                                                ' Car(car_idx).subcmd = Dijkstra_fn_ary(Dijkstra_list(1).ary, Tag_ID_Fork_List, Car(car_idx).get_tagId, temp_point, Car(k).get_tagId.ToString + "," + Car(car_idx).Block_Point.ToString)
                                            ElseIf Car(car_idx).get_pin = 10 And Not Car(car_idx).cmd_Shelf_Car_Type = "" Then
                                                For ii As Integer = 1 To Dijkstra_list.Length - 1
                                                    If Car(car_idx).cmd_Shelf_Car_Type = Dijkstra_list(ii).name And Car(car_idx).get_pin = 10 Then
                                                        Car(car_idx).subcmd = Dijkstra_fn_ary(Dijkstra_list(ii).ary, Tag_ID_List, Car(car_idx).get_tagId, temp_point, Car_main + "," + (Car(k).get_tagId Mod 10000).ToString + "," + car_subcmd + "," + Car(car_idx).Block_Point.ToString + "," + AllBlockPoint, Car(car_idx).Block_Path)
                                                    End If
                                                Next
                                            Else
                                                Car(car_idx).subcmd = Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, Car(car_idx).get_tagId, temp_point, Car_main + "," + (Car(k).get_tagId Mod 10000).ToString + "," + car_subcmd + "," + Car(car_idx).Block_Point.ToString + "," + AllBlockPoint, Car(car_idx).Block_Path)
                                            End If

                                            settext(Car(car_idx).device_no.ToString + ":" + "回到退避點" + Car(car_idx).subcmd, True, Car(car_idx).device_no)
                                            Car(car_idx).sflag = 1
                                        End If
                                    End If


                                End If
                            Else
                                'k車還有命令 那就先等待  目前還沒有規劃其他工作
                                '預計可以提早退避
                                If In_Subcmd(Car(k).main_subcmd, Car(car_idx).get_tagId().ToString) Then
                                    '預計可以提早退避 k車的main_subcmd 有包含現在的車
                                    settext(Car(k).device_no.ToString + ":可以提早退避" + Car(k).subcmd, True, Car(car_idx).device_no)
                                End If


                            End If
                        Car(car_idx).main_subcmd = Car(car_idx).subcmd
                        '重新過濾其他車子的路徑
                        For ii As Integer = 0 To car_no - 1
                            If (Not ii = car_idx) Then
                                If Car(ii).subcmd = "" Then
                                    Car(ii).subcmd = Car(ii).get_tagId().ToString
                                End If
                                car_subcmd = Get_group_path(Car(ii).subcmd) '包含原本的subcmd
                                Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, Car(ii).subcmd, Car(car_idx).width, Car(car_idx).height, Tag_point_list, Car(ii).AXIS_X, Car(ii).AXIS_Y, Car(ii).AXIS_Z)
                                Car(car_idx).subcmd = Check_Path_group(Car(car_idx).subcmd, car_subcmd)
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
                '無到達路徑
            End If

                '----------------------------------------------------------
                '處理充電站
                If Not (Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString()) Then


                    For k As Integer = 0 To ChargerClient.Length - 1
                        If ChargerClient(k).HoldingResponse(7) > 0 Then
                            Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, ChargerClient(k).tag_id.ToString, Car(car_idx).width, Car(car_idx).height, Tag_point_list)
                        End If
                    Next
                    If Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString() Then
                        settext(Car(car_idx).device_no.ToString + ":充電電極伸出", True, Car(car_idx).device_no)
                        Car(car_idx).subcmd_req = "等充電電擊縮回"
                    End If
                End If


                If Not (Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString()) Then


                    For k As Integer = 0 To Door_List.Length - 1
                        If Door_List(k).flag Then
                            If (Door_List(k).up_sensor = 1 And Door_List(k).write_down = 0 And Door_List(k).control_flag = car_idx) Or Door_List(k).tagid = Car(car_idx).get_tagId Then
                                ' da()
                            Else

                                Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, Door_List(k).tagid.ToString, Car(car_idx).width, Car(car_idx).height, Tag_point_list)

                            End If
                        End If

                    Next
                    If Car(car_idx).subcmd = "" Or Car(car_idx).subcmd = Car(car_idx).get_tagId.ToString() Then
                        settext(Car(car_idx).device_no.ToString + ":無路徑door", True, Car(car_idx).device_no)
                        Car(car_idx).subcmd_req = "等開門"
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
                                Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, LFT_List(k).tagid.ToString, Car(car_idx).width, Car(car_idx).height, Tag_point_list)
                            Else
                                '電梯被車子預約
                                If LFT_List(k).control_flag = car_idx Then
                                    '預約電梯的車子
                                    If LFT_List(k).open_sensor = Query_Floor(Car(car_idx).get_tagId) Or Car(car_idx).get_tagId = LFT_List(k).tagid Or Car(car_idx).get_tagId = LFT_List(k).tagid + 10000 Then
                                        '相同樓層且開門 1000 不擋
                                    Else
                                        Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, LFT_List(k).tagid.ToString, Car(car_idx).width, Car(car_idx).height, Tag_point_list)
                                    End If
                                    ' Dim debug As Integer = Query_Floor(Car(car_idx).To_pos)

                                    For ii As Integer = 1 To 7
                                        If Query_Floor(Car(car_idx).get_tagId) = ii Then
                                            settext((ii + LFT_List(k).tagid).ToString + "不擋1", True, Car(car_idx).device_no)
                                        ElseIf (Car(car_idx).get_tagId = LFT_List(k).tagid Or Car(car_idx).get_tagId = LFT_List(k).tagid + 10000) And select_to_floor(Car(car_idx).main_subcmd, LFT_List(k).tagid) = LFT_List(k).open_sensor Then
                                            '如果車子在電梯內且開門的樓層是出口的樓層就不擋
                                            settext((ii + LFT_List(k).tagid).ToString + "不擋2", True, Car(car_idx).device_no)
                                        Else
                                            Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, (ii + LFT_List(k).tagid).ToString, Car(car_idx).width, Car(car_idx).height, Tag_point_list)
                                        End If
                                    Next
                                Else
                                    '  Dim lft_path As String = ""
                                    For ii As Integer = LFT_List(k).tagid To LFT_List(k).tagid + 7
                                        Car(car_idx).subcmd = Check_Path(Car(car_idx).subcmd, ii.ToString, Car(car_idx).width, Car(car_idx).height, Tag_point_list)
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

                    If Car(car_idx).subcmd.Split(",").Length > (Car(car_idx).MaxPath + 5) And Car(car_idx).sflag = 0 Then
                        Dim path_temp() As String = Car(car_idx).subcmd.Split(",")
                        Car(car_idx).subcmd = Car(car_idx).subcmd.Remove(Car(car_idx).subcmd.IndexOf("," + path_temp(Car(car_idx).MaxPath - 1) + ",")) ' 移除
                    ElseIf Car(car_idx).subcmd.Split(",").Length > Car(car_idx).RetreatPath And Car(car_idx).sflag = 1 Then
                        Dim path_temp() As String = Car(car_idx).subcmd.Split(",")
                        Dim idx As Integer = Car(car_idx).subcmd.IndexOf("," + path_temp(Car(car_idx).RetreatPath - 1) + ",")
                        If idx > 0 Then
                            Car(car_idx).subcmd = Car(car_idx).subcmd.Remove(Car(car_idx).subcmd.IndexOf("," + path_temp(Car(car_idx).RetreatPath - 1) + ",")) ' 移除
                        Else
                            Car(car_idx).subcmd = ""
                        End If

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
                                If Car(car_idx).Car_type = "FORK" Then
                                    For j As Integer = 0 To path_fork_base.Length - 1
                                        If path_fork_base(j).From_Point = path_ary(i) And path_fork_base(j).To_Point = path_ary(i + 1) And Not path_fork_base(j).speed0 = 0 Then
                                            path_output += "@" + path_ary(i).ToString + ",0," + path_fork_base(j).action0 + (CInt(path_fork_base(j).Sensor0) + sensor_offset).ToString + "," + path_fork_base(j).speed0.ToString
                                            last_direction = 0
                                        ElseIf path_fork_base(j).To_Point = path_ary(i) And path_fork_base(j).From_Point = path_ary(i + 1) And Not path_fork_base(j).speed1 = 0 Then
                                            path_output += "@" + path_ary(i).ToString + ",1," + path_fork_base(j).action1 + (CInt(path_fork_base(j).Sensor1) + sensor_offset).ToString + "," + path_fork_base(j).speed1.ToString
                                            last_direction = 1
                                        End If
                                    Next
                                ElseIf Not Car(car_idx).fast_subcmd = "" Then

                                    For j As Integer = 0 To path_base.Length - 1
                                        If path_base(j).From_Point = path_ary(i) And path_base(j).To_Point = path_ary(i + 1) Then
                                            path_output += "@" + path_ary(i).ToString + ",0," + path_base(j).action0 + (CInt(path_base(j).Sensor0) + sensor_offset).ToString + ",30"
                                            last_direction = 0
                                        ElseIf path_base(j).To_Point = path_ary(i) And path_base(j).From_Point = path_ary(i + 1) Then
                                            path_output += "@" + path_ary(i).ToString + ",1," + path_base(j).action1 + (CInt(path_base(j).Sensor1) + sensor_offset).ToString + ",30"
                                            last_direction = 1
                                        End If
                                    Next

                                Else
                                    For j As Integer = 0 To path_base.Length - 1
                                        If path_base(j).From_Point = path_ary(i) And path_base(j).To_Point = path_ary(i + 1) Then
                                            path_output += "@" + path_ary(i).ToString + ",0," + path_base(j).action0 + (CInt(path_base(j).Sensor0) + sensor_offset).ToString + "," + path_base(j).speed0.ToString
                                            last_direction = 0
                                        ElseIf path_base(j).To_Point = path_ary(i) And path_base(j).From_Point = path_ary(i + 1) Then
                                            path_output += "@" + path_ary(i).ToString + ",1," + path_base(j).action1 + (CInt(path_base(j).Sensor1) + sensor_offset).ToString + "," + path_base(j).speed1.ToString
                                            last_direction = 1
                                        End If
                                    Next

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
                    settext(Car(car_idx).device_no.ToString + ":規劃路徑" + path_output.Substring(1), True, Car(car_idx).device_no)
                    '字串轉成命令點位傳入AGV，並啟動AGV
                    If cmdtype = 11 Then
                        If Not Car(car_idx).subcmd = old_subcmd Then
                            Car(car_idx).cmd2Car(path_output.Substring(1), cmdtype)
                        End If

                    Else
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
    Function Get_Car_path(ByVal subcmd As String, ByVal h As Integer, ByVal w As Integer) As String
        Get_Car_path = subcmd
        If Not subcmd = "" Then
            Dim subcmd_list() As String = subcmd.Split(",")
            For i As Integer = 0 To subcmd_list.Length - 1
                For j As Integer = 0 To Tag_ID_List.Length

                Next
            Next
        End If
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
    Function In_String(ByVal str As String, ByVal val As String, Optional ByVal split As String = ",") As Boolean
        Dim str_list() As String
        str_list = str.Split(split)
        If Array.IndexOf(str_list, val) > -1 Then
            Return True
        End If
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
    Function Send_CMD(ByVal car_no As Integer, ByVal From_Point As Integer, ByVal To_Point As Integer, Optional ByVal user As String = "AGVC", Optional ByVal Pri_Wt As Integer = 50)
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

                Query = "INSERT INTO `agv_cmd_list` ( AGVNo,`CmdFrom`, `CmdTo`, `Pri_Wt`,RequestTime, `Requestor`) VALUES ('" + car_no.ToString + "','" + From_Point.ToString + "', '" + To_Point.ToString + "', " + Pri_Wt.ToString + ",now(), '" + user + "');"
                Query = " insert into agv_cmd_list(AGVNo,CmdFrom,	CmdTo,Pri_Wt,RequestTime,Requestor,RequestName)	 "
                Query += " SELECT A.`AGVNo` , " + From_Point.ToString + " AS from_point, " + To_Point.ToString + "," + Pri_Wt.ToString + ",now(),  '" + user + "', '" + user + "' "
                Query += " FROM `agv_list` A LEFT JOIN agv_cmd_list B ON A.`AGVNo` = B.`AGVNo`"
                Query += " WHERE A.`AGVNo` =" + car_no.ToString
                Query += " and " + To_Point.ToString + " in (select Tag_ID FROM `point` ) limit 0,1 "
                ' Query += " and not " + To_Point.ToString + " in (select Position FROM `agv_list` ) limit 0,1"
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
                    Query = "insert into  `agv_cmd_list`(`AGVNo`,`CmdFrom`,`CmdTo`,`Pri_Wt`,`Requestor`,`Shelf_Car_No`,`Shelf_Car_type`,`Shelf_Car_Size`) select '" + car_no.ToString + "' as AGVNo,'" + From_Point.ToString + "','" + To_Point.ToString + "'," + Pri_Wt.ToString + ",'" + user + "',Shelf_Car_No,`Shelf_Car_type`,`Shelf_Car_Size` from `shelf_car` where LOCATION='" + From_Point.ToString + "' "
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
    Function Send_CMD_CST(ByVal car_no As Integer, ByVal From_Point As Integer, ByVal To_Point As Integer, ByVal CST As String, ByVal mcscmdkey As String, Optional ByVal Pri_Wt As Integer = 50, Optional ByVal user As String = "MCS")
        Send_CMD_CST = 0
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand

        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        Dim Query As String = ""
        ' Dim mysql_data As Object
        Try


            Query = " insert into agv_cmd_list(AGVNo,CmdFrom,CmdTo,RollData,Pri_Wt,RequestTime,Requestor,RequestName,mcscmdkey)	"
            Query += " SELECT A.`AGVNo` , " + From_Point.ToString + " AS from_point, " + To_Point.ToString + ",'" + CST + "','" + Pri_Wt.ToString + "',now(), '" + user + "','" + user + "' ,'" + mcscmdkey + "'"
            Query += " FROM `agv_list` A LEFT JOIN agv_cmd_list B ON A.`AGVNo` = B.`AGVNo`"
            Query += " WHERE A.`AGVNo` =" + car_no.ToString
            Query += " and not " + To_Point.ToString + " in (select CmdTo FROM `agv_cmd_list` ) "
            Query += " and " + To_Point.ToString + " in (select Tag_ID FROM `point` ) "
            Query += "  limit 0,1"
            sqlCommand.CommandText = Query
            'Cmd_status.AppendText("Query=" + Query)
            Send_CMD_CST = sqlCommand.ExecuteNonQuery()
            ListView1_ReNew()

        Catch ex As Exception
            Send_CMD_CST = 0
            'Cmd_status.AppendText("Query=" + Query + ":" + ex.Message)
        End Try

        oConn.Close()
        oConn.Dispose()
    End Function
    Function Install_CMD(ByVal carrier As Object)
        Dim temp As GEM.Carrier = carrier
        Install_CMD = 0
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand

        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        Dim Query As String = ""

        Try


            Query = "INSERT INTO `transfer` (`COMMANDID`, `PRIORITY`, `CARRIERID`, `SOURCE`, `DEST`, `NEXT_DEST`, `CARRIERTYPE`, `EMPTYFLAG`, `PROCESSID`, `ttransferState`,Req_time) "
            Query += " VALUES ('" + temp.CommandID + "', '" + temp.PRIORITY.ToString + "', '" + temp.CarrierID + "', '" + temp.SOURCE + "', '" + temp.DEST + "', '" + temp.NEXTDest + "', '" + temp.CarrierType + "', '" + temp.EmptyCarrier + "', '" + temp.PROCESSID + "', '1',now());"

            sqlCommand.CommandText = Query
            'Cmd_status.AppendText("Query=" + Query)
            Install_CMD = sqlCommand.ExecuteNonQuery()


        Catch ex As Exception
            Install_CMD = 0
            'Cmd_status.AppendText("Query=" + Query + ":" + ex.Message)
        End Try

        oConn.Close()
        oConn.Dispose()
    End Function
    Function update_CMD(ByVal COMMANDID As String, ByVal ttransferState As Integer)
        update_CMD = 0
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand

        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        Dim Query As String = ""

        Try


            Query = "update `transfer` set ttransferState =" + ttransferState.ToString + " where COMMANDID ='" + COMMANDID + "'"
            sqlCommand.CommandText = Query
            'Cmd_status.AppendText("Query=" + Query)
            update_CMD = sqlCommand.ExecuteNonQuery()


        Catch ex As Exception
            update_CMD = 0
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



    



    Private Sub Button16_Click_3(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Car(view_car_idx).flag Then
            Car(view_car_idx).To_AGV(6) = 0
        Else
            MsgBox(view_car_idx.ToString + "號車未啟用")

        End If
    End Sub

 

    Private Sub Car12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 12
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car13_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 13
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 15
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car14_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 14
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub


    Private Sub Car16_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 16
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car17_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 17
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 18
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car19_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 19
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car20_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 20
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car21_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 21
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car22_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 22
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car23_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 23
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car24_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 24
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car25_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 25
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car26_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 26
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car27_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 27
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car28_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 28
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car29_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim temp As Integer = view_car_idx
        Try

            view_car_idx = 29
            txtCar.Text = Car(view_car_idx).device_no.ToString
        Catch ex As Exception
            view_car_idx = temp
            txtCar.Text = Car(view_car_idx).device_no.ToString
        End Try
    End Sub

    Private Sub Car30_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
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

        Dim a As String = ""
        If CInt(From_cb.Text) = 0 Then
            a = Dijkstra_fn_ary(Dijkstra_list(1).ary, Tag_ID_Fork_List, Car(view_car_idx).get_tagId, To_cb.Text, TextBox3.Text)
        ElseIf Not car_type.Text = "" Then
            For i As Integer = 0 To Dijkstra_list.Length - 1
                If Dijkstra_list(i).name = car_type.Text Then
                    a = Dijkstra_fn_ary(Dijkstra_list(i).ary, Tag_ID_Fork_List, CInt(From_cb.Text), To_cb.Text, TextBox3.Text)
                End If
            Next
        Else

            a = Dijkstra_fn_ary(Dijkstra_list(1).ary, Tag_ID_Fork_List, CInt(From_cb.Text), To_cb.Text, TextBox3.Text)
        End If
        MsgBox(a)
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

                    For j As Integer = 0 To path_fork_base.Length - 1

                        If path_fork_base(j).From_Point = path_ary(i) And path_fork_base(j).To_Point = path_ary(i + 1) And Not path_fork_base(j).speed0 = 0 Then
                            path_output += "@" + path_ary(i).ToString + ",0," + path_fork_base(j).action0 + (CInt(path_fork_base(j).Sensor0) + sensor_offset).ToString + "," + path_fork_base(j).speed0.ToString
                            last_direction = 0
                        ElseIf path_fork_base(j).To_Point = path_ary(i) And path_fork_base(j).From_Point = path_ary(i + 1) And Not path_fork_base(j).speed1 = 0 Then
                            path_output += "@" + path_ary(i).ToString + ",1," + path_fork_base(j).action1 + (CInt(path_fork_base(j).Sensor1) + sensor_offset).ToString + "," + path_fork_base(j).speed1.ToString
                            last_direction = 1
                        End If
                    Next
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

        MsgBox(path_output)
        settext(path_output)
    End Sub

    Private Sub CheckBox1_CheckedChanged_3(ByVal sender As System.Object, ByVal e As System.EventArgs)

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

            writer.WriteStartElement("Ratio")
            writer.WriteString(ratio.Text)
            AGVratio = CDbl(ratio.Text)
            writer.WriteEndElement()

            writer.WriteStartElement("McsPort")
            writer.WriteString(McsPortTxt.Text)
            McsPort = CInt(McsPortTxt.Text)
            writer.WriteEndElement()

            writer.WriteStartElement("SOC")
            writer.WriteString(SOCTxt.Text)
            SOC = CInt(SOCTxt.Text)
            writer.WriteEndElement()

            writer.WriteStartElement("MapX")
            writer.WriteString(MapX.Text)
            map_offset_X = CInt(MapX.Text)
            writer.WriteEndElement()

            writer.WriteStartElement("MapY")
            writer.WriteString(MapY.Text)
            map_offset_Y = CInt(MapY.Text)
            writer.WriteEndElement()

            writer.WriteStartElement("BlockPoint")
            writer.WriteString(BlockPoint.Text)
            AllBlockPoint = BlockPoint.Text
            AllBlockPointList = AllBlockPoint.Split(",")
            writer.WriteEndElement()

            For i As Integer = 200 To 249



                If Me.Controls.Find("Err" + i.ToString(), True).Length = 1 Then
                    Dim chb As CheckBox = Me.Controls.Find("Err" + i.ToString(), True)(0)
                    'If chb.Length = 1 Then
                    writer.WriteStartElement("Err" + i.ToString)
                    writer.WriteString(chb.Checked.ToString)
                    For j As Integer = 0 To car_no - 1
                        If chb.Checked = True Then
                            Car(j).warning(i - 200) = j
                        Else
                            Car(j).warning(i - 200) = 0
                        End If
                    Next
                    writer.WriteEndElement()
                End If


                ' End If

            Next



            writer.WriteEndElement()
            writer.WriteEndDocument()
        Catch ex As Exception
            MsgBox("存檔失敗" + ex.Message)
        End Try
        writer.Close()

        For i As Integer = 0 To LFT_List.Length - 1
            LFT_List(i).IR_sensor = Me.IR_check.Checked
        Next
    End Sub

 

    Private Sub Button18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button18.Click

        Dim a As String = ""
        If CInt(From_cb.Text) = 0 Then
            a = Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, Car(view_car_idx).get_tagId, To_cb.Text, TextBox3.Text + "," + AllBlockPoint)
        ElseIf Not car_type.Text = "" Then
            For i As Integer = 0 To Dijkstra_list.Length - 1
                If Dijkstra_list(i).name = car_type.Text Then
                    a = Dijkstra_fn_ary(Dijkstra_list(i).ary, Tag_ID_List, CInt(From_cb.Text), To_cb.Text, TextBox3.Text + "," + AllBlockPoint)
                End If
            Next
        Else
            a = Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, CInt(From_cb.Text), To_cb.Text, TextBox3.Text + "," + AllBlockPoint)
        End If

        Dim path_ary() As String
        path_ary = a.Split(",")
        Dim sensor_offset As Integer = 0
        Dim path_output As String = ""
        Dim last_direction As Integer = 0
        Dim path_ary_Len As Integer = path_ary.Length
        'Dim path_output_list(50) As Integer
        'Dim path_output_list_idx As Integer = 0
        MsgBox(a)
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
                    For j As Integer = 0 To path_base.Length - 1
                        If path_base(j).From_Point = path_ary(i) And path_base(j).To_Point = path_ary(i + 1) And Not path_base(j).speed0 = 0 Then
                            path_output += "@" + path_ary(i).ToString + ",0," + path_base(j).action0 + (CInt(path_base(j).Sensor0) + sensor_offset).ToString + "," + path_base(j).speed0.ToString
                            last_direction = 0
                        ElseIf path_base(j).To_Point = path_ary(i) And path_base(j).From_Point = path_ary(i + 1) And Not path_base(j).speed1 = 0 Then
                            path_output += "@" + path_ary(i).ToString + ",1," + path_base(j).action1 + (CInt(path_base(j).Sensor1) + sensor_offset).ToString + "," + path_base(j).speed1.ToString
                            last_direction = 1
                        End If
                    Next
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

        MsgBox(path_output)
    End Sub



    Private Sub Button18_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button18.Click

    End Sub

    Private Sub Button22_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button22.Click
        LFT_List(CInt(LFT_idx.Text)).reconnect()

    End Sub
    Private Sub ShowSECSIIMessage(ByVal myRawData() As Byte)
        ' Dim myStack(10) As Integer
        ' Dim myStackPtr As Integer
        Dim lOffset As Integer
        Dim lItemNum As Integer
        Dim ItemData As Object = New Object
        Dim lLength As Integer
        Dim lItemType As Integer
        Dim DisplayString As String
        Dim i As Integer
        Dim cnt As Integer = 0
        ' Verify whether the input data is an array or not
        If IsArray(myRawData) = False Or UBound(myRawData) = 0 Then
            Exit Sub
        End If
        Dim showmessage As String = ""
        lLength = UBound(myRawData) + 1
        'myStackPtr = 0
        lOffset = 0

        While lOffset < lLength And cnt < 1000

            cnt += 1
            lItemType = comQSWrapper.GetDataItemType(myRawData, lOffset)

            If lItemType = SECSComm.LIST_TYPE Then
                lItemNum = 99
                lOffset = comQSWrapper.DataItemIn(myRawData, lOffset, SECSComm.LIST_TYPE, lItemNum, Nothing)
                ' Display the data item

                showmessage += "<L[" & lItemNum & "]"
                ' Increase the indent level
                ' myStack(myStackPtr) = lItemNum
                ' myStackPtr = myStackPtr + 1
            ElseIf lItemType = SECSComm.ASCII_TYPE Then
                lOffset = comQSWrapper.DataItemIn(myRawData, lOffset, SECSComm.ASCII_TYPE, lItemNum, ItemData)

                showmessage += "<A[" & lItemNum & "] " & Chr(34) & ItemData & Chr(34) & ">"
            ElseIf lItemType = SECSComm.JIS_TYPE Then
                lOffset = comQSWrapper.DataItemIn(myRawData, lOffset, SECSComm.JIS_TYPE, lItemNum, ItemData)

                showmessage += "<J[" & lItemNum & "] " & Chr(34) & ItemData & Chr(34) & ">"
            ElseIf lItemType = SECSComm.BINARY_TYPE Then
                lOffset = comQSWrapper.DataItemIn(myRawData, lOffset, SECSComm.BINARY_TYPE, lItemNum, ItemData)

                DisplayString = ""
                For i = 0 To lItemNum - 1
                    DisplayString = DisplayString & " 0x" & Hex(ItemData(i))
                Next
                showmessage += ("<B[" & lItemNum & "]" & DisplayString & ">")
            ElseIf lItemType = SECSComm.BOOLEAN_TYPE Then
                lOffset = comQSWrapper.DataItemIn(myRawData, lOffset, SECSComm.BOOLEAN_TYPE, lItemNum, ItemData)

                DisplayString = ""
                For i = 0 To lItemNum - 1
                    DisplayString = DisplayString & " 0x" & Hex(ItemData(i))
                Next
                showmessage += ("<Boolean[" & lItemNum & "]" & DisplayString & ">")
            ElseIf lItemType = SECSComm.UINT_1_TYPE Then
                lOffset = comQSWrapper.DataItemIn(myRawData, lOffset, SECSComm.UINT_1_TYPE, lItemNum, ItemData)

                DisplayString = ""
                For i = 0 To lItemNum - 1
                    DisplayString = DisplayString & " " & ItemData(i)
                Next
                showmessage += ("<U1[" & lItemNum & "]" & DisplayString & ">")
            ElseIf lItemType = SECSComm.UINT_2_TYPE Then
                lOffset = comQSWrapper.DataItemIn(myRawData, lOffset, SECSComm.UINT_2_TYPE, lItemNum, ItemData)

                DisplayString = ""
                For i = 0 To lItemNum - 1
                    DisplayString = DisplayString & " " & ItemData(i)
                Next
                showmessage += ("<U2[" & lItemNum & "]" & DisplayString & ">")
            ElseIf lItemType = SECSComm.UINT_4_TYPE Then
                lOffset = comQSWrapper.DataItemIn(myRawData, lOffset, SECSComm.UINT_4_TYPE, lItemNum, ItemData)

                DisplayString = ""
                For i = 0 To lItemNum - 1
                    DisplayString = DisplayString & " " & ItemData(i)
                Next
                showmessage += ("<U4[" & lItemNum & "]" & DisplayString & ">")
            ElseIf lItemType = SECSComm.INT_1_TYPE Then
                lOffset = comQSWrapper.DataItemIn(myRawData, lOffset, SECSComm.INT_1_TYPE, lItemNum, ItemData)

                DisplayString = ""
                For i = 0 To lItemNum - 1
                    DisplayString = DisplayString & " " & ItemData(i)
                Next
                showmessage += ("<I1[" & lItemNum & "]" & DisplayString & ">")
            ElseIf lItemType = SECSComm.INT_2_TYPE Then
                lOffset = comQSWrapper.DataItemIn(myRawData, lOffset, SECSComm.INT_2_TYPE, lItemNum, ItemData)

                DisplayString = ""
                For i = 0 To lItemNum - 1
                    DisplayString = DisplayString & " " & ItemData(i)
                Next
                showmessage += ("<I2[" & lItemNum & "]" & DisplayString & ">")
            ElseIf lItemType = SECSComm.INT_4_TYPE Then
                lOffset = comQSWrapper.DataItemIn(myRawData, lOffset, SECSComm.INT_4_TYPE, lItemNum, ItemData)

                DisplayString = ""
                For i = 0 To lItemNum - 1
                    DisplayString = DisplayString & " " & ItemData(i)
                Next
                showmessage += ("<I4[" & lItemNum & "]" & DisplayString & ">")
            ElseIf lItemType = SECSComm.FT_4_TYPE Then
                lOffset = comQSWrapper.DataItemIn(myRawData, lOffset, SECSComm.FT_4_TYPE, lItemNum, ItemData)

                DisplayString = ""
                For i = 0 To lItemNum - 1
                    DisplayString = DisplayString & " " & ItemData(i)
                Next
                showmessage += ("<F4[" & lItemNum & "]" & DisplayString & ">")
            ElseIf lItemType = SECSComm.FT_8_TYPE Then
                lOffset = comQSWrapper.DataItemIn(myRawData, lOffset, SECSComm.FT_8_TYPE, lItemNum, ItemData)
                DisplayString = ""
                For i = 0 To lItemNum - 1
                    DisplayString = DisplayString & " " & ItemData(i)
                Next
                showmessage += ("<F8[" & lItemNum & "]" & DisplayString & ">")
            Else
            End If
        End While
        setCommtext(showmessage)
    End Sub


    Private Sub Button24_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        comQGWrapper.UpdateSV(GEM_SC_STATE, GEM.SC_INIT)
    End Sub

    Private Sub Button25_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

    Private Sub Button27_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button27.Click
        comQGWrapper.UpdateSV(GEM_SC_STATE, GEM.SC_AUTO)
        For i As Integer = 0 To car_no - 1
            Car(i).online = True
        Next
        Button7.Text = "ONLINE"
    End Sub

    Private Sub btn_OnLineRemote_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btn_OnLineRemote.Click
        comQGWrapper.UpdateSV(GEM_CONTROL_STATE, GEM.OnlineRemoteval)
        ' start_flag = True
    End Sub
    Dim PAUSING_CNT As Integer = 0
    Dim hartbit As Integer = 0
    Dim pause_cnt As Integer = 0
    Dim pre_cnt As Integer = 0
    Dim loader_flag As Integer = 0
    Private Sub MCS_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MCS.Tick
        Dim lSVID As Integer
        Dim GetFormat As Integer
        Dim Value As Object = New Object
        Dim oConn As MySqlConnection
        Dim idx As Integer = -1
        Dim sqlCommand As New MySqlCommand
        oConn = New MySqlConnection(Mysql_str)
        Dim Query As String = ""
        oConn.Open()
        sqlCommand.Connection = oConn
        'If a.flag = False Then
        '    a.init("192.168.8.70", 502, 1100)
        'End If
        lSVID = GEM_CONTROL_STATE
        comQGWrapper.GetSV(lSVID, GetFormat, Value)
        Me.agv_info.Invalidate()
        Dim cmd_send_flag As Boolean = False
        'g_iGemControlState = Value
        Dim car_zone As Boolean = False
        

        ' Dim car_cnt As Integer = 0
        'For i As Integer = 2 To car_no - 1
        '    Car(i).Site = "TP"
        '    If Car(i).status >= 0 And Car(i).Lock_user = "" And Not Car(i).device_status(6) = 48 Then
        '        car_cnt += 1
        '        If car_zone = False Then
        '            Car(i).Site = "TP,TC"
        '            car_zone = True

        '        End If
        '        '5台車台北車
        '        If car_cnt = 5 Then
        '            Car(i).Site = "TP,TC"
        '        End If
        '    End If
        'Next
        'If car_cnt = 1 Then
        '    For i As Integer = 2 To car_no - 1
        '        Car(i).Site = "TP,TC"
        '    Next
        'End If


        '狀態顯示
        Select Case Value
            Case 1
                Me.lbl_CcontrolStats.BackColor = Color.Red
                Me.btn_OnLineRemote.Enabled = False
                Me.btn_OnlineLocal.Enabled = False

                Me.lbl_CcontrolStats.Text = "1:OffLine (EQP OffLine)"
                Me.btn_OnLine.Enabled = True
                Me.btn_Offline.Enabled = False
            Case 2
                Me.lbl_CcontrolStats.BackColor = Color.Red
                Me.btn_OnLineRemote.Enabled = False
                Me.btn_OnlineLocal.Enabled = False

                Me.lbl_CcontrolStats.Text = "2:OffLine (Attempt OnLine)"
                Me.btn_OnLine.Enabled = False
                Me.btn_Offline.Enabled = False
            Case 3
                Me.lbl_CcontrolStats.BackColor = Color.Red
                Me.btn_OnLineRemote.Enabled = False
                Me.btn_OnlineLocal.Enabled = False

                Me.lbl_CcontrolStats.Text = "3:OffLine (Host OffLine)"
                Me.btn_OnLine.Enabled = False
                Me.btn_Offline.Enabled = True
            Case 4
                Me.lbl_CcontrolStats.Text = "4:OnLine Local"
                Me.lbl_CcontrolStats.BackColor = Color.Yellow
                Me.btn_OnLine.Enabled = False
                Me.btn_Offline.Enabled = True
                Me.btn_OnLineRemote.Enabled = True
                Me.btn_OnlineLocal.Enabled = False
            Case 5
                Me.lbl_CcontrolStats.Text = "5:OnLine Remote"
                Me.lbl_CcontrolStats.BackColor = Color.GreenYellow
                Me.btn_OnLine.Enabled = False
                Me.btn_Offline.Enabled = True
                Me.btn_OnLineRemote.Enabled = False
                Me.btn_OnlineLocal.Enabled = True
                If pause_cnt > 20 Then
                    pause_cnt = 0
                    comQGWrapper.UpdateSV(GEM_SC_STATE, GEM.SC_AUTO)
                    For i As Integer = 0 To car_no - 1
                        Car(i).online = True
                    Next
                    Button7.Text = "ONLINE"
                End If
        End Select
        Dim SC_State As Integer
        'PAUSE 切換
        comQGWrapper.GetSV(GEM_SC_STATE, 1, SC_State)
        If SC_State = GEM.SC_PAUSING Then
            '等待所有命令結束
            '不接受新的命令

            PAUSING_CNT += 1
            If PAUSING_CNT > 5 Then
                SC_State = GEM.SC_PAUSED
                PAUSING_CNT = 100
                comQGWrapper.UpdateSV(GEM_SC_STATE, GEM.SC_PAUSED)
            End If
        End If

        If SC_State = GEM.SC_AUTO Then
            lbl_SC_Stats.Text = "AUTO"
            pause_cnt = 0
        ElseIf SC_State = GEM.SC_PAUSING Then
            lbl_SC_Stats.Text = "AUTO->PAUSE"
        ElseIf SC_State = GEM.SC_PAUSED Then
            lbl_SC_Stats.Text = "PAUSED"
            pause_cnt += 1
        End If

        If ConnectionState = 1 Then
            ConnectionState = 1
            Me.lbl_SECSConnectState.Text = "Connection"
            Me.lbl_SECSConnectState.BackColor = Color.GreenYellow
        Else
            'Ver:4.10
            lbl_SECSConnectState.Text = "Disconnection(Stop)"
            lbl_SECSConnectState.BackColor = Color.Red
        End If

        hartbit += 1
        If hartbit = 15 And ConnectionState = 1 Then
            comQSWrapper.TestLink()
        End If
        'If hartbit > 25 And ConnectionState = 1 Then
        '    Dim a As UInteger = 0
        '    hartbit = 0
        '    comQSWrapper.SendSECSIIMessage(1, 1, 1, a, Nothing, )
        '    setCommtext("system:" + a.ToString + "送出S1F1")

        'End If
        'event 
        Dim CONTROL_STATE As UInt16 = 0
        comQGWrapper.GetSV(GEM_CONTROL_STATE, SECSComm.UINT_2_TYPE, CONTROL_STATE)
        If Not CONTROL_STATE = Pre_ControlState And ConnectionState = 1 Then
            Pre_ControlState = CONTROL_STATE
            If CONTROL_STATE = 1 Then
                'eq offline
                comQGWrapper.EventReportSend(1)
            ElseIf CONTROL_STATE = 2 Then
                'eq Attempt offline
            ElseIf CONTROL_STATE = 3 Then
                'host offline
            ElseIf CONTROL_STATE = 4 Then
                'online local
                comQGWrapper.EventReportSend(2)
            ElseIf CONTROL_STATE = 5 Then
                'online remote
                comQGWrapper.EventReportSend(3)
            End If
        Else
            Pre_ControlState = CONTROL_STATE
        End If
        Dim SCSTATE As UInt16 = 0
        comQGWrapper.GetSV(GEM_SC_STATE, SECSComm.UINT_2_TYPE, SCSTATE)
        If Not SCSTATE = Pre_SCState And ConnectionState = 1 Then
            Pre_SCState = SCSTATE
            If SCSTATE = 0 Then
                'None
                'comQGWrapper.EventReportSend(1)
            ElseIf SCSTATE = 1 Then
                'INT
                comQGWrapper.EventReportSend(54)
            ElseIf SCSTATE = 2 Then
                'PAUSED
                comQGWrapper.EventReportSend(55)
            ElseIf SCSTATE = 3 Then
                'PAUSING->AUTO
                comQGWrapper.EventReportSend(53)
            ElseIf SCSTATE = 4 Then
                ''AUTO->PAUSEING
                comQGWrapper.EventReportSend(57)
                comQGWrapper.EventReportSendOb(160, comQGWrapper.Zone(0).ZoneName + "," + comQGWrapper.Zone(0).ZoneCapacity.ToString + "," + comQGWrapper.Zone(0).ZoneSize.ToString + "," + comQGWrapper.Zone(0).ZoneType.ToString)
                comQGWrapper.EventReportSendOb(160, comQGWrapper.Zone(1).ZoneName + "," + comQGWrapper.Zone(1).ZoneCapacity.ToString + "," + comQGWrapper.Zone(1).ZoneSize.ToString + "," + comQGWrapper.Zone(1).ZoneType.ToString)
            End If
        Else
            Pre_SCState = SCSTATE
        End If

        If Me.EQ_BG.IsBusy = False Then
            If ConnectionState = 1 And start_flag Or 1 Then
                EQ_BG.RunWorkerAsync()
            End If
        End If
        If ChargerClient(0).HoldingResponse(16) = 1 Then
            Del_CMD(1)
            '去充電站    
            Send_CMD(Car(0).device_no, 1, 8197, "Loader", 50)
            Send_CMD(Car(0).device_no, 1, 8195, "Loader", 51)
            loader_flag = 1
        ElseIf ChargerClient(0).HoldingResponse(16) = 2 Then
            '去待命點
            loader_flag = 0
            Del_CMD(1)
            Update_SQL("delete from agv_cmd_list where `AGVNo`=1 ")
            Send_CMD(Car(0).device_no, 4, 4101, "Loader", 60)

        End If

        If loader_flag = 1 Then
            'Loop

            Send_CMD(Car(0).device_no, 1, 8197, "Loader", 50)
            Send_CMD(Car(0).device_no, 1, 8195, "Loader", 51)
        End If




        Dim cnt1 As Integer = 0

        For ii As Integer = 0 To Car.Length - 1
            If Car(ii).status = 0 And Car(ii).flag = True And Car(ii).Lock_user = "" Then


                For i As Integer = 0 To ListView1.Items.Count - 1
                    If ListView1.Items(i).SubItems(1).Text = Car(ii).device_no.ToString Then
                        cnt1 += 1
                    End If
                Next

                If cnt1 = 0 Then
                    If Car(ii).get_loading = 3 Then
                        Dim tagid1 As Integer = Search_EMPTY_Shelf(Car(ii).get_tagId, "", AllBlockPoint + "," + Car(ii).Block_Point)
                        Send_CMD_CST(Car(ii).device_no, 2, tagid1, Car(ii).get_cstid, Now.Ticks.ToString, 49, "AutoOut")
                        settext("send 2 to shelf ")
                    End If
                End If
            End If
        Next

        '-------------------------
        Dim cmdlist(500) As CmdPwt
        Dim CmdPwtLen As Integer = 0
        For i As Integer = 0 To Car.Length - 1
            For j As Integer = 0 To comQGWrapper.CST.Length - 1
                If Car(i).flag And (Car(i).cmd_idx = -2 Or Car(i).device_status(6) = 48) And (Car(i).status = 0 Or Car(i).status = 2 Or Car(i).status = 5) And Car(i).get_loading = 0 And Car(i).Lock_user = "" And Car(i).get_SOC > Car(i).Recharge_SOC Then
                    If Not comQGWrapper.CST(j).DEST = "" And Not comQGWrapper.CST(j).CarrierID = "" Then
                        cmdlist(CmdPwtLen).CarIdx = i
                        cmdlist(CmdPwtLen).CstIdx = j
                        cmdlist(CmdPwtLen).distance = -1 * TagID_Dis(Car(i).get_tagId, comQGWrapper.CST(j).CarrierLoc)
                        If comQGWrapper.CST(j).CarrierLoc Is Nothing Then

                            cmdlist(CmdPwtLen).EQPwt = 0
                        Else

                            If comQGWrapper.CST(j).CarrierLoc.StartsWith("CGL") Then
                                cmdlist(CmdPwtLen).EQPwt = 100000
                            Else
                                cmdlist(CmdPwtLen).EQPwt = 0
                            End If
                        End If
                        cmdlist(CmdPwtLen).Pwt = comQGWrapper.CST(j).PRIORITY
                        cmdlist(CmdPwtLen).mcstime = CLng(Now.ToString("yyyyMMddHHmmss")) - comQGWrapper.CST(j).mcstime
                        cmdlist(CmdPwtLen).total = cmdlist(CmdPwtLen).distance + cmdlist(CmdPwtLen).EQPwt + cmdlist(CmdPwtLen).Pwt + cmdlist(CmdPwtLen).mcstime
                        CmdPwtLen += 1
                    End If
                End If

            Next
        Next
        Array.Resize(cmdlist, CmdPwtLen)

        For i As Integer = 0 To CmdPwtLen - 1

            For j As Integer = i To CmdPwtLen - 1
                If cmdlist(i).total < cmdlist(j).total Then
                    Dim temp As CmdPwt = cmdlist(i)
                    cmdlist(i) = cmdlist(j)
                    cmdlist(j) = temp
                End If
            Next
        Next
        Dim str As String = ""
        For cmdi As Integer = 0 To cmdlist.Length - 1
            Try
                str += Car(cmdlist(cmdi).CarIdx).device_no.ToString + ","
                str += comQGWrapper.CST(cmdlist(cmdi).CstIdx).CarrierID.ToString + ","
                str += cmdlist(cmdi).distance.ToString + ","
                str += cmdlist(cmdi).EQPwt.ToString + ","
                str += cmdlist(cmdi).Pwt.ToString + ","
                str += cmdlist(cmdi).mcstime.ToString + ","
                str += cmdlist(cmdi).total.ToString + vbCrLf
            Catch ex As Exception

            End Try

        Next
        setCommtext(str)
        For cmdi As Integer = 0 To cmdlist.Length - 1
            Dim car_i As Integer = cmdlist(cmdi).CarIdx
            Dim flag1 As Boolean = False
            For j As Integer = 0 To ListView1.Items.Count - 1
                If ListView1.Items(j).SubItems(1).Text = Car(car_i).device_no And (Not ListView1.Items(j).SubItems(2).Text = "4" Or (ListView1.Items(j).SubItems(2).Text = "4" And CInt(Car(car_i).get_SOC) <= (85))) Then
                    flag1 = True
                    Exit For
                End If
            Next
            If flag1 = False Then
                '找到車子 開始找CSTT 
                '先排序 判斷SITE 
                ' Dim car_site_list() As String = Car(car_i).Site.Split(",")
                If cmd_send_flag = True Then
                    Exit For
                End If
                ' Dim list(comQGWrapper.CST.Length - 1) As Integer
                Dim i As Integer = cmdlist(cmdi).CstIdx
                Application.DoEvents()
                '處理傳送事件 待改 按照權重派送  MCS 權重 + time 權重(分) + 自加權重 + 搜尋最高權重 
                '計算距離  AGV -CST 距離
                '命令新增或是修改 未排序
                If Not comQGWrapper.CST(i).CarrierID = "" And ConnectionState = 1 Then
                    'For j As Integer = 0 To comQGWrapper.Zone.Length - 1
                    '    If comQGWrapper.CST(i).CarrierLoc = comQGWrapper.Zone(j).ZoneName Then
                    '        comQGWrapper.Zone(j).CST_count += 1
                    '    End If
                    'Next
                    If comQGWrapper.CST(i).TransferState = GEM.TransferState_Queued Or comQGWrapper.CST(i).TransferState = GEM.TransferState_Transferring Then
                        'TransferState_Queued
                        Dim cmd_flag As Integer = 0
                        For j As Integer = 0 To Car.Length - 1
                            For k As Integer = 0 To Me.ListView1.Items.Count - 1
                                If ListView1.Items(k).SubItems(13).Text.ToString = comQGWrapper.CST(i).CommandID Or ListView1.Items(k).SubItems(11).Text.ToString = comQGWrapper.CST(i).CarrierID Then
                                    cmd_flag = 1
                                    Exit For
                                End If
                            Next
                        Next

                        If cmd_flag = 0 Then
                            '沒有命令 ，判斷是到達目的地或是暫存
                            '  MsgBox("命令結束")
                            setCommtext(comQGWrapper.CST(i).CarrierZoneName + "=" + comQGWrapper.CST(i).DEST)
                            If Not comQGWrapper.CST(i).CarrierZoneName = comQGWrapper.CST(i).DEST Then
                                '開始處理命令
                                '未到達目的地 下命令
                                Dim reason As String = ""
                                Dim NextLoc As String = ""
                                Dim FromPoint, ToPoint As Tag_Point

                                FromPoint = New Tag_Point
                                ToPoint = New Tag_Point
                                For j As Integer = 0 To Tag_point_list.Length - 1
                                    If Tag_point_list(j).LOC = comQGWrapper.CST(i).CarrierLoc And Not comQGWrapper.CST(i).CarrierLoc = "" Then
                                        '判斷是EQ or SHELF
                                        Dim flag As Boolean = False
                                        If Tag_point_list(j).tagtype = 3 Then

                                            For k As Integer = 0 To comQGWrapper.EqPort.Length - 1
                                                '檢查訊號
                                                If comQGWrapper.EqPort(k).UnLoadAvail = 0 And comQGWrapper.EqPort(k).PortID = Tag_point_list(j).LOC Then
                                                    flag = True
                                                End If
                                            Next
                                        Else
                                            For k As Integer = 0 To comQGWrapper.ShelfData.Length - 1
                                                '檢查狀態
                                                If comQGWrapper.ShelfData(k).tag_id = Tag_point_list(j).TagId And comQGWrapper.ShelfData(k).Shelf_Status = "" Then
                                                    flag = True
                                                End If

                                            Next

                                        End If
                                        If flag = True Then
                                            FromPoint = Tag_point_list(j)
                                            comQGWrapper.CST(i).EQ_Retry = 0
                                            Exit For
                                        Else
                                            comQGWrapper.CST(i).EQ_Retry += 1
                                            If comQGWrapper.CST(i).EQ_Retry > 200 Then
                                                reason = "FromNotReady"
                                                Query = "Delete from mcs_cmd_list where  CARRIERID='" + comQGWrapper.CST(i).CarrierID + "' or REQUEST_TIME < '" + Now.AddDays(-1).ToString("yyyy-MM-dd HH:mm:ss") + "'"
                                                settext("刪除MCS Cmd EQ Error:" + Query)
                                                Update_SQL(Query)
                                                Query = "update mcs_cmd_history  set End_time=now() where  COMMANDID='" + comQGWrapper.CST(i).CommandID + "'"
                                                Update_SQL(Query)
                                                comQGWrapper.CST(i).DEST = ""
                                                comQGWrapper.CST(i).PRIORITY = 0
                                                comQGWrapper.CST(i).mcstime = 0
                                                comQGWrapper.CST(i).CommandID = ""
                                                comQGWrapper.CST(i).CarrierState = GEM.CS_STOREDCOMPLETED
                                                comQGWrapper.EventReportSendOb(GEM.EVENT_TransferAbortInitiated, comQGWrapper.CST(i))
                                                comQGWrapper.EventReportSendOb(GEM.EVENT_TransferAbortCompleted, comQGWrapper.CST(i))
                                                For k As Integer = ListView2.Items.Count - 1 To 0 Step -1
                                                    Try
                                                        If ListView2.Items(k).SubItems(0).Text = comQGWrapper.CST(i).CarrierID Then
                                                            ListView2.Items.RemoveAt(k)
                                                        End If
                                                    Catch ex As Exception
                                                    End Try
                                                Next
                                            End If


                                        End If



                                    End If
                                Next
                                '就近找到目的地
                                If comQGWrapper.CST(i).DEST.StartsWith(comQGWrapper.Eqpname.Substring(0, 8)) Then
                                    Dim tagid As Integer = Search_EMPTY_Shelf(FromPoint.TagId, comQGWrapper.CST(i).DEST, AllBlockPoint, Car(0).Block_Path)
                                    If tagid > 0 Then
                                        ToPoint = Tag_point_list(Tag_Point_ByTagid(Tag_point_list, tagid)) '回傳目的地
                                    Else
                                        'setCommtext("from " + FromPoint.TagId.ToString + "找不到符合的目的地")

                                        If FromPoint.TagId = 0 Then
                                            reason = "FromNotReady:From:0"
                                        Else
                                            reason = "from " + FromPoint.TagId.ToString + "找不到符合的目的地"
                                        End If

                                    End If
                                Else
                                    'EQ 
                                    For j As Integer = 0 To Tag_point_list.Length - 1
                                        If Tag_point_list(j).LOC = comQGWrapper.CST(i).DEST And Not comQGWrapper.CST(i).DEST = "" Then
                                            Dim flag As Boolean = False
                                            If Tag_point_list(j).tagtype = 3 Then

                                                For k As Integer = 0 To comQGWrapper.EqPort.Length - 1
                                                    If comQGWrapper.EqPort(k).LoadAvail = 0 And comQGWrapper.EqPort(k).PortID = Tag_point_list(j).LOC Then
                                                        flag = True
                                                    End If
                                                Next
                                            Else
                                                flag = True
                                            End If
                                            '判斷EQ的LEQ
                                            If flag = True Then
                                                ToPoint = Tag_point_list(j)
                                                setCommtext("找符合的EQ" + ToPoint.TagId.ToString())
                                                Exit For
                                            Else
                                                reason = "ToEQNoReady"
                                                comQGWrapper.CST(i).EQ_Retry += 1
                                                setCommtext("ToEQNoReady")
                                            End If

                                        End If
                                    Next
                                End If
                                '先取得 來源與目標的 位置idx                           
                                '先找放置位置
                                '再找要不要跨傳
                                '如果再common 的話 就傳到目的地 
                                '辨識目的
                                '單筆命令，只會有一筆搬送命令
                                '命令有幾種狀態初始化 傳送 暫停 結束 
                                '資訊 CMD 狀態 原因 From To NEXT  Pri. Wt 
                                '先保持原來架構

                                Dim site As String = ""

                                If Not FromPoint.site Is Nothing And Not ToPoint.site Is Nothing Then

                                    setCommtext(FromPoint.site + "to" + ToPoint.site)
                                    reason = FromPoint.site + "to" + ToPoint.site
                                    site = CheckSite(FromPoint.site, ToPoint.site)

                                    Dim Caridx As Integer = -1
                                    idx = -1
                                    If site = "" And Not Car(car_i).Site = "TP,TC" Then
                                        '不在相同區域，需要轉傳
                                        '選COMMON ZONE
                                        '選車
                                        '派車
                                        Dim commonzone As Integer = 0
                                        For j As Integer = Tag_point_list.Length - 1 To 0 Step -1
                                            '搜尋空COMMON ZONE
                                            If Tag_point_list(j).site.IndexOf("common") > -1 Then
                                                idx = -1
                                                idx = comQGWrapper.CST_SearchByLoc(Tag_point_list(j).LOC)

                                                If idx = -1 Then
                                                    '沒有CST搜尋 是否有鎖住
                                                    Dim shelf_idx As Integer = -1
                                                    For k As Integer = 0 To comQGWrapper.ShelfData.Length - 1
                                                        If comQGWrapper.ShelfData(k).Shelf_Status = "" And Tag_point_list(j).TagId = comQGWrapper.ShelfData(k).tag_id Then
                                                            shelf_idx = k
                                                            Exit For
                                                        End If
                                                    Next
                                                    Dim inline(2) As Integer
                                                    Dim outline(2) As Integer
                                                    For k As Integer = 0 To 2
                                                        inline(k) = 4016 - k * 2
                                                        outline(k) = 4015 - k * 2
                                                    Next

                                                    If shelf_idx > -1 Then
                                                        If FromPoint.TagId < 3000 And Array.IndexOf(inline, Tag_point_list(j).TagId) > -1 Then
                                                            commonzone = Tag_point_list(j).TagId
                                                            Exit For
                                                        ElseIf FromPoint.TagId > 3000 And Array.IndexOf(outline, Tag_point_list(j).TagId) > -1 Then
                                                            'out 1,2號車 3號線 優先放3號線
                                                            commonzone = Tag_point_list(j).TagId
                                                            Exit For
                                                        End If
                                                        'If Car(car_i).device_no <= 2 Then
                                                        '    If FromPoint.TagId < 3000 And Array.IndexOf(inline, Tag_point_list(j).TagId) > -1 Then
                                                        '        'in 1,2號車 3號線 優先放4號線
                                                        '        commonzone = Tag_point_list(j).TagId
                                                        '        Exit For
                                                        '    ElseIf FromPoint.TagId > 3000 And FromPoint.TagId < 4000 And Array.IndexOf(outline, Tag_point_list(j).TagId) > -1 Then
                                                        '        'out 1,2號車 3號線 優先放3號線
                                                        '        commonzone = Tag_point_list(j).TagId
                                                        '        Exit For
                                                        '    ElseIf FromPoint.TagId > 4000 And Array.IndexOf(inline, Tag_point_list(j).TagId) > -1 Then
                                                        '        'out 1,2號車 4號線 優先放4號線
                                                        '        commonzone = Tag_point_list(j).TagId
                                                        '        Exit For
                                                        '    End If
                                                        'Else
                                                        '    If FromPoint.TagId < 3000 And Array.IndexOf(outline, Tag_point_list(j).TagId) > -1 And (ToPoint.TagId = 3010 Or ToPoint.TagId = 3011 Or ToPoint.TagId = 3013 Or ToPoint.TagId = 3014 Or ToPoint.TagId = 3024 Or ToPoint.TagId = 3025 Or ToPoint.TagId = 3026) Then
                                                        '        '3號線的機台 放進來的時候優先使用3號轉傳區
                                                        '        commonzone = Tag_point_list(j).TagId
                                                        '        Exit For
                                                        '    ElseIf FromPoint.TagId < 3000 And Array.IndexOf(inline, Tag_point_list(j).TagId) > -1 Then
                                                        '        commonzone = Tag_point_list(j).TagId
                                                        '        Exit For

                                                        '    ElseIf FromPoint.TagId > 3000 And Array.IndexOf(outline, Tag_point_list(j).TagId) > -1 Then
                                                        '        commonzone = Tag_point_list(j).TagId
                                                        '        Exit For

                                                        '    End If




                                                        'End If
                                                    End If
                                                End If
                                            End If
                                        Next
                                        If commonzone > 0 Then

                                            If cmd_send_flag = False Then
                                                Caridx = SELECT_Car(FromPoint.site, car_i)
                                            End If
                                            If Caridx > -1 Then
                                                '
                                                cmd_flag = Send_CMD_CST(Car(Caridx).device_no, FromPoint.TagId, commonzone, comQGWrapper.CST(i).CarrierID, comQGWrapper.CST(i).CommandID)
                                                '   ListView1_ReNew()
                                                If cmd_flag > 0 Then
                                                    cmd_send_flag = True
                                                    NextLoc = commonzone.ToString
                                                    comQGWrapper.CST(i).note = "Send to Car" + Car(Caridx).device_no.ToString
                                                    If (Car(Caridx).device_no = 1 Or Car(Caridx).device_no = 2) And commonzone = 3003 Then
                                                        Send_CMD(Car(Caridx).device_no, 1, 3011, "MCS_common")
                                                    End If
                                                Else
                                                    reason = comQGWrapper.CST(i).CarrierID + "commonzone busy"
                                                End If
                                            Else
                                                setCommtext(comQGWrapper.CST(i).CarrierID + "No car to commonzone")
                                                reason = "No car to commonzone"
                                            End If
                                        Else
                                            setCommtext(comQGWrapper.CST(i).CarrierID + " commonzone full")
                                            reason = "commonzone full"
                                        End If
                                    ElseIf Car(car_i).Site = "TP,TC" Then
                                        '跨區直傳
                                        Caridx = car_i
                                        cmd_flag = Send_CMD_CST(Car(Caridx).device_no, FromPoint.TagId, ToPoint.TagId, comQGWrapper.CST(i).CarrierID, comQGWrapper.CST(i).CommandID)
                                        ' ListView1_ReNew()
                                        If cmd_flag > 0 Then
                                            cmd_send_flag = True
                                            NextLoc = ToPoint.TagId.ToString
                                            comQGWrapper.CST(i).note = "Send to Car" + Car(Caridx).device_no.ToString
                                        Else
                                            reason = comQGWrapper.CST(i).CarrierID + "SendCmdErr"
                                        End If
                                    Else
                                        '相同區域，直傳
                                        '選車
                                        '派車
                                        If cmd_send_flag = False Then
                                            Caridx = SELECT_Car(site, car_i)
                                        End If

                                        If Caridx > -1 Then
                                            cmd_flag = Send_CMD_CST(Car(Caridx).device_no, FromPoint.TagId, ToPoint.TagId, comQGWrapper.CST(i).CarrierID, comQGWrapper.CST(i).CommandID)
                                            ' ListView1_ReNew()
                                            If cmd_flag > 0 Then
                                                cmd_send_flag = True
                                                NextLoc = ToPoint.TagId.ToString
                                                comQGWrapper.CST(i).note = "Send to Car" + Car(Caridx).device_no.ToString
                                            Else
                                                reason = comQGWrapper.CST(i).CarrierID + "commonzone busy"
                                            End If
                                        Else
                                            'setCommtext(comQGWrapper.CST(i).CarrierID + " commonzone or AGV busy2")
                                            reason = "No car to Dest"
                                        End If

                                    End If

                                    If Caridx > -1 Then
                                        If cmd_flag > 0 And comQGWrapper.CST(i).TransferState = GEM.TransferState_Queued Then
                                            Car(Caridx).CommandID = comQGWrapper.CST(i).CommandID
                                            comQGWrapper.EventReportSendOb(GEM.EVENT_TransferInitiated, comQGWrapper.CST(i))
                                            comQGWrapper.CST(i).TransferState = GEM.TransferState_Transferring
                                            ' i = 9999
                                        Else
                                            setCommtext(comQGWrapper.CST(i).CarrierID + " Send Cmd Error")
                                            reason = "Send Cmd Error"
                                        End If

                                    End If


                                End If

                                '判斷結束 
                                If cmd_send_flag = False And Not reason = comQGWrapper.CST(i).note Then
                                    comQGWrapper.CST(i).note = reason
                                    'wewgwef
                                    Try
                                        Query = "update mcs_cmd_list set reason ='" + comQGWrapper.CST(i).note + "' where CARRIERID='" + comQGWrapper.CST(i).CarrierID + "'"
                                        sqlCommand.CommandText = Query
                                        sqlCommand.ExecuteNonQuery()

                                    Catch ex As Exception
                                        settext(Query + ":" + ex.Message)
                                    End Try
                                End If
                            End If


                        End If


                    End If


                End If
            End If

            Application.DoEvents()
        Next


        For i As Integer = 0 To comQGWrapper.CST.Length - 1
            If Not comQGWrapper.CST(i).CarrierID = "" And ConnectionState = 1 Then

                If comQGWrapper.CST(i).TransferState = GEM.TransferState_Queued Or comQGWrapper.CST(i).TransferState = GEM.TransferState_Transferring Then
                    Dim cmd_flag As Integer = 0
                    For j As Integer = 0 To Car.Length - 1
                        For k As Integer = 0 To Me.ListView1.Items.Count - 1
                            If ListView1.Items(k).SubItems(13).Text.ToString = comQGWrapper.CST(i).CommandID Or ListView1.Items(k).SubItems(11).Text.ToString = comQGWrapper.CST(i).CarrierID Then
                                cmd_flag = 1
                                Exit For
                            End If
                        Next
                    Next

                    If cmd_flag = 0 Then
                        '沒有命令 ，判斷是到達目的地或是暫存
                        '  MsgBox("命令結束")
                        setCommtext(comQGWrapper.CST(i).CarrierZoneName + "=" + comQGWrapper.CST(i).DEST)
                        If comQGWrapper.CST(i).CarrierZoneName = comQGWrapper.CST(i).DEST Then
                            comQGWrapper.EventReportSendOb(GEM.EVENT_TransferCompleted, comQGWrapper.CST(i))
                            Dim Query1 As String = ""
                            Query1 = "Delete from mcs_cmd_list where CARRIERID='" + comQGWrapper.CST(i).CarrierID + "' or REQUEST_TIME < '" + Now.AddDays(-1).ToString("yyyy-MM-dd HH:mm:ss") + "'"

                            Update_SQL(Query1)
                            Query1 = "update mcs_cmd_history  set End_time=now() where  COMMANDID='" + comQGWrapper.CST(i).CommandID + "'"
                            Update_SQL(Query1)
                            comQGWrapper.CST(i).TransferState = 0
                            comQGWrapper.CST(i).CommandID = ""
                            comQGWrapper.CST(i).DEST = ""
                            For j As Integer = ListView2.Items.Count - 1 To 0 Step -1
                                Try
                                    If ListView2.Items(j).SubItems(0).Text = comQGWrapper.CST(i).CarrierID Then
                                        ListView2.Items.RemoveAt(j)


                                    End If
                                Catch ex As Exception
                                End Try
                            Next
                        End If
                    End If
                ElseIf comQGWrapper.CST(i).TransferState = GEM.TransferState_Canceling Then
                    '取消中
                    Dim cmd_flag As Boolean = False
                    Dim car_flag As Boolean = False
                    For j As Integer = 0 To Car.Length - 1
                        For k As Integer = 0 To Me.ListView1.Items.Count - 1
                            If ListView1.Items(k).SubItems(0).ToString = comQGWrapper.CST(i).CommandID Then
                                cmd_flag = True
                                Exit For
                            End If
                        Next
                    Next
                    For j As Integer = 0 To Car.Length - 1
                        If Car(j).CommandID = comQGWrapper.CST(i).CommandID Then
                            car_flag = True
                            Exit For
                        End If
                    Next
                    If cmd_flag = True And car_flag = True Then
                        '傳送命令
                        comQGWrapper.EventReportSendOb(GEM.EVENT_TransferInitiated, comQGWrapper.CST(i))
                        comQGWrapper.CST(i).TransferState = GEM.TransferState_Transferring
                    Else
                        ' 取消命令 回到傳送 中 在GEM處理
                    End If
                ElseIf comQGWrapper.CST(i).TransferState = GEM.TransferState_Aborting Then

                    Dim cmd_flag As Boolean = False
                    Dim car_flag As Boolean = False
                    For j As Integer = 0 To Car.Length - 1
                        For k As Integer = 0 To Me.ListView1.Items.Count - 1
                            If ListView1.Items(k).SubItems(0).ToString = comQGWrapper.CST(i).CommandID Then
                                cmd_flag = True
                                Exit For
                            End If
                        Next
                    Next
                    For j As Integer = 0 To Car.Length - 1
                        If Car(j).CommandID = comQGWrapper.CST(i).CommandID Then
                            car_flag = True
                            Exit For
                        End If
                    Next

                    If cmd_flag And car_flag = False Then
                        '可以取消命令
                        comQGWrapper.EventReportSendOb(GEM.EVENT_TransferAbortCompleted, comQGWrapper.CST(i))
                        comQGWrapper.CST(i).TransferState = 0
                        comQGWrapper.CST(i).CommandID = ""
                        setCommtext(comQGWrapper.CST(i).CarrierID + ":EVENT_TransferAbortCompleted")

                    Else
                        '傳送命令
                        comQGWrapper.EventReportSendOb(GEM.EVENT_TransferAbortFailed, comQGWrapper.CST(i))
                        'history
                        If cmd_flag And car_flag Then
                            comQGWrapper.CST(i).TransferState = GEM.TransferState_Transferring
                        Else
                            comQGWrapper.CST(i).TransferState = GEM.TransferState_Paused
                        End If


                    End If
                End If
                '處理CST 狀態事件
                If Not comQGWrapper.CST(i).CarrierState = comQGWrapper.CST(i).Pre_CarrierState Then
                    Dim Pre_CarrierState As UShort = comQGWrapper.CST(i).Pre_CarrierState
                    comQGWrapper.CST(i).Pre_CarrierState = comQGWrapper.CST(i).CarrierState
                    setCommtext(comQGWrapper.CST(i).CarrierID + ":" + Pre_CarrierState.ToString + "->" + comQGWrapper.CST(i).CarrierState.ToString)
                    If comQGWrapper.CST(i).CarrierState = GEM.CS_TRANSFERRING Then
                        comQGWrapper.EventReportSendOb(GEM.EVENT_CarrierResumed, comQGWrapper.CST(i))
                    ElseIf comQGWrapper.CST(i).CarrierState = GEM.CS_STOREDCOMPLETED Then
                        comQGWrapper.EventReportSendOb(GEM.EVENT_CarrierStored, comQGWrapper.CST(i))
                    ElseIf comQGWrapper.CST(i).CarrierState = GEM.CS_WAITOUT Then
                        comQGWrapper.EventReportSendOb(GEM.EVENT_CarrierWaitOut, comQGWrapper.CST(i))
                    ElseIf comQGWrapper.CST(i).CarrierState = GEM.CS_NONE Then
                        comQGWrapper.EventReportSendOb(GEM.EVENT_CarrierRemoved, comQGWrapper.CST(i))
                    ElseIf comQGWrapper.CST(i).CarrierState = GEM.CS_WAITIN Then
                        comQGWrapper.EventReportSendOb(GEM.EVENT_CarrierWaitIn, comQGWrapper.CST(i))
                    End If

                End If
            End If
            If Not comQGWrapper.CST(i).CarrierZoneName = comQGWrapper.CST(i).DEST And Not comQGWrapper.CST(i).DEST = "" And Not comQGWrapper.CST(i).CarrierID = "" Then
                Dim item As New ListViewItem()
                idx = -1
                item.Text = comQGWrapper.CST(i).CarrierID
                item.SubItems.Add(comQGWrapper.CST(i).CommandID)
                item.SubItems.Add(comQGWrapper.CST(i).PRIORITY)
                item.SubItems.Add(comQGWrapper.CST(i).CarrierZoneName)
                item.SubItems.Add(comQGWrapper.CST(i).CarrierLoc)

                item.SubItems.Add(comQGWrapper.CST(i).DEST)
                item.SubItems.Add(comQGWrapper.CST(i).note)
                item.SubItems.Add(((comQGWrapper.CST(i).mcstime) + comQGWrapper.CST(i).PRIORITY * 10).ToString)
                item.SubItems.Add(i)
                item.SubItems.Add(comQGWrapper.CST(i).PROCESSID)
                item.SubItems.Add(comQGWrapper.CST(i).SOURCE)
                item.SubItems.Add(comQGWrapper.CST(i).distance.ToString)
                ' ListView2.Items.Add(item)
                For j As Integer = 0 To ListView2.Items.Count - 1
                    If ListView2.Items(j).SubItems(0).Text = comQGWrapper.CST(i).CarrierID Then
                        idx = j
                        Exit For
                    End If
                Next
                If idx >= 0 Then
                    '修改
                    Dim flag As Boolean = False

                    For j As Integer = 0 To item.SubItems.Count - 1
                        If Not item.SubItems(j).Text = ListView2.Items(idx).SubItems(j).Text Then
                            ListView2.Items(idx).SubItems(j).Text = item.SubItems(j).Text
                        End If
                    Next
                Else
                    ListView2.Items.Add(item)
                End If
                '
            End If
        Next

        ' MCS 命令處理完
        For i As Integer = 0 To comQGWrapper.Zone.Length - 1
            comQGWrapper.Zone(i).ZoneCapacity = comQGWrapper.Zone(i).ZoneSize - comQGWrapper.Zone(i).CST_count - comQGWrapper.Zone(i).DisabledLocations.Split(",").Length
            If Not comQGWrapper.Zone(i).pre_ZoneCapacity = comQGWrapper.Zone(i).ZoneCapacity And start_flag Then
                comQGWrapper.EventReportSendOb(GEM.EVENT_ZoneCapacityCange, comQGWrapper.Zone(i).ZoneName + "," + comQGWrapper.Zone(i).ZoneCapacity.ToString + "," + comQGWrapper.Zone(i).ZoneSize.ToString + "," + comQGWrapper.Zone(i).ZoneType.ToString)
                comQGWrapper.Zone(i).pre_ZoneCapacity = comQGWrapper.Zone(i).ZoneCapacity
            Else
                comQGWrapper.Zone(i).pre_ZoneCapacity = comQGWrapper.Zone(i).ZoneCapacity
            End If
        Next

        'For i As Integer = 0 To Car.Length - 1
        '    If Car(i).flag = True And Car(i).Car_type = "CRANE" Then

        '        If Not Car(i).Pre_State = Car(i).State Then
        '            Car(i).Pre_State = Car(i).State
        '            If Car(i).State = "IDLE" Then
        '                comQGWrapper.EventReportSendOb(GEM.EVENT_CraneIdle, Car(i).CommandID + "," + comQGWrapper.Eqpname + "C" + Car(i).device_no.ToString)
        '            ElseIf Car(i).State = "ACTIVE" Then
        '                comQGWrapper.EventReportSendOb(GEM.EVENT_CraneActive, Car(i).CommandID + "," + comQGWrapper.Eqpname + "C" + Car(i).device_no.ToString)
        '            End If
        '        End If
        '    End If
        'Next
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

            If Car(i).BMS1(7) > 100 And Car(i).BMS1(7) < 7000 And Car(i).BMS2(7) > 4000 And Car(i).BMS2(7) < 7000 And flag = True Then

                Dim file_str As String = ".\log\" + Now.ToString("yyyyMMdd") + "_BMS" + Car(i).device_no.ToString + ".log"
                Dim filestream As StreamWriter = New StreamWriter(file_str, True)
                Dim bms As String = int2str(Car(i).BMS1, 0, 53)
                filestream.WriteLine(Now.ToString("yyyy-MM-dd HH:mm:ss") + ":" + bms)
                bms = int2str(Car(i).BMS2, 0, 53)
                filestream.WriteLine(Now.ToString("yyyy-MM-dd HH:mm:ss") + ":" + bms)
                filestream.Flush()
                filestream.Close()
            End If
        Next



        Dim mReader As MySqlDataReader

        sqlCommand.Connection = oConn
        Query = "  SELECT CarrierID,LOC,LM_User FROM `carrierupdate` WHERE UpdateTime < '2021-01-01'"
        sqlCommand.CommandText = Query

        mReader = sqlCommand.ExecuteReader()
        Dim cstid As String = ""
        '  Dim loc As String = ""
        Dim cstidx As Integer = -1
        Dim locidx As Integer = -1
        Dim loc As New Tag_Point
        Dim cnt As Integer = 0
        While mReader.Read
            cnt += 1
            cstid = mReader.Item(0)
            ' loc = mReader.Item(1)
            cstidx = comQGWrapper.CST_SearchByCSTID(cstid)
            If mReader.Item(1).ToString = "ADDPWT" And (cstidx > -1) Then


                comQGWrapper.CST(cstidx).mcstime -= 10000


            Else
                For j As Integer = 0 To Tag_point_list.Length - 1
                    If Tag_point_list(j).LOC = mReader.Item(1) Then
                        loc = Tag_point_list(j)
                        Exit For
                    End If
                Next
                locidx = comQGWrapper.CST_SearchByLoc(loc.LOC)
                If locidx > -1 Then
                    '移除舊的CST 位置
                    If Not comQGWrapper.CST(locidx).CarrierID = cstid Then
                        comQGWrapper.CST_REMOVE(comQGWrapper.CST(locidx).CarrierID)
                        Query = "update `carrier`  set LOC_NAME='',LOC_TYPE='4',SUB_LOC='' ,CARRIER_STATUS	=0 "
                        Query += " where  CARRIER_ID='" + comQGWrapper.CST(locidx).CarrierID + "'"
                        Update_SQL(Query)
                    End If

                End If


                If cstidx = -1 Then
                    '新增位置
                    comQGWrapper.CST_Add(cstid, loc.ZONE_NAME, loc.LOC)
                    Query = "insert ignore into `carrier` (STK_NAME,CARRIER_ID,LOT_ID,LOC_NAME,LOC_TYPE,SUB_LOC,CARRIER_STATUS,STORED_TIME,REQ_TIME,UPDATE_DATE,UPDATE_BY,CREATE_DATE,CREATE_BY) " + _
                 "VALUES ('" + comQGWrapper.Eqpname + "', '" + cstid + "', '" + cstid + "','" + loc.ZONE_NAME + "',1,'" + loc.LOC + "', '4', now(), '', now(), 'AGVC', now(), 'AGVC');"
                    Update_SQL(Query)
                Else
                    '變更位置
                    comQGWrapper.CST_Change(comQGWrapper.CST(cstidx).CarrierID, loc.ZONE_NAME, loc.LOC, "4")
                    Query = "update `carrier`  set LOC_NAME='" + loc.ZONE_NAME + "',LOC_TYPE=" + loc.tagtype.ToString + ",SUB_LOC='" + loc.LOC + "' ,CARRIER_STATUS	=" + comQGWrapper.CST(cstidx).CarrierState.ToString
                    Query += " where  CARRIER_ID='" + cstid + "'"
                    Update_SQL(Query)
                End If
            End If



        End While

        mReader.Close()
        If cnt > 0 Then
            Query = "update `carrierupdate`  set UpdateTime=now() where UpdateTime <'2021-01-01' "
            sqlCommand.CommandText = Query
            sqlCommand.ExecuteNonQuery()
        End If



        oConn.Close()
        oConn.Dispose()
        For i As Integer = 0 To comQGWrapper.EqPort.Length - 1
            If comQGWrapper.EqPort(i).LoadAvail = 0 Then
                comQGWrapper.EqPort(i).state = "L-Req"
            ElseIf comQGWrapper.EqPort(i).UnLoadAvail = 0 Then
                comQGWrapper.EqPort(i).state = "UL-Req"
            ElseIf comQGWrapper.EqPort(i).ONLINE = 1 Then
                comQGWrapper.EqPort(i).state = "ONLINE"
            ElseIf comQGWrapper.EqPort(i).ERR = 1 Then
                comQGWrapper.EqPort(i).state = "ERROR"
            Else
                comQGWrapper.EqPort(i).state = "OFFLINE"
            End If

            If Not comQGWrapper.EqPort(i).state = comQGWrapper.EqPort(i).PreState Then

                Update_SQL("update port set EQ_State ='" + comQGWrapper.EqPort(i).state + "',PreState='" + comQGWrapper.EqPort(i).PreState + "' where PORT_ID='" + comQGWrapper.EqPort(i).PortID + "'")
                Update_SQL("INSERT INTO `port_history` (`PortID`, `Status`,PreState, `updatetime`,PreStateTime) VALUES ('" + comQGWrapper.EqPort(i).PortID + "', '" + comQGWrapper.EqPort(i).state + "','" + comQGWrapper.EqPort(i).PreState + "', CURRENT_TIMESTAMP,'" + comQGWrapper.EqPort(i).PreStateTime + "')")
                comQGWrapper.EqPort(i).PreState = comQGWrapper.EqPort(i).state
                comQGWrapper.EqPort(i).PreStateTime = Now().ToString("yyyy-MM-dd HH:mm:ss")
            End If
        Next
        For i As Integer = 0 To Car.Length - 1
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
                Dim VC1_MAX, VC1_MIN As Integer
                Dim VC2_MAX, VC2_MIN As Integer
                VC1_MAX = 0
                VC2_MAX = 0
                VC1_MIN = 9999
                VC2_MIN = 9999

                For j As Integer = 18 To 32
                    If Car(i).BMS1(j) > VC1_MAX Then
                        VC1_MAX = Car(i).BMS1(j)
                    End If
                    If Car(i).BMS1(j) < VC1_MIN Then
                        VC1_MIN = Car(i).BMS1(j)
                    End If
                    If Car(i).BMS2(j) > VC2_MAX Then
                        VC2_MAX = Car(i).BMS2(j)
                    End If
                    If Car(i).BMS1(j) < VC2_MIN Then
                        VC2_MIN = Car(i).BMS2(j)
                    End If
                Next
                Query = "INSERT INTO `agv_tagid_history` (`AGV_No` ,`Pre_TagID` ,`TagID` ,`RecordTime`,keep_time,cmd_idx,speed,Volt,Loading,Shelf_Car,distance,Temp,Humidity,direction,Auto_Info,AGV_X,AGV_Y,AGV_TH,`VB1`, `IB1`, `BT1`, `SOC1`, `SOH1`, `PROT1`, `STAT1`, `CHG_AH1`, `DSG_AH1`, `CYCLE1`, `VB2`, `IB2`, `BT2`, `SOC2`, `SOH2`, `PROT2`, `STAT2`, `CHG_AH2`, `DSG_AH2`, `CYCLE2`,`VC1_MIN`,`VC1_MAX`,`BT1_2`,`VC2_MIN`,`VC2_MAX`,`BT2_2`,subcmd,bat_SN1,bat_SN2) " + _
                                 " VALUES ('" + Car(i).device_no.ToString + "', '" + Car(i).Pre_TagID.ToString + "', '" + Car(i).get_tagId.ToString + "', '" + Now().ToString("yyyy-MM-dd HH:mm:ss") + "',''," + Car(i).cmd_sql_idx.ToString + "," + Car(i).get_Speed.ToString + "," + Car(i).get_Volt.ToString + "," + Car(i).device_status(7).ToString + "," + Car(i).get_Shelf_Car_No.ToString + "," + Car(i).get_distance.ToString + "," + Car(i).device_status(21).ToString + "," + Car(i).device_status(22).ToString + "," + Car(i).get_direction.ToString + "," + Car(i).status.ToString + "," + Car(i).AXIS_X.ToString + "," + Car(i).AXIS_Y.ToString + "," + Car(i).AXIS_Z.ToString + _
                                 "," + Car(i).BMS1(7).ToString + "," + Car(i).BMS1(8).ToString + "," + Car(i).BMS1(10).ToString + "," + Car(i).BMS1(14).ToString + "," + Car(i).BMS1(15).ToString + "," + Car(i).BMS1(16).ToString + "," + Car(i).BMS1(17).ToString + "," + (Car(i).BMS1(37) * 65536 + Car(i).BMS1(38)).ToString + "," + (Car(i).BMS1(39) * 65536 + Car(i).BMS1(40)).ToString + "," + Car(i).BMS1(41).ToString + _
                                 "," + Car(i).BMS2(7).ToString + "," + Car(i).BMS2(8).ToString + "," + Car(i).BMS2(10).ToString + "," + Car(i).BMS2(14).ToString + "," + Car(i).BMS2(15).ToString + "," + Car(i).BMS2(16).ToString + "," + Car(i).BMS2(17).ToString + "," + (Car(i).BMS2(37) * 65536 + Car(i).BMS2(38)).ToString + "," + (Car(i).BMS2(39) * 65536 + Car(i).BMS2(40)).ToString + "," + Car(i).BMS2(41).ToString + _
                                 "," + VC1_MIN.ToString + "," + VC1_MAX.ToString + "," + Car(i).BMS1(11).ToString + "," + VC2_MIN.ToString + "," + VC2_MAX.ToString + "," + Car(i).BMS2(11).ToString + _
                                 ",'" + Car(i).subcmd + "','" + Car(i).bat_SN(0) + "','" + Car(i).bat_SN(1) + "');"
                Update_SQL(Query)

                Car(i).agv_status_time = Now.ToString("yyyy-MM-dd HH:mm:ss")
            End If
        Next
        If BG_Update.IsBusy = False Then
            BG_Update.RunWorkerAsync()
        End If

    End Sub

    Function QueryPosition(ByVal tagid As Integer) As String
        QueryPosition = ""
        For i As Integer = 0 To Tag_point_list.Length - 1

            Tag_point_list(i).TagId = tagid
            Return Tag_point_list(i).floor
        Next
    End Function
    Function CheckSite(ByVal SiteA As String, ByVal SiteB As String) As String
        CheckSite = ""
        Dim SiteAList() As String = SiteA.Split(",")
        Dim SiteBList() As String = SiteB.Split(",")
        Dim idx As Integer = 0
        For i As Integer = 0 To SiteAList.Length - 1
            If Array.IndexOf(SiteBList, SiteAList(i)) > -1 Then
                Return SiteAList(i)
                If idx = 0 Then
                    CheckSite = SiteAList(i)
                Else
                    CheckSite += "," + SiteAList(i)
                End If
            End If
        Next

    End Function
    Function SELECT_Car(ByVal Site As String, ByVal car_idx As Integer) As Integer
        SELECT_Car = -1
        Dim sitelist() As String = Site.Split(",")
        Dim idx As Integer = 0
        Dim car_site_list() As String = Car(car_idx).Site.Split(",")
        For j As Integer = 0 To car_site_list.Length - 1
            If Array.IndexOf(sitelist, car_site_list(j)) > -1 Then
                SELECT_Car = car_idx
            End If
        Next





    End Function
    Dim port_time As Integer = 0
    ' Dim chaanger As Integer = 0
    Private Sub EQ_BG_DoWork(ByVal sender As System.Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles EQ_BG.DoWork
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        Dim Query As String = ""
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn



     
        For i As Integer = 0 To eqp_client.Length - 1
            If My.Computer.Network.Ping(eqp_client(i).ipadress) Then

                Dim readflag As Boolean = False
                readflag = eqp_client(i).Read_DI()
                For j As Integer = 0 To comQGWrapper.EqPort.Length - 1
                    If eqp_client(i).ipadress = comQGWrapper.EqPort(j).ip And eqp_client(i).port = comQGWrapper.EqPort(j).port Then
                        comQGWrapper.EqPort(j).ONLINE = eqp_client(i).DI(0)
                        comQGWrapper.EqPort(j).ERR = eqp_client(i).DI(1)
                        comQGWrapper.EqPort(j).READY = eqp_client(i).DI(comQGWrapper.EqPort(j).adr)
                        comQGWrapper.EqPort(j).LOADED = eqp_client(i).DI(comQGWrapper.EqPort(j).adr + 1)
                        If comQGWrapper.EqPort(j).READY = 1 And comQGWrapper.EqPort(j).ONLINE = 1 And comQGWrapper.EqPort(j).ERR = 0 Then
                            If port_time = 60 Then
                                comQGWrapper.EventReportSendOb(GEM.EVENT_PortInService, comQGWrapper.EqPort(j).PortID)
                                comQGWrapper.EventReportSendOb(GEM.EVENT_LoadRequestReport, comQGWrapper.EqPort(j).PortID + "," + comQGWrapper.EqPort(j).LoadAvail.ToString + "," + comQGWrapper.EqPort(j).UnLoadAvail.ToString)
                            End If
                            If comQGWrapper.EqPort(j).LOADED = 1 Then
                                '有載 那就是onload req
                                If Not (comQGWrapper.EqPort(j).LoadAvail = 1 And comQGWrapper.EqPort(j).UnLoadAvail = 0) Then
                                    comQGWrapper.EqPort(j).LoadAvail = 1
                                    comQGWrapper.EqPort(j).UnLoadAvail = 0
                                    If Not port_time = 60 Then
                                        comQGWrapper.EventReportSendOb(GEM.EVENT_PortInService, comQGWrapper.EqPort(j).PortID)
                                    End If

                                    Thread.Sleep(100)
                                    comQGWrapper.EventReportSendOb(GEM.EVENT_LoadRequestReport, comQGWrapper.EqPort(j).PortID + "," + comQGWrapper.EqPort(j).LoadAvail.ToString + "," + comQGWrapper.EqPort(j).UnLoadAvail.ToString)
                                End If
                            Else
                                If Not (comQGWrapper.EqPort(j).LoadAvail = 0 And comQGWrapper.EqPort(j).UnLoadAvail = 1) Then
                                    comQGWrapper.EqPort(j).LoadAvail = 0
                                    comQGWrapper.EqPort(j).UnLoadAvail = 1
                                    If Not port_time = 60 Then
                                        comQGWrapper.EventReportSendOb(GEM.EVENT_PortInService, comQGWrapper.EqPort(j).PortID)
                                    End If
                                    Thread.Sleep(100)
                                    comQGWrapper.EventReportSendOb(GEM.EVENT_LoadRequestReport, comQGWrapper.EqPort(j).PortID + "," + comQGWrapper.EqPort(j).LoadAvail.ToString + "," + comQGWrapper.EqPort(j).UnLoadAvail.ToString)
                                End If
                                End If
                        Else
                                If Not (comQGWrapper.EqPort(j).LoadAvail = 1 And comQGWrapper.EqPort(j).UnLoadAvail = 1) Then
                                    comQGWrapper.EqPort(j).LoadAvail = 1
                                    comQGWrapper.EqPort(j).UnLoadAvail = 1
                                    comQGWrapper.EventReportSendOb(GEM.EVENT_LoadRequestReport, comQGWrapper.EqPort(j).PortID + "," + comQGWrapper.EqPort(j).LoadAvail.ToString + "," + comQGWrapper.EqPort(j).UnLoadAvail.ToString)
                                End If


                        End If

                    End If
                Next
            End If
        Next

        port_time += 1
        If port_time > 60 Then
            port_time = 0
        End If
        For i As Integer = 0 To ChargerClient.Length - 1
            Try
                If My.Computer.Network.Ping(ChargerClient(i).ipadress) Then
                    'Dim readflag As Boolean = False
                    'setCommtext("Ping->" + ChargerClient(i).EQ_ID + ":" + ChargerClient(i).ipadress)
                    Dim BMS1(50) As Integer
                    Dim BMS2(50) As Integer
                    Dim BMS1_fw As String = ""
                    Dim BMS2_fw As String = ""
                    Dim ToCharger(20) As Integer
                    ToCharger(0) = Now.Second
                    Dim VC1_MIN, VC1_MAX, VC2_MIN, VC2_MAX As Integer
                    ChargerClient(i).ReadOK = ChargerClient(i).Read_HoldingReg(ChargerClient(i).HoldingReg, 60, ChargerClient(i).HoldingResponse)
                    ' ChargerClient(i).Write_HoldingReg(200, 1, ToCharger)
                    Dim Status As String = "Auto"




                    If ChargerClient(i).HoldingResponse(19) > 0 Then
                        If Not ChargerClient(i).HoldingResponse(19) = ChargerClient(i).Pre_State Then
                            Query = "INSERT ignore INTO  `alarm` (`cmd_idx`, `HAPPEN_DATE`, `CLEAR_DATE`, `ALM_ID`, `ALM_DEVICE`, `ALM_TYPE`, `ALM_MSG`, `SUB_LOC`, `ALARM_SPACE`, `CST_ID`) "
                            Query += "VALUES ('',now(),now(), '" + (3000 + ChargerClient(i).HoldingResponse(19)).ToString + "', '" + ChargerClient(i).tag_id.ToString + "', '', '', '" + ChargerClient(i).tag_id.ToString + "', '', '') ;"
                            sqlCommand.CommandText = Query
                            sqlCommand.ExecuteNonQuery()
                            ChargerClient(i).Pre_State = ChargerClient(i).HoldingResponse(19)
                        End If
                        Status = "DOWN"
                    ElseIf ChargerClient(i).HoldingResponse(18) = 0 Then
                        Status = "Manual"
                    ElseIf ChargerClient(i).HoldingResponse(18) = 2 Then
                        Dim BMS1_serial(16) As Integer
                        Dim BMS2_serial(16) As Integer
                        Status = "ManualCharge"
                        ChargerClient(i).ReadOK = ChargerClient(i).Read_HoldingReg(3053, 16, BMS1_serial)
                        ChargerClient(i).ReadOK = ChargerClient(i).Read_HoldingReg(3153, 16, BMS2_serial)
                        BMS1_fw = int2bytestr(BMS1_serial, 0, 16)
                        BMS2_fw = int2bytestr(BMS2_serial, 0, 16)
                        ChargerClient(i).SN1 = BMS1_fw
                        ChargerClient(i).SN2 = BMS2_fw
                        ChargerClient(i).Read_HoldingReg(3000, 50, BMS1)
                        ChargerClient(i).Read_HoldingReg(3100, 50, BMS2)
                        ChargerClient(i).BMS1 = BMS1.Clone
                        ChargerClient(i).BMS2 = BMS2.Clone

                        VC1_MIN = GetVcMin(BMS1)
                        VC1_MAX = GetVcMax(BMS1)
                        VC2_MIN = GetVcMin(BMS2)
                        VC2_MAX = GetVcMax(BMS2)
                        '電池判斷
                        '第一階段上位異常 

                        '電池1 狀態異常
                        If ChargerClient(i).BMS1(16) > 0 Then
                            If (BmsAlertIdx And ChargerClient(i).BMS1(16)) > 0 And ChargerClient(i).BMS1(16) Then
                                If InttoBitidx(ChargerClient(i).BMS1(16)) <> 11 And InttoBitidx(ChargerClient(i).BMS1(16)) <> 10 Then
                                    ToCharger(20) = 30000 + InttoBitidx(ChargerClient(i).BMS1(16))
                                End If

                            ElseIf (BmsWarmIdx And ChargerClient(i).BMS1(16)) > 0 Then
                                If InttoBitidx(ChargerClient(i).BMS2(16)) <> 11 And InttoBitidx(ChargerClient(i).BMS1(16)) <> 10 Then
                                    ToCharger(20) = 30000 + InttoBitidx(ChargerClient(i).BMS1(16))
                                End If

                            End If
                        End If
                        '電池1  BMS硬體異常
                        If ChargerClient(i).BMS1(17) >= 64 Then
                            ToCharger(20) = 30016 + InttoBitidx(ChargerClient(i).BMS1(17))
                        End If

                        '電池2 狀態異常
                        If ChargerClient(i).BMS2(16) > 0 Then
                            If (BmsAlertIdx And ChargerClient(i).BMS2(16)) > 0 Then
                                ToCharger(20) = 30100 + InttoBitidx(ChargerClient(i).BMS2(16))
                            End If
                        End If
                        '電池2 BMS硬體異常
                        If ChargerClient(i).BMS2(17) >= 64 Then
                            ToCharger(20) = 30116 + InttoBitidx(ChargerClient(i).BMS2(17))
                        End If




                        Dim bms1check, bms2check As Integer
                        bms1check = Car(0).CheckBms(ChargerClient(i).BMS1, ChargerClient(i).BMSAlarm1)
                        bms2check = Car(0).CheckBms(ChargerClient(i).BMS2, ChargerClient(i).BMSAlarm2)
                        '要有心跳才偵測異常

                        '電池一 上位偵測異常
                        If ChargerClient(i).BMSAlarm1(17) = 0 Then
                            If bms1check > 0 Then
                                If (BmsAlertIdx And bms1check) > 0 Then
                                    ToCharger(20) = 30032 + InttoBitidx(bms1check)
                                   
                                End If
                            End If
                        End If

                        '電池二 上位偵測異常
                        If ChargerClient(i).BMSAlarm2(17) = 0 Then
                            If bms2check > 0 Then
                                If (BmsAlertIdx And bms1check) > 0 Then
                                    ToCharger(20) = 30132 + InttoBitidx(bms2check)

                                End If
                            End If
                        End If

                        If ChargerClient(i).BMSAlarm1(17) > 300 Then
                            ' ToCharger(20) = 30065 '正常連線且心跳異常

                        End If
                        If ChargerClient(i).BMSAlarm2(17) > 300 Then
                            ' ToCharger(20) = 30165 '正常連線且心跳異常

                        End If




                        Dim file_str As String = ".\log\" + Now.ToString("yyyyMMdd") + "_BMS_C" + ChargerClient(i).EQ_ID.ToString + ".log"
                        Dim filestream As StreamWriter = New StreamWriter(file_str, True)

                        If BMS1(0) = 6 Then
                            Dim bms As String = int2str(BMS1, 0, 50)
                            filestream.WriteLine(Now.ToString("yyyy-MM-dd HH:mm:ss") + ":" + bms)
                        End If
                        If BMS2(0) = 6 Then
                            Dim bms As String = int2str(BMS2, 0, 50)
                            filestream.WriteLine(Now.ToString("yyyy-MM-dd HH:mm:ss") + ":" + bms)
                        End If

                        filestream.Flush()
                        filestream.Close()
                    ElseIf ChargerClient(i).HoldingResponse(7) > 0 Then
                        Status = "RUN"
                    End If
                    'ToCharger(20) = chaanger
                    ' If ToCharger(20) > 0 Then
                    ChargerClient(i).Write_HoldingReg(200, 21, ToCharger)
                    ' End If

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
                        Dim chargerstatus As Integer = 0
                        If ChargerClient(i).HoldingResponse(19) > 0 Then
                            chargerstatus = -1
                        ElseIf ChargerClient(i).HoldingResponse(18) = 2 Then
                            '手動充電
                            chargerstatus = 3
                        ElseIf ChargerClient(i).HoldingResponse(7) > 10000 Or (ChargerClient(i).HoldingResponse(23) << 16) + ChargerClient(i).HoldingResponse(22) > 2 Then
                            chargerstatus = 2
                        Else
                            chargerstatus = ChargerClient(i).HoldingResponse(18)
                        End If
                        If (ChargerClient(i).HoldingResponse(7) > 0 And ChargerClient(i).HoldingResponse(7) < 10000) Or (ChargerClient(i).HoldingResponse(7) > 10000 And ChargerClient(i).HoldingResponse(0) > 0) Or (ChargerClient(i).HoldingResponse(18) = 2) Or (ChargerClient(i).HoldingResponse(52) > 0) Then
                            Dim carno As Integer = 0
                            For j As Integer = 0 To Car.Length - 1
                                If Car(j).get_tagId = ChargerClient(i).tag_id And chargerstatus = 2 Then
                                    carno = Car(j).device_no

                                End If
                            Next
                            Query = "INSERT ignore INTO `agv`.`charger_history` (`tagid`, `X`, `Y`, `T1`, `ForkLocation`,LineT, `AutoStatus`, `Err`, `OUT_V`, `OUT_A`, `OUT_Watt`,OUT_T, " + _
                                               "`OUT_mAh`, `OUT_Wh`, `steptime`, `totaltime`, `STEPIndex`, `IN_V`, `IN_A`, `IN_Watt`, `IN_kWh`, `IN_totaltime`,carno,OUT_T2, `VB1`, `IB1`, `BT1`, `SOC1`, `SOH1`, `PROT1`, `STAT1`, `CHG_AH1`, `DSG_AH1`, `CYCLE1`, `VB2`, `IB2`, `BT2`, `SOC2`, `SOH2`, `PROT2`, `STAT2`, `CHG_AH2`, `DSG_AH2`, `CYCLE2`,`VC1_MIN`,`VC1_MAX`,`BT1_2`,`VC2_MIN`,`VC2_MAX`,`BT2_2`,bms1_fw,bms2_fw) " + _
                                               "VALUES ('" + ChargerClient(i).tag_id.ToString + "', '" + ChargerClient(i).HoldingResponse(1).ToString + "', '" + ChargerClient(i).HoldingResponse(2).ToString + "', '" + ChargerClient(i).HoldingResponse(3).ToString + "'," + _
                                               " '" + ChargerClient(i).HoldingResponse(7).ToString + "', '" + ChargerClient(i).HoldingResponse(8).ToString + "', '" + chargerstatus.ToString + "', '" + ChargerClient(i).HoldingResponse(19).ToString + "', '" + ((ChargerClient(i).HoldingResponse(21) << 16) + ChargerClient(i).HoldingResponse(20)).ToString + "'," + _
                                               " '" + ((ChargerClient(i).HoldingResponse(23) << 16) + ChargerClient(i).HoldingResponse(22)).ToString + "', '" + ((ChargerClient(i).HoldingResponse(25) << 16) + ChargerClient(i).HoldingResponse(24)).ToString + "', '" + ((ChargerClient(i).HoldingResponse(27) << 16) + ChargerClient(i).HoldingResponse(26)).ToString + "', '" + ((ChargerClient(i).HoldingResponse(29) << 16) + ChargerClient(i).HoldingResponse(28)).ToString + "', " + _
                                               " '" + ((ChargerClient(i).HoldingResponse(31) << 16) + ChargerClient(i).HoldingResponse(30)).ToString + "', '" + step_time + "', " + _
                                               " '" + totaltime + "', '" + ChargerClient(i).HoldingResponse(40).ToString + "', '" + ChargerClient(i).HoldingResponse(50).ToString + "' " + _
                                               ", '" + ChargerClient(i).HoldingResponse(52).ToString + "', '" + ChargerClient(i).HoldingResponse(24).ToString + "', '" + ChargerClient(i).HoldingResponse(56).ToString + "', '" + ChargerClient(i).HoldingResponse(58).ToString + "'," + carno.ToString + ",'" + ChargerClient(i).HoldingResponse(9).ToString + "'" + _
                                "," + BMS1(7).ToString + "," + BMS1(8).ToString + "," + BMS1(10).ToString + "," + BMS1(14).ToString + "," + BMS1(15).ToString + "," + BMS1(16).ToString + "," + BMS1(17).ToString + "," + (BMS1(37) * 65536 + BMS1(38)).ToString + "," + (BMS1(39) * 65536 + BMS1(40)).ToString + "," + BMS1(41).ToString + _
                               "," + BMS2(7).ToString + "," + BMS2(8).ToString + "," + BMS2(10).ToString + "," + BMS2(14).ToString + "," + BMS2(15).ToString + "," + BMS2(16).ToString + "," + BMS2(17).ToString + "," + (BMS2(37) * 65536 + BMS2(38)).ToString + "," + (BMS2(39) * 65536 + BMS2(40)).ToString + "," + BMS2(41).ToString + _
                               "," + VC1_MIN.ToString + "," + VC1_MAX.ToString + "," + BMS1(11).ToString + "," + VC2_MIN.ToString + "," + VC2_MAX.ToString + "," + BMS2(11).ToString + _
                                ",'" + BMS1_fw + "','" + BMS2_fw + "');"
                            sqlCommand.CommandText = Query
                            sqlCommand.ExecuteNonQuery()
                        End If

                        Query = "update  `charger`  set Status ='" + Status + "',err=" + ChargerClient(i).HoldingResponse(19).ToString + ",Amp=" + Amp.ToString + ",Volt=" + Volt.ToString + " where PORT_ID='" + ChargerClient(i).EQ_ID + "'"
                        sqlCommand.CommandText = Query
                        sqlCommand.ExecuteNonQuery()
                        Query = "update  `charger_status`  set X ='" + ChargerClient(i).HoldingResponse(1).ToString + "',Y=" + ChargerClient(i).HoldingResponse(2).ToString + ",T1=" + ChargerClient(i).HoldingResponse(3).ToString + ",ForkLocation=" + ChargerClient(i).HoldingResponse(7).ToString + ",LineT=" + ChargerClient(i).HoldingResponse(8).ToString + _
                            ",AutoStatus ='" + chargerstatus.ToString + "',Err=" + ChargerClient(i).HoldingResponse(19).ToString + ",OUT_V=" + ((ChargerClient(i).HoldingResponse(21) << 16) + ChargerClient(i).HoldingResponse(20)).ToString + ",OUT_A=" + ((ChargerClient(i).HoldingResponse(23) << 16) + ChargerClient(i).HoldingResponse(22)).ToString + _
                            ",OUT_Watt ='" + ((ChargerClient(i).HoldingResponse(25) << 16) + ChargerClient(i).HoldingResponse(24)).ToString + "',OUT_T=" + ((ChargerClient(i).HoldingResponse(27) << 16) + ChargerClient(i).HoldingResponse(26)).ToString + ",OUT_mAh=" + ((ChargerClient(i).HoldingResponse(29) << 16) + ChargerClient(i).HoldingResponse(28)).ToString + ",OUT_Wh=" + ((ChargerClient(i).HoldingResponse(31) << 16) + ChargerClient(i).HoldingResponse(30)).ToString + _
                            ",steptime ='" + step_time + "',totaltime=" + totaltime + ",STEPIndex=" + ChargerClient(i).HoldingResponse(40).ToString + ",IN_V=" + ChargerClient(i).HoldingResponse(50).ToString + _
                             ",IN_A ='" + ChargerClient(i).HoldingResponse(52).ToString + "',IN_Watt=" + ChargerClient(i).HoldingResponse(54).ToString + ",IN_kWh=" + ChargerClient(i).HoldingResponse(56).ToString + ",IN_totaltime=" + ChargerClient(i).HoldingResponse(58).ToString + _
                              ",cur_time =now() ,out_t2='" + ChargerClient(i).HoldingResponse(9).ToString + "'" + _
                               ",VB1=" + BMS1(7).ToString + ",IB1=" + BMS1(8).ToString + " ,BT1=" + BMS1(10).ToString + ",SOC1=" + BMS1(14).ToString + ",SOH1=" + BMS1(15).ToString + _
                            ",PROT1=" + BMS1(16).ToString + ",STAT1=" + BMS1(17).ToString + " ,CHG_AH1=" + (BMS1(37) * 65536 + BMS1(38)).ToString + ",DSG_AH1=" + (BMS1(39) * 65536 + BMS1(40)).ToString + ",CYCLE1=" + BMS1(41).ToString + _
                             ",VB2=" + BMS2(7).ToString + ",IB2=" + BMS2(8).ToString + " ,BT2=" + BMS2(10).ToString + ",SOC2=" + BMS2(14).ToString + ",SOH2=" + BMS2(15).ToString + _
                            ",PROT2=" + BMS2(16).ToString + ",STAT2=" + BMS2(17).ToString + " ,CHG_AH2=" + (BMS2(37) * 65536 + BMS2(38)).ToString + ",DSG_AH2=" + (BMS2(39) * 65536 + BMS2(40)).ToString + ",CYCLE2=" + BMS2(41).ToString + _
                             ",VC1_MAX=" + VC1_MAX.ToString + ",VC1_MIN=" + VC1_MIN.ToString + " ,VC2_MAX=" + VC2_MAX.ToString + ",VC2_MIN=" + VC2_MIN.ToString + ",BT1_2=" + BMS1(11).ToString + ",BT2_2=" + BMS2(11).ToString + ",BMS1_fw='" + BMS1_fw + "',BMS2_fw='" + BMS2_fw + "'" + _
                            " where tagid=" + ChargerClient(i).tag_id.ToString
                        sqlCommand.CommandText = Query
                        sqlCommand.ExecuteNonQuery()
                    Catch ex As Exception
                        settext("EQ_BQ " + Query)
                    End Try
                Else
                    Query = "update  `charger_status`  set AutoStatus =-2" + _
                     " where tagid=" + ChargerClient(i).tag_id.ToString
                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()
                    ChargerClient(i).ReadOK = False
                End If


            Catch ex As Exception

            End Try
        Next
        '處理AGV車子電池異常




        oConn.Close()
        oConn.Dispose()

    End Sub

    Private Sub Button26_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button26.Click
        comQGWrapper.UpdateSV(GEM_SC_STATE, GEM.SC_PAUSING)
        For i As Integer = 0 To car_no - 1
            Car(i).online = False
        Next
        Button7.Text = "OFFLINE"
    End Sub

 
    Dim temp As GEM.Carrier


    Private Sub PictureBox4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles agv_info.Click

    End Sub
    Dim showType As Integer = 0
    Dim view_eq_idx As Integer = 0
    Private Sub PictureBox4_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles agv_info.Paint

        'RM
        ' Dim pen_ As Pen
        'Dim mycolor1 As New SolidBrush(Color.Green)   '定義字體顏色
        Dim x As Integer = 10
        Dim y As Integer = 5
        Dim offset As Integer = 80
        If showType = 1 Then
            'AGV RM
            e.Graphics.DrawString("AGV info", New Font("Gill Sans MT", 12, FontStyle.Regular), New SolidBrush(Color.Green), x, y) '畫布寫上字串
            y += 30
            e.Graphics.DrawString("AGVID", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(Car(view_car_idx).device_no.ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("Location:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(Car(view_car_idx).get_tagId().ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            e.Graphics.DrawString("(" + Car(view_car_idx).AXIS_X.ToString + "," + Car(view_car_idx).AXIS_Y.ToString + "," + Car(view_car_idx).AXIS_Z.ToString + ")", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset + 40, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("CarrierID:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(Car(view_car_idx).get_cstid().ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("Cmd:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(Car(view_car_idx).subcmd, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            Dim b1 As Double
            If Car(view_car_idx).BMS1(8) > 32765 Then
                b1 = -(65535 - Car(view_car_idx).BMS1(8)) / 10
            Else
                b1 = Car(view_car_idx).BMS1(8) / 10
            End If
            If Car(view_car_idx).BMS2(8) > 32765 Then
                b1 += -(65535 - Car(view_car_idx).BMS2(8)) / 10
            Else
                b1 += Car(view_car_idx).BMS2(8) / 10
            End If

            e.Graphics.DrawString("SOC: ", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(Car(view_car_idx).BMS1(14).ToString + "%  " + Car(view_car_idx).BMS2(14).ToString + "%(" + Round(b1, 1).ToString + " A)", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18

            e.Graphics.DrawString("T1:" + Car(view_car_idx).T1.ToString + "      T2:" + Car(view_car_idx).T2.ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("T3:" + Car(view_car_idx).T3.ToString + "      T4:" + Car(view_car_idx).T4.ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            y += 18
            'Fork HP
            e.Graphics.DrawString("StayTime:" + DateDiff(DateInterval.Second, CDate(Car(view_car_idx).Pre_TagID_time), CDate(Now())).ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("Area:" + Car(view_car_idx).Site, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串

            Dim rect As New Rectangle(x, y, 15, 15)
            Dim iolist(11) As String
            Dim IOdata(11) As Boolean
            If Car(view_car_idx).status > -2 Then


                iolist(0) = "FORK HP"
                IOdata(0) = If((Car(view_car_idx).device_status(31) >> 2) Mod 2 = 1, True, False)

                iolist(1) = "LFT HP"
                IOdata(1) = If((Car(view_car_idx).device_status(30) >> 8) Mod 2 = 1, True, False)
                iolist(2) = "TURN HP"
                IOdata(2) = If((Car(view_car_idx).device_status(30) >> 13) Mod 2 = 1, True, False)
                iolist(3) = "橫移 HP"
                IOdata(3) = If((Car(view_car_idx).device_status(30) >> 5) Mod 2 = 1, True, False)

                iolist(4) = "Auto"
                IOdata(4) = If(Car(view_car_idx).get_auto = 0 And Car(view_car_idx).flag, True, False)
                iolist(5) = "Error"
                IOdata(5) = If(Car(view_car_idx).get_Err() > 0, True, False)

                iolist(6) = "Online"
                IOdata(6) = If(Car(view_car_idx).status < 0 Or Car(view_car_idx).flag = False, False, True)
                iolist(7) = "Busy"
                IOdata(7) = If(Car(view_car_idx).device_status(14) > 0, True, False)
                iolist(8) = "Load"
                IOdata(8) = If(Car(view_car_idx).get_loading >= 3, True, False)
                iolist(9) = "RUN"
                IOdata(9) = If(Car(view_car_idx).device_status(15) > 0, True, False)
                iolist(10) = "Map"
                IOdata(10) = If(Car(view_car_idx).device_status(13) > 0, True, False)
                iolist(11) = ""
            End If
            For i As Integer = 0 To (iolist.Length) / 3 - 1
                y += 20
                x = 5
                For j As Integer = 0 To 2
                    rect = New Rectangle(x, y, 15, 15)
                    If IOdata(i * 3 + j) Then
                        e.Graphics.FillEllipse(New SolidBrush(Color.Green), rect)
                    End If

                    e.Graphics.DrawEllipse(New Pen(Color.Black, 1), rect)

                    e.Graphics.DrawString(iolist(i * 3 + j), New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.Blue), x + 20, y) '畫布寫上字串
                    x += 80
                Next


            Next
        ElseIf showType = 2 Then
            'EQ
            Dim PORT_STN As Tag_Point = New Tag_Point
            For i As Integer = 0 To Tag_point_list.Length - 1
                If Tag_point_list(i).TagId = comQGWrapper.EqPort(view_eq_idx).tag_id Then
                    PORT_STN = Tag_point_list(i)
                    Exit For
                End If
            Next

            e.Graphics.DrawString("EQ info", New Font("Gill Sans MT", 12, FontStyle.Regular), New SolidBrush(Color.Green), x, y) '畫布寫上字串
            y += 30
            e.Graphics.DrawString("PortID:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(comQGWrapper.EqPort(view_eq_idx).PortID, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("PortNo:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(PORT_STN.stkval, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            Dim idx As Integer = comQGWrapper.CST_SearchByLoc(comQGWrapper.EqPort(view_eq_idx).PortID)

            y += 18
            If idx > -1 Then

                e.Graphics.DrawString("CST:" + comQGWrapper.CST(idx).CarrierID, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            Else

                e.Graphics.DrawString("CST:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            End If
            y += 18
            e.Graphics.DrawString("Share:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(PORT_STN.site.ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("Tagid:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(comQGWrapper.EqPort(view_eq_idx).tag_id.ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18

            e.Graphics.DrawString("IP:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(comQGWrapper.EqPort(view_eq_idx).ip.ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            ' y += 18
            ' e.Graphics.DrawString("Location:", New Font("Microsoft JhengHei", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            ' e.Graphics.DrawString(Car(view_car_idx).get_tagId().ToString, New Font("Microsoft JhengHei", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + 60, y) '畫布寫上字串
            ' e.Graphics.DrawString(Car(view_car_idx).AXIS_X.ToString, New Font("Microsoft JhengHei", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + 120, y) '畫布寫上字串
            ' e.Graphics.DrawString(Car(view_car_idx).AXIS_Y.ToString, New Font("Microsoft JhengHei", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + 150, y) '畫布寫上字串
            'e.Graphics.DrawString(Car(view_car_idx).AXIS_Z.ToString, New Font("Microsoft JhengHei", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + 180, y) '畫布寫上字串

            'y += 18
            'e.Graphics.DrawString("Cmd:", New Font("Microsoft JhengHei", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串

            y += 20
            'Fork HP

            Dim rect As New Rectangle(x, y, 15, 15)
            Dim iolist(8) As String
            Dim IOdata(8) As Boolean
            iolist(0) = "L-Req"
            If comQGWrapper.EqPort(view_eq_idx).LoadAvail = 0 Then
                IOdata(0) = True
            End If
            iolist(1) = "UL-Req"
            If comQGWrapper.EqPort(view_eq_idx).UnLoadAvail = 0 Then
                IOdata(1) = True
            End If

            iolist(2) = "Ready"
            iolist(3) = "Online"
            If comQGWrapper.EqPort(view_eq_idx).ONLINE = 1 Then
                IOdata(3) = True
            End If
            iolist(4) = "TR-req"
            iolist(5) = "Busy"
            iolist(6) = "Comp"
            iolist(7) = "Err"
            If comQGWrapper.EqPort(view_eq_idx).ERR = 1 Then
                IOdata(7) = True
            End If
            iolist(8) = ""
            For i As Integer = 0 To (iolist.Length) / 3 - 1
                y += 20
                x = 5
                For j As Integer = 0 To 2

                    If Not iolist(i * 3 + j) = "" Then
                        rect = New Rectangle(x, y, 15, 15)
                        If IOdata(i * 3 + j) Then
                            e.Graphics.FillEllipse(New SolidBrush(Color.Green), rect)
                        End If
                        '  e.Graphics.FillEllipse(New SolidBrush(Color.Green), rect)
                        e.Graphics.DrawEllipse(New Pen(Color.Black, 1), rect)
                        e.Graphics.DrawString(iolist(i * 3 + j), New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.Blue), x + 20, y) '畫布寫上字串
                        x += 80
                    End If

                Next


            Next
        ElseIf showType = 3 Then
            ' shelf 
            Dim SHELF_STN As Tag_Point = New Tag_Point
            For i As Integer = 0 To Tag_point_list.Length - 1
                If Tag_point_list(i).TagId = comQGWrapper.ShelfData(view_shelf_idx).tag_id Then
                    SHELF_STN = Tag_point_list(i)
                    Exit For
                End If
            Next
            e.Graphics.DrawString("Shelf info", New Font("Gill Sans MT", 12, FontStyle.Regular), New SolidBrush(Color.Green), x, y) '畫布寫上字串
            y += 30
            e.Graphics.DrawString("Shelf:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(comQGWrapper.ShelfData(view_shelf_idx).Shelf_Loc, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18

            e.Graphics.DrawString("PortNo:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(SHELF_STN.stkval.ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18

            e.Graphics.DrawString("Share:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(SHELF_STN.site.ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("Carrier:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串

            e.Graphics.DrawString(comQGWrapper.ShelfData(view_shelf_idx).CarrierID, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串

     

        y += 18
        e.Graphics.DrawString("Zone_Name:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
        e.Graphics.DrawString(comQGWrapper.ShelfData(view_shelf_idx).Zone_Name.ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
        y += 18
        e.Graphics.DrawString("Status:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
        e.Graphics.DrawString(comQGWrapper.ShelfData(view_shelf_idx).Shelf_Status.ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
        y += 18
        e.Graphics.DrawString("Tagid:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
        e.Graphics.DrawString(comQGWrapper.ShelfData(view_shelf_idx).tag_id.ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
        y += 18

        ElseIf showType = 4 Then
            '充電站

            ChargerClient(view_charger_idx).HoldingResponse(0) = 0
            Dim Status As String = "Auto"
            If ChargerClient(view_charger_idx).HoldingResponse(18) = 0 Then
                Status = "Manual"
            ElseIf ChargerClient(view_charger_idx).HoldingResponse(18) = 2 Then
                Status = "ManualCharge"
            End If
            e.Graphics.DrawString(ChargerClient(view_charger_idx).EQ_ID.ToString, New Font("Gill Sans MT", 12, FontStyle.Regular), New SolidBrush(Color.Green), x, y) '畫布寫上字串
            y += 30
            e.Graphics.DrawString("IP:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(ChargerClient(view_charger_idx).ipadress, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("Position :", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(ChargerClient(view_charger_idx).HoldingResponse(7).ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("Temperature:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(ChargerClient(view_charger_idx).HoldingResponse(3).ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("Line Temp:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(ChargerClient(view_charger_idx).HoldingResponse(8).ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("Fork Temp:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(((ChargerClient(view_charger_idx).HoldingResponse(27) << 16) + ChargerClient(view_charger_idx).HoldingResponse(26)).ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("OUT V:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(((ChargerClient(view_charger_idx).HoldingResponse(21) << 16) + ChargerClient(view_charger_idx).HoldingResponse(20)).ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("OUT A:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(((ChargerClient(view_charger_idx).HoldingResponse(23) << 16) + ChargerClient(view_charger_idx).HoldingResponse(22)).ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("Status:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(Status, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("Error:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(ChargerClient(view_charger_idx).HoldingResponse(19).ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("Connection:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(ChargerClient(view_charger_idx).ReadOK.ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18
            e.Graphics.DrawString("Tagid:", New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x, y) '畫布寫上字串
            e.Graphics.DrawString(ChargerClient(view_charger_idx).tag_id.ToString, New Font("Gill Sans MT", 10, FontStyle.Regular), New SolidBrush(Color.WhiteSmoke), x + offset, y) '畫布寫上字串
            y += 18


        End If


    End Sub

    Private Sub btn_OnlineLocal_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btn_OnlineLocal.Click
        comQGWrapper.UpdateSV(GEM_CONTROL_STATE, GEM.OnlineLoaclval)
    End Sub

    Private Sub btn_Offline_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btn_Offline.Click
        comQGWrapper.UpdateSV(GEM_CONTROL_STATE, GEM.OfflineEQval)
    End Sub

    Private Sub btn_OnLine_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btn_OnLine.Click

    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles pic_close.Click
        agv_info.Hide()
        pic_close.Hide()
        txtCar.Hide()
        From_cb.Hide()

        To_cb.Hide()
        rolldateTxt.Hide()

        SendBtn.Hide()

    End Sub

  
    'Function path_check(ByVal cmd1 As String, ByVal cmd2 As String)

    '    Dim point1() As String = cmd1.Split(",")
    '    Dim point2() As String = cmd2.Split(",")
    '    Dim idx1(point1.Length - 1) As Integer
    '    Dim idx2(point2.Length - 1) As Integer
    '    Dim ii As Integer = 0
    '    Dim jj As Integer = 0

    '    For j As Integer = 0 To idx1.Length - 1

    '        For i As Integer = 0 To Tag_point_list.Length - 1
    '            If CInt(point1(j)) = Tag_point_list(i).TagId Then
    '                idx1(ii) = i
    '                ii += 1
    '            End If
    '        Next
    '    Next
    '    For j As Integer = 0 To idx2.Length - 1
    '        For i As Integer = 0 To Tag_point_list.Length - 1
    '            If CInt(point2(j)) = Tag_point_list(i).TagId Then
    '                idx2(jj) = i
    '                jj += 1
    '            End If
    '        Next
    '    Next
    '    Dim minX1, maxX1, minY1, maxY1 As Integer
    '    Dim minX2, maxX2, minY2, maxY2 As Integer
    '    Dim len As Integer = point1.Length

    '    For i As Integer = 0 To idx1.Length - 1
    '        For j As Integer = 0 To idx2.Length - 1
    '            If Tag_point_list(idx1(i)).th = 0 Or Tag_point_list(idx1(i)).th = 180 Then
    '                minX1 = Tag_point_list(idx1(i)).X - Car(0).width / 2
    '                maxX1 = Tag_point_list(idx1(i)).X + Car(0).width / 2
    '                minY1 = Tag_point_list(idx1(i)).Y - Car(0).height / 2
    '                maxY1 = Tag_point_list(idx1(i)).Y + Car(0).height / 2
    '            ElseIf Tag_point_list(idx1(i)).th = 90 Or Tag_point_list(idx1(i)).th = -90 Then
    '                minX1 = Tag_point_list(idx1(i)).X - Car(0).height / 2
    '                maxX1 = Tag_point_list(idx1(i)).X + Car(0).height / 2
    '                minY1 = Tag_point_list(idx1(i)).Y - Car(0).width / 2
    '                maxY1 = Tag_point_list(idx1(i)).Y + Car(0).width / 2
    '            End If
    '            If Tag_point_list(idx2(j)).th = 0 Or Tag_point_list(idx2(j)).th = 180 Then
    '                minX2 = Tag_point_list(idx2(j)).X - Car(0).width / 2
    '                maxX2 = Tag_point_list(idx2(j)).X + Car(0).width / 2
    '                minY2 = Tag_point_list(idx2(j)).Y - Car(0).height / 2
    '                maxY2 = Tag_point_list(idx2(j)).Y + Car(0).height / 2
    '            ElseIf Tag_point_list(idx2(j)).th = 90 Or Tag_point_list(idx2(j)).th = -90 Then
    '                minX2 = Tag_point_list(idx2(j)).X - Car(0).height / 2
    '                maxX2 = Tag_point_list(idx2(j)).X + Car(0).height / 2
    '                minY2 = Tag_point_list(idx2(j)).Y - Car(0).width / 2
    '                maxY2 = Tag_point_list(idx2(j)).Y + Car(0).width / 2
    '            End If


    '            If (maxX1 > minX2 And maxX2 > minX1 And maxY1 > minY2 And maxY2 > minY1) Then

    '                len = i
    '                Exit For

    '            End If
    '        Next
    '        If Not len = point1.Length Then
    '            Exit For
    '        End If
    '    Next

    '    Return String.Join(",", point1, 0, len)


    'End Function



    Private Sub From_cb_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles From_cb.TextChanged
        If IsNumeric(From_cb.Text) Then
            If CInt(From_cb.Text) >= 10 Then
                For i As Integer = 0 To Tag_point_list.Length - 1
                    If Tag_point_list(i).TagId.ToString = From_cb.Text Then
                        Dim idx As Integer = -1
                        idx = comQGWrapper.CST_SearchByLoc(Tag_point_list(i).LOC)
                        If idx > -1 Then
                            rolldateTxt.Text = comQGWrapper.CST(idx).CarrierID
                        End If
                        Exit Sub
                    End If
                Next

                'MsgBox("找不到CST")

            End If
        End If

    End Sub

 
    Private Sub From_cb_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles From_cb.SelectedIndexChanged

    End Sub

 
    Function Search_EMPTY_Shelf(ByVal FromTagid As Integer, Optional ByVal DEST As String = "", Optional ByVal blockpoint As String = "", Optional ByVal blockPath As String = "") As Integer
        Dim destlist As String = ""
        Dim cnt As Integer = 0

        Dim cmdto_list(ListView1.Items.Count - 1) As String
        For i As Integer = 0 To ListView1.Items.Count - 1
            cmdto_list(i) = ListView1.Items(i).SubItems(3).Text
        Next
        For j As Integer = 0 To comQGWrapper.ShelfData.Length - 1
                  '空棚位 ,派送命令時 更新
            If comQGWrapper.ShelfData(j).CarrierID = "" And (In_String(DEST, comQGWrapper.ShelfData(j).Zone_Name) Or DEST = "") And comQGWrapper.ShelfData(j).Shelf_Status = "" And Not comQGWrapper.ShelfData(j).tag_id = FromTagid Then
                '紀錄所有空棚位
                If Array.IndexOf(cmdto_list, comQGWrapper.ShelfData(j).tag_id.ToString) = -1 Then
                    If cnt = 0 Then
                        destlist += comQGWrapper.ShelfData(j).tag_id.ToString
                    Else
                        destlist += "," + comQGWrapper.ShelfData(j).tag_id.ToString
                    End If
                    cnt += 1
                End If
            End If
        Next
        Dim cmd As String = Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, FromTagid, destlist, blockpoint, blockPath)
        Try
            If Not cmd = "" Then
                'setCommtext("預計路徑:" + FromTagid.ToString + "->" + destlist + ">>" + cmd)
                Dim smdlist() As String = cmd.Split(",")
                'Return CInt(smdlist(smdlist.Length - 1))
                Search_EMPTY_Shelf = Tag_point_list(Tag_Point_ByTagid(Tag_point_list, CInt(smdlist(smdlist.Length - 1)))).TagId  '回傳目的地
            Else
                Search_EMPTY_Shelf = -1
                '  setCommtext("from " + FromTagid.ToString + " to" + destlist + "找不到符合的")
            End If
        Catch ex As Exception
            Search_EMPTY_Shelf = -1
            'setCommtext("from " + FromTagid.ToString + " to" + destlist + "找不到符合的")
        End Try

    End Function

    'Function Search_EMPTY_Shelf(ByVal frompoint As Integer)
    '    Dim destlist As String = ""
    '    Dim cnt As Integer = 0
    '    TextBox10.Text = ""

    '    For i As Integer = 0 To comQGWrapper.ShelfData.Length - 1
    '        TextBox10.Text += comQGWrapper.ShelfData(i).tag_id.ToString + "," + comQGWrapper.ShelfData(i).Shelf_Loc + "," + comQGWrapper.ShelfData(i).CarrierID + "," + comQGWrapper.ShelfData(i).Shelf_Status + vbCrLf
    '        If comQGWrapper.ShelfData(i).CarrierID = "" And comQGWrapper.ShelfData(i).Shelf_Status = "" And Not comQGWrapper.ShelfData(i).tag_id = frompoint Then
    '            If cnt = 0 Then
    '                destlist += comQGWrapper.ShelfData(i).tag_id.ToString
    '            Else
    '                destlist += "," + comQGWrapper.ShelfData(i).tag_id.ToString
    '            End If
    '            cnt += 1
    '        End If
    '    Next
    '    Dim cmd As String = Dijkstra_fn_ary(Dijkstra_list(0).ary, Tag_ID_List, frompoint, destlist, "")
    '    If Not cmd = "" Then
    '        Dim smdlist() As String = cmd.Split(",")
    '        Return CInt(smdlist(smdlist.Length - 1))
    '    End If
    '    Return -1
    'End Function

   
    Function Serach_Shelf_ByTagId(ByVal Tagid As Integer) As Integer
        Serach_Shelf_ByTagId = -1
        For i As Integer = 0 To comQGWrapper.ShelfData.Length - 1
            If comQGWrapper.ShelfData(i).tag_id = Tagid Then
                Serach_Shelf_ByTagId = i

            End If
        Next

    End Function

    Sub Del_CMD(ByVal carno As Integer)
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        oConn = New MySqlConnection(Mysql_str)
        oConn.Open()
        sqlCommand.Connection = oConn
        Dim Query As String = "delete from agv_cmd_list where `AGVNo`=" + carno.ToString + " and CmdFrom=4"
        sqlCommand.CommandText = Query
        sqlCommand.ExecuteNonQuery()
        oConn.Close()
        oConn.Dispose()
    End Sub


  

    Private Sub AGVIO1_03_Click(sender As System.Object, e As System.EventArgs) Handles AGVIO1_02.Click

    End Sub

    Private Sub Label113_Click(sender As System.Object, e As System.EventArgs) Handles AGVIO1_06.Click

    End Sub

    Private Sub Label107_Click(sender As System.Object, e As System.EventArgs) Handles AGVIO1_13.Click

    End Sub

    Private Sub Button16_Click_4(sender As System.Object, e As System.EventArgs) Handles Button16.Click

        If showType = 1 Then
            Update_SQL("update `point` set `X`=" + Car(view_car_idx).AXIS_X.ToString + ",`Y`=" + Car(view_car_idx).AXIS_Y.ToString + ",th=" + Car(view_car_idx).AXIS_Z.ToString + " where tag_id=" + Car(view_car_idx).get_tagId().ToString)
            For i As Integer = 0 To Tag_point_list.Length - 1
                If Tag_point_list(i).TagId = Car(view_car_idx).get_tagId() Then
                    Tag_point_list(i).X = Car(view_car_idx).AXIS_X
                    Tag_point_list(i).Y = Car(view_car_idx).AXIS_Y
                    Tag_point_list(i).th = Car(view_car_idx).AXIS_Z

                End If
            Next
        ElseIf showType = 2 Then
            'eq
            'comQGWrapper.EqPort(view_eq_idx)

        ElseIf showType = 3 Then
            'shelf
            If Button16.Text = "Lock" Then
                Dim cst_idx As Integer

                comQGWrapper.ShelfData(view_shelf_idx).Shelf_Status = "X"
                cst_idx = comQGWrapper.CST_SearchByLoc(comQGWrapper.ShelfData(view_shelf_idx).Shelf_Loc)
                If cst_idx > -1 Then
                    comQGWrapper.CST(cst_idx).CarrierState = GEM.CS_BLOCKED
                End If


                Update_SQL("update `shelf` set Shelf_Status='X' where tag_id=" + comQGWrapper.ShelfData(view_shelf_idx).tag_id.ToString)
                Button16.Text = "Unlock"
            Else
                Dim cst_idx As Integer
                comQGWrapper.ShelfData(view_shelf_idx).Shelf_Status = ""
                Update_SQL("update `shelf` set Shelf_Status='' where tag_id=" + comQGWrapper.ShelfData(view_shelf_idx).tag_id.ToString)
                Button16.Text = "Lock"
                cst_idx = comQGWrapper.CST_SearchByLoc(comQGWrapper.ShelfData(view_shelf_idx).Shelf_Loc)
                If cst_idx > -1 Then
                    comQGWrapper.CST(cst_idx).CarrierState = GEM.CS_STOREDCOMPLETED
                End If

            End If
        ElseIf showType = 4 Then
            '開關
            '1.重啟智能充電器
            '2.遠端RESET
            Dim val(1) As Integer
            val(0) = 2
            ChargerClient(view_charger_idx).Write_HoldingReg(1170, 1, val)

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
    Private Sub PictureBox1_MouseWheel(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles PictureBox1.MouseWheel
        If e.Delta > 0 Then
            AGVratio += 0.002
            ratio.Text = AGVratio.ToString
            'MsgBox(AGVratio)
        Else

            AGVratio -= 0.002
            ratio.Text = AGVratio.ToString
            'MsgBox(AGVratio)
        End If
        Me.PictureBox1.Invalidate()
    End Sub

   
    Private Sub TransferToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles TransferToolStripMenuItem.Click
        transfer.Mysql_str = Mysql_str

        transfer.Show()

    End Sub
   
  
  

    Private Sub TabPage1_Click(sender As System.Object, e As System.EventArgs) Handles TabPage1.Click

    End Sub
    Dim debug_subcmd As Boolean = False
    Private Sub Button14_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs)



    End Sub

    'Private Sub Button20_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button20.Click
    '    If a.tcp_client Is Nothing Then
    '        a.init("192.168.8.70", 502, 1100)
    '    Else
    '        If a.tcp_client.Connected = False Then
    '            a.init("192.168.8.70", 502, 1100)
    '        End If
    '    End If
    'End Sub
    'Private Sub Button28_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button28.Click

    '    setCommtext(int2str(a.Response, 0, 59))


    'End Sub
    Private Sub Button24_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button24.Click
        Dim idx As Integer = -1
        idx = comQGWrapper.CST_SearchByCSTID(CSTList.Text)
        If idx > -1 And Not ToLocList.Text = "" Then

            comQGWrapper.CST(idx).CommandID = comQGWrapper.gettime
            comQGWrapper.CST(idx).DEST = ToLocList.Text
            comQGWrapper.CST(idx).PRIORITY = 50
            comQGWrapper.CST(idx).TransferState = 1
        End If

    End Sub


    Private Sub Button29_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

    Private Sub CarrierToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CarrierToolStripMenuItem.Click


        Carrier.Show()
    End Sub

    Private Sub Button20_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs)
        MsgBox(Search_EMPTY_Shelf(4003, "", AllBlockPoint + "," + Car(0).Block_Point))
    End Sub

    Private Sub Button30_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button30.Click
        If IsNumeric(AGV_SetNo.Text) Then
            Dim flag As Boolean = False
            For i As Integer = 0 To Car.Length - 1
                If CInt(AGV_SetNo.Text) = Car(i).device_no Then
                    Car(i).Recharge_volt = CInt(Recharge_volt.Text)
                    Car(i).Recharge_SOC = CInt(Recharge_SOC.Text)
                    Car(i).Recharge_Point_list = Recharge_Point.Text
                    Car(i).wait_point = CInt(wait_point.Text)
                    Car(i).Block_Point = block_point.Text
                    Car(i).Block_Path = block_Path.Text
                    Car(i).width = CInt(setwidth.Text)
                    Car(i).height = CInt(Setheight.Text)
                    Car(i).ReverseXY = CInt(ReverseXY.Text)
                    Car(i).offset_X = CInt(SetOffset_X.Text)
                    Car(i).offset_Y = CInt(SetOffset_Y.Text)
                    Car(i).MaxPath = CInt(MaxPath.Text)
                    Car(i).RePath = CInt(RePath.Text)
                    Car(i).RetreatPath = CInt(RetreatPath.Text)
                    Car(i).Site = SiteTxt.Text
                    Dim Query As String = "update agv_list set Recharge_volt=" + Recharge_volt.Text + ",Recharge_SOC=" + Recharge_SOC.Text + ",Recharge_Point='" + Recharge_Point.Text + "',wait_point=" + wait_point.Text + _
                        ",block_point='" + block_point.Text + "',block_Path='" + block_Path.Text + "',car_site='" + SiteTxt.Text + "',width=" + Setwidth.Text + ",height=" + Setheight.Text + ",ReverseXY=" + ReverseXY.Text + _
                        ",offset_X=" + SetOffset_X.Text + ",offset_Y=" + SetOffset_Y.Text + ",MaxPath=" + MaxPath.Text + ",RePath=" + RePath.Text + ",RetreatPath=" + RetreatPath.Text + _
                        " where AGVNo=" + Car(i).device_no.ToString
                    MsgBox("更新回報:" + Update_SQL(Query).ToString)

                    flag = True
                End If
            Next
            If flag = False Then
                MsgBox("找不到車輛")
            End If
        Else
            MsgBox("AGV No錯誤")
        End If


    End Sub

    Private Sub AGV_SetNo_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AGV_SetNo.SelectedIndexChanged
        For i As Integer = 0 To Car.Length - 1
            If CInt(AGV_SetNo.Text) = Car(i).device_no Then
                Recharge_volt.Text = Car(i).Recharge_volt.ToString
                Recharge_SOC.Text = Car(i).Recharge_SOC.ToString
                Recharge_Point.Text = Car(i).Recharge_Point_list
                wait_point.Text = Car(i).wait_point.ToString
                block_point.Text = Car(i).Block_Point.ToString
                Setwidth.Text = Car(i).width.ToString
                Setheight.Text = Car(i).height.ToString
                ReverseXY.Text = Car(i).ReverseXY.ToString
                SetOffset_X.Text = Car(i).offset_X.ToString
                SetOffset_Y.Text = Car(i).offset_Y.ToString
                MaxPath.Text = Car(i).MaxPath.ToString
                RetreatPath.Text = Car(i).RetreatPath.ToString
                RePath.Text = Car(i).RePath.ToString
                block_Path.Text = Car(i).Block_Path.ToString
                SiteTxt.Text = Car(i).Site.ToString
            End If
        Next

    End Sub

    Private Sub ShelfToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        shelfForm.Show()
    End Sub

    Private Sub Button28_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)


    End Sub
    Dim int_x As Integer = 0
    Dim int_y As Integer = 0
    Dim move_flag As Boolean = False
    Dim map_offset_X As Integer = 0
    Dim map_offset_Y As Integer = 0
    Private Sub PictureBox1_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBox1.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Left Then
            int_x = e.X
            int_y = e.Y
            move_flag = True
        End If
    End Sub

    Private Sub PictureBox1_MouseUp(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBox1.MouseUp
        move_flag = False
    End Sub



    Private Sub PictureBox1_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles PictureBox1.MouseMove
        If move_flag Then
            map_offset_X -= CInt(e.X - int_x)
            map_offset_Y -= CInt(e.Y - int_y)
            MapX.Text = map_offset_X
            MapY.Text = map_offset_Y
            int_x = e.X
            int_y = e.Y
            '  settext(CInt(e.X - int_x).ToString + " " + map_offset_X.ToString)
            Me.PictureBox1.Invalidate()
        End If

    End Sub

    Private Sub PictureBox1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox1.Click

    End Sub

    Private Sub Button35_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button35.Click

    End Sub

    Private Sub DeleteToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DeleteToolStripMenuItem.Click
        If Me.ListView2.SelectedItems.Count = 1 Then
            ' MsgBox(Me.ListView1.SelectedItems(0).SubItems(0).Text)


            Dim idx As Integer = -1
            idx = comQGWrapper.CST_SearchByCSTID(Me.ListView2.SelectedItems(0).Text)
            If (idx > -1) Then
                Dim Query As String = "Delete from mcs_cmd_list where  CARRIERID='" + comQGWrapper.CST(idx).CarrierID + "' or REQUEST_TIME < '" + Now.AddDays(-1).ToString("yyyy-MM-dd HH:mm:ss") + "'"
                Update_SQL(Query)
                Query = "update mcs_cmd_history  set End_time=now() where  COMMANDID='" + comQGWrapper.CST(idx).CommandID + "'"
                Update_SQL(Query)

                comQGWrapper.CST(idx).DEST = ""
                comQGWrapper.CST(idx).PRIORITY = 0
                comQGWrapper.CST(idx).mcstime = 0
                comQGWrapper.CST(idx).CommandID = ""
                comQGWrapper.CST(idx).CarrierState = GEM.CS_STOREDCOMPLETED
                comQGWrapper.EventReportSendOb(GEM.EVENT_TransferAbortInitiated, comQGWrapper.CST(idx))
                comQGWrapper.EventReportSendOb(GEM.EVENT_TransferAbortCompleted, comQGWrapper.CST(idx))
            End If
            For i As Integer = ListView2.SelectedIndices.Count - 1 To 0 Step -1
                ListView2.Items.RemoveAt(ListView2.SelectedIndices(i))
            Next
        Else
            MsgBox("請先選擇命令")
        End If
    End Sub

    Private Sub Button29_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Del_CMD(3)
    End Sub
    Private Sub Button40_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        MsgBox(Check_Path("3003,4003", "1", 260, 75, Tag_point_list, 5120 - 99999, 99999 + 70, 0))
    End Sub

    Private Sub AddpwtToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AddpwtToolStripMenuItem.Click
        If Me.ListView2.SelectedItems.Count = 1 Then
            ' MsgBox(Me.ListView1.SelectedItems(0).SubItems(0).Text)


            Dim idx As Integer = -1
            idx = comQGWrapper.CST_SearchByCSTID(Me.ListView2.SelectedItems(0).Text)
            If (idx > -1) Then
                comQGWrapper.CST(idx).mcstime -= 10000
            End If

        Else
            MsgBox("請先選擇命令")
        End If
    End Sub
    Function GetCarPoint(ByVal tagid As Integer, ByVal PointTh As Integer, ByVal width As Integer, ByVal height As Integer) As String
        GetCarPoint = "1"
        Dim idx As Integer = -1
        For i As Integer = 0 To Tag_point_list.Length - 1
            If Tag_point_list(i).TagId = tagid Then
                idx = i
            End If
        Next
        If idx > -1 Then


            Dim X(3) As Integer
            Dim Y(3) As Integer
            Dim PointX As Integer = Tag_point_list(idx).X
            Dim PointY As Integer = Tag_point_list(idx).Y
            X(0) = (width / 2) * Math.Cos(PointTh * Math.PI / 180) + (height / 2) * Math.Sin(PointTh * Math.PI / 180) + PointX
            Y(0) = -(width / 2) * Math.Sin(PointTh * Math.PI / 180) + (height / 2) * Math.Cos(PointTh * Math.PI / 180) + PointY
            X(1) = (width / 2) * Math.Cos(PointTh * Math.PI / 180) + (-height / 2) * Math.Sin(PointTh * Math.PI / 180) + PointX
            Y(1) = -(width / 2) * Math.Sin(PointTh * Math.PI / 180) + (-height / 2) * Math.Cos(PointTh * Math.PI / 180) + PointY
            X(3) = (-width / 2) * Math.Cos(PointTh * Math.PI / 180) + (height / 2) * Math.Sin(PointTh * Math.PI / 180) + PointX
            Y(3) = -(-width / 2) * Math.Sin(PointTh * Math.PI / 180) + (height / 2) * Math.Cos(PointTh * Math.PI / 180) + PointY
            X(2) = (-width / 2) * Math.Cos(PointTh * Math.PI / 180) + (-height / 2) * Math.Sin(PointTh * Math.PI / 180) + PointX
            Y(2) = -(-width / 2) * Math.Sin(PointTh * Math.PI / 180) + (-height / 2) * Math.Cos(PointTh * Math.PI / 180) + PointY
            Dim minX, maxX, minY, maxY As Integer
            Array.Sort(X)
            Array.Sort(Y)
            minX = X(0)
            maxX = X(3)
            minY = Y(0)
            maxY = Y(3)
            Dim list(Tag_point_list.Length) As Integer
            For i As Integer = 0 To Tag_point_list.Length - 1
                If Tag_point_list(i).X >= minX And Tag_point_list(i).X <= maxX And Tag_point_list(i).Y >= minY And Tag_point_list(i).Y <= maxY Then
                    GetCarPoint += "," + Tag_point_list(i).TagId.ToString
                End If
            Next
        End If
    End Function
    Function BitCheck(ByVal val As Integer, ByVal offset As Integer) As Boolean
        BitCheck = False
        If (val >> offset) Mod 2 = 1 Then
            BitCheck = True
        End If
    End Function

    Function TagID_Dis(ByVal StartPoint As Integer, ByVal Loc As String, Optional ByVal cartyno As Integer = 3) As Long
        TagID_Dis = 100000
        Dim S_idx As Integer = -1
        Dim E_idx As Integer = -1

        If StartPoint > 0 And Not Loc = "" Then


            For i As Integer = 0 To Tag_point_list.Length - 1
                If StartPoint = Tag_point_list(i).TagId Then
                    S_idx = i
                    Exit For
                End If

            Next
            For i As Integer = 0 To Tag_point_list.Length - 1
                If Loc = Tag_point_list(i).LOC Then
                    E_idx = i
                    Exit For
                End If
            Next
            If S_idx > -1 And E_idx > -1 Then
                If cartyno > 2 Then
                    TagID_Dis = distance(Tag_point_list(S_idx).X, Tag_point_list(E_idx).X, Tag_point_list(S_idx).Y, Tag_point_list(E_idx).Y) * 10
                Else
                    TagID_Dis = Math.Abs(Tag_point_list(S_idx).TagId - Tag_point_list(E_idx).TagId) * 1000
                End If

            End If

        End If


    End Function
    Function distance(ByVal X1 As Long, ByVal X2 As Long, ByVal Y1 As Long, ByVal Y2 As Long) As Long
        Return Sqrt(Abs(X1 - X2) ^ 2 + Abs(Y1 - Y2) ^ 2)
    End Function
    Function short_path(ByVal maincmd As String, ByVal fastcmd As String, Optional ByVal maxlen As Integer = 15) As String
        short_path = ""

        Dim main() As String = maincmd.Split(",")
        Dim fast() As String = fastcmd.Split(",")
        Dim startidx As Integer = 0
        startidx = Math.Max(main.Length, fast.Length)
        For i As Integer = 0 To Math.Min(main.Length - 1, fast.Length - 1)
            If Not main(i) = fast(i) Then
                startidx = i
                Exit For
            End If
        Next
        If main(main.Length - 1) = fast(fast.Length - 1) Then
            '至少終點一致

            For i As Integer = fast.Length - 1 To 0 Step -1
                If i >= maxlen Then
                    i = maxlen - 1
                End If
                For j As Integer = startidx To main.Length - 1
                    If fast(i) = main(j) Then
                        '相同
                        ' If Not i = j Then
                        Return String.Join(",", fast, 0, i + 1)
                        ' End If

                    End If
                Next
            Next
        End If
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
  
    Private Sub Button4_Click_2(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        If showType = 4 Then
            '開關
            '1.重啟智能充電器
            '2.遠端RESET
            Dim val(1) As Integer
            val(0) = 1
            ChargerClient(view_charger_idx).Write_HoldingReg(1170, 1, val)
        Else
            MsgBox("請選擇充電站!!")
        End If

    End Sub

    Private Sub Button5_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        If testval = 0 Then
            testval = 2

        Else
            testval = 0
        End If
    End Sub

    Private Sub BG_Update_DoWork(ByVal sender As System.Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles BG_Update.DoWork
        Dim oConn As MySqlConnection
        Dim sqlCommand As New MySqlCommand
        oConn = New MySqlConnection(Mysql_str)
        'Dim Query As String = "SELECT CmdKey,AGVno,CmdFrom,CmdTo,Pri_Wt,CMD_Status,RequestTime,Requestor FROM `agv_cmd_list` where 1 order by Pri_Wt DESC "
        Dim i As Integer = 0
        Dim Query As String = ""
        oConn.Open()
        sqlCommand.Connection = oConn
        Try
            For i = 0 To car_no - 1
                If Car(i).flag = True Then
                    Dim temp_Shelf_Car_No As String = ""

                    If Car(i).get_Shelf_Car_No > 0 And Agvc_shelfcheck.Checked = True Then
                        temp_Shelf_Car_No = Car(i).get_Shelf_Car_No.ToString
                    ElseIf Car(i).cmd_Shelf_Car_No > 0 Then
                        temp_Shelf_Car_No = Car(i).cmd_Shelf_Car_No.ToString
                    Else
                        temp_Shelf_Car_No = " 0 "
                    End If
                    Dim VC1_MAX As Integer = Car(i).GetVcMax(Car(i).BMS1)
                    Dim VC1_MIN As Integer = Car(i).GetVcMin(Car(i).BMS1)
                    Dim VC2_MAX As Integer = Car(i).GetVcMax(Car(i).BMS2)
                    Dim VC2_MIN As Integer = Car(i).GetVcMin(Car(i).BMS2)
                    'Query = "update  `agv_list` set CmdKey=" + Car(i).cmd_sql_idx.ToString + ",carWork=" + Car(i).device_status(6).ToString + ",AGVAction='" + Car(i).get_pin().ToString + "',"
                    'Query += "Status='" + Car(i).status.ToString + "',Position=" + Car(i).get_tagId().ToString + ",ErrorCode='" + Car(i).get_Err().ToString + "',"
                    'Query += "Speed=" + Car(i).get_Speed().ToString + ",BatteryVoltage=" + Car(i).get_Volt().ToString + ",Shelf_Car_No=" + temp_Shelf_Car_No + ",Loading='" + _
                    '    Car(i).get_loading.ToString + "',distance=" + Car(i).get_distance.ToString + ",Temp=" + Car(i).device_status(21).ToString + ",tag_change_time='" + _
                    '    Car(i).Pre_TagID_time.ToString("yyyy-MM-dd HH:mm:ss") + "',AGV_X=" + Car(i).AXIS_X.ToString + " ,AGV_Y=" + Car(i).AXIS_Y.ToString + ",AGV_TH=" + Car(i).AXIS_Z.ToString + _
                    '    ",VB1=" + Car(i).BMS1(7).ToString + ",IB1=" + Car(i).BMS1(8).ToString + " ,BT1=" + Car(i).BMS1(10).ToString + ",SOC1=" + Car(i).BMS1(14).ToString + ",SOH1=" + Car(i).BMS1(15).ToString + _
                    '    ",PROT1=" + Car(i).BMS1(16).ToString + ",STAT1=" + Car(i).BMS1(17).ToString + " ,CHG_AH1=" + (Car(i).BMS1(37) * 65536 + Car(i).BMS1(38)).ToString + ",DSG_AH1=" + (Car(i).BMS1(39) * 65536 + Car(i).BMS1(40)).ToString + ",CYCLE1=" + Car(i).BMS1(41).ToString + _
                    '     ",VB2=" + Car(i).BMS2(7).ToString + ",IB2=" + Car(i).BMS2(8).ToString + " ,BT2=" + Car(i).BMS2(10).ToString + ",SOC2=" + Car(i).BMS2(14).ToString + ",SOH2=" + Car(i).BMS2(15).ToString + _
                    '    ",PROT2=" + Car(i).BMS2(16).ToString + ",STAT2=" + Car(i).BMS2(17).ToString + " ,CHG_AH2=" + (Car(i).BMS2(37) * 65536 + Car(i).BMS2(38)).ToString + ",DSG_AH2=" + (Car(i).BMS2(39) * 65536 + Car(i).BMS2(40)).ToString + ",CYCLE2=" + Car(i).BMS2(41).ToString + _
                    '     ",VC1_MAX=" + VC1_MAX.ToString + ",VC1_MIN=" + VC1_MIN.ToString + " ,VC2_MAX=" + VC2_MAX.ToString + ",VC2_MIN=" + VC2_MIN.ToString + ",BT1_2=" + Car(i).BMS1(11).ToString + ",BT2_2=" + Car(i).BMS2(11).ToString + ",car_site='" + Car(i).Site.ToString + "'" + _
                    '    "  where AGVNo=" + Car(i).device_no.ToString


                    Query = "update  `agv_list` set CmdKey=" + Car(i).cmd_sql_idx.ToString + ",carWork=" + Car(i).device_status(6).ToString + ",AGVAction='" + Car(i).get_pin().ToString + "',"
                    Query += "Status='" + Car(i).status.ToString + "',Position=" + Car(i).get_tagId().ToString + ",ErrorCode='" + Car(i).get_Err().ToString + "',"
                    Query += "Speed=" + Car(i).get_Speed().ToString + ",BatteryVoltage=" + Car(i).get_Volt().ToString + ",Shelf_Car_No=" + temp_Shelf_Car_No + ",PIN=" + (Car(i).get_pin + Car(i).get_isbusy * 10).ToString + ",Loading='" + _
                        Car(i).get_loading.ToString + "',distance=" + Car(i).get_distance.ToString + ",Temp=" + Car(i).device_status(21).ToString + ",tag_change_time='" + _
                        Car(i).Pre_TagID_time.ToString("yyyy-MM-dd HH:mm:ss") + "'" + _
                        ",VB1=" + Car(i).BMS1(7).ToString + ",IB1=" + Car(i).BMS1(8).ToString + " ,BT1=" + Car(i).BMS1(10).ToString + ",SOC1=" + Car(i).BMS1(14).ToString + ",SOH1=" + Car(i).BMS1(15).ToString + _
                        ",PROT1=" + Car(i).BMS1(16).ToString + ",STAT1=" + Car(i).BMS1(17).ToString + " ,CHG_AH1=" + (Car(i).BMS1(37) * 65536 + Car(i).BMS1(38)).ToString + ",DSG_AH1=" + (Car(i).BMS1(39) * 65536 + Car(i).BMS1(40)).ToString + ",CYCLE1=" + Car(i).BMS1(41).ToString + _
                         ",VB2=" + Car(i).BMS2(7).ToString + ",IB2=" + Car(i).BMS2(8).ToString + " ,BT2=" + Car(i).BMS2(10).ToString + ",SOC2=" + Car(i).BMS2(14).ToString + ",SOH2=" + Car(i).BMS2(15).ToString + _
                        ",PROT2=" + Car(i).BMS2(16).ToString + ",STAT2=" + Car(i).BMS2(17).ToString + " ,CHG_AH2=" + (Car(i).BMS2(37) * 65536 + Car(i).BMS2(38)).ToString + ",DSG_AH2=" + (Car(i).BMS2(39) * 65536 + Car(i).BMS2(40)).ToString + ",CYCLE2=" + Car(i).BMS2(41).ToString + _
                         ",VC1_MAX=" + VC1_MAX.ToString + ",VC1_MIN=" + VC1_MIN.ToString + " ,VC2_MAX=" + VC2_MAX.ToString + ",VC2_MIN=" + VC2_MIN.ToString + ",BT1_2=" + Car(i).BMS1(11).ToString + ",BT2_2=" + Car(i).BMS2(11).ToString + ",car_site='" + Car(i).Site.ToString + "'" + _
                        "  where AGVNo=" + Car(i).device_no.ToString
                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()
                    Query = "UPDATE `agv`.`agv_bat` SET "
                    For j As Integer = 1 To 16
                        Query += " bat1VC" + j.ToString + "=" + Car(i).BMS1(17 + j).ToString + " , "
                        Query += " bat2VC" + j.ToString + "=" + Car(i).BMS2(17 + j).ToString + " , "
                    Next
                    Query += " LM_TIME=now() where AGVNo=" + Car(i).device_no.ToString
                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()
                End If

                If Car(i).Car_type = "FORK" And Car(i).cmd_Shelf_Car_No > 0 And Car(i).cmd_idx > 0 And Car(i).get_pin = 10 And Car(i).get_loading = 3 Then
                    Query = "update `shelf_car` set LOCATION=" + Car(i).get_tagId.ToString + ",updateTime=now(),updateName='List_ReNew' where Shelf_Car_No=" + Car(i).cmd_Shelf_Car_No.ToString
                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()
                ElseIf Not Car(i).Car_type = "FORK" And Car(i).get_Shelf_Car_No > 0 And Car(i).cmd_idx > 0 And Car(i).get_pin = 10 And Agvc_shelfcheck.Checked = True Then
                    Query = "update `shelf_car` set LOCATION=" + Car(i).get_tagId.ToString + ",updateTime=now(),updateName='List_ReNew' where Shelf_Car_No=" + Car(i).get_Shelf_Car_No.ToString
                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()

                ElseIf Not Car(i).Car_type = "FORK" And Car(i).cmd_Shelf_Car_No > 0 And Car(i).cmd_idx > 0 And Car(i).get_pin = 10 And Car(i).step_i > 1 Then
                    Query = "update `shelf_car` set LOCATION=" + Car(i).get_tagId.ToString + ",updateTime=now(),updateName='List_ReNew' where Shelf_Car_No=" + Car(i).cmd_Shelf_Car_No.ToString

                    sqlCommand.CommandText = Query
                    sqlCommand.ExecuteNonQuery()
                End If
            Next

            ' settext("Update SQL:" + Query)

            Query = "update `system_info` set updatetime=now() where SYSTEM_TYPE='AGVC'"

            sqlCommand.CommandText = Query
            sqlCommand.ExecuteNonQuery()
        Catch ex As Exception
            settext("Update SQL:" + Query.ToString)
            settext("Update SQL ERROR")
        End Try
        Try

            oConn.Close()
            oConn.Dispose()
        Catch ex As Exception

        End Try


    End Sub

    Private Sub 畫面縮小ToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles 畫面縮小ToolStripMenuItem.Click
        Err_lb.Left -= 600
        Label24.Left -= 600

        GroupBox3.Left -= 600
        GroupBox9.Left -= 600
        agv_info.Left -= 600
        pic_close.Left -= 600
        txtCar.Left -= 600
        From_cb.Left -= 600
        To_cb.Left -= 600
        rolldateTxt.Left -= 600
        SendBtn.Left -= 600
        Button16.Left -= 600
    End Sub

    Private Sub ZoomOutToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ZoomOutToolStripMenuItem.Click
        Err_lb.Left += 600
        Label24.Left += 600

        GroupBox3.Left += 600
        GroupBox9.Left += 600
        agv_info.Left += 600
        pic_close.Left += 600
        txtCar.Left += 600
        From_cb.Left += 600
        To_cb.Left += 600
        rolldateTxt.Left += 600
        SendBtn.Left += 600
        Button16.Left += 600
    End Sub

    Private Sub Button10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Del_CMD(1)
        'loop
        Send_CMD(Car(0).device_no, 1, 4101, "Loader", 51)
        Send_CMD(Car(0).device_no, 1, 8197, "Loader", 50)
        loader_flag = 1

    End Sub

    Private Sub Button11_Click_2(ByVal sender As System.Object, ByVal e As System.EventArgs)

        loader_flag = 0
        Update_SQL("delete from agv_cmd_list where `AGVNo`=1 ")
        Send_CMD(Car(0).device_no, 4, 4101, "Loader", 60)
    End Sub
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

    Private Sub Button10_Click_2(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button10.Click
   

    End Sub
End Class
