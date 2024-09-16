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
