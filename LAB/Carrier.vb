Imports MySql.Data.MySqlClient

Public Class Carrier

    Private Sub Carrier_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Button2_Click(sender, e)

    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Carrier_list.Items.Clear()

        For i As Integer = 0 To Form1.comQGWrapper.CST.Length - 1
            If Not Form1.comQGWrapper.CST(i).CarrierID = "" Then
                Dim item As New ListViewItem()
                item.Text = Form1.comQGWrapper.CST(i).CarrierID
                item.SubItems.Add(Form1.comQGWrapper.CST(i).CarrierZoneName)
                item.SubItems.Add(Form1.comQGWrapper.CST(i).CarrierLoc)
                item.SubItems.Add(Form1.comQGWrapper.CST(i).DEST)
                Carrier_list.Items.Add(item)
            End If
        Next
        ZoneName.Items.Clear()
        SubLoc.Items.Clear()

        For i As Integer = 0 To Form1.comQGWrapper.ShelfData.Length - 1

            If ZoneName.Items.IndexOf(Form1.comQGWrapper.ShelfData(i).Zone_Name) = -1 Then
                ZoneName.Items.Add(Form1.comQGWrapper.ShelfData(i).Zone_Name)
            End If

            ' SubLoc.Items.Add(Form1.comQGWrapper.ShelfData(i).Shelf_Loc)
        Next
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        If Carrier_list.SelectedItems.Count > 0 Then
            For i As Integer = 0 To Carrier_list.SelectedItems.Count - 1
                Dim response As MsgBoxResult
                response = MsgBox("確認要刪除" + Carrier_list.SelectedItems(i).Text + "?", vbYesNo + vbExclamation, "CarrierInfo")

                If response = MsgBoxResult.Yes Then

                    Dim idx As Integer = -1
                    idx = Form1.comQGWrapper.CST_SearchByCSTID(Carrier_list.SelectedItems(i).Text)
                    If idx > -1 Then

                        For j As Integer = 0 To Form1.comQGWrapper.ShelfData.Length - 1
                            If Form1.comQGWrapper.ShelfData(j).CarrierID = Form1.comQGWrapper.CST(idx).CarrierID Then

                                Form1.comQGWrapper.ShelfData(j).CarrierID = ""
                            End If
                        Next
                        Form1.Update_SQL("delete from `carrier` where CARRIER_ID='" + Carrier_list.SelectedItems(i).Text + "'")
                        Form1.comQGWrapper.CST(idx).CarrierID = ""
                       
                    End If
                Else
                    Exit Sub
                End If
            Next

        Else
            MsgBox("請選擇要刪除的CarrierID")
        End If
        Button2_Click(sender, e)
        '
    End Sub

   

    Private Sub Carrier_list_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Carrier_list.SelectedIndexChanged
        If Carrier_list.SelectedItems.Count > 0 Then
            CarrierIDTxt.Text = Carrier_list.SelectedItems(0).Text
        End If

    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click

        Dim idx As Integer = -1
        If CarrierIDTxt.Text.Length >= 6 Then


            idx = Form1.comQGWrapper.CST_SearchByCSTID(CarrierIDTxt.Text)
            If idx = -1 Then
                '棚位 CST 建立
                For j As Integer = 0 To Form1.comQGWrapper.ShelfData.Length - 1
                    If Form1.comQGWrapper.ShelfData(j).Zone_Name = ZoneName.Text And Form1.comQGWrapper.ShelfData(j).Shelf_Loc = SubLoc.Text Then
                        If Form1.comQGWrapper.ShelfData(j).CarrierID = "" Then
                            Form1.comQGWrapper.ShelfData(j).CarrierID = CarrierIDTxt.Text
                            Form1.comQGWrapper.CST_Add(CarrierIDTxt.Text, ZoneName.Text, SubLoc.Text)
                            Dim Query As String = "insert into `carrier` (STK_NAME,CARRIER_ID,LOT_ID,LOC_NAME,LOC_TYPE,SUB_LOC,CARRIER_STATUS,STORED_TIME,REQ_TIME,UPDATE_DATE,UPDATE_BY,CREATE_DATE,CREATE_BY) " + _
                                "VALUES ('" + Form1.comQGWrapper.Eqpname + "', '" + CarrierIDTxt.Text + "', '" + CarrierIDTxt.Text + "','" + ZoneName.Text + "',1,'" + SubLoc.Text + "', '4', '', '', '', '', '', '');"

                            Dim ans As Integer
                            ans = Form1.Update_SQL(Query)
                            Form1.settext(Query + ":" + ans.ToString)
                        Else
                            MsgBox("棚位有CST")
                        End If

                    End If
                Next
                'EQ CST 建立
                For j As Integer = 0 To Form1.comQGWrapper.EqPort.Length - 1
                    If Form1.comQGWrapper.EqPort(j).PortID = ZoneName.Text Then
                        SubLoc.Text = ZoneName.Text
                        Form1.comQGWrapper.CST_Add(CarrierIDTxt.Text, ZoneName.Text, SubLoc.Text, GEM.CS_NONE)
                        Dim Query As String = "insert into `carrier` (STK_NAME,CARRIER_ID,LOT_ID,LOC_NAME,LOC_TYPE,SUB_LOC,CARRIER_STATUS,STORED_TIME,REQ_TIME,UPDATE_DATE,UPDATE_BY,CREATE_DATE,CREATE_BY) " + _
                                "VALUES ('" + Form1.comQGWrapper.Eqpname + "', '" + CarrierIDTxt.Text + "', '" + CarrierIDTxt.Text + "','" + ZoneName.Text + "',3,'" + SubLoc.Text + "', '0', '', '', '', '', '', '');"
                        Dim ans As Integer
                        ans = Form1.Update_SQL(Query)
                        Form1.settext(Query + ":" + ans.ToString)
                    End If

                Next

            Else
                MsgBox("CST重複")
            End If
        Else
            MsgBox("CST長度錯誤")
        End If

        Button2_Click(sender, e)
    End Sub

    Private Sub ZoneName_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ZoneName.SelectedIndexChanged
        SubLoc.Items.Clear()
        SubLoc.Text = ""
        For i As Integer = 0 To Form1.comQGWrapper.ShelfData.Length - 1
            If Form1.comQGWrapper.ShelfData(i).Zone_Name = ZoneName.Text And Form1.comQGWrapper.ShelfData(i).CarrierID = "" Then
                SubLoc.Items.Add(Form1.comQGWrapper.ShelfData(i).Shelf_Loc)
            End If
        Next
    End Sub

    Private Sub SubLoc_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SubLoc.SelectedIndexChanged

    End Sub

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        Dim idx As Integer = -1
        idx = Form1.comQGWrapper.CST_SearchByCSTID(CarrierIDTxt.Text)
        If idx > -1 Then
            Form1.comQGWrapper.CST(idx).CarrierState = CInt(TextBox2.Text)
            Form1.comQGWrapper.EventReportSendOb(CInt(TextBox1.Text), Form1.comQGWrapper.CST(idx))

        End If
    End Sub
End Class