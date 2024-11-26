        Dim a() As String
        Dim subcmd_list() As String = car_subcmd.Split(",")
        Dim subcmd_len As Integer = subcmd_list.Length
        a = Path_Used.Split(",")


        For j As Integer = 0 To a.Length - 1
            For k As Integer = 0 To subcmd_len - 1
                If Microsoft.VisualBasic.Right(a(j), 4).PadLeft(4, "0") = Microsoft.VisualBasic.Right(subcmd_list(k), 4).PadLeft(4, "0") Then
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
