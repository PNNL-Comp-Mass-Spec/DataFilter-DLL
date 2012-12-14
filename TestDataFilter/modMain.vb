Module modMain

	Sub Main()

		TestFilter(1000, 3, 3, 0, 5, 1, True)

	End Sub

	Private Sub TestFilter(Optional ByVal intDataPointCount As Integer = 100, _
	  Optional ByVal NumPointsLeft As Integer = 3, _
	  Optional ByVal NumPointsRight As Integer = 3, _
	  Optional ByVal PolynomialDegree As Short = 0, _
	  Optional ByVal intAmplitude As Integer = 5, _
	  Optional ByVal intNoiseLevel As Integer = 1, _
	  Optional ByVal blnRandomize As Boolean = True)

		Dim dblDataUnsmoothed() As Double
		Dim dblDataSG() As Double
		Dim dblDataButterworth() As Double

		ReDim dblDataSG(intDataPointCount - 1)

		Dim intIndex As Integer

		Dim objFilter As New DataFilter.clsDataFilter()

		If intAmplitude < 1 Then intAmplitude = 1
		If intNoiseLevel < 1 Then intNoiseLevel = 1

		If blnRandomize Then Randomize()

		Console.WriteLine("Source Data", "TestFilter")
		For intIndex = 0 To intDataPointCount - 1
			dblDataSG(intIndex) = intAmplitude * (Math.Sin(intIndex / intDataPointCount * 4) + Rnd(1) / (intAmplitude / 10) * intNoiseLevel)
			If intIndex > 0.4 * intDataPointCount And intIndex < 0.7 * intDataPointCount Then
				dblDataSG(intIndex) = dblDataSG(intIndex) * Math.Abs(intIndex - 0.55 * intDataPointCount) * 2
			End If
		Next intIndex

		ReDim dblDataUnsmoothed(intDataPointCount - 1)
		ReDim dblDataButterworth(intDataPointCount - 1)

		dblDataSG.CopyTo(dblDataUnsmoothed, 0)
		dblDataSG.CopyTo(dblDataButterworth, 0)

		objFilter.SavitzkyGolayFilter(dblDataSG, 0, intDataPointCount - 1, 3, 3, 0)
		objFilter.ButterworthFilter(dblDataButterworth, 0, intDataPointCount - 1)

		Dim intIterator As Integer = 1
		Dim strFilePath As String = ""
		Dim srOutFile As System.IO.StreamWriter = Nothing
		Dim blnSuccess As Boolean
		Dim strLineOut As String

		Do
			Try
				strFilePath = "DataSmoothTest" & intIterator & ".txt"
				srOutFile = New System.IO.StreamWriter(New System.IO.FileStream(strFilePath, IO.FileMode.Create, IO.FileAccess.Write, IO.FileShare.Read))
				blnSuccess = True
			Catch ex As Exception
				intIterator += 1
			End Try
		Loop While Not blnSuccess And intIterator < 100

		Console.WriteLine("")
		strLineOut = "Unsmoothed" & ControlChars.Tab & "SavGolayFilter" & ControlChars.Tab & "ButterworthFilter"
		Console.WriteLine(strLineOut)

		If Not srOutFile Is Nothing Then srOutFile.WriteLine(strLineOut)

		For intIndex = 0 To intDataPointCount - 1
			strLineOut = dblDataUnsmoothed(intIndex) & ControlChars.Tab & dblDataSG(intIndex) & ControlChars.Tab & dblDataButterworth(intIndex)
			Console.WriteLine(strLineOut)
			If Not srOutFile Is Nothing Then srOutFile.WriteLine(strLineOut)
		Next intIndex

		If Not srOutFile Is Nothing Then srOutFile.Close()

		Console.WriteLine()
		Console.WriteLine("Note: data has been written to file " & strFilePath)

	End Sub

End Module
