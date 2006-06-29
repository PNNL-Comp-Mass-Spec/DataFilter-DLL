Option Strict Off
Option Explicit On
Module modTest
	
    Public Sub Main()
        Dim objFilter As New clsSavitkyGolay.SavitkyGolayFilter
        objFilter.TestFilter()
    End Sub

End Module