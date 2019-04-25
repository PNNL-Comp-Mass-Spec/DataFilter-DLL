Option Strict On

Public Class clsButterworthFilter

    ' The following function is from the XPRESS software, written by Jimmy Eng
    ' http://cvs.sourceforge.net/viewcvs.py/sashimi/XPRESS-RAMP/addxpress3.c?rev=1.13&view=markup

    Public Function FilterData(ByRef ZeroBased1DArray() As Single, ByVal IndexStart As Integer, ByVal IndexEnd As Integer) As Boolean

        Dim i As Integer
        Dim ii As Integer

        Dim blnProcessingDataSubset As Boolean

        Dim intDataCount As Integer

        Dim dTmpFilter() As Double
        Dim dFilteredMS() As Double     ' Filtered data out

        '
        ' Define 5th order butterworth filter w/ cut-off frequency
        ' of 0.15 where 1.0 corresponds to half the sample rate.
        '
        Const FILTER_SIZE As Integer = 6
        Dim a() As Double = New Double() {1.0, -3.4789, 5.0098, -3.6995, 1.3942, -0.2138}
        Dim b() As Double = New Double() {0.0004, 0.0018, 0.0037, 0.0037, 0.0018, 0.0004}

        If ZeroBased1DArray Is Nothing OrElse ZeroBased1DArray.Length <= 0 Then Return False


        ' Copy data from ZeroBased1DArray to dTmpFilter, only copying a certain range if IndexStart and IndexEnd are >= 0
        If IndexStart < 0 Or IndexEnd < 0 Or IndexEnd < IndexStart Then
            blnProcessingDataSubset = False

            ReDim dTmpFilter(ZeroBased1DArray.Length - 1)
            ZeroBased1DArray.CopyTo(dTmpFilter, 0)
        Else
            blnProcessingDataSubset = True

            ReDim dTmpFilter(IndexEnd - IndexStart)
            For i = IndexStart To IndexEnd
                dTmpFilter(i - IndexStart) = ZeroBased1DArray(i)
            Next i
        End If

        intDataCount = dTmpFilter.Length

        ' Pass MS profile through IIR low pass filter:
        ' y(n) = b(1)*x(n) + b(2)*x(n-1) + ... + b(nb+1)*x(n-nb)
        '        - a(2)*y(n-1) - ... - a(na+1)*y(n-na)

        ReDim dFilteredMS(intDataCount - 1)
        For i = 0 To intDataCount - 1
            dFilteredMS(i) = b(0) * dTmpFilter(i)

            For ii = 1 To FILTER_SIZE - 1
                If i - ii >= 0 Then
                    dFilteredMS(i) += b(ii) * dTmpFilter(i - ii)
                    dFilteredMS(i) -= a(ii) * dFilteredMS(i - ii)
                End If
            Next ii
        Next i

        ' Filtered sequence is reversed and re-filtered resulting
        ' in zero-phase distortion and double the filter order.

        Array.Reverse(dFilteredMS)
        dFilteredMS.CopyTo(dTmpFilter, 0)

        ReDim dFilteredMS(intDataCount - 1)
        For i = 0 To intDataCount - 1
            dFilteredMS(i) = b(0) * dTmpFilter(i)

            For ii = 1 To FILTER_SIZE - 1
                If i - ii >= 0 Then
                    dFilteredMS(i) += b(ii) * dTmpFilter(i - ii)
                    dFilteredMS(i) -= a(ii) * dFilteredMS(i - ii)
                End If
            Next ii
        Next i


        ' Filtered sequence is reversed again
        Array.Reverse(dFilteredMS)

        ' Update ZeroBased1DArray with the filtered data
        If blnProcessingDataSubset Then
            For i = 0 To intDataCount - 1
                ZeroBased1DArray(i + IndexStart) = CSng(dFilteredMS(i))
            Next i
        Else
            For i = 0 To intDataCount - 1
                ZeroBased1DArray(i) = CSng(dFilteredMS(i))
            Next i
        End If

        Return True
    
    End Function

    ' /*
    '  * Use my standard filtering routine
    '  */
    ' void FILTER_MS(double *dOrigMS,
    '         double *dFilteredMS)
    ' {
    '    int  i,
    '         iArraySize=MAX_MS_SCAN*sizeof(double);
    '    double dTmpFilter[MAX_MS_SCAN];
    ' 
    '    /*
    '     * Defines 5th order butterworth filter w/ cut-off frequency
    '     * of 0.15 where 1.0 corresponse to half the sample rate.
    '     */
    '    double a[FILTER_SIZE]={1.0000, -3.4789, 5.0098, -3.6995, 1.3942, -0.2138},
    '           b[FILTER_SIZE]={0.0004, 0.0018, 0.0037, 0.0037, 0.0018, 0.0004};
    ' 
    '    memset(dFilteredMS, 0, iArraySize);
    '    memcpy(dTmpFilter, dOrigMS, sizeof(dTmpFilter));
    ' 
    '    /*
    '     * Pass MS profile through IIR low pass filter:
    '     * y(n) = b(1)*x(n) + b(2)*x(n-1) + ... + b(nb+1)*x(n-nb)
    '     *      - a(2)*y(n-1) - ... - a(na+1)*y(n-na)
    '     */
    '    for (i=0; i<MAX_MS_SCAN; i++)
    '    {
    '       int ii;
    ' 
    '       dFilteredMS[i]=b[0]*dTmpFilter[i];
    '       for (ii=1;ii<FILTER_SIZE;ii++)
    '       {
    '          if ((i-ii)>=0)
    '          {
    '             dFilteredMS[i] += b[ii]*dTmpFilter[i-ii];
    '             dFilteredMS[i] -= a[ii]*dFilteredMS[i-ii];
    '          }
    '       }
    '    }
    ' 
    '    /*
    '     * Filtered sequence is reversed and re-filtered resulting
    '     * in zero-phase distortion and double the filter order.
    '     */
    '    for (i=0; i<MAX_MS_SCAN; i++)
    '       dTmpFilter[i]=dFilteredMS[MAX_MS_SCAN-1-i];
    ' 
    '    memset(dFilteredMS, 0, iArraySize);
    '    for (i=0; i<MAX_MS_SCAN; i++)
    '    {
    '       int ii;
    ' 
    '       dFilteredMS[i]=b[0]*dTmpFilter[i];
    '       for (ii=1;ii<FILTER_SIZE;ii++)
    '       {
    '          if ((i-ii)>=0)
    '          {
    '             dFilteredMS[i] += b[ii]*dTmpFilter[i-ii];
    '             dFilteredMS[i] -= a[ii]*dFilteredMS[i-ii];
    '          }
    '       }
    '    }
    ' 
    '    /*
    '     * Filtered sequence is reversed again
    '     */
    '    for (i=0; i<MAX_MS_SCAN; i++)
    '       dTmpFilter[i]=dFilteredMS[MAX_MS_SCAN-1-i];
    ' 
    '    memcpy(dFilteredMS, dTmpFilter, iArraySize);
    ' 
    ' } /*FILTER_MS*/
    ' 

End Class
