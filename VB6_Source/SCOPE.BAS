Attribute VB_Name = "SCOPE1"
'
' The Finnigan/Extrell data file format is:
'  All variable are in sun byte order.
'
' ------------------------
' | 64 byte header       |
' ------------------------
' | Variable length      |
' | sequence definition  |
' | text                 |
' ------------------------
' | Data block           |
' ------------------------
' | variable number of   |
' | trailor records      |
' ------------------------
' | trailor record       |
' | names                |
' ------------------------
' | extended data        |
' ------------------------
'
' 64 bytes header format, in c notation
' typedef struct
' {
'   char  ByteFormat;
'   char  Processor;
'   char  MagicNum;    // 0x3E
'   char  FileType;    // 0x01 = time domain
'                      // 0x02 = freq, magnitude data
'                      // 0x03 = freq, magnitude and phase
'                      // 0x25 = TIC data
'   int   RevLevel;
'   int   Descriptor;
'   long  SeqSize;
'   long  unknown;
'   long  DataSize;
'   long  TrailorRecordsSize;
'   long  TrailorRecordsNamesSize;
'   long  ExtendedDataSize
'   char  name[] = "PARAMETERS-v1.0"
' } header;
' The VB definition, without the string...
   Private Const WM_USER As Long = &H400
   Private Const EM_FORMATRANGE As Long = WM_USER + 57
   Private Const EM_SETTARGETDEVICE As Long = WM_USER + 72
   Private Const PHYSICALOFFSETX As Long = 112
   Private Const PHYSICALOFFSETY As Long = 113

   Private Declare Function GetDeviceCaps Lib "gdi32" ( _
      ByVal hdc As Long, ByVal nIndex As Long) As Long
   Private Declare Function SendMessage Lib "user32" Alias "SendMessageA" _
      (ByVal hwnd As Long, ByVal msg As Long, ByVal wp As Long, _
      LP As Any) As Long
   Private Declare Function CreateDC Lib "gdi32" Alias "CreateDCA" _
      (ByVal lpDriverName As String, ByVal lpDeviceName As String, _
      ByVal lpOutput As Long, ByVal lpInitData As Long) As Long
      
Type PeakList
   Tolerence As Double
   Peaks() As Double
End Type

Private Type Rect
   Left As Long
   Top As Long
   Right As Long
   Bottom As Long
End Type

Private Type CharRange
   cpMin As Long     ' First character of range (0 for start of doc)
   cpMax As Long     ' Last character of range (-1 for end of doc)
End Type
   
Private Type FormatRange
   hdc As Long       ' Actual DC to draw on
   hdcTarget As Long ' Target DC for determining text formatting
   rc As Rect        ' Region of the DC to draw to (in twips)
   rcPage As Rect    ' Region of the entire DC (page size) (in twips)
   chrg As CharRange ' Range of text to draw (see above declaration)
End Type

Type FinniganHeader
   ByteFormat As Byte
   Processor As Byte
   MagicNum As Byte
   FileType As Byte
   RevLevel As Integer
   Descriptor As Integer
   SeqSize As Long
   reserved As Long
   DataSize As Long
   TrailorRecordsSize As Long
   TrailorRecordsNamesSize As Long
   ExtendedDataSize As Long
End Type

Public Enum FileTypes
   ftUnknown = -1
   ftbinary = 1
   ftExtrel = 2
   ftICR2LS = 3
   ftExcite = 4
   ftACSII = 5
   ftMIDAS = 6
   ftIONSPEC = 7
   ftBRUKER = 8
   ftMIDASInt = 9
End Enum

'
' trailor record format, in c notation
' typedef struct
' {
'   char  location;   // 1 local, 0x11 in extended data
'   char  type;       // 4=int, 8=string, 44 = long array
'   int   length;     // data length in bytes
'   long  unknown;
'   char  data[8];    // data, or offset in ext data to data
' } trailor;
'
Declare Function IntRead Lib "kernel32" Alias "_hread" (ByVal hl&, ivals%, ByVal count&) As Long

Public Const key = "GAAKG7YU"
' Constants & global data allocations
' for ICR-2LS
'Residue     Monoisotopic    Average  MF
' Ala A   71.03711    71.0788         C3 H5 N1 O1
' Arg R   156.10111   156.1875
' Asn N   114.05293   114.1038
' Asp D   115.02694   115.0886
' Cys C   103.00919   103.1388
' Glu E   129.04259   129.1155
' Gly G   57.02146    57.0519
' His H   137.05891   137.1411
' Ile I   113.08406   113.1594
' Leu L   113.08406   113.1594
' Lys K   128.09496   128.1741
' Met M   131.04049   131.1926
' Orn O   114.07931   114.1472
' Phe F   147.06841   147.1766
' Pro P   97.05276    97.1167
' Ser S   87.03203    87.0782
' Thr T   101.04768   101.1051
' Trp W   186.07931   186.2132
' Tyr Y   163.06333   163.176
' Val V   99.06841    99.1326

Option Explicit
Global version As String
Global Const BUFSIZE = 2000
Global Const MAXSCOPES = 15
Global Const MaxMass = 100000
Global Const MAXCOMMANDS = 25
Global Const NUMMACROS = 10
Global Const Pie = 3.141592654
Global Const Title = 0
Global Const Xaxis = 1
Global Const Yaxis = 2
Global Const NUMBERS = 3
Global Const MAXPEAKS = 100
Type Colors
   ScopeData As Integer
   ScopeAxis As Integer
   InsetData As Integer
   InsetAxis As Integer
   Trace2Data As Integer
End Type
Type Hosts
   HostName As String
   UserID As String
   Password As String
   Path As String
   FileName As String
End Type
' Data structure used by peak picking routine
Type Peaks
   InUse As Integer             ' true if peaks values present
   PI As Integer                ' Plot structure index value who owns these peaks
   NumPeaks As Integer          ' Number of peaks
   Peak(MAXPEAKS) As Double     ' Peak values
   Xloc(MAXPEAKS) As Double     ' X axis locations
   FileName As String
   Mode As Integer
   Save2File As Integer
   Threshold As Single
End Type
' Data structure for system directories
Type Dirs
   StartupDir As String
   SetupDir As String
   MacroDir As String
   DataDir As String
   ExciteDir As String
   CalDir As String
   ExportDir As String
End Type
Type FN
   FNname As String
   FNSize As Integer
   FNbold As Integer
   FNitalic As Integer
End Type
Type SF
   Typ As Integer
   NumDigits As Integer
End Type
Type Setups
   AxisFonts(4) As FN
   Axis(2) As SF
End Type
Type Definition
   Start As Single
   Stop As Single
   Amp As Single
   DeltaAmp As Single
   MorF As Integer
End Type
Type ExciteChannel
   WaveType As Integer
   NumDefs As Integer
   OutAmp As Single
   Defs(100) As Definition
   Scans As Integer
   NumPoints As Long
   InActive As Integer
   Marker As Integer
End Type
Type ExciteStruct
   Comment As String
   NumPoints As Long
   ClockFreq As Single
   NumChannels As Integer
   Mode As Integer
   Scans As Integer
   WindowMode As Integer    ' 0=AMU, 1=Hz
   SORImode As Integer      ' 0=Hz,  1 = Energy
   SORIenergyMode As Integer  ' 0 = P-P Voltage, 1 = delta in Hz, 2 = delta in m/z
   WW As Single
   Threshold As Single
   DeltaHz As Single
   Energy As Single
   DeltaHz2 As Single
   Energy2 As Single
   MinMZ As Single
   ExclThres As Single
   PPvoltage As Single
   PPvoltage2 As Single
   MultiplePeaks As Integer
   DynamicExclusion As Integer
   PeakTolerence As Single
   M2F As Integer
   AttA As Double
   AttB As Double
   AttC As Double
   A As Double
   b As Double
   c As Double
   d As Double
   RT As Double
   NumPeaks As Integer
   RequirePairs As Integer
   RequireCS As Integer
   NumReqCS As Integer
   PairDelta As String
   PairTol As Single
   AmpTol As Single
   chan(7) As ExciteChannel
End Type
Type DefaultStruct
   NumZeroFills As Integer
   AppType As Integer
End Type
Type MacroStruct
   key As Integer
   Description As String
   InUse As Integer
   Recording As Integer
   Commands(MAXCOMMANDS) As String
   NumCommands As Integer
End Type
Type PlotStructure
   Comment As String
   iStat As Integer
   Percent As Integer
   Title As String             ' Plot title
   ZoomBox As Integer          ' Flag, true if zoom box is on
   NoYaxisNumbers As Integer   ' Flag, true if no y axis numbers are printed
   NoXaxisNumbers As Integer   ' Flag, true if no x axis numbers are printed
   NoYaxis As Integer          ' Flag, true if no Y axis is to be displayed
   Allow As Integer            ' Flag, true if we will allow user to edit parms
   FilePointer As Long         ' Byte pointer to start of data in file
   MouseX As Integer
   MouseY As Integer
   MouseXold As Integer
   MouseYold As Integer
   OriginX As Integer
   OriginY As Integer
   MaxX As Integer
   MaxY As Integer
   XaxisName As String
   YaxisName As String
   Ymin As Single
   Ymax As Single
   Xmin As Single
   Xmax As Single
   XminFull As Single
   XmaxFull As Single
   LastXmax(5) As Single
   LastXmin(5) As Single
   LastYmax(5) As Single
   LastYmin(5) As Single
   LastLevel As Integer
   NumYTicks As Integer
   NumXTicks As Integer
   XaxisZoom As Integer
   YaxisZoom As Integer
   YaxisFixed As Integer
   YaxisAutoScale As Integer
   ZeroFills As Integer
   appodize As Integer
   A As Double
   b As Double
   c As Double
   X(BUFSIZE) As Single     ' Display Buffer x axis data
   Y(BUFSIZE) As Single     ' Display Buffer y axis data max
   Ym(BUFSIZE) As Single    ' Display Buffer y axis data min
   n As Single              ' Number of display elements
   InUse As Integer         ' Flag, true if structure
   harray As Long           ' Handles to huge memory block
   Lstart As Long           ' Display buffer starting index
   Skip As Integer          ' display buffer number of points to skip,
   MaxPoint As Long
   CalType As Integer
   ByteOrder As Integer
   HugeSize As Long         ' Number of elements, Buffer size
   HugeSizeInitial As Long  ' Initial buffer size before zero fill
   Type As Integer          ' Type of data
                            '      1 = Time
                            '      2 = Freq
                            '      3 = Mass
                            '      5 = X,Y floating point pairs...
   LowMassFreq As Double    ' Low mass freq, 0 if BB
   FreqShift As Double
   SampleRate As Double
   FileName As String
   FileType As Integer      ' Type of data file
                            '    1 = integer time, no header
                            '    2 = floats, sun extrel format
                            '    3 = floats, with header
                            '    4 = Excite Waveform
                            '    5 = ASCII data
                            '    6 = Midas format
                            '    7 = IonSpec format OMEGA version 3.3
   NumSegments As Integer   ' used with Stack plots, indicates the
                            ' the number of spectra
   Intensity As Double
End Type
Type IonSpecHeader
   ftype           As Byte
   revmaj          As Byte
   revmin          As Byte
   timacq(19)      As Byte
   xmemo(59)       As Byte
   sample(3, 49)   As Byte
   prsure(3)       As Single
   pulprs(3)       As Single
   usrnam(5, 6)    As Byte
   lasnam(3, 12)   As Byte
   valnam(3, 12)   As Byte
   laspwr(3, 11)   As Byte
   lasppt(3)       As Long
   xmit1 As Single: xmit2 As Single: rcvr1 As Single: rcvr2 As Single
   trap1 As Single: trap2 As Single
   lens(5)  As Single
   qoffs As Single: qfreq As Single: qampl As Single: filcur As Single
   emscur As Single: Energy As Single
   mxreq As Long: mxspec As Long: nscan As Long
   swcfp(7) As Integer
   aqmode As Long
   scnini As Single
   scnpul As Long
   qps As Byte: bps As Byte: ejs As Byte: lps(3) As Byte
   vps(3) As Byte: sws(30) As Byte: ups(5) As Byte: dps As Byte
   qpd As Single: bpd As Single: ejd As Single: lpd(3) As Single
   vpd(3) As Single: swd(30) As Single
   upd(5) As Single: dpd As Single: qpw As Single: bpw As Single
   ejw As Single: lpw(3) As Single
   vpw(3) As Single: sww(30) As Single: upw(5) As Single: dpw As Single
   qpl As Byte: bpl As Byte: ejl As Byte: lpl(3) As Byte: vpl(3) As Byte
   swl(30) As Byte: upl(5) As Byte: dpl As Byte: qpc As Byte: bpc As Byte
   ejc As Byte: lpc(3) As Byte: vpc(3) As Byte
   swc(30) As Byte: upc(5) As Byte: dpc As Byte: qpp As Byte: bpp As Byte
   ejp As Byte: lpp(3) As Byte: Vpp(3) As Byte
   swp(30) As Byte: upp(5) As Byte: dpp As Byte: lpn(3) As Byte: vpn(3) As Byte
   swsm(30) As Double: swem(30) As Double
   swamp(30) As Single
   calver As Byte
   A As Double: b As Double: c As Double
   lmass As Double: hmass As Double: lmfrq As Double: hmfrq As Double
   adcr As Double
   naq As Long
   hmode As Byte: tmode As Byte: pmode As Byte: qft As Byte: extrig As Byte
   detmod As Long: iadcr As Long
   swampamp As Single
   scntype As Long
End Type
Public Type CalRec
   A As Double
   b As Double
   c As Double
   CalType As Integer
End Type
Public Type IsotopicData
   Freq As Double
   FreqShift As Double
   Abundance As Double
   CS As Integer
   mz As Double
   Fit As Double
   Fitp1 As Double
   Fitm1 As Double
   AverageMW As Double
   MonoMW As Double
   OrginalMZ As Double
   OrginalMonoMW As Double
   MostMW As Double
   id As String
   Used As Boolean
   Ignore As Boolean
   ppm As Double
End Type
Public Type MassXformRecord
   DeCAL As Boolean
   Header As String
   Intensity As Double
   CSdf As Double
   ISOCal As CalRec
   ISOs() As IsotopicData
   PST As String
   AutoCalRec As String
   Trailor As String
   FullRecord As String
End Type
Global MacroPlaying As Boolean
Global NTflag As Integer
Global ISH As IonSpecHeader
Global LastScope As Integer
Global H(4) As Hosts
Global pk As Peaks
Global SysDirs As Dirs
Global AxisSetup As Setups
Global ExciteData(2) As ExciteStruct
Global Res As String
Global MacroEnabled As Integer
Global Scopes(MAXSCOPES) As New Scope
Global ActiveForm As Form
Global UseHeaderCal As Integer
Global CopyPlot As PlotStructure
Global Plots(MAXSCOPES) As PlotStructure
Global psold  As PlotStructure
Global Defaults As DefaultStruct
Global ActivePlot As Integer
Global Macs(NUMMACROS + 1) As MacroStruct
' Global cal data
Global A As Double
Global b As Double
Global c As Double
Global CalType As Integer
Global EG2 As New ExciteGen
Global EP2 As New ExcParms
Global s2 As New Series
Global HomeDir As String
Global FillMode As Integer
Type IntRec
   ivals(1024) As Integer
End Type
Global DefaultColors As Colors
Global LogFileName As String
Global PeakX() As Double
Global PeakY() As Double
Global NumPeaks As Integer
Global ScopesUnload As Boolean
Global ScopeVisible As Boolean
Global ReportErrors As Boolean
Global M As New Mercury
Dim PKlist As PeakList
Global ScopeLoadHandler As Handler
Global AcquireDataHandler As Handler
Global ExciteGenerateHandler As Handler
Global ExciteDownLoadHandler As Handler
Global MXR() As MassXformRecord
Dim filterFWHM As Double
' This function will save the objects values into a ini file
' defined by FileName. Only objects with a defined tag will
' be saved.
Public Sub SaveTagedObjects(frm As Form, FileName As String, key As String)
Dim obj As Object
Dim strTag As String, strValue As String
Dim i As Integer

   For Each obj In frm
      If obj.Tag <> "" Then
         strTag = obj.Tag
         If TypeName(obj) = "ComboBox" Then
            strValue = ""
            If obj.ListCount > 0 Then
               For i = 0 To obj.ListCount - 1
                  obj.ListIndex = i
                  If strValue <> "" Then strValue = strValue & ","
                  strValue = strValue & obj
               Next
               Call WritePrivateProfileString(key, strTag, strValue, FileName)
            End If
         Else
            strValue = obj
            Call WritePrivateProfileString(key, strTag, strValue, FileName)
         End If
      End If
   Next
End Sub
Public Sub ReadTagedObjects(frm As Form, FileName As String, key As String)
Dim obj As Object
Dim strTag As String, strToken
Dim strValue As String
Dim i As Integer, li As Integer

   For Each obj In frm
      If obj.Tag <> "" Then
         strTag = obj.Tag
         If TypeName(obj) = "ComboBox" Then
            obj.Clear
            strValue = String(80, " ")
            li = GetPrivateProfileString(key, strTag, "", strValue, 20, FileName)
            If (li <> 0) Then
               Mid(strValue, li + 1, 1) = " "
               i = 0
               Do
                  strToken = GetToken(strValue, i)
                  If strToken = "" Then Exit Do
                  obj.AddItem (strToken)
                  i = i + 1
               Loop
            End If
         Else
            strValue = String(80, " ")
            li = GetPrivateProfileString(key, strTag, "", strValue, 20, FileName)
            If (li <> 0) Then
               Mid(strValue, li + 1, 1) = " "
               obj = Trim(strValue)
            End If
         End If
      End If
   Next
End Sub

Public Function HugeFileSize(FileName As String) As Double
Dim FileHandle As Long, iStat As Long
Dim fhi As BY_HANDLE_FILE_INFORMATION
Dim Fs As Double

   FileHandle = lopen(FileName, 0&)
   iStat = GetFileInformationByHandle(FileHandle, fhi)
   If fhi.nFileSizeLow < 0 Then
      Fs = 2 ^ 32 + fhi.nFileSizeLow + fhi.nFileSizeHigh * 2 ^ 32
   Else
      Fs = fhi.nFileSizeLow + fhi.nFileSizeHigh * 2 ^ 32
   End If
   HugeFileSize = Fs
   lclose (FileHandle)
End Function
Public Sub Derivative(PI As Integer)
Dim i As Long
Dim T1 As Single, T2 As Single

   ' Take the derivative of the waveform...
   For i = 0 To Plots(PI).HugeSize - 2
      Call GetHugeEl(Plots(PI).harray, 4, i, T1)
      Call GetHugeEl(Plots(PI).harray, 4, i + 1, T2)
      T1 = (T2 - T1)
      Call SetHugeEl(Plots(PI).harray, 4, i, T1)
   Next
End Sub
Public Sub Integral(PI As Integer)
Dim i As Long
Dim T1 As Single, T2 As Single

   ' Intergrate the waveform...
   T2 = 0
   For i = 0 To Plots(PI).HugeSize - 1
      Call GetHugeEl(Plots(PI).harray, 4, i, T1)
      T2 = T1 + T2
      Call SetHugeEl(Plots(PI).harray, 4, i, T2)
   Next
End Sub
Public Sub TimeZero(PI As Integer)
Dim MaxX As Single
Dim MaxIndex As Long, i As Long, j As Long, k As Long
Dim Sdata() As Single, dt As Single
Dim Re As Single, Im As Single, mult As Single

   ' Find peak
   MaxX = FindPeak(Plots(PI), CSng(Xvalue(Plots(PI), 0)), CSng(Xvalue(Plots(PI), Plots(PI).HugeSize)))
   MaxIndex = FindIndex(Plots(PI), MaxX)
   ' Find FWHM
   filterFWHM = FindFWHM(Plots(PI), MaxIndex)
   ' Allocate space for data and read into array...
   ReDim Sdata(Plots(PI).HugeSize) As Single
   For i = 0 To Plots(PI).HugeSize - 1
      Call GetHugeEl(Plots(PI).harray, 4, i, Sdata(i))
   Next
   ' do the rearangement...
   For i = MaxIndex To Plots(PI).HugeSize - 1
      Call SetHugeEl(Plots(PI).harray, 4, i - MaxIndex, Sdata(i))
   Next
   For i = 0 To MaxIndex - 1
      Call SetHugeEl(Plots(PI).harray, 4, Plots(PI).HugeSize - MaxIndex + i, Sdata(i))
   Next
   ' Log details to the scope
   Call LogMess(Plots(PI), "TimeZero function parameters: ")
   Call LogMess(Plots(PI), "Peak: " & Format(MaxX))
   Call LogMess(Plots(PI), "FWHM: " & Format(filterFWHM))
End Sub
Public Sub WindowFunction(PI As Integer, Hmult As Single, Rmult As Single)
Dim i As Long, j As Long, k As Long
Dim T1 As Single, T2 As Single, T3 As Single, T4 As Single, MaxT As Single, fVal As Single

   ' Do the windowing function...
   MaxT = CSng(Xvalue(Plots(PI), Plots(PI).HugeSize))
   T1 = filterFWHM * Hmult
   T2 = filterFWHM * Rmult + T1
   T3 = MaxT - T2
   T4 = MaxT - T1
   ' Ramp from T1 to T2
   j = FindIndex(Plots(PI), T1)
   k = FindIndex(Plots(PI), T2)
   For i = j To k
      Call GetHugeEl(Plots(PI).harray, 4, i, fVal)
      fVal = fVal * (1 - (i - j) / (k - j))
      Call SetHugeEl(Plots(PI).harray, 4, i, fVal)
   Next
   ' Zero between T2 and T3
   j = FindIndex(Plots(PI), T2)
   k = FindIndex(Plots(PI), T3)
   fVal = 0#
   For i = j To k
      Call SetHugeEl(Plots(PI).harray, 4, i, fVal)
   Next
   ' Ramp from T3 to T4
   j = FindIndex(Plots(PI), T3)
   k = FindIndex(Plots(PI), T4)
   For i = j To k
      Call GetHugeEl(Plots(PI).harray, 4, i, fVal)
      fVal = fVal * ((i - j) / (k - j))
      Call SetHugeEl(Plots(PI).harray, 4, i, fVal)
   Next
   ' Log details to the scope
   Call LogMess(Plots(PI), "Window function parameters: ")
   Call LogMess(Plots(PI), "T1:   " & Format(T1))
   Call LogMess(Plots(PI), "T2:   " & Format(T2))
   Call LogMess(Plots(PI), "T3:   " & Format(T3))
   Call LogMess(Plots(PI), "T4:   " & Format(T4))
   Call LogMess(Plots(PI), "Hold multiplier:   " & Format(Hmult))
   Call LogMess(Plots(PI), "Ramp multiplier:   " & Format(Rmult))
End Sub
Public Sub FilterFunction(PI As Integer, Hmult As Single, Rmult As Single)
Dim MaxX As Single, FWHM As Double
Dim MaxIndex As Long, i As Long, j As Long, k As Long
Dim Sdata() As Single, Area As Double, dt As Single
Dim Re As Single, Im As Single, mult As Single
Dim T1 As Single, T2 As Single, T3 As Single, T4 As Single, MaxT As Single, fVal As Single

   ' Find peak
   MaxX = FindPeak(Plots(PI), CSng(Xvalue(Plots(PI), 0)), CSng(Xvalue(Plots(PI), Plots(PI).HugeSize)))
   MaxIndex = FindIndex(Plots(PI), MaxX)
   ' Find FWHM
   FWHM = FindFWHM(Plots(PI), MaxIndex)
   ' Allocate space for data and read into array...
   ReDim Sdata(Plots(PI).HugeSize) As Single
   Area = 0
   For i = 0 To Plots(PI).HugeSize - 1
      Call GetHugeEl(Plots(PI).harray, 4, i, Sdata(i))
      If (Sdata(i) >= 0) Then Area = Area + Abs(Sdata(i))
   Next
   For i = 0 To Plots(PI).HugeSize - 1
'  Sdata(i) = Sdata(i) / Area
   Next
   ' do the rearangement...
   For i = MaxIndex To Plots(PI).HugeSize - 1
      Call SetHugeEl(Plots(PI).harray, 4, i - MaxIndex, Sdata(i))
   Next
   For i = 0 To MaxIndex - 1
      Call SetHugeEl(Plots(PI).harray, 4, Plots(PI).HugeSize - MaxIndex + i, Sdata(i))
   Next
   ' Now do the windowing function...
   MaxT = CSng(Xvalue(Plots(PI), Plots(PI).HugeSize))
   T1 = FWHM * Hmult
   T2 = FWHM * Rmult + T1
   T3 = MaxT - T2
   T4 = MaxT - T1
   ' Ramp from T1 to T2
   j = FindIndex(Plots(PI), T1)
   k = FindIndex(Plots(PI), T2)
   For i = j To k
      Call GetHugeEl(Plots(PI).harray, 4, i, fVal)
      fVal = fVal * (1 - (i - j) / (k - j))
      Call SetHugeEl(Plots(PI).harray, 4, i, fVal)
   Next
   ' Zero between T2 and T3
   j = FindIndex(Plots(PI), T2)
   k = FindIndex(Plots(PI), T3)
   fVal = 0#
   For i = j To k
      Call SetHugeEl(Plots(PI).harray, 4, i, fVal)
   Next
   ' Ramp from T3 to T4
   j = FindIndex(Plots(PI), T3)
   k = FindIndex(Plots(PI), T4)
   For i = j To k
      Call GetHugeEl(Plots(PI).harray, 4, i, fVal)
      fVal = fVal * ((i - j) / (k - j))
      Call SetHugeEl(Plots(PI).harray, 4, i, fVal)
   Next
   Call Convert2Freq(Plots(PI))
   ' Calculate the area
   Area = 0
   dt = Xvalue(Plots(PI), 1) - Xvalue(Plots(PI), 0)
   For i = 0 To Plots(PI).HugeSize / 2 - 1
      Call GetHugeEl(Plots(PI).harray, 4, i * 2, Re)
      Call GetHugeEl(Plots(PI).harray, 4, i * 2 + 1, Im)
      If Sqr(Re * Re + Im * Im) > Area Then Area = Sqr(Re * Re + Im * Im)
   Next
   ' Normalize
   For i = 0 To Plots(PI).HugeSize / 2 - 1
      Call GetHugeEl(Plots(PI).harray, 4, i * 2, Re)
      Call GetHugeEl(Plots(PI).harray, 4, i * 2 + 1, Im)
      Re = Re / Area
      Im = Im / Area
      Call SetHugeEl(Plots(PI).harray, 4, i * 2, Re)
      Call SetHugeEl(Plots(PI).harray, 4, i * 2 + 1, Im)
   Next
   ' Log details to the scope
   Call LogMess(Plots(PI), "Filter function parameters: ")
   Call LogMess(Plots(PI), "Peak: " & Format(MaxX))
   Call LogMess(Plots(PI), "FWHM: " & Format(FWHM))
   Call LogMess(Plots(PI), "T1:   " & Format(T1))
   Call LogMess(Plots(PI), "T2:   " & Format(T2))
   Call LogMess(Plots(PI), "T3:   " & Format(T3))
   Call LogMess(Plots(PI), "T4:   " & Format(T4))
   Call LogMess(Plots(PI), "Hold multiplier:   " & Format(Hmult))
   Call LogMess(Plots(PI), "Ramp multiplier:   " & Format(Rmult))
End Sub
Public Sub RemoveDeCAL(MXR() As MassXformRecord)
Dim i As Long, j As Long, k As Long
Dim Md As Double
Dim ps As PlotStructure

   On Error Resume Next
   For i = 0 To UBound(MXR)
      MXR(i).FullRecord = ""
      If MXR(i).DeCAL = True Then
         MXR(i).DeCAL = False
         MXR(i).AutoCalRec = ""
         ps.CalType = MXR(i).ISOCal.CalType
         ps.A = MXR(i).ISOCal.A
         ps.b = MXR(i).ISOCal.b
         ps.c = MXR(i).ISOCal.c
         k = -1
         k = UBound(MXR(i).ISOs)
         For j = 0 To k
            With MXR(i).ISOs(j)
               Md = (Freq2Mass(ps, .Freq - .FreqShift) - Freq2Mass(ps, .Freq)) * .CS
               .mz = Freq2Mass(ps, .Freq - .FreqShift)
               .MonoMW = .MonoMW + Md
               .AverageMW = .AverageMW + Md
               .MostMW = .MostMW + Md
               .Freq = .Freq - .FreqShift
               .FreqShift = 0#
            End With
         Next
      End If
   Next
End Sub

Function GeneratePekFileName(FilePath As String) As String
Dim k As Integer
Dim RootName As String, DateString As String

   ' First look for the last back slash
   For k = Len(FilePath) - 1 To 1 Step -1
      If Mid(FilePath, k, 1) = "\" Then Exit For
   Next
   If Mid(FilePath, k, 1) = "\" Then k = k + 1
   RootName = Mid(FilePath, k)
   If Right(RootName, 1) = "\" Then RootName = Mid(RootName, 1, Len(RootName) - 1)
   DateString = Date
   For k = 1 To Len(DateString)
      If Mid(DateString, k, 1) = "/" Then Mid(DateString, k, 1) = "_"
   Next
   RootName = RootName & "_" & DateString & ".Pek"
   GeneratePekFileName = RootName
End Function
Function GenerateTICFileName(FilePath As String) As String
Dim k As Integer
Dim RootName As String, DateString As String

   ' First look for the last back slash
   For k = Len(FilePath) - 1 To 1 Step -1
      If Mid(FilePath, k, 1) = "\" Then Exit For
   Next
   If Mid(FilePath, k, 1) = "\" Then k = k + 1
   RootName = Mid(FilePath, k)
   If Right(RootName, 1) = "\" Then RootName = Mid(RootName, 1, Len(RootName) - 1)
   DateString = Date
   For k = 1 To Len(DateString)
      If Mid(DateString, k, 1) = "/" Then Mid(DateString, k, 1) = "_"
   Next
   RootName = RootName & "_" & DateString & ".TIC"
   GenerateTICFileName = RootName
End Function
Function GetBrukerMin(LB As ListBox) As Long
Dim i As Integer, j As Integer, k As Integer
Dim Min As Integer, Num As Integer
   
   Min = -1
   For k = 0 To LB.ListCount - 1
      LB.ListIndex = k
      If InStr(LB.Text, "\acqus") <> 0 Then
         i = InStr(LB.Text, "\acqus")
         For j = i - 1 To 1 Step -1
            If Mid(LB.Text, j, 1) = "\" Then
               Num = val(Mid(LB.Text, j + 1))
               If Min = -1 Then Min = Num
               If Num < Min Then Min = Num
               Exit For
            End If
         Next
      End If
   Next
   GetBrukerMin = Min
End Function
Function GetBrukerMax(LB As ListBox) As Long
Dim i As Integer, j As Integer, k As Integer
Dim Max As Integer, Num As Integer
   
   Max = -1
   For k = 0 To LB.ListCount - 1
      LB.ListIndex = k
      If InStr(LB.Text, "\acqus") <> 0 Then
         i = InStr(LB.Text, "\acqus")
         For j = i - 1 To 1 Step -1
            If Mid(LB.Text, j, 1) = "\" Then
               Num = val(Mid(LB.Text, j + 1))
               If Max = -1 Then Max = Num
               If Num > Max Then Max = Num
               Exit For
            End If
         Next
      End If
   Next
   GetBrukerMax = Max
End Function
Function GetBrukerNext(LB As ListBox, Current As Integer) As String
Dim i As Integer, j As Integer, k As Integer
Dim nearest As Integer, Num As Integer, Rnum As Integer
   
   Current = Current + 1
   On Error Resume Next
   LB.ListIndex = Current
   Rnum = Current
   GetBrukerNext = LB.Text
   nearest = 30000
   For k = 0 To LB.ListCount - 1
      LB.ListIndex = k
      If InStr(LB.Text, "\acqus") <> 0 Then
         i = InStr(LB.Text, "\acqus")
         For j = i - 1 To 1 Step -1
            If Mid(LB.Text, j, 1) = "\" Then
               Num = val(Mid(LB.Text, j + 1))
               If Num = Current Then
                  GetBrukerNext = LB.Text
                  Exit Function
               End If
               If Num > Current Then
                  If (Num - Current) < (nearest - Current) Then
                     nearest = Num
                     Rnum = nearest
                     GetBrukerNext = LB.Text
                  End If
               End If
               Exit For
            End If
         Next
      End If
   Next
   Current = Rnum
End Function
Function Aline(Data1() As Single, Data2() As Single, M As Single, b As Single) As Single
Dim li As Long, lj As Long
Dim IndexAdj As Single, err As Single

   li = UBound(Data1)
   lj = UBound(Data2)
   IndexAdj = lj / li
   IndexAdj = 1
   ' Calculate the error
   err = 0#
   For li = 0 To UBound(Data2)
      lj = li * IndexAdj * M + b
      If (lj >= 0) And (lj <= UBound(Data1)) Then
         err = err + Abs(Data1(lj) - Data2(li))
      Else
         err = err + Abs(Data2(li))
      End If
   Next
   Aline = err
End Function
Function TICalinement(S1 As Integer, s2 As Integer, M As Single, b As Single) As Single
Dim Data1() As Single, Data2() As Single
Dim li As Long, Num As Long, Max As Single
Dim err As Single, BestM As Single, T As Single
Dim BestB As Single, Area As Single, j As Integer

   ' Filter each data set
   If (S1 < 0) Or (s2 < 0) Then
      M = 1
      b = 0
      TICalinement = 10
      Exit Function
   End If
   Call BoxCarIntegrate(Plots(S1), Xvalue(Plots(S1), 0), Xvalue(Plots(S1), Plots(S1).HugeSize), 50)
   Call BoxCarIntegrate(Plots(s2), Xvalue(Plots(s2), 0), Xvalue(Plots(s2), Plots(s2).HugeSize), 50)
   ' Get the data from each scope and normalize
   Num = Plots(S1).HugeSize
   ReDim Data1(Num) As Single
   Num = Plots(s2).HugeSize
   ReDim Data2(Num) As Single
   Max = 0
   For li = 0 To UBound(Data1)
      Call GetHugeEl(Plots(S1).harray, 4, li, Data1(li))
      If Data1(li) > Max Then Max = Data1(li)
   Next
   For li = 0 To UBound(Data1)
      Data1(li) = Data1(li) / Max
   Next
   Max = 0
   Area = 0
   For li = 0 To UBound(Data2)
      Call GetHugeEl(Plots(s2).harray, 4, li, Data2(li))
      If Data2(li) > Max Then Max = Data2(li)
   Next
   If Max = 0 Then
      M = 1
      b = 0
      TICalinement = 10
      Exit Function
   End If
   For li = 0 To UBound(Data2)
      Data2(li) = Data2(li) / Max
      Area = Area + Data2(li)
   Next
   err = Aline(Data1, Data2, 1, 0)
   BestM = 1
   BestB = 0
   TICalinement = 10
   For j = 0 To 10
      err = Aline(Data1, Data2, 0.1, BestB)
      For M = 0.1 To 10 Step 0.05
         T = Aline(Data1, Data2, M, BestB)
         If T <= err Then
            err = T
            BestM = M
         End If
      Next
      ' find BestM
'      T = Aline(Data1, Data2, BestM, BestB)
'      If T > Aline(Data1, Data2, BestM + 0.05, BestB) Then
'        Do
'           T = Aline(Data1, Data2, BestM + 0.05, BestB)
'           If T < err Then
'               err = T
'               BestM = BestM + 0.05
'            Else
'               Exit Do
'            End If
'        Loop
'      Else
'        Do
'           T = Aline(Data1, Data2, BestM - 0.05, BestB)
'           If T < err Then
'               err = T
'               BestM = BestM - 0.05
'            Else
'               Exit Do
'            End If
'        Loop
'      End If
      ' Find BestB
      T = Aline(Data1, Data2, BestM, BestB)
      If T > Aline(Data1, Data2, BestM, BestB + 1) Then
        Do
           T = Aline(Data1, Data2, BestM, BestB + 1)
           If T < err Then
               err = T
               BestB = BestB + 1
            Else
               Exit Do
            End If
        Loop
      Else
        Do
           T = Aline(Data1, Data2, BestM, BestB - 1)
           If T < err Then
               err = T
               BestB = BestB - 1
            Else
               Exit Do
            End If
        Loop
      End If
      If (err / Area >= TICalinement) Then Exit For
      TICalinement = err / Area
   Next
   M = BestM / Plots(S1).Xmax
   b = BestB / Plots(S1).Xmax
   TICalinement = 1 - TICalinement
   If TICalinement < 0 Then TICalinement = 0
End Function
Function Bin2Long(Bin As String) As Long
Dim i As Integer
Dim li As Long

   Bin2Long = 0
   li = 1
   For i = Len(Bin) To 1 Step -1
      If Mid(Bin, i, 1) = "1" Then
         Bin2Long = Bin2Long + li
      ElseIf Mid(Bin, i, 1) <> "0" Then
         Exit Function
      End If
      li = li * 2
   Next
End Function
' This function is used to calculate the SORI frequence delta given the following
' Q = charge state
' M = mass
' Vpp = SORI voltage peak to peak
' eV = energy in electron volts
Public Function SORIenergy(q As Integer, M As Double, Vpp As Double, eV As Double) As Double
   SORIenergy = 1000#
End Function

Public Sub Report(MXR() As MassXformRecord, FileName As String, Optional OnlyAC As Boolean = False, Optional NewCalMessage As Boolean = False)
Dim i As Long, j As Long, k As Long, l As Long
Dim FileNum As Integer, ps As PlotStructure

   On Error Resume Next
   i = -1
   i = UBound(MXR)
   If i < 0 Then Exit Sub
   FileNum = FreeFile
   Open FileName For Append Access Write As FileNum
   For j = 0 To i ' Step 2
      If (OnlyAC = False) Or (OnlyAC = True And MXR(j).DeCAL = True) Then
         ' If MXR(j).FullRecord <> "" Then
         '   Print #FileNum, MXR(j).FullRecord
         'Else
            ps.A = MXR(j).ISOCal.A
            ps.b = MXR(j).ISOCal.b
            ps.c = MXR(j).ISOCal.c
            ps.CalType = MXR(j).ISOCal.CalType
            Print #FileNum, MXR(j).Header;
            ps.Comment = ""
            Call LogIsoRecords(ps, MXR(j).ISOs)
            Print #FileNum, ps.Comment;
            Print #FileNum, MXR(j).PST
            If NewCalMessage Then
               Print #FileNum, "New calibration applied to all isotopic data:"
               Print #FileNum, " " & GenCal.GetCalibrationName(MXR(j).ISOCal.CalType)
               Print #FileNum, " A = " & Format(MXR(j).ISOCal.A)
               Print #FileNum, " B = " & Format(MXR(j).ISOCal.b)
               Print #FileNum, " C = " & Format(MXR(j).ISOCal.c)
            End If
            Print #FileNum, MXR(j).AutoCalRec;
            k = InStr(MXR(j).Trailor, "Number of isotopic distributions Identified")
            If k <> 0 Then
               Print #FileNum, Mid(MXR(j).Trailor, 1, k - 1);
               ps.Comment = ""
               Call ReportSequenceCoverage(ps)
               Print #FileNum, ps.Comment
            Else
               Print #FileNum, MXR(j).Trailor
            End If
         'End If
      End If
   Next
   Close FileNum
End Sub
Public Function GetExtension(FileName As String) As String
Dim i As Integer

   GetExtension = ""
   For i = Len(FileName) To 1 Step -1
      If Mid(FileName, i, 1) = "." Then
         GetExtension = Mid(FileName, i + 1)
         Exit Function
      End If
   Next
End Function

Public Function ReadPEK(FileName As String, MXR() As MassXformRecord) As Long
Dim FileNum As Integer
Dim NumRecs As Long, StartStr As Long, StopStr As Long
Dim str As String
Dim i As Long, j As Long
Dim IsoRecs As Boolean
Dim NumIso As Integer
Dim AdvancedIso As Boolean
Dim Fs As Double
Dim ps As PlotStructure

'   On Error Resume Next
   On Error GoTo ReadPekError
   ReadPEK = -1
   ' Open file and read the PEK file into MXR records...
   FileNum = FreeFile
   NumRecs = -1
   Open FileName For Input Access Read As FileNum
   Line Input #FileNum, str
   IsoRecs = False
   Do
      If InStr(str, "Horn Mass Transform Parameters:") <> 0 Or (EOF(FileNum)) Then
         ' The last record is now finished. Now determine the Trailor and Auto
         ' Cal record.
         If (NumRecs > -1) Or (EOF(FileNum)) Then
            With MXR(NumRecs)
               .Header = .Header & Chr(13) & Chr(10)
               .AutoCalRec = ""
               StartStr = InStr(.FullRecord, "Processing stop time: ")
               If StartStr > 0 Then StartStr = InStr(Mid(.FullRecord, StartStr), Chr(10)) + StartStr
               If StartStr > 0 Then StartStr = StartStr + 1
               StopStr = InStr(.FullRecord, "Number of peaks in spectrum = ")
               Fs = 0#
               If NumIso > -1 Then
                  For j = 0 To UBound(.ISOs)
                    .ISOs(j).FreqShift = 0#
                  Next
               End If
               If StartStr > 0 And StopStr > StartStr Then
                  .AutoCalRec = Mid(.FullRecord, StartStr, StopStr - StartStr)
                  .DeCAL = True
                  i = InStr(.AutoCalRec, "Freq shift = ")
                  If i <> 0 Then
                     Fs = val(Mid(.AutoCalRec, i + 13))
                     ' apply this to all iso records
                     For j = 0 To UBound(.ISOs)
                        .ISOs(j).FreqShift = Fs
                     Next
                  End If
               End If
               If StopStr > 0 Then .Trailor = Mid(.FullRecord, StopStr)
               ' look for charge state freq shift
               i = InStr(.Header, "freq shift = ")
               If i <> 0 Then
                  .CSdf = val(Mid(.Header, i + 13))
               End If
               i = InStr(.Header, " I = ")
               If i <> 0 Then
                  .Intensity = val(Mid(.Header, i + 5))
               End If
            End With
            If EOF(FileNum) Then Exit Do
         End If
         ' Create a record for this data
         NumRecs = NumRecs + 1
         AdvancedIso = False
         ReDim Preserve MXR(NumRecs) As MassXformRecord
         MXR(NumRecs).FullRecord = str
         MXR(NumRecs).CSdf = 0#
         MXR(NumRecs).DeCAL = False
         MXR(NumRecs).Header = ""
         MXR(NumRecs).Intensity = 0#
         MXR(NumRecs).PST = ""
         MXR(NumRecs).AutoCalRec = ""
         MXR(NumRecs).Trailor = ""
         Do
            Line Input #FileNum, str
            If str = "Isotopic analysis advanced parameters:" Then AdvancedIso = True
            If str = "Charge state analysis advanced parameters:" Then AdvancedIso = False
            If (Mid(str, 1, 12) = "Calibration:") Or (AdvancedIso And (Mid(str, 1, 13) = " Calibration:")) Then
               MXR(NumRecs).FullRecord = MXR(NumRecs).FullRecord & Chr(13) & Chr(10) & str
               Line Input #FileNum, str   ' Cal type
               MXR(NumRecs).FullRecord = MXR(NumRecs).FullRecord & Chr(13) & Chr(10) & str
               MXR(NumRecs).ISOCal.CalType = GenCal.GetCalibrationType(Trim(str))
               Line Input #FileNum, str   ' A
               MXR(NumRecs).FullRecord = MXR(NumRecs).FullRecord & Chr(13) & Chr(10) & str
               MXR(NumRecs).ISOCal.A = val(Mid(Trim(str), 4))
               Line Input #FileNum, str   ' B
               MXR(NumRecs).FullRecord = MXR(NumRecs).FullRecord & Chr(13) & Chr(10) & str
               MXR(NumRecs).ISOCal.b = val(Mid(Trim(str), 4))
               Line Input #FileNum, str   ' C
               MXR(NumRecs).ISOCal.c = val(Mid(Trim(str), 4))
            End If
            If Mid(str, 1, 21) = "Processing stop time:" Then
               MXR(NumRecs).PST = str
               IsoRecs = False
            End If
            If IsoRecs = True Then
               ' allocate a array element...
               NumIso = NumIso + 1
               ReDim Preserve MXR(NumRecs).ISOs(NumIso) As IsotopicData
               MXR(NumRecs).ISOs(NumIso).CS = val(GetToken(str, 0))
               MXR(NumRecs).ISOs(NumIso).Abundance = val(GetToken(str, 1))
               MXR(NumRecs).ISOs(NumIso).mz = val(GetToken(str, 2))
               MXR(NumRecs).ISOs(NumIso).Fit = val(GetToken(str, 3))
               MXR(NumRecs).ISOs(NumIso).AverageMW = val(GetToken(str, 4))
               MXR(NumRecs).ISOs(NumIso).MonoMW = val(GetToken(str, 5))
               MXR(NumRecs).ISOs(NumIso).MostMW = val(GetToken(str, 6))
               MXR(NumRecs).ISOs(NumIso).id = GetToken(str, 7)
               MXR(NumRecs).ISOs(NumIso).ppm = 0
               MXR(NumRecs).ISOs(NumIso).Used = True
               MXR(NumRecs).ISOs(NumIso).Ignore = False
            End If
            If Mid(str, 1, 21) = "CS,  Abundance,   m/z" Then
               IsoRecs = True
               NumIso = -1
               MXR(NumRecs).Header = MXR(NumRecs).FullRecord & Chr(13) & Chr(10) & str
            End If
            If InStr(str, "Horn Mass Transform Parameters:") <> 0 Then Exit Do
            If EOF(FileNum) Then Exit Do
            MXR(NumRecs).FullRecord = MXR(NumRecs).FullRecord & Chr(13) & Chr(10) & str
         Loop
      Else
         Line Input #FileNum, str
      End If
'      If EOF(FileNum) Then Exit Do
   Loop
   ' calculate all of the m/z peak frequencies...
   On Error Resume Next
   For i = 0 To UBound(MXR)
      ps.CalType = MXR(i).ISOCal.CalType
      ps.A = MXR(i).ISOCal.A
      ps.b = MXR(i).ISOCal.b
      ps.c = MXR(i).ISOCal.c
      For j = 0 To UBound(MXR(i).ISOs)
         MXR(i).ISOs(j).Freq = Mass2Freq(ps, MXR(i).ISOs(j).mz)
      Next
   Next
   ReadPEK = NumRecs
'   Call Report(MXR, "c:\windows\desktop\test.pek")
   Exit Function
ReadPekError:
   MsgBox Error
End Function
Public Sub ScaleArray(PI As Integer, k As Single, A As Single, b As Single, c As Single, d As Single, f As Single, G As Single)
Dim li As Long
Dim X As Single
Dim iStat As Integer
Dim mz As Single
Dim T As Single
Dim Y As Single
Dim xx As Single
Dim yy As Single

   MacroRecord ("ScaleArray," & Format$(k) & "," & Format$(A) & "," & Format$(b) & "," & Format$(c) & "," & Format$(d) & "," & Format$(f) & "," & Format$(G))
   On Error GoTo ScaleArrayError
   If Plots(PI).Type = 1 Then
      For li = 0 To Plots(PI).HugeSize
         iStat = GetHugeEl(Plots(PI).harray, 4, li, X)
         T = Xvalue(Plots(PI), li)
         X = X * (k + A * T + b * T * T + c * T * T * T + f * Exp(T / d)) + G
         iStat = SetHugeEl(Plots(PI).harray, 4, li, X)
      Next li
   Else
      For li = 0 To Plots(PI).HugeSize / 2
         On Error Resume Next
         iStat = GetHugeEl(Plots(PI).harray, 4, li * 2, X)
         iStat = GetHugeEl(Plots(PI).harray, 4, li * 2 + 1, Y)
         mz = Xvalue(Plots(PI), li)
         X = X * (k + mz * b + mz * mz * c + f * Exp(mz / d))
         Y = Y * (k + mz * b + mz * mz * c + f * Exp(mz / d))
         If (X <> 0#) Then
            yy = Sqr(A * A / ((Y / X) ^ 2 + 1#))
            xx = yy * Y / X
            X = X + xx
            Y = Y - yy
         Else
            Y = Y + A
         End If
         iStat = SetHugeEl(Plots(PI).harray, 4, li * 2, X)
         iStat = SetHugeEl(Plots(PI).harray, 4, li * 2 + 1, Y)
      Next li
   End If
   Exit Sub
ScaleArrayError:
   Call ErrorMess.LogError(Error$ & " in ScaleArray!", "")
   Exit Sub
End Sub
Public Sub BaseLineRestore(PI As Integer)
   Dim li As Long, Average As Double
   Dim fVal As Single, iStat As Long
   
   Call MacroRecord("BaseLineRestore")
   Average = 0#
   For li = 0 To Plots(PI).HugeSize - 1
      iStat = GetHugeEl(Plots(PI).harray, 4, li, fVal)
      Average = Average + fVal
   Next li
   Average = Average / Plots(PI).HugeSize
   For li = 0 To Plots(PI).HugeSize - 1
      iStat = GetHugeEl(Plots(PI).harray, 4, li, fVal)
      fVal = fVal - Average
      iStat = SetHugeEl(Plots(PI).harray, 4, li, fVal)
  Next li
End Sub
Function FindPI(ps As PlotStructure) As Integer
Dim i As Integer

   For i = 0 To MAXSCOPES
      If (ps.harray = Plots(i).harray) Then
         FindPI = i
         Exit Function
      End If
   Next
   FindPI = -1
End Function

Sub SumFile(PI As Integer, FileName As String, Optional ScanNum As Integer = 0)
Dim NPI As Integer

   ScopeVisible = False
   NPI = ICR_2LS.LoadDataFile(FileName)
   If ScanNum > 1 And Plots(NPI).NumSegments >= ScanNum Then
      Call HugeErase(Plots(NPI).harray)
      Call LoadFile(NPI, Plots(NPI).FileName, ScanNum, False, 0)
   End If
   If NPI >= 0 Then
      CopyPlot = Plots(NPI)
      Call SpectraMath(PI, Asc("+"))
      Unload Scopes(NPI)
      If ScanNum = 0 Then
         Call LogMess(Plots(PI), FileName & " Added")
      Else
         Call LogMess(Plots(PI), FileName & " Scan number " & Format(ScanNum) & " Added")
      End If
   End If
   ScopeVisible = True
End Sub
Sub ReadAllFiles(O As Object, Path As String, Optional Tail As String = "")
   Dim FileName As String
   Dim Dirs() As String
   Dim NumDirs As Integer
   Dim i As Integer
   
   On Error GoTo ReadAllFilesError
   NumDirs = 0
   ' Add all files to the list box
   FileName = Dir(Path & "\*.*")
   Do
      If FileName = "" Then Exit Do
      If InStr(UCase(FileName), ".LOG") = 0 And InStr(UCase(FileName), ".TIC") = 0 And InStr(UCase(FileName), ".PAR") = 0 And InStr(UCase(FileName), ".PEK") = 0 Then
         If Tail <> "" Then
            If Len(Tail) <= Len(FileName) Then
               If Mid(UCase(FileName), Len(FileName) - Len(Tail) + 1, Len(Tail)) = UCase(Tail) Then O.AddItem Path & "\" & FileName
            End If
         Else
            O.AddItem Path & "\" & FileName
         End If
      End If
      FileName = Dir
   Loop
   ' Build a list of all Dirs...
   FileName = Dir(Path & "\*.*", vbDirectory)
   Do
      If FileName = "" Then Exit Do
      If FileName <> "." And FileName <> ".." Then
         ReDim Preserve Dirs(NumDirs) As String
         Dirs(NumDirs) = Path & "\" & FileName
         NumDirs = NumDirs + 1
      End If
      FileName = Dir
   Loop
   ' Now use recursion and go through the list of dirs...
   For i = 0 To NumDirs - 1
      Call ReadAllFiles(O, Dirs(i), Tail)
   Next
   Exit Sub
ReadAllFilesError:
   Exit Sub
End Sub

Function CreateFontObject(O As Object, angle As Integer, TwipsPerPixelX As Integer) As Long
   Dim lf As LOGFONT
   Dim i As Integer, ByteArrayLimit As Integer
   Dim TempByteArray() As Byte

   lf.lfHeight = O.TextHeight("X") / TwipsPerPixelX
   lf.lfWidth = 0
   lf.lfEscapement = angle * 10
   lf.lfWeight = O.Font.Weight
   If O.Font.Italic Then lf.lfItalic = 1
   If O.Font.underline Then lf.lfUnderline = 1
   If O.Font.StrikeThrough Then lf.lfStrikeOut = 1
   lf.lfOutPrecision = OUT_DEFAULT_PRECIS
   lf.lfClipPrecision = OUT_DEFAULT_PRECIS
   lf.lfQuality = DEFAULT_QUALITY
   lf.lfPitchAndFamily = DEFAULT_PITCH Or FF_DONTCARE
   lf.lfCharSet = ANSI_CHARSET
   TempByteArray = O.Font & Chr$(0)
   ByteArrayLimit = UBound(TempByteArray)
   For i = 0 To ByteArrayLimit
      lf.lfFaceName(i) = TempByteArray(i)
   Next i
   CreateFontObject = CreateFontIndirect(lf)
End Function
Function GetStringLine(str As String, Line As Integer) As String
' This function will extract the selected line from a string.
' A line is defined by its EOL characters, CR and/or LF
Dim i As Integer, Pos As Integer, Start As Integer

   GetStringLine = ""
   Pos = 0
   For i = 0 To Line
      Start = Pos + 1
      Pos = InStr(Start, str, Chr(10))
      If Pos = 0 Then Pos = InStr(Start, str, Chr(13))
      If Pos = 0 Then
         If i = Line Then
            GetStringLine = Mid(str, Start)
            Exit Function
         Else
            Exit Function
         End If
      End If
   Next
   GetStringLine = Mid(str, Start, Pos - Start - 1)
End Function
Function GetString(FileNum As Integer) As String
   Dim str As String
   Dim c As Byte
   
   str = ""
   Do
      c = Asc(Input(1, FileNum))
      If (c = 10) Or EOF(FileNum) Then
         GetString = str
         Exit Function
      End If
      If (c <> 13) Then
         str = str & Chr$(c)
      End If
    Loop
End Function

Sub SaveTheMetafile(FileName$, mfhnd&, xExt%, yExt%)
    Dim fhnd&
    Dim di&, dl&
    Dim mfglbhnd&
    Dim newmf&
    Dim dc&
    Dim mfile As METAFILEHEADER
    Dim mfinfosize&
    Dim currentfileloc&
    Dim GPTR&
    Dim oldsize As size

    ' Open the file to write
    fhnd = lcreat(FileName$, 0)
    If fhnd < 0 Then Exit Sub
    If mfhnd = 0 Then Exit Sub
    ' First write a placeable header file header
    mfile.key = &H9AC6CDD7  ' The key - required
    mfile.hMF = 0           ' Must be 0
    mfile.bbox.Left = 0
    mfile.bbox.Top = 0
    ' These should be calculated using GetDeviceCaps
    mfile.bbox.Right = xExt + 1 ' Size in metafile units of bounding area
    mfile.bbox.Bottom = yExt + 1
    mfile.inch = 725 ' Number of metafile units per inch
    mfile.reserved = 0
    ' Build the checksum
    mfile.checksum = &H9AC6 Xor &HCDD7 ' 9ac6 xor cdd7
    mfile.checksum = mfile.checksum Xor mfile.bbox.Right
    mfile.checksum = mfile.checksum Xor mfile.bbox.Bottom
    mfile.checksum = mfile.checksum Xor mfile.inch
    ' Write the buffer
    di = lwrite(fhnd, mfile, Len(mfile))
    newmf = mfhnd
    ' Determine the size of the metafile data...
    mfinfosize = GetMetaFileBitsExSize(mfhnd, 0, 0)
    ' Allocate space for it...
    mfglbhnd = GlobalAlloc(GMEM_MOVEABLE, mfinfosize)
    GPTR = GlobalLock(mfglbhnd)
    ' read it
    mfinfosize = GetMetaFileBitsExSize(mfhnd, mfinfosize, GPTR)
    ' Save it
    dl = hwrite(fhnd, ByVal GPTR, mfinfosize)
    di = GlobalUnlock(mfglbhnd)
    GlobalFree (mfglbhnd)
    di = lclose(fhnd)
End Sub
Function Byte2String(byt() As Byte) As String
   Dim li As Integer
   
   Byte2String = ""
   For li = 0 To UBound(byt)
      If byt(li) = 0 Then Exit For
      Byte2String = Byte2String & Chr(byt(li))
   Next
End Function
Function AllocateScope(Optional Index As Integer = -1, Optional Visible As Boolean = True) As Integer
Dim Ns As Integer
 
   Ns = -1
   On Error GoTo AllocateScopeError
   If Index = -1 Then
      Ns = NextScope()
      If (Ns = -1) Then Exit Function
   Else
      Ns = Index
   End If
   Scopes(Ns).PI = Ns
   Plots(Ns).InUse = True
   If ScopeVisible = False Then Visible = False
   Scopes(Ns).Visible = Visible
   If Visible = True Then
      Scopes(Ns).Show
   End If
   AllocateScope = Ns
   Exit Function
AllocateScopeError:
   Call ErrorMess.LogError(Error$ & " in AllocateScope!", "")
   If Ns <> -1 Then
      Plots(Ns).InUse = 0
      If Plots(Ns).harray <> 0 Then
         Call HugeErase(Plots(Ns).harray)
         Plots(Ns).harray = 0
      End If
   End If
   AllocateScope = -1
   Exit Function
End Function
Function BuildTICScope(Title As String) As Integer
Dim RICPI As Integer, i As Integer

   RICPI = NextScope()
   BuildTICScope = RICPI
   If RICPI <> -1 Then
      ' Here with a new scope plot structure created
      Call SetScopeDefaults(Plots(RICPI))
      Plots(RICPI).Ymax = 100
      Plots(RICPI).Ymin = 0
      Plots(RICPI).XminFull = 0
      Plots(RICPI).XmaxFull = 128
      Plots(RICPI).Xmin = 0
      Plots(RICPI).Xmax = 128
      Plots(RICPI).HugeSize = 128
      Plots(RICPI).HugeSizeInitial = 128
      Plots(RICPI).Title = Title
      Plots(RICPI).XaxisName = "File #"
      Plots(RICPI).YaxisName = "Amplitude"
      Plots(RICPI).ZeroFills = 0
      Plots(RICPI).appodize = 0
      Plots(RICPI).SampleRate = 1#
      Plots(RICPI).A = A
      Plots(RICPI).b = b
      Plots(RICPI).c = c
      Plots(RICPI).Type = 1
      Plots(RICPI).FileType = 3
      Plots(RICPI).FreqShift = 0#
      Plots(RICPI).LowMassFreq = 0#
      For i = 0 To 127
         Plots(RICPI).Y(i) = 0
      Next
      If HugeDim(Plots(RICPI).harray, 4, Plots(RICPI).HugeSize) = 0 Then
         Call HugeZero(Plots(RICPI).harray, Plots(RICPI).HugeSize)
         Call AllocateScope(RICPI)
      End If
   End If
End Function
Function Interpolate(X() As Single, Y() As Single, Length As Integer, xVal As Single) As Single
   Dim i As Integer
   
   If (X(0) < X(Length - 1)) Then
      If xVal < X(0) Then
         Interpolate = Y(0)
         Exit Function
      End If
      If xVal > X(Length - 1) Then
         Interpolate = Y(Length - 1)
         Exit Function
      End If
      For i = 0 To Length - 2
         If (xVal > X(i)) And (xVal <= X(i + 1)) Then
            Interpolate = Y(i) + (Y(i + 1) - Y(i)) * (xVal - X(i)) / (X(i + 1) - X(i))
            Exit Function
         End If
      Next
   Else
      If xVal < X(Length - 1) Then
         Interpolate = Y(Length - 1)
         Exit Function
      End If
      If xVal > X(0) Then
         Interpolate = Y(0)
         Exit Function
      End If
      For i = 0 To Length - 2
         If (xVal < X(i)) And (xVal >= X(i + 1)) Then
            Interpolate = Y(i + 1) + (Y(i) - Y(i + 1)) * (xVal - X(i)) / (X(i) - X(i + 1))
            Exit Function
         End If
      Next
   End If
   Interpolate = 0#
End Function

Sub Appodization(ps As PlotStructure, ApFn As Integer, Xstart As Single, Xstop As Single, Inverted As Integer, Optional ApexPos As Single = 50#)
   Dim Lstart As Long
   Dim Lstop As Long
   Dim temp As Long
   Dim li As Long, lj As Long, fVal As Single
   
   If ps.Type = 5 Then Exit Sub
   If Inverted Then Call LogMess(ps, "Inverted apodization")
   Select Case ApFn
      Case 1
         Call LogMess(ps, "Apodization = Parzen")
      Case 2
         Call LogMess(ps, "Apodization = Hanning")
      Case 3
         Call LogMess(ps, "Apodization = Welch")
      Case 4
         Call LogMess(ps, "Apodization = Triangle")
   End Select
   MacroRecord ("Appodization," + Format$(ApFn) + "," + Format$(Xstart) + "," + Format$(Xstop) + "," + Format$(ApexPos))
   ps.appodize = ApFn
   If Xstop = 0 Then
      Xstop = Xvalue(ps, ps.HugeSize)
   End If
   Lstart = FindIndex(ps, Xstart)
   Lstop = FindIndex(ps, Xstop)
   If Lstart > Lstop Then
      temp = Lstop
      Lstop = Lstart
      Lstart = temp
   End If
   If Inverted = False And ApexPos <> 50# Then
      lj = Lstart + ((Lstop - Lstart) * ApexPos) / 100
      For li = Lstart To lj - 1
         Call GetHugeEl(ps.harray, 4, li, fVal)
         fVal = fVal * (li - Lstart) / (lj - Lstart)
         Call SetHugeEl(ps.harray, 4, li, fVal)
      Next
      For li = lj To Lstop - 1
         Call GetHugeEl(ps.harray, 4, li, fVal)
         fVal = fVal * (Lstop - li) / (Lstop - lj)
         Call SetHugeEl(ps.harray, 4, li, fVal)
      Next
   Else
      If Inverted Then
         Call Window(ps.harray, Lstart, Lstop, -ApFn, ps.Type)
      Else
         Call Window(ps.harray, Lstart, Lstop, ApFn, ps.Type)
      End If
   End If
End Sub

Function BuildNewScope() As Integer
   Dim Lstart As Long
   Dim Lstop As Long
   Dim ln As Long
   Dim PI As Integer

   Call MacroRecord("BuildNewScope")
   PI = NextScope()
   If (PI = -1) Then
      MsgBox "No more scopes are avaliable!", 48, "ERROR"
      BuildNewScope = -1
      Exit Function
   End If
   Call CalculateStartStopLen(Plots(ActivePlot), Lstart, Lstop, ln)
   Call PrepScope(Plots(PI), Plots(ActivePlot), Lstart, Lstop, ln)
   Plots(PI).FileName = Plots(ActivePlot).FileName
   Plots(PI).NumSegments = 0
   Call AllocateScope(PI)
   BuildNewScope = PI
End Function

Sub CalculateStartStopLen(ps As PlotStructure, Lstart As Long, Lstop As Long, Length As Long)
   Dim ln As Long

   Lstart = FindIndex(ps, ps.Xmin)
   Lstop = FindIndex(ps, ps.Xmax)
   ln = 2 ^ Int(Log(Abs(Lstop - Lstart)) / Log(2) + 0.9)
   If Lstop > Lstart Then
      Lstop = Lstart + ln
      If ps.Type = 1 Then
         If Lstop >= ps.HugeSize Then
            Lstop = ps.HugeSize
            Lstart = Lstop - ln
         End If
      Else
         If Lstop >= ps.HugeSize / 2 Then
            Lstop = ps.HugeSize / 2
            Lstart = Lstop - ln
         End If
      End If
   Else
      Lstart = Lstop + ln
      If ps.Type = 1 Then
         If Lstart >= ps.HugeSize Then
            Lstart = ps.HugeSize - ln
            Lstop = ln - Lstart
         End If
      Else
         If Lstart >= ps.HugeSize / 2 Then
            Lstart = ps.HugeSize / 2
            Lstop = Lstart - ln
         End If
      End If
   End If
   Length = ln
End Sub

' Inside flag is non zero if range inside the start and stop is to be
' cleared
' Mode = 0 clear outside range
'        1 clear inside range
'        2 clear range and keep peaks
Sub ClearScopeData(ps As PlotStructure, Dstart As Single, Dstop As Single, Threshold As Single, Width As Long, Mode As Integer)
   Dim fVal As Single
   Dim Lstop As Long
   Dim Lstart As Long
   Dim temp As Long, NumBelow As Long
   Dim li As Long, lj As Long, LastClear As Long, lk As Long
   Dim Thres As Double, Sum As Double
   Dim iStat As Integer, State As Integer

   Select Case Mode
      Case 0
         Call LogMess(ps, "Clear outside range, " & Format(Dstart) & ", " & Format(Dstop))
      Case 1
         Call LogMess(ps, "Clear inside range, " & Format(Dstart) & ", " & Format(Dstop))
      Case 2
         Call LogMess(ps, "Clear range and keep peaks, " & Format(Dstart) & ", " & Format(Dstop))
   End Select
   MacroRecord ("ClearScopeData," + Format$(Dstart) + "," + Format$(Dstop) + "," + Format$(Threshold) + "," + Format$(Width) + "," + Format$(Mode))
   fVal = 0#
   Lstart = FindIndex(ps, Dstart)
   Lstop = FindIndex(ps, Dstop)
   If Lstart > Lstop Then
      temp = Lstop
      Lstop = Lstart
      Lstart = temp
   End If
   If Mode = 0 Then
      If ps.Type = 5 Then
         fVal = 0
         For lk = 0 To Lstart
            Call SetHugeEl(ps.harray, 4, lk * 2 + 1, fVal)
         Next
         For lk = Lstop To ps.HugeSize
            Call SetHugeEl(ps.harray, 4, lk * 2 + 1, fVal)
         Next
      ElseIf ps.Type = 1 Then
         iStat = HugeZeroRange(ps.harray, 0, Lstart)
         iStat = HugeZeroRange(ps.harray, Lstop, ps.HugeSize - Lstop)
      Else
         iStat = HugeZeroRange(ps.harray, 0, 2 * Lstart)
         iStat = HugeZeroRange(ps.harray, 2 * Lstop, ps.HugeSize - 2 * Lstop)
      End If
   ElseIf Mode = 1 Then
      If ps.Type = 5 Then
         fVal = 0
         For lk = Lstart To Lstop
            Call SetHugeEl(ps.harray, 4, lk * 2 + 1, fVal)
         Next
      ElseIf ps.Type = 1 Then
         iStat = HugeZeroRange(ps.harray, Lstart, Lstop - Lstart)
      Else
         iStat = HugeZeroRange(ps.harray, 2 * Lstart, 2 * (Lstop - Lstart))
      End If
   ElseIf Mode = 2 Then
      ' first calculate the average
      fVal = 0#
      Sum = 0#
      Thres = 0#
      If Threshold <> 0# Then
         For li = Lstart + 1 To Lstop - 1
            Sum = Sum + Yvalue(ps, li)
         Next li
         Thres = (Sum / CDbl(Lstop - Lstart)) * Threshold
      End If
      ' Now loop through again clearing all but peaks...
      LastClear = Lstart
      State = 0  ' 0 = find peak
                 ' 1 = find end of peak
      For li = Lstart To Lstop - 1
         Select Case State
            Case 0
               If (Yvalue(ps, li) > Thres) Then
                  If ((li - Width / 2) > LastClear) Then
                     ' clear from LastClear to li-Width/2
                     If ps.Type = 5 Then
                        fVal = 0
                        For lk = LastClear To (li - Width / 2)
                           Call SetHugeEl(ps.harray, 4, lk * 2 + 1, fVal)
                        Next
                     ElseIf ps.Type = 1 Then
                        iStat = HugeZeroRange(ps.harray, LastClear, (li - Width / 2) - LastClear)
                     Else
                        iStat = HugeZeroRange(ps.harray, 2 * LastClear, 2 * ((li - Width / 2) - LastClear))
                     End If
                     State = 1
                     NumBelow = 0
                  End If
               End If
            Case 1
               ' Find end of peak
               If (Yvalue(ps, li) < Thres) Then
                  NumBelow = NumBelow + 1
                  If NumBelow >= Width / 2 Then
                     LastClear = li
                     State = 0
                  End If
               Else
                  NumBelow = 0
               End If
         End Select
      Next li
      If ps.Type = 5 Then
         fVal = 0
         For lk = LastClear To Lstop
            Call SetHugeEl(ps.harray, 4, lk * 2 + 1, fVal)
         Next
      ElseIf ps.Type = 1 Then
         iStat = HugeZeroRange(ps.harray, LastClear, Lstop - LastClear)
      Else
         iStat = HugeZeroRange(ps.harray, 2 * LastClear, 2 * (Lstop - LastClear))
      End If
      For li = 0 To ps.HugeSize - 1
         If (li < Lstart) Or (li > Lstop) Then
            If ps.Type = 5 Then
               iStat = SetHugeEl(ps.harray, 4, li * 2 + 1, fVal)
            ElseIf ps.Type = 1 Then
               iStat = SetHugeEl(ps.harray, 4, li, fVal)
            Else
               iStat = SetHugeEl(ps.harray, 4, li * 2, fVal)
               iStat = SetHugeEl(ps.harray, 4, (li * 2) + 1, fVal)
            End If
         End If
      Next li
  End If
End Sub
Function Moment(ps As PlotStructure, Dstart As Single, Dstop As Single) As Double
   Dim Y As Double, xy As Double
   Dim Lstop As Long
   Dim Lstart As Long
   Dim temp As Long
   Dim li As Long

   Lstart = FindIndex(ps, Dstart)
   Lstop = FindIndex(ps, Dstop)
   If Lstart > Lstop Then
      temp = Lstop
      Lstop = Lstart
      Lstart = temp
   End If
   Y = 0#
   xy = 0#
   For li = Lstart To Lstop - 1
      Y = Y + Yvalue(ps, li)
      xy = xy + Xvalue(ps, li) * Yvalue(ps, li)
   Next
   Moment = xy / Y
End Function
Sub SavitzkyGolayFilter(ps As PlotStructure, Xstart As Single, Xstop As Single, Nl As Long, Nr As Long, M As Long)
Dim c() As Single, n As Long, CC() As Double
Dim nP As Long, i As Integer

   MacroRecord ("SavitzkyGolay," & Format$(Xstart) & "," & Format$(Xstop) & "," & Format$(Nl) & "," & Format$(Nr) & "," & Format$(M))
   nP = Nl + Nr + 1
   ReDim c(nP) As Single
   Call savgol(c(0), nP, Nl, Nr, 0, M)
   ' now un wrap the coefficents...
   n = Nr * 2
   If (Nl > Nr) Then n = Nl * 2
   ReDim CC(n) As Double
   For i = 0 To Nl
      CC(n / 2 - i) = c(i + 1)
   Next
   For i = 1 To Nr
      CC(n / 2 + i) = c(n - i)
   Next
   Call SavitzkyGolay(ps, Xstart, Xstop, CC)
End Sub
Sub SavitzkyGolay(ps As PlotStructure, Dstart As Single, Dstop As Single, c() As Double)
   Dim fVal As Single, Sum As Single
   Dim Lstop As Long
   Dim Lstart As Long
   Dim temp As Long
   Dim li As Long, lj As Long, le As Long, ls As Long
   Dim iStat As Integer, i As Integer
   Dim harray As Long
   Dim Y(1000) As Single, Ymax As Single, Ymin As Single
   Dim n As Long, Width As Long

   Width = Int(UBound(c) / 2)
   fVal = 0#
   Lstart = FindIndex(ps, Dstart)
   Lstop = FindIndex(ps, Dstop)
   If Lstart > Lstop Then
      temp = Lstop
      Lstop = Lstart
      Lstart = temp
   End If
   ' Get a temporary buffer
   iStat = HugeDim(harray, 4, ps.HugeSize)
   If (iStat <> 0) Then
      MsgBox ("ERROR! can't allocate memory. " + Format(iStat))
    Exit Sub
   End If
  Call CopyData(ps.harray, harray, 0, ps.HugeSize, ps.HugeSize)
  For li = Lstart To Lstop - 1
      ls = li - Width
      le = li + Width + 1
      If ls < Lstart Then ls = Lstart
      If le >= Lstop Then le = Lstop - 1
      If ps.Type = 5 Then
         n = HugeExtract(ps.harray, Y(0), Ymax, Ymin, ls, le, 1000, 6)
      ElseIf ps.Type = 1 Then
         n = HugeExtract(ps.harray, Y(0), Ymax, Ymin, ls, le, 1000, 1)
      Else
         n = HugeExtract(ps.harray, Y(0), Ymax, Ymin, ls, le, 1000, 4)
         n = HugeExtract(ps.harray, Y(0), Ymax, Ymin, ls, le, 1000, 3)
      End If
      If n = UBound(c) + 1 Then
         Sum = 0#
         For lj = 0 To n - 1
            Sum = Sum + Y(lj) * c(lj)
         Next lj
         If ps.Type = 5 Then
            iStat = SetHugeEl(harray, 4, li * 2 + 1, Sum)
         ElseIf ps.Type = 1 Then
            iStat = SetHugeEl(harray, 4, li, Sum)
         Else
            iStat = SetHugeEl(harray, 4, li * 2, Sum)
            iStat = SetHugeEl(harray, 4, (li * 2) + 1, 0#)
         End If
      End If
   Next li
   Call CopyData(harray, ps.harray, 0, ps.HugeSize, ps.HugeSize)
   iStat = HugeErase(harray)
End Sub
Sub BoxCarIntegrate(ps As PlotStructure, Dstart As Single, Dstop As Single, Width As Long)
   Dim fVal As Single, Sum As Single
   Dim Lstop As Long
   Dim Lstart As Long
   Dim temp As Long
   Dim li As Long, lj As Long, le As Long, ls As Long
   Dim iStat As Integer
   Dim harray As Long
   Dim Y(1000) As Single, Ymax As Single, Ymin As Single
   Dim n As Long

   MacroRecord ("BoxCarIntegrate," + Format$(Dstart) + "," + Format$(Dstop) + "," + Format$(Width))
   fVal = 0#
   Lstart = FindIndex(ps, Dstart)
   Lstop = FindIndex(ps, Dstop)
   If Lstart > Lstop Then
      temp = Lstop
      Lstop = Lstart
      Lstart = temp
   End If
   ' Get a temporary buffer
   iStat = HugeDim(harray, 4, ps.HugeSize)
   If (iStat <> 0) Then
      MsgBox ("ERROR! can't allocate memory. " + Format(iStat))
    Exit Sub
   End If
  Call CopyData(ps.harray, harray, 0, ps.HugeSize, ps.HugeSize)
  For li = Lstart To Lstop - 1
      ls = li - Width
      le = li + Width
      If ls < Lstart Then ls = Lstart
      If le >= Lstop Then le = Lstop - 1
      If ps.Type = 5 Then
         n = HugeExtract(ps.harray, Y(0), Ymax, Ymin, ls, le, 1000, 6)
      ElseIf ps.Type = 1 Then
         n = HugeExtract(ps.harray, Y(0), Ymax, Ymin, ls, le, 1000, 1)
      Else
         n = HugeExtract(ps.harray, Y(0), Ymax, Ymin, ls, le, 1000, 4)
         n = HugeExtract(ps.harray, Y(0), Ymax, Ymin, ls, le, 1000, 3)
      End If
      Sum = 0#
      For lj = 0 To n - 1
         Sum = Sum + Y(lj)
      Next lj
      Sum = Sum / n
      If ps.Type = 5 Then
         iStat = SetHugeEl(harray, 4, li * 2 + 1, Sum)
      ElseIf ps.Type = 1 Then
         iStat = SetHugeEl(harray, 4, li, Sum)
      Else
         iStat = SetHugeEl(harray, 4, li * 2, Sum)
         iStat = SetHugeEl(harray, 4, (li * 2) + 1, 0#)
      End If
   Next li
   Call CopyData(harray, ps.harray, 0, ps.HugeSize, ps.HugeSize)
   iStat = HugeErase(harray)
End Sub
Sub CommandFile(FileName As String)
Dim FileNum As Integer
Dim cmd As String

   On Error GoTo CommandFileError
   FileNum = FreeFile
   Open FileName For Input As FileNum
   ' now read all of the file commmands in to the last
   ' macro...
   Macs(NUMMACROS).key = 255
   Macs(NUMMACROS).NumCommands = 0
   While (Not EOF(FileNum))
      Line Input #FileNum, cmd
      If cmd = "Stop" Then
         Close FileNum
         Kill FileName
         Unload ICR_2LS
      End If
      Macs(NUMMACROS).Commands(Macs(NUMMACROS).NumCommands) = cmd
      Macs(NUMMACROS).NumCommands = 1 + Macs(NUMMACROS).NumCommands
   Wend
   Close FileNum
   Kill FileName
   ' now play the macro...
   Call MacroPlay(-1, 255)
   Exit Sub
CommandFileError:
   Exit Sub
End Sub

Function MZ2Freq(mz As Double) As Double
   Dim ps As PlotStructure
   
   ps.CalType = CalType
   ps.A = A
   ps.b = b
   ps.c = c
   MZ2Freq = Mass2Freq(ps, mz)
End Function
Function Freq2MZ(Freq As Double) As Double
   Dim ps As PlotStructure
   
   ps.CalType = CalType
   ps.A = A
   ps.b = b
   ps.c = c
   Freq2MZ = Freq2Mass(ps, Freq)
End Function
Function Convert2Freq(ps As PlotStructure) As Integer
   Dim iStat As Integer

   If ps.Type = 4 Then Exit Function
   If ps.Type = 5 Then Exit Function
   ps.LastLevel = 0
   Call LogMess(ps, "Convert to frequency")
   Call MacroRecord("Convert2Freq")
   iStat = 0
   If ps.Type <> 2 Then
      ps.XaxisName = "Frequency (Hz)"
      If ps.Type <> 3 Then
         iStat = realft(ps.HugeSize, ps.harray, 1)
      End If
      ps.Type = 2
      iStat = ExtractPlotData(ps, Xvalue(ps, 0&), Xvalue(ps, ps.HugeSize / 2))
      Call SaveAsLast(ps)
      If (iStat < 0) Then
         MsgBox ("ERROR! Unable to extract plot data.")
      End If
   End If
   Convert2Freq = iStat
End Function

Function Convert2Mag(ps As PlotStructure) As Integer
   Dim iStat As Integer
   Dim fVal As Single
   Dim li As Long

   If ps.Type = 4 Then Exit Function
   If ps.Type = 5 Then Exit Function
   Call LogMess(ps, "Convert to magnitude mode")
   ps.LastLevel = 0
   Call MacroRecord("Convert2Mag")
   iStat = 0
   If ps.Type <> 1 Then
      For li = 0 To ps.HugeSize / 2
         fVal = Yvalue(ps, li)
         iStat = SetHugeEl(ps.harray, 4, 2 * li, fVal)
         fVal = 0#
         iStat = SetHugeEl(ps.harray, 4, 2 * li + 1, fVal)
      Next li
      iStat = ExtractPlotData(ps, Xvalue(ps, 0&), Xvalue(ps, ps.HugeSize))
      Call SaveAsLast(ps)
      If (iStat < 0) Then
         MsgBox ("ERROR! Unable to extract plot data.")
      End If
   End If
   Convert2Mag = iStat
End Function

Function Convert2Mass(ps As PlotStructure) As Integer
   Dim iStat As Integer

   If ps.Type = 4 Then Exit Function
   If ps.Type = 5 Then Exit Function
   Call LogMess(ps, "Convert to mass")
   ps.LastLevel = 0
   Call MacroRecord("Convert2Mass")
   iStat = 0
   If ps.Type <> 3 Then
      If ps.Type = 1 Then
         iStat = realft(ps.HugeSize, ps.harray, 1)
      End If
      ps.XaxisName = "m/z"
      ps.Type = 3
      If ps.CalType = 2 Or ps.CalType = 7 Then
         If ps.Intensity = 0 Then
            ps.Intensity = GenCal.CalculateIntensity(FindPI(ps))
         End If
      End If
      iStat = ExtractPlotData(ps, Xvalue(ps, 0), Xvalue(ps, ps.HugeSize / 2))
      Call SaveAsLast(ps)
      If (iStat < 0) Then
         MsgBox ("ERROR! Unable to extract plot data.")
      End If
   End If
   Convert2Mass = iStat
End Function

Function Convert2Time(ps As PlotStructure) As Integer
   Dim iStat As Integer

   If ps.Type = 4 Then Exit Function
   If ps.Type = 5 Then Exit Function
   Call LogMess(ps, "Convert to time")
   ps.LastLevel = 0
   Call MacroRecord("Convert2Time")
   iStat = 0
   If ps.Type <> 1 Then
      ps.XaxisName = "Time (sec)"
      ps.Type = 1
      iStat = SetHugeEl(ps.harray, 4, 0, 0#)
      iStat = SetHugeEl(ps.harray, 4, 1, 0#)
      iStat = realft(ps.HugeSize, ps.harray, -1)
      iStat = ExtractPlotData(ps, Xvalue(ps, 0&), Xvalue(ps, ps.HugeSize))
      Call SaveAsLast(ps)
      If (iStat < 0) Then
         MsgBox ("ERROR! Unable to extract plot data.")
      End If
   End If
   Convert2Time = iStat
End Function

Sub CopyData(hArraySrc As Long, hArrayDes As Long, Start As Long, Lstop As Long, ln As Long)
   Dim li As Long
   Dim lj As Long
   Dim fVal As Single
   Dim iStat As Integer
   
   li = Start
   If Lstop < Start Then
      Start = Lstop
      Lstop = li
   End If
   lj = 0
   For li = Start To Start + ln - 1
      If li < Lstop Then
         iStat = GetHugeEl(hArraySrc, 4, li, fVal)
         iStat = SetHugeEl(hArrayDes, 4, lj, fVal)
      Else
         fVal = 0
         iStat = SetHugeEl(hArrayDes, 4, lj, fVal)
      End If
      lj = lj + 1
   Next li
End Sub

Sub DrawAxis(S As Object, hdc As Long, ps As PlotStructure, color As Integer, Width As Long, height As Long, TwipsPerPixelX As Integer, TwipsPerPixelY As Integer, Optional Left As Long = 0, Optional Top As Long = 0)
   Dim Tag As String
   Dim TagWidth As Integer, Max As Integer
   Dim absmax As Single
   Dim i As Integer
   Dim hndPen As Long, oldhdc As Long
   Dim FonttoUse As Long, oldFont As Long
   Dim OldColor As Long, X As Long, Y As Long
   Dim ScaleWidth As Long, ScaleHeight As Long
   
   ScaleWidth = Width
   ScaleHeight = height
   On Error Resume Next
   OldColor = S.ForeColor
   S.ForeColor = QBColor(color)
   hndPen = CreatePen(PS_SOLID, 1, QBColor(color))
   oldhdc = SelectObject(hdc, hndPen)
   Call SetTextColor(hdc, QBColor(color))
   Call SetFont(AxisSetup.AxisFonts(NUMBERS))
   FonttoUse = CreateFontObject(S, 0, TwipsPerPixelX)
   If FonttoUse = 0 Then Exit Sub
   oldFont = SelectObject(hdc, FonttoUse)
   ps.ZoomBox = False
   If (hdc = S.hdc) Then S.Cls
   S.Scale
   Set ActiveForm = S
   ' Set up the Y axis
   Max = 0
   absmax = ps.Ymax
   If Abs(ps.Ymin) > ps.Ymax Then absmax = Abs(ps.Ymax)
   For i = 0 To ps.NumYTicks
      If ps.Percent = True Then
         Tag = sprint((100# * ps.Ymin) / absmax + i * (100# / absmax) * (ps.Ymax - ps.Ymin) / ps.NumYTicks, AxisSetup.Axis(1))
      Else
         Tag = sprint(ps.Ymin + i * (ps.Ymax - ps.Ymin) / ps.NumYTicks, AxisSetup.Axis(1))
      End If
      If (FontWidth(Tag, AxisSetup.AxisFonts(NUMBERS)) > Max) Then Max = FontWidth(Tag, AxisSetup.AxisFonts(NUMBERS))
   Next i
   If ps.NoYaxisNumbers Then Max = 0
   ' Determine the extents
   If ps.YaxisName = "" Then
      ps.OriginX = Left + Max + FontWidth("X", AxisSetup.AxisFonts(NUMBERS))
   Else
      ps.OriginX = Left + Max + 1.5 * FontWidth("X", AxisSetup.AxisFonts(Yaxis)) + FontWidth("X", AxisSetup.AxisFonts(NUMBERS))
   End If
   If ps.Title = "" Then
      ps.MaxY = Top + FontHeight("X", AxisSetup.AxisFonts(NUMBERS))
   Else
      ps.MaxY = Top + 1.5 * FontHeight("X", AxisSetup.AxisFonts(Title))
   End If
   If ps.XaxisName = "" Then
      ps.OriginY = ScaleHeight - 2.5 * FontHeight("X", AxisSetup.AxisFonts(NUMBERS))
   Else
      ps.OriginY = ScaleHeight - 2.5 * FontHeight("X", AxisSetup.AxisFonts(NUMBERS)) - FontHeight("X", AxisSetup.AxisFonts(Xaxis))
   End If
   If ps.NoXaxisNumbers Then ps.OriginY = ps.OriginY + FontHeight("X", AxisSetup.AxisFonts(NUMBERS))
   ps.MaxX = ScaleWidth - FontWidth("X", AxisSetup.AxisFonts(NUMBERS))
   If Not ps.NoYaxis Then
'      S.Line (ps.OriginX, ps.OriginY)-(ps.OriginX, ps.MaxY)
      X = ps.OriginX
      Y = ps.OriginY
      Call MoveToEx(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY, 0&)
      Call LineTo(hdc, X / TwipsPerPixelX, ps.MaxY / TwipsPerPixelY)
   End If
   Call SetFont(AxisSetup.AxisFonts(NUMBERS))
   For i = 0 To ps.NumYTicks
      X = ps.OriginX - Max - S.TextWidth("X") / 2
      Y = ps.OriginY + CLng(i) * CLng(ps.MaxY - ps.OriginY) / CLng(ps.NumYTicks) - CLng(S.TextHeight("X") / 2)
      If ps.Percent = True Then
         If Not ps.NoYaxisNumbers Then
            Tag = sprint((100# * ps.Ymin) / absmax + i * (100# / absmax) * (ps.Ymax - ps.Ymin) / ps.NumYTicks, AxisSetup.Axis(1))
            Call TextOut(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY, Tag, Len(Tag))
         End If
      Else
         If Not ps.NoYaxisNumbers Then
            Tag = sprint(ps.Ymin + i * (ps.Ymax - ps.Ymin) / ps.NumYTicks, AxisSetup.Axis(1))
            Call TextOut(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY, Tag, Len(Tag))
         End If
      End If
      X = ps.OriginX
      Y = ps.OriginY + CLng(i) * CLng(ps.MaxY - ps.OriginY) / CLng(ps.NumYTicks)
      If Not ps.NoYaxis Then
         Call MoveToEx(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY, 0&)
         Call LineTo(hdc, (X - S.TextWidth("X") / 2) / TwipsPerPixelX, Y / TwipsPerPixelY)
      End If
   Next i
   ' Now do the X axis
'   S.Line (ps.OriginX, ps.OriginY)-(ps.MaxX, ps.OriginY)
   X = ps.OriginX
   Y = ps.OriginY
   Call MoveToEx(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY, 0&)
   Call LineTo(hdc, ps.MaxX / TwipsPerPixelX, Y / TwipsPerPixelY)
   For i = 0 To ps.NumXTicks
      Y = ps.OriginY + S.TextHeight("X") * 0.5
      X = CSng(ps.OriginX) + CSng(i) * (CSng(ps.MaxX) - CSng(ps.OriginX)) / CSng(ps.NumXTicks)
      Tag = sprint(ps.Xmin + i * (ps.Xmax - ps.Xmin) / ps.NumXTicks, AxisSetup.Axis(0))
      X = X - S.TextWidth(Tag) / 2
      If X < 0 Then X = 0
      If (S.TextWidth(Tag) + X) > ScaleWidth Then X = ScaleWidth - S.TextWidth(Tag) - S.TextWidth("X")
      If Not ps.NoXaxisNumbers Then Call TextOut(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY, Tag, Len(Tag))
      Y = ps.OriginY
      X = CSng(ps.OriginX) + CSng(i) * (CSng(ps.MaxX) - CSng(ps.OriginX)) / CSng(ps.NumXTicks)
      Call MoveToEx(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY, 0&)
      Call LineTo(hdc, X / TwipsPerPixelX, (Y + S.TextWidth("X") / 2) / TwipsPerPixelY)
   Next i
   ' Now place all titles on plot
   Call SetFont(AxisSetup.AxisFonts(Title))
   Call DeleteObject(FonttoUse)
   FonttoUse = CreateFontObject(S, 0, TwipsPerPixelX)
   Call SelectObject(hdc, FonttoUse)
   Y = S.TextHeight("X") / 4
   X = ScaleWidth / 2 - S.TextWidth(ps.Title) / 2
   If (ps.Title <> "") Then Call TextOut(hdc, (Left / 2 + X) / TwipsPerPixelX, (Top + Y) / TwipsPerPixelY, ps.Title, Len(ps.Title))
   Call SetFont(AxisSetup.AxisFonts(Xaxis))
   Call DeleteObject(FonttoUse)
   FonttoUse = CreateFontObject(S, 0, TwipsPerPixelX)
   Call SelectObject(hdc, FonttoUse)
   Y = ScaleHeight - FontHeight("X", AxisSetup.AxisFonts(NUMBERS)) - FontHeight("X", AxisSetup.AxisFonts(Xaxis))
   X = ps.MaxX + (ps.OriginX - ps.MaxX) / 2 - S.TextWidth(ps.XaxisName) / 2
   If (ps.XaxisName <> "") Then Call TextOut(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY, ps.XaxisName, Len(ps.XaxisName))
   Call SetFont(AxisSetup.AxisFonts(Yaxis))
   Y = ps.MaxY + (ps.OriginY - ps.MaxY) / 2 + S.TextWidth(ps.YaxisName) / 2
   Call SelectObject(hdc, oldhdc)
   Call DeleteObject(hndPen)
   Call SelectObject(hdc, oldFont)
   Call DeleteObject(FonttoUse)
   If (ps.YaxisName <> "") Then
      If RotateText(S, hdc, ps.YaxisName, 90, CSng(Left), CSng(Y), CSng(TwipsPerPixelX), CSng(TwipsPerPixelY)) = -1 Then
         For i = 1 To Len(ps.YaxisName)
            X = Left + FontWidth("X", AxisSetup.AxisFonts(Yaxis)) - FontWidth(Mid$(ps.YaxisName, i, 1), AxisSetup.AxisFonts(Yaxis)) / 2
            Y = CSng(ps.MaxY) + (CSng(ps.OriginY) - CSng(ps.MaxY)) / 2# - (Len(ps.YaxisName) * FontHeight("X", AxisSetup.AxisFonts(Yaxis))) / 2 + (i - 1) * FontHeight("X", AxisSetup.AxisFonts(Yaxis))
            Call TextOut(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY, Mid$(ps.YaxisName, i, 1), 1)
         Next i
      End If
   End If
   Call SetFont(AxisSetup.AxisFonts(NUMBERS))
   S.ForeColor = OldColor
End Sub

Function ExtractPlotData(ps As PlotStructure, Dstart As Single, Dstop As Single) As Integer
   Dim Lstart As Long
   Dim Lstop As Long
   Dim temp As Long
   Dim Adv As Long
   Dim li As Long
   Dim Max As Single
   Dim Min As Single
   Dim i As Integer
   Dim T1 As Single, T2 As Single

   If ps.harray = 0 Then Exit Function
   ' MsgBox Format(Dstart) & ", " & Format(Dstop)
   ExtractPlotData = 0
   If (ps.harray = 0) Then
      ' MsgBox "harray = 0!"
      Exit Function
   End If
   MacroRecord ("ExtractPlotData," + Format$(Dstart) + "," + Format$(Dstop))
   On Error GoTo ExtractPlotDataError
   ' This function extracts data from huge memory and
   ' places it in display buffer
   If ps.XaxisZoom <> 1 Then
      Dstart = ps.Xmin
      Dstop = ps.Xmax
   End If
   Lstart = FindIndex(ps, Dstart)
   Lstop = FindIndex(ps, Dstop)
   If Lstart = Lstop Then
'      MsgBox Format(Lstart)
      Exit Function
   End If
   If Lstop < Lstart Then
      temp = Lstart
      Lstart = Lstop
      Lstop = temp
   End If
   Max = ps.Ymax
   Min = ps.Ymin
   ps.Lstart = Lstart
   If ps.Type = 5 Then
      ' MsgBox Format(Dstart) & ", " & Format(Dstop) & ", " & Format(Lstart) & ", " & Format(Lstop)
      ' here if we are extracting data from a block if X,Y pairs of floats
      ps.n = CInt(HugeExtract(ps.harray, ps.Y(0), ps.Ymax, ps.Ymin, Lstart, Lstop, CLng(BUFSIZE), 6&))
      ' MsgBox "Extracted Y data " & Format(ps.n) & ", Min " & Format(ps.Ymin) & ", Max " & Format(ps.Ymax)
      ' MsgBox Format(ps.Y(1)) & ", " & Format(ps.Y(25))
      If (ps.Ymax = ps.Ymin) Or (ps.YaxisFixed = True) Then
         ps.Ymax = Max
         ps.Ymin = Min
      End If
      If ps.YaxisZoom = True Then
         ps.Ymax = Max
         ps.Ymin = Min
         Max = Ycursor(ps.MouseY, ps)
         Min = Ycursor(ps.MouseYold, ps)
         ps.Ymax = Max
         ps.Ymin = Min
      End If
      Adv = (Lstop - Lstart) / BUFSIZE + 0.5
      ps.Skip = Adv
      Call HugeExtract(ps.harray, ps.X(0), T1, T2, Lstart, Lstop, CLng(BUFSIZE), 7&)
      ' MsgBox Format(ps.X(1)) & ", " & Format(ps.X(25))
   End If
   If ps.Type = 1 Or ps.Type = 4 Then
      ps.n = CInt(HugeExtract(ps.harray, ps.Y(0), ps.Ymax, ps.Ymin, Lstart, Lstop, CLng(BUFSIZE), 2&))
      If (ps.Ymax = ps.Ymin) Or (ps.YaxisFixed = True) Then
         ps.Ymax = Max
         ps.Ymin = Min
      End If
      If ps.YaxisZoom = True Then
         ps.Ymax = Max
         ps.Ymin = Min
         Max = Ycursor(ps.MouseY, ps)
         Min = Ycursor(ps.MouseYold, ps)
         ps.Ymax = Max
         ps.Ymin = Min
      End If
      Adv = (Lstop - Lstart) / BUFSIZE + 0.5
      ps.Skip = Adv
      For li = Lstart To Lstop Step Adv
         ps.X(i) = Xvalue(ps, li)
         i = i + 1
      Next li
   End If
   If (ps.Type = 2) Then
      ' calculate the start and stop index
      If Lstop > ps.HugeSize / 2 Then Lstop = ps.HugeSize / 2
      ps.n = CInt(HugeExtract(ps.harray, ps.Ym(0), ps.Ymax, ps.Ymin, Lstart, Lstop, BUFSIZE, 4))
      ps.n = HugeExtract(ps.harray, ps.Y(0), ps.Ymax, ps.Ymin, Lstart, Lstop, CLng(BUFSIZE), 3&)
      ps.Ymin = 0#
      If (ps.Ymax = ps.Ymin) Or (ps.YaxisFixed = True) Then
         ps.Ymax = Max
         ps.Ymin = Min
      End If
      If ps.YaxisZoom = True Then
         ps.Ymax = Max
         ps.Ymin = Min
         Max = Ycursor(ps.MouseY, ps)
         Min = Ycursor(ps.MouseYold, ps)
         ps.Ymax = Max
         ps.Ymin = Min
      End If
      Adv = (Lstop - Lstart) / BUFSIZE + 0.5
      ps.Skip = Adv
      For li = Lstart To Lstop Step Adv
         ps.X(i) = Xvalue(ps, li)
         i = i + 1
      Next li
   End If
   If ps.Type = 3 Then
      ' calculate the start and stop index
      If Lstop > ps.HugeSize / 2 Then Lstop = ps.HugeSize / 2
      ps.n = CInt(HugeExtract(ps.harray, ps.Ym(0), ps.Ymax, ps.Ymin, Lstart, Lstop, BUFSIZE, 4))
      ps.n = CInt(HugeExtract(ps.harray, ps.Y(0), ps.Ymax, ps.Ymin, Lstart, Lstop, BUFSIZE, 3))
      ps.Ymin = 0#
      If (ps.Ymax = ps.Ymin) Or (ps.YaxisFixed = True) Then
         ps.Ymax = Max
         ps.Ymin = Min
      End If
      If ps.YaxisZoom = True Then
         ps.Ymax = Max
         ps.Ymin = Min
         Max = Ycursor(ps.MouseY, ps)
         Min = Ycursor(ps.MouseYold, ps)
         ps.Ymax = Max
         ps.Ymin = Min
      End If
      Adv = (Lstop - Lstart) / BUFSIZE + 0.5
      ps.Skip = Adv
      For li = Lstart To Lstop Step Adv
         ps.X(i) = Xvalue(ps, li)
         i = i + 1
      Next li
   End If
   ps.Xmin = Xvalue(ps, Lstart)
   ps.Xmax = Xvalue(ps, Lstop)
   ' MsgBox Format(ps.Xmin) & ", " & Format(ps.Xmax)
   If ps.Xmin > ps.Xmax Then
      ps.Xmax = Xvalue(ps, Lstart)
      ps.Xmin = Xvalue(ps, Lstop)
   End If
   Exit Function
ExtractPlotDataError:
   Erase ps.Y
   ps.Ymax = 0
   ps.Ymin = 0
   Call ErrorMess.LogError(Error$ & " in ExtractPlotData!", "")
   Exit Function
End Function
Function FindMax(ps As PlotStructure, Xmin As Single, Xmax As Single, Optional MaxValue As Single = 0#) As Single
Dim Lstart As Long, Lstop As Long, temp As Long, li As Long
Dim fVal As Single

   Lstart = FindIndex(ps, Xmin)
   Lstop = FindIndex(ps, Xmax)
   If Lstart > Lstop Then
      temp = Lstart
      Lstart = Lstop
      Lstop = temp
   End If
   FindMax = 0#
   For li = Lstart To Lstop
      fVal = Yvalue(ps, li)
      If fVal > FindMax Then FindMax = fVal
   Next
End Function
Function FindMin(ps As PlotStructure, Xmin As Single, Xmax As Single, Optional MaxValue As Single = 0#) As Single
Dim Lstart As Long, Lstop As Long, temp As Long, li As Long
Dim fVal As Single

   Lstart = FindIndex(ps, Xmin)
   Lstop = FindIndex(ps, Xmax)
   If Lstart > Lstop Then
      temp = Lstart
      Lstart = Lstop
      Lstop = temp
   End If
   FindMin = Yvalue(ps, Lstart)
   For li = Lstart To Lstop
      fVal = Yvalue(ps, li)
      If fVal < FindMin Then FindMin = fVal
   Next
End Function
Function FindAverage(ps As PlotStructure, Xmin As Single, Xmax As Single, Optional MaxValue As Single = 0#) As Single
Dim Lstart As Long, Lstop As Long, temp As Long, li As Long
Dim Threshold As Single, fVal As Single, Num As Long

   Lstart = FindIndex(ps, Xmin)
   Lstop = FindIndex(ps, Xmax)
   If Lstart > Lstop Then
      temp = Lstart
      Lstart = Lstop
      Lstop = temp
   End If
   Threshold = 0#
   Num = 0
   If MaxValue = 0# Then
      For li = Lstart To Lstop
         Threshold = Threshold + Yvalue(ps, li)
         Num = Num + 1
      Next
   Else
      For li = Lstart To Lstop
         fVal = Yvalue(ps, li)
         If fVal < MaxValue Then
            Num = Num + 1
            Threshold = Threshold + fVal
         End If
      Next
   End If
   FindAverage = Threshold / Num
End Function
Function FindDetectSlice(FileName As String) As String
Dim Pos As Long
Dim Trailor As Long
Dim FileNum As Integer
Dim Rtype As Byte
Dim Dtype As Byte
Dim cnt As Integer
Dim Tag As String
Dim ln As Integer
Dim FH As FinniganHeader

   FindDetectSlice = ""
   If ReadFinniganHeader(FileName, FH) = -1 Then Exit Function
   cnt = FindTrailorIndex(FileName, "Detect_Slice")
   If cnt = -1 Then Exit Function
   Trailor = 65 + FH.SeqSize + FH.DataSize + 16 * cnt
   Rtype = ReadSunFileByte(FileName, Trailor)
   Dtype = ReadSunFileByte(FileName, Trailor + 1)
   ln = ReadSunFileInt(FileName, Trailor + 2)
   If (Rtype = 1) And (Dtype = 8) Then
      ' Now read the detect slice number!
      FileNum = FreeFile
      Open FileName For Binary Access Read As FileNum
      Seek FileNum, Trailor + 8
      Tag = String$(ln - 1, " ")
      Get FileNum, , Tag
      Close FileNum
      FindDetectSlice = Tag
      Exit Function
   Else
      ' If here, then the slice name is saved in misc area
      If (Rtype = 17) And (Dtype = 8) Then
         Pos = 65 + FH.SeqSize + FH.DataSize + FH.TrailorRecordsSize + FH.TrailorRecordsNamesSize
         Trailor = Pos + ReadSunFileLong(FileName, Trailor + 8)
         FileNum = FreeFile
         Open FileName For Binary Access Read As FileNum
         Seek FileNum, Trailor
         Tag = String$(ln - 1, " ")
         Get FileNum, , Tag
         Close FileNum
         FindDetectSlice = Tag
         Exit Function
      End If
   End If
End Function

' This function tries to determine the file type.
' -1 is returned if the file type is unknown, else
'    2 = floats, sun extrel format
'    3 = floats, with header
'    4 = Excite Waveform
'    5 = ASCII data
'    6 = Midas format
'    7 = IonSpec OMEGA version 3.3
Function FindFileType(FileName As String) As Integer
   Dim FileNum As Integer
   Dim rec As String
   Dim Ft As Long, Fti As Integer
   
   ' open file
   FileName = FileName
   FileNum = FreeFile
   Open FileName For Binary Access Read As FileNum
   ' test for FTMS format
   Seek FileNum, 49
   rec = "               "
   Get FileNum, , rec
   If rec = "PARAMETERS-v1.0" Then
      FindFileType = 2
      Close FileNum
      Exit Function
   End If
   ' test for icr-2ls format
   Seek FileNum, 1
   rec = "                 "
   Get FileNum, , rec
   If rec = "ICR-2LS Data File" Then
      FindFileType = 3
      Close FileNum
      Exit Function
   End If
   ' test for midas format
   Seek FileNum, 1
   rec = "               "
   Get FileNum, , rec
   If rec = "MIDAS Data File" Then
      FindFileType = 6
      Close FileNum
      Exit Function
   End If
   ' test for IonSpec OMEGA version 3.3
   Seek FileNum, 1
   Get FileNum, , ISH
   If (ISH.revmaj = 3) And (ISH.revmin = 3) Then
      FindFileType = 7
      Close FileNum
      Exit Function
   End If
   ' Test for IonSpec version 6 or 7
   Seek FileNum, 5
   Get FileNum, , Ft
   If Ft = 6 Or Ft = 7 Then
      FindFileType = 7
      Exit Function
   End If
   ' Test for IonSpec OMEGA version 5.xx
   Seek FileNum, 1
   Get FileNum, , Fti
   If Fti = 6400 Then
      ' if here its a OMEGA 7.xx, this code is not complete!
      ' I need more info on this file type
      Seek FileNum, 133
      Get FileNum, , ISH.revmaj
      ISH.revmin = 0
      If ISH.revmaj = 7 Then
         FindFileType = 7
      Else
         FindFileType = -1
      End If
      Close FileNum
      Exit Function
   End If
   Seek FileNum, 1
   Get FileNum, , Ft
   If (Ft = 61089) Then
      Seek FileNum, 5
      Get FileNum, , ISH.revmaj
      Seek FileNum, 9
      Get FileNum, , ISH.revmin
      If (ISH.revmaj = 5) Or (ISH.revmaj = 6) Then
         FindFileType = 7
      Else
         FindFileType = -1
      End If
      Close FileNum
      Exit Function
   End If
   ' test for bruker data file
   Seek FileNum, 1
   rec = String(29, " ")
   Get FileNum, , rec
   If rec = "##TITLE= XMASS Parameter file" Or rec = "##TITLE= UXNMR Parameter file" Then
      FindFileType = ftBRUKER
      Close FileNum
      Exit Function
   End If
   Close FileNum
   FindFileType = -1
End Function

Function FindIndex(ps As PlotStructure, Value As Single) As Long
   Dim Index As Long, li As Long
   Dim Freq As Single, fVal As Single

   Select Case ps.Type
      Case 1   ' Time
         Index = Value * ps.SampleRate
         If Index > ps.HugeSize Then Index = ps.HugeSize
         If Index < 0 Then Index = 0
      Case 4
         If Value2Time(ps, CDbl(Value)) * ps.SampleRate > ps.HugeSize Then
            Index = ps.HugeSize
         Else
            Index = Value2Time(ps, CDbl(Value)) * ps.SampleRate
         End If
         If Index > ps.HugeSize Then Index = ps.HugeSize
         If Index < 0 Then Index = 0
      Case 2   ' Freq
         If (ps.LowMassFreq = 0) Then
            Index = ((Value - ps.FreqShift) / ps.SampleRate) * ps.HugeSize
         Else
            Index = ((ps.LowMassFreq - Value - ps.FreqShift) / ps.SampleRate) * ps.HugeSize
         End If
         If Index > ps.HugeSize / 2 Then Index = ps.HugeSize
         If Index <= 0 Then Index = 0
      Case 3
         If (ps.LowMassFreq = 0) Then
            Index = ((Mass2Freq(ps, CDbl(Value)) - ps.FreqShift) / ps.SampleRate) * ps.HugeSize
         Else
            Freq = Mass2Freq(ps, CDbl(Value))
            If Freq > ps.LowMassFreq Then Freq = ps.LowMassFreq
            If Freq < (ps.LowMassFreq - ps.SampleRate / 2) Then Freq = (ps.LowMassFreq - ps.SampleRate / 2)
            Index = ((ps.LowMassFreq - Mass2Freq(ps, CDbl(Value)) - ps.FreqShift) / ps.SampleRate) * ps.HugeSize
         End If
         If Index > ps.HugeSize / 2 Then Index = ps.HugeSize / 2
         If Index <= 0 Then Index = 0
      Case 5
         ' here for X,Y pairs
         li = 2 ^ 29
         Index = 0
         Do
            If (li Or Index) < ps.HugeSize / 2 Then
               Call GetHugeEl(ps.harray, 4, (Index Or li) * 2, fVal)
               If fVal < Value Then
                  Index = Index Or li
               End If
               If li = 0 Then Exit Do
            End If
            li = Int(li / 2)
         Loop
         If Index > ps.HugeSize / 2 - 1 Then Index = ps.HugeSize / 2 - 1
         ' MsgBox Format(Value) & ", " & Format(Index)
   End Select
   FindIndex = Index
End Function
Function FindTICoffsets(FileName As String, Offsets() As Long) As Long
Dim FH As FinniganHeader
Dim Pos As Long
Dim Rtype As Byte
Dim Dtype As Byte
Dim cnt As Long, OffsetsLocation As Long
Dim i As Integer, j As Integer

   FindTICoffsets = -1
   If ReadFinniganHeader(FileName, FH) = -1 Then Exit Function
   cnt = FindTrailorIndex(FileName, "TIC_Index_Offsets")
   If cnt = -1 Then Exit Function
   ' Now read the number of spectra in TIC
   Pos = 65 + FH.SeqSize + FH.DataSize + 16 * cnt
   Rtype = ReadSunFileByte(FileName, Pos)
   Dtype = ReadSunFileByte(FileName, Pos + 1)
   i = ReadSunFileInt(FileName, Pos + 2) / 4
   ReDim Offsets(i) As Long
   If Rtype = 1 And Dtype = 68 Then
      For j = 0 To i - 1
         Offsets(j) = 64 + 1024 + FH.SeqSize + ReadSunFileLong(FileName, Pos + 8 + 4 * j)
      Next
      FindTICoffsets = 0
      Exit Function
   End If
   If Rtype = 17 And Dtype = 68 Then
      OffsetsLocation = ReadSunFileLong(FileName, Pos + 8)
      Pos = 65 + FH.SeqSize + FH.DataSize + FH.TrailorRecordsSize + FH.TrailorRecordsNamesSize
      OffsetsLocation = OffsetsLocation + Pos
      For j = 0 To i - 1
         Offsets(j) = 64 + 1024 + FH.SeqSize + ReadSunFileLong(FileName, OffsetsLocation + 4 * j)
      Next
      FindTICoffsets = 0
      Exit Function
   End If
End Function
Sub FindStartStopIndex(ps As PlotStructure, Dstart As Single, Dstop As Single, Lstart As Long, Lstop As Long)
   Dim temp As Long
   
   Lstart = FindIndex(ps, Dstart)
   Lstop = FindIndex(ps, Dstop)
   If Lstop < Lstart Then
      temp = Lstart
      Lstart = Lstop
      Lstop = temp
   End If
End Sub
Function BuildHistogram(ps As PlotStructure, Dstart As Single, Dstop As Single, Optional NoScope As Boolean = False, Optional MaxX As Single = 0)
Const HistSize = 2048
Dim Lstart As Long, Lstop As Long, PI As Integer
Dim History(HistSize) As Long, Max As Single, BinSize As Single
Dim li As Long, lj As Long, fVal As Single
Dim MaxY As Long, Min As Single
Dim Ans As String * 100, Thres As Single

   li = GetPrivateProfileString("Scope", "HistThres", "0", Ans, 40, "icr-2ls.ini")
   Thres = val(Ans)
   Call FindStartStopIndex(ps, Dstart, Dstop, Lstart, Lstop)
   Max = FindMax(ps, Dstart, Dstop)
   Min = FindMin(ps, Dstart, Dstop)
   If Min < Thres Then Min = Thres
   If Min > 0 Then Min = 0
   BinSize = (Max - Min) / HistSize
   ' Now loop through the data and build the historgam
   For li = Lstart To Lstop
      fVal = Yvalue(ps, li) - Min
      If fVal >= Thres Then
         lj = fVal / BinSize
         If lj >= 0 Then
            History(lj) = History(lj) + 1
         End If
      End If
   Next
   ' Find the max value's x axis...
   MaxY = History(2)
   MaxX = BinSize * 2
   For li = 2 To HistSize - 1
      If History(li) >= MaxY Then
         MaxY = History(li)
         MaxX = BinSize * li
      End If
   Next
   If NoScope = True Then Exit Function
   ' Now build a scope and display the histogram...
   PI = NextScope
   BuildHistogram = PI
   If PI < 0 Then Exit Function
   Call SetScopeDefaults(Plots(PI))
   Plots(PI).HugeSize = HistSize
   Plots(PI).HugeSizeInitial = HistSize
   Plots(PI).SampleRate = 1# / BinSize
   Plots(PI).Type = 1
   Plots(PI).Title = ps.Title & " Histogram"
   Plots(PI).XaxisName = "Bin amplitude"
   Plots(PI).YaxisName = "Number of events"
   If HugeDim(Plots(PI).harray, 4, Plots(PI).HugeSize) = 0 Then
      For li = 0 To HistSize - 1
         fVal = History(li)
         Call SetHugeEl(Plots(PI).harray, 4, li, fVal)
      Next
      Plots(PI).XminFull = Xvalue(Plots(PI), 0&)
      Plots(PI).XmaxFull = Xvalue(Plots(PI), Plots(PI).HugeSize)
      Call ExtractPlotData(Plots(PI), Plots(PI).XminFull, Plots(PI).XmaxFull)
      Call AllocateScope(PI)
   Else
      BuildHistogram = -1
   End If
   Call LogMess(Plots(PI), "Minimum = " & Format(Min))
   Call LogMess(Plots(PI), "Maximum = " & Format(Max))
End Function
Function FindPeakIntensity(Peak As Double) As Double
Dim i As Long

   On Error Resume Next
   For i = 0 To UBound(PeakX)
      If Abs(PeakX(i) - Peak) < 0.000001 Then
         FindPeakIntensity = PeakY(i)
         Exit Function
      End If
   Next
End Function
Function FindPeak(ps As PlotStructure, Dstart As Single, Dstop As Single) As Single
   Dim Lstart As Long
   Dim Lstop As Long
   Dim li As Long
   Dim Max As Single

   Call FindStartStopIndex(ps, Dstart, Dstop, Lstart, Lstop)
   Max = Yvalue(ps, Lstart)
   For li = Lstart To Lstop
      If Yvalue(ps, li) >= Max Then
         Max = Yvalue(ps, li)
         FindPeak = Xvalue(ps, li)
      End If
   Next li
End Function
Sub SetPeakToZero(ps As PlotStructure, Dstart As Single, FWHM As Single, Optional CountThres As Integer = 1)
   Dim Lstart As Long, li As Long
   Dim LastPoint As Single, ThisPoint As Single
   Dim GoingDown As Integer, count As Integer

   Lstart = FindIndex(ps, Dstart)
   LastPoint = Yvalue(ps, Lstart)
   GoingDown = True
   count = 0
   For li = Lstart + 1 To ps.HugeSize / 2
      ThisPoint = Yvalue(ps, li)
      If (ThisPoint <= LastPoint) Then
         count = 0
      Else
         count = count + 1
         If count >= CountThres Then Exit For
      End If
      If ps.Type = 5 Then
         Call SetHugeEl(ps.harray, 4, 2 * li + 1, 0#)
      Else
         Call SetHugeEl(ps.harray, 4, 2 * li, 0#)
         Call SetHugeEl(ps.harray, 4, 2 * li + 1, 0#)
      End If
      LastPoint = ThisPoint
   Next
   count = 0
   LastPoint = Yvalue(ps, Lstart)
   For li = Lstart To 0 Step -1
      ThisPoint = Yvalue(ps, li)
      If (ThisPoint <= LastPoint) Then
         count = 0
      Else
         count = count + 1
         If count >= CountThres Then Exit For
      End If
      If ps.Type = 5 Then
         Call SetHugeEl(ps.harray, 4, 2 * li + 1, 0#)
      Else
         Call SetHugeEl(ps.harray, 4, 2 * li, 0#)
         Call SetHugeEl(ps.harray, 4, 2 * li + 1, 0#)
      End If
      LastPoint = ThisPoint
   Next
End Sub
Function FindNextPeak(ps As PlotStructure, Dstart As Single, Dstop As Single, Threshold As Single) As Single
   Dim Lstart As Long
   Dim Lstop As Long
   Dim temp As Long
   Dim li As Long, SS As Integer
   Dim Max As Single, LastPoint As Single, ThisPoint As Single
   Dim GoingDown As Integer, PeakFound As Integer

   FindNextPeak = -1#
   Lstart = FindIndex(ps, Dstart)
   Lstop = FindIndex(ps, Dstop)
   SS = 1
   If Lstop < Lstart Then
      SS = -1
   End If
   LastPoint = Yvalue(ps, Lstart)
   GoingDown = True
   PeakFound = False
   For li = Lstart To Lstop Step SS
      ThisPoint = Yvalue(ps, li)
      If ThisPoint <= LastPoint Then
         If (GoingDown = False) And (PeakFound = True) Then Exit Function
         GoingDown = True
         Max = Yvalue(ps, li)
      Else
         GoingDown = False
      End If
      If (ThisPoint >= Threshold) And (ThisPoint > Max) And (GoingDown = False) Then
         Max = ThisPoint
         PeakFound = True
         FindNextPeak = Xvalue(ps, li)
      End If
      LastPoint = ThisPoint
   Next li
End Function
Function FindTICsize(FileName As String) As Long
Dim FH As FinniganHeader
Dim Pos As Long
Dim Rtype As Byte
Dim Dtype As Byte
Dim cnt As Long

   FindTICsize = -1
   If ReadFinniganHeader(FileName, FH) = -1 Then Exit Function
   cnt = FindTrailorIndex(FileName, "Data_Count")
   If cnt = -1 Then Exit Function
   ' Now read the number of spectra in TIC
   Pos = 65 + FH.SeqSize + FH.DataSize + 16 * cnt
   Rtype = ReadSunFileByte(FileName, Pos)
   Dtype = ReadSunFileByte(FileName, Pos + 1)
   If Rtype = 1 And Dtype = 4 Then
      FindTICsize = ReadSunFileLong(FileName, Pos + 8)
   End If
End Function
Function FontHeight(Tag As String, fntyp As FN) As Integer
   Call SetFont(fntyp)
   FontHeight = ActiveForm.TextHeight(Tag)
End Function
Function FindTrailorIndex(FileName As String, VarName As String) As Long
Dim FH As FinniganHeader
Dim FileNum As Integer
Dim Pos As Long, Length As Long, cnt As Long
Dim St As Long, ln As Long
Dim Labels As String, Tag As String

   FindTrailorIndex = -1
   If (ReadFinniganHeader(FileName, FH) <> 0) Then Exit Function
   Pos = FH.SeqSize + FH.DataSize + FH.TrailorRecordsSize + 65
   Length = FH.TrailorRecordsNamesSize
   FileNum = FreeFile
   Open FileName For Binary Access Read As FileNum
   Seek FileNum, Pos
   Labels = String$(Length, " ")
   Get FileNum, , Labels
   Close FileNum
   St = 1
   cnt = 0
   While St < Length
      ln = InStr(St, Labels, Chr$(0))
      Tag = Mid$(Labels, St, ln - St)
      If Tag = VarName Then
         FindTrailorIndex = cnt
      End If
      cnt = cnt + 1
      St = ln + 1
   Wend
End Function

Function FontWidth(Tag As String, fntyp As FN) As Integer
   Call SetFont(fntyp)
   FontWidth = ActiveForm.TextWidth(Tag)
End Function

Function IsColorPrinter() As Integer
   On Error GoTo IsColorPrinterError
'   Printer.ColorMode = 2
   IsColorPrinter = True
   Exit Function
IsColorPrinterError:
   IsColorPrinter = False
   Exit Function
End Function

Function LoadAll(FileName As String) As Integer
   Dim FileNum As Integer
   Dim FileHandle As Long
   Dim i As Integer
   Dim j As Integer
   Dim FileH As Integer
   Dim Pos As Long
   Dim li As Long
   Dim iStat As Long
   Dim str As String
   Dim version As Single
   
   ' first make sure there are no open scopes
   For i = 0 To MAXSCOPES - 1
      If Plots(i).InUse Then
         MsgBox "You must close all scopes before loading!"
         Exit Function
      End If
   Next i
   FileNum = FreeFile
   Open FileName For Binary Access Read As FileNum
   ' Read header stuff...
   Get FileNum, , i
   str = String$(i, " ")
   Get FileNum, , str     ' file type and version
   version = val(Mid(str, InStr(str, "Version") + 7))
   Get FileNum, , i
   str = String$(i, " ")
   Get FileNum, , str
   If str <> "" Then MsgBox str
   ' First load all of the plot structures...
   For i = 0 To MAXSCOPES - 1
      Get FileNum, , Plots(i)
   Next i
   For i = 0 To MAXSCOPES - 1
      If Plots(i).InUse = False Then Plots(i).InUse = 2
   Next i
   ' Now load all data...
   Pos = Seek(FileNum)
   FileHandle = lopen(FileName, OF_READ)
   For i = 0 To MAXSCOPES - 1
      If Plots(i).InUse = True Then
         Plots(i).harray = 0
         iStat = HugeDim(Plots(i).harray, 4, Plots(i).HugeSize)
         li = HugeLoadFloat(Plots(i).harray, Plots(i).HugeSize, FileHandle, Pos)
         iStat = ExtractPlotData(Plots(i), Plots(i).Xmin, Plots(i).Xmax)
         Pos = Pos + Plots(i).HugeSize * 4
         Plots(i).InUse = False
         AllocateScope
         DoEvents
      End If
   Next i
   lclose (FileHandle)
   Seek FileNum, Pos
   For i = 0 To MAXSCOPES - 1
      If Plots(i).InUse = True Then
         Scopes(i).RestoreState (FileNum)
      End If
   Next i
   For i = 0 To MAXSCOPES - 1
      If Plots(i).InUse = 2 Then Plots(i).InUse = False
   Next i
   If (version >= 2#) Then
      Get FileNum, , pk
   Else
      Seek FileNum, Seek(FileNum) + 822
   End If
   Get FileNum, , AxisSetup
   For i = 0 To NUMMACROS - 1
      Get FileNum, , Macs(i)
   Next i
   Get #FileNum, , ExciteData
   Get #FileNum, , SysDirs
   For i = 0 To 3
      Get FileNum, , H(i)
   Next i
   Close FileNum
End Function
Function LoadData(ps As PlotStructure, FilePointer As Long, Optional ffp As Double = 0#) As Long
   Dim FIDFileName As String
   Dim FileNum As Integer
   Dim i As Integer, ln As Integer
   Dim ftype As Integer
   Dim ivals As IntRec
   Dim Num As Long, li As Long, LIndex As Long
   Dim FileHandle As Long
   Dim Pos As Integer
   Dim iStat As Long, DataCount As Long
   Dim ByteOrder As Integer
   Dim Frec As String
   Dim Offsets() As Long
   Dim fVal As Single, tVal As Single, temp As Single

   On Error GoTo LoadDataError
   If ps.FileType = 1 Then
      ' Here if its a integer file with no header
      ' Assume the data structure values are defined
      ' Open file
      FileHandle = lopen(ps.FileName, 0&)
      ' Now allocate huge memory space for data
      iStat = HugeDim(ps.harray, 4, ps.HugeSize)
      If (iStat <> 0) Then
         MsgBox ("ERROR! can't allocate memory. " + Format(iStat))
         LoadData = iStat
         Exit Function
      End If
      ByteOrder = LoadScope.cbxByteOrder
      If LoadScope.opt16bit Then
         iStat = HugeLoadInt(ps.harray, ps.HugeSize, FileHandle, FilePointer, ByteOrder)
      End If
      If LoadScope.opt32bit Then
         iStat = HugeLoadLong(ps.harray, ps.HugeSize, FileHandle, FilePointer, ByteOrder)
      End If
      If LoadScope.opt24bit Then
         iStat = HugeLoad24(ps.harray, ps.HugeSize, FileHandle, FilePointer, ByteOrder)
      End If
      If LoadScope.optFloat Then
         If ByteOrder = 0 Then
            iStat = HugeLoadFloat(ps.harray, ps.HugeSize, FileHandle, FilePointer)
         Else
            iStat = HugeLoadSunFloat(ps.harray, ps.HugeSize, FileHandle, FilePointer, 1)
         End If
      End If
      If (iStat < 0) Then
         If ReportErrors Then MsgBox ("ERROR! can't read input file." + Format(iStat))
      End If
      LoadData = iStat
      lclose (FileHandle)
   End If
   If ps.FileType = 2 Then    ' Finnigan data files...
      FileNum = FreeFile
      Open ps.FileName For Binary Access Read As FileNum
      Seek FileNum, 4
      Get FileNum, , ftype
      Close FileNum
      ftype = ftype And &HFF
      If ftype <> 1 And ftype <> 2 And ftype <> 3 And ftype <> 37 Then
         If MacroPlaying <> True Then Call ErrorMess.LogError("Error", "This Finnigan data file type is not supported!")
         LoadData = -1
         Exit Function
      End If
      If ftype = 1 Then
         ps.Type = 1
      Else
         ps.Type = 3
      End If
      If ftype = 37 Then
         ' Here if its a TIC file!
         ps.Type = 1
         ps.HugeSize = FindTICsize(ps.FileName)
         If ps.HugeSize = -1 Then Exit Function
         DataCount = ps.HugeSize
         ps.SampleRate = 1
         ps.XaxisName = "Spectrum #"
         ps.Xmin = 0
         ps.Xmax = DataCount
         ps.HugeSize = 2 ^ Int(Log(CSng(ps.HugeSize)) / Log(2) + 0.99999)
         If FindTICoffsets(ps.FileName, Offsets) = -1 Then Exit Function
      End If
      iStat = HugeDim(ps.harray, 4, ps.HugeSize)
      If (iStat <> 0) Then
         MsgBox ("ERROR! can't allocate memory.")
         LoadData = iStat
         Exit Function
      End If
      ' now read and convert data
      FileHandle = lopen(ps.FileName, 0&)
      If ftype = 37 Then
         ' Here if TIC file
         Call HugeZeroRange(ps.harray, 0&, ps.HugeSize)
         For i = 0 To UBound(Offsets) - 1
            ln = DataCount - i * 64
            If ln > 64 Then ln = 64
            iStat = HugeLoadSunFloat(ps.harray + i * 256, ln, FileHandle, Offsets(i), 1)
         Next
      Else
         Call HugeZeroRange(ps.harray, 0&, ps.HugeSize)
         iStat = HugeLoadSunFloat(ps.harray, ps.HugeSize, FileHandle, FilePointer, ftype)
         If ((ftype = 2) Or (ftype = 3)) And (ps.LowMassFreq <> 0#) Then
            iStat = HugeReverseOrder(ps.harray, ps.HugeSize, 4)
         End If
      End If
      lclose (FileHandle)
      If (iStat < 0) Then
         If ReportErrors Then MsgBox ("ERROR! can't read input file, in LoadData")
      End If
      LoadData = iStat
   End If
   If (ps.FileType = 3) Or (ps.FileType = 6) Or (ps.FileType = 9) Then
      FileHandle = lopen(ps.FileName, 0&)
      iStat = HugeDim(ps.harray, 4, ps.HugeSize)
      If (iStat <> 0) Then
         MsgBox ("ERROR! can't allocate memory.")
         LoadData = -1
         Exit Function
      End If
      If ps.FileType <> 9 Then
         iStat = HugeLoadFloat(ps.harray, ps.HugeSize, FileHandle, FilePointer)
      Else
         iStat = HugeLoadInt(ps.harray, ps.HugeSize, FileHandle, FilePointer, 0)
      End If
      If (iStat < 0) Then
         If ReportErrors Then MsgBox ("ERROR! can't read input file, in LoadData")
      End If
      LoadData = iStat
      lclose (FileHandle)
   End If
   If (ps.FileType = 7) Then  ' IonSpec data
      FileHandle = lopen(ps.FileName, 0&)
      iStat = HugeDim(ps.harray, 4, ps.HugeSize)
      If (iStat <> 0) Then
         MsgBox ("ERROR! can't allocate memory.")
         LoadData = -1
         Exit Function
      End If
      If ISH.revmaj = 3 Then
         iStat = HugeLoadInt(ps.harray, ps.HugeSize, FileHandle, FilePointer, 0)
      Else
         If ISH.ftype = 16 Then
            iStat = HugeLoadInt(ps.harray, ps.HugeSize, FileHandle, FilePointer, 0)
         Else
            iStat = HugeLoadLong(ps.harray, ps.HugeSize, FileHandle, FilePointer, 0)
         End If
      End If
      If (iStat < 0) Then
         If ReportErrors Then MsgBox ("ERROR! can't read input file, in LoadData")
      End If
      LoadData = iStat
      lclose (FileHandle)
   End If
   If ps.FileType = 5 Then  ' ASCII formated data
      ' Fill in the plot parameters from the load scope values
      ps.Xmin = CSng(LoadScope.tbxLow)
      ps.Xmax = CSng(LoadScope.tbxHigh)
      iStat = HugeDim(ps.harray, 4, ps.HugeSize)
      If (iStat <> 0) Then
         MsgBox ("ERROR! can't allocate memory.")
         LoadData = -1
         Exit Function
      End If
      iStat = HugeZero(ps.harray, ps.HugeSize)
      FileNum = FreeFile
      Open ps.FileName For Input Access Read As FileNum
      If FilePointer = 0 Then FilePointer = 1
      Seek FileNum, FilePointer
      ' Skip requested lines...
      For i = 0 To CInt(LoadScope.tbxSkip) - 1
'         Input #FileNum, Frec
         Frec = GetString(FileNum)
      Next i
      For li = 0 To ps.HugeSize - 1
         If Not EOF(FileNum) Then
'            Input #FileNum, Frec
            Frec = GetString(FileNum)
            fVal = val(GetToken(Frec, CInt(LoadScope.tbxYcol) - 1))
            If CInt(LoadScope.tbxXcol) <> -1 Then
               tVal = val(GetToken(Frec, CInt(LoadScope.tbxXcol) - 1))
               LIndex = FindIndex(ps, tVal)
               If ps.Type <> 1 Then LIndex = LIndex * 2
               If LoadScope.chkHistogram = 1 Then
                  iStat = GetHugeEl(ps.harray, 4, LIndex, temp)
                  temp = temp + fVal
               Else
                  temp = fVal
               End If
               iStat = SetHugeEl(ps.harray, 4, LIndex, temp)
               li = 0
            Else
               iStat = SetHugeEl(ps.harray, 4, li, fVal)
            End If
         End If
      Next li
      Close FileNum
      LoadData = 0
   End If
   If ps.FileType = ftBRUKER Then
      ' Build the file path to fid. the current filename points to
      ' acqus. replace with fid and open...
      For i = Len(ps.FileName) To 1 Step -1
         If (Mid(ps.FileName, i, 1) = "\") Then Exit For
         If (Mid(ps.FileName, i, 1) = ":") Then Exit For
      Next
      FIDFileName = Mid(ps.FileName, 1, i) & "fid"
      If Dir(FIDFileName) = "" Then FIDFileName = Mid(ps.FileName, 1, i) & "ser"
      If Dir(FIDFileName) = "" Then
         ICR_2LS.FileSelect.DialogTitle = "Can't find FID or SER! please locate"
         ICR_2LS.FileSelect.FileName = "*.*"
         ICR_2LS.FileSelect.Flags = &H2
         ICR_2LS.FileSelect.Action = 1
         FIDFileName = ICR_2LS.FileSelect.FileName
      End If
      ps.Title = FIDFileName
      ps.HugeSize = 2 ^ Int(Log(CSng(ps.HugeSize)) / Log(2) + 0.9)
      ps.HugeSizeInitial = ps.HugeSize
      iStat = HugeDim(ps.harray, 4, ps.HugeSize)
      If (iStat <> 0) Then
         MsgBox ("ERROR! can't allocate memory.")
         LoadData = -1
         Exit Function
      End If
      FileHandle = lopen(FIDFileName, 0&)
      If ps.ByteOrder = 0 Then
         'iStat = HugeLoadLong(ps.harray, ps.HugeSize, FileHandle, FilePointer, 0)
         iStat = HugeLoadLongBig(ps.harray, ps.HugeSize, FileHandle, ffp, 0)
      Else
         'iStat = HugeLoadLong(ps.harray, ps.HugeSize, FileHandle, FilePointer, 1)
         iStat = HugeLoadLongBig(ps.harray, ps.HugeSize, FileHandle, ffp, 1)
      End If
      lclose (FileHandle)
   End If
'   Beep
   Exit Function
LoadDataError:
   Exit Function
End Function

Function LoadDefaults() As Integer
   Dim FileNum As Integer
   Dim i As Integer
   Dim str As String
   
   LoadDefaults = True
   On Error GoTo LoadDefaultsError
'   ChDir SysDirs.StartupDir
'   ChDrive SysDirs.StartupDir
   ChDir HomeDir
   ChDrive HomeDir
   FileNum = FreeFile
   Open "Default.cfg" For Binary Access Read As FileNum
   If EOF(FileNum) = True Then GoTo LoadDefaultsError
   Get FileNum, , AxisSetup
   If EOF(FileNum) = True Then GoTo LoadDefaultsError
   For i = 0 To NUMMACROS - 1
      Get FileNum, , Macs(i)
      If EOF(FileNum) = True Then GoTo LoadDefaultsError
   Next i
   Get #FileNum, , ExciteData(0)
   ExciteGen.Board = 0
   Load ExciteGen
   Get #FileNum, , ExciteData(1)
   EG2.Board = 1
   Load EG2
   If EOF(FileNum) = True Then GoTo LoadDefaultsError
   Get #FileNum, , SysDirs
   If EOF(FileNum) = True Then GoTo LoadDefaultsError
   For i = 0 To 3
      Get FileNum, , H(i)
      If EOF(FileNum) = True Then GoTo LoadDefaultsError
   Next i
   Get #FileNum, , i
   str = String(i, " ")
   Get #FileNum, , str
   GenCal.txtStartMZ = str
   Get #FileNum, , i
   str = String(i, " ")
   Get #FileNum, , str
   GenCal.txtStopMZ = str
   Get #FileNum, , i
   str = String(i, " ")
   Get #FileNum, , str
   GenCal.txtThreshold = str
   Get #FileNum, , i
   str = String(i, " ")
   Get #FileNum, , str
   GenCal.optTIC = str
   Get #FileNum, , i
   str = String(i, " ")
   Get #FileNum, , str
   GenCal.optPeaks = str
   Close FileNum
   Exit Function
LoadDefaultsError:
   Close FileNum
   LoadDefaults = False
   Exit Function
End Function

Sub LoadFile(PI As Integer, FileName As String, Num As Integer, Lscope As Integer, Optional NumPoints As Long = 0)
   Dim FilePointer As Long
   Dim fp As Long
   Dim FileNum As Integer
   Dim iStat As Long
   Dim Fname As String, FIDsize As Long
   Dim ffp As Double

   On Error Resume Next
   Fname = FileName
   Call SetScopeDefaults(Plots(PI))
   Plots(PI).Title = Fname
   If (ConnectParms.tbxFileName <> "") Then Plots(PI).Title = ConnectParms.tbxFileName
   Plots(PI).FileName = Fname
   If Plots(PI).FileType = 1 Then       ' binary
      FilePointer = ReadHeader(Plots(PI))
      FilePointer = FilePointer + Num * Plots(PI).HugeSize * 2
      If Plots(PI).iStat = -1 Then Exit Sub
      Plots(PI).A = A
      Plots(PI).b = b
      Plots(PI).c = c
      iStat = LoadData(Plots(PI), FilePointer)
      If iStat < 0 Then Exit Sub
      iStat = ExtractPlotData(Plots(PI), Xvalue(Plots(PI), 0&), Xvalue(Plots(PI), Plots(PI).HugeSize))
      Call SaveAsLast(Plots(PI))
   End If
   If Plots(PI).FileType = 3 Then    ' Floats, with header
      Plots(PI).Xmin = 0#
      Plots(PI).Xmax = 0#
      FilePointer = ReadHeader(Plots(PI))
      fp = FilePointer
      FilePointer = FilePointer + Num * Plots(PI).HugeSize * 4
      If NumPoints <> 0 And Plots(PI).Type = 1 Then
         Plots(PI).HugeSize = NumPoints
         Plots(PI).HugeSizeInitial = NumPoints
      End If
      If Plots(PI).iStat = -1 Then Exit Sub
      iStat = LoadData(Plots(PI), FilePointer)
      If iStat < 0 Then Exit Sub
      If (Plots(PI).Xmax = 0 And Plots(PI).Xmin = 0) Or (Plots(PI).Xmax = Plots(PI).Xmin) Then
         iStat = ExtractPlotData(Plots(PI), Xvalue(Plots(PI), 0&), Xvalue(Plots(PI), Plots(PI).HugeSize))
      Else
         iStat = ExtractPlotData(Plots(PI), Plots(PI).Xmin, Plots(PI).Xmax)
      End If
      Call SaveAsLast(Plots(PI))
   End If
   If Plots(PI).FileType = 2 Then    ' Sun Extrel format
      FilePointer = ReadHeader(Plots(PI))
      FilePointer = FilePointer + Num * Plots(PI).HugeSize * 4
      If NumPoints <> 0 And Plots(PI).Type = 1 Then
         Plots(PI).HugeSize = NumPoints
         Plots(PI).HugeSizeInitial = NumPoints
      End If
      If Plots(PI).iStat = -1 Then Exit Sub
      iStat = LoadData(Plots(PI), FilePointer)
      If iStat < 0 Then Exit Sub
      iStat = ExtractPlotData(Plots(PI), Xvalue(Plots(PI), 0&), Xvalue(Plots(PI), Plots(PI).HugeSize))
      Call SaveAsLast(Plots(PI))
   End If
   If Plots(PI).FileType = 5 Then   ' ASCII file format
      Plots(PI).FilePointer = 1
      FilePointer = ReadHeader(Plots(PI))
      FilePointer = FilePointer + Num * Plots(PI).HugeSize * 2
      If Plots(PI).iStat = -1 Then Exit Sub
      Plots(PI).A = A
      Plots(PI).b = b
      Plots(PI).c = c
      iStat = LoadData(Plots(PI), FilePointer)
      If iStat < 0 Then Exit Sub
      iStat = ExtractPlotData(Plots(PI), Xvalue(Plots(PI), 0&), Xvalue(Plots(PI), Plots(PI).HugeSize))
      Call SaveAsLast(Plots(PI))
   End If
   If Plots(PI).FileType = 6 Then ' MIDAS file type
      FilePointer = ReadHeader(Plots(PI))
      FilePointer = FilePointer + Num * Plots(PI).HugeSize * 2
      If NumPoints <> 0 And Plots(PI).Type = 1 Then
         Plots(PI).HugeSize = NumPoints
         Plots(PI).HugeSizeInitial = NumPoints
      End If
      If Plots(PI).iStat = -1 Then Exit Sub
      iStat = LoadData(Plots(PI), FilePointer)
      If iStat < 0 Then Exit Sub
      iStat = ExtractPlotData(Plots(PI), Xvalue(Plots(PI), 0&), Xvalue(Plots(PI), Plots(PI).HugeSize))
      Call SaveAsLast(Plots(PI))
   End If
   If Plots(PI).FileType = 7 Then ' IonSpec omega 3.3
      FilePointer = ReadHeader(Plots(PI))
      FilePointer = FilePointer + Num * Plots(PI).HugeSize * 2
      If NumPoints <> 0 And Plots(PI).Type = 1 Then
         Plots(PI).HugeSize = NumPoints
         Plots(PI).HugeSizeInitial = NumPoints
      End If
      If Plots(PI).iStat = -1 Then Exit Sub
      iStat = LoadData(Plots(PI), FilePointer)
      If iStat < 0 Then Exit Sub
      iStat = ExtractPlotData(Plots(PI), Xvalue(Plots(PI), 0&), Xvalue(Plots(PI), Plots(PI).HugeSize))
      Call SaveAsLast(Plots(PI))
   End If
   If Plots(PI).FileType = ftBRUKER Then  ' Bruker data file
      ffp = ReadHeader(Plots(PI))
      ffp = ffp + CDbl(Num) * CDbl(Plots(PI).HugeSize * 4)
      FIDsize = Plots(PI).HugeSize
      If NumPoints <> 0 And Plots(PI).Type = 1 Then
         Plots(PI).HugeSize = NumPoints
         Plots(PI).HugeSizeInitial = NumPoints
      End If
      If Plots(PI).iStat = -1 Then Exit Sub
      iStat = LoadData(Plots(PI), 0, ffp)
      ' Plots(PI).HugeSizeInitial = NumPoints
      ' Title has the fid file name, so lets get the number of scans in this fid...
      ' Plots(PI).NumSegments = FileLen(Plots(PI).title) / (FIDsize * 4)
      Plots(PI).NumSegments = HugeFileSize(Plots(PI).Title) / (FIDsize * 4)
      ' Plots(PI).NumSegments = (2 ^ 32 + FileLen(Plots(PI).title)) / (FIDsize * 4)
      If Plots(PI).NumSegments = 1 Then Plots(PI).NumSegments = 0
      If iStat < 0 Then Exit Sub
      iStat = ExtractPlotData(Plots(PI), Xvalue(Plots(PI), 0&), Xvalue(Plots(PI), Plots(PI).HugeSize))
      Call SaveAsLast(Plots(PI))
   End If
   If Lscope Then
      Call AllocateScope(PI)
      Call Scopes(PI).PaintScope
      If Plots(PI).FileType = 3 Then Call LoadTags(PI)
   End If
   If Plots(PI).FileType = 3 Then    ' Floats, with header
   End If
   If Plots(PI).NumSegments > 1 Then
      Scopes(PI).Caption = "Scope " & Format(PI + 1, "#") & ", " & Format(Num + 1, "#") & " of " & Format(Plots(PI).NumSegments, "#")
   End If
End Sub

Sub MacroPlay(ByVal k As Integer, key As Integer)
   Dim i As Integer, Pos As Integer
   Dim j As Integer, l As Integer
   Dim iStat As Long
   Dim Result As Double
   Dim FileNum As Integer
   
   PeakListPrinted = False
   On Error Resume Next
   If (k <> -1) Then
      Scopes(k).SetFocus
      Set ActiveForm = Scopes(k)
      ActivePlot = k
   End If
   For i = 0 To NUMMACROS
      If (Macs(i).key = key) Then
         For j = 0 To Macs(i).NumCommands - 1
            DoEvents
            If ("ExtractPlotData" = Mid$(Macs(i).Commands(j), 1, 15)) Then
               iStat = ExtractPlotData(Plots(k), CDbl(ParseString(Macs(i).Commands(j), 1)), CDbl(ParseString(Macs(i).Commands(j), 2)))
               ActiveForm.Refresh
            ElseIf (Macs(i).Commands(j) = "CopyPlot") Then
               CopyPlot = Plots(k)
            ElseIf (Macs(i).Commands(j) = "Convert2Freq") Then
               iStat = Convert2Freq(Plots(k))
               ActiveForm.Refresh
            ElseIf (Macs(i).Commands(j) = "Convert2Mag") Then
               iStat = Convert2Mag(Plots(k))
               ActiveForm.Refresh
            ElseIf (Macs(i).Commands(j) = "Convert2Mass") Then
               iStat = Convert2Mass(Plots(k))
               ActiveForm.Refresh
            ElseIf (Macs(i).Commands(j) = "Convert2Time") Then
               iStat = Convert2Time(Plots(k))
               ActiveForm.Refresh
            ElseIf ("DIOsetBit" = Mid$(Macs(i).Commands(j), 1, 9)) Then
               Call ICR_2LS.DIObit(CInt(ParseString(Macs(i).Commands(j), 1)), True)
            ElseIf ("DIOresetBit" = Mid$(Macs(i).Commands(j), 1, 11)) Then
               Call ICR_2LS.DIObit(CInt(ParseString(Macs(i).Commands(j), 1)), False)
            ElseIf ("ExciteDownload" = Mid$(Macs(i).Commands(j), 1, 14)) Then
               If (CInt(ParseString(Macs(i).Commands(j), 1)) = 0) Then
                  Call ExciteGen.Download_Click
               Else
                  Call EG2.Download_Click
               End If
            ElseIf ("ExciteClear" = Mid$(Macs(i).Commands(j), 1, 11)) Then
               If (CInt(ParseString(Macs(i).Commands(j), 1)) = 0) Then
                  ExciteData(0).chan(CInt(ParseString(Macs(i).Commands(j), 2))).NumDefs = 0
               Else
                  ExciteData(1).chan(CInt(ParseString(Macs(i).Commands(j), 2))).NumDefs = 0
               End If
            ElseIf ("ExciteSet" = Mid$(Macs(i).Commands(j), 1, 9)) Then
               If (CInt(ParseString(Macs(i).Commands(j), 1)) = 0) Then
                  ExciteData(0).chan(CInt(ParseString(Macs(i).Commands(j), 2))).NumDefs = CInt(ParseString(Macs(i).Commands(j), 3))
               Else
                  ExciteData(1).chan(CInt(ParseString(Macs(i).Commands(j), 2))).NumDefs = CInt(ParseString(Macs(i).Commands(j), 3))
               End If
            ElseIf ("ExcitePaste" = Mid$(Macs(i).Commands(j), 1, 11)) Then
               If (CInt(ParseString(Macs(i).Commands(j), 1)) = 0) Then
                  Call ExciteGen.PasteParms(CInt(ParseString(Macs(i).Commands(j), 2)))
               Else
                  Call EG2.PasteParms(CInt(ParseString(Macs(i).Commands(j), 2)))
               End If
            ElseIf ("Load" = Mid$(Macs(i).Commands(j), 1, 4)) Then
               k = NextScope()
               If (k = -1) Then
                  Exit Sub
               End If
               Plots(k).FileType = FindFileType(ParseString(Macs(i).Commands(j), 1))
               If (Plots(k).FileType <> -1) Then Call LoadFile(k, ParseString(Macs(i).Commands(j), 1), 0, True) Else Exit Sub
               Scopes(k).SetFocus
               Set ActiveForm = Scopes(k)
               ActivePlot = k
            ElseIf ("Save" = ParseString(Macs(i).Commands(j), 0)) Then
               export.SaveFile = Plots(ActivePlot).FileName
               Call export.Accept_Click
               ActiveForm.Refresh
            ElseIf ("SaveEx" = ParseString(Macs(i).Commands(j), 0)) Then
               Plots(ActivePlot).FileName = ParseString(Macs(i).Commands(j), 1) & "." & Format(NIacquire.LogCount, "00000")
               export.SaveFile = Plots(ActivePlot).FileName
               Call export.Accept_Click
               ActiveForm.Refresh
            ElseIf ("#" = Mid$(Macs(i).Commands(j), 1, 1)) Then
               If ParseString(Macs(i).Commands(j), 1) <> "" Then
                  Call SpectraMath(CInt(ParseString(Macs(i).Commands(j), 1)), Asc("#"))
                  Scopes(CInt(ParseString(Macs(i).Commands(j), 1))).Refresh
               Else
                  Call SpectraMath(k, Asc("#"))
                  Scopes(k).Refresh
               End If
            ElseIf ("+" = Mid$(Macs(i).Commands(j), 1, 1)) Then
               If ParseString(Macs(i).Commands(j), 1) <> "" Then
                  Call SpectraMath(CInt(ParseString(Macs(i).Commands(j), 1)), Asc("+"))
                  Scopes(CInt(ParseString(Macs(i).Commands(j), 1))).Refresh
               Else
                  Call SpectraMath(k, Asc("+"))
                  Scopes(k).Refresh
               End If
            ElseIf ("-" = Mid$(Macs(i).Commands(j), 1, 1)) Then
               If ParseString(Macs(i).Commands(j), 1) <> "" Then
                  Call SpectraMath(CInt(ParseString(Macs(i).Commands(j), 1)), Asc("-"))
                  Scopes(CInt(ParseString(Macs(i).Commands(j), 1))).Refresh
               Else
                  Call SpectraMath(k, Asc("-"))
                  Scopes(k).Refresh
               End If
            ElseIf ("*" = Mid$(Macs(i).Commands(j), 1, 1)) Then
               If ParseString(Macs(i).Commands(j), 1) <> "" Then
                  Call SpectraMath(CInt(ParseString(Macs(i).Commands(j), 1)), Asc("*"))
                  Scopes(CInt(ParseString(Macs(i).Commands(j), 1))).Refresh
               Else
                  Call SpectraMath(k, Asc("*"))
                  Scopes(k).Refresh
               End If
            ElseIf ("/" = Mid$(Macs(i).Commands(j), 1, 1)) Then
               If ParseString(Macs(i).Commands(j), 1) <> "" Then
                  Call SpectraMath(CInt(ParseString(Macs(i).Commands(j), 1)), Asc("/"))
                  Scopes(CInt(ParseString(Macs(i).Commands(j), 1))).Refresh
               Else
                  Call SpectraMath(k, Asc("/"))
                  Scopes(k).Refresh
               End If
            ElseIf ("FilterFunction" = Mid$(Macs(i).Commands(j), 1, 14)) Then
               Call FilterFunction(k, CSng(ParseString(Macs(i).Commands(j), 1)), CSng(ParseString(Macs(i).Commands(j), 2)))
               ActiveForm.Refresh
            ElseIf ("WindowFunction" = Mid$(Macs(i).Commands(j), 1, 14)) Then
               Call WindowFunction(k, CSng(ParseString(Macs(i).Commands(j), 1)), CSng(ParseString(Macs(i).Commands(j), 2)))
               ActiveForm.Refresh
            ElseIf (Macs(i).Commands(j) = "TimeZero") Then
               Call TimeZero(k)
               ActiveForm.Refresh
            ElseIf (Macs(i).Commands(j) = "Derivative") Then
               Call Derivative(k)
               ActiveForm.Refresh
            ElseIf (Macs(i).Commands(j) = "Integral") Then
               Call Integral(k)
               ActiveForm.Refresh
            ElseIf ("Appodization" = Mid$(Macs(i).Commands(j), 1, 12)) Then
               Call Appodization(Plots(k), CInt(ParseString(Macs(i).Commands(j), 1)), CDbl(ParseString(Macs(i).Commands(j), 2)), CDbl(ParseString(Macs(i).Commands(j), 3)), False, CDbl(ParseString(Macs(i).Commands(j), 4)))
               ActiveForm.Refresh
            ElseIf (Macs(i).Commands(j) = "ZoomTransform") Then
               k = ZoomTransform(Plots(k))
            ElseIf (Macs(i).Commands(j) = "BaseLineRestore") Then
               Call BaseLineRestore(k)
            ElseIf (Macs(i).Commands(j) = "BuildNewScope") Then
               k = BuildNewScope()
            ElseIf ("ZeroFills" = Mid$(Macs(i).Commands(j), 1, 9)) Then
               iStat = ZeroFills(Plots(k), CInt(ParseString(Macs(i).Commands(j), 1)))
            ElseIf ("MassTransform" = Mid$(Macs(i).Commands(j), 1, 13)) Then
               Call HornMassTransform(k, CDbl(ParseString(Macs(i).Commands(j), 1)), CDbl(ParseString(Macs(i).Commands(j), 2)))
            ElseIf ("SaveComments" = Mid$(Macs(i).Commands(j), 1, 12)) Then
               ' open file for append and write comments to file...
               FileNum = FreeFile
               Pos = InStr(Macs(i).Commands(j), ",")
               If (Pos > 0) Then
                  Open Mid$(Macs(i).Commands(j), Pos + 1) For Append Access Write As FileNum
                     Print #FileNum, Plots(k).Comment
                  Close FileNum
               End If
            ElseIf ("ClearScopeData" = Mid$(Macs(i).Commands(j), 1, 14)) Then
               Call ClearScopeData(Plots(k), CDbl(ParseString(Macs(i).Commands(j), 1)), CDbl(ParseString(Macs(i).Commands(j), 2)), CSng(ParseString(Macs(i).Commands(j), 3)), CInt(ParseString(Macs(i).Commands(j), 4)), CInt(ParseString(Macs(i).Commands(j), 5)))
               ActiveForm.Refresh
            ElseIf (Macs(i).Commands(j) = "PrintScope") Then
               Call ActiveForm.PPrint(0#, 0#, CSng(Printer.ScaleWidth), CSng(Printer.ScaleHeight), False)
               Printer.EndDoc
            ElseIf (Mid$(Macs(i).Commands(j), 1, 12) = "ProcessPeaks") Then
               If ParseString(Macs(i).Commands(j), 9) <> "" Then
                  Result = ProcessPeaks(Plots(k), CDbl(ParseString(Macs(i).Commands(j), 1)), CDbl(ParseString(Macs(i).Commands(j), 2)), CInt(ParseString(Macs(i).Commands(j), 3)), ParseString(Macs(i).Commands(j), 4), CSng(ParseString(Macs(i).Commands(j), 5)), CInt(ParseString(Macs(i).Commands(j), 6)), CInt(ParseString(Macs(i).Commands(j), 7)), CInt(ParseString(Macs(i).Commands(j), 8)), CSng(ParseString(Macs(i).Commands(j), 9)))
               Else
                  Result = ProcessPeaks(Plots(k), CDbl(ParseString(Macs(i).Commands(j), 1)), CDbl(ParseString(Macs(i).Commands(j), 2)), CInt(ParseString(Macs(i).Commands(j), 3)), ParseString(Macs(i).Commands(j), 4), CSng(ParseString(Macs(i).Commands(j), 5)), CInt(ParseString(Macs(i).Commands(j), 6)), CInt(ParseString(Macs(i).Commands(j), 7)), CInt(ParseString(Macs(i).Commands(j), 8)))
               End If
            ElseIf (Mid$(Macs(i).Commands(j), 1, 15) = "BoxCarIntegrate") Then
               Call BoxCarIntegrate(Plots(k), CDbl(ParseString(Macs(i).Commands(j), 1)), CDbl(ParseString(Macs(i).Commands(j), 2)), CLng(ParseString(Macs(i).Commands(j), 3)))
            ElseIf (Macs(i).Commands(j) = "UnLoad") Then
               Unload ActiveForm
            ElseIf (Mid$(Macs(i).Commands(j), 1, 10) = "ScaleArray") Then
               Call ScaleArray(k, CSng(ParseString(Macs(i).Commands(j), 1)), CSng(ParseString(Macs(i).Commands(j), 2)), CSng(ParseString(Macs(i).Commands(j), 3)), CSng(ParseString(Macs(i).Commands(j), 4)), CSng(ParseString(Macs(i).Commands(j), 5)), CSng(ParseString(Macs(i).Commands(j), 6)), CSng(ParseString(Macs(i).Commands(j), 7)))
            ElseIf (Mid$(Macs(i).Commands(j), 1, 13) = "SavitzkyGolay") Then
               Call SavitzkyGolayFilter(Plots(k), CSng(ParseString(Macs(i).Commands(j), 1)), CSng(ParseString(Macs(i).Commands(j), 2)), CLng(ParseString(Macs(i).Commands(j), 3)), CLng(ParseString(Macs(i).Commands(j), 4)), CLng(ParseString(Macs(i).Commands(j), 5)))
            End If
         Next j
      End If
   Next i
End Sub

Sub MacroRecord(cmd As String)
   Dim i As Integer
   
   If Not MacroEnabled Then Exit Sub
   For i = 0 To NUMMACROS - 1
      If (Macs(i).Recording) Then
         If (Macs(i).NumCommands < MAXCOMMANDS) Then
            If (Macs(i).NumCommands > 0) Then
               If (InStr(cmd, "ExtractPlotData") <> 0) Then
                  If (InStr(Macs(i).Commands(Macs(i).NumCommands - 1), "ExtractPlotData") <> 0) Then
                     Macs(i).NumCommands = Macs(i).NumCommands - 1
                  End If
               End If
            End If
            Macs(i).Commands(Macs(i).NumCommands) = cmd
            Macs(i).NumCommands = Macs(i).NumCommands + 1
         End If
      End If
   Next i
End Sub

Sub MakeWaveFile(ps As PlotStructure, FileName As String)
   Dim FileNum
   Dim FileHandle As Long
   Dim Max As Single
   Dim Lstart As Long
   Dim Lstop As Long
   Dim temp As Long
   Dim iStat As Long
      
   On Error GoTo MakeWaveFileError
   If ps.Type <> 1 Then Exit Sub
   FileNum = FreeFile
   Open FileName For Binary Access Write As FileNum
   FileHandle = lopen(FileName, OF_READWRITE)
   Max = ps.Ymax
   If Abs(ps.Ymin) > Max Then Max = Abs(ps.Ymin)
   Lstart = FindIndex(ps, ps.Xmin)
   Lstop = FindIndex(ps, ps.Xmax)
   If Lstart > Lstop Then
      temp = Lstart
      Lstart = Lstop
      Lstop = temp
   End If
   iStat = GenerateWaveFile(ps.harray, Lstart, Lstop, Max, FileHandle)
   Close FileNum
   lclose (FileHandle)
   Exit Sub
MakeWaveFileError:
   Exit Sub
End Sub

Sub MarkPeaks(ps As PlotStructure)
   Dim i As Integer, ds As Long
   Dim Psum As Double, Pnum As Double
   Dim Ans As String * 10, fmt As String
   
   Psum = 0#
   Pnum = 0#
   ds = GetPrivateProfileInt("Scope", "PeakDigits", 2&, "icr-2ls.ini")
   fmt = "." & String(ds, "#")
   For i = 0 To pk.NumPeaks - 1
      If (pk.Xloc(i) >= ps.Xmin) And (pk.Xloc(i) <= ps.Xmax) Then
         ActiveForm.PSet (X2cursor(CDbl(pk.Xloc(i)), ps), Y2cursor(CDbl(pk.Peak(i)), ps) - 100)
         ActiveForm.Line -(X2cursor(CDbl(pk.Xloc(i)), ps), Y2cursor(CDbl(pk.Peak(i)), ps) + 100)
         ActiveForm.PSet (X2cursor(CDbl(pk.Xloc(i)), ps) - 100, Y2cursor(CDbl(pk.Peak(i)), ps))
         ActiveForm.Line -(X2cursor(CDbl(pk.Xloc(i)), ps) + 100, Y2cursor(CDbl(pk.Peak(i)), ps))
         ActiveForm.CurrentX = X2cursor(CDbl(pk.Xloc(i)), ps) + 100
         ActiveForm.CurrentY = Y2cursor(CDbl(pk.Peak(i)), ps)
         ActiveForm.Print Format(pk.Xloc(i), fmt)
      End If
      ' Now calculate the average MW from the peak list and write the
      ' result to the scope
      Pnum = Pnum + pk.Peak(i)
      Psum = Psum + pk.Xloc(i) * pk.Peak(i)
   Next i
   Call GetPrivateProfileString("Scope", "NoAverageMZ", "N", Ans, 10, "icr-2ls.ini")
   If Mid(Trim(Ans), 1, 1) <> "Y" Then
      ActiveForm.CurrentX = ps.OriginX + 100
      ActiveForm.CurrentY = ps.MaxY
      ActiveForm.Print "Ave m/z = "; Psum / Pnum
   End If
   If (Pnum > 0) Then Averagine.tbxMW = Psum / Pnum
End Sub
Function NewFile(ps As PlotStructure, FileName As String, Optional NumPoints As Long = 0) As Integer
   Dim FilePointer As Long
   Dim iStat As Long

   psold = ps                    ' save current plot data
   ps.FileName = FileName
   If ps.harray <> 0 Then iStat = HugeErase(ps.harray)  ' free present array
   FilePointer = ReadHeader(ps)
   If NumPoints <> 0 And ps.Type = 1 Then
      ps.HugeSize = NumPoints
      ps.HugeSizeInitial = NumPoints
   End If
   iStat = LoadData(ps, FilePointer)
   ps.Title = FileName
   If iStat < 0 Then
      ps.Title = FileName & " Error loading file!"
      NewFile = iStat
      Exit Function
   End If
   NewFile = 0
End Function

Function NextScope() As Integer
   Dim i As Integer
   
   For i = 0 To MAXSCOPES - 1
      If Plots(i).InUse = False Then
         NextScope = i
         Exit Function
      End If
   Next i
   NextScope = -1
End Function
' This function expects a string that contains several cr lf terminated
' string packed in one string.
Function UnpackString(pstring As String, Num As Integer) As String
Dim i As Long, j As Long, k As Long
Dim Pos As Long

   Pos = 1
   For i = 2 To Num
      If (InStr(Pos, pstring, Chr(10)) <> 0) Or (InStr(Pos, pstring, Chr(13)) <> 0) Then
         j = InStr(Pos, pstring, Chr(10))
         If (j = 0) Then j = InStr(Pos, pstring, Chr(13))
         j = j + 1
         If Mid(pstring, j) = Chr(10) Then j = j + 1
         If Mid(pstring, j) = Chr(13) Then j = j + 1
         Pos = j
      Else
         UnpackString = ""
         Exit Function
      End If
   Next
   ' here when we are pointing to the string we want
   j = InStr(Pos, pstring, Chr(10))
   k = InStr(Pos, pstring, Chr(13))
   If (j <> 0) And (k < j) Then j = k
   If (j = 0) Then j = k
   If (j = 0) Then
      UnpackString = Mid(pstring, Pos)
   Else
      UnpackString = Mid(pstring, Pos, j - Pos)
   End If
End Function
Function GetTabToken(InString As String, Num As Integer) As String
Dim str As String, c As String, LastC As String
Dim i As Long, Cnum As Integer
Dim token As String, EOT As Boolean

   token = ""
   Cnum = 0
   GetTabToken = ""
   str = InString
   For i = 1 To Len(str)
      c = Mid(str, i, 1)
      If c = Chr(9) Then
         EOT = False
         If c = " " Then
            If LastC <> Chr(9) Then EOT = True
         End If
         If c = Chr(9) And LastC <> " " Then EOT = True
         If EOT = True Then
            If Num = Cnum Then Exit For
            Cnum = Cnum + 1
         End If
         token = ""
      Else
         token = token & c
      End If
      LastC = c
   Next
   If Cnum = Num Then GetTabToken = token
End Function
Function GetToken(InString As String, Num As Integer) As String
Dim str As String, c As String, LastC As String
Dim i As Long, Cnum As Integer
Dim token As String, EOT As Boolean

   ' First trim off spaces
   str = Trim(InString)
   ' Now look for the first character that is not a
   ' Space, comma, tab
   token = ""
   Cnum = 0
   GetToken = ""
   For i = 1 To Len(str)
      c = Mid(str, i, 1)
      If c = " " Or c = "," Or c = Chr(9) Then
         EOT = False
         If c = " " Then
            If LastC <> " " And LastC <> "," And LastC <> Chr(9) Then EOT = True
         End If
         If c = "," And LastC <> " " Then EOT = True
         If c = Chr(9) And LastC <> " " Then EOT = True
         If EOT = True Then
            If Num = Cnum Then Exit For
            Cnum = Cnum + 1
         End If
         token = ""
      Else
         token = token & c
      End If
      LastC = c
   Next
   If Cnum = Num Then GetToken = token
End Function
' This function pulls tokens from a string. These tokens are comma or
' space delimited. The string is expected not to have a cr or lf
Function ParseString(InString As String, Num As Integer, Optional Delimiter As Integer = 44) As String
   Dim Start As Integer
   Dim i As Long
   Dim j As Long

   On Error GoTo ParseStringError
   j = 0
   Start = 1
   For i = 1 To Len(InString)
      If ((Mid$(InString, i, 1) = Chr(Delimiter)) Or (Mid$(InString, i, 1) = " ")) Then ' And (i > Start) Then
         If j = Num Then
            If Start > Len(InString) Then
               ParseString = ""
               Exit Function
            End If
            ParseString = Mid$(InString, Start, i - Start)
            If Mid(ParseString, 1, 1) = "," Then ParseString = ""
            Exit Function
         End If
         j = j + 1
         Start = i + 1
         While (Mid$(InString, Start, 1) = " ")
            Start = Start + 1
         Wend
      End If
   Next i
   If (Num > j) Then
      ParseString = ""
   Else
      ParseString = Mid$(InString, Start, i - Start)
   End If
   Exit Function
ParseStringError:
   ParseString = ""
   Exit Function
End Function

Function PasteParms(ps As PlotStructure) As Integer
   Dim iStat As Integer
   
   If CopyPlot.InUse = 0 Then Exit Function
   If ps.Type = 1 Then
      ' Here is input data is time domain, do appodization
      If (CopyPlot.appodize > 0) Then Call Appodization(ps, CopyPlot.appodize, Xvalue(ps, 0), Xvalue(ps, ps.HugeSize), False)
      ' and zero filling
      If (CopyPlot.ZeroFills > 0) Then iStat = ZeroFills(ps, CopyPlot.ZeroFills)
   End If
   If CopyPlot.Type = 1 Then iStat = Convert2Time(ps)
   If CopyPlot.Type = 2 Then iStat = Convert2Freq(ps)
   If CopyPlot.Type = 3 Then iStat = Convert2Mass(ps)
   ps.Xmin = CopyPlot.Xmin
   ps.Xmax = CopyPlot.Xmax
   iStat = ExtractPlotData(ps, ps.Xmin, ps.Xmax)
   PasteParms = iStat
End Function

Function PictureFontHeight(S As Control, Tag As String, fntyp As FN) As Integer
   Call SetPictureFont(S, fntyp)
   PictureFontHeight = S.TextHeight(Tag)
End Function

Function PictureFontWidth(S As Control, Tag As String, fntyp As FN) As Integer
   Call SetPictureFont(S, fntyp)
   PictureFontWidth = S.TextWidth(Tag)
End Function
Sub PlotDataFill(hdc As Long, ps As PlotStructure, pSd As PlotStructure, color As Integer, TwipsPerPixelX As Integer, TwipsPerPixelY As Integer, Optional MaxPoint As Integer = 0)
   Dim X As Long, Y As Long, lStat As Long, Ym As Long, LastX As Long
   Dim i As Long, iStat As Integer, hndPen As Long, hndold As Long
   Dim li As Long, lj As Long, fVal As Single, St As Long
   Dim Ans As String, Ymax As Single
   Dim Lstart As Long, Lstop As Long

   hndPen = CreatePen(PS_SOLID, 1, QBColor(color))
   hndold = SelectObject(hdc, hndPen)
   Lstart = FindIndex(ps, ps.Xmax)
   Lstop = FindIndex(ps, ps.Xmin)
   If Lstop < Lstart Then
      li = Lstart
      Lstart = Lstop
      Lstop = li
   End If
   St = (Lstop - Lstart) / 10000
   If St < 1 Then St = 1
   For li = Lstart To Lstop Step St
      lj = li
      If ((ps.Type = 3) Or (ps.Type = 2)) Then
         If St > 1 Then
            lj = li
            Ymax = Yvalue(ps, li)
            For i = li To li + St - 1
               If Yvalue(ps, i) >= Ymax Then
                  lj = i
                  Ymax = Yvalue(ps, i)
               End If
            Next
         End If
      End If
      Y = (CDbl(Yvalue(ps, lj) - ps.Ymin) / CDbl(ps.Ymax - ps.Ymin)) * CDbl(ps.MaxY - ps.OriginY) + CDbl(ps.OriginY)
      X = (CDbl(Xvalue(ps, lj) - CDbl(ps.Xmin)) / CDbl(ps.Xmax - ps.Xmin)) * CDbl(ps.MaxX - ps.OriginX) + CDbl(ps.OriginX)
      If Y < ps.MaxY Then Y = ps.MaxY
      If Y > ps.OriginY Then Y = ps.OriginY
      If X < ps.OriginX Then X = ps.OriginX
      If X > ps.MaxX Then X = ps.MaxX
      If (lj = Lstart) Then
         lStat = MoveToEx(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY, 0&)
      Else
         iStat = LineTo(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY)
      End If
      LastX = X
   Next
   Call SelectObject(hdc, hndold)
   Call DeleteObject(hndPen)
   Exit Sub
End Sub

' plot structure ps is used to define the scope size and limits
' plot structure psd contains the data to be ploted
Sub PlotData(hdc As Long, ps As PlotStructure, pSd As PlotStructure, color As Integer, TwipsPerPixelX As Integer, TwipsPerPixelY As Integer, Optional MaxPoint As Integer = 0)
   Dim X As Long, Y As Long, lStat As Long, Ym As Long, LastX As Long
   Dim i As Integer, iStat As Integer, hndPen As Long, hndold As Long
   Dim li As Long
   Dim Ans As String

   On Error GoTo PlotDataError
   If FillMode Then
      Call PlotDataFill(hdc, ps, pSd, color, TwipsPerPixelX, TwipsPerPixelY, MaxPoint)
      Exit Sub
   End If
   hndPen = CreatePen(PS_SOLID, 1, QBColor(color))
   hndold = SelectObject(hdc, hndPen)
   li = pSd.Lstart
   For i = 0 To pSd.n - 1
      If (MaxPoint > 0) Then
         If (i >= MaxPoint) Then Exit For
      End If
      Y = (CDbl(pSd.Y(i) - ps.Ymin) / CDbl(ps.Ymax - ps.Ymin)) * CDbl(ps.MaxY - ps.OriginY) + CDbl(ps.OriginY)
      X = (CDbl(Xvalue(pSd, li + CLng(i) * pSd.Skip) - CDbl(ps.Xmin)) / CDbl(ps.Xmax - ps.Xmin)) * CDbl(ps.MaxX - ps.OriginX) + CDbl(ps.OriginX)
      If Y < ps.MaxY Then Y = ps.MaxY
      If Y > ps.OriginY Then Y = ps.OriginY
      If X < ps.OriginX Then X = ps.OriginX
      If X > ps.MaxX Then X = ps.MaxX
      If (i = 0) Then
         lStat = MoveToEx(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY, 0&)
      Else
         iStat = LineTo(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY)
      End If
      If FillMode Then
         If ((ps.Type = 3) Or (ps.Type = 2)) And Abs((LastX - X) / TwipsPerPixelX) <= 1 Then
            If ps.Y(i) <> ps.Ym(i) Then
               Ym = (CDbl(pSd.Ym(i) - ps.Ymin) / CDbl(ps.Ymax - ps.Ymin)) * CDbl(ps.MaxY - ps.OriginY) + CDbl(ps.OriginY)
               lStat = MoveToEx(hdc, X / TwipsPerPixelX, Ym / TwipsPerPixelY, 0&)
               iStat = LineTo(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY)
            End If
         End If
      End If
      LastX = X
   Next i
   Call SelectObject(hdc, hndold)
   Call DeleteObject(hndPen)
   Exit Sub
PlotDataError:
'   Call ErrorMess.LogError(Error$ & " in PlotData!", "")
   Exit Sub
End Sub
Sub PlotPictureAxis(S As Control, ps As PlotStructure, color As Integer)
   Dim Tag As String
   Dim TagWidth As Integer, Max As Integer
   Dim absmax As Single
   Dim i As Integer
   Dim OldColor As Long, Y As Long
   
   OldColor = S.ForeColor
   S.ForeColor = QBColor(color)
   ps.ZoomBox = False
   S.Cls
   S.Scale
   ' Set up the Y axis
   Max = 0
   Call SetPictureFont(S, AxisSetup.AxisFonts(NUMBERS))
   absmax = ps.Ymax
   If Abs(ps.Ymin) > ps.Ymax Then absmax = Abs(ps.Ymax)
   For i = 0 To ps.NumYTicks
      If ps.Percent = True Then
         Tag = sprint((100# * ps.Ymin) / absmax + i * (100# / absmax) * (ps.Ymax - ps.Ymin) / ps.NumYTicks, AxisSetup.Axis(1))
      Else
         Tag = sprint(ps.Ymin + i * (ps.Ymax - ps.Ymin) / ps.NumYTicks, AxisSetup.Axis(1))
      End If
      If (PictureFontWidth(S, Tag, AxisSetup.AxisFonts(NUMBERS)) > Max) Then Max = PictureFontWidth(S, Tag, AxisSetup.AxisFonts(NUMBERS))
   Next i
   If ps.NoYaxisNumbers Then Max = 0
   ' Determine the extents
   If ps.YaxisName = "" Then
      ps.OriginX = Max + PictureFontWidth(S, "X", AxisSetup.AxisFonts(NUMBERS))
   Else
      ps.OriginX = Max + 1.5 * PictureFontWidth(S, "X", AxisSetup.AxisFonts(Yaxis)) + PictureFontWidth(S, "X", AxisSetup.AxisFonts(NUMBERS))
   End If
   If ps.Title = "" Then
      ps.MaxY = PictureFontHeight(S, "X", AxisSetup.AxisFonts(NUMBERS))
   Else
      ps.MaxY = 1.5 * PictureFontHeight(S, "X", AxisSetup.AxisFonts(Title))
   End If
   If ps.XaxisName = "" Then
      ps.OriginY = S.ScaleHeight - 1.5 * PictureFontHeight(S, "X", AxisSetup.AxisFonts(NUMBERS))
   Else
      ps.OriginY = S.ScaleHeight - 1.5 * PictureFontHeight(S, "X", AxisSetup.AxisFonts(NUMBERS)) - PictureFontHeight(S, "X", AxisSetup.AxisFonts(Xaxis))
   End If
   If ps.NoXaxisNumbers Then ps.OriginY = ps.OriginY + PictureFontHeight(S, "X", AxisSetup.AxisFonts(NUMBERS))
   ps.MaxX = S.ScaleWidth - PictureFontWidth(S, "X", AxisSetup.AxisFonts(NUMBERS))
   If Not ps.NoYaxis Then S.Line (ps.OriginX, ps.OriginY)-(ps.OriginX, ps.MaxY)
   Call SetPictureFont(S, AxisSetup.AxisFonts(NUMBERS))
   For i = 0 To ps.NumYTicks
      S.CurrentX = ps.OriginX - Max - S.TextWidth("X") / 2
      S.CurrentY = ps.OriginY + i * (ps.MaxY - ps.OriginY) / ps.NumYTicks - S.TextHeight("X") / 2
      If ps.Percent = True Then
         If Not ps.NoYaxisNumbers Then S.Print sprint((100# * ps.Ymin) / absmax + i * (100# / absmax) * (ps.Ymax - ps.Ymin) / ps.NumYTicks, AxisSetup.Axis(1))
      Else
         If Not ps.NoYaxisNumbers Then S.Print sprint(ps.Ymin + i * (ps.Ymax - ps.Ymin) / ps.NumYTicks, AxisSetup.Axis(1))
      End If
      S.CurrentX = ps.OriginX
      S.CurrentY = ps.OriginY + i * (ps.MaxY - ps.OriginY) / ps.NumYTicks
      If Not ps.NoYaxis Then S.Line -Step(-S.TextWidth("X") / 2, 0)
   Next i
   ' Now do the X axis
   S.Line (ps.OriginX, ps.OriginY)-(ps.MaxX, ps.OriginY)
   For i = 0 To ps.NumXTicks
      S.CurrentY = ps.OriginY + S.TextHeight("X") * 0.5
      S.CurrentX = ps.OriginX + CLng(i) * CLng(ps.MaxX - ps.OriginX) / CLng(ps.NumXTicks)
      Tag = sprint(ps.Xmin + i * (ps.Xmax - ps.Xmin) / ps.NumXTicks, AxisSetup.Axis(0))
      S.CurrentX = S.CurrentX - S.TextWidth(Tag) / 2
      If S.CurrentX < 0 Then S.CurrentX = 0
      If (S.TextWidth(Tag) + S.CurrentX) > S.ScaleWidth Then S.CurrentX = S.ScaleWidth - S.TextWidth(Tag) - S.TextWidth("X")
      If Not ps.NoXaxisNumbers Then S.Print Tag
      S.CurrentY = ps.OriginY
      S.CurrentX = ps.OriginX + CLng(i) * CLng(ps.MaxX - ps.OriginX) / CLng(ps.NumXTicks)
      S.Line -Step(0, S.TextWidth("X") / 2)
   Next i
   ' Now place all titles on plot
   Call SetPictureFont(S, AxisSetup.AxisFonts(Title))
   S.CurrentY = S.TextHeight("X") / 4
   S.CurrentX = S.ScaleWidth / 2 - S.TextWidth(ps.Title) / 2
   S.Print ps.Title
   S.CurrentY = S.ScaleHeight - PictureFontHeight(S, "X", AxisSetup.AxisFonts(Xaxis))
   Call SetPictureFont(S, AxisSetup.AxisFonts(Xaxis))
   S.CurrentX = ps.MaxX + (ps.OriginX - ps.MaxX) / 2 - S.TextWidth(ps.XaxisName) / 2
   S.Print ps.XaxisName
   Call SetPictureFont(S, AxisSetup.AxisFonts(Yaxis))
   Y = ps.MaxY + (ps.OriginY - ps.MaxY) / 2 + S.TextWidth(ps.YaxisName) / 2
   Call RotateText(S, S.hdc, ps.YaxisName, 90, CSng(0), CSng(Y), CSng(Screen.TwipsPerPixelX), CSng(Screen.TwipsPerPixelY))
   Call SetPictureFont(S, AxisSetup.AxisFonts(NUMBERS))
   S.ForeColor = OldColor
End Sub

Sub PlotPictureData(S As PictureBox, ps As PlotStructure, color As Integer)
   Dim X As Long, Y As Long
   Dim i As Integer
   Dim li As Long

   On Error GoTo PlotPictureError
   li = ps.Lstart
   For i = 0 To ps.n - 1
      Y = ((ps.Y(i) - ps.Ymin) / (ps.Ymax - ps.Ymin)) * (ps.MaxY - ps.OriginY) + ps.OriginY
      X = (CDbl(Xvalue(ps, li + CLng(i) * ps.Skip) - CDbl(ps.Xmin)) / CDbl(ps.Xmax - ps.Xmin)) * CDbl(ps.MaxX - ps.OriginX) + CDbl(ps.OriginX)
'      x = ((ps.x(i) - ps.Xmin) / (ps.Xmax - ps.Xmin)) * (ps.MaxX - ps.OriginX) + ps.OriginX
      If Y < ps.MaxY Then Y = ps.MaxY
      If Y > ps.OriginY Then Y = ps.OriginY
      If X < ps.OriginX Then X = ps.OriginX
      If X > ps.MaxX Then X = ps.MaxX
      If (i = 0) Then
         S.PSet (X, Y), QBColor(color)
      Else
         S.Line -(X, Y), QBColor(color)
      End If
   Next i
   Exit Sub
PlotPictureError:
   Call ErrorMess.LogError(Error$ & " in PlotPicture!", "")
   Exit Sub
End Sub

Sub PrepScope(ps As PlotStructure, Mother As PlotStructure, Lstart As Long, Lstop As Long, ln As Long)
   Dim f0 As Single
   Dim iStat As Integer

   ps.LastLevel = 0
   ps = Mother
   ps.Comment = ""
   ps.FileName = ""
   ps.Title = Mother.Title & ", " & Format(Mother.Xmin) & " to " & Format(Mother.Xmax)
   ps.InUse = 0
   ps.ZeroFills = 0
   If ps.Type = 1 Then
      ps.HugeSize = ln
      ps.HugeSizeInitial = ps.HugeSize
   Else
      ps.HugeSize = ln * 2
      ps.HugeSizeInitial = ps.HugeSize
   End If
   iStat = HugeDim(ps.harray, 4, ps.HugeSize)
   If (iStat <> 0) Then
      MsgBox ("ERROR! can't allocate memory.")
      Exit Sub
   End If
   If ps.Type = 1 Then
      Call CopyData(Mother.harray, ps.harray, Lstart, Lstop, ps.HugeSize)
   End If
   If ps.Type = 2 Then
      ps.SampleRate = ps.SampleRate / (Mother.HugeSize / ps.HugeSize)
      If (ps.LowMassFreq = 0) Then
         ps.FreqShift = Xvalue(Mother, Lstart)
         Call CopyData(Mother.harray, ps.harray, Lstart * 2, Lstop * 2, ps.HugeSize)
      Else
         ps.FreqShift = ps.LowMassFreq - Xvalue(Mother, Lstop)
         Call CopyData(Mother.harray, ps.harray, Lstop * 2, Lstart * 2, ps.HugeSize)
         f0 = 0#
         iStat = SetHugeEl(ps.harray, 4, 0, f0)
         iStat = SetHugeEl(ps.harray, 4, 1, f0)
      End If
   End If
   If ps.Type = 3 Then
      ps.SampleRate = ps.SampleRate / (Mother.HugeSize / ps.HugeSize)
      If (Mother.HugeSize = ps.HugeSize) Then
         Call CopyData(Mother.harray, ps.harray, 0, ps.HugeSize, ps.HugeSize)
      ElseIf (ps.LowMassFreq = 0) Then
         ps.FreqShift = Mass2Freq(ps, Xvalue(Mother, Lstop))
         If (ps.FreqShift > Mass2Freq(ps, Xvalue(Mother, Lstart))) Then ps.FreqShift = Mass2Freq(ps, Xvalue(Mother, Lstart))
         Call CopyData(Mother.harray, ps.harray, Lstop * 2, Lstart * 2, ps.HugeSize)
      Else
         ps.FreqShift = ps.LowMassFreq - Mass2Freq(ps, Xvalue(Mother, Lstart))
         Call CopyData(Mother.harray, ps.harray, Lstart * 2, Lstop * 2, ps.HugeSize)
         f0 = 0#
         iStat = SetHugeEl(ps.harray, 4, 0, f0)
         iStat = SetHugeEl(ps.harray, 4, 1, f0)
      End If
   End If
   ps.Xmin = Xvalue(ps, 0&)
   ps.Xmax = Xvalue(ps, ps.HugeSize)
   If ps.Type = 3 Then
      iStat = ExtractPlotData(ps, Mother.Xmin, Mother.Xmax)
   Else
      iStat = ExtractPlotData(ps, Xvalue(ps, 0&), Xvalue(ps, ps.HugeSize))
   End If
End Sub

Sub PrintAxis(ps As PlotStructure, l As Single, T As Single, w As Single, H As Single, Clp As Integer, color As Integer)
   Dim Tag As String
   Dim TagWidth As Integer, Max As Integer
   Dim absmax As Single
   Dim Pwidth As Single
   Dim Pheight As Single
   Dim i As Integer
   Dim OldColor As Long, Y As Long
   
   If IsColorPrinter() Then
      OldColor = Printer.ForeColor
      Printer.ForeColor = QBColor(color)
   End If
   ps.ZoomBox = False
   Printer.Scale
   Pwidth = w
   Pheight = H
   ' Set up the Y axis
   Max = 0
   Call SetPrinterFont(AxisSetup.AxisFonts(NUMBERS))
   absmax = ps.Ymax
   If Abs(ps.Ymin) > ps.Ymax Then absmax = Abs(ps.Ymax)
   For i = 0 To ps.NumYTicks
      If ps.Percent = True Then
         Tag = sprint((100# * ps.Ymin) / absmax + i * (100# / absmax) * (ps.Ymax - ps.Ymin) / ps.NumYTicks, AxisSetup.Axis(1))
      Else
         Tag = sprint(ps.Ymin + i * (ps.Ymax - ps.Ymin) / ps.NumYTicks, AxisSetup.Axis(1))
      End If
      If (PrinterFontWidth(Tag, AxisSetup.AxisFonts(NUMBERS)) > Max) Then Max = PrinterFontWidth(Tag, AxisSetup.AxisFonts(NUMBERS))
   Next i
   If ps.NoYaxisNumbers Then Max = 0
   ' Determine the extents
   If ps.YaxisName = "" Then
      ps.OriginX = Max + PrinterFontWidth("X", AxisSetup.AxisFonts(NUMBERS))
   Else
      ps.OriginX = Max + 1.5 * PrinterFontWidth("X", AxisSetup.AxisFonts(Yaxis)) + PrinterFontWidth("X", AxisSetup.AxisFonts(NUMBERS))
   End If
   If ps.Title = "" Then
      ps.MaxY = PrinterFontHeight("X", AxisSetup.AxisFonts(NUMBERS))
   Else
      ps.MaxY = 1.5 * PrinterFontHeight("X", AxisSetup.AxisFonts(Title))
   End If
   If ps.XaxisName = "" Then
      ps.OriginY = Pheight - 1.5 * PrinterFontHeight("X", AxisSetup.AxisFonts(NUMBERS))
   Else
      ps.OriginY = Pheight - 1.5 * PrinterFontHeight("X", AxisSetup.AxisFonts(NUMBERS)) - PrinterFontHeight("X", AxisSetup.AxisFonts(Xaxis))
   End If
   If ps.NoXaxisNumbers Then ps.OriginY = ps.OriginY + PrinterFontHeight("X", AxisSetup.AxisFonts(NUMBERS))
   ps.MaxX = Pwidth - PrinterFontWidth("X", AxisSetup.AxisFonts(NUMBERS))
   ps.MaxX = ps.MaxX + l
   ps.OriginX = ps.OriginX + l
   ps.MaxY = ps.MaxY + T
   ps.OriginY = ps.OriginY + T
   If Not ps.NoYaxis Then Printer.Line (ps.OriginX, ps.OriginY)-(ps.OriginX, ps.MaxY)
   Call SetPrinterFont(AxisSetup.AxisFonts(NUMBERS))
   For i = 0 To ps.NumYTicks
      Printer.CurrentX = CSng(ps.OriginX) - CSng(Max) - Printer.TextWidth("X") / 2
      Printer.CurrentY = CSng(ps.OriginY) + CSng(i) * (CSng(ps.MaxY) - CSng(ps.OriginY)) / CSng(ps.NumYTicks) - CSng(Printer.TextHeight("X")) / 2#
      If ps.Percent = True Then
         If Not ps.NoYaxisNumbers Then Printer.Print sprint((100# * ps.Ymin) / absmax + i * (100# / absmax) * (ps.Ymax - ps.Ymin) / ps.NumYTicks, AxisSetup.Axis(1))
      Else
         If Not ps.NoYaxisNumbers Then Printer.Print sprint(ps.Ymin + i * (ps.Ymax - ps.Ymin) / ps.NumYTicks, AxisSetup.Axis(1))
      End If
      Printer.CurrentX = CSng(ps.OriginX)
      Printer.CurrentY = CSng(ps.OriginY) + CSng(i) * (CSng(ps.MaxY) - CSng(ps.OriginY)) / CSng(ps.NumYTicks)
      If Not ps.NoYaxis Then Printer.Line -Step(-Printer.TextWidth("X") / 2, 0)
   Next i
   ' Now do the X axis
   Printer.Line (ps.OriginX, ps.OriginY)-(ps.MaxX, ps.OriginY)
   For i = 0 To ps.NumXTicks
      Printer.CurrentY = CSng(ps.OriginY) + CSng(Printer.TextHeight("X")) * 0.5
      Printer.CurrentX = CSng(ps.OriginX) + CSng(i) * (CSng(ps.MaxX) - CSng(ps.OriginX)) / CSng(ps.NumXTicks)
      Tag = sprint(ps.Xmin + i * (ps.Xmax - ps.Xmin) / ps.NumXTicks, AxisSetup.Axis(0))
      Printer.CurrentX = Printer.CurrentX - Printer.TextWidth(Tag) / 2
      If Printer.CurrentX < 0 Then Printer.CurrentX = l
      If (Clp) Then
         If (Printer.TextWidth(Tag) + Printer.CurrentX) > (Pwidth + l) Then Printer.CurrentX = Pwidth - Printer.TextWidth(Tag) - Printer.TextWidth("X") + l
      Else
         If (Printer.TextWidth(Tag) + Printer.CurrentX) > (Printer.ScaleWidth) Then Printer.CurrentX = Printer.ScaleWidth - Printer.TextWidth(Tag) - Printer.TextWidth("X")
      End If
      If Not ps.NoXaxisNumbers Then Printer.Print Tag
      Printer.CurrentY = CSng(ps.OriginY)
      Printer.CurrentX = CSng(ps.OriginX) + CSng(i) * (CSng(ps.MaxX) - CSng(ps.OriginX)) / CSng(ps.NumXTicks)
      Printer.Line -Step(0, Printer.TextWidth("X") / 2)
   Next i
   ' Now place all titles on plot
   Call SetPrinterFont(AxisSetup.AxisFonts(Title))
   Printer.CurrentY = (Printer.TextHeight("X") / 4) + T
   Printer.CurrentX = Pwidth / 2 - Printer.TextWidth(ps.Title) / 2 + l
   Printer.Print ps.Title
   Printer.CurrentY = Pheight - PrinterFontHeight("X", AxisSetup.AxisFonts(Xaxis)) + T
   Call SetPrinterFont(AxisSetup.AxisFonts(Xaxis))
   Printer.CurrentX = CSng(ps.MaxX) + (CSng(ps.OriginX) - CSng(ps.MaxX)) / 2# - Printer.TextWidth(ps.XaxisName) / 2#
   Printer.Print ps.XaxisName
   Call SetPrinterFont(AxisSetup.AxisFonts(Yaxis))
   Y = ps.MaxY + (ps.OriginY - ps.MaxY) / 2 + Printer.TextWidth(ps.YaxisName) / 2
   If RotateText(Printer, Printer.hdc, ps.YaxisName, 90, CSng(l), CSng(Y), CSng(Printer.TwipsPerPixelX), CSng(Printer.TwipsPerPixelY)) = -1 Then
      For i = 1 To Len(ps.YaxisName)
         Printer.CurrentX = Printer.TextWidth("X") - Printer.TextWidth(Mid$(ps.YaxisName, i, 1)) / 2 + l
         Printer.CurrentY = CSng(ps.MaxY) + (CSng(ps.OriginY) - CSng(ps.MaxY)) / 2# - (Len(ps.YaxisName) * Printer.TextHeight("X")) / 2 + (i - 1) * Printer.TextHeight("X")
         Printer.Print Mid$(ps.YaxisName, i, 1)
      Next i
   End If
   Call SetPrinterFont(AxisSetup.AxisFonts(NUMBERS))
   If IsColorPrinter() Then Printer.ForeColor = OldColor
End Sub

Sub PrintData(ps As PlotStructure, pSd As PlotStructure, color As Integer)
   Dim X As Long, Y As Long
   Dim i As Integer
   Dim li As Long

   On Error GoTo PrintDataError
   li = pSd.Lstart
   For i = 0 To pSd.n - 1
      Y = ((pSd.Y(i) - ps.Ymin) / (ps.Ymax - ps.Ymin)) * (ps.MaxY - ps.OriginY) + ps.OriginY
      X = (CDbl(Xvalue(pSd, li + CLng(i) * pSd.Skip) - CDbl(ps.Xmin)) / CDbl(ps.Xmax - ps.Xmin)) * CDbl(ps.MaxX - ps.OriginX) + CDbl(ps.OriginX)
'      x = ((pSd.x(i) - ps.Xmin) / (ps.Xmax - ps.Xmin)) * (ps.MaxX - ps.OriginX) + ps.OriginX
      If Y < ps.MaxY Then Y = ps.MaxY
      If Y > ps.OriginY Then Y = ps.OriginY
      If X < ps.OriginX Then X = ps.OriginX
      If X > ps.MaxX Then X = ps.MaxX
      If IsColorPrinter() Then
         If (i = 0) Then
            Printer.PSet (X, Y)
         Else
            Printer.Line -(X, Y)
         End If
      Else
         If (i = 0) Then
            Printer.PSet (X, Y), QBColor(color)
         Else
            Printer.Line -(X, Y), QBColor(color)
         End If
      End If
   Next i
   Exit Sub
PrintDataError:
   Call ErrorMess.LogError(Error$ & " in PrintData!", "")
   Exit Sub
End Sub
Function PrinterFontHeight(Tag As String, fntyp As FN) As Integer
   Call SetPrinterFont(fntyp)
   PrinterFontHeight = Printer.TextHeight(Tag)
End Function

Function PrinterFontWidth(Tag As String, fntyp As FN) As Integer
   Call SetPrinterFont(fntyp)
   PrinterFontWidth = Printer.TextWidth(Tag)
End Function

Sub PrintPeaks(ps As PlotStructure)
   Dim i As Integer, ds As Integer
   Dim fmt As String
   
   ds = GetPrivateProfileInt("Scope", "PeakDigits", 2&, "icr-2ls.ini")
   fmt = "." & String(ds, "#")
   For i = 0 To pk.NumPeaks - 1
      If (pk.Xloc(i) >= ps.Xmin) And (pk.Xloc(i) <= ps.Xmax) Then
         Printer.PSet (X2cursor(pk.Xloc(i), ps), Y2cursor(pk.Peak(i), ps) - 100)
         Printer.Line -(X2cursor(pk.Xloc(i), ps), Y2cursor(pk.Peak(i), ps) + 100)
         Printer.PSet (X2cursor(pk.Xloc(i), ps) - 100, Y2cursor(pk.Peak(i), ps))
         Printer.Line -(X2cursor(pk.Xloc(i), ps) + 100, Y2cursor(pk.Peak(i), ps))
         Printer.CurrentX = X2cursor(pk.Xloc(i), ps) + 100
         Printer.CurrentY = Y2cursor(pk.Peak(i), ps) + 50
         Printer.Print Format(pk.Xloc(i), fmt)
      End If
   Next i

End Sub
Function FindFWHM(ps As PlotStructure, li As Long) As Double
Dim Peak As Double, upper As Double, lower As Double
Dim Y1 As Double, Y2 As Double, X1 As Double, X2 As Double
Dim Index As Long, j As Integer
Dim iStat As Integer
Dim mass As Double
Dim coe(1)  As Double, Points As Long
Dim mse As Double
Dim vx() As Double
Dim vy() As Double

   FindFWHM = 0
   mass = Xvalue(ps, li)
   Peak = Yvalue(ps, li)
   If Peak = 0 Then
      Exit Function
   End If
   lower = 0#
   upper = 0#
   For Index = li To ps.HugeSize / 2
      Y1 = Yvalue(ps, Index)
      If (Y1 > Peak) Then Exit For
      If (Y1 < Peak / 2) Or (Abs(mass - Xvalue(ps, Index)) > 5#) Or Yvalue(ps, Index + 1) > Y1 Then
         Y2 = Yvalue(ps, Index - 1)
         X1 = Xvalue(ps, Index)
         X2 = Xvalue(ps, Index - 1)
         If ((Y2 - Y1) <> 0) And (Y1 < Peak / 2) Then
            upper = X1 - (X1 - X2) * (Peak / 2 - Y1) / (Y2 - Y1)
            Exit For
         Else
            upper = X1
            Points = Index - li + 1
            If Points >= 3 Then
               ReDim vx(Points) As Double
               ReDim vy(Points) As Double
               For j = 0 To Points - 1
                  vx(j) = Xvalue(ps, li + j)
                  vy(j) = Yvalue(ps, li + j)
               Next
               iStat = CurvReg(vy(0), vx(0), Points, coe(0), 1, mse)
               upper = coe(1) * Peak / 2 + coe(0)
            End If
         End If
         Exit For
      End If
   Next
   For Index = li To 0 Step -1
      Y1 = Yvalue(ps, Index)
      If (Y1 > Peak) Then Exit For
      If (Y1 < Peak / 2) Or (Abs(mass - Xvalue(ps, Index)) > 5#) Or Yvalue(ps, Index - 1) > Y1 Then
         Y2 = Yvalue(ps, Index + 1)
         X1 = Xvalue(ps, Index)
         X2 = Xvalue(ps, Index + 1)
         If ((Y2 - Y1) <> 0) And (Y1 < Peak / 2) Then
            lower = X1 + (X2 - X1) * (Peak / 2 - Y1) / (Y2 - Y1)
            Exit For
         Else
            lower = X1
            Points = li - Index + 1
            If Points >= 3 Then
               ReDim vx(Points) As Double
               ReDim vy(Points) As Double
               For j = 0 To Points - 1
                  vx(j) = Xvalue(ps, Index + j)
                  vy(j) = Yvalue(ps, Index + j)
               Next
               iStat = CurvReg(vy(0), vx(0), Points, coe(0), 1, mse)
               lower = coe(1) * Peak / 2 + coe(0)
            End If
         End If
         Exit For
      End If
   Next
   If upper = 0 Then upper = mass + Abs(mass - lower)
   If lower = 0 Then lower = mass - Abs(mass - upper)
   FindFWHM = Abs(upper - lower)
   Erase vx
   Erase vy
End Function
Function LorentzianLS(ps As PlotStructure, A As Double, FWHM As Double, Vo As Double, li As Long) As Double
Dim Lstart As Long, Lstop As Long
Dim Index As Long
Dim RMSerror As Double, u As Double, Y1 As Double, Y2 As Double

   ' first determine the range to work over...
   Lstart = FindIndex(ps, CSng(Xvalue(ps, li) + FWHM)) + 1
   Lstop = FindIndex(ps, CSng(Xvalue(ps, li) - FWHM)) - 1
   If Lstart > Lstop Then
      Index = Lstop
      Lstop = Lstart
      Lstart = Index
   End If
   RMSerror = 0
   For Index = Lstart To Lstop
      u = 2 / FWHM * (Xvalue(ps, Index) - Vo)
      Y1 = A / (1 + u * u)
      Y2 = Yvalue(ps, Index)
      RMSerror = RMSerror + (Y1 - Y2) ^ 2
   Next
   LorentzianLS = RMSerror
End Function
Function QuadraticFit(ps As PlotStructure, li As Long) As Double
Dim X1 As Double, X2 As Double, x3 As Double, d As Double
Dim Y1 As Double, Y2 As Double, y3 As Double

'QuadraticFit = LorentzianFit(ps, li)
'Exit Function
   X1 = Xvalue(ps, li - 1)
   X2 = Xvalue(ps, li)
   x3 = Xvalue(ps, li + 1)
   Y1 = Yvalue(ps, li - 1)
   Y2 = Yvalue(ps, li)
   y3 = Yvalue(ps, li + 1)
   d = ((Y2 - Y1) * (x3 - X2) - (y3 - Y2) * (X2 - X1))
   If d = 0# Then
      QuadraticFit = X2
      Exit Function
   End If
   QuadraticFit = ((X1 + X2) - ((Y2 - Y1) * (x3 - X2) * (X1 - x3)) / ((Y2 - Y1) * (x3 - X2) - (y3 - Y2) * (X2 - X1))) / 2#
End Function
Function LorentzianFit(ps As PlotStructure, li As Long) As Double
Dim i As Integer, j As Integer
Dim FWHM As Double
Dim A As Double
Dim Vo As Double
Dim E As Double, CE As Double, le As Double

   FWHM = FindFWHM(ps, li)
   A = Yvalue(ps, li)
   Vo = Xvalue(ps, li)
   E = Abs(Xvalue(ps, li) - Xvalue(ps, li + 1)) / 100#
   CE = LorentzianLS(ps, A, FWHM, Vo, li)
'   For j = 0 To 4
   For i = 0 To 50
      le = CE
      Vo = Vo + E
      CE = LorentzianLS(ps, A, FWHM, Vo, li)
      If (CE > le) Then Exit For
   Next
   Vo = Vo - E
   CE = LorentzianLS(ps, A, FWHM, Vo, li)
   For i = 0 To 50
      le = CE
      Vo = Vo - E
      CE = LorentzianLS(ps, A, FWHM, Vo, li)
      If (CE > le) Then Exit For
   Next
   Vo = Vo + E
'   ce = LorentzianLS(ps, A, FWHM, Vo, li)
'   E = E / 10#
'   Next j
   LorentzianFit = Vo
End Function
Function GetMostIntensePeak(Optional Ymax As Double = 0) As Double
Dim i As Integer, j As Integer
Dim d As Double

   GetMostIntensePeak = -1#
   d = 0#
   If pk.NumPeaks > 0 Then
      For i = 0 To (pk.NumPeaks - 1)
         If pk.Peak(i) > d Then
            d = pk.Peak(i)
            j = i
         End If
      Next i
   End If
   If d > 0# Then
      GetMostIntensePeak = pk.Xloc(j)
      Ymax = pk.Peak(j)
   End If
End Function
Function InPeakList(Peak As Double) As Boolean
Dim i As Integer

   InPeakList = True
   i = -1
   On Error Resume Next
   i = UBound(PKlist.Peaks)
   If i < 0 Then Exit Function
   InPeakList = False
   For i = 0 To UBound(PKlist.Peaks)
      If Abs(Peak - PKlist.Peaks(i)) < PKlist.Tolerence Then
         InPeakList = True
         Exit Function
      End If
   Next
End Function
Sub ReportAttentionList(ps As PlotStructure)
Dim i As Long, j As Long, Yval As Double
Dim rstr As String, xstr As String
   
   i = -1
   On Error Resume Next
   i = UBound(PKlist.Peaks)
   If i = -1 Then Exit Sub
   ' Print each peak...
   rstr = ""
   xstr = ""
   For i = 0 To UBound(PKlist.Peaks)
      Yval = 0
      For j = 0 To NumPeaks
         If Abs(PeakX(j) - PKlist.Peaks(i)) <= PKlist.Tolerence Then
            Yval = PeakY(j)
            Exit For
         End If
      Next
      If xstr = "" Then
         xstr = Format(PKlist.Peaks(i))
      Else
         xstr = xstr & Chr(9) & Format(PKlist.Peaks(i))
      End If
      If rstr = "" Then
         rstr = Format(Yval)
      Else
         rstr = rstr & Chr(9) & Format(Yval)
      End If
   Next
   Call LogMess(ps, xstr)
   Call LogMess(ps, rstr)
End Sub
Sub LoadAttentionList(FileName As String)
Dim FileNum As Integer, i As Integer
Dim Num As Long, str As String
Dim dVal As Double

   ' Open attention filename
   Erase PKlist.Peaks
   On Error GoTo LoadAttentionListError
   FileNum = FreeFile
   Open FileName For Binary Access Read As FileNum
   ' Read list...
   Num = -1
   Do
      str = GetString(FileNum)
      If str <> "" Then
         If InStr(str, "Tolerance") <> 0 Then
            i = InStr("Tolerance", str)
            i = i + 10
            str = Mid(str, i)
            i = InStr(str, "=")
            If i > 0 Then str = Mid(str, i + 1)
            PKlist.Tolerence = val(str)
         Else
            dVal = val(str)
            Num = Num + 1
            ReDim Preserve PKlist.Peaks(Num) As Double
            PKlist.Peaks(Num) = dVal
         End If
      End If
      If EOF(FileNum) Then Exit Do
   Loop
   Close FileNum
   Exit Sub
LoadAttentionListError:
   Exit Sub
End Sub
' Mode = 0 for file overwrite mode
'      = 1 to save results to file
' Rule = 0 for peak pick
'        1 for RIC
'        2 for Covariance
'        4 pick pick with lorentizian fit
'        5 Pick pick with 3 point quadratic
' Save = 0 save no data to file
'        1 save peak location and magnitude
Function ProcessPeaks(ps As PlotStructure, Dstart As Single, Dstop As Single, Save As Integer, FileName As String, Threshold As Single, Order As Integer, Mode As Integer, Rule As Integer, Optional SignalToNoise As Single = 2#, Optional Display As Boolean = True, Optional Report As Boolean = True) As Double
   Dim li As Long
   Dim temp As Long
   Dim Lstart As Long
   Dim Lstop As Long
   Dim Val1 As Single
   Dim Val2 As Single
   Dim G As Single, T As Single
   Dim d As Single
   Dim Thres As Single
   Dim Sum As Double, Sn As Double, Y1 As Double, Y2 As Double, FWHM As Double
   Dim Arm As Integer
   Dim FileNum As Integer
   Dim NumScopes As Integer
   Dim i As Integer, j As Integer, CS As Integer, NewFile
   Dim Si(MAXSCOPES) As Integer
   Dim ftemp As Double
   Dim nmass As Single, fVal As Single
   Dim NoHeader As String
   Dim AttFile As String
   Dim Ttype As String
   
   If ps.harray = 0 Then Exit Function
   NumPeaks = 0
'   On Error GoTo Done
   NoHeader = String(10, " ")
   Call GetPrivateProfileString("Peaks", "NoHeader", "N", NoHeader, 10, "icr-2ls.ini")
   AttFile = String(100, " ")
   i = GetPrivateProfileString("Peaks", "AttentionList", "", AttFile, 100, "icr-2ls.ini")
   If i > 0 Then AttFile = Mid(AttFile, 1, i)
   Ttype = String(20, " ")
   i = GetPrivateProfileString("Peaks", "Threshold", "Percent", Ttype, 20, "icr-2ls.ini")
   If i > 0 Then Ttype = Mid(Ttype, 1, i)
   Call LoadAttentionList(AttFile)
   MacroRecord ("ProcessPeaks," + Format$(Dstart) + "," + Format$(Dstop) + "," + Format$(Save) + "," + FileName + "," + Format$(Threshold) + "," + Format$(Order) + "," + Format$(Mode) + "," + Format$(Rule) + "," + Format$(SignalToNoise))
   If Save = 1 Or Save = 2 Then
      FileNum = FreeFile
      NewFile = True
      If Mode = 0 Then
         Open FileName For Output Access Write As FileNum
         Seek #FileNum, 1
      Else
         If Dir(FileName) <> "" Then NewFile = False
         Open FileName For Append Access Write As FileNum
      End If
      If (Rule = 1 Or Rule = 2) And NewFile And (InStr(NoHeader, "N") <> 0) Then
         If Rule = 1 Then
            Print #FileNum, "RIC file,"; Time; Date
            Print #FileNum, "Threshold: "; Threshold
            Print #FileNum, "Start m/z: "; Dstart
            Print #FileNum, "Stop m/z: "; Dstop
            Print #FileNum, "File name", "RIC"
         Else
            Print #FileNum, "SPC file,"; Time; Date
            Print #FileNum, "Threshold: "; Threshold
            Print #FileNum, "Order: "; Order
            Print #FileNum, "Start m/z: "; Dstart
            Print #FileNum, "Stop m/z: "; Dstop
            Print #FileNum, "File name", "SPC"
         End If
      End If
      If (Rule = 0 Or Rule = 4 Or Rule = 5) And (InStr(NoHeader, "N") <> 0) Then
         Print #FileNum, "Peak location file,"; Time; Date
         Print #FileNum, "File name: "; ps.FileName
         Print #FileNum, "Peak threshold: "; Threshold; " Percent"
         If Rule = 0 Then Print #FileNum, "Mode: Apex"
         If Rule = 4 Then Print #FileNum, "Mode: Lorentizan"
         If Rule = 5 Then Print #FileNum, "Mode: 3 point quadratic"
         If Save = 1 Then  ' Location and magnitude
            Print #FileNum, "m/z", ps.YaxisName
         End If
      End If
   End If
   Lstart = FindIndex(ps, Dstart)
   Lstop = FindIndex(ps, Dstop)
   If Lstart > Lstop Then
      temp = Lstart
      Lstart = Lstop
      Lstop = temp
   End If
   If Rule = 0 Or Rule = 4 Or Rule = 5 Then  ' Peak picker mode...
                                 ' 0 = Apex mode
                                 ' 4 = Lorentizan mode
                                 ' 5 = 3 point quadratic
      Sum = 0#
      If Report Then
         Call LogMess(ps, "Peak picker results:")
         If (Rule = 0) Then
            Call LogMess(ps, "Apex mode")
         ElseIf (Rule = 4) Then
            Call LogMess(ps, "Lorentizian mode")
         ElseIf (Rule = 5) Then
            Call LogMess(ps, "3 point quadratic mode")
         End If
      End If
      ' init the peaks data structure
      If Display Then pk.InUse = True
      pk.NumPeaks = 0
      pk.PI = ActivePlot
      ' Loop through the selected range and locate peaks.
      Lstart = FindIndex(ps, Dstart)
      Lstop = FindIndex(ps, Dstop)
      If Lstart > Lstop Then
         temp = Lstart
         Lstart = Lstop
         Lstop = temp
      End If
      If Ttype = "Percent" Then
         If Threshold <= 0# Then
            Thres = FindAverage(ps, ps.Xmin, ps.Xmax)      '********************
'              Debug.Print "Threshold = " & Thres
            If MassXformParms.txtSmoothPeaks = 1 Then
               Thres = FindAverage(ps, ps.Xmin, ps.Xmax, Thres * 5#) * 2.65   'Use a number > 2.65 for unsmoothed data
            Else
               Thres = FindAverage(ps, ps.Xmin, ps.Xmax, Thres * 5#) * 3.65   'Use a number > 2.65 for unsmoothed data
'              Debug.Print "Threshold = " & Thres
            End If
         Else
            Thres = ps.Ymax * CSng(Threshold) / 100#
            Thres = FindMax(ps, Dstart, Dstop) * CSng(Threshold) / 100#
         End If
      ElseIf Ttype = "Absolute" Then
         Thres = Threshold
      ElseIf Ttype = "Histogram" Then
         Call BuildHistogram(ps, Dstart, Dstop, True, Thres)
         If Threshold <= 0# Then
            Thres = Thres * 15#
         Else
            Thres = Thres * Threshold
         End If
      End If
      ZThres = Thres ' for use in zscore
      ThresPct = 100 * Thres / ps.Ymax  ' for use in zscore
      ' Read 1st value
      Val1 = Yvalue(ps, Lstart + 5)
      For li = Lstart + 1 To Lstop - 1
         ' Read next value
         Val2 = Yvalue(ps, li)
         If NumPeaks > 30000 Then Exit For
         ' Calculate difference
         If (Val2 - Val1) <> 0 Then d = Val2 - Val1 Else d = 0#
         ' if D is negative, and Arm is true then Val1 is peak
         If (d < 0) And (Arm = True) Then
            Sn = FindSignalToNoise(ps, Xvalue(ps, li - 1))
            If (Sn < SignalToNoise) Then
               FWHM = FindFWHM(ps, li - 1)
               If FWHM > 0 And FWHM < 0.5 Then
                  Y1 = FindIndex(ps, Xvalue(ps, li - 1) + FWHM)
                  Y2 = FindIndex(ps, Xvalue(ps, li - 1) - FWHM)
                  If (Yvalue(ps, Y1) + Yvalue(ps, Y2)) <> 0 Then
                     Sn = 2# * Yvalue(ps, li - 1) / (Yvalue(ps, Y1) + Yvalue(ps, Y2))
                  Else
                     Sn = 10
                  End If
               End If
            End If
         If (Sn > SignalToNoise) And InPeakList(Xvalue(ps, li - 1)) Then
            ' if here, then peak apex is in Val1
            If (NumPeaks = 0) Then
               ReDim PeakX(NumPeaks) As Double
               ReDim PeakY(NumPeaks) As Double
            Else
               ReDim Preserve PeakX(NumPeaks) As Double
               ReDim Preserve PeakY(NumPeaks) As Double
            End If
            PeakY(NumPeaks) = Val1
            PeakX(NumPeaks) = Xvalue(ps, li - 1)
            If Rule = 4 Then PeakX(NumPeaks) = LorentzianFit(ps, li - 1)
            If Rule = 5 Then PeakX(NumPeaks) = QuadraticFit(ps, li - 1)
            NumPeaks = NumPeaks + 1
            ' here with peak located
            If pk.NumPeaks < MAXPEAKS Then
               pk.Peak(pk.NumPeaks) = Val1
               pk.Xloc(pk.NumPeaks) = Xvalue(ps, li - 1)
               If Rule = 4 Then pk.Xloc(pk.NumPeaks) = LorentzianFit(ps, li - 1)
               If Rule = 5 Then pk.Xloc(pk.NumPeaks) = QuadraticFit(ps, li - 1)
               If Report Then Call LogMess(ps, CStr(pk.Xloc(pk.NumPeaks)) & Chr(9) & pk.Peak(pk.NumPeaks))
               Sum = Sum + pk.Peak(pk.NumPeaks)
               If Save = 1 Then
                  Print #FileNum, pk.Xloc(pk.NumPeaks), pk.Peak(pk.NumPeaks)
               End If
               pk.NumPeaks = pk.NumPeaks + 1
            Else
               If Save = 1 Then
                  If Rule = 4 Then
                     Print #FileNum, LorentzianFit(ps, li - 1), Val1
                  ElseIf Rule = 0 Then
                     Print #FileNum, Xvalue(ps, li - 1), Val1
                  ElseIf Rule = 5 Then
                     Print #FileNum, QuadraticFit(ps, li - 1), Val1
                  End If
               End If
            End If
         End If
         End If
         ' if D is positive, and Val2 is above threshold then set arm flag
         If (d > 0) And (Val2 > Thres) Then
            Arm = True
         Else
            Arm = False
         End If
         Val1 = Val2
      Next li
      If Report Then Call LogMess(ps, "Total Ion Intensity = " & CStr(Sum))
   End If
   If Rule = 1 Then   ' RIC mode
      Sum = 0#
      Thres = 0#
      If Threshold < 0# Then Thres = -Threshold
      If Threshold > 0# Then
         For li = Lstart + 1 To Lstop - 1
            ' Read next value
            Sum = Sum + Yvalue(ps, li)
         Next li
         Thres = (Sum / CDbl(Lstop - Lstart)) * Threshold
         Sum = 0#
      End If
      For li = Lstart + 1 To Lstop - 1
         ' Read next value
         Val1 = Yvalue(ps, li)
         If (Val1 > Thres) Then Sum = Sum + Val1
      Next li
      Call Scopes(FindPI(ps)).MakeNewTag("RIC = " & Format$(Sum), CDbl(Plots(ActivePlot).Xmin), CDbl(Plots(ActivePlot).Ymax))
      If Report Then Call LogMess(ps, "RIC = " & Format$(Sum))
      If Save = 1 Then
         Print #FileNum, ps.FileName, Sum
      End If
   End If
   If Rule = 2 Then   ' Covariance
      Sum = 0#
      Thres = 0#
      If Threshold <> 0# Then
         For li = Lstart + 1 To Lstop - 1
            ' Read next value
            Sum = Sum + Yvalue(ps, li)
         Next li
         Thres = (Sum / CDbl(Lstop - Lstart)) * Threshold
      End If
      ' Make sure we have the open scopes we need
      Sum = 0#
      NumScopes = 1
      Si(0) = ActivePlot
      For i = 0 To MAXSCOPES - 1
         If (Plots(i).InUse) And (i <> ActivePlot) Then
            If (ps.HugeSize = Plots(i).HugeSize) And _
               (ps.Type = Plots(i).Type) And _
               (ps.Xmin = Plots(i).Xmin) And _
               (ps.Xmax = Plots(i).Xmax) Then
               Si(NumScopes) = i
               NumScopes = NumScopes + 1
            End If
         End If
      Next i
      ' make sure we have only order number of scopes
      If NumScopes = Order Then
         For li = Lstart + 1 To Lstop - 1
            ftemp = Yvalue(Plots(Si(0)), li)
            If (ftemp < Thres) Then ftemp = 0#
            For i = 1 To Order - 1
               ftemp = ftemp * Yvalue(Plots(Si(i)), li)
            Next i
            Sum = Sum + ftemp
         Next li
         ' now delete the oldest scope
         Do
           If LastScope = Si(0) Then LastScope = LastScope + 1
           If Plots(LastScope).InUse Then
               Unload Scopes(LastScope)
               Exit Do
           End If
           LastScope = LastScope + 1
           If LastScope >= MAXSCOPES Then LastScope = 0
         Loop
      Else
         LastScope = 0
      End If
      Call Scopes(Si(0)).MakeNewTag("SPC = " & Format$(Sum), CDbl(Plots(ActivePlot).Xmin), CDbl(Plots(ActivePlot).Ymax))
      If Save = 1 Then
         Print #FileNum, ps.FileName, Sum
      End If
   End If
   If Save = 1 Or Save = 2 Then
      Close (FileNum)
   End If
   ProcessPeaks = Sum
   On Error Resume Next
   Scopes(Si(0)).Refresh
   Call ReportAttentionList(ps)
   Exit Function
Done:
   Call ErrorMess.LogError(Error$ & " in ProcessPeaks!", "")
   Exit Function
End Function
Sub LogFile(Mess As String, Optional TimeStamp As Integer = True)
Dim FileNum As Integer

   On Error Resume Next
   If (LogFileName = "") Then Exit Sub
   FileNum = FreeFile
   Open LogFileName For Append Access Write As FileNum
   If (TimeStamp) Then
      Print #FileNum, Mess & ", " & Time & "  " & Date
   Else
      Print #FileNum, Mess
   End If
   Close FileNum
End Sub
Sub LogMess(ps As PlotStructure, Mess As String)
   ps.Comment = ps.Comment & Mess & Chr$(13) & Chr$(10)
End Sub

Function ReadFinniganHeader(FileName As String, FH As FinniganHeader) As Integer

   FH.ByteFormat = ReadSunFileByte(FileName, 1)
   FH.Processor = ReadSunFileByte(FileName, 2)
   FH.MagicNum = ReadSunFileByte(FileName, 3)
   FH.FileType = ReadSunFileByte(FileName, 4)
   FH.RevLevel = ReadSunFileInt(FileName, 5)
   FH.Descriptor = ReadSunFileInt(FileName, 7)
   FH.SeqSize = ReadSunFileLong(FileName, 9)
   FH.DataSize = ReadSunFileLong(FileName, 17)
   FH.TrailorRecordsSize = ReadSunFileLong(FileName, 21)
   FH.TrailorRecordsNamesSize = ReadSunFileLong(FileName, 25)
   FH.ExtendedDataSize = ReadSunFileLong(FileName, 29)
   ReadFinniganHeader = 0
   If (FH.MagicNum <> 62) Then ReadFinniganHeader = -1
   If (FH.FileType <> 1) And (FH.FileType <> 2) And (FH.FileType <> 3) And (FH.FileType <> 37) Then ReadFinniganHeader = -1
End Function
Sub LoadTags(PI As Integer)
Dim FileNum As Integer, i As Integer, comma As Integer
Dim rec As String
Dim Found As Boolean, c As Integer
Dim TagText As String, X As Double, Y As Double

   On Error Resume Next
   FileNum = FreeFile
   Found = False
   Open Plots(PI).FileName For Input Access Read As FileNum
   Do
      rec = GetString(FileNum)
      If rec = "TagsEnd" Then Exit Do
      If rec = "End" Then Exit Do
      If EOF(FileNum) = True Then Exit Do
      If Found Then
         ' Now decode the tag...
         comma = 0
         For i = Len(rec) To 1 Step -1
            ' Find the first comma
            If Mid(rec, i, 1) = "," Then comma = comma + 1
            If comma = 3 Then Exit For
         Next
         If comma = 3 Then
            If i > 1 Then TagText = Mid(rec, 1, i - 1) Else TagText = ""
            X = val(ParseString(Mid(rec, i + 1), 0))
            Y = val(ParseString(Mid(rec, i + 1), 1))
            c = val(ParseString(Mid(rec, i + 1), 2))
         End If
         Call Scopes(PI).MakeNewTag(TagText, X, Y, c)
      End If
      If rec = "Tags:" Then Found = True
   Loop
   Close (FileNum)
End Sub
' This routine reads the file header, if found, and
' returns a pointer to start of data.
Function ReadHeader(ps As PlotStructure) As Long
   Dim FileNum As Integer
   Dim ftype As Integer
   Dim rec As String
   Dim rec1 As String
   Dim Start As Long, i As Long, j As Long
   Dim en As Long
   Dim Pos As Long, CalType As Integer, li As Long
   Dim Tag As String, TotalTime As Double
   Dim Atrap As Double, Strap As Double, Climit As Double, temp As Double
   Dim Ft As Long, Fti As Integer
   Dim Ans As String, fVal As Double
   Dim ZeroFill As Boolean, ZerofillNumber As Integer
   Dim b1 As Byte, b2 As Byte, b3 As Byte
   Dim newExperiment As IonSpecExperiment
   
   On Error Resume Next
   ZeroFill = False
   ZerofillNumber = 0
   ps.MaxPoint = 0
   ps.FreqShift = 0#
   ps.YaxisName = "Amplitude"
   ps.XaxisName = ""
   Select Case ps.FileType
      Case 1   ' integer time, no header
         Load Parms
         Parms.Show 1
         ps.Type = 1
         ReadHeader = 0
         ps.HugeSizeInitial = ps.HugeSize
      Case 2   ' floats, sun extrel format
         Tag = FindDetectSlice(ps.FileName)
         If Tag <> "" Then
            Tag = Tag + " {"
         Else
            Tag = "Type=detect"
         End If
         ftype = ReadSunFileByte(ps.FileName, 4)
         Pos = ReadSunFileInt(ps.FileName, 11)
         FileNum = FreeFile
         Open ps.FileName For Binary Access Read As FileNum
         Seek FileNum, 65
         rec1 = Input(Pos, #FileNum)
         Start = InStr(rec1, Tag)
         en = InStr(Start, rec1, "}")
         rec = Mid$(rec1, Start, en - Start)
         Start = 1
         If InStr(Start, rec, "dataPoints:") <> 0 Then ps.HugeSize = val(Mid(rec, InStr(Start, rec, "dataPoints:") + 11))
         If InStr(Start, rec, "dwell:") <> 0 Then
            ps.SampleRate = val(Mid(rec, InStr(Start, rec, "dwell:") + 6))
            ps.SampleRate = 1# / ps.SampleRate
         End If
         If InStr(rec, "Zerofill=true") <> 0 Then ZeroFill = True
         If InStr(rec, "ZerofillNumber=") <> 0 Then ZerofillNumber = val(Mid(rec, InStr(rec, "ZerofillNumber=") + 15))
         If InStr(rec, "analyzerTrapVoltage:") <> 0 Then Atrap = val(Mid(rec, InStr(rec, "analyzerTrapVoltage:") + 20))
         If InStr(rec, "conductanceLimitVoltage:") <> 0 Then Climit = val(Mid(rec, InStr(rec, "conductanceLimitVoltage:") + 24))
         If InStr(rec, "sourceTrapVoltage:") <> 0 Then Strap = val(Mid(rec, InStr(rec, "sourceTrapVoltage:") + 18))
         If InStr(rec, "calType:") <> 0 Then CalType = val(Mid(rec, InStr(rec, "calType:") + 8))
         If InStr(rec, "calReferenceFrequency:") <> 0 Then ps.LowMassFreq = val(Mid(rec, InStr(rec, "calReferenceFrequency:") + 22))
         If CalType = 1 Or CalType = 2 Then
            If InStr(rec, "calC0:") <> 0 Then ps.A = val(Mid(rec, InStr(rec, "calC0:") + 6))
            If InStr(rec, "calC1:") <> 0 Then ps.b = val(Mid(rec, InStr(rec, "calC1:") + 6))
            If InStr(rec, "calC2:") <> 0 Then ps.c = val(Mid(rec, InStr(rec, "calC2:") + 6))
         End If
         If CalType = 3 Then
            If InStr(rec, "calC0:") <> 0 Then ps.c = val(Mid(rec, InStr(rec, "calC0:") + 6))
            If InStr(rec, "calC1:") <> 0 Then ps.A = val(Mid(rec, InStr(rec, "calC1:") + 6))
            If InStr(rec, "calC2:") <> 0 Then ps.b = val(Mid(rec, InStr(rec, "calC2:") + 6))
         End If
         If InStr(Start, rec, "chirpStartFrequency:") <> 0 Then ps.LowMassFreq = val(Mid(rec, InStr(Start, rec, "chirpStartFrequency:") + 20))
         If InStr(Start, rec, "detectType:analyzer") <> 0 Then
            ps.b = ps.b * Abs(Atrap)
         Else
            ps.b = ps.b * Abs(Strap)
         End If
         ' FTMS cal types:
         '   1  m/z = A/f + |Vt|B/f^2
         '   3  m/z = A/f + |Vt|B/f^2 + C
         '   2  m/z = A/f + |Vt|B/f^2 + I|Vt|C/f^2
         ps.CalType = CalType
         If CalType = 2 Then
'            Call ErrorMess.LogError("Warning, type 2 calibration, make sure intensity option is defined in calibration generation dialog!", ps.FileName)
            ps.c = ps.c * Abs(Atrap)
         End If
         If (ps.A = 0#) And (ps.b = 0#) And (ps.c = 0#) Then
            ' no header cal data found, use defaults!
            Call ErrorMess.LogError("No calibration data in file, using defaults...", ps.FileName)
            ps.A = A
            ps.b = b
            ps.c = c
         End If
         If ftype = 1 Then
            ps.Type = 1
         Else
            ps.Type = 3
         End If
         If ftype = 2 Then
            If ZeroFill = True Then
               ps.HugeSize = ps.HugeSize * CLng(2 ^ ZerofillNumber)
            End If
         End If
         Close FileNum
         ' Determin position the file pointer to start of data
         FileNum = FreeFile
         Open ps.FileName For Binary Access Read As FileNum
         Seek FileNum, 11
         Get FileNum, , Pos
         Pos = (((Pos And &HFF00) / 256) And &HFF) Or ((Pos And &HFF) * 256)
         ps.FilePointer = Pos + 64
         Close FileNum
         ps.HugeSizeInitial = ps.HugeSize
         If ps.Allow Then
            Load Parms
            Parms.Show 1
            ps.Allow = False
         End If
         ' Determine the sequence total time...
         Pos = 1
         TotalTime = 0#
         Do
            li = InStr(Pos, rec1, "time:")
            If li = 0 Then Exit Do
            Pos = li + 5 ' Pos + li
            TotalTime = TotalTime + val(Mid(rec1, Pos))
         Loop
         Call LogMess(ps, "Total Sequence Time = " & Format(TotalTime))
      Case 3   ' floats, with header, icr-2ls format
         FileNum = FreeFile
         Open ps.FileName For Input Access Read As FileNum
         ps.appodize = 0
         ps.ZeroFills = 0
         Do
            Input #FileNum, rec
            If EOF(FileNum) = True Then Exit Do
            If rec = "End" <> 0 Then Exit Do
            If InStr(rec, "Comment:") <> 0 Then
               i = Seek(FileNum)
               Do
                  Input #FileNum, rec
                  If InStr(rec, "CommentEnd") <> 0 Then
                     j = Seek(FileNum) - 12
                     Seek FileNum, i
                     ps.Comment = Input(j - i, #FileNum)
                     Seek FileNum, j + 12
                     Exit Do
                  End If
               Loop
            End If
            If InStr(rec, "FileType:") <> 0 Then ps.Type = val(Mid(rec, InStr(rec, "FileType:") + 9))
            If InStr(rec, "DataType:") <> 0 Then ps.FileType = val(Mid(rec, InStr(rec, "DataType:") + 9))
            If InStr(rec, "NumberOfSamples:") <> 0 Then ps.HugeSize = val(Mid(rec, InStr(rec, "NumberOfSamples:") + 16))
            If InStr(rec, "SampleRate:") <> 0 Then ps.SampleRate = val(Mid(rec, InStr(rec, "SampleRate:") + 11))
            If InStr(rec, "LowMassFreq:") <> 0 Then ps.LowMassFreq = val(Mid(rec, InStr(rec, "LowMassFreq:") + 12))
            If InStr(rec, "FreqShift:") <> 0 Then ps.FreqShift = val(Mid(rec, InStr(rec, "FreqShift:") + 10))
            If InStr(rec, "NumberSegments:") <> 0 Then ps.NumSegments = val(Mid(rec, InStr(rec, "NumberSegments:") + 15))
            If InStr(rec, "MaxPoint:") <> 0 Then ps.MaxPoint = val(Mid(rec, InStr(rec, "MaxPoint:") + 9))
            If InStr(rec, "CalType:") <> 0 Then ps.CalType = val(Mid(rec, InStr(rec, "CalType:") + 8))
            If InStr(rec, "CalA:") <> 0 Then ps.A = val(Mid(rec, InStr(rec, "CalA:") + 5))
            If InStr(rec, "CalB:") <> 0 Then ps.b = val(Mid(rec, InStr(rec, "CalB:") + 5))
            If InStr(rec, "CalC:") <> 0 Then ps.c = val(Mid(rec, InStr(rec, "CalC:") + 5))
            If InStr(rec, "Intensity:") <> 0 Then ps.Intensity = val(Mid(rec, InStr(rec, "Intensity:") + 10))
            If InStr(rec, "Appodization:") <> 0 Then ps.appodize = val(Mid(rec, InStr(rec, "Appodization:") + 13))
            If InStr(rec, "ZeroFills:") <> 0 Then ps.ZeroFills = val(Mid(rec, InStr(rec, "ZeroFills:") + 10))
            If InStr(rec, "Xtitle:") <> 0 Then ps.XaxisName = Mid(rec, InStr(rec, "Xtitle:") + 7)
            If InStr(rec, "Ytitle:") <> 0 Then ps.YaxisName = Mid(rec, InStr(rec, "Ytitle:") + 7)
            If InStr(rec, "CurrentXmin:") <> 0 Then ps.Xmin = val(Mid(rec, InStr(rec, "CurrentXmin:") + 12))
            If InStr(rec, "CurrentXmax:") <> 0 Then ps.Xmax = val(Mid(rec, InStr(rec, "CurrentXmax:") + 12))
         Loop
         ps.HugeSizeInitial = ps.HugeSize / (2 ^ ps.ZeroFills)
         If (ps.A = 0#) And (ps.b = 0#) And (ps.c = 0#) Then
            ' no header cal data found, use defaults!
            Ans = String(10, " ")
            Call GetPrivateProfileString("Scope", "NoCalWarning", "N", Ans, 10, "icr-2ls.ini")
            If (InStr(Ans, "N") = 0) Then MsgBox "No calibration data in file, using defaults...", 48, "Warning"
            ps.A = A
            ps.b = b
            ps.c = c
         End If
         ps.FilePointer = Seek(FileNum)
         Close FileNum
         If ps.Allow Then
            Load Parms
            Parms.Show 1
            ps.Allow = False
         End If
      Case 5   ' ascii time, no header
         Load Parms
         Parms.Show 1
         ' ps.Type = 1
         ReadHeader = 0
         ps.HugeSizeInitial = ps.HugeSize
      Case 6   ' midas file format
         FileNum = FreeFile
         Open ps.FileName For Binary Access Read As FileNum
         ps.appodize = 0
         ps.ZeroFills = 0
         ps.Type = 1
         ps.FileType = 6
         ps.LowMassFreq = 0#
         ps.FreqShift = 0#
         ps.NumSegments = 0
         ps.appodize = 0
         ps.ZeroFills = 0
         Do
            rec = ReadString(FileNum)
            If EOF(FileNum) = True Then Exit Do
            If InStr(rec, "Data:") <> 0 Then Exit Do
            If InStr(rec, "Data Points:") <> 0 Then ps.HugeSize = val(Mid(rec, InStr(rec, "Data Points:") + 12))
            If InStr(rec, "Bandwidth:") <> 0 Then ps.SampleRate = val(Mid(rec, InStr(rec, "Bandwidth:") + 10)) * 2#
            If InStr(rec, "Coeff0:") <> 0 Then ps.A = val(Mid(rec, InStr(rec, "Coeff0:") + 7))
            If InStr(rec, "Coeff1:") <> 0 Then ps.b = val(Mid(rec, InStr(rec, "Coeff1:") + 7))
            If InStr(rec, "Coeff2:") <> 0 Then ps.c = val(Mid(rec, InStr(rec, "Coeff2:") + 7))
            If InStr(rec, "dreferencefreq:") <> 0 Then ps.LowMassFreq = val(Mid(rec, InStr(rec, "dreferencefreq:") + 15))
            If InStr(rec, "Storage Type: int") <> 0 Then ps.FileType = 9
         Loop
         ps.HugeSizeInitial = ps.HugeSize / (2 ^ ps.ZeroFills)
         If (ps.A = 0#) And (ps.b = 0#) And (ps.c = 0#) Then
            ' no header cal data found, use defaults!
            MsgBox "No calibration data in file, using defaults...", 48, "Warning"
            ps.A = A
            ps.b = b
            ps.c = c
         End If
         ps.FilePointer = Seek(FileNum) - 1
         Close FileNum
         If ps.Allow Then
            ps.Allow = False
            Load Parms
            Parms.Show 1
         End If
      Case 7  ' IonSpec
         FileNum = FreeFile
         Open ps.FileName For Binary Access Read As FileNum
         ' Test for version 6 or 7
         Seek FileNum, 5
         Get FileNum, , Ft
         If Ft = 6 Or Ft = 7 Then
            ' Here if its version 6 or 7, now read the header...
            Seek FileNum, 1
            Get FileNum, 1, newExperiment
            ps.HugeSize = newExperiment.TransientPoints
            ps.A = newExperiment.CalCstA
            ps.b = newExperiment.CalCstB
            ps.c = 0#
            ps.CalType = 6
            ps.Type = 1
            ps.SampleRate = newExperiment.AdcRate
            ps.LowMassFreq = 0#
            If newExperiment.DetectMode <> 0 Then
               ps.LowMassFreq = newExperiment.MixerFrequency
            End If
            Get FileNum, , Ft
            ISH.ftype = Ft
            ps.FilePointer = Loc(FileNum)
         Else
         ' determine the type of ionspec data file. 3.3 5.xx
         Seek FileNum, 1
         Get FileNum, , Fti
         Seek FileNum, 1
         Get FileNum, , Ft
         If (Ft = 61089) Then
            Seek FileNum, 5
            Get FileNum, , ISH.revmaj
            Seek FileNum, 9
            Get FileNum, , ISH.revmin
            If ISH.revmaj = 5 Then
               Seek FileNum, 2497
               Get FileNum, , ps.HugeSize
               Seek FileNum, 857
               Get FileNum, , ps.SampleRate
               Seek FileNum, 825
               Get FileNum, , ISH.hmode
               If (ISH.hmode <> 1) Then
                  ps.LowMassFreq = 0#
               Else
                  Seek FileNum, 849
                  Get FileNum, , ps.LowMassFreq
                  ps.LowMassFreq = Mass2Freq(ps, ps.LowMassFreq)
               End If
               ps.A = A
               ps.b = b
               ps.c = c
               Seek FileNum, 22841
               Get FileNum, , ISH.ftype
               ps.FilePointer = 22844
               ps.Type = 1
            End If
            If ISH.revmaj = 6 Then
               Seek FileNum, 1677
               Get FileNum, , ps.HugeSize
               Seek FileNum, 865
               Get FileNum, , ps.SampleRate
               Seek FileNum, 825
               Get FileNum, , ISH.hmode
               If (ISH.hmode <> 1) Then
                  ps.LowMassFreq = 0#
               Else
                  Seek FileNum, 857
                  Get FileNum, , ps.LowMassFreq
               End If
               Seek FileNum, 1709
               Get FileNum, , ps.A
               Seek FileNum, 1717
               Get FileNum, , ps.b
               'ps.A = A
               'ps.b = b
               ps.c = -2
               Seek FileNum, 21997
               Get FileNum, , ISH.ftype
               ps.FilePointer = 22000
               ps.Type = 1
            End If
         ElseIf Fti = 6400 Then
            Seek FileNum, 1805
            Get FileNum, , ps.HugeSize
            Seek FileNum, 1837
            Get FileNum, , ps.A
            Get FileNum, , ps.b
            ps.CalType = 6
            Seek FileNum, 993
            Get FileNum, , ps.SampleRate
            ps.LowMassFreq = 0
            ps.FreqShift = 0
            ps.HugeSizeInitial = ps.HugeSize
            ps.FilePointer = 22128
            ISH.ftype = 16
            ps.Type = 1
         Else ' must be a 3.3 ionspec file
            Seek FileNum, 1
            Get FileNum, , ISH
            ps.HugeSize = ISH.naq
            ps.SampleRate = ISH.adcr
            ps.LowMassFreq = ISH.lmfrq
            If (ISH.hmode <> 1) Then ps.LowMassFreq = 0#
            ps.A = ISH.A
            ps.b = ISH.b
            ps.c = ISH.c
            ps.FilePointer = Len(ISH) - 1
            ps.Type = 1
         End If
         End If
         Close FileNum
      Case ftBRUKER
         ' open the file and load the calibration data and the
         ' data acquisition parameters
         FileNum = FreeFile
         Open ps.FileName For Binary Access Read As FileNum
         ps.appodize = 0
         ps.ZeroFills = 0
         ps.Type = 1
         ps.FileType = ftBRUKER
         ps.LowMassFreq = 0#
         ps.FreqShift = 0#
         ps.NumSegments = 0
         ps.appodize = 0
         ps.ZeroFills = 0
         ps.c = 0 ' -2#
         ps.CalType = 6
         ps.ByteOrder = 1
         Do
            rec = ReadString(FileNum)
            If EOF(FileNum) = True Then Exit Do
            If InStr(rec, "##$ML1= ") <> 0 Then ps.A = val(Mid(rec, InStr(rec, "##$ML1= ") + 8))
            If InStr(rec, "##$ML2= ") <> 0 Then ps.b = -val(Mid(rec, InStr(rec, "##$ML2= ") + 8))
            If InStr(rec, "##$SW_h= ") <> 0 Then ps.SampleRate = val(Mid(rec, InStr(rec, "##$SW_h= ") + 9)) * 2#
            If InStr(rec, "##$BYTORDA= ") <> 0 Then ps.ByteOrder = val(Mid(rec, InStr(rec, "##$BYTORDA= ") + 12))
            If InStr(rec, "##$TD= ") <> 0 Then
               ps.HugeSize = val(Mid(rec, InStr(rec, "##$TD= ") + 7))
'               ps.HugeSize = 2 ^ Int(Log(CSng(ps.HugeSize)) / Log(2) + 0.9)
            End If
            If InStr(rec, "##$FR_low= ") <> 0 Then ps.FreqShift = val(Mid(rec, InStr(rec, "##$FR_low= ") + 11))
            If ps.SampleRate > ps.FreqShift Then ps.FreqShift = 0#
         Loop
         ps.HugeSizeInitial = ps.HugeSize
         ps.FilePointer = 0
         Close FileNum
   End Select
   If ps.XaxisName = "" Then
      If ps.Type = 1 Then
         ps.XaxisName = "Time (sec)"
      End If
      If ps.Type = 2 Then
         ps.XaxisName = "Frequency (Hz)"
      End If
      If ps.Type = 3 Then
         ps.XaxisName = "m/z"
      End If
   End If
   ReadHeader = ps.FilePointer
End Function
Function ReadString(FileNum As Integer, Optional Delimiter As Byte = 10) As String
   Dim Res As String
   Dim byt As Byte
   
   Res = ""
   Do
      Get FileNum, , byt
      If byt = Delimiter Then Exit Do
      If (byt <> 10) And (byt <> 13) Then
         Res = Res & Chr$(byt)
      End If
      If (EOF(FileNum)) Then Exit Do
   Loop
   ReadString = Res
End Function


Function ReadSunFileByte(FileName As String, Pos As Long) As Byte
Dim FileNum As Integer
Dim bval As Byte

   FileNum = FreeFile
   Open FileName For Binary Access Read As FileNum
   Seek FileNum, Pos
   Get FileNum, , bval
   Close FileNum
   ReadSunFileByte = bval
End Function

Function ReadSunFileLong(FileName As String, Pos As Long) As Long
Dim lval As Long
Dim temp As Long
Dim i As Integer

   lval = 0
   For i = 0 To 3
      lval = lval * 256
      temp = ReadSunFileByte(FileName, Pos + i)
      lval = lval + temp
   Next i
   ReadSunFileLong = lval
End Function
Function ReadSunFileInt(FileName As String, Pos As Long) As Long
Dim ival As Long
Dim temp As Long
Dim i As Integer

   ival = 0
   For i = 0 To 1
      ival = ival * 256
      temp = ReadSunFileByte(FileName, Pos + i)
      ival = ival + temp
   Next i
   ReadSunFileInt = ival
End Function

Function RotateText(O As Object, hdc As Long, Tag As String, angle As Integer, X As Single, Y As Single, TwipsPerPixelX As Single, TwipsPerPixelY As Single) As Integer
   Dim oldhdc As Long, dl As Long
   Dim FonttoUse As Long, hPrintDc As Long
   Dim tm As TEXTMETRIC
   Dim iStat As Integer
   Dim di As DOCINFO

   On Error Resume Next
   FonttoUse = CreateFontObject(O, angle, CLng(TwipsPerPixelX))
   If FonttoUse = 0 Then Exit Function
   oldhdc = SelectObject(hdc, FonttoUse)
   dl = GetTextMetrics(O.hdc, tm)
   If (tm.tmPitchAndFamily And TMPF_TRUETYPE) <> TMPF_TRUETYPE Then
      iStat = -1
   Else
      iStat = 0
      If (O.DeviceName = Printer.DeviceName) Then
         If Error$ = "" Then
            RotateText = -1
            Exit Function
            hPrintDc = CreateDC(O.DeviceName, O.DeviceName, 0, 0)
            di.cbSize = 20
            di.fwType = 0
            dl = StartDoc(hPrintDc, di)
            dl = StartPage(hPrintDc)
            dl = SelectObject(hPrintDc, FonttoUse)
            dl = TextOut(hPrintDc, X / TwipsPerPixelX, Y / TwipsPerPixelY, Tag, Len(Tag))
            dl = EndPage(hPrintDc)
            dl = EndDoc(hPrintDc)
            Call DeleteDC(hPrintDc)
         Else
            dl = TextOut(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY, Tag, Len(Tag))
         End If
      Else
         dl = TextOut(hdc, X / TwipsPerPixelX, Y / TwipsPerPixelY, Tag, Len(Tag))
      End If
   End If
   dl = SelectObject(hdc, oldhdc)
   dl = DeleteObject(FonttoUse)
   RotateText = iStat
End Function

Function SaveAll(FileName As String, Comment As String) As Integer
   Dim FileNum As Integer
   Dim i As Integer
   Dim FileH As Integer
   Dim Pos As Long
   Dim li As Long
   Dim str As String
   
   FileNum = FreeFile
   Open FileName For Binary Access Write As FileNum
   ' File header stuff...
   str = "ICR-2LS, Save all data file, " & version
   i = Len(str)
   Put FileNum, , i
   Put FileNum, , str
   i = Len(Comment)
   Put FileNum, , i
   Put FileNum, , Comment
   ' First save all of the plot structures...
   For i = 0 To MAXSCOPES - 1
      Put FileNum, , Plots(i)
   Next i
   ' Now save all data...
   Pos = Seek(FileNum)
   Close FileNum
   FileH = lopen(FileName, OF_READWRITE)
   For i = 0 To MAXSCOPES - 1
      If Plots(i).InUse Then
         li = HugeSaveFloat(Plots(i).harray, Plots(i).HugeSize, FileH, Pos)
         Pos = Pos + Plots(i).HugeSize * 4
      End If
   Next i
   lclose (FileH)
   Open FileName For Binary Access Write As FileNum
   Seek FileNum, Pos
   For i = 0 To MAXSCOPES - 1
      If Plots(i).InUse Then
         Scopes(i).SaveState (FileNum)
      End If
   Next i
   Put FileNum, , pk
   Put FileNum, , AxisSetup
   For i = 0 To NUMMACROS - 1
      Put FileNum, , Macs(i)
   Next i
   Put #FileNum, , ExciteData
   Put #FileNum, , SysDirs
   For i = 0 To 3
      Put FileNum, , H(i)
   Next i
   Close FileNum
End Function

Sub SaveAsLast(ps As PlotStructure)
   Dim i As Integer
   
   If ps.LastLevel < 5 Then
      ps.LastXmax(ps.LastLevel) = ps.Xmax
      ps.LastXmin(ps.LastLevel) = ps.Xmin
      ps.LastYmax(ps.LastLevel) = ps.Ymax
      ps.LastYmin(ps.LastLevel) = ps.Ymin
      ps.LastLevel = ps.LastLevel + 1
   Else
      For i = 0 To ps.LastLevel - 2
         ps.LastXmax(i) = ps.LastXmax(i + 1)
         ps.LastXmin(i) = ps.LastXmin(i + 1)
         ps.LastYmax(i) = ps.LastYmax(i + 1)
         ps.LastYmin(i) = ps.LastYmin(i + 1)
         ps.LastXmax(ps.LastLevel - 1) = ps.Xmax
         ps.LastXmin(ps.LastLevel - 1) = ps.Xmin
         ps.LastYmax(ps.LastLevel - 1) = ps.Ymax
         ps.LastYmin(ps.LastLevel - 1) = ps.Ymin
      Next i
   End If
End Sub

Sub SaveDefaults()
   Dim FileNum As Integer
   Dim i As Integer
   
   On Error GoTo SaveDefaultsError
'   ChDir SysDirs.StartupDir
'   ChDrive SysDirs.StartupDir
   ChDir HomeDir
   ChDrive HomeDir
   FileNum = FreeFile
   Open "Default.cfg" For Binary Access Write As FileNum
   Put FileNum, , AxisSetup
   For i = 0 To NUMMACROS - 1
      Put FileNum, , Macs(i)
   Next i
   Put #FileNum, , ExciteData(0)
   Put #FileNum, , ExciteData(1)
   Put #FileNum, , SysDirs
   For i = 0 To 3
      Put FileNum, , H(i)
   Next i
   Put #FileNum, , CInt(Len(CStr(GenCal.txtStartMZ)))
   Put #FileNum, , CStr(GenCal.txtStartMZ)
   Put #FileNum, , CInt(Len(CStr(GenCal.txtStopMZ)))
   Put #FileNum, , CStr(GenCal.txtStopMZ)
   Put #FileNum, , CInt(Len(CStr(GenCal.txtThreshold)))
   Put #FileNum, , CStr(GenCal.txtThreshold)
   Put #FileNum, , CInt(Len(CStr(GenCal.optTIC)))
   Put #FileNum, , CStr(GenCal.optTIC)
   Put #FileNum, , CInt(Len(CStr(GenCal.optPeaks)))
   Put #FileNum, , CStr(GenCal.optPeaks)
   Close FileNum
   Call LoadScope.SaveUserData(LoadScope.UserName)
   Exit Sub
SaveDefaultsError:
   Close FileNum
   Exit Sub
End Sub

Sub SetFont(fntyp As FN)
   On Error GoTo SetFontError
   ActiveForm.FontName = fntyp.FNname
   ActiveForm.FontSize = fntyp.FNSize
   If fntyp.FNbold <> 0 Then
      ActiveForm.FontBold = True
   Else
      ActiveForm.FontBold = False
   End If
   If fntyp.FNitalic <> 0 Then
      ActiveForm.FontItalic = True
   Else
      ActiveForm.FontItalic = False
   End If
   Exit Sub
SetFontError:
'   MsgBox Error$, 48, "Error"
   Resume Next
End Sub

Sub SetPictureFont(p As Control, fntyp As FN)
   On Error GoTo SetPictureFontError
   p.FontName = fntyp.FNname
   p.FontSize = fntyp.FNSize
   If fntyp.FNbold <> 0 Then
      p.FontBold = True
   Else
      p.FontBold = False
   End If
   If fntyp.FNitalic <> 0 Then
      p.FontItalic = True
   Else
      p.FontItalic = False
   End If
   Exit Sub
SetPictureFontError:
   Resume Next
End Sub

Sub SetPrinterFont(fntyp As FN)

   On Error GoTo SetPrinterFontError
   Printer.FontName = fntyp.FNname
   Printer.FontSize = fntyp.FNSize
   If fntyp.FNbold <> 0 Then
      Printer.FontBold = True
   Else
      Printer.FontBold = False
   End If
   If fntyp.FNitalic <> 0 Then
      Printer.FontItalic = True
   Else
      Printer.FontItalic = False
   End If
   Exit Sub
SetPrinterFontError:
'   MsgBox Error$, 48, "Error"
   Resume Next
End Sub

Sub SetScopeDefaults(ps As PlotStructure)
   ps.FileName = ""
   ps.MaxPoint = 0
   ps.CalType = 0
   ps.Intensity = 0
   ps.XaxisZoom = 1
   ps.YaxisZoom = False
   ps.YaxisFixed = False
   ps.YaxisAutoScale = True
   ps.NoYaxisNumbers = False
   ps.NoXaxisNumbers = False
   ps.NoYaxis = False
   ps.ZeroFills = 0
   ps.appodize = 0
   ps.NumYTicks = 3
   ps.NumXTicks = 4
   ps.Comment = ""
   ps.NumSegments = 0
End Sub

Sub SmoothPeaks(ps As PlotStructure, Dstart As Single, Dstop As Single)
   Dim fVal As Single, Sum As Single
   Dim Lstop As Long
   Dim Lstart As Long
   Dim temp As Long
   Dim li As Long, lj As Long, le As Long, ls As Long
   Dim iStat As Integer
   Dim harray As Long
   Dim Y(1000) As Single, Ymax As Single, Ymin As Single
   Dim n As Long

   fVal = 0#
   Lstart = FindIndex(ps, Dstart)
   Lstop = FindIndex(ps, Dstop)
   If Lstart > Lstop Then
      temp = Lstop
      Lstop = Lstart
      Lstart = temp
   End If
   ' Get a temporary buffer
   iStat = HugeDim(harray, 4, ps.HugeSize)
   If (iStat <> 0) Then
      MsgBox ("ERROR! can't allocate memory. " + Format(iStat))
    Exit Sub
   End If
  Call CopyData(ps.harray, harray, 0, ps.HugeSize, ps.HugeSize)
  For li = Lstart To Lstop - 3
      ls = li + 1
      le = li + 2
      If ls < Lstart Then ls = Lstart
      If le > Lstop Then le = Lstop
      If ps.Type = 1 Then
         n = HugeExtract(ps.harray, Y(0), Ymax, Ymin, li, le + 1, 1000, 1)
      Else
         n = HugeExtract(ps.harray, Y(0), Ymax, Ymin, li, le + 1, 1000, 4)
         n = HugeExtract(ps.harray, Y(0), Ymax, Ymin, li, le + 1, 1000, 3)
      End If
'      Sum = 0#
'      For lj = 0 To N - 1
'         Sum = Sum + Y(lj)
'      Next lj
'      Sum = Sum / N
        Sum = (2 * Y(1) + Y(0) + Y(2)) / 4
      
      If ps.Type = 1 Then
         iStat = SetHugeEl(harray, 4, ls, Sum)
      Else
         iStat = SetHugeEl(harray, 4, ls * 2, Sum)
         iStat = SetHugeEl(harray, 4, (ls * 2) + 1, 0#)
      End If
   Next li
   Call CopyData(harray, ps.harray, 0, ps.HugeSize, ps.HugeSize)
   iStat = HugeErase(harray)
End Sub
Sub MisMatchedSpectraMath(PI As Integer, CPI As Integer, key As Integer, Optional adjust As Single = 1#)
Dim li As Long
Dim r(2) As Single, V1 As Single

   For li = 0 To (Plots(PI).HugeSize / 2) - 1
      Select Case key
         Case 43   ' + addition
            r(0) = Scopes(PI).GetYvalue(Xvalue(Plots(PI), li)) + adjust * Scopes(CPI).GetYvalue(Xvalue(Plots(PI), li))
            r(1) = 0
         Case 45   ' - subtraction
            r(0) = Scopes(PI).GetYvalue(Xvalue(Plots(PI), li)) - adjust * Scopes(CPI).GetYvalue(Xvalue(Plots(PI), li))
            r(1) = 0
         Case 42   ' * multiplication
            r(0) = Scopes(PI).GetYvalue(Xvalue(Plots(PI), li)) * adjust * Scopes(CPI).GetYvalue(Xvalue(Plots(PI), li))
            r(1) = 0
         Case 47   ' / division
            V1 = adjust * Scopes(CPI).GetYvalue(Xvalue(Plots(PI), li))
            If (V1 <> 0#) Then
               r(0) = Scopes(PI).GetYvalue(Xvalue(Plots(PI), li)) / V1
            Else
               r(0) = 0#
            End If
            r(1) = 0
      End Select
      Call SetHugeEl(Plots(PI).harray, 8, li, r(0))
   Next
End Sub
Sub SpectraMath(PI As Integer, key As Integer)
   Dim iStat As Integer
   Dim li As Long
   Dim Num As Integer, i As Integer
   Dim X(1024) As Single
   Dim Y(1024) As Single
   Dim r(1024) As Single, M As Double, M1 As Double
   Dim CPI As Integer
   Dim rarray As Long
   
   For i = 0 To UBound(Scopes)
      If (Plots(i).harray = CopyPlot.harray) Then
         CPI = i
         Exit For
      End If
   Next
   If CopyPlot.InUse = 0 Then Exit Sub
   If CopyPlot.Type <> Plots(PI).Type Then Exit Sub
   If (CopyPlot.Type = 3) Then
      If (Scopes(PI).GetYvalue(Xvalue(Plots(PI), 0)) <> Scopes(CPI).GetYvalue(Xvalue(Plots(CPI), 0))) Then
         Call MisMatchedSpectraMath(PI, CPI, key)
         Exit Sub
      End If
      If (Scopes(PI).GetYvalue(Xvalue(Plots(PI), Plots(PI).HugeSize / 2)) <> Scopes(CPI).GetYvalue(Xvalue(Plots(CPI), Plots(CPI).HugeSize / 2))) Then
         Call MisMatchedSpectraMath(PI, CPI, key)
         Exit Sub
      End If
   End If
   If CopyPlot.HugeSize <> Plots(PI).HugeSize Then Exit Sub
   Num = 1024
   If CopyPlot.HugeSize < Num Then Num = 1
   For li = 0 To CopyPlot.HugeSize - 1 Step Num
      iStat = GetHugeEl(Plots(PI).harray, 4 * Num, li / Num, X(0))
      iStat = GetHugeEl(CopyPlot.harray, 4 * Num, li / Num, Y(0))
      Select Case key
         Case 43   ' + addition
            For i = 0 To Num - 1
               r(i) = X(i) + Y(i)
            Next i
         Case 45   ' - subtraction
            For i = 0 To Num - 1
               r(i) = X(i) - Y(i)
            Next i
         Case 42   ' * multiplication
            For i = 0 To Num - 1
               r(i) = X(i) * Y(i)
            Next i
         Case 35   ' Optimal filter
            For i = 0 To Num - 2 Step 2
               r(i) = (X(i) * Y(i) - X(i + 1) * Y(i + 1))
               r(i + 1) = (X(i + 1) * Y(i) + X(i) * Y(i + 1))
            Next
         Case 47   ' / division
            For i = 0 To Num - 1
               If (Y(i) > 0#) Then
                  r(i) = X(i) / Y(i)
               Else
                  r(i) = 0
               End If
            Next i
      End Select
      iStat = SetHugeEl(Plots(PI).harray, 4 * Num, li / Num, r(0))
   Next li
   iStat = ExtractPlotData(Plots(PI), Plots(PI).Xmin, Plots(PI).Xmax)
End Sub

Function sprint(fVal As Single, fmt As SF) As String
   Dim fmtstr As String
   Dim Length As Integer
   Dim i As Integer
   Dim pl As Integer

   For i = 0 To fmt.NumDigits - 1
      If i = 0 Then
         fmtstr = "0."
      Else
         fmtstr = fmtstr + "#"
      End If
   Next i
   If fmt.Typ = 0 Then
      fmtstr = fmtstr + "e+##"
      sprint = Format$(fVal, fmtstr)
   Else
      fmtstr = Format$(fVal, fmtstr)
      Length = fmt.NumDigits
      If fVal < 0# Then Length = Length + 1
      pl = InStr(fmtstr, ".")
      If pl > Length Then
         sprint = Left$(fmtstr, pl - 1)
         Exit Function
      End If
      If Length = 0 Then
         sprint = fmtstr
      Else
         sprint = Left$(fmtstr, Length + 1)
      End If
   End If
End Function

Sub SwapWithLast(ps As PlotStructure)
   
   If ps.LastLevel > 0 Then
      ps.Xmax = ps.LastXmax(ps.LastLevel - 1)
      ps.Xmin = ps.LastXmin(ps.LastLevel - 1)
      ps.Ymax = ps.LastYmax(ps.LastLevel - 1)
      ps.Ymin = ps.LastYmin(ps.LastLevel - 1)
      ps.LastLevel = ps.LastLevel - 1
   End If
End Sub

Sub WriteHeader(ps As PlotStructure, FileName As String)
   Dim FileNum As Integer, i As Integer
   Dim TagText As String
   Dim TagPos As String
   
   On Error Resume Next
   FileNum = FreeFile
   Open FileName For Output Access Write As FileNum
   Print #FileNum, "ICR-2LS Data File,  GA Anderson & JE Bruce"
   Print #FileNum, version
   Print #FileNum, "FileName:"; ps.FileName
   Print #FileNum, "title:"; ps.Title
   Print #FileNum, "Ytitle:"; ps.YaxisName
   Print #FileNum, "Xtitle:"; ps.XaxisName
   Print #FileNum, "Comment:"
   Print #FileNum, ps.Comment
   Print #FileNum, "CommentEnd"
   Print #FileNum, "FileType:"; ps.Type
   Print #FileNum, " ValidTypes:1=Time,2=Freq,3=Mass"
   Print #FileNum, "DataType:"; ps.FileType
   Print #FileNum, " ValidTypes:1=Integer,no header,2=Floats,Sun Extrel,3=Floats with header,4=Excite waveform"
   Print #FileNum, "Appodization:"; ps.appodize
   Print #FileNum, " ValidFunctions:0=Square,1=Parzen,2=Hanning,3=Welch"
   Print #FileNum, "ZeroFills:"; ps.ZeroFills
   Print #FileNum, "NumberOfSamples:"; ps.HugeSize
   Print #FileNum, "SampleRate:"; ps.SampleRate
   Print #FileNum, "LowMassFreq:"; ps.LowMassFreq
   Print #FileNum, "FreqShift:"; ps.FreqShift
   Print #FileNum, "NumberSegments:"; ps.NumSegments
   Print #FileNum, "MaxPoint:"; ps.MaxPoint
   Print #FileNum, "CalType:"; ps.CalType
   Print #FileNum, "CalA:"; ps.A
   Print #FileNum, "CalB:"; ps.b
   Print #FileNum, "CalC:"; ps.c
   Print #FileNum, "Intensity:"; ps.Intensity
   Print #FileNum, "CurrentXmin:"; ps.Xmin
   Print #FileNum, "CurrentXmax:"; ps.Xmax
   Print #FileNum, "Tags:"
   i = 1
   Do
      TagText = Scopes(ActivePlot).Ptag(i)
      If Error <> "" Then Exit Do
      TagPos = Scopes(ActivePlot).Ptag(i).Tag
      Print #FileNum, TagText; ","; TagPos
      i = i + 1
   Loop
   Print #FileNum, "TagsEnd"
   Print #FileNum, "End"
   Print #FileNum, Chr(27);
   Close FileNum
End Sub
' Converts X value to cursor position
Function X2cursor(X As Double, ps As PlotStructure) As Integer
   Dim temp As Single

   On Error Resume Next
   temp = (X - ps.Xmin) / (ps.Xmax - ps.Xmin)
   temp = temp * (ps.MaxX - ps.OriginX) + ps.OriginX
   If temp < 0 Then temp = 0
   If temp < 32767 Then
      X2cursor = CInt(temp)
   Else
      X2cursor = 32767
   End If
End Function

' Converts cursor position to x axis value
Function Xcursor(X As Integer, ps As PlotStructure) As Double
   Dim temp As Double

   temp = (X - ps.OriginX) / (ps.MaxX - ps.OriginX)
   Xcursor = temp * CDbl(ps.Xmax - ps.Xmin) + CDbl(ps.Xmin)
End Function
Function Xvalue(ps As PlotStructure, ByVal Index As Long) As Double
Dim fVal As Single
Dim i As Long

   If ps.harray = 0 Then Exit Function
   If Index < 0 Then Index = 0
   Select Case ps.Type
      Case 1 ' Time
         If Index >= ps.HugeSize Then Index = ps.HugeSize - 1
         Xvalue = Index / ps.SampleRate
      Case 4 ' Time series with calibration function applied
         If Index >= ps.HugeSize Then Index = ps.HugeSize - 1
         Xvalue = Time2Value(ps, CDbl(Index / ps.SampleRate))
      Case 2 ' Freq
         If Index >= ps.HugeSize / 2 Then Index = ps.HugeSize / 2 - 1
         If (ps.LowMassFreq = 0) Then
            Xvalue = ps.FreqShift + (Index * ps.SampleRate) / (ps.HugeSize)
         Else
            Xvalue = ps.LowMassFreq - ps.FreqShift - (Index * ps.SampleRate) / (ps.HugeSize)
         End If
      Case 3 ' Mass
         If ps.CalType = 2 Or ps.CalType = 7 Then
            If GenCal.optEachPeak Then
               ps.Intensity = Yvalue(ps, Index)
            End If
         End If
         If Index >= ps.HugeSize / 2 Then Index = ps.HugeSize / 2 - 1
         If (ps.LowMassFreq = 0) Then
            Xvalue = Freq2Mass(ps, ps.FreqShift + (Index * ps.SampleRate) / (ps.HugeSize))
            If Xvalue < 0# Then
               Xvalue = Freq2Mass(ps, (Index * ps.SampleRate) / (ps.HugeSize))
               If Xvalue < 0# Then Xvalue = 0
            End If
         Else
            Xvalue = Freq2Mass(ps, ps.LowMassFreq - ps.FreqShift - (Index * ps.SampleRate) / (ps.HugeSize))
            If Xvalue < 0# Then Xvalue = 0#
         End If
      Case 5
         Call GetHugeEl(ps.harray, 4, Index * 2, fVal)
         ' MsgBox "Xvalue " & Format(Index) & ", " & Format(fVal)
         Xvalue = fVal
   End Select
End Function

' Converts Y value to cursor position
Function Y2cursor(Y As Double, ps As PlotStructure) As Integer
   Dim temp As Single

   On Error Resume Next
   temp = (Y - ps.Ymin) / (ps.Ymax - ps.Ymin)
   temp = temp * (ps.MaxY - ps.OriginY) + ps.OriginY
   If temp < 0 Then temp = 0
   If temp < 32767 Then
      Y2cursor = CInt(temp)
   Else
      Y2cursor = 32767
   End If
End Function
' Returns the the Y value at the current cursor pos
Function Ycursor(Y As Integer, ps As PlotStructure) As Single
   Dim temp As Single

   temp = (Y - ps.OriginY) / (ps.MaxY - ps.OriginY)
   Ycursor = temp * (ps.Ymax - ps.Ymin) + ps.Ymin
End Function
Function YvalueAverage(ps As PlotStructure, ByVal i As Long, Num As Integer) As Single
Dim j As Long
Dim Sum As Single

   If Num <= 1 Then
      YvalueAverage = Yvalue(ps, i)
      Exit Function
   End If
   Sum = 0#
   For j = i - Num / 2 To i + Num / 2
      Sum = Sum + Yvalue(ps, j)
   Next
   YvalueAverage = Sum / (Num + 1)
End Function
' Returns the Y value at index position i
Function Yvalue(ps As PlotStructure, ByVal i As Long) As Single
   Dim fVal As Single
   Dim A As Single, b As Single, fVals(2) As Single
   Dim iStat As Integer

   On Error Resume Next
   If i < 0 Then i = 0
   Select Case ps.Type
      Case 1 ' Time
         iStat = GetHugeEl(ps.harray, 4, i, fVal)
         Yvalue = fVal
      Case 4 ' Time
         iStat = GetHugeEl(ps.harray, 4, i, fVal)
         Yvalue = fVal
      Case 2 ' Freq
         iStat = GetHugeEl(ps.harray, 8, i, fVals(0))
         Yvalue = Sqr(fVals(0) * fVals(0) + fVals(1) * fVals(1))
      Case 3 ' Mass
         If (i > (ps.HugeSize / 2) - 1) Then i = (ps.HugeSize / 2) - 1
         iStat = GetHugeEl(ps.harray, 8, i, fVals(0))
         Yvalue = Sqr(fVals(0) * fVals(0) + fVals(1) * fVals(1))
      Case 5 ' X,Y pairs, return Y
         iStat = GetHugeEl(ps.harray, 4, i * 2 + 1, fVals(0))
         Yvalue = fVals(0)
   End Select
End Function

Function ZeroFills(ps As PlotStructure, NumFill As Integer) As Integer
   Dim iStat As Integer
   Dim li As Long
   Dim size As Long
   Dim Num As Long
   Dim i As Integer
   Dim fVal As Single

   If ps.Type = 5 Then Exit Function
   Call LogMess(ps, "Zerofills = " & Format(NumFill))
   MacroRecord ("ZeroFills," + Format$(NumFill))
   iStat = HugeRedim(ps.harray, 4, (2 ^ NumFill) * ps.HugeSize)
   If (iStat < 0) Then
      MsgBox "Error!, can't locate that much memory.", 48, "Error"
      ZeroFills = iStat
      Exit Function
   End If
   size = (2 ^ NumFill) * ps.HugeSize
   ps.HugeSize = size
   ps.ZeroFills = NumFill
   fVal = 0#
   For li = ps.HugeSizeInitial To size - 1 Step 1024
      Num = 1024
      If (li + 1024) > (size) Then
         Num = size - li
      End If
      iStat = HugeFill(ps.harray, 4&, li, Num, fVal)
   Next li
   ZeroFills = 0
End Function

' returns index of new plot structure
Function ZoomTransform(ps As PlotStructure) As Integer
   Dim PI As Integer
   Dim Lstart As Long
   Dim Lstop As Long
   Dim ln As Long
   Dim iStat As Integer

   If ps.Type = 5 Then Exit Function
   MacroRecord ("ZoomTransform")
   MacroEnabled = False
   If ps.Type = 1 Then
      MsgBox "You can not Zoom Transform Time Domain data!", 48, "ERROR"
      Exit Function
   End If
   PI = NextScope()
   If (PI = -1) Then
      MsgBox "No more scopes are avaliable!", 48, "ERROR"
      Exit Function
   End If
   Call CalculateStartStopLen(ps, Lstart, Lstop, ln)
   Call PrepScope(Plots(PI), ps, Lstart, Lstop, ln)
   iStat = Convert2Time(Plots(PI))
   Call Appodization(Plots(PI), Defaults.AppType, Plots(PI).Xmin, Plots(PI).Xmax, False)
   iStat = ZeroFills(Plots(PI), Defaults.NumZeroFills)
   iStat = Convert2Mass(Plots(PI))
   iStat = ExtractPlotData(Plots(PI), Xvalue(Plots(PI), 0&), Xvalue(Plots(PI), Plots(PI).HugeSize))
   Call AllocateScope(PI)
   Scopes(PI).SetFocus
   MacroEnabled = True
   ZoomTransform = PI
End Function

Public Sub PrintRTF(RTF As RichTextBox, LeftMarginWidth As Long, TopMarginHeight, RightMarginWidth, BottomMarginHeight)
      Dim LeftOffset As Long, TopOffset As Long
      Dim LeftMargin As Long, TopMargin As Long
      Dim RightMargin As Long, BottomMargin As Long
      Dim fr As FormatRange
      Dim rcDrawTo As Rect
      Dim rcPage As Rect
      Dim TextLength As Long
      Dim NextCharPosition As Long
      Dim r As Long

      ' Start a print job to get a valid Printer.hDC
      Printer.Print Space(1)
      Printer.ScaleMode = vbTwips

      ' Get the offsett to the printable area on the page in twips
      LeftOffset = Printer.ScaleX(GetDeviceCaps(Printer.hdc, _
         PHYSICALOFFSETX), vbPixels, vbTwips)
      TopOffset = Printer.ScaleY(GetDeviceCaps(Printer.hdc, _
         PHYSICALOFFSETY), vbPixels, vbTwips)

      ' Calculate the Left, Top, Right, and Bottom margins
      LeftMargin = LeftMarginWidth - LeftOffset
      TopMargin = TopMarginHeight - TopOffset
      RightMargin = (Printer.Width - RightMarginWidth) - LeftOffset
      BottomMargin = (Printer.height - BottomMarginHeight) - TopOffset

      ' Set printable area rect
      rcPage.Left = 0
      rcPage.Top = 0
      rcPage.Right = Printer.ScaleWidth
      rcPage.Bottom = Printer.ScaleHeight

      ' Set rect in which to print (relative to printable area)
      rcDrawTo.Left = LeftMargin
      rcDrawTo.Top = TopMargin
      rcDrawTo.Right = RightMargin
      rcDrawTo.Bottom = BottomMargin

      ' Set up the print instructions
      fr.hdc = Printer.hdc        ' Use the same DC for measuring and rendering
      fr.hdcTarget = Printer.hdc  ' Point at printer hDC
      fr.rc = rcDrawTo            ' Indicate the area on page to draw to
      fr.rcPage = rcPage          ' Indicate entire size of page
      fr.chrg.cpMin = 0           ' Indicate start of text through
      fr.chrg.cpMax = -1          ' end of the text

      ' Get length of text in RTF
      TextLength = Len(RTF.Text)

      ' Loop printing each page until done
      Do
         rcDrawTo.Left = LeftMargin
         rcDrawTo.Top = TopMargin
         rcDrawTo.Right = RightMargin
         rcDrawTo.Bottom = BottomMargin
         ' Print the page by sending EM_FORMATRANGE message
         NextCharPosition = SendMessage(RTF.hwnd, EM_FORMATRANGE, True, fr)
         If NextCharPosition >= TextLength Then Exit Do  'If done then exit
         fr.chrg.cpMin = NextCharPosition ' Starting position for next page
         Printer.NewPage                  ' Move on to next page
         Printer.Print Space(1) ' Re-initialize hDC
         fr.hdc = Printer.hdc
         fr.hdcTarget = Printer.hdc
      Loop

      ' Commit the print job
      Printer.EndDoc

      ' Allow the RTF to free up memory
      r = SendMessage(RTF.hwnd, EM_FORMATRANGE, False, ByVal CLng(0))
   End Sub

