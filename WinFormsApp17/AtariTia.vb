Public NotInheritable Class AtariTia
    Public Const FrameWidth As Integer = 160
    Public Const FrameHeight As Integer = 192

    ' TIA timing: 228 color clocks per scanline, 3 color clocks per CPU cycle = 76 CPU cycles/scanline
    ' NTSC: 262 scanlines total (3 VSYNC + 37 VBLANK + 192 visible + 30 overscan)
    Private Const CpuCyclesPerScanline As Integer = 76
    Private Const TotalScanlines As Integer = 262
    Private Const VisibleStartLine As Integer = 40  ' After VSYNC + VBLANK

    ' NTSC color palette (128 colors)
    Private Shared ReadOnly NtscPaletteData() As Integer = {
        &HFF000000, &HFF444444, &HFF6C6C6C, &HFF909090, &HFFB0B0B0, &HFFC8C8C8, &HFFDCDCDC, &HFFECECEC,
        &HFF444400, &HFF646410, &HFF848424, &HFF9C9C34, &HFFB4B440, &HFFC8C850, &HFFDCD85C, &HFFECE86C,
        &HFF702800, &HFF844414, &HFF985C28, &HFFAC7838, &HFFBC8C4C, &HFFCCA05C, &HFFDCB468, &HFFECC878,
        &HFF841800, &HFF983418, &HFFAC5030, &HFFC06848, &HFFD0805C, &HFFE09470, &HFFECA880, &HFFFCBC94,
        &HFF880000, &HFF9C2020, &HFFB03C3C, &HFFC05858, &HFFD07070, &HFFE08888, &HFFECA0A0, &HFFFCB4B4,
        &HFF78005C, &HFF8C2074, &HFFA03C88, &HFFB0589C, &HFFC070B0, &HFFD084C0, &HFFDC9CD0, &HFFECB0E0,
        &HFF480078, &HFF602090, &HFF783CA4, &HFF8C58B8, &HFFA070CC, &HFFB484DC, &HFFC49CEC, &HFFD4B0FC,
        &HFF140084, &HFF302098, &HFF4C3CAC, &HFF6858C0, &HFF7C70D0, &HFF9488E0, &HFFA8A0EC, &HFFBCB4FC,
        &HFF000088, &HFF1C209C, &HFF3840B0, &HFF505CC0, &HFF6874D0, &HFF7C8CE0, &HFF90A4EC, &HFFA4B8FC,
        &HFF00187C, &HFF1C3890, &HFF3854A8, &HFF5070BC, &HFF6888CC, &HFF7C9CDC, &HFF90B4EC, &HFFA4C8FC,
        &HFF002C5C, &HFF1C4C78, &HFF386890, &HFF5084AC, &HFF689CC0, &HFF7CB4D4, &HFF90CCE8, &HFFA4E0FC,
        &HFF003C2C, &HFF1C5C48, &HFF387C64, &HFF509C80, &HFF68B494, &HFF7CD0AC, &HFF90E4C0, &HFFA4FCD4,
        &HFF003C00, &HFF205C20, &HFF407C40, &HFF5C9C5C, &HFF74B474, &HFF8CD08C, &HFFA4E4A4, &HFFB8FCB8,
        &HFF143800, &HFF345C1C, &HFF507C38, &HFF6C9850, &HFF84B468, &HFF9CCC7C, &HFFB4E490, &HFFC8FCA4,
        &HFF2C3000, &HFF4C501C, &HFF687034, &HFF848C4C, &HFF9CA864, &HFFB4C078, &HFFCCD488, &HFFE0EC9C,
        &HFF442800, &HFF644818, &HFF846830, &HFFA08444, &HFFB89C58, &HFFD0B46C, &HFFE8CC7C, &HFFFCE08C
    }

    ' Bit reversal lookup tables for performance
    Private Shared ReadOnly ReverseBits4Table() As Byte = {0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15}
    Private Shared ReadOnly ReverseBits8Table() As Byte = InitReverseBits8Table()

    Private Shared Function InitReverseBits8Table() As Byte()
        Dim table(255) As Byte
        For i As Integer = 0 To 255
            Dim r As Integer = 0
            For j As Integer = 0 To 7
                r = r Or (((i >> j) And 1) << (7 - j))
            Next
            table(i) = CByte(r)
        Next
        Return table
    End Function

    Private _scanlineCycles As Integer
    Private _scanline As Integer
    Private _frameComplete As Boolean

    ' TIA registers (basic set for display)
    Private _colubk As Byte  ' Background color
    Private _colupf As Byte  ' Playfield color
    Private _colup0 As Byte  ' Player 0 color
    Private _colup1 As Byte  ' Player 1 color
    Private _pf0 As Byte     ' Playfield 0
    Private _pf1 As Byte     ' Playfield 1
    Private _pf2 As Byte     ' Playfield 2
    Private _ctrlpf As Byte  ' Playfield control

    Public ReadOnly Property FrameComplete As Boolean
        Get
            Return _frameComplete
        End Get
    End Property

    Public Sub Reset()
        _scanline = 0
        _scanlineCycles = 0
        _frameComplete = False
        _colubk = 0
        _colupf = 0
        _colup0 = 0
        _colup1 = 0
        _pf0 = 0
        _pf1 = 0
        _pf2 = 0
        _ctrlpf = 0
    End Sub

    Public Sub BeginFrame()
        _scanline = 0
        _scanlineCycles = 0
        _frameComplete = False
    End Sub

    Public Sub StepCpuCycles(cpuCycles As Integer, frameBufferArgb As Integer())
        _scanlineCycles += cpuCycles

        While _scanlineCycles >= CpuCyclesPerScanline
            _scanlineCycles -= CpuCyclesPerScanline

            ' Render visible scanline
            Dim visibleLine As Integer = _scanline - VisibleStartLine
            If visibleLine >= 0 AndAlso visibleLine < FrameHeight Then
                RenderScanline(visibleLine, frameBufferArgb)
            End If

            _scanline += 1
            If _scanline >= TotalScanlines Then
                _frameComplete = True
                Exit While
            End If
        End While
    End Sub

    Private Sub RenderScanline(line As Integer, frameBufferArgb As Integer())
        Dim offset As Integer = line * FrameWidth
        Dim bgColor As Integer = NtscPaletteData((_colubk >> 1) And 127)
        Dim pfColor As Integer = NtscPaletteData((_colupf >> 1) And 127)

        ' Build 20-bit playfield pattern from PF0, PF1, PF2
        ' PF0: D4-D7 (4 bits, displayed left to right)
        ' PF1: D7-D0 (8 bits, displayed left to right, but stored reversed)
        ' PF2: D0-D7 (8 bits, displayed left to right)
        Dim pf As UInteger = 0
        ' PF0 bits 4-7 become bits 0-3 of playfield (reversed)
        pf = pf Or CUInt(ReverseBits4Table((_pf0 >> 4) And &HF))
        ' PF1 bits 7-0 become bits 4-11 (not reversed)
        pf = pf Or (CUInt(_pf1) << 4)
        ' PF2 bits 0-7 become bits 12-19 (reversed)
        pf = pf Or (CUInt(ReverseBits8Table(_pf2)) << 12)

        For x As Integer = 0 To FrameWidth - 1
            Dim pfBit As Integer
            If x < 80 Then
                ' Left half - use playfield bits 0-19
                pfBit = CInt((pf >> (x \ 4)) And 1UI)
            Else
                ' Right half - mirror or repeat based on CTRLPF
                Dim rx As Integer = x - 80
                If (_ctrlpf And 1) <> 0 Then
                    ' Reflected
                    pfBit = CInt((pf >> (19 - (rx \ 4))) And 1UI)
                Else
                    ' Repeated
                    pfBit = CInt((pf >> (rx \ 4)) And 1UI)
                End If
            End If

            If pfBit <> 0 Then
                frameBufferArgb(offset + x) = pfColor
            Else
                frameBufferArgb(offset + x) = bgColor
            End If
        Next
    End Sub

    Public Function Read8(reg As UShort) As Byte
        ' TIA read registers (active low bits, normally return collision latches, etc.)
        Select Case reg And &H0FUS
            Case &H00 To &H07 ' Collision registers
                Return 0
            Case &H08 To &H0B ' Input ports
                Return &H80 ' High bit set = not pressed
            Case &H0C, &H0D ' Input latches
                Return &H80
            Case Else
                Return 0
        End Select
    End Function

    Public Sub Write8(reg As UShort, value As Byte)
        Select Case reg And &H3FUS
            Case &H00 ' VSYNC
                ' Vertical sync control (bit 1)
            Case &H01 ' VBLANK
                ' Vertical blank control
            Case &H02 ' WSYNC
                ' Wait for horizontal sync - halt CPU until end of scanline
                _scanlineCycles = CpuCyclesPerScanline - 3
            Case &H03 ' RSYNC
                ' Reset horizontal sync
            Case &H04 ' NUSIZ0 - player/missile 0 size
            Case &H05 ' NUSIZ1 - player/missile 1 size
            Case &H06 ' COLUP0
                _colup0 = value
            Case &H07 ' COLUP1
                _colup1 = value
            Case &H08 ' COLUPF
                _colupf = value
            Case &H09 ' COLUBK
                _colubk = value
            Case &H0A ' CTRLPF
                _ctrlpf = value
            Case &H0D ' PF0
                _pf0 = value
            Case &H0E ' PF1
                _pf1 = value
            Case &H0F ' PF2
                _pf2 = value
            ' Additional registers would be handled here for sprites, etc.
        End Select
    End Sub
End Class