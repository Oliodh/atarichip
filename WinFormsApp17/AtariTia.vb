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

    ' NUSIZ player size modes
    Private Const NUSIZ_NORMAL_WIDTH As Integer = 0
    Private Const NUSIZ_DOUBLE_WIDTH As Integer = 5
    Private Const NUSIZ_QUAD_WIDTH As Integer = 7

    ' Collision detection bit masks
    Private Const COLLISION_BIT_HIGH As Byte = &H80
    Private Const COLLISION_BIT_LOW As Byte = &H40
    
    ' VBLANK control bit mask (bit 1)
    Private Const VBLANK_ENABLE_MASK As Byte = &H02
    
    ' Display colors
    Private Const BLACK_COLOR As Integer = &HFF000000

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
    Private _vblank As Byte  ' Vertical blank control
    Private _colubk As Byte  ' Background color
    Private _colupf As Byte  ' Playfield color
    Private _colup0 As Byte  ' Player 0 color
    Private _colup1 As Byte  ' Player 1 color
    Private _pf0 As Byte     ' Playfield 0
    Private _pf1 As Byte     ' Playfield 1
    Private _pf2 As Byte     ' Playfield 2
    Private _ctrlpf As Byte  ' Playfield control

    ' Player/Missile/Ball graphics
    Private _grp0 As Byte    ' Player 0 graphics
    Private _grp1 As Byte    ' Player 1 graphics
    Private _enam0 As Byte   ' Missile 0 enable
    Private _enam1 As Byte   ' Missile 1 enable
    Private _enabl As Byte   ' Ball enable

    ' Position counters (horizontal position)
    Private _posP0 As Integer ' Player 0 position
    Private _posP1 As Integer ' Player 1 position
    Private _posM0 As Integer ' Missile 0 position
    Private _posM1 As Integer ' Missile 1 position
    Private _posBL As Integer ' Ball position

    ' Horizontal motion
    Private _hmp0 As SByte   ' Player 0 motion
    Private _hmp1 As SByte   ' Player 1 motion
    Private _hmm0 As SByte   ' Missile 0 motion
    Private _hmm1 As SByte   ' Missile 1 motion
    Private _hmbl As SByte   ' Ball motion

    ' Size and copies
    Private _nusiz0 As Byte  ' Player 0 size/copies
    Private _nusiz1 As Byte  ' Player 1 size/copies

    ' Reflection
    Private _refp0 As Byte   ' Player 0 reflect
    Private _refp1 As Byte   ' Player 1 reflect

    ' Vertical delay
    Private _vdelp0 As Byte  ' Player 0 vertical delay
    Private _vdelp1 As Byte  ' Player 1 vertical delay
    Private _vdelbl As Byte  ' Ball vertical delay

    ' Old graphics for vertical delay
    Private _grp0Old As Byte
    Private _grp1Old As Byte

    ' Collision detection latches
    Private _cxm0p As Byte   ' Missile 0 to Player collisions
    Private _cxm1p As Byte   ' Missile 1 to Player collisions
    Private _cxp0fb As Byte  ' Player 0 to Playfield/Ball
    Private _cxp1fb As Byte  ' Player 1 to Playfield/Ball
    Private _cxm0fb As Byte  ' Missile 0 to Playfield/Ball
    Private _cxm1fb As Byte  ' Missile 1 to Playfield/Ball
    Private _cxblpf As Byte  ' Ball to Playfield
    Private _cxppmm As Byte  ' Player and Missile collisions

    ' Input ports
    Private _inpt4 As Byte = &H80  ' Fire button P0 (active low)
    Private _inpt5 As Byte = &H80  ' Fire button P1 (active low)

    Public ReadOnly Property FrameComplete As Boolean
        Get
            Return _frameComplete
        End Get
    End Property

    Public Sub Reset()
        _scanline = 0
        _scanlineCycles = 0
        _frameComplete = False
        _vblank = 0
        _colubk = 0
        _colupf = 0
        _colup0 = 0
        _colup1 = 0
        _pf0 = 0
        _pf1 = 0
        _pf2 = 0
        _ctrlpf = 0
        _grp0 = 0
        _grp1 = 0
        _enam0 = 0
        _enam1 = 0
        _enabl = 0
        _posP0 = 0
        _posP1 = 0
        _posM0 = 0
        _posM1 = 0
        _posBL = 0
        _hmp0 = 0
        _hmp1 = 0
        _hmm0 = 0
        _hmm1 = 0
        _hmbl = 0
        _nusiz0 = 0
        _nusiz1 = 0
        _refp0 = 0
        _refp1 = 0
        _vdelp0 = 0
        _vdelp1 = 0
        _vdelbl = 0
        _grp0Old = 0
        _grp1Old = 0
        _inpt4 = &H80
        _inpt5 = &H80
        ClearCollisions()
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
        
        ' Check if VBLANK is enabled (bit 1)
        If (_vblank And VBLANK_ENABLE_MASK) <> 0 Then
            ' VBLANK is active - render black screen
            For x As Integer = 0 To FrameWidth - 1
                frameBufferArgb(offset + x) = BLACK_COLOR
            Next
            Return
        End If
        
        Dim bgColor As Integer = NtscPaletteData((_colubk >> 1) And 127)
        Dim pfColor As Integer = NtscPaletteData((_colupf >> 1) And 127)
        Dim p0Color As Integer = NtscPaletteData((_colup0 >> 1) And 127)
        Dim p1Color As Integer = NtscPaletteData((_colup1 >> 1) And 127)

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

        ' Determine which player graphics to use (with vertical delay)
        Dim grp0Display As Byte = If((_vdelp0 And 1) <> 0, _grp0Old, _grp0)
        Dim grp1Display As Byte = If((_vdelp1 And 1) <> 0, _grp1Old, _grp1)

        For x As Integer = 0 To FrameWidth - 1
            ' Check playfield
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

            ' Check sprites
            Dim p0Pixel As Boolean = GetPlayerPixel(grp0Display, x, _posP0, _nusiz0, _refp0)
            Dim p1Pixel As Boolean = GetPlayerPixel(grp1Display, x, _posP1, _nusiz1, _refp1)
            Dim m0Pixel As Boolean = GetMissilePixel(x, _posM0, _enam0, _nusiz0)
            Dim m1Pixel As Boolean = GetMissilePixel(x, _posM1, _enam1, _nusiz1)
            Dim blPixel As Boolean = GetBallPixel(x)

            ' Collision detection
            If m0Pixel And p1Pixel Then _cxm0p = _cxm0p Or COLLISION_BIT_HIGH
            If m0Pixel And p0Pixel Then _cxm0p = _cxm0p Or COLLISION_BIT_LOW
            If m1Pixel And p0Pixel Then _cxm1p = _cxm1p Or COLLISION_BIT_HIGH
            If m1Pixel And p1Pixel Then _cxm1p = _cxm1p Or COLLISION_BIT_LOW
            If p0Pixel And pfBit <> 0 Then _cxp0fb = _cxp0fb Or COLLISION_BIT_HIGH
            If p0Pixel And blPixel Then _cxp0fb = _cxp0fb Or COLLISION_BIT_LOW
            If p1Pixel And pfBit <> 0 Then _cxp1fb = _cxp1fb Or COLLISION_BIT_HIGH
            If p1Pixel And blPixel Then _cxp1fb = _cxp1fb Or COLLISION_BIT_LOW
            If m0Pixel And pfBit <> 0 Then _cxm0fb = _cxm0fb Or COLLISION_BIT_HIGH
            If m0Pixel And blPixel Then _cxm0fb = _cxm0fb Or COLLISION_BIT_LOW
            If m1Pixel And pfBit <> 0 Then _cxm1fb = _cxm1fb Or COLLISION_BIT_HIGH
            If m1Pixel And blPixel Then _cxm1fb = _cxm1fb Or COLLISION_BIT_LOW
            If blPixel And pfBit <> 0 Then _cxblpf = _cxblpf Or COLLISION_BIT_HIGH
            If p0Pixel And p1Pixel Then _cxppmm = _cxppmm Or COLLISION_BIT_HIGH
            If m0Pixel And m1Pixel Then _cxppmm = _cxppmm Or COLLISION_BIT_LOW

            ' Priority rendering (CTRLPF bit 2 controls playfield priority)
            Dim pfPriority As Boolean = (_ctrlpf And 4) <> 0

            Dim finalColor As Integer = bgColor

            If pfPriority Then
                ' Playfield has priority over players
                If pfBit <> 0 Then
                    ' Check if playfield should use player colors (CTRLPF bit 1)
                    If (_ctrlpf And 2) <> 0 Then
                        ' Score mode - left uses P0 color, right uses P1 color
                        finalColor = If(x < 80, p0Color, p1Color)
                    Else
                        finalColor = pfColor
                    End If
                ElseIf blPixel Then
                    finalColor = pfColor
                ElseIf p0Pixel OrElse m0Pixel Then
                    finalColor = p0Color
                ElseIf p1Pixel OrElse m1Pixel Then
                    finalColor = p1Color
                End If
            Else
                ' Players have priority over playfield
                If p0Pixel OrElse m0Pixel Then
                    finalColor = p0Color
                ElseIf p1Pixel OrElse m1Pixel Then
                    finalColor = p1Color
                ElseIf blPixel Then
                    finalColor = pfColor
                ElseIf pfBit <> 0 Then
                    ' Check if playfield should use player colors (CTRLPF bit 1)
                    If (_ctrlpf And 2) <> 0 Then
                        ' Score mode - left uses P0 color, right uses P1 color
                        finalColor = If(x < 80, p0Color, p1Color)
                    Else
                        finalColor = pfColor
                    End If
                End If
            End If

            frameBufferArgb(offset + x) = finalColor
        Next
    End Sub

    Public Function Read8(reg As UShort) As Byte
        ' TIA read registers (active low bits, normally return collision latches, etc.)
        Select Case reg And &H0FUS
            Case &H00 ' CXM0P - Missile 0 to Player collisions
                Return _cxm0p
            Case &H01 ' CXM1P - Missile 1 to Player collisions
                Return _cxm1p
            Case &H02 ' CXP0FB - Player 0 to Playfield/Ball
                Return _cxp0fb
            Case &H03 ' CXP1FB - Player 1 to Playfield/Ball
                Return _cxp1fb
            Case &H04 ' CXM0FB - Missile 0 to Playfield/Ball
                Return _cxm0fb
            Case &H05 ' CXM1FB - Missile 1 to Playfield/Ball
                Return _cxm1fb
            Case &H06 ' CXBLPF - Ball to Playfield
                Return _cxblpf
            Case &H07 ' CXPPMM - Player and Missile collisions
                Return _cxppmm
            Case &H08 To &H0B ' Input ports (INPT0-INPT3 for paddles, not implemented)
                Return &H80
            Case &H0C ' INPT4 - Fire button P0
                Return _inpt4
            Case &H0D ' INPT5 - Fire button P1
                Return _inpt5
            Case Else
                Return 0
        End Select
    End Function

    Public Sub Write8(reg As UShort, value As Byte)
        Select Case reg And &H3FUS
            Case &H00 ' VSYNC
                ' Vertical sync control (bit 1)
            Case &H01 ' VBLANK
                ' Vertical blank control (bit 1 enables blanking)
                _vblank = value
            Case &H02 ' WSYNC
                ' Wait for horizontal sync - halt CPU until end of scanline
                _scanlineCycles = CpuCyclesPerScanline - 3
            Case &H03 ' RSYNC
                ' Reset horizontal sync
            Case &H04 ' NUSIZ0 - player/missile 0 size
                _nusiz0 = value
            Case &H05 ' NUSIZ1 - player/missile 1 size
                _nusiz1 = value
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
            Case &H0B ' REFP0
                _refp0 = value
            Case &H0C ' REFP1
                _refp1 = value
            Case &H0D ' PF0
                _pf0 = value
            Case &H0E ' PF1
                _pf1 = value
            Case &H0F ' PF2
                _pf2 = value
            Case &H10 ' RESP0 - Reset Player 0 position
                _posP0 = GetCurrentPixel()
            Case &H11 ' RESP1 - Reset Player 1 position
                _posP1 = GetCurrentPixel()
            Case &H12 ' RESM0 - Reset Missile 0 position
                _posM0 = GetCurrentPixel()
            Case &H13 ' RESM1 - Reset Missile 1 position
                _posM1 = GetCurrentPixel()
            Case &H14 ' RESBL - Reset Ball position
                _posBL = GetCurrentPixel()
            Case &H1B ' GRP0
                _grp0Old = _grp0
                _grp0 = value
            Case &H1C ' GRP1
                _grp1Old = _grp1
                _grp1 = value
            Case &H1D ' ENAM0
                _enam0 = value
            Case &H1E ' ENAM1
                _enam1 = value
            Case &H1F ' ENABL
                _enabl = value
            Case &H20 ' HMP0
                _hmp0 = ConvertToSignedMotion(value)
            Case &H21 ' HMP1
                _hmp1 = ConvertToSignedMotion(value)
            Case &H22 ' HMM0
                _hmm0 = ConvertToSignedMotion(value)
            Case &H23 ' HMM1
                _hmm1 = ConvertToSignedMotion(value)
            Case &H24 ' HMBL
                _hmbl = ConvertToSignedMotion(value)
            Case &H25 ' VDELP0
                _vdelp0 = value
            Case &H26 ' VDELP1
                _vdelp1 = value
            Case &H27 ' VDELBL
                _vdelbl = value
            Case &H2A ' HMOVE - Apply horizontal motion
                ApplyHorizontalMotion()
            Case &H2B ' HMCLR - Clear horizontal motion
                _hmp0 = 0
                _hmp1 = 0
                _hmm0 = 0
                _hmm1 = 0
                _hmbl = 0
            Case &H2C ' CXCLR - Clear collision latches
                ClearCollisions()
            ' Additional registers would be handled here for sprites, etc.
        End Select
    End Sub

    Private Sub ClearCollisions()
        _cxm0p = 0
        _cxm1p = 0
        _cxp0fb = 0
        _cxp1fb = 0
        _cxm0fb = 0
        _cxm1fb = 0
        _cxblpf = 0
        _cxppmm = 0
    End Sub

    Private Function ConvertToSignedMotion(value As Byte) As SByte
        ' Convert horizontal motion register value to signed byte
        ' Upper 4 bits (bits 7-4) are the motion value in two's complement (-8 to +7)
        ' Shift right by 4 to get motion value, then sign-extend to 8 bits
        Dim motion4bit As Integer = value >> 4
        ' Check if bit 3 (sign bit of 4-bit value) is set for sign extension
        If (motion4bit And &H8) <> 0 Then
            ' Negative: sign-extend by ORing with 0xF0
            Return CSByte(motion4bit Or &HF0)
        Else
            ' Positive: already in correct range 0-7
            Return CSByte(motion4bit)
        End If
    End Function

    Private Function GetCurrentPixel() As Integer
        ' Calculate current horizontal pixel position based on scanline cycles
        ' Each CPU cycle is 3 color clocks, and each pixel is 1 color clock
        ' We need to account for the horizontal blanking period (68 color clocks)
        Dim colorClock As Integer = _scanlineCycles * 3
        Dim pixel As Integer = colorClock - 68
        If pixel < 0 Then pixel = 0
        If pixel >= FrameWidth Then pixel = FrameWidth - 1
        Return pixel
    End Function

    Private Sub ApplyHorizontalMotion()
        ' Apply horizontal motion values to positions with proper wrapping
        _posP0 = WrapPosition(_posP0 + _hmp0)
        _posP1 = WrapPosition(_posP1 + _hmp1)
        _posM0 = WrapPosition(_posM0 + _hmm0)
        _posM1 = WrapPosition(_posM1 + _hmm1)
        _posBL = WrapPosition(_posBL + _hmbl)
    End Sub

    Private Function WrapPosition(position As Integer) As Integer
        ' Wrap position to screen width, handling both negative and positive overflow
        Return ((position Mod FrameWidth) + FrameWidth) Mod FrameWidth
    End Function

    Private Function GetCopyOffsets(copyMode As Integer) As Integer()
        ' Get copy offsets based on NUSIZ copy mode
        Select Case copyMode
            Case 0 ' One copy
                Return {0}
            Case 1 ' Two copies close
                Return {0, 16}
            Case 2 ' Two copies medium
                Return {0, 32}
            Case 3 ' Three copies close
                Return {0, 16, 32}
            Case 4 ' Two copies wide
                Return {0, 64}
            Case NUSIZ_DOUBLE_WIDTH, NUSIZ_QUAD_WIDTH ' Double/Quad size (one copy)
                Return {0}
            Case 6 ' Three copies medium
                Return {0, 32, 64}
            Case Else
                Return {0}
        End Select
    End Function

    Private Function GetPlayerPixel(grp As Byte, x As Integer, pos As Integer, nusiz As Byte, refp As Byte) As Boolean
        ' Check if this pixel should display the player
        Dim relX As Integer = x - pos
        If relX < 0 Then Return False
        
        ' Get player width from NUSIZ (bits 0-2: 0=1x, 5=2x, 7=4x)
        Dim sizeMode As Integer = nusiz And &H7
        Dim pixelWidth As Integer = 1
        If sizeMode = NUSIZ_DOUBLE_WIDTH Then pixelWidth = 2
        If sizeMode = NUSIZ_QUAD_WIDTH Then pixelWidth = 4
        
        ' Get the copy offsets
        Dim copyOffsets() As Integer = GetCopyOffsets(nusiz And &H7)
        
        ' Check each copy
        For Each offset In copyOffsets
            Dim copyX As Integer = relX - offset
            If copyX >= 0 AndAlso copyX < 8 * pixelWidth Then
                Dim bitIndex As Integer = copyX \ pixelWidth
                If (refp And 8) <> 0 Then
                    ' Reflected
                    bitIndex = 7 - bitIndex
                End If
                If ((grp >> bitIndex) And 1) <> 0 Then
                    Return True
                End If
            End If
        Next
        
        Return False
    End Function

    Private Function GetMissilePixel(x As Integer, pos As Integer, enam As Byte, nusiz As Byte) As Boolean
        ' Check if missile is enabled
        If (enam And 2) = 0 Then Return False
        
        Dim relX As Integer = x - pos
        If relX < 0 Then Return False
        
        ' Get missile width from NUSIZ bits 4-5
        Dim missileSize As Integer = (nusiz >> 4) And 3
        Dim width As Integer = 1 << missileSize ' 1, 2, 4, or 8 pixels
        
        ' Get copy offsets (same as player)
        Dim copyOffsets() As Integer = GetCopyOffsets(nusiz And &H7)
        
        ' Check each copy
        For Each offset In copyOffsets
            Dim copyX As Integer = relX - offset
            If copyX >= 0 AndAlso copyX < width Then
                Return True
            End If
        Next
        
        Return False
    End Function

    Private Function GetBallPixel(x As Integer) As Boolean
        ' Check if ball is enabled
        If (_enabl And 2) = 0 Then Return False
        
        Dim relX As Integer = x - _posBL
        If relX < 0 Then Return False
        
        ' Get ball size from CTRLPF bits 4-5
        Dim ballSize As Integer = (_ctrlpf >> 4) And 3
        Dim width As Integer = 1 << ballSize ' 1, 2, 4, or 8 pixels
        
        Return relX < width
    End Function

    Public Sub SetFireButton0(pressed As Boolean)
        ' Fire button is active low (0 = pressed, &H80 = not pressed)
        _inpt4 = If(pressed, CByte(0), CByte(&H80))
    End Sub

    Public Sub SetFireButton1(pressed As Boolean)
        ' Fire button is active low (0 = pressed, &H80 = not pressed)
        _inpt5 = If(pressed, CByte(0), CByte(&H80))
    End Sub
End Class