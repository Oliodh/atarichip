Public NotInheritable Class AtariTia
    Public Const FrameWidth As Integer = 160
    Public Const FrameHeight As Integer = 192

    Private _scanline As Integer
    Private _frameComplete As Boolean

    Public ReadOnly Property FrameComplete As Boolean
        Get
            Return _frameComplete
        End Get
    End Property

    Public Sub Reset()
        _scanline = 0
        _frameComplete = False
    End Sub

    Public Sub BeginFrame()
        _scanline = 0
        _frameComplete = False
    End Sub

    Public Sub StepCpuCycles(cpuCycles As Integer, frameBufferArgb As Integer())
        ' Placeholder: fill with a simple pattern so the plumbing is visible.
        ' Real implementation must:
        ' - advance TIA 3 color-clocks per CPU cycle
        ' - model WSYNC, HMOVE, playfield, players, missiles, ball, collisions
        ' - build scanlines with correct timing (including VBLANK/VSYNC)
        Dim linesPerFrame As Integer = FrameHeight
        _scanline += cpuCycles \ 10

        If _scanline >= linesPerFrame Then
            _scanline = linesPerFrame
            _frameComplete = True
        End If

        For y As Integer = 0 To FrameHeight - 1
            For x As Integer = 0 To FrameWidth - 1
                Dim c As Integer = If(((x \ 8) Xor (y \ 8)) And 1 = 0, &HFF202020, &HFF404040)
                frameBufferArgb(y * FrameWidth + x) = c
            Next
        Next
    End Sub

    Public Function Read8(reg As UShort) As Byte
        ' TODO: collision latches, input pins.
        Return 0
    End Function

    Public Sub Write8(reg As UShort, value As Byte)
        ' TODO: TIA registers (VSYNC, VBLANK, WSYNC, COLUBK, COLUP0, PFx, GRPx, etc.)
    End Sub
End Class