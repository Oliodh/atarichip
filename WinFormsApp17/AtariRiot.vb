Public NotInheritable Class AtariRiot
    Private ReadOnly _ram As Byte() = New Byte(127) {}

    ' Timer state
    Private _timerValue As Integer
    Private _timerInterval As Integer = 1  ' 1, 8, 64, or 1024
    Private _timerCycles As Integer
    Private _timerUnderflow As Boolean

    ' I/O ports
    Private _swcha As Byte = &HFF  ' Joystick port (active low)
    Private _swchb As Byte = &H0B  ' Console switches (active low for pressed)
    Private _swacnt As Byte       ' Port A DDR
    Private _swbcnt As Byte       ' Port B DDR

    Public Sub Reset()
        Array.Clear(_ram, 0, _ram.Length)
        _timerValue = 0
        _timerInterval = 1
        _timerCycles = 0
        _timerUnderflow = False
        _swcha = &HFF
        _swchb = &H0B  ' Color TV, P0 diff = B, P1 diff = B, select/reset not pressed
        _swacnt = 0
        _swbcnt = 0
    End Sub

    Public Sub StepCpuCycles(cpuCycles As Integer)
        If _timerInterval = 0 Then Return

        _timerCycles += cpuCycles

        While _timerCycles >= _timerInterval
            _timerCycles -= _timerInterval
            _timerValue -= 1

            If _timerValue < 0 Then
                _timerUnderflow = True
                _timerValue = &HFF
                _timerInterval = 1  ' After underflow, counts down every cycle
            End If
        End While
    End Sub

    Public Function ReadRam(addr As UShort) As Byte
        Return _ram(addr And &H7FUS)
    End Function

    Public Sub WriteRam(addr As UShort, value As Byte)
        _ram(addr And &H7FUS) = value
    End Sub

    Public Function ReadIo(addr As UShort) As Byte
        Select Case addr And &H07US
            Case &H00 ' SWCHA - Port A data (joysticks)
                Return _swcha
            Case &H01 ' SWACNT - Port A DDR
                Return _swacnt
            Case &H02 ' SWCHB - Port B data (console switches)
                Return _swchb
            Case &H03 ' SWBCNT - Port B DDR
                Return _swbcnt
            Case &H04 ' INTIM - Timer output
                _timerUnderflow = False
                Return CByte(_timerValue And &HFF)
            Case &H05 ' TIMINT - Timer interrupt status (active when underflowed)
                Return If(_timerUnderflow, CByte(&H80), CByte(0))
            Case Else
                Return 0
        End Select
    End Function

    Public Sub WriteIo(addr As UShort, value As Byte)
        Select Case addr And &H1FUS
            Case &H00 ' SWCHA
                _swcha = value
            Case &H01 ' SWACNT
                _swacnt = value
            Case &H02 ' SWCHB
                _swchb = value
            Case &H03 ' SWBCNT
                _swbcnt = value
            Case &H14 ' TIM1T - Set timer with 1-cycle interval
                _timerValue = value
                _timerInterval = 1
                _timerCycles = 0
                _timerUnderflow = False
            Case &H15 ' TIM8T - Set timer with 8-cycle interval
                _timerValue = value
                _timerInterval = 8
                _timerCycles = 0
                _timerUnderflow = False
            Case &H16 ' TIM64T - Set timer with 64-cycle interval
                _timerValue = value
                _timerInterval = 64
                _timerCycles = 0
                _timerUnderflow = False
            Case &H17 ' T1024T - Set timer with 1024-cycle interval
                _timerValue = value
                _timerInterval = 1024
                _timerCycles = 0
                _timerUnderflow = False
        End Select
    End Sub

    ' Joystick control methods
    ' SWCHA bit mapping for Player 0: bit 7=right, 6=left, 5=down, 4=up
    ' SWCHA bit mapping for Player 1: bit 3=right, 2=left, 1=down, 0=up
    ' Active low (0 = pressed)
    
    Public Sub SetJoystick0Up(pressed As Boolean)
        If pressed Then
            _swcha = _swcha And &HEF  ' Clear bit 4
        Else
            _swcha = _swcha Or &H10
        End If
    End Sub

    Public Sub SetJoystick0Down(pressed As Boolean)
        If pressed Then
            _swcha = _swcha And &HDF  ' Clear bit 5
        Else
            _swcha = _swcha Or &H20
        End If
    End Sub

    Public Sub SetJoystick0Left(pressed As Boolean)
        If pressed Then
            _swcha = _swcha And &HBF  ' Clear bit 6
        Else
            _swcha = _swcha Or &H40
        End If
    End Sub

    Public Sub SetJoystick0Right(pressed As Boolean)
        If pressed Then
            _swcha = _swcha And &H7F  ' Clear bit 7
        Else
            _swcha = _swcha Or &H80
        End If
    End Sub

    Public Sub SetConsoleReset(pressed As Boolean)
        ' SWCHB bit 0 = Reset (active low)
        If pressed Then
            _swchb = _swchb And &HFE  ' Clear bit 0
        Else
            _swchb = _swchb Or &H1
        End If
    End Sub

    Public Sub SetConsoleSelect(pressed As Boolean)
        ' SWCHB bit 1 = Select (active low)
        If pressed Then
            _swchb = _swchb And &HFD  ' Clear bit 1
        Else
            _swchb = _swchb Or &H2
        End If
    End Sub
End Class