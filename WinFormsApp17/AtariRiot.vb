Public NotInheritable Class AtariRiot
    Private ReadOnly _ram As Byte() = New Byte(127) {}

    Public Sub Reset()
        Array.Clear(_ram, 0, _ram.Length)
    End Sub

    Public Sub StepCpuCycles(cpuCycles As Integer)
        ' TODO: timer decrements at configured rates.
    End Sub

    Public Function ReadRam(addr As UShort) As Byte
        Return _ram(addr And &H7FUS)
    End Function

    Public Sub WriteRam(addr As UShort, value As Byte)
        _ram(addr And &H7FUS) = value
    End Sub

    Public Function ReadIo(addr As UShort) As Byte
        ' TODO: SWCHA/SWCHB/INTIM/etc.
        Return 0
    End Function

    Public Sub WriteIo(addr As UShort, value As Byte)
        ' TODO: TIM1T/TIM8T/TIM64T/T1024T, etc.
    End Sub
End Class