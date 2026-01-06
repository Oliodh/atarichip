Public NotInheritable Class VcsBus
    Private ReadOnly _rom As Byte()

    Public ReadOnly Property Tia As AtariTia
    Public ReadOnly Property Riot As AtariRiot

    Public Sub New(rom As Byte())
        _rom = CType(rom.Clone(), Byte())
        Tia = New AtariTia()
        Riot = New AtariRiot()
    End Sub

    Public Function Read8(addr As UShort) As Byte
        Dim a As UShort = CUShort(addr And &H1FFFUS)

        ' Cartridge ROM: A12=1 (addresses 1000-1FFF)
        If (a And &H1000US) <> 0 Then
            Dim romIndex As Integer = CInt(a And &HFFFUS)
            If _rom.Length = 4096 Then
                Return _rom(romIndex)
            End If

            ' If you load other sizes, you need mappers (F8/F6/F4/etc.).
            ' For now, mirror whatever was loaded.
            Return _rom(romIndex Mod _rom.Length)
        End If

        ' A12=0: Check A7 to distinguish TIA vs RIOT
        If (a And &H80US) = 0 Then
            ' A7=0: TIA (mirrored every 0x40 bytes)
            Return Tia.Read8(CUShort(a And &H3FUS))
        End If

        ' A7=1: RIOT area - Check A9 to distinguish RAM vs I/O
        If (a And &H200US) = 0 Then
            ' A9=0: RIOT RAM (mirrored, 128 bytes)
            Return Riot.ReadRam(CUShort(a And &H7FUS))
        Else
            ' A9=1: RIOT I/O/Timer
            Return Riot.ReadIo(CUShort(a))
        End If
    End Function

    Public Sub Write8(addr As UShort, value As Byte)
        Dim a As UShort = CUShort(addr And &H1FFFUS)

        ' Cartridge ROM: A12=1 (addresses 1000-1FFF)
        ' Writes to ROM area ignored (unless mapper bank switching, not yet).
        If (a And &H1000US) <> 0 Then
            Return
        End If

        ' A12=0: Check A7 to distinguish TIA vs RIOT
        If (a And &H80US) = 0 Then
            ' A7=0: TIA (mirrored every 0x40 bytes)
            Tia.Write8(CUShort(a And &H3FUS), value)
            Return
        End If

        ' A7=1: RIOT area - Check A9 to distinguish RAM vs I/O
        If (a And &H200US) = 0 Then
            ' A9=0: RIOT RAM (mirrored, 128 bytes)
            Riot.WriteRam(CUShort(a And &H7FUS), value)
        Else
            ' A9=1: RIOT I/O/Timer
            Riot.WriteIo(CUShort(a), value)
        End If
    End Sub
End Class