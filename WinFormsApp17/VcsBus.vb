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

        ' 2600 decoding is heavily mirrored.
        ' TIA: 0000-007F (mirrored)
        If a < &H80US Then
            Return Tia.Read8(CUShort(a And &H3FUS))
        End If

        ' RIOT: 0080-00FF RAM (mirrored)
        If a < &H100US Then
            Return Riot.ReadRam(CUShort(a And &H7FUS))
        End If

        ' RIOT I/O/timer: 0280-0297 (mirrors exist) - simplified mapping here.
        If a >= &H280US AndAlso a <= &H297US Then
            Return Riot.ReadIo(CUShort(a))
        End If

        ' Cartridge ROM: 1000-1FFF (4K)
        If a >= &H1000US Then
            Dim romIndex As Integer = CInt(a And &HFFFUS)
            If _rom.Length = 4096 Then
                Return _rom(romIndex)
            End If

            ' If you load other sizes, you need mappers (F8/F6/F4/etc.).
            ' For now, mirror whatever was loaded.
            Return _rom(romIndex Mod _rom.Length)
        End If

        Return 0
    End Function

    Public Sub Write8(addr As UShort, value As Byte)
        Dim a As UShort = CUShort(addr And &H1FFFUS)

        If a < &H80US Then
            Tia.Write8(CUShort(a And &H3FUS), value)
            Return
        End If

        If a < &H100US Then
            Riot.WriteRam(CUShort(a And &H7FUS), value)
            Return
        End If

        If a >= &H280US AndAlso a <= &H297US Then
            Riot.WriteIo(CUShort(a), value)
            Return
        End If

        ' Writes to ROM area ignored (unless mapper bank switching, not yet).
    End Sub
End Class