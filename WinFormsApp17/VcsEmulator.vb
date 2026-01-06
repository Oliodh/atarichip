Public NotInheritable Class VcsEmulator
    Private ReadOnly _bus As VcsBus
    Private ReadOnly _cpu As Cpu6502

    Public Sub New(rom As Byte())
        If rom Is Nothing OrElse rom.Length = 0 Then Throw New ArgumentException("ROM is empty.", NameOf(rom))
        _bus = New VcsBus(rom)
        _cpu = New Cpu6502(_bus)
    End Sub

    Public Sub Reset()
        _cpu.Reset()
        _bus.Tia.Reset()
        _bus.Riot.Reset()
    End Sub

    Public Sub RunFrame(frameBufferArgb As Integer())
        ' Atari 2600 is typically stepped cycle-by-cycle:
        ' - CPU executes cycles
        ' - TIA advances 3 color-clocks per CPU cycle (and builds scanlines)
        '
        ' For now, we just run enough CPU cycles to let the TIA produce a frame.
        _bus.Tia.BeginFrame()

        While Not _bus.Tia.FrameComplete
            Dim cpuCycles As Integer = _cpu.StepInstruction()
            _bus.Tia.StepCpuCycles(cpuCycles, frameBufferArgb)

            ' RIOT timer would also tick here; currently stubbed.
            _bus.Riot.StepCpuCycles(cpuCycles)
        End While
    End Sub
End Class