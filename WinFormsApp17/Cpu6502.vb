Public NotInheritable Class Cpu6502
    Private ReadOnly _bus As VcsBus

    Private _a As Byte
    Private _x As Byte
    Private _y As Byte
    Private _sp As Byte
    Private _p As Byte
    Private _pc As UShort

    Private Const FlagC As Byte = 1 << 0
    Private Const FlagZ As Byte = 1 << 1
    Private Const FlagI As Byte = 1 << 2
    Private Const FlagD As Byte = 1 << 3
    Private Const FlagB As Byte = 1 << 4
    Private Const FlagU As Byte = 1 << 5
    Private Const FlagV As Byte = 1 << 6
    Private Const FlagN As Byte = 1 << 7

    Public Sub New(bus As VcsBus)
        _bus = bus
    End Sub

    Public Sub Reset()
        _a = 0
        _x = 0
        _y = 0
        _sp = &HFD
        _p = CByte(FlagI Or FlagU)

        Dim lo As Byte = Read8(&HFFFCUS)
        Dim hi As Byte = Read8(&HFFFDUS)
        _pc = CUShort(lo Or (hi << 8))
    End Sub

    Public Function StepInstruction() As Integer
        Dim opcode As Byte = Fetch8()

        Select Case opcode
            Case &H00 ' BRK
                _pc = CUShort(_pc + 1)
                Push8(CByte((_pc >> 8) And &HFFUS))
                Push8(CByte(_pc And &HFFUS))
                Push8(CByte(_p Or FlagB Or FlagU))
                _p = CByte(_p Or FlagI)
                Dim loVec As Byte = Read8(&HFFFEUS)
                Dim hiVec As Byte = Read8(&HFFFFUS)
                _pc = CUShort(loVec Or (hiVec << 8))
                Return 7

            Case &HEA ' NOP
                Return 2

            Case &HA9 ' LDA #imm
                _a = Fetch8()
                SetZN(_a)
                Return 2

            Case &HA2 ' LDX #imm
                _x = Fetch8()
                SetZN(_x)
                Return 2

            Case &HA0 ' LDY #imm
                _y = Fetch8()
                SetZN(_y)
                Return 2

            Case &H4C ' JMP abs
                Dim addr As UShort = Fetch16()
                _pc = addr
                Return 3

            Case &H20 ' JSR abs
                Dim addr As UShort = Fetch16()
                Dim ret As UShort = CUShort(_pc - 1)
                Push8(CByte((ret >> 8) And &HFFUS))
                Push8(CByte(ret And &HFFUS))
                _pc = addr
                Return 6

            Case &H60 ' RTS
                Dim lo = Pop8()
                Dim hi = Pop8()
                _pc = CUShort((CUShort(hi) << 8) Or lo)
                _pc = CUShort(_pc + 1)
                Return 6

            Case Else
                Throw New NotSupportedException($"Unimplemented opcode: ${opcode:X2} at PC=${CUShort(_pc - 1):X4}")
        End Select
    End Function

    Private Function Read8(addr As UShort) As Byte
        Return _bus.Read8(addr)
    End Function

    Private Sub Write8(addr As UShort, value As Byte)
        _bus.Write8(addr, value)
    End Sub

    Private Function Fetch8() As Byte
        Dim v As Byte = Read8(_pc)
        _pc = CUShort(_pc + 1)
        Return v
    End Function

    Private Function Fetch16() As UShort
        Dim lo As Byte = Fetch8()
        Dim hi As Byte = Fetch8()
        Return CUShort(lo Or (hi << 8))
    End Function

    Private Sub Push8(v As Byte)
        Write8(CUShort(&H100US Or _sp), v)
        _sp = CByte((_sp - 1) And &HFF)
    End Sub

    Private Function Pop8() As Byte
        _sp = CByte((_sp + 1) And &HFF)
        Return Read8(CUShort(&H100US Or _sp))
    End Function

    Private Sub SetZN(v As Byte)
        If v = 0 Then
            _p = CByte(_p Or FlagZ)
        Else
            _p = CByte(_p And Not FlagZ)
        End If

        If (v And &H80) <> 0 Then
            _p = CByte(_p Or FlagN)
        Else
            _p = CByte(_p And Not FlagN)
        End If
    End Sub
End Class
