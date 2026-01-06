Public NotInheritable Class Cpu6502
    Private ReadOnly _bus As VcsBus

    Private _a As Byte
    Private _x As Byte
    Private _y As Byte
    Private _sp As Byte
    Private _p As Byte
    Private _pc As UShort

    Private Const FlagC As Byte = 1
    Private Const FlagZ As Byte = 2
    Private Const FlagI As Byte = 4
    Private Const FlagD As Byte = 8
    Private Const FlagB As Byte = 16
    Private Const FlagU As Byte = 32
    Private Const FlagV As Byte = 64
    Private Const FlagN As Byte = 128

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
        _pc = CUShort(lo Or (CUShort(hi) << 8))
    End Sub

    Public Function StepInstruction() As Integer
        Dim opcode As Byte = Fetch8()
        Return ExecuteOpcode(opcode)
    End Function

    Private Function ExecuteOpcode(opcode As Byte) As Integer
        Select Case opcode
            ' --- ADC ---
            Case &H69 : Return Op_ADC(AddrImm(), 2)
            Case &H65 : Return Op_ADC(AddrZp(), 3)
            Case &H75 : Return Op_ADC(AddrZpX(), 4)
            Case &H6D : Return Op_ADC(AddrAbs(), 4)
            Case &H7D : Return Op_ADC_AbsX()
            Case &H79 : Return Op_ADC_AbsY()
            Case &H61 : Return Op_ADC(AddrIndX(), 6)
            Case &H71 : Return Op_ADC_IndY()

            ' --- AND ---
            Case &H29 : Return Op_AND(AddrImm(), 2)
            Case &H25 : Return Op_AND(AddrZp(), 3)
            Case &H35 : Return Op_AND(AddrZpX(), 4)
            Case &H2D : Return Op_AND(AddrAbs(), 4)
            Case &H3D : Return Op_AND_AbsX()
            Case &H39 : Return Op_AND_AbsY()
            Case &H21 : Return Op_AND(AddrIndX(), 6)
            Case &H31 : Return Op_AND_IndY()

            ' --- ASL ---
            Case &H0A : Return Op_ASL_A()
            Case &H06 : Return Op_ASL_Mem(AddrZp(), 5)
            Case &H16 : Return Op_ASL_Mem(AddrZpX(), 6)
            Case &H0E : Return Op_ASL_Mem(AddrAbs(), 6)
            Case &H1E : Return Op_ASL_Mem(AddrAbsX_NoPage(), 7)

            ' --- Branch ---
            Case &H90 : Return Op_Branch(Not GetFlag(FlagC)) ' BCC
            Case &HB0 : Return Op_Branch(GetFlag(FlagC))     ' BCS
            Case &HF0 : Return Op_Branch(GetFlag(FlagZ))     ' BEQ
            Case &H30 : Return Op_Branch(GetFlag(FlagN))     ' BMI
            Case &HD0 : Return Op_Branch(Not GetFlag(FlagZ)) ' BNE
            Case &H10 : Return Op_Branch(Not GetFlag(FlagN)) ' BPL
            Case &H50 : Return Op_Branch(Not GetFlag(FlagV)) ' BVC
            Case &H70 : Return Op_Branch(GetFlag(FlagV))     ' BVS

            ' --- BIT ---
            Case &H24 : Return Op_BIT(AddrZp(), 3)
            Case &H2C : Return Op_BIT(AddrAbs(), 4)

            ' --- BRK ---
            Case &H00 : Return Op_BRK()

            ' --- Clear/Set flags ---
            Case &H18 : ClearFlag(FlagC) : Return 2 ' CLC
            Case &HD8 : ClearFlag(FlagD) : Return 2 ' CLD
            Case &H58 : ClearFlag(FlagI) : Return 2 ' CLI
            Case &HB8 : ClearFlag(FlagV) : Return 2 ' CLV
            Case &H38 : SetFlag(FlagC) : Return 2   ' SEC
            Case &HF8 : SetFlag(FlagD) : Return 2   ' SED
            Case &H78 : SetFlag(FlagI) : Return 2   ' SEI

            ' --- CMP ---
            Case &HC9 : Return Op_CMP(_a, AddrImm(), 2)
            Case &HC5 : Return Op_CMP(_a, AddrZp(), 3)
            Case &HD5 : Return Op_CMP(_a, AddrZpX(), 4)
            Case &HCD : Return Op_CMP(_a, AddrAbs(), 4)
            Case &HDD : Return Op_CMP_AbsX()
            Case &HD9 : Return Op_CMP_AbsY()
            Case &HC1 : Return Op_CMP(_a, AddrIndX(), 6)
            Case &HD1 : Return Op_CMP_IndY()

            ' --- CPX ---
            Case &HE0 : Return Op_CMP(_x, AddrImm(), 2)
            Case &HE4 : Return Op_CMP(_x, AddrZp(), 3)
            Case &HEC : Return Op_CMP(_x, AddrAbs(), 4)

            ' --- CPY ---
            Case &HC0 : Return Op_CMP(_y, AddrImm(), 2)
            Case &HC4 : Return Op_CMP(_y, AddrZp(), 3)
            Case &HCC : Return Op_CMP(_y, AddrAbs(), 4)

            ' --- DEC ---
            Case &HC6 : Return Op_DEC_Mem(AddrZp(), 5)
            Case &HD6 : Return Op_DEC_Mem(AddrZpX(), 6)
            Case &HCE : Return Op_DEC_Mem(AddrAbs(), 6)
            Case &HDE : Return Op_DEC_Mem(AddrAbsX_NoPage(), 7)

            ' --- DEX/DEY ---
            Case &HCA : _x = CByte((_x - 1) And &HFF) : SetZN(_x) : Return 2
            Case &H88 : _y = CByte((_y - 1) And &HFF) : SetZN(_y) : Return 2

            ' --- EOR ---
            Case &H49 : Return Op_EOR(AddrImm(), 2)
            Case &H45 : Return Op_EOR(AddrZp(), 3)
            Case &H55 : Return Op_EOR(AddrZpX(), 4)
            Case &H4D : Return Op_EOR(AddrAbs(), 4)
            Case &H5D : Return Op_EOR_AbsX()
            Case &H59 : Return Op_EOR_AbsY()
            Case &H41 : Return Op_EOR(AddrIndX(), 6)
            Case &H51 : Return Op_EOR_IndY()

            ' --- INC ---
            Case &HE6 : Return Op_INC_Mem(AddrZp(), 5)
            Case &HF6 : Return Op_INC_Mem(AddrZpX(), 6)
            Case &HEE : Return Op_INC_Mem(AddrAbs(), 6)
            Case &HFE : Return Op_INC_Mem(AddrAbsX_NoPage(), 7)

            ' --- INX/INY ---
            Case &HE8 : _x = CByte((_x + 1) And &HFF) : SetZN(_x) : Return 2
            Case &HC8 : _y = CByte((_y + 1) And &HFF) : SetZN(_y) : Return 2

            ' --- JMP ---
            Case &H4C : _pc = Fetch16() : Return 3
            Case &H6C : Return Op_JMP_Ind()

            ' --- JSR ---
            Case &H20 : Return Op_JSR()

            ' --- LDA ---
            Case &HA9 : _a = Fetch8() : SetZN(_a) : Return 2
            Case &HA5 : _a = Read8(AddrZp()) : SetZN(_a) : Return 3
            Case &HB5 : _a = Read8(AddrZpX()) : SetZN(_a) : Return 4
            Case &HAD : _a = Read8(AddrAbs()) : SetZN(_a) : Return 4
            Case &HBD : Return Op_LDA_AbsX()
            Case &HB9 : Return Op_LDA_AbsY()
            Case &HA1 : _a = Read8(AddrIndX()) : SetZN(_a) : Return 6
            Case &HB1 : Return Op_LDA_IndY()

            ' --- LDX ---
            Case &HA2 : _x = Fetch8() : SetZN(_x) : Return 2
            Case &HA6 : _x = Read8(AddrZp()) : SetZN(_x) : Return 3
            Case &HB6 : _x = Read8(AddrZpY()) : SetZN(_x) : Return 4
            Case &HAE : _x = Read8(AddrAbs()) : SetZN(_x) : Return 4
            Case &HBE : Return Op_LDX_AbsY()

            ' --- LDY ---
            Case &HA0 : _y = Fetch8() : SetZN(_y) : Return 2
            Case &HA4 : _y = Read8(AddrZp()) : SetZN(_y) : Return 3
            Case &HB4 : _y = Read8(AddrZpX()) : SetZN(_y) : Return 4
            Case &HAC : _y = Read8(AddrAbs()) : SetZN(_y) : Return 4
            Case &HBC : Return Op_LDY_AbsX()

            ' --- LSR ---
            Case &H4A : Return Op_LSR_A()
            Case &H46 : Return Op_LSR_Mem(AddrZp(), 5)
            Case &H56 : Return Op_LSR_Mem(AddrZpX(), 6)
            Case &H4E : Return Op_LSR_Mem(AddrAbs(), 6)
            Case &H5E : Return Op_LSR_Mem(AddrAbsX_NoPage(), 7)

            ' --- NOP ---
            Case &HEA : Return 2

            ' --- ORA ---
            Case &H09 : Return Op_ORA(AddrImm(), 2)
            Case &H05 : Return Op_ORA(AddrZp(), 3)
            Case &H15 : Return Op_ORA(AddrZpX(), 4)
            Case &H0D : Return Op_ORA(AddrAbs(), 4)
            Case &H1D : Return Op_ORA_AbsX()
            Case &H19 : Return Op_ORA_AbsY()
            Case &H01 : Return Op_ORA(AddrIndX(), 6)
            Case &H11 : Return Op_ORA_IndY()

            ' --- PHA/PHP/PLA/PLP ---
            Case &H48 : Push8(_a) : Return 3
            Case &H08 : Push8(CByte(_p Or FlagB Or FlagU)) : Return 3
            Case &H68 : _a = Pop8() : SetZN(_a) : Return 4
            Case &H28 : _p = CByte((Pop8() And (Not FlagB)) Or FlagU) : Return 4

            ' --- ROL ---
            Case &H2A : Return Op_ROL_A()
            Case &H26 : Return Op_ROL_Mem(AddrZp(), 5)
            Case &H36 : Return Op_ROL_Mem(AddrZpX(), 6)
            Case &H2E : Return Op_ROL_Mem(AddrAbs(), 6)
            Case &H3E : Return Op_ROL_Mem(AddrAbsX_NoPage(), 7)

            ' --- ROR ---
            Case &H6A : Return Op_ROR_A()
            Case &H66 : Return Op_ROR_Mem(AddrZp(), 5)
            Case &H76 : Return Op_ROR_Mem(AddrZpX(), 6)
            Case &H6E : Return Op_ROR_Mem(AddrAbs(), 6)
            Case &H7E : Return Op_ROR_Mem(AddrAbsX_NoPage(), 7)

            ' --- RTI ---
            Case &H40 : Return Op_RTI()

            ' --- RTS ---
            Case &H60 : Return Op_RTS()

            ' --- SBC ---
            Case &HE9 : Return Op_SBC(AddrImm(), 2)
            Case &HE5 : Return Op_SBC(AddrZp(), 3)
            Case &HF5 : Return Op_SBC(AddrZpX(), 4)
            Case &HED : Return Op_SBC(AddrAbs(), 4)
            Case &HFD : Return Op_SBC_AbsX()
            Case &HF9 : Return Op_SBC_AbsY()
            Case &HE1 : Return Op_SBC(AddrIndX(), 6)
            Case &HF1 : Return Op_SBC_IndY()

            ' --- STA ---
            Case &H85 : Write8(AddrZp(), _a) : Return 3
            Case &H95 : Write8(AddrZpX(), _a) : Return 4
            Case &H8D : Write8(AddrAbs(), _a) : Return 4
            Case &H9D : Write8(AddrAbsX_NoPage(), _a) : Return 5
            Case &H99 : Write8(AddrAbsY_NoPage(), _a) : Return 5
            Case &H81 : Write8(AddrIndX(), _a) : Return 6
            Case &H91 : Write8(AddrIndY_NoPage(), _a) : Return 6

            ' --- STX ---
            Case &H86 : Write8(AddrZp(), _x) : Return 3
            Case &H96 : Write8(AddrZpY(), _x) : Return 4
            Case &H8E : Write8(AddrAbs(), _x) : Return 4

            ' --- STY ---
            Case &H84 : Write8(AddrZp(), _y) : Return 3
            Case &H94 : Write8(AddrZpX(), _y) : Return 4
            Case &H8C : Write8(AddrAbs(), _y) : Return 4

            ' --- TAX/TAY/TSX/TXA/TXS/TYA ---
            Case &HAA : _x = _a : SetZN(_x) : Return 2
            Case &HA8 : _y = _a : SetZN(_y) : Return 2
            Case &HBA : _x = _sp : SetZN(_x) : Return 2
            Case &H8A : _a = _x : SetZN(_a) : Return 2
            Case &H9A : _sp = _x : Return 2
            Case &H98 : _a = _y : SetZN(_a) : Return 2

            ' --- Undocumented NOPs (common ones) ---
            Case &H1A, &H3A, &H5A, &H7A, &HDA, &HFA : Return 2                          ' 1-byte NOPs
            Case &H04, &H44, &H64 : Fetch8() : Return 3                                  ' 2-byte NOPs (ZP)
            Case &H0C : Fetch16() : Return 4                                             ' 3-byte NOP (abs)
            Case &H14, &H34, &H54, &H74, &HD4, &HF4 : Fetch8() : Return 4                ' 2-byte NOPs (ZP,X)
            Case &H1C, &H3C, &H5C, &H7C, &HDC, &HFC : AddrAbsX_WithPage() : Return 4     ' 3-byte NOPs (abs,X)
            Case &H80, &H82, &H89, &HC2, &HE2 : Fetch8() : Return 2                      ' 2-byte NOPs (imm)

            Case Else
                ' Treat unknown opcodes as 1-byte NOPs to avoid crashes
                Return 2
        End Select
    End Function

    ' ========== Addressing Modes ==========

    Private Function AddrImm() As UShort
        Dim addr As UShort = _pc
        _pc = CUShort(_pc + 1)
        Return addr
    End Function

    Private Function AddrZp() As UShort
        Return CUShort(Fetch8())
    End Function

    Private Function AddrZpX() As UShort
        Return CUShort((Fetch8() + _x) And &HFF)
    End Function

    Private Function AddrZpY() As UShort
        Return CUShort((Fetch8() + _y) And &HFF)
    End Function

    Private Function AddrAbs() As UShort
        Return Fetch16()
    End Function

    Private Function AddrAbsX_NoPage() As UShort
        Return CUShort((Fetch16() + _x) And &HFFFF)
    End Function

    Private Function AddrAbsX_WithPage() As (Addr As UShort, PageCross As Boolean)
        Dim base As UShort = Fetch16()
        Dim addr As UShort = CUShort((base + _x) And &HFFFF)
        Return (addr, (base And &HFF00US) <> (addr And &HFF00US))
    End Function

    Private Function AddrAbsY_NoPage() As UShort
        Return CUShort((Fetch16() + _y) And &HFFFF)
    End Function

    Private Function AddrAbsY_WithPage() As (Addr As UShort, PageCross As Boolean)
        Dim base As UShort = Fetch16()
        Dim addr As UShort = CUShort((base + _y) And &HFFFF)
        Return (addr, (base And &HFF00US) <> (addr And &HFF00US))
    End Function

    Private Function AddrIndX() As UShort
        Dim zp As Byte = CByte((Fetch8() + _x) And &HFF)
        Dim lo As Byte = Read8(CUShort(zp))
        Dim hi As Byte = Read8(CUShort((zp + 1) And &HFF))
        Return CUShort(lo Or (CUShort(hi) << 8))
    End Function

    Private Function AddrIndY_NoPage() As UShort
        Dim zp As Byte = Fetch8()
        Dim lo As Byte = Read8(CUShort(zp))
        Dim hi As Byte = Read8(CUShort((zp + 1) And &HFF))
        Dim base As UShort = CUShort(lo Or (CUShort(hi) << 8))
        Return CUShort((base + _y) And &HFFFF)
    End Function

    Private Function AddrIndY_WithPage() As (Addr As UShort, PageCross As Boolean)
        Dim zp As Byte = Fetch8()
        Dim lo As Byte = Read8(CUShort(zp))
        Dim hi As Byte = Read8(CUShort((zp + 1) And &HFF))
        Dim base As UShort = CUShort(lo Or (CUShort(hi) << 8))
        Dim addr As UShort = CUShort((base + _y) And &HFFFF)
        Return (addr, (base And &HFF00US) <> (addr And &HFF00US))
    End Function

    ' ========== Operations ==========

    Private Function Op_ADC(addr As UShort, baseCycles As Integer) As Integer
        Dim v As Byte = Read8(addr)
        DoADC(v)
        Return baseCycles
    End Function

    Private Function Op_ADC_AbsX() As Integer
        Dim r = AddrAbsX_WithPage()
        DoADC(Read8(r.Addr))
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_ADC_AbsY() As Integer
        Dim r = AddrAbsY_WithPage()
        DoADC(Read8(r.Addr))
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_ADC_IndY() As Integer
        Dim r = AddrIndY_WithPage()
        DoADC(Read8(r.Addr))
        Return If(r.PageCross, 6, 5)
    End Function

    Private Sub DoADC(v As Byte)
        Dim c As Integer = If(GetFlag(FlagC), 1, 0)
        Dim sum As Integer = CInt(_a) + CInt(v) + c
        Dim overflow As Boolean = ((Not (_a Xor v)) And (_a Xor CByte(sum And &HFF)) And &H80) <> 0
        _a = CByte(sum And &HFF)
        SetOrClearFlag(FlagC, sum > 255)
        SetOrClearFlag(FlagV, overflow)
        SetZN(_a)
    End Sub

    Private Function Op_SBC(addr As UShort, baseCycles As Integer) As Integer
        DoSBC(Read8(addr))
        Return baseCycles
    End Function

    Private Function Op_SBC_AbsX() As Integer
        Dim r = AddrAbsX_WithPage()
        DoSBC(Read8(r.Addr))
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_SBC_AbsY() As Integer
        Dim r = AddrAbsY_WithPage()
        DoSBC(Read8(r.Addr))
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_SBC_IndY() As Integer
        Dim r = AddrIndY_WithPage()
        DoSBC(Read8(r.Addr))
        Return If(r.PageCross, 6, 5)
    End Function

    Private Sub DoSBC(v As Byte)
        DoADC(CByte(v Xor &HFF))
    End Sub

    Private Function Op_AND(addr As UShort, baseCycles As Integer) As Integer
        _a = CByte(_a And Read8(addr))
        SetZN(_a)
        Return baseCycles
    End Function

    Private Function Op_AND_AbsX() As Integer
        Dim r = AddrAbsX_WithPage()
        _a = CByte(_a And Read8(r.Addr))
        SetZN(_a)
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_AND_AbsY() As Integer
        Dim r = AddrAbsY_WithPage()
        _a = CByte(_a And Read8(r.Addr))
        SetZN(_a)
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_AND_IndY() As Integer
        Dim r = AddrIndY_WithPage()
        _a = CByte(_a And Read8(r.Addr))
        SetZN(_a)
        Return If(r.PageCross, 6, 5)
    End Function

    Private Function Op_ORA(addr As UShort, baseCycles As Integer) As Integer
        _a = CByte(_a Or Read8(addr))
        SetZN(_a)
        Return baseCycles
    End Function

    Private Function Op_ORA_AbsX() As Integer
        Dim r = AddrAbsX_WithPage()
        _a = CByte(_a Or Read8(r.Addr))
        SetZN(_a)
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_ORA_AbsY() As Integer
        Dim r = AddrAbsY_WithPage()
        _a = CByte(_a Or Read8(r.Addr))
        SetZN(_a)
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_ORA_IndY() As Integer
        Dim r = AddrIndY_WithPage()
        _a = CByte(_a Or Read8(r.Addr))
        SetZN(_a)
        Return If(r.PageCross, 6, 5)
    End Function

    Private Function Op_EOR(addr As UShort, baseCycles As Integer) As Integer
        _a = CByte(_a Xor Read8(addr))
        SetZN(_a)
        Return baseCycles
    End Function

    Private Function Op_EOR_AbsX() As Integer
        Dim r = AddrAbsX_WithPage()
        _a = CByte(_a Xor Read8(r.Addr))
        SetZN(_a)
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_EOR_AbsY() As Integer
        Dim r = AddrAbsY_WithPage()
        _a = CByte(_a Xor Read8(r.Addr))
        SetZN(_a)
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_EOR_IndY() As Integer
        Dim r = AddrIndY_WithPage()
        _a = CByte(_a Xor Read8(r.Addr))
        SetZN(_a)
        Return If(r.PageCross, 6, 5)
    End Function

    Private Function Op_CMP(reg As Byte, addr As UShort, baseCycles As Integer) As Integer
        Dim v As Byte = Read8(addr)
        Dim diff As Integer = CInt(reg) - CInt(v)
        SetOrClearFlag(FlagC, reg >= v)
        SetZN(CByte(diff And &HFF))
        Return baseCycles
    End Function

    Private Function Op_CMP_AbsX() As Integer
        Dim r = AddrAbsX_WithPage()
        Dim v As Byte = Read8(r.Addr)
        Dim diff As Integer = CInt(_a) - CInt(v)
        SetOrClearFlag(FlagC, _a >= v)
        SetZN(CByte(diff And &HFF))
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_CMP_AbsY() As Integer
        Dim r = AddrAbsY_WithPage()
        Dim v As Byte = Read8(r.Addr)
        Dim diff As Integer = CInt(_a) - CInt(v)
        SetOrClearFlag(FlagC, _a >= v)
        SetZN(CByte(diff And &HFF))
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_CMP_IndY() As Integer
        Dim r = AddrIndY_WithPage()
        Dim v As Byte = Read8(r.Addr)
        Dim diff As Integer = CInt(_a) - CInt(v)
        SetOrClearFlag(FlagC, _a >= v)
        SetZN(CByte(diff And &HFF))
        Return If(r.PageCross, 6, 5)
    End Function

    Private Function Op_BIT(addr As UShort, baseCycles As Integer) As Integer
        Dim v As Byte = Read8(addr)
        SetOrClearFlag(FlagZ, (_a And v) = 0)
        SetOrClearFlag(FlagV, (v And &H40) <> 0)
        SetOrClearFlag(FlagN, (v And &H80) <> 0)
        Return baseCycles
    End Function

    Private Function Op_ASL_A() As Integer
        SetOrClearFlag(FlagC, (_a And &H80) <> 0)
        _a = CByte((_a << 1) And &HFF)
        SetZN(_a)
        Return 2
    End Function

    Private Function Op_ASL_Mem(addr As UShort, baseCycles As Integer) As Integer
        Dim v As Byte = Read8(addr)
        SetOrClearFlag(FlagC, (v And &H80) <> 0)
        v = CByte((v << 1) And &HFF)
        Write8(addr, v)
        SetZN(v)
        Return baseCycles
    End Function

    Private Function Op_LSR_A() As Integer
        SetOrClearFlag(FlagC, (_a And 1) <> 0)
        _a = CByte(_a >> 1)
        SetZN(_a)
        Return 2
    End Function

    Private Function Op_LSR_Mem(addr As UShort, baseCycles As Integer) As Integer
        Dim v As Byte = Read8(addr)
        SetOrClearFlag(FlagC, (v And 1) <> 0)
        v = CByte(v >> 1)
        Write8(addr, v)
        SetZN(v)
        Return baseCycles
    End Function

    Private Function Op_ROL_A() As Integer
        Dim oldC As Integer = If(GetFlag(FlagC), 1, 0)
        SetOrClearFlag(FlagC, (_a And &H80) <> 0)
        _a = CByte(((_a << 1) Or oldC) And &HFF)
        SetZN(_a)
        Return 2
    End Function

    Private Function Op_ROL_Mem(addr As UShort, baseCycles As Integer) As Integer
        Dim v As Byte = Read8(addr)
        Dim oldC As Integer = If(GetFlag(FlagC), 1, 0)
        SetOrClearFlag(FlagC, (v And &H80) <> 0)
        v = CByte(((v << 1) Or oldC) And &HFF)
        Write8(addr, v)
        SetZN(v)
        Return baseCycles
    End Function

    Private Function Op_ROR_A() As Integer
        Dim oldC As Integer = If(GetFlag(FlagC), &H80, 0)
        SetOrClearFlag(FlagC, (_a And 1) <> 0)
        _a = CByte((_a >> 1) Or oldC)
        SetZN(_a)
        Return 2
    End Function

    Private Function Op_ROR_Mem(addr As UShort, baseCycles As Integer) As Integer
        Dim v As Byte = Read8(addr)
        Dim oldC As Integer = If(GetFlag(FlagC), &H80, 0)
        SetOrClearFlag(FlagC, (v And 1) <> 0)
        v = CByte((v >> 1) Or oldC)
        Write8(addr, v)
        SetZN(v)
        Return baseCycles
    End Function

    Private Function Op_INC_Mem(addr As UShort, baseCycles As Integer) As Integer
        Dim v As Byte = CByte((Read8(addr) + 1) And &HFF)
        Write8(addr, v)
        SetZN(v)
        Return baseCycles
    End Function

    Private Function Op_DEC_Mem(addr As UShort, baseCycles As Integer) As Integer
        Dim v As Byte = CByte((Read8(addr) - 1) And &HFF)
        Write8(addr, v)
        SetZN(v)
        Return baseCycles
    End Function

    Private Function Op_LDA_AbsX() As Integer
        Dim r = AddrAbsX_WithPage()
        _a = Read8(r.Addr)
        SetZN(_a)
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_LDA_AbsY() As Integer
        Dim r = AddrAbsY_WithPage()
        _a = Read8(r.Addr)
        SetZN(_a)
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_LDA_IndY() As Integer
        Dim r = AddrIndY_WithPage()
        _a = Read8(r.Addr)
        SetZN(_a)
        Return If(r.PageCross, 6, 5)
    End Function

    Private Function Op_LDX_AbsY() As Integer
        Dim r = AddrAbsY_WithPage()
        _x = Read8(r.Addr)
        SetZN(_x)
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_LDY_AbsX() As Integer
        Dim r = AddrAbsX_WithPage()
        _y = Read8(r.Addr)
        SetZN(_y)
        Return If(r.PageCross, 5, 4)
    End Function

    Private Function Op_Branch(condition As Boolean) As Integer
        Dim offset As SByte = CSByte(Fetch8())
        If condition Then
            Dim oldPC As UShort = _pc
            _pc = CUShort((_pc + offset) And &HFFFF)
            If (oldPC And &HFF00US) <> (_pc And &HFF00US) Then
                Return 4 ' Page crossed
            End If
            Return 3 ' Branch taken, no page cross
        End If
        Return 2 ' Branch not taken
    End Function

    Private Function Op_BRK() As Integer
        _pc = CUShort(_pc + 1)
        Push8(CByte((_pc >> 8) And &HFF))
        Push8(CByte(_pc And &HFF))
        Push8(CByte(_p Or FlagB Or FlagU))
        SetFlag(FlagI)
        Dim loVec As Byte = Read8(&HFFFEUS)
        Dim hiVec As Byte = Read8(&HFFFFUS)
        _pc = CUShort(loVec Or (CUShort(hiVec) << 8))
        Return 7
    End Function

    Private Function Op_RTI() As Integer
        _p = CByte((Pop8() And (Not FlagB)) Or FlagU)
        Dim lo As Byte = Pop8()
        Dim hi As Byte = Pop8()
        _pc = CUShort(lo Or (CUShort(hi) << 8))
        Return 6
    End Function

    Private Function Op_RTS() As Integer
        Dim lo As Byte = Pop8()
        Dim hi As Byte = Pop8()
        _pc = CUShort(lo Or (CUShort(hi) << 8))
        _pc = CUShort(_pc + 1)
        Return 6
    End Function

    Private Function Op_JSR() As Integer
        Dim addr As UShort = Fetch16()
        Dim ret As UShort = CUShort(_pc - 1)
        Push8(CByte((ret >> 8) And &HFF))
        Push8(CByte(ret And &HFF))
        _pc = addr
        Return 6
    End Function

    Private Function Op_JMP_Ind() As Integer
        Dim ptr As UShort = Fetch16()
        ' 6502 page wrap bug: if ptr is $xxFF, high byte comes from $xx00
        Dim lo As Byte = Read8(ptr)
        Dim hiAddr As UShort = CUShort((ptr And &HFF00US) Or ((ptr + 1) And &HFFUS))
        Dim hi As Byte = Read8(hiAddr)
        _pc = CUShort(lo Or (CUShort(hi) << 8))
        Return 5
    End Function

    ' ========== Helpers ==========

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
        Return CUShort(lo Or (CUShort(hi) << 8))
    End Function

    Private Sub Push8(v As Byte)
        Write8(CUShort(&H100US Or _sp), v)
        _sp = CByte((_sp - 1) And &HFF)
    End Sub

    Private Function Pop8() As Byte
        _sp = CByte((_sp + 1) And &HFF)
        Return Read8(CUShort(&H100US Or _sp))
    End Function

    Private Function GetFlag(flag As Byte) As Boolean
        Return (_p And flag) <> 0
    End Function

    Private Sub SetFlag(flag As Byte)
        _p = CByte(_p Or flag)
    End Sub

    Private Sub ClearFlag(flag As Byte)
        _p = CByte(_p And (Not flag))
    End Sub

    Private Sub SetOrClearFlag(flag As Byte, condition As Boolean)
        If condition Then
            SetFlag(flag)
        Else
            ClearFlag(flag)
        End If
    End Sub

    Private Sub SetZN(v As Byte)
        SetOrClearFlag(FlagZ, v = 0)
        SetOrClearFlag(FlagN, (v And &H80) <> 0)
    End Sub
End Class
