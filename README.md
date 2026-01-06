# AtariChip - Atari 2600 Emulator

A work-in-progress Atari 2600 emulator written in VB.NET using Windows Forms.

## Current Status

This emulator is **not yet complete** enough to fully run games like Space Invaders. It is currently in early development.

### What's Implemented

- **6502 CPU**: Fully implemented with all standard opcodes and common undocumented NOPs
- **Playfield Rendering**: PF0, PF1, PF2 with mirror/repeat modes
- **Background Color**: COLUBK
- **Playfield Color**: COLUPF
- **RIOT Timer**: TIM1T, TIM8T, TIM64T, T1024T with underflow handling
- **RIOT RAM**: 128 bytes of RAM
- **Basic TIA Sync**: WSYNC support
- **NTSC Palette**: 128-color palette

### What's Missing (Not Yet Implemented)

- **Player/Sprite Graphics**: GRP0, GRP1 (essential for game characters and enemies)
- **Missile Graphics**: ENAM0, ENAM1
- **Ball Graphics**: ENABL
- **Position Registers**: RESP0, RESP1, RESM0, RESM1, RESBL
- **Motion Registers**: HMP0, HMP1, HMM0, HMM1, HMBL
- **Collision Detection**: All collision registers return 0
- **Player/Missile Copies**: NUSIZ0, NUSIZ1 (stubbed)
- **Input Handling**: Joystick and console switches are not connected to keyboard
- **Audio**: TIA sound registers (AUDC0, AUDC1, AUDF0, AUDF1, AUDV0, AUDV1)
- **Bank Switching**: Only basic 4K ROM support (mappers like F8/F6/F4 not implemented)

## Building

Requires .NET 10.0 SDK with Windows Forms support:

```bash
dotnet build WinFormsApp17/WinFormsApp17.vbproj
```

## Usage

1. Click "Choose ROM" to load an Atari 2600 ROM file (.bin, .rom, .a26)
2. Click "Reset" to reset the emulator
3. The emulator runs automatically at ~60 FPS

## License

This project is for educational purposes.