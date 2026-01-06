# AtariChip - Atari 2600 Emulator

A work-in-progress Atari 2600 emulator written in VB.NET using Windows Forms.

## Current Status

This emulator now implements all essential graphics and input features for running most Atari 2600 games. It includes sprite rendering, collision detection, and joystick input. However, it still lacks audio and bank switching for larger ROMs.

### What's Implemented

- **6502 CPU**: Fully implemented with all standard opcodes and common undocumented NOPs
- **Playfield Rendering**: PF0, PF1, PF2 with mirror/repeat modes
- **Background Color**: COLUBK
- **Playfield Color**: COLUPF
- **Player/Sprite Graphics**: GRP0, GRP1 with vertical delay support
- **Player Colors**: COLUP0, COLUP1
- **Player Reflection**: REFP0, REFP1 for horizontal mirroring
- **Missile Graphics**: ENAM0, ENAM1 with enable control
- **Ball Graphics**: ENABL with enable control
- **Position Registers**: RESP0, RESP1, RESM0, RESM1, RESBL for positioning objects
- **Motion Registers**: HMP0, HMP1, HMM0, HMM1, HMBL with HMOVE and HMCLR
- **Size and Copies**: NUSIZ0, NUSIZ1 for player/missile sizing and multiple copies
- **Collision Detection**: All collision registers (CXM0P, CXM1P, CXP0FB, CXP1FB, CXM0FB, CXM1FB, CXBLPF, CXPPMM)
- **Priority Control**: CTRLPF bit 2 for playfield priority, bit 1 for score mode
- **Input Handling**: Arrow keys for joystick, Space/Ctrl for fire button, F2/F3 for Select/Reset
- **RIOT Timer**: TIM1T, TIM8T, TIM64T, T1024T with underflow handling
- **RIOT RAM**: 128 bytes of RAM
- **Basic TIA Sync**: WSYNC support
- **NTSC Palette**: 128-color palette

### What's Missing (Not Yet Implemented)

- **Audio**: TIA sound registers (AUDC0, AUDC1, AUDF0, AUDF1, AUDV0, AUDV1)
- **Bank Switching**: Only basic 4K ROM support (mappers like F8/F6/F4 not implemented)
- **Paddle/Driving Controller Input**: Only joystick is implemented
- **Latched Input (INPT4/5 latch mode)**: Fire buttons work in immediate mode only

## Building

Requires .NET 10.0 SDK with Windows Forms support:

```bash
dotnet build WinFormsApp17/WinFormsApp17.vbproj
```

## Usage

1. Click "Choose ROM" to load an Atari 2600 ROM file (.bin, .rom, .a26)
2. Click "Reset" to reset the emulator
3. The emulator runs automatically at ~60 FPS

### Controls

- **Arrow Keys**: Joystick movement (Up, Down, Left, Right)
- **Space**: Fire button
- **F2**: Console Select switch
- **F3**: Console Reset switch

## License

This project is for educational purposes.