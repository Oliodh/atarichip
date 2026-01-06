Imports System.Drawing
Imports System.Drawing.Imaging
Imports System.IO
Imports System.Runtime.InteropServices
Imports System.Windows.Forms

Public Class Form1
    Private _selectedRomPath As String
    Private _emulator As VcsEmulator
    Private ReadOnly _frameBuffer As Integer() = New Integer(AtariTia.FrameWidth * AtariTia.FrameHeight - 1) {}
    Private ReadOnly _frameBufferHandle As GCHandle
    Private ReadOnly _bitmap As Bitmap
    Private _frameBufferPinned As Boolean
    Private _isRendering As Boolean

    Public Sub New()
        InitializeComponent()
        Try
            _frameBufferHandle = GCHandle.Alloc(_frameBuffer, GCHandleType.Pinned)
            _frameBufferPinned = True
            _bitmap = New Bitmap(AtariTia.FrameWidth, AtariTia.FrameHeight, AtariTia.FrameWidth * 4, PixelFormat.Format32bppArgb, _frameBufferHandle.AddrOfPinnedObject())
            PictureBox1.Image = _bitmap
            
            ' Enable key preview to capture keyboard input
            Me.KeyPreview = True
        Catch ex As Exception
            _frameBufferHandle = New GCHandle()
            _frameBufferPinned = False
            _bitmap = Nothing
            MessageBox.Show(Me, $"Failed to prepare frame buffer: {ex.Message}", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub

    Private Function EnsureEmulatorLoaded(caption As String) As Boolean
        If _emulator Is Nothing Then
            MessageBox.Show(Me, "Choose a ROM first.", caption, MessageBoxButtons.OK, MessageBoxIcon.Information)
            Return False
        End If

        Return True
    End Function

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Using dialog As New OpenFileDialog()
            dialog.Filter = "Atari ROMs (*.bin;*.rom;*.a26)|*.bin;*.rom;*.a26|All files (*.*)|*.*"
            dialog.Title = "Choose ROM"

            If dialog.ShowDialog(Me) = DialogResult.OK Then
                Try
                    FrameTimer.Stop()
                    _selectedRomPath = dialog.FileName
                    Dim rom As Byte() = File.ReadAllBytes(_selectedRomPath)
                    _emulator = New VcsEmulator(rom)
                    _emulator.Reset()
                    Text = $"AtariChip - {Path.GetFileName(_selectedRomPath)}"
                    RunFrameAndRender()
                    FrameTimer.Start()
                Catch ex As Exception
                    _selectedRomPath = Nothing
                    _emulator = Nothing
                    FrameTimer.Stop()
                    MessageBox.Show(Me, $"Failed to load ROM: {ex.Message}", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            End If
        End Using
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        If Not EnsureEmulatorLoaded("Reset") Then Return

        _emulator.Reset()
        RunFrameAndRender()
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        If Not EnsureEmulatorLoaded("Run") Then Return

        RunFrameAndRender()
    End Sub

    Private Sub FrameTimer_Tick(sender As Object, e As EventArgs) Handles FrameTimer.Tick
        RunFrameAndRender()
    End Sub

    Private Sub RunFrameAndRender()
        If _emulator Is Nothing OrElse _isRendering Then Return
        _isRendering = True

        Try
            _emulator.RunFrame(_frameBuffer)
            RenderFrame()
        Catch ex As Exception
            FrameTimer.Stop()
            MessageBox.Show(Me, $"Emulation error: {ex.Message}", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        Finally
            _isRendering = False
        End Try
    End Sub

    Private Sub RenderFrame()
        ' Bitmap is backed by the pinned frame buffer; refresh to display latest pixels.
        PictureBox1.Invalidate()
        PictureBox1.Update()
    End Sub

    Protected Overrides Sub OnFormClosed(e As FormClosedEventArgs)
        FrameTimer.Stop()
        If _frameBufferPinned AndAlso _frameBufferHandle.IsAllocated Then _frameBufferHandle.Free()
        If _bitmap IsNot Nothing Then _bitmap.Dispose()
        MyBase.OnFormClosed(e)
    End Sub

    Protected Overrides Sub OnKeyDown(e As KeyEventArgs)
        MyBase.OnKeyDown(e)
        If _emulator Is Nothing Then Return

        Select Case e.KeyCode
            Case Keys.Up
                _emulator.Riot.SetJoystick0Up(True)
            Case Keys.Down
                _emulator.Riot.SetJoystick0Down(True)
            Case Keys.Left
                _emulator.Riot.SetJoystick0Left(True)
            Case Keys.Right
                _emulator.Riot.SetJoystick0Right(True)
            Case Keys.Space, Keys.ControlKey
                _emulator.Tia.SetFireButton0(True)
            Case Keys.F2
                _emulator.Riot.SetConsoleSelect(True)
            Case Keys.F3
                _emulator.Riot.SetConsoleReset(True)
        End Select

        e.Handled = True
    End Sub

    Protected Overrides Sub OnKeyUp(e As KeyEventArgs)
        MyBase.OnKeyUp(e)
        If _emulator Is Nothing Then Return

        Select Case e.KeyCode
            Case Keys.Up
                _emulator.Riot.SetJoystick0Up(False)
            Case Keys.Down
                _emulator.Riot.SetJoystick0Down(False)
            Case Keys.Left
                _emulator.Riot.SetJoystick0Left(False)
            Case Keys.Right
                _emulator.Riot.SetJoystick0Right(False)
            Case Keys.Space, Keys.ControlKey
                _emulator.Tia.SetFireButton0(False)
            Case Keys.F2
                _emulator.Riot.SetConsoleSelect(False)
            Case Keys.F3
                _emulator.Riot.SetConsoleReset(False)
        End Select

        e.Handled = True
    End Sub
End Class
