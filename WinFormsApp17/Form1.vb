Imports System.Drawing
Imports System.Drawing.Imaging
Imports System.IO
Imports System.Runtime.InteropServices
Imports System.Windows.Forms

Public Class Form1
    Private _selectedRomPath As String
    Private _emulator As VcsEmulator
    Private ReadOnly _frameBuffer As Integer() = New Integer(AtariTia.FrameWidth * AtariTia.FrameHeight - 1) {}
    Private ReadOnly _frameBufferHandle As GCHandle = GCHandle.Alloc(_frameBuffer, GCHandleType.Pinned)
    Private ReadOnly _bitmap As New Bitmap(AtariTia.FrameWidth, AtariTia.FrameHeight, AtariTia.FrameWidth * 4, PixelFormat.Format32bppArgb, _frameBufferHandle.AddrOfPinnedObject())
    Private _isRendering As Boolean

    Public Sub New()
        InitializeComponent()
        PictureBox1.Image = _bitmap
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
        If _bitmap IsNot Nothing Then
            Try
                _bitmap.Dispose()
            Catch
                ' Ignore dispose failures while tearing down the form.
            End Try
        End If
        If _frameBufferHandle.IsAllocated Then _frameBufferHandle.Free()
        MyBase.OnFormClosed(e)
    End Sub
End Class
