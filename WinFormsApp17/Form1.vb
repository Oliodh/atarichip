Imports System.IO
Imports System.Windows.Forms

Public Class Form1
    Private _selectedRomPath As String
    Private _emulator As VcsEmulator
    Private ReadOnly _frameBuffer As Integer() = New Integer(AtariTia.FrameWidth * AtariTia.FrameHeight - 1) {}

    Public Sub New()
        InitializeComponent()
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Using dialog As New OpenFileDialog()
            dialog.Filter = "Atari ROMs (*.bin;*.rom;*.a26)|*.bin;*.rom;*.a26|All files (*.*)|*.*"
            dialog.Title = "Choose ROM"

            If dialog.ShowDialog(Me) = DialogResult.OK Then
                _selectedRomPath = dialog.FileName
                Dim rom As Byte() = File.ReadAllBytes(_selectedRomPath)
                _emulator = New VcsEmulator(rom)
                _emulator.Reset()
                Text = $"AtariChip - {Path.GetFileName(_selectedRomPath)}"
            End If
        End Using
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        If _emulator Is Nothing Then
            MessageBox.Show(Me, "Load a ROM first.", "Reset", MessageBoxButtons.OK, MessageBoxIcon.Information)
            Return
        End If

        _emulator.Reset()
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        If _emulator Is Nothing Then
            MessageBox.Show(Me, "Choose a ROM first.", "Run", MessageBoxButtons.OK, MessageBoxIcon.Information)
            Return
        End If

        _emulator.RunFrame(_frameBuffer)
    End Sub
End Class
