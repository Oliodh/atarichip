Imports System.IO
Imports System.Windows.Forms

Public Class Form1
    Private _selectedRomPath As String
    Private _emulator As VcsEmulator
    Private ReadOnly _frameBuffer As Integer() = New Integer(AtariTia.FrameWidth * AtariTia.FrameHeight - 1) {}

    Public Sub New()
        InitializeComponent()
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
                    _selectedRomPath = dialog.FileName
                    Dim rom As Byte() = File.ReadAllBytes(_selectedRomPath)
                    _emulator = New VcsEmulator(rom)
                    _emulator.Reset()
                    Text = $"AtariChip - {Path.GetFileName(_selectedRomPath)}"
                Catch ex As Exception
                    _selectedRomPath = Nothing
                    _emulator = Nothing
                    MessageBox.Show(Me, $"Failed to load ROM: {ex.Message}", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            End If
        End Using
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        If Not EnsureEmulatorLoaded("Reset") Then Return

        _emulator.Reset()
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        If Not EnsureEmulatorLoaded("Run") Then Return

        _emulator.RunFrame(_frameBuffer)
    End Sub
End Class
