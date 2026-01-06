<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class Form1
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    Friend WithEvents Button1 As Button
    Friend WithEvents Button2 As Button
    Friend WithEvents Button3 As Button
    Friend WithEvents PictureBox1 As PictureBox
    Friend WithEvents FrameTimer As Timer

    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        components = New System.ComponentModel.Container()
        Button1 = New Button()
        Button2 = New Button()
        Button3 = New Button()
        PictureBox1 = New PictureBox()
        FrameTimer = New Timer(components)
        CType(PictureBox1, ComponentModel.ISupportInitialize).BeginInit()
        SuspendLayout()
        '
        'Button1
        '
        Button1.Location = New Point(12, 12)
        Button1.Name = "Button1"
        Button1.Size = New Size(100, 30)
        Button1.TabIndex = 0
        Button1.Text = "Choose ROM"
        Button1.UseVisualStyleBackColor = True
        '
        'Button2
        '
        Button2.Location = New Point(118, 12)
        Button2.Name = "Button2"
        Button2.Size = New Size(100, 30)
        Button2.TabIndex = 1
        Button2.Text = "Reset"
        Button2.UseVisualStyleBackColor = True
        '
        'Button3
        '
        Button3.Location = New Point(224, 12)
        Button3.Name = "Button3"
        Button3.Size = New Size(100, 30)
        Button3.TabIndex = 2
        Button3.Text = "Run"
        Button3.UseVisualStyleBackColor = True
        '
        'PictureBox1
        '
        PictureBox1.Location = New Point(12, 48)
        PictureBox1.Name = "PictureBox1"
        PictureBox1.Size = New Size(320, 384)
        PictureBox1.SizeMode = PictureBoxSizeMode.StretchImage
        PictureBox1.TabIndex = 3
        PictureBox1.TabStop = False
        '
        'FrameTimer
        '
        FrameTimer.Interval = 17 ' ~60 FPS (17 ms)
        '
        'Form1
        '
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(800, 450)
        Controls.Add(PictureBox1)
        Controls.Add(Button3)
        Controls.Add(Button2)
        Controls.Add(Button1)
        Name = "Form1"
        Text = "Form1"
        CType(PictureBox1, ComponentModel.ISupportInitialize).EndInit()
        ResumeLayout(False)
    End Sub

    End Class
