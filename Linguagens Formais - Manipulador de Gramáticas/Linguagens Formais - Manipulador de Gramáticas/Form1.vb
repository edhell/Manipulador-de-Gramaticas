Public Class Form1

    Private naoTerminais As New List(Of String)
    Private terminais As New List(Of String)
    Private simboloInicial As String
    Private producoesLetra As String = "P"
    Private producoes As New List(Of String)
    Private ok_nTerm As Boolean = False
    Private ok_term As Boolean = False
    Private ok_simbolo As Boolean = False
    Private ok_producoes As Boolean = False

    Private Enum gramatica
        GI      ' Gramática Irrestritas, Tipo 0
        GSC     ' Gramática Sensíveis ao contexto, Tipo 1
        GLC     ' Gramática Livre de contexto, Tipo 2
        GR      ' Gramática Regular, Tipo 3
    End Enum

#Region "Botões"
    '' BOTAO ADD →
    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        If Not TextBox5.Text.Contains("→") Then : TextBox5.AppendText("→") : End If
    End Sub

    '' BOTAO ADD ε
    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        If Not TextBox5.Text.Contains("ε") Then : TextBox5.AppendText("ε") : End If
    End Sub

    '' BOTAO ADD Divisor para add mais "saídas" à Produção
    Private Sub Button9_Click(sender As Object, e As EventArgs) Handles Button9.Click
        If TextBox5.Text.Trim.Last <> "|" Then : TextBox5.AppendText(" | ") : End If
    End Sub

    '' BOTAO CARREGAR ARQUIVO
    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        MsgBox("Carregar arquivo, em desenvolvimento...")
    End Sub

    '' BOTAO GERAR SENTENÇAS ALEATORIAMENTE
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        If producoes.Count = 0 Then
            informar("Adicione ao menos uma 'Produção'.")
            Return
        End If
        If Not ok_nTerm Then
            informar("Informe 'não-terminais'.")
            Return
        End If
        If Not ok_term Then
            informar("Informe 'terminais'.")
            Return
        End If
        If Not ok_simbolo Then
            informar("Selecione o 'Símbolo Inicial'.")
            Return
        End If
        If Not ok_producoes Then
            informar("Há algum erro nas produções.")
            Return
        End If

        '' Tudo OK!, agora só gerar as sentenças: (pelo menos 3)
        informar("Gerando sentenças... Em desenvolvimento...")
        ' S→Aa | ε | S | 
        Try
            Dim _ladoEsquerdo As New List(Of String)
            Dim _ladoDireito As New List(Of String)

            Dim sentenca As String = ""
            Dim cnt As Integer = 0

            For Each p In producoes
                If Not String.IsNullOrWhiteSpace(p) Then
                    Dim x() As String = p.Split("→")
                    _ladoEsquerdo.Add(x(0))
                    _ladoDireito.Add(x(1))
                End If
            Next

            '' Pega o primeiro item:
            sentenca = _ladoDireito(0)

            For index = 1 To NumericUpDown1.Value

                Dim nTermIndex As Integer = temNterminal(sentenca, _ladoEsquerdo)

                While nTermIndex > -1

                    For Each i In _ladoEsquerdo
                        If sentenca(nTermIndex) = i Then
                            sentenca = sentenca.Replace(i, _ladoDireito(cnt))

                        End If
                        cnt += 1
                    Next
                    nTermIndex = temNterminal(sentenca, _ladoEsquerdo)
                End While

                RichTextBox2.AppendText(sentenca & vbNewLine)
                sentenca = _ladoDireito(index)
                cnt = 0

                'For Each i In _ladoEsquerdo
                '    If sentenca.Contains(i) Then
                '        sentenca.Replace(i, _ladoDireito(cnt))

                '    End If
                '    cnt += 1
                'Next
            Next


        Catch ex As Exception
            informar("Erro ao Gerar sentenças. " & ex.Message)
        End Try


    End Sub

    '' BOTAO VERIFICAR
    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        MsgBox("As verificações são automáticas.")

    End Sub

    '' BOTAO AJUDA
    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        MsgBox("Em desenvolvimento...")
    End Sub

    '' BOTAO SAIR
    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        Me.Close()
    End Sub

    '' BOTAO ADD PRODUÇÃO A LISTA
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        producoes.Clear()

        '' Verifica se há várias "saidas" em uma produção
        Try
            Dim x() As String = TextBox5.Text.Split("→")
            Dim ladoEsquerdo = x(0) & "→"
            Dim ladoDireito = x(1)

            '' Para cada "saída":
            If ladoDireito.Contains("|") Then
                Dim saidas() As String = ladoDireito.Split("|")
                For Each s In saidas
                    If s.Trim <> "" Then
                        RichTextBox1.AppendText(ladoEsquerdo & s.Trim & vbNewLine)
                    End If

                Next
            Else
                RichTextBox1.AppendText(TextBox5.Text & vbNewLine)
            End If

        Catch ex As Exception
            informar("Ocorreu um erro ao verificar produção. " & ex.Message)
            RichTextBox1.AppendText(TextBox5.Text & vbNewLine)
        End Try


        For Each linha In RichTextBox1.Lines
            producoes.Add(linha.Trim)
        Next

        'verificarTipoGramatica()
    End Sub

#End Region

#Region "Funções"

    '' Informa ao usuário
    Private Sub informar(v As String)
        ToolStripStatusLabel2.Text = v
    End Sub

    '' FUNCAO CONTA CARACTERES EM UMA STRING
    Private Function ContaCaracter(frase As String, letra As String) As Integer
        Dim I As Integer, wQtd As Integer

        For I = 1 To Len(frase)
            If Mid(frase, I, 1) = letra Then
                wQtd = wQtd + 1
            End If
        Next

        Return wQtd

    End Function

    '' VERIFICA GRAMATICA:
    Private Sub verificaGramatica()
        TextBox4.Text = ""

        If Not ok_nTerm Then : informar("Corrigir entrada 'não terminais'.") : TextBox1.Focus() : Return : End If
        If Not ok_term Then : informar("Corrigir entrada 'terminais'.") : TextBox2.Focus() : Return : End If
        If Not ok_simbolo Then : informar("Selecione um 'Símbolo Inicial'.") : ComboBox1.Focus() : Return : End If
        'If Not ok_producoes Then : informar("Corrigir Produções.") : rich1.Focus() : Return : End If

        If naoTerminais.Count = 0 Then : informar("Adicionar 'não terminais'.") : TextBox1.Focus() : Return : End If
        If terminais.Count = 0 Then : informar("Adicionar 'terminais'.") : TextBox1.Focus() : Return : End If

        '' Símbolos Não terminais:
        TextBox4.Text = "G = ({"
        If naoTerminais.Count > 0 Then
            For Each n In naoTerminais
                TextBox4.AppendText(n & ", ")
            Next

            TextBox4.Text = TextBox4.Text.Substring(0, TextBox4.TextLength - 2)
        End If
        TextBox4.AppendText("}, {")

        '' Símbolos Terminais:
        If terminais.Count > 0 Then
            For Each n In terminais
                TextBox4.AppendText(n & ", ")
            Next

            TextBox4.Text = TextBox4.Text.Substring(0, TextBox4.TextLength - 2)
        End If
        TextBox4.AppendText("}, ")

        '' Simbolo Produções:
        TextBox4.AppendText(producoesLetra & ", ")

        '' Símbolo Inicial:
        TextBox4.AppendText(simboloInicial & ")")

        TextBox5.Text = simboloInicial & "→"
        If naoTerminais.Count > 1 Then : TextBox5.AppendText(naoTerminais.Item(1)) : End If
        If terminais.Count > 0 Then : TextBox5.AppendText(terminais.Item(0)) : End If

    End Sub

    '' VERIFICA PRODUÇÃO
    Private Function verficarProducao()
        If ContaCaracter(TextBox5.Text, "ε") > 1 Then : Return "Erro na 'Produção'." : End If
        If ContaCaracter(TextBox5.Text, "→") > 1 Then : Return "Erro na 'Produção'." : End If
        If ContaCaracter(TextBox5.Text, "→") = 0 Then : Return "Erro na 'Produção'." : End If

        If TextBox5.Text.Length < 3 Then : Return "Erro, 'Produção' incompleta." : End If

        Dim x() As String = TextBox5.Text.Split("→")
        Dim ladoEsquerdo = x(0)
        Dim ladoDireito = x(1)

        '' Verifica se simbodo da esquerda é um não terminal válido:
        If naoTerminais.Contains(ladoEsquerdo) = False And ladoEsquerdo.Length = 1 Then
            Return "Erro, Símbolo 'Não terminal' da produção não é valido."
        End If

        '' Verifica se há várias "saidas" em uma produção
        If ladoDireito.Contains("|") Then
            '...
        End If

        Return ""

    End Function

    '' FUNCAO VERIFICA TIPO DA GRAMATICA
    Private Sub verificarTipoGramatica()
        'GI      ' Gramática Irrestritas, Tipo 0
        'São aquelas às quais nenhuma limitação é imposta.
        '   Lado esquerdo: sequência de quaisquer símbolos, desde que tenha um não-terminal 
        '   Lado direito: qualquer sequência de símbolos, inclusive a setença vazia
        ' P: BC -> CB
        ' PODE TER ε

        'GSC     ' Gramática Sensíveis ao contexto, Tipo 1
        'São aquelas que devem obedecer às regras específicas.
        '   Lado esquerdo: comprimento menor ou igual a sentença do lado direito
        '   Lado direito: comprimento maior ou igual a sentença do lado esquerdo e não é aceita asetença vazia
        ' P: S -> aSBC
        ' NAO PODE TER ε

        'GLC     ' Gramática Livre de contexto, Tipo 2
        'São aquelas que devem obedecer às regras específicas.
        '   Lado esquerdo: sempre ocorrer um e apenas 1 não-terminal
        '   Lado direito: não é aceita a setença vazia'
        ' NAO PODE TER ε

        'GR      ' Gramática Regular, Tipo 3
        'São aquelas que devem obedecer às regras específicas.
        '   Lado esquerdo: deve ocorrer sempre um e apenas um não-terminal
        '   Lado direito: pode ocorrer somente um terminal ou um terminal seguido de um não terminal
        ' PODE TER ε
        Dim gi As Boolean = True
        Dim gsc As Boolean = True
        Dim glc As Boolean = True
        Dim gr As Boolean = True

        Try
            For Each p In producoes
                If p.Contains("→") Then

                    If p.Contains("ε") Then
                        gsc = False
                        glc = False
                    End If

                    Dim x() As String = p.Split("→")
                    Dim ladoEsquerdo = x(0).Trim
                    Dim ladoDireito = x(1).Trim

                    '' Gramatica Regular:
                    If ladoEsquerdo.Length > 1 Then : gr = False : End If
                    If ladoDireito.Length > 2 Then : gr = False : End If

                    '' Gramática Irrestrita:
                    If ladoEsquerdo.Length < 1 Then : gi = False : End If
                    If ladoDireito.Length < 1 Then : gi = False : End If

                    '' Gramática Sensíveis ao Contexto:
                    If ladoEsquerdo.Length >= ladoDireito.Length Then : gsc = False : End If
                    'If ladoDireito.Length <= ladoEsquerdo.Length Then : gsc = False : End If

                    '' Gramática Livres de Contexto:
                    If ladoEsquerdo.Length > 1 Then : glc = False : End If

                End If
            Next

            '' Verifica qual gramática é:
            ok_producoes = True
            If gr = True Then
                Label1.Text = "Gramática: GR - Tipo 3"
                informar("Gramática Regular - Tipo 3")
            ElseIf glc = True Then
                Label1.Text = "Gramática: GLC - Tipo 2"
                informar("Gramática Livre de Contexto - Tipo 2")
            ElseIf gsc = True Then
                Label1.Text = "Gramática: GSC - Tipo 1"
                informar("Gramática Sensível ao Contexto - Tipo 1")
            ElseIf gi = True Then
                Label1.Text = "Gramática: GI - Tipo 0"
                informar("Gramática Irrestrita - Tipo 0")
            Else
                Label1.Text = "Gramática: Erro"
                informar("Gramática fora dos padrões conhecidos.")
                ok_producoes = False
            End If

        Catch ex As Exception
            informar("Erro ao verificar produções, deve haver erros. " & ex.Message)
            ok_producoes = False
        End Try


    End Sub

    '' FUNCAO VERIFICA SE AINDA TEM ALGUM TERMINAL
    Private Function temNterminal(sentenca As String, ladoEsquerdo As List(Of String)) As Integer
        For Each i In ladoEsquerdo
            If sentenca.Contains(i) Then
                Return sentenca.IndexOf(i)
            End If
        Next
        Return -1

    End Function

#End Region

#Region "Eventos de Verificação"

    '' AO DIGITAR NAO TERMINAL
    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged
        ComboBox1.Items.Clear()
        naoTerminais.Clear()
        ComboBox1.SelectedIndex = -1
        If TextBox1.Text = "" Then : ComboBox1.Text = "Adicione um Não terminal." : End If

        '' Deixa todas maiúsculas:
        'TextBox1.Text = TextBox1.Text.ToUpper
        'TextBox1.Select(TextBox1.Text.Length, TextBox1.Text.Length)

        '' Verifica integridade e adiciona à seleção dos Simbolos Iniciais:
        Try
            Dim nTerm() = TextBox1.Text.Split(",")
            For Each n In nTerm
                If n.Trim.Length > 1 Then
                    informar("Entrada 'não terminais' contem erros.")
                    ok_nTerm = False
                    Return
                ElseIf n.Trim <> "" Then
                    informar("Esperando...")
                    naoTerminais.Add(n.Trim)
                    ok_nTerm = True
                    ComboBox1.Items.Add(n.Trim)

                End If
            Next
        Catch ex As Exception
            informar("Erro ao verificar não terminais. " & ex.Message)
        End Try

        '' Seleciona o primeiro (caso tenha):
        If ComboBox1.Items.Count > 0 Then
            ComboBox1.SelectedIndex = 0
        End If

        verificaGramatica()

    End Sub

    '' Corrige entrada dos Não Terminais:
    Private Sub TextBox1_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox1.KeyPress
        e.KeyChar = e.KeyChar.ToString.ToUpper
    End Sub

    '' AO DIGITAR TERMINAL
    Private Sub TextBox2_TextChanged(sender As Object, e As EventArgs) Handles TextBox2.TextChanged
        terminais.Clear()

        Try
            Dim nTerm() = TextBox2.Text.Split(",")
            For Each n In nTerm
                If n.Trim.Length > 1 And n.Trim <> "" Then
                    informar("Entrada 'terminais' contem erros.")
                    ok_term = False
                    Return
                ElseIf n.Trim <> "" Then
                    informar("Esperando...")
                    terminais.Add(n.Trim)
                    ok_term = True

                End If
            Next
        Catch ex As Exception
            informar("Erro ao verificar terminais. " & ex.Message)
        End Try

        verificaGramatica()

    End Sub

    '' Corrige entrada dos Terminais
    Private Sub TextBox2_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox2.KeyPress
        e.KeyChar = e.KeyChar.ToString.ToLower
    End Sub

    '' AO SELECIONAR SIMBOLO INICIAL
    Private Sub ComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox1.SelectedIndexChanged

        If ComboBox1.SelectedIndex > -1 Then
            simboloInicial = ComboBox1.SelectedItem
            ok_simbolo = True
        Else
            ok_simbolo = False
        End If

        verificaGramatica()

    End Sub

    '' AO DIGITAR PRODUÇÃO
    Private Sub TextBox5_TextChanged(sender As Object, e As EventArgs) Handles TextBox5.TextChanged
        Dim retorno As String = verficarProducao()

        If retorno = "" Then
            informar("Esperando...")
            Button1.Enabled = True
        Else
            informar(retorno)
            Button1.Enabled = False
            'TextBox5.Focus()
        End If

    End Sub

    '' AO MODIFICAR PRODUÇÕES:
    Private Sub RichTextBox1_TextChanged(sender As Object, e As EventArgs) Handles RichTextBox1.TextChanged
        producoes.Clear()
        For Each linha In RichTextBox1.Lines
            producoes.Add(linha.Trim)
        Next

        verificarTipoGramatica()
    End Sub

#End Region

End Class
