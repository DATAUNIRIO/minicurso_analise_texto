#-------------------------------------------------
# Texto como dado (text as data)
# prof. Steven Ross
#-------------------------------------------------

# vetor
x <- c(0.3, 0.9, 1.1)          
y <- c("jose","maria","joao")          

w <- c("0.2", "12.9", "45.1")          

funcionarios <- data.frame(nome = c("Marx", "Weber", "Durkheim","Arendt", "Maquiavel"),
                           sexo = c("M", "M", "M", "F","M"),
                           salario = c(1000, 1200, 1300, 2000, 500))
funcionarios


turma <- data.frame(nome_aluno = c("Jurgen Habermas", "Karl Popper", "John Rawls","Paulo Freire"),
                    sexo = c("M", "M", "M","M"),
                    CR = c(6.98, 7.01, 7.03, 8.88))
turma

funcionarios$nome
turma$nome_aluno

#--------------------------------------------------
sigla1 <- "FCS/UERJ"
class(sigla3)

sigla2 <- 'FCS/UERJ'
identical(sigla1,sigla2)

sigla3 <- 'Fcs/Uerj'
identical(sigla1,sigla3)


# Atividade 1 -  Como podemos colocar a sigla1 e a sigla3 iguais? (maiusculas e minusculas)

Poeminho_do_Contra = c("Todos esses que aí estão",
                      "Atravancando meu caminho",
                      "Eles passarão...",
                      "Eu passarinho!")

length(Poeminho_do_Contra)

Poeminho_do_Contra[3]
Poeminho_do_Contra[4]

banco_do_contra = data.frame(Poeminho_do_Contra)

# Contando caracteres
# A função nchar() é um forma de se obter o número de caracteres de uma string.

nchar(Poeminho_do_Contra)


toupper(Poeminho_do_Contra)
tolower(Poeminho_do_Contra)
