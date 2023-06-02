#-------------------------------------------------
# Texto como dado (text as data)
# prof. Steven Ross
#-------------------------------------------------

x <- c(0.3, 0.9, 1.1)          
y <- c("jose","maria","joao")          

w <- c("0.2", "12.9", "45.1")          

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

#------------------------------------------------------------------------

library(corpus)
text <- "love loving lovingly loved lover lovely love"
text

# Stemming é o ato de remover inflexões de uma palavra 
text_tokens(text) 
text_tokens(text, stemmer = "en") # english stemmer

texto <- "caminhar caminhando caminhou andou andar andando"
texto
text_tokens(texto) 
text_tokens(texto, stemmer = "pt") 

#remotes::install_github("DATAUNIRIO/lemmar")

library(lemmar)
lemmatize_pt(texto)

lemmatize_pt(Poeminho_do_Contra)


#-----------------------------------------------------------------------------
library(readtext)
library(tidytext)
library(dplyr)

dom = readtext("https://raw.githubusercontent.com/DATAUNIRIO/minicurso_analise_texto/main/dados/txt/Dom_Casmurro.txt")

head(dom$text)

#--------------------------------------------------------------------------------------------------------------
dom$text =  chartr(
  "áéóūáéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
  "aeouaeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC",
  dom$text)

dom$text = tolower(dom$text)

head(dom$text)

tidy_dom = dom %>%
  unnest_tokens(word, text)

tidy_dom %>% tibble() %>%
  count(word, sort = TRUE) 

library(stopwords)
palavras_banidas = stopwords("pt")
palavras_banidas = tibble(palavras_banidas)
palavras_banidas = palavras_banidas %>% rename(word=palavras_banidas)

tidy_dom %>%
  anti_join(palavras_banidas) %>% 
  tibble() %>%
  count(word, sort = TRUE) 

palavras_extra = tibble(word = c('tambem'))
palavras_banidas = palavras_banidas %>% add_row(palavras_extra)

TF = tidy_dom %>%
  anti_join(palavras_banidas) %>% 
  tibble() %>%
  count(word, sort = TRUE)  %>% 
  print(n=50)


library(wordcloud2)
wordcloud2(demoFreq)
#wordcloud2(TF)

library(ggplot2)
tidy_dom %>%
  anti_join(palavras_banidas) %>% 
  tibble() %>%
  count(word, sort = TRUE) %>%
  filter(n > 150) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill='red') +
  labs(y = NULL)

#------------------------------------------------------------
#------------------------------------------------------------
# Estudo de caso 1 (nuvem de palavras do sertões)
# Importar
# Transformar em tokens
#------------------------------------------------------------
#------------------------------------------------------------
library(tidytext)
library(dplyr)

link_dados = 'https://github.com/DATAUNIRIO/minicurso_analise_texto/raw/main/dados/rdata/frases_sertoes.RDATA'
load(url(link_dados))

frases_sertoes = frases_sertoes %>% pull(sents) %>% tibble() 
colnames(frases_sertoes) = 'texto'

tidy_sertoes =  frases_sertoes %>%
  unnest_tokens(word, texto)

