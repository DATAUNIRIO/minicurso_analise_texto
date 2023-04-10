#-------------------------------------------------
# Texto como dado (text as data)
# prof. Steven Ross
#-------------------------------------------------



library(readtext)
dom = readtext("C:/Users/Hp/Documents/GitHub/minicurso_analise_texto/dados/Dom_Casmurro.txt")

library(tidytext)
library(dplyr)
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

mais_palavras = tibble(word=c("é","á"))
palavras_banidas = palavras_banidas %>% add_row(mais_palavras)

contagem = tidy_dom %>%
  anti_join(palavras_banidas) %>% 
  tibble() %>%
  count(word, sort = TRUE)  %>% 
  print(n=50)


library(wordcloud2)
wordcloud2(demoFreq)
#wordcloud2(contagem)

library(corpus)
text <- "love loving lovingly loved lover lovely love"
text

text_tokens(text) 

# Stemming é o ato de remover inflexões de uma palavra 
text_tokens(text, stemmer = "en") # english stemmer



texto <- "caminhar caminhando caminhou andou andar andando"
texto
text_tokens(texto) 
text_tokens(texto, stemmer = "pt") 

#remotes::install_github("DATAUNIRIO/lemmar")
library(lemmar)
lemmatize_pt(texto)
lemmatize_pt(Poeminho_do_Contra)


#library(spacyr)
parsedtxt <- spacy_parse(texto)
parsedtxt

spacy_parse(Poeminho_do_Contra)

