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


