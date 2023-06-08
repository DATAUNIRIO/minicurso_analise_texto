#------------------------------------------------------------
#------------------------------------------------------------
#  Revisão 
#------------------------------------------------------------
#------------------------------------------------------------

library(readtext)
library(tidytext)
library(dplyr)
library(stopwords)
library(tidyr)


dom = readtext("https://raw.githubusercontent.com/DATAUNIRIO/minicurso_analise_texto/main/dados/txt/Dom_Casmurro.txt")
head(dom$text)

palavras_banidas = stopwords("pt")
palavras_banidas = tibble(palavras_banidas)
palavras_banidas = palavras_banidas %>% rename(word=palavras_banidas)

#--------------------------------------------------------------------------------------------------------------
dom$text =  chartr(
  "áéóūáéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
  "aeouaeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC",
  dom$text)
dom$text = tolower(dom$text)
head(dom$text)

#------------------------------------------------------------
#------------------------------------------------------------
#Bigramas 
#------------------------------------------------------------
#------------------------------------------------------------

bigrama_dom <- dom %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrama_dom %>%   tibble()  %>%
  count(bigram, sort = TRUE)

bigramas <- bigrama_dom %>% tibble() %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigramas <- bigramas %>%
  filter(!word1 %in% palavras_banidas$word) %>%
  filter(!word2 %in% palavras_banidas$word)

contagem_bigramas <- bigramas %>% 
  count(word1, word2, sort = TRUE)

contagem_bigramas %>% filter(word1 == "capitu")

#Trigramas
trigrama_dom <- dom %>%  tibble()  %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

trigrama_dom %>% count(trigram, sort = TRUE)

#n-gramas
library(igraph)
library(ggraph)

rede_bigrama <- contagem_bigramas %>%
  filter(n > 17) %>%
  graph_from_data_frame()

ggraph(rede_bigrama, layout = 'kk') +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) 

#-------------------------------------------------------
#-------------------------------------------------------
# install.packages("devtools")
# devtools::install_github("hadley/emo")
#-------------------------------------------------------
#-------------------------------------------------------
remove(list = ls())

library(emo)
library(tidyr)
library(dplyr)
library(reactable)

load(url('https://github.com/DATAUNIRIO/minicurso_analise_texto/raw/main/dados/rdata/Paola_Carosella.RData'))

table(Paola$classe)

emoji<-Paola %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(50) 

emoji %>%  reactable()

#Top emoji por classe
emoji_por_classe <-Paola %>%
  group_by(classe) %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(10)

emoji_por_classe %>%  reactable()
