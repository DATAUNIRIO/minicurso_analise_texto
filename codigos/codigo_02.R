#------------------------------------------------------------
#------------------------------------------------------------
#  Revisão 
#------------------------------------------------------------
#------------------------------------------------------------

library(readtext)
library(tidytext)
library(dplyr)
dom = readtext("https://raw.githubusercontent.com/DATAUNIRIO/minicurso_analise_texto/main/dados/txt/Dom_Casmurro.txt")
head(dom$text)

library(stopwords)
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

library(tidyr)
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
rede_bigrama <- contagem_bigramas %>%
  filter(n > 17) %>%
  graph_from_data_frame()

rede_bigrama

library(ggraph)
set.seed(12345)

ggraph(rede_bigrama, layout = 'kk') +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) 

#----------------------------------------------------------------------------
# Machado de Assis
link_dados = 'https://github.com/DATAUNIRIO/minicurso_analise_texto/raw/main/dados/rdata/frases_dom_casmurro.RDATA'
load(url(link_dados))
# Euclides da Cunha
link_dados = 'https://github.com/DATAUNIRIO/minicurso_analise_texto/raw/main/dados/rdata/frases_sertoes.RDATA'
load(url(link_dados))
# Guimarães Rosa
#link_dados = 'https://github.com/DATAUNIRIO/minicurso_analise_texto/raw/main/dados/rdata/frases_guimaraes_rosa.RData'
#load(url(link_dados))

names(frases_dom_casmurro)
names(frases_sertoes)

#---------------------------------------------------------
# tf-idf 
#---------------------------------------------------------

frases_dom_casmurro   = frases_dom_casmurro %>% select(text)
frases_sertoes        = frases_sertoes %>% select(sents)
# frases_guimaraes_rosa = frases_guimaraes_rosa %>% select(word)

tidy_MA  = frases_dom_casmurro %>% unnest_tokens(word, text)
tidy_EC  = frases_sertoes %>% unnest_tokens(word, sents)
# tidy_GR  = frases_guimaraes_rosa %>% unnest_tokens(word, word)


tidy_MA = tidy_MA %>% anti_join(palavras_banidas) %>%   count(word, sort = TRUE)
tidy_EC = tidy_EC %>% anti_join(palavras_banidas) %>%   count(word, sort = TRUE)
#tidy_GR = tidy_GR %>% anti_join(palavras_banidas) %>%   count(word, sort = TRUE)

tidy_MA$autor = 'Machado de Assis'
tidy_EC$autor = 'Euclides da Cunha'
#tidy_GR$autor = 'Guimarães Rosa'

livros = tidy_MA %>% add_row(tidy_EC)
livros_tf_idf <- livros %>% bind_tf_idf(word, autor, n)

livros_tf_idf %>%
  arrange(desc(tf_idf))

library(forcats)

livros_tf_idf %>%
  group_by(autor) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = autor)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~autor, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


#-------------------------------------------------------
#-------------------------------------------------------
# install.packages("devtools")
# devtools::install_github("hadley/emo")
#-------------------------------------------------------
#-------------------------------------------------------

library(emo)
library(tidyr)
library(dplyr)
library(reactable)

load(url("https://github.com/GIEL-Investigacao-Eleitoral/analise_textos/raw/main/dados/rdata/banco_tweets.RData"))
table(banco_tweets$veiculo)

emoji<-banco_tweets %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(50) 

emoji %>%  reactable()

#Top emoji por veiculo
emoji_por_veiculo<-banco_tweets %>%
  group_by(veiculo) %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(5)

emoji_por_veiculo %>%  reactable()
