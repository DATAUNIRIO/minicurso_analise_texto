
# Machado de Assis
link_dados = 'https://github.com/DATAUNIRIO/minicurso_analise_texto/raw/main/dados/rdata/frases_dom_casmurro.RDATA'
load(url(link_dados))
# Guimarães Rosa
link_dados = 'https://github.com/DATAUNIRIO/minicurso_analise_texto/raw/main/dados/rdata/frases_guimaraes_rosa.RData'
load(url(link_dados))
# Euclides da Cunha
link_dados = 'https://github.com/DATAUNIRIO/minicurso_analise_texto/raw/main/dados/rdata/frases_sertoes.RDATA'
load(url(link_dados))

names(frases_dom_casmurro)
names(frases_guimaraes_rosa)
names(frases_sertoes)

#---------------------------------------------------------
# tf-idf 
#---------------------------------------------------------

frases_dom_casmurro   = frases_dom_casmurro %>% select(text)
frases_guimaraes_rosa = frases_guimaraes_rosa %>% select(word)
frases_sertoes        = frases_sertoes %>% select(sents)

tidy_MA  = frases_dom_casmurro %>% unnest_tokens(word, text)
tidy_GR  = frases_guimaraes_rosa %>% unnest_tokens(word, word)
tidy_EC  = frases_sertoes %>% unnest_tokens(word, sents)

library(stopwords)
palavras_banidas = stopwords("pt")
palavras_banidas = tibble(palavras_banidas)
palavras_banidas = palavras_banidas %>% rename(word=palavras_banidas)

tidy_MA = tidy_MA %>% anti_join(palavras_banidas) %>%   count(word, sort = TRUE)
tidy_GR = tidy_GR %>% anti_join(palavras_banidas) %>%   count(word, sort = TRUE)
tidy_EC = tidy_EC %>% anti_join(palavras_banidas) %>%   count(word, sort = TRUE)
  
tidy_MA$autor = 'Machado de Assis'
tidy_GR$autor = 'Guimarães Rosa'
tidy_EC$autor = 'Euclides da Cunha'

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
