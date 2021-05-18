# ANALISIS DE LOS PLANES DE GOBIERNO DE LOS PARTIDOS POLITICOS PARA LA SEGUNDA VUELTA 2021

# INSTALACION DE PAQUETES NECESARIOS
library(pdftools) # PARA MANIPULAR ARCHIVOS PDF
library(tidyverse) # PARA TRANSFORMAR TABLAS
library(stringr) # PARA TRANSFORMAR TEXTOS
library(tidytext) # PARA MINERIA DE TEXTOS
library(stopwords) # PARA IDENTIFICAR PALABRAS VACIAS
library(ggplot2) # PARA GRAFICAR RESULTADOS
library(scales) # PARA MANIPULAR LAS ESCALAS DE LOS GRAFICOS
library(zip) # PARA DESCOMPRIMIR ARCHIVOS
library(wordcloud) # GRAFICAR NUBES DE PLABRAS
library(reshape2) # TRATAR MARCOS DE DATOS
library(forcats) # CAMBIAR VALORES DE NIVEL
library(igraph) # ANALISIS Y VISUALIZACION DE REDES
library(ggraph) # GRAFICAR REDES DE PALABRAS CAPA POR CAPA
library(widyr) # AMPLIAR, PROCESAR Y VOLVER A ORDENAR LOS DATOS
library(tm) # MINERIA DE TEXTO
library(topicmodels) # CLASIFICACION NO SUPERVISADA DE TEXTO - LDA
library(scales) # EDITAR EL ESCALADO EN LOS GRAFICOS

# CARGA DE ARCHIVOS PARA ANALISIS

# DESCARGA DE PLANES
download.file(url = "https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16490.pdf", mode = "wb",
              destfile = '../10.- Plan_gob21/FP.pdf')

download.file(url = "https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16542.pdf", mode = "wb",
              destfile = '../10.- Plan_gob21/PL.pdf')

# LIMPIEZA DE LO QUE NO ES PLAN DE GOBIERNO

# 79 PAGINAS DE PLAN, NO INCLUYE PORTADA, INTRODUCCION, INDICE Y RESUMENES
pdf_fp <- tibble(text = unlist(strsplit(pdf_text('../10.- Plan_gob21/FP.pdf'), "\n")[7:80])) 

# 68 PAGINAS PLAN, NO INCLUYE PORTADA, DEDICATORIAS, INTRODUCCION, INDICE, HOJA DE VIDA DEL AUTOR 
pdf_pl <- tibble(text = unlist(strsplit(pdf_text('../10.- Plan_gob21/PL.pdf'), "\n")[8:75]))

# PLANES ESTRUCTURADOS
plan_fp <- left_join(left_join(
pdf_fp %>%
  mutate(autor = "Fuerza Popular",
         linenumber = row_number(),
         div1 = cumsum(str_detect(text,"(Pilar Estratégico|Eje\\s\\d+)")),
         div2 = cumsum(str_detect(text,"^(\\d|\\d\\d)\\.\\d\\. .+")),
         div2 = ifelse(div2 == 0, 1, div2),
         div2 = str_c(div1,div2)),
pdf_fp %>%
  mutate(div1 = cumsum(str_detect(text,"(Pilar Estratégico|Eje\\s\\d+)")),
         chapter = str_extract(text,"(Pilar Estratégico\\s\\d.+|Eje\\s\\d.+)")) %>%
  filter(!is.na(chapter)) %>%
  select(-text), by = "div1"),
pdf_fp %>%
  mutate(div1 = cumsum(str_detect(text,"(Pilar Estratégico|Eje\\s\\d+)")),
         div2 = cumsum(str_detect(text,"^(\\d|\\d\\d)\\.\\d\\. .+")),
         div2 = ifelse(div2 == 0, 1, div2),
         div2 = str_c(div1,div2),
         section = str_extract(text,"^(\\d|\\d\\d)\\.\\d\\. .+")) %>%
  filter(!is.na(section)) %>%
  select(-text, -div1), by = "div2") %>%
  mutate(section = ifelse(is.na(section), str_extract(chapter, "(?<=Eje ).+"), section)) %>%
  mutate(section = str_replace(section,":",".")) %>%
  select(-div1, -div2, -section)


plan_pl <- left_join(
  tibble(pdf_pl) %>%
  mutate(autor = "Peru Libre",
         linenumber = row_number(),
         div = cumsum(str_detect(text,"(CAPÍTULO [:upper:])")),
         div = ifelse(div == 0, 1, div)),
  tibble(chapter = str_c(c("CAP I", "CAP II", "CAP III", "CAP IV", "CAP V", "CAP VI", "CAP VII", "CAP VIII",
                           "CAP IX", "CAP X", "CAP XI", "CAP XII", "CAP XIII", "CAP XIV", "CAP XV", "CAP XVI",
                           "CAP XVII", "CAP XVIII", "CAP XIX", "CAP XX", "CAP XXI"),
                         c("SOBRE LA NATURALEZA DEL PARTIDO", "HACIA UNA NUEVA CONSTITUCIÓN POLÍTICA",
                           "NUEVO RÉGIMEN ECONÓMICO DEL ESTADO", "NUEVA ESCUELA PÚBLICA ORIENTADA A LA LIBERACIÓN",
                           "NUEVA SALUD PÚBLICA ORIENTADA A INTERESES DEL PUEBLO",
                           "SOBRE POLÍTICA DE TRANSPORTE Y MEDIOS DE COMUNICACIÓN",
                           "POLÍTICA AGRARIA COMO SEGURIDAD NACIONAL", "SOBRE POLÍTICA MEDIO AMBIENTAL",
                           "SOBRE CULTURA Y TURISMO", "SOBRE LA DESCENTRALIZACIÓN", "SOBRE POLÍTICA LABORAL",
                           "SOBRE POLÍTICA ANTICORRUPCIÓN", "POLÍTICA EN SEGURIDAD CIUDADANA", "SOBRE LOS DERECHOS HUMANOS",
                           "SOBRE POLÍTICA DE JUSTICIA", "LA MUJER SOCIALISTA",
                           "SOBRE NUESTRAS PRINCIPALES FUENTES DE RIQUEZA", "SOBRE NUESTRA SOBERANÍA",
                           "NUESTRA POSTURA FRENTE AL EMPRESARIADO PRIVADO", "NUESTROS DERECHOS SOBRE EL MAR TERRITORIAL",
                           "NUESTRA POLÍTICA EXTERIOR"), sep = " - "),
         div = seq(1,21)), by = "div") %>%
  select(-div)


# UNION DE TABLAS
planes_gob <- bind_rows(plan_fp, plan_pl) 

# TOKENIZACION
planes_tokn <- planes_gob %>%
  unnest_tokens(word, text)

# PALABRAS VACIAS
stop_words <- tibble(word = unique(c(data_stopwords_snowball$es,
                                   data_stopwords_stopwordsiso$es,
                                   date_names_lang("es")[[1]],
                                   date_names_lang("es")[[2]],
                                   date_names_lang("es")[[3]],
                                   c("perú","soles", "usd", "perú libre", "fuerza popular", "peruano", "gobierno", "política",
                                     "politica", "peruana","peruano", "peruanos", "peruanas", "país", "libre", "país", "pais",
                                     "capítulo", "millones","mil","miles", "cien", "cientos", "etc", "nacional", "país"))) %>%
                     sort())

# ELIMINAR PALABRAS VACIAS
planes_tokn_cl <- planes_tokn %>%
  anti_join(stop_words, by = "word") %>%
  mutate(num_char = str_detect(word, "[:digit:]")) %>%
  filter(num_char != TRUE) %>%
  select(-num_char)

# CONTEO DE PALABRAS FRECUENTES
planes_tokn_cl %>%
  filter(autor == "Fuerza Popular") %>%
  count(word, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

planes_tokn_cl %>%
  filter(autor == "Peru Libre") %>%
  count(word, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# MIDIENDO LA FRECUENCIA DE PALABRAS
frequency <- planes_tokn_cl %>% 
  count(autor, word) %>%
  group_by(autor) %>%
  mutate(proporcion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = autor, values_from = proporcion) %>%
  pivot_longer(`Peru Libre`, names_to = "autor", values_to = "proporcion")

# GRAFICO DE CORRELACIÓN ENTRE GRUPOS DE PALABRAS
ggplot(frequency, aes(x = proporcion, y = `Fuerza Popular`, 
                      color = abs(`Fuerza Popular` - proporcion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = "Fuerza Popular", x = "Peru Libre") +
  labs(title = "EN QUÉ SE PARECEN Y EN QUÉ NO, LOS DOS PLANES DE GOBIERNO",
       subtitle = "Elecciones 2021 - 2da vuelta, se parecen cuando las palabras están cerca de la línea diagonal
         cáculo de la correlación entre grupos de palabras -> Cor = 0.4554293, p-value < 2.2e-16",
       caption = "Fuentes de información:
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16490.pdf
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16542.pdf
         Elaborado por: @renzo_ruesta https://github.com/Renzo-Ruesta") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# CORRLEACION DE PALABRAS Cor = 0.4554293, p-value < 2.2e-16
cor.test(data = frequency[frequency$autor == "Peru Libre",],
         ~ proporcion + `Fuerza Popular`)


# VECTORES DE PALABRAS PARA ANALISIS DE SENTIMIENTOS

download.file("https://github.com/jboscomendoza/lexicos-nrc-afinn/archive/refs/heads/master.zip",
              '../10.- Plan_gob21/master.zip')

unzip('../10.- Plan_gob21/master.zip', exdir = './16.- Planes de gob/')


afinn <- full_join(tibble(read.csv("../10.- Plan_gob21/lexicos-nrc-afinn-master/lexico_afinn.csv")),
                   tibble(read.csv("../10.- Plan_gob21/lexicos-nrc-afinn-master/lexico_nrc.csv")), 
                   by = c("palabra", "word")) %>%
  select(word = palabra, puntuacion, sentimiento) %>%
  mutate(sent_n_p = if_else(puntuacion < 0, "negativo", "positivo"))

# CONTEO DE PALABRAS CON SENTIMIENTOS
planes_tokn_cl %>%
  filter(autor == "Peru Libre") %>%
  inner_join(afinn %>% 
               filter(sent_n_p == "positivo")) %>%
  count(word, sort = TRUE)

# GRAFICO DE COMPARACION DE SENTIMIENTOS POS Y NEG ENTRE 0301 Y 0302
planes_tokn_cl %>%
  inner_join(afinn, by = "word") %>%
  count(autor, index = linenumber %/% 20, sent_n_p) %>%
  spread(sent_n_p, n, fill = 0) %>%
  mutate(sent_n_p = positivo - negativo) %>%
  ggplot(aes(index, sent_n_p, fill = autor)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~autor, ncol = 2, scales = "free_x") +
  labs(y = "Niveles de sentimiento", x = "grupos de palabras") +
  labs(title = "PALABRAS CON SENTIMIENTOS POSITIVOS Y NEGATIVOS EN LOS DOS PLANES DE GOBIERNO",
       subtitle = "Elecciones 2021 - 2da vuelta - 6 de junio
       Las barras ubicadas encima de cero, representan la cantidad de palabras con sentimientos positivos usadas en los planes de gobierno",
       caption = "Fuentes de información:
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16490.pdf
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16542.pdf
         Elaborado por: @renzo_ruesta https://github.com/Renzo-Ruesta") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# GRÁFICO DE PALABRAS POSITIVAS Y NEGATIVAS USADAS
planes_tokn_cl %>%
  inner_join(afinn, by = "word") %>%
  filter(!is.na(sent_n_p)) %>%
  count(autor, word, sent_n_p, sort = T) %>%
  ungroup() %>%
  group_by(sent_n_p) %>%
  slice_max(n, n = 30) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sent_n_p)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~autor, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

# GRAFICO DE NUBE DE PALABRAS CON SENTIMIENTOS ASOCIADOS
par(mar=c(3,3,3,3), mfcol=c(1,2))
#Primer gráfico 
set.seed(123)
planes_tokn_cl %>%
  filter(autor == "Fuerza Popular") %>%
  inner_join(afinn %>%
               select(word, sentiment = sent_n_p) 
             %>% unique()) %>%
  filter(!is.na(sentiment)) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = brewer.pal(1, "Dark2"), max.words = 150, title.size = 2)
mtext(c("Fuerza Polular"), side = 3, line = -0.5, cex = 1)
#Segundo gráfico
set.seed(123)
planes_tokn_cl %>%
  filter(autor == "Peru Libre") %>%
  inner_join(afinn %>%
               select(word, sentiment = sent_n_p) 
             %>% unique()) %>%
  filter(!is.na(sentiment)) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = brewer.pal(1, "Dark2"), max.words = 150, title.size = 2)
mtext(c("Peru Libre"), side = 3, line = -0.5, cex = -1)
# titulo
title(main = list("NUBE DE PALABRAS CON SENTIMIENTOS POSITIVOS Y NEGATIVOS DE LOS PLANES DE GOBIERNO
                  Elecciones 2021 - 2da vuelta - 6 de junio", cex = 0.98),
      outer = T, line = -2)
title(sub = list("Fuentes de información:
        https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16490.pdf
      https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16542.pdf
      Elaborado por: @renzo_ruesta https://github.com/Renzo-Ruesta", font = 0.2, cex = 0.8),
      outer = T, line = -1.2)

# FRECUENCIAS DE TERMINOS EN LOS PLANES DE GOBIERNO 
planes_words <- planes_tokn_cl %>%
  count(autor, word, sort = TRUE)

total_words <- planes_words %>% 
  group_by(autor) %>% 
  summarize(total = sum(n))

planes_words <- left_join(planes_words, total_words)

# NUMERO DE VECES QUE APARECE UNA PALABRA EN LOS PLANES DE GOBIERNO
planes_words %>%
ggplot(aes(n/total, fill = autor)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0018) +
  facet_wrap(~autor, ncol = 2, scales = "free_y")

# QUE PLAN DE GOBIERNO HACE USO DE MAS PALABRAS COMUNES
# SEGUN LA LEY EMPIRICA DE ZIPF: La ley de Zipf establece que la frecuencia con la que aparece una palabra 
# es inversamente proporcional a su rango.
freq_by_rank <- planes_words %>% 
  group_by(autor) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = autor)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) + 
  scale_x_log10() +
  scale_y_log10()

# CALCULO DE LA PENDIENTE PARA LA SECCION MEDIA DEL RANKIN
rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

# QUE PLAN DE GOBIERNO USA UN PORCENTAJE MENOR DE PALABRAS MENOS COMUNES
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = autor)) + 
  geom_abline(intercept = -1.7334, slope = -0.5982, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# LA IDEA DE TF-IDF ES ENCONTRAR LAS PALABRAS IMPORTANTES PARA EL CONTENIDO DE CADA DOCUMENTO
# disminuyendo el peso de las palabras de uso común y aumentando el peso de las palabras que no se usan mucho
autor_tf_idf <- planes_words %>%
  bind_tf_idf(word, autor, n) %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# GRAFICO DE LAS PALABRAS MAS IMPORTANTES DE LOS PLANES DE GOBIERNO
autor_tf_idf %>%
  group_by(autor) %>%
  slice_max(tf_idf, n = 20) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = autor)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~autor, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  labs(title = "PALABRAS MÁS REPRESENTATIVAS DE CADA PLAN DE GOBIERNO",
       subtitle = "Elecciones 2021 - 2da vuelta - 6 de junio
       Se muestran las palabras que representan más a cada plan de gobierno",
       caption = "Fuentes de información:
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16490.pdf
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16542.pdf
         Elaborado por: @renzo_ruesta https://github.com/Renzo-Ruesta") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# ANALISIS DE BIGRAMAS
autor_bigrams <- planes_gob %>%
  select(autor, text) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- autor_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(autor, word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_filtered %>%
  filter(word2 == "pandemia" | word2 == "covid") %>%
  count(autor, word1, sort = TRUE)

bigram_tf_idf <- bigrams_united %>%
  count(autor, bigram) %>%
  bind_tf_idf(bigram, autor, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(autor) %>% 
  top_n(13) %>% 
  ungroup() %>%
  ggplot(aes(bigram, tf_idf, fill = autor)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~autor, ncol = 2, scales = "free") +
  coord_flip() +
  labs(title = "LOS BIGRAMAS MÁS REPRESENTATIVOS DE CADA PLAN DE GOBIERNO",
       subtitle = "Elecciones 2021 - 2da vuelta - 6 de junio
       Se muestran los bigramas que representan más a cada plan de gobierno",
       caption = "Fuentes de información:
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16490.pdf
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16542.pdf
         Elaborado por: @renzo_ruesta https://github.com/Renzo-Ruesta") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# ANALISIS DE TRIGRAMAS
planes_gob %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(autor, word1, word2, word3, sort = TRUE)

# ESTRUCTURA DE TEXTO: ANALISIS DE NODOS
bigram_graph_fp <- bigram_counts %>%
  filter(!is.na(word1)|!is.na(word2)) %>%
  filter(autor == "Fuerza Popular", n > 2) %>%
  select(-autor) %>%
  graph_from_data_frame()

bigram_graph_pl <- bigram_counts %>%
  filter(!is.na(word1)|!is.na(word2)) %>%
  filter(autor == "Peru Libre", n > 2) %>%
  select(-autor) %>%
  graph_from_data_frame()

# GRAFICO DE CONEXIONES DE PALABRAS MAS COMUNES
set.seed(123)
ggraph(bigram_graph_fp, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(123)
ggraph(bigram_graph_pl, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# GRAFICO DE CONEXIONES DE PALABRAS MAS COMUNES
set.seed(123)
ggraph(bigram_graph_fp, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = grid::arrow(type = "closed", length = unit(.15, "inches")),
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

set.seed(123)
ggraph(bigram_graph_pl, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = grid::arrow(type = "closed", length = unit(.15, "inches")),
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# CREACION DE FUNCIONES PARA CREAR DATASET Y VISUALIZACION DE NODOS
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

# GRAFICO DE NODOS O FAMILIAS DE PALABRAS
planes_gob %>%
  filter(autor == "Fuerza Popular") %>%
  count_bigrams() %>%
  filter(n > 2,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()
  
# SECCIONANDO PALABRAS
planes_section_words_fp <- planes_gob %>%
  select(autor, text) %>%
  filter(autor == "Fuerza Popular") %>%
  mutate(section = row_number() %/% 5) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

planes_section_words_pl <- planes_gob %>%
  select(autor, text) %>%
  filter(autor == "Peru Libre") %>%
  mutate(section = row_number() %/% 5) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

# CUENTA DE PALABRAS QUE COEXISTEN DENTRO DE LAS SECCIONES
word_pairs_fp <- planes_section_words_fp %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs_pl <- planes_section_words_pl %>%
  pairwise_count(word, section, sort = TRUE)

# CONTANDO Y CORRELACIONANDO ENTRE SECCIONES
# El coeficiente phi es equivalente a la correlación de Pearson
word_cors_fp <- planes_section_words_fp %>%
  group_by(word) %>%
  filter(n() >= 3) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors_pl <- planes_section_words_pl %>%
  group_by(word) %>%
  filter(n() >= 3) %>%
  pairwise_cor(word, section, sort = TRUE)

# TEMAS PRINCIPALES EN LOS DEBATES
word_cors_fp %>%
  filter(item1 %in% c("corrupción", "pandemia", "economía", "educación", "salud", "seguridad")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 8) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  labs(title = "PALABRAS IMPORTANTES RELACIONADAS CON LOS PRINCIPALES TEMAS DE DEBATE EN FUERZA POPULAR",
       subtitle = "Elecciones 2021 - 2da vuelta - 6 de junio
       Se muestran las palabras más importantes que representan a cada plan de gobierno",
       caption = "Fuentes de información:
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16490.pdf
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16542.pdf
         Elaborado por: @renzo_ruesta https://github.com/Renzo-Ruesta") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

word_cors_pl %>%
  filter(item1 %in% c("corrupción", "pandemia", "economía", "educación", "salud", "seguridad")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 8) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  labs(title = "PALABRAS IMPORTANTES RELACIONADAS CON LOS PRINCIPALES TEMAS DE DEBATE EN PERU LIBRE",
       subtitle = "Elecciones 2021 - 2da vuelta - 6 de junio
       Se muestran las palabras más importantes que representan a cada plan de gobierno",
       caption = "Fuentes de información:
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16490.pdf
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16542.pdf
         Elaborado por: @renzo_ruesta https://github.com/Renzo-Ruesta") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# CORRELACIONES Y GRUPOS DE PALABRAS
set.seed(2016)
word_cors_fp %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "FUERZA POPULAR: PARES DE PALABRAS QUE MUESTRAN UN GRADO ALTO DE CORRELACIÓN",
       subtitle = "Elecciones 2021 - 2da vuelta - 6 de junio
       Se muestran los pares de palabras que muestran al menos una correlación de 50% de aparecer en la misma sección",
       caption = "Fuentes de información:
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16490.pdf
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16542.pdf
         Elaborado por: @renzo_ruesta https://github.com/Renzo-Ruesta") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

set.seed(2016)
word_cors_pl %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  labs(title = "PERU LIBRE: PARES DE PALABRAS QUE MUESTRAN UN GRADO ALTO DE CORRELACIÓN",
       subtitle = "Elecciones 2021 - 2da vuelta - 6 de junio
       Se muestran los pares de palabras que muestran al menos una correlación de 50% de aparecer en la misma sección",
       caption = "Fuentes de información:
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16490.pdf
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16542.pdf
         Elaborado por: @renzo_ruesta https://github.com/Renzo-Ruesta") +
  theme


# CORRELACIONES Y GRUPOS DE PALABRAS
word_cors_fp %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +   geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
  theme_void() +
  labs(title = "FUERZA POPULAR: PODEMOS DIFERENCIAR EL CONJUNTO DE PALABRAS CON MAYOR CORRELACIÓN O ÉNFASIS EN EL PLAN DE GOBIERNO",
       subtitle = "Elecciones 2021 - 2da vuelta - 6 de junio
       Se muestran las palabras con correlaciones por encima del 50%",
       caption = "Fuentes de información:
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16490.pdf
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16542.pdf
         Elaborado por: @renzo_ruesta https://github.com/Renzo-Ruesta") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

word_cors_pl %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +   geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
  theme_void() +
  labs(title = "PERU LIBRE: PODEMOS DIFERENCIAR EL CONJUNTO DE PALABRAS CON MAYOR CORRELACIÓN O ÉNFASIS EN EL PLAN DE GOBIERNO",
       subtitle = "Elecciones 2021 - 2da vuelta - 6 de junio
       Se muestran las palabras con correlaciones por encima del 50%",
       caption = "Fuentes de información:
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16490.pdf
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16542.pdf
         Elaborado por: @renzo_ruesta https://github.com/Renzo-Ruesta") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# MODELADO DE TEMAS: METODO MATEMATICO PARA SEPARAR GRUPOS NATURALES DE PALABRAS

# MATRIZ DE TERMINOS Y DOCUMENTOS
chapters_dtm_fp <- planes_tokn_cl %>%
  filter(autor == "Fuerza Popular") %>%
  count(chapter, word, sort = TRUE) %>%
  cast_dtm(chapter, word, n)

chapters_dtm_pl <- planes_tokn_cl %>%
  filter(autor == "Peru Libre") %>%
  count(chapter, word, sort = TRUE) %>%
  cast_dtm(chapter, word, n)

# ASINACION DE DIRICHLET LATENTE (LDA) k TEMAS
chapters_lda_fp <- LDA(chapters_dtm_fp, method = "VEM", k = 10, control = list(seed = 1234)) # method="Gibbs"
chapters_lda_pl <- LDA(chapters_dtm_pl, method = "VEM", k = 10, control = list(seed = 1234))

# BETAS - CLASIFICACION POR TEMA: Cada tema como una mezcla de palabras
chapters_topics_lda_fp <- tidy(chapters_lda_fp, matrix = "beta")
chapters_topics_lda_pl <- tidy(chapters_lda_pl, matrix = "beta")

# Probabilidad de que un conjunto de palabras esten asociadas a un tema
top_terms_lda_fp <- chapters_topics_lda_fp %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms_lda_pl <- chapters_topics_lda_pl %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

# GRAFICO DE PALABRAS MAS COMUNES POR TEMAS
top_terms_lda_fp %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 5) +
  scale_y_reordered() +
  labs(title = "FUERZA POPULAR: AGRUPAMIENTO NATURAL DE PALABRAS EN 10 GRANDES TEMAS",
       subtitle = "Elecciones 2021 - 2da vuelta - 6 de junio
       Se muestran las probabilidades (beta) de que un conjunto de palabras pertenezca a un tema
       Podrás ser capaz de asignar un nombre de tema a cada uno de los 10 números
       Para agrupar las palabras se usó el modelo matemático LDA - Latent Dirichlet allocation",
       caption = "Fuentes de información:
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16490.pdf
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16542.pdf
         Elaborado por: @renzo_ruesta https://github.com/Renzo-Ruesta") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

top_terms_lda_pl %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 5) +
  scale_y_reordered() +
  labs(title = "PERU LIBRE: AGRUPAMIENTO NATURAL DE PALABRAS EN 10 GRANDES TEMAS",
       subtitle = "Elecciones 2021 - 2da vuelta - 6 de junio
       Se muestran las probabilidades (beta) de que un conjunto de palabras pertenezca a un tema
       Podrás ser capaz de asignar un nombre de tema a cada uno de los 10 números
       Para agrupar las palabras se usó el modelo matemático LDA - Latent Dirichlet allocation",
       caption = "Fuentes de información:
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16490.pdf
         https://declara.jne.gob.pe/ASSETS/PLANGOBIERNO/FILEPLANGOBIERNO/16542.pdf
         Elaborado por: @renzo_ruesta https://github.com/Renzo-Ruesta") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# GAMMAS - CLASFICACION POR DOCUMENTO: Cada documento como una mezcla de temas
# Probabilidad de que un conjunto de temas esten asociados a un documento o capitulo
chapters_gamma_lda_fp <- tidy(chapters_lda_fp, matrix = "gamma") %>%
  separate(document, c("chapter", "title"), sep = ":", convert = TRUE)

chapters_gamma_lda_pl <- tidy(chapters_lda_pl, matrix = "gamma") %>%
  separate(document, c("chapter", "title"), sep = " - ", convert = TRUE)

# GRAFICO DE LOS TITUTLOS EN EL ORDEN DE LOS TEMAS 1, 2 , ETC ANTES DE TRAZAR
chapters_gamma_lda_fp %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title) +
  labs(x = "topic", y = expression(gamma))

chapters_gamma_lda_pl %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title) +
  labs(x = "topic", y = expression(gamma))

# ENCONTRAR EL TEMA QUE ESTA MAS ASOCIADO A CADA CAPITULO
chapter_classifications_lda_fp <- chapters_gamma_lda_fp %>%
  group_by(title, chapter) %>%
  slice_max(gamma) %>%
  ungroup()

chapter_classifications_lda_pl <- chapters_gamma_lda_pl %>%
  group_by(title, chapter) %>%
  slice_max(gamma) %>%
  ungroup()

# COMPARACION CON CLASIFICACION DE TEMA CON EL TEMA DE CONSENSO DE CADA TITULO
chapters_topics_lda_fp <- chapter_classifications_lda_fp %>%
  count(title, topic) %>%
  group_by(title) %>%
  slice_max(n, n = 1) %>% 
  ungroup() %>%
  transmute(consensus = title, topic)

chapters_topics_lda_pl <- chapter_classifications_lda_pl %>%
  count(title, topic) %>%
  group_by(title) %>%
  slice_max(n, n = 1) %>% 
  ungroup() %>%
  transmute(consensus = title, topic)

# ERRORES DE CLASIFICACION
chapter_classifications_lda_fp %>%
  inner_join(chapters_topics_lda_fp, by = "topic") %>%
  filter(title != consensus)

chapter_classifications_lda_pl %>%
  inner_join(chapters_topics_lda_pl, by = "topic") %>%
  filter(title != consensus)

# ASIGNACION DE PALABRAS A CADA CAPITULO O DOCUMENTO A UN TEMA
assignments_lda_fp <- augment(chapters_lda_fp, data = chapters_dtm_fp)

assignments_lda_pl <- augment(chapters_lda_pl, data = chapters_dtm_pl)

# MATRIZ DE CONFUNCION
assignments_lda_fp <- assignments_lda_fp %>%
  separate(document, c("chapter", "title"), 
           sep = ":", convert = TRUE) %>%
  inner_join(chapters_topics_lda_fp, by = c(".topic" = "topic"))

assignments_lda_pl <- assignments_lda_pl %>%
  separate(document, c("chapter", "title"), 
           sep = " - ", convert = TRUE) %>%
  inner_join(chapters_topics_lda_pl, by = c(".topic" = "topic"))

# GRAFICO DE FRECUENCIA DE ASIGNACION DE PALABRAS: MATRIZ DE CONFUSION
assignments_lda_fp %>%
  count(title, consensus, wt = count) %>%
  mutate(across(c(title, consensus), ~str_wrap(., 20))) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "darkred", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Las palabras se asignaron a",
       y = "Las palabras vinieron de",
       fill = "% de asignaciones")

assignments_lda_pl %>%
  count(title, consensus, wt = count) %>%
  mutate(across(c(title, consensus), ~str_wrap(., 20))) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "darkred", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Las palabras se asignaron a",
       y = "Las palabras vinieron de",
       fill = "% de asignaciones")
