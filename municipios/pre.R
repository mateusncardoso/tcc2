library(tidyverse)

elegiveis <- read_csv("dados/municipios/elegiveis.csv")

elegiveis %>% 
  rename(id_municipio = 1) %>% 
  distinct(id_municipio) %>% 
  mutate(tipo = 'e') %>% 
  mutate(id_municipio = as.character(id_municipio))-> elegiveis

# 3780 municipios elegíveis
# 2903 aderiram
# restaram 998 elegíveis (grupo de controle)

aderiram <- readxl::read_xlsx("dados/municipios/participantes.xlsx")


aderiram %>% 
  mutate(IBGE = as.character(IBGE),
         tipo = 'a') %>% 
  filter(`Situação do aceite` == "Aceito") %>% 
  select(1, 4, 5, 6, 21) %>% 
  rename(id_municipio = 1,
         situacao = 2,
         data_aceite = 3,
         etapa = 4) %>% 
  mutate(id_municipio = substr(id_municipio,1 , 6),
         ano = substr(data_aceite, 7, 10),
         ano = as.integer(ano))-> aderiram


elegiveis %>% 
  filter(!id_municipio %in% aderiram$id_municipio) -> elegiveis

final <- bind_rows(aderiram, elegiveis)

geobr::read_municipality(year = 2020) -> muni

geobr::read_country() -> br

muni %>% 
  mutate(code_muni = as.character(code_muni),
         code_muni = substr(code_muni, 1, 6)) %>% 
  left_join(final, by = c("code_muni" = "id_municipio")) -> cu


cu %>% 
  mutate(tipo = ifelse(is.na(tipo) == TRUE, "ne", tipo)) %>% 
  ggplot()+
  geom_sf(aes(fill = tipo), color = NA)+
  geom_sf(data = br, fill = NA)+
  scale_fill_brewer(palette = "Accent",
                    labels = c('Aderiram',
                               'Não aderiram',
                               'Não elegíveis'),
                    name = "")+
  theme_minimal()+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = 'serif', size = 14))


ggsave("mapa.png", dpi = 300)



final %>% 
  mutate(tipo = ifelse(is.na(tipo) == T, "e", tipo)) ->final

full_join(obitos_evitaveis, final) -> cu


obitos_fetais %>% 
  left_join(populacao, by = c("ano", "id_municipio")) %>% 
  mutate(grupo = ifelse(id_municipio %in% aderiram$id_municipio, "a", ifelse(
  id_municipio %in% elegiveis$id_municipio, "e", "ne")),
  per_1000 = total.x / (de0_a_4/1000)) %>% 
  mutate(grupo = factor(grupo,
                        levels = c('a', 'e', 'ne'),
                        labels = c("Municípios que aderiram",
                                   "Municípios elegíveis",
                                   "Municípios não elegíveis"))) %>% 
  group_by(ano, grupo) %>% 
  summarise(media = mean(per_1000, na.rm = T)) %>% 
  ggplot(aes(x = ano, y = media, linetype = grupo))+
  geom_vline(xintercept = 2016, color = "grey50")+
  geom_line(size=1)+
  scale_y_continuous(limits = c(0, 1.5))+
  theme_minimal()+
  labs(title = "Média de óbitos fetais",
  subtitle = "A cada 1000 habitantes de 0 a 4 anos",
  linetype = "Grupo",
  x = "Ano",
  y = "Média")

ggsave('2.png')

obitos_evitaveis %>% 
  left_join(populacao, by = c("ano", "id_municipio")) %>% 
  mutate(grupo = ifelse(id_municipio %in% aderiram$id_municipio, "a", ifelse(
    id_municipio %in% elegiveis$id_municipio, "e", "ne")),
    per_1000 = total.x / (de0_a_4/1000)) %>% 
  mutate(grupo = factor(grupo,
                        levels = c('a', 'e', 'ne'),
                        labels = c("Municípios que aderiram",
                                   "Municípios elegíveis",
                                   "Municípios não elegíveis"))) %>% 
  group_by(ano, grupo) %>% 
  summarise(media = mean(per_1000, na.rm = T)) %>% 
  ggplot(aes(x = ano, y = media, linetype = grupo))+
  geom_vline(xintercept = 2016, color = "grey50")+
  geom_line(size=1)+
  theme_minimal()+
  labs(title = "Média de óbitos por causas evitáveis - 0 a 4 anos",
  subtitle = "A cada 1000 habitantes de 0 a 4 anos",
  linetype = "Grupo",
  x = "Ano",
  y = "Média")

obitos_fetais %>% 
  filter(id_municipio %in% aderiram$id_municipio)
