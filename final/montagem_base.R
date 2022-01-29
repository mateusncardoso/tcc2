library(tidyverse)

imunizacao <- readRDS('C:/Users/cardo/Desktop/tcc/dados/final/imunizacao.rds')
obitos_causas_externas <- readRDS("C:/Users/cardo/Desktop/tcc/dados/final/obitos_causas_externas.rds")
obitos_evitaveis <- readRDS("C:/Users/cardo/Desktop/tcc/dados/final/obitos_evitaveis.rds")
obitos_fetais <- readRDS("C:/Users/cardo/Desktop/tcc/dados/final/obitos_fetais.rds")
pessoal_infra <- readRDS("C:/Users/cardo/Desktop/tcc/dados/final/pessoal_infra.rds")
pib <- readRDS("C:/Users/cardo/Desktop/tcc/dados/final/pib.rds")
populacao <- readRDS("C:/Users/cardo/Desktop/tcc/dados/final/populacao.rds")

# unindo id_municipio de 6 digitos

list(populacao, obitos_causas_externas, obitos_evitaveis,
     obitos_fetais, imunizacao) %>% 
  reduce(left_join) %>% 
  rename('pop_total' = 3) -> a


rm(obitos_causas_externas, obitos_evitaveis,
   obitos_fetais, populacao, imunizacao)  



# unindo  id_municipio 7 digitos

pib %>% 
  mutate(id_municipio = as.character(id_municipio)) -> pib
  
pessoal_infra %>% 
  left_join(pib) %>% 
  mutate(id_municipio = substr(id_municipio, 1, 6)) %>% 
  right_join(a) -> b

rm(pessoal_infra, pib, a)

# municipios criança feliz

elegiveis <- read_csv("C:/Users/cardo/Desktop/tcc/dados/municipios/elegiveis.csv")

elegiveis %>% 
  rename(id_municipio = 1) %>% 
  distinct(id_municipio) %>% 
  mutate(tipo = 'e') %>% 
  mutate(id_municipio = as.character(id_municipio))-> elegiveis

# 3780 municipios elegíveis
# 2903 aderiram
# restaram 998 elegíveis (grupo de controle)

aderiram <- readxl::read_xlsx("C:/Users/cardo/Desktop/tcc/dados/municipios/participantes.xlsx")


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



# filtrando base para municipios elegiveis
b %>% 
  filter(id_municipio %in% elegiveis$id_municipio) -> b

# criando grupo 
elegiveis %>% 
  filter(!id_municipio %in% aderiram$id_municipio) -> elegiveis

b %>% 
  mutate(grupo = ifelse(id_municipio %in% elegiveis$id_municipio,
                        0, 1)) ->b


# criando variavel de tempo
aderiram %>% 
  rename('ano_adesao' = 6) -> aderiram

aderiram %>% 
  select(1, 6) -> aderiram

b %>% 
  left_join(aderiram) -> b

b %>% 
  mutate(ano_adesao = ifelse(grupo == 0, 2016, ano_adesao))-> b

b %>% 
  mutate(depois = ifelse(ano >= ano_adesao, 1, 0)) -> b

diarreia <- read.csv2('dados/final/mortalidade_diarreia.csv', encoding = 'Latin1')

diarreia %>% 
  rename('id_municipio' = 1) %>% 
  mutate(id_municipio = as.character(id_municipio),
         id_municipio = substr(id_municipio, 1, 6)) %>% 
  pivot_longer(!id_municipio, values_to = 'diarreia', names_to = 'ano') %>% 
  mutate(ano = as.integer(substr(ano, 2, 5))) %>% 
  right_join(b) -> b

b %>% 
  mutate(interacao = grupo*depois, 
         tempo_exposicao = ano - ano_adesao,
         tempo_exposicao = ifelse(tempo_exposicao < 0, 0, tempo_exposicao)) -> b

#write.csv(b, 'base_final.csv')

#summary(lm(diarreia ~ interacao + as.factor(ano) + as.factor(id_municipio), data = b))

#summary(fixest::feols(diarreia ~ interacao + docentes + 
#                           escolas + estabelecimentos + medicos + pib_per_capita_real + 
#                           pop_total + p_de0_a_14 + p_de15_a_29 + p_de30_a_59 + 
#                           p_mais_60 + p_homens + ano + id_municipio,
#                         cluster = c("id_municipio"), var = diarreia, data = b))



library(clubSandwich)

library(plm)


reg_diarreia <- plm::plm(diarreia ~ interacao + docentes + 
                         escolas + estabelecimentos + medicos + pib_per_capita_real + 
                         pop_total + p_de0_a_14 + p_de15_a_29 + p_de30_a_59 + p_mais_60 + 
                         p_homens + de0_a_4 + factor(tempo_exposicao), 
                    data = b,
                    index = c("id_municipio", "ano"), 
                    model = "within",
                    weights = de0_a_4)

summary(reg_diarreia)

reg_evitaveis <- plm::plm(obitos_causas_externas ~ interacao + docentes + 
                           escolas + estabelecimentos + medicos + pib_per_capita_real + 
                           pop_total + p_de0_a_14 + p_de15_a_29 + p_de30_a_59 +
                           p_homens + de0_a_4 + factor(tempo_exposicao), 
                         data = b,
                         index = c("id_municipio", "ano"), 
                         model = "within",
                         weights = de0_a_4)

summary(reg_evitaveis)

# print summary using robust standard errors
lmtest::coeftest(reg_diarreia, vcov. = vcovHC, type = "HC1")
