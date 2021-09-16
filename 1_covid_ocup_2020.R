################################################################################
########## MORTOS POR COVID BRASIL 2020 - ANÁLISE DOS DADOS DO SIM #############
################################################################################

##################################################
# IMPORTAÇÃO E TRATAMENTO DOS DADOS ORIGINÁRIOS  #
##################################################

# (1)
library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(fs)
library(sf)
library(geobr)

##
dir_create(here(c("data",
                  "documents",
                  "scripts",
                  "outputs",
                  "old")))
##
dir_tree()

# (2)

## SIM 2020
# URL <- "https://opendatasus.saude.gov.br/dataset/57c4b23e-6ffa-43ef-a1e7-23103f73f376/resource/da17c5f6-aa89-4e7d-b7e2-ec4b77a5dc31/download/dobrano_.csv"
# sim_2020 <- fread(URL) # Também disponível no DataLake Base dos Dados

sim_2020_raw <- fread(here("data", "DATASUS_SIM-2020.csv"))

## CBO 2002

###
cbo_grande_grupo <- fread(here("data", "CBO2002 - Grande Grupo.csv"), 
                          encoding = "Latin-1") %>%
  transmute(id_cbo_1 = CODIGO, cbo_1 = TITULO)
cbo_subgrupo_principal <- fread(here("data", "CBO2002 - SubGrupo Principal.csv"), 
                                encoding = "Latin-1") %>%
  transmute(id_cbo_2 = CODIGO, cbo_2 = TITULO)
cbo_ocup <- fread(here("data", "CBO2002 - Ocupacao.csv"), 
                  encoding = "Latin-1") %>%
  transmute(id_cbo_6 = CODIGO, cbo_6 = TITULO) 
cbo_familia <- fread(here("data", "CBO2002 - Familia.csv"), 
                     encoding = "Latin-1") %>%
  transmute(id_cbo_4 = CODIGO, cbo_4 = TITULO)
cbo_subgrupo <- fread(here("data", "CBO2002 - SubGrupo.csv"), 
                      encoding = "Latin-1") %>%
  transmute(id_cbo_3 = CODIGO, cbo_3 = TITULO)

## Geo municípios + Geo Metropolitanas
geo_municipios <- read_municipality(year = 2020) 
geo_metropolitanas <- read_metro_area(year = 2018)

################################################################################

# (3)
sim_2020 <- sim_2020_raw %>%
  transmute(id_obito = as.character(contador),
            CODMUNRES,
            SEXO = case_when(SEXO == 1 ~ "Homem", SEXO == 2 ~ "Mulher"),
            RACACOR = case_when(RACACOR == 1 | 
                                  RACACOR == 3 ~ "Branco ou Amarelo",
                                RACACOR == 2 | RACACOR == 4 ~ "Preto ou pardo",
                                RACACOR == 5 ~ "Indigena"),
            IDADE = as.integer(difftime(dmy(DTOBITO), dmy(DTNASC),
                                        units = "days")/365),
            ESC = case_when(ESC == 1 ~ "Nenhuma",
                            ESC == 2 ~ "1 a 3 anos de estudo",
                            ESC == 3 ~ "4 a 7 anos de estudo",
                            ESC == 4 ~ "8 a 11 anos de estudo", 
                            ESC == 5 ~ "12 anos ou mais",
                            TRUE ~ "NA ou ignorado"),
            IDADE_FAIXA = case_when(IDADE>0  & IDADE<11  ~ "0 a 10 anos",
                                    IDADE>10 & IDADE<21 ~ "11 a 20 anos",
                                    IDADE>20 & IDADE<31 ~ "21 a 30 anos",
                                    IDADE>30 & IDADE<41 ~ "31 a 40 anos",
                                    IDADE>40 & IDADE<51 ~ "41 a 50 anos",
                                    IDADE>50 & IDADE<61 ~ "51 a 60 anos",
                                    IDADE>60 & IDADE<71 ~ "61 a 70 anos",
                                    IDADE>70 ~ "Mais de 70 anos"),
            PEA = if_else(IDADE >= 18 & IDADE < 65, 1, 0),
            COVID = if_else(CAUSABAS == "B342", 1, 0),
            id_cbo_6 = OCUP)

# (4) Informações ocupacionais
sim_2020 <- sim_2020 %>%
  mutate(id_cbo_1 = as.numeric(str_sub(as.character(id_cbo_6), end = -6L)),
         id_cbo_2 = as.numeric(str_sub(as.character(id_cbo_6), end = -5L)),
         id_cbo_3 = as.numeric(str_sub(as.character(id_cbo_6), end = -4L)),
         id_cbo_4 = as.numeric(str_sub(as.character(id_cbo_6), end = -3L)),
         id_cbo_1 = ifelse(!is.na(id_cbo_6) & is.na(id_cbo_1), 0, id_cbo_1)) %>%
  left_join(cbo_ocup, by = "id_cbo_6") %>% 
  left_join(cbo_familia, by = "id_cbo_4") %>%
  left_join(cbo_subgrupo, by = "id_cbo_3") %>%
  left_join(cbo_subgrupo_principal, by = "id_cbo_2") %>%
  left_join(cbo_grande_grupo, by = "id_cbo_1")

# (5) Informações geográficas

##
geo <- geo_municipios %>%
  st_drop_geometry() %>%
  left_join(geo_metropolitanas %>% 
              st_drop_geometry() %>% 
              select(code_muni, name_metro), by = "code_muni") %>%
  transmute(code_muni,
            code_muni_6 = as.numeric(str_sub(as.character(code_muni),
                                             end = -2L)),
            name_muni,
            metro = case_when(!is.na(name_metro) ~ "Metropolitano",
                              TRUE ~ "Não Metropolitano"),
            name_metro, abbrev_state, name_region)

## Duplicado: Município de Murici - AL em duas RM's (Zona da Mata e Maceió)
filter(geo_metropolitanas, duplicated(code_muni))

##
geo <- geo %>% 
  filter(!c(code_muni == 2705507 & name_metro != "RM Maceió")) 

##
filter(geo, duplicated(code_muni))

## Opção por município de residência para a ánalise
## Quase o dobro de municípios - indicativo de deslocamento ao CBD
sim_2020_raw %>%
  filter(CAUSABAS == "B342") %>% ### Causa = Covid-19
  select(CODMUNOCOR, CODMUNRES) %>%
  summarize(across(everything(), n_distinct))

##
sim_2020 <- sim_2020 %>%
  left_join(geo, by = c("CODMUNRES" = "code_muni_6")) %>%
  select(-CODMUNRES)
  
################################################################################

# (6) Geral

##
glimpse(sim_2020)

## Dividindo observações de interesse em objetos
###
pea_sim_2020 <- sim_2020 %>% 
  filter(PEA == 1)
###
covid_sim_2020 <- sim_2020 %>% 
  filter(COVID == 1)

##
stats_pea <- pea_sim_2020 %>%
  summarize(across(everything(), ~ list(n = sum(!is.na(.)),
                                        n_distinct = n_distinct(.),
                                        Missing = sum(is.na(.)),
                                        Missing_prop = round(mean(is.na(.)), 2),
                                        Mean = round(mean(., na.rm = TRUE), 2)
  )), 
  Stat = c("N", "N_distinct", "Missing", "Missing_prop", "Mean")) %>%
  select(Stat, everything())

##
stats_covid <- covid_sim_2020 %>%
  summarize(across(everything(), ~ list(n = sum(!is.na(.)),
                                        n_distinct = n_distinct(.),
                                        Missing = sum(is.na(.)),
                                        Missing_prop = round(mean(is.na(.)), 2),
                                        Mean = round(mean(., na.rm = TRUE), 2)
  )), 
  Stat = c("N", "N_distinct", "Missing", "Missing_prop", "Mean")) %>%
  select(Stat, everything())

# (7) Perfil ocupacional

##
pea_sim_2020 %>%
  group_by(id_cbo_4, cbo_4) %>%
  summarize(n = n(),
            prop = n() / n_distinct(pea_sim_2020$id_obito)) %>%
  ungroup() %>%
  arrange(desc(n))

covid_sim_2020 %>%
group_by(id_cbo_4, cbo_4) %>%
  summarize(n = n(),
            prop = n() / n_distinct(covid_sim_2020$id_obito)) %>%
  ungroup() %>%
  arrange(desc(n))

## NA id_cbo_4 vs NA nome cbo_4 
pea_sim_2020 %>%
  summarize(across(everything(), ~ mean(is.na(.)))) ### 18.1% vs 46.8% 
covid_sim_2020 %>%
  summarize(across(everything(), ~ mean(is.na(.)))) ### 15.3% vs 59.2%

##
pea_sim_2020 %>%
  filter(is.na(cbo_4) & !is.na(id_cbo_4) & !is.na(id_cbo_3) & 
           !is.na(id_cbo_6)) %>%
  summarize(unique(id_cbo_4)) ### CBO's 9999, 9989 e 2231 - Médicos  

covid_sim_2020 %>%
  filter(is.na(cbo_4) & 
           !is.na(id_cbo_4) & !is.na(id_cbo_3) & !is.na(id_cbo_6)) %>%
  summarize(unique(id_cbo_4)) ### CBO's 9999, 9989 e 2231 - Médicos

##
covid_sim_2020 %>%
  group_by(id_cbo_6, cbo_6, id_cbo_4, cbo_4) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  arrange(desc(id_cbo_6))

## 9989-99 IGNORADO
## 9999-91 ESTUDANTE
## 9999-92 DONA DE CASA
## 9999-93 APOSENTADO/PENSIONISTA
## 9999-94 DESEMPREGADO CRONICO OU CUJA OCUPACAO HABITUAL NAO FOI POSSIVEL OBTER
## 9999-95 PRESIDIARIO (PESSOAS CONFINADAS EM INSTITUICOES PENAIS, INCLUSIVE MENORES DE IDADE)

## Ajustando categorias nos BD's
pea_sim_2020 <- pea_sim_2020 %>%
  mutate(cbo_4 = case_when(id_cbo_4 %in% c(2231, 2251, 2252, 2253) ~ "Médicos",
                           id_cbo_4 == 9999 ~ "Inativos",
                           id_cbo_4 == 9989 ~ "Ignorado",
                           TRUE ~ cbo_4)) %>%
  filter(!is.na(cbo_4) & cbo_4 != "Inativos" & cbo_4 != "Ignorado")

sim_2020 <- sim_2020 %>%
  mutate(cbo_4 = case_when(id_cbo_4 %in% c(2231, 2251, 2252, 2253) ~ "Médicos",
                           id_cbo_4 == 9999 ~ "Inativos",
                           id_cbo_4 == 9989 ~ "Ignorado",
                           TRUE ~ cbo_4),
         PEA = case_when(!is.na(cbo_4) & cbo_4 != "Inativos" & 
                           cbo_4 != "Ignorado" ~ 1,
                         TRUE ~ 0))

covid_sim_2020 <- covid_sim_2020 %>%
  mutate(cbo_4 = case_when(id_cbo_4 %in% c(2231, 2251, 2252, 2253) ~ "Médicos",
                           id_cbo_4 == 9999 ~ "Inativos",
                           id_cbo_4 == 9989 ~ "Ignorado",
                           TRUE ~ cbo_4),
         PEA = case_when(!is.na(cbo_4) & cbo_4 != "Inativos" & 
                           cbo_4 != "Ignorado" ~ 1,
                         TRUE ~ 0))

##
covid_sim_2020 %>%
  summarize(na_cbo_4 = sum(is.na(cbo_4)),
            na_cbo_4_prop = mean(is.na(cbo_4)),
            cbo_4_ignorado = sum(cbo_4 == "Ignorado", na.rm = TRUE),
            cbo_4_ignorado_prop = mean(cbo_4 == "Ignorado", na.rm = TRUE),
            inativos = sum(cbo_4 == "Inativos", na.rm = TRUE),
            inativos_prop = mean(cbo_4 == "Inativos", na.rm = TRUE))


### 31708 obitos com ocupação missing data (stricto sensu) - 15.3%
### 3851 obitos de Ocupação Ignorada - 2.2%
### 85991 obitos de Inativos - 49.2%

pea_sim_2020 %>%
  summarize(na_cbo_4 = sum(is.na(cbo_4)),
            na_cbo_4_prop = mean(is.na(cbo_4)),
            cbo_4_ignorado = sum(cbo_4 == "Ignorado", na.rm = TRUE),
            cbo_4_ignorado_prop = mean(cbo_4 == "Ignorado", na.rm = TRUE),
            inativos = sum(cbo_4 == "Inativos", na.rm = TRUE),
            inativos_prop = mean(cbo_4 == "Inativos", na.rm = TRUE))

# (8) Raça/cor

## Óbitos por cor
pea_sim_2020 %>%
  group_by(RACACOR) %>%
  summarize(n = n(),
            prop = n() / n_distinct(pea_sim_2020$id_obito)) %>%
  ungroup() %>%
  arrange(desc(n))

covid_sim_2020 %>%
  group_by(RACACOR) %>%
  summarize(n = n(),
            prop = n() / n_distinct(covid_sim_2020$id_obito)) %>%
  ungroup() %>%
  arrange(desc(n))

## NA's 
covid_sim_2020 %>%
  summarize(na_cor = mean(is.na(RACACOR))) # 3.2%
pea_sim_2020 %>%
  summarize(na_cor = mean(is.na(RACACOR))) # 1.4%

# (9) Escolaridade

## NA escolaridade 
covid_sim_2020 %>%
  summarize(sem_escolaridade = mean(ESC == "NA ou ignorado")) ### 18.7%
pea_sim_2020 %>%
  summarize(sem_escolaridade = mean(ESC == "NA ou ignorado")) ### 10.8%


## NA escolaridade por ocupação
covid_sim_2020 %>%
  filter(ESC == "NA ou ignorado") %>%
  group_by(cbo_4) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
pea_sim_2020 %>%
  filter(ESC == "NA ou ignorado") %>%
  group_by(cbo_4) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# (10) Perfil Geográfico
covid_sim_2020 %>%
  anti_join(geo_municipios %>% st_drop_geometry(), by = c("code_muni")) %>%
  count() ## 67 obitos sem cidade registrada

################################################################################

# (11) Exportação
##
glimpse(pea_sim_2020)

##
# write_csv(sim_2020, here("outputs", "covid_SIM_2020.csv")) ### todos os óbitos Covid em 2020
# write_csv(pea_sim_2020, here("outputs", "pea_SIM_2020.csv")) ### todos os óbitos PEA em 2020
