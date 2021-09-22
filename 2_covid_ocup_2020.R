################################################################################
########## MORTOS POR COVID BRASIL 2020 - ANÁLISE DOS DADOS DO SIM #############
################################################################################

####################################################
# ANÁLISE EXPLORATÓRIA E CONSTRUÇÃO DE INDICADORES #
####################################################

library(tidyverse)
library(here)
library(ggrepel)
library(ggpmisc)
library(scales)
library(readxl)
library(openxlsx)
library(googledrive)

# (1)
glimpse(pea_sim_2020)

## 
view(slice_sample(pea_sim_2020, prop = .01), "amostra_pea_sim_2020")

##
stats_pea_sim_2020 <- pea_sim_2020 %>%
  filter(COVID == 1) %>%
  summarize(across(everything(), ~ list(n = sum(!is.na(.)),
                                        n_distinct = n_distinct(.),
                                        Missing = sum(is.na(.)),
                                        Missing_prop = round(mean(is.na(.)), 2),
                                        Mean = round(mean(., na.rm = TRUE), 2)
  )), 
  Stat = c("N", "N_distinct", "Missing", "Missing_prop", "Mean")) %>%
  select(Stat, everything())

##
sim_2020 %>%
  filter(COVID == 1) %>%
  group_by(SEXO, RACACOR) %>%
  summarize(obitos = n_distinct(id_obito)) %>%
  ungroup() %>%
  ggplot(aes(obitos, fct_reorder(RACACOR, desc(obitos)), fill = SEXO)) +
  geom_col(colour = "black") +
  labs(title = "Óbitos por covid - PEA + PIA",
       x = "",
       y = "",
       caption = "Fonte: SIM/DATASUS, 2020",) +
  theme(plot.title = element_text(size=10, face = "bold"), 
        # legend.position = "bottom", 
        legend.title = element_blank())

##
pea_sim_2020 %>%
  filter(COVID == 1) %>%
  group_by(SEXO, RACACOR) %>%
  summarize(obitos = n_distinct(id_obito)) %>%
  ungroup() %>%
  ggplot(aes(obitos, fct_reorder(RACACOR, desc(obitos)), fill = SEXO)) +
  geom_col(colour = "black") +
  labs(title = "Óbitos por covid - PEA",
       x = "",
       y = "",
       caption = "Fonte: SIM/DATASUS, 2020",) +
  theme(plot.title = element_text(size=10, face = "bold"), 
        # legend.position = "bottom", 
        legend.title = element_blank())

# (2) Excluídos
excluidos_covid_pea <- sim_2020 %>%
  filter(COVID == 1) %>%
  filter(is.na(cbo_4) | cbo_4 == "Inativos" | cbo_4 == "Ignorado" | 
           IDADE < 18 | IDADE >= 65)

## Estimando a dimensão dos excluídos no total de óbitos covid 2020
sim_2020 %>%
  filter(COVID == 1) %>%
  summarize(obitos_covid = n_distinct(id_obito),
            excluidos = n_distinct(excluidos_covid_pea$id_obito),
            prop_excluidos = n_distinct(excluidos_covid_pea$id_obito) / 
              n_distinct(id_obito)) ## 82.7% dos óbitos por covid

##
sim_2020 %>%
  filter(COVID == 1) %>%
  summarize("ocupação NA" = mean(is.na(cbo_4), na.rm = TRUE),
            "inativos em idade ativa" = mean(cbo_4 == "Inativos" &
                                               IDADE >= 18 & 
                                               IDADE < 65, na.rm = TRUE),
            "ocupação ignorada" = mean(cbo_4 == "Ignorado", na.rm = TRUE),
            "crianças e adolescentes" = mean(IDADE < 18, na.rm = TRUE),
            "idosos" = mean(IDADE >= 65, na.rm = TRUE)) %>%
  pivot_longer(cols = everything()) %>%
  arrange(desc(value)) %>%
  mutate(name = factor(name, levels = name)) %>%
  ggplot(aes(value, fct_reorder(name, desc(value)))) +
  geom_col(aes(fill = name), colour = "black") +
  ggrepel::geom_label_repel(aes(label = paste0(scales::percent(value), 
                                               " do total de óbitos de ", 
                                               name,
                                               " registrados em 2020")),
                            direction = "x",
                            segment.size = 0,
                            xlim = c(0.01, NA)) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.7), 
                     breaks = seq(0, 7, .10)) +
  labs(x = "",
       y = "",
       title = "Óbitos de covid excluídos representam:") +
  theme(plot.title = element_text(size=12, face = "italic", hjust = 0.05),
        axis.text.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.position = "none",
        panel.background = element_blank())

#
excluidos_covid_pea %>%
  filter(is.na(cbo_4) | cbo_4 == "Ignorado") %>%
  group_by(SEXO, RACACOR) %>%
  summarize(obitos = n_distinct(id_obito)) %>%
  ungroup() %>%
  ggplot(aes(obitos, fct_reorder(RACACOR, desc(obitos)), fill = SEXO)) +
  geom_col(colour = "black") +  
  labs(x = "",
       y = "",
       title = "Óbitos por covid - ocupação NA ou Ignorada",
       caption = "Fonte: SIM/DATASUS, 2020") +
  theme(plot.title = element_text(size=10, face = "bold"), 
        legend.position = "bottom", 
        legend.title = element_blank())

## 
excluidos_covid_pea %>%
  filter(cbo_4 == "Inativos") %>%
  group_by(SEXO, RACACOR) %>%
  summarize(obitos = n_distinct(id_obito)) %>%
  ungroup() %>%
  ggplot(aes(obitos, fct_reorder(RACACOR, desc(obitos)), fill = SEXO)) +
  geom_col(colour = "black") +  
  labs(x = "",
       y = "",
       title = "Óbitos por covid - inativos",
       caption = "Fonte: SIM/DATASUS, 2020") +
  theme(plot.title = element_text(size=10, face = "bold"), 
        legend.position = "bottom", 
        legend.title = element_blank())

# (3) Dimensionando o número geral de mortes por cbo_4 para ponderação dos dados
###
pea_cbo_4 <- pea_sim_2020 %>%
  group_by(id_cbo_4, cbo_4) %>%
  summarize(obitos_cbo_4 = n_distinct(id_obito),
            idade_media_cbo_4 = round(mean(IDADE), 1)) %>%
  arrange(desc(obitos_cbo_4))
###
pea_cbo_4_sexo <- pea_sim_2020 %>%
  group_by(id_cbo_4, cbo_4, SEXO) %>%
  summarize(obitos_cbo_4_sexo = n_distinct(id_obito)) %>%
  ungroup() %>%
  arrange(desc(obitos_cbo_4_sexo))
###
pea_cbo_4_cor <- pea_sim_2020 %>%
  group_by(id_cbo_4, cbo_4, RACACOR) %>%
  summarize(obitos_cbo_4_cor = n_distinct(id_obito)) %>%
  ungroup() %>%
  arrange(desc(obitos_cbo_4_cor))
###
pea_cbo_4_sexo_cor <- pea_sim_2020 %>%
  group_by(id_cbo_4, cbo_4, SEXO, RACACOR) %>%
  summarize(obitos_cbo_4_sexo_cor = n_distinct(id_obito)) %>%
  ungroup() %>%
  arrange(desc(obitos_cbo_4_sexo_cor))


## (4) Criando sínteses COVID e PEA por família de ocupação (CBO 4 dígitos)
### Filtradas as ocupações com menos do que 50 óbitos registrados no SIM 2020
sint_covid_pea <- pea_sim_2020 %>%
  filter(COVID == 1) %>%
  group_by(id_cbo_4, cbo_4) %>%
  summarize(obitos_covid = n_distinct(id_obito),
            prop_obitos_covid = obitos_covid / 206646) %>%
  ungroup() %>%
  left_join(pea_cbo_4, by = c("id_cbo_4", "cbo_4")) %>%
  mutate(obitos_covid_cada_100_cbo_4 = obitos_covid / obitos_cbo_4 * 100) %>%
  filter(obitos_cbo_4 > 50) 

###
excluidos_sint_covid_pea <- pea_sim_2020 %>%
  filter(COVID == 1) %>%
  group_by(id_cbo_4, cbo_4) %>%
  summarize(obitos_covid = n_distinct(id_obito),
            prop_obitos_covid = obitos_covid / 206646) %>%
  ungroup() %>%
  left_join(pea_cbo_4, by = c("id_cbo_4", "cbo_4")) %>%
  filter(obitos_cbo_4 <= 50) 

##
sum(sint_covid_pea$obitos_covid) ### 34996 observações 

##
list(obitos_sim = sim_2020$id_obito, 
     obitos_pea = pea_sim_2020$id_obito,
     obitos_covid = sim_2020 %>% filter(COVID == 1),
     obitos_covid_pea = pea_sim_2020 %>% filter(COVID == 1)) %>%
  map_dfr(., n_distinct)

## Boxplots
##
sint_covid_pea %>% 
  ggplot(aes(obitos_covid)) + 
  geom_boxplot() +
  labs(x = "Óbitos covid",
       caption = "* Ocupações com pelo menos 50 óbitos")

## ponderado por proxy de chance de morte na categoria: obitos a cada 100
sint_covid_pea %>% 
  ggplot(aes(obitos_covid_cada_100_cbo_4)) + 
  geom_boxplot() +
  labs(x = "Óbitos covid a cada 100 óbitos na categoria",
       caption = "* Ocupações com pelo menos 50 óbitos") 

## Histogramas
sint_covid_pea %>% 
  ggplot(aes(obitos_covid_cada_100_cbo_4)) + 
  geom_histogram(aes(y = ..density..), bins = 40, alpha = .7) +
  geom_density(aes(y = ..density..), alpha = .3) +
  labs(x = "Óbitos covid a cada 100 óbitos na categoria",
       caption = "* Ocupações com pelo menos 50 óbitos") 

## Top 10
###
top_10_covid_pea <- sint_covid_pea %>%
  top_n(10, obitos_covid) %>%
  arrange(desc(obitos_covid))

top_10_covid_pea_cada_100 <- sint_covid_pea %>%
  top_n(10, obitos_covid_cada_100_cbo_4) %>%
  arrange(desc(obitos_covid_cada_100_cbo_4))

###
top_10_covid_pea
top_10_covid_pea_cada_100

## Dispersão
###
top_10_covid_pea %>% 
  ggplot(aes(obitos_covid, obitos_cbo_4), label = cbo_4) + 
  geom_point(aes(colour = cbo_4), shape = 1) +
  geom_label_repel(aes(label = cbo_4, fill = factor(cbo_4)),
                   colour = "white", fontface = "bold") +
  scale_x_continuous(breaks = seq(0, 3800, 200)) +
  labs(caption = "Fonte: SIM/DATASUS, 2020",
       title = "Óbitos covid na categoria vs óbitos na categoria",
       x = "",
       y = "") +
  theme(plot.title = element_text(size = 14, face = "bold"), 
        legend.position = "none")

###
top_10_covid_pea_cada_100 %>% 
  ggplot(aes(obitos_covid_cada_100_cbo_4, obitos_covid), label = cbo_4) + 
  geom_point(aes(colour = factor(cbo_4), size = obitos_cbo_4)) +
  # geom_jitter(aes(colour = factor(cbo_4), size = obitos_cbo_4)) +
  # ggrepel::geom_text_repel(aes(label = cbo_4, size = 500),
  #                          position = position_stack(vjust = 1.05)) +
  geom_label_repel(aes(label = cbo_4),
                   colour = "black", fontface = "bold") +
  scale_x_continuous(breaks = seq(10, 50, 5)) +
  labs(caption = "Fonte: SIM/DATASUS, 2020",
       x = "% de óbitos covid na categoria",
       y = "Óbitos covid na categoria") +
  theme(plot.title = element_text(size=10), legend.position = "none")

# (5) Covid: perfil ocupacional e marcadores sociais da diferença

## Idade
## Dispersão + Regressão Linear
sint_covid_pea %>%
  ggplot(aes(idade_media_cbo_4, obitos_covid_cada_100_cbo_4, 
             size = obitos_cbo_4)) + 
  geom_point(alpha = .3) +
  geom_smooth(method = lm) +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                        aes(label = paste(..eq.label.., 
                                          ..rr.label..,
                                          sep = "~~~")),
                        parse = TRUE) +
  labs(x = "Idade média dos óbitos na categoria",
       y = "% óbitos covid",
       caption = "* Ocupações com pelo menos 50 óbitos") +
  theme(plot.title = element_text(size=10), legend.position = "none")


glimpse(sint_covid_pea)

#
sint_covid_pea %>%
  ggplot(aes(idade_media_cbo_4, obitos_covid)) + 
  geom_point(alpha = .3) +
  geom_smooth(method = lm) +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                        aes(label = paste(..eq.label.., ..rr.label.., 
                                          sep = "~~~")),
                        parse = TRUE) +
  labs(x = "Idade média dos óbitos na categoria",
       y = "Óbitos covid") +
  theme(plot.title = element_text(size=10), legend.position = "none")

## Raça/cor
sint_cor <- pea_sim_2020 %>%
  filter(COVID == 1) %>%
  filter(!is.na(RACACOR)) %>%
  group_by(id_cbo_4, cbo_4, RACACOR) %>%
  summarize(obitos_covid = n_distinct(id_obito)) %>%
  ungroup() %>%
  left_join(pea_cbo_4_cor, by = c("id_cbo_4", "cbo_4", "RACACOR")) %>%
  rowwise() %>%
  mutate(obitos_covid_cada_100_cbo_4_cor = obitos_covid / obitos_cbo_4_cor * 100) %>%
  ungroup() %>%
  filter(!is.na(cbo_4) & obitos_cbo_4_cor > 50)

## NA's em raça/cor
pea_sim_2020 %>%
  filter(COVID == 1) %>%
  filter(is.na(RACACOR)) %>%
  count() ### 739

## Densidade (escala logarítimica)
sint_cor %>% 
  filter(RACACOR != "Indigena") %>%
  ggplot(aes(log(obitos_covid_cada_100_cbo_4_cor), fill = RACACOR)) + 
  geom_density(alpha = .3) +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(x = "Óbitos covid a cada 100 da mesma cor na categoria (log)",
       caption = "* Ocupações com pelo menos 50 óbitos")

## Histogramas de densidade
sint_cor %>% 
  filter(RACACOR != "Indigena") %>%
  ggplot(aes(log(obitos_covid_cada_100_cbo_4_cor))) + 
  geom_histogram(aes(y = ..density..), bins = 30, alpha = .7) +
  geom_density(aes(y = ..density..), alpha = .3) +
  theme_bw() + 
  facet_wrap(~ RACACOR) +
  labs(x = "Óbitos covid a cada 100 da mesma cor na categoria",
       caption = "* Ocupações com pelo menos 50 óbitos")

## Gênero
sint_sexo <- pea_sim_2020 %>%
  filter(COVID == 1) %>%
  group_by(id_cbo_4, cbo_4, SEXO) %>%
  summarize(obitos_covid = n_distinct(id_obito)) %>%
  ungroup() %>%
  left_join(pea_cbo_4_sexo, by = c("id_cbo_4", "cbo_4", "SEXO")) %>%
  rowwise() %>%
  mutate(obitos_covid_cada_100_cbo_4_sexo = obitos_covid / obitos_cbo_4_sexo * 100) %>%
  ungroup() %>%
  filter(!is.na(cbo_4) & obitos_cbo_4_sexo > 50)

##
sint_sexo %>% 
  ggplot(aes(obitos_covid_cada_100_cbo_4_sexo, fill = SEXO)) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(x = "Óbitos covid a cada 100 do mesmo sexo na categoria",
       caption = "* Ocupações com pelo menos 50 óbitos")

# (6) Síntese: óbitos pea e covid por cbo_4, sexo e cor
sint_sexo_cor <- pea_sim_2020 %>%
  filter(COVID == 1) %>%
  filter(!is.na(RACACOR)) %>%
  group_by(id_cbo_4, cbo_4, SEXO, RACACOR) %>%
  summarize(obitos_covid = n_distinct(id_obito)) %>%
  ungroup() %>%
  left_join(pea_cbo_4_sexo_cor, 
            by = c("id_cbo_4", "cbo_4", "SEXO", "RACACOR")) %>%
  rowwise() %>%
  mutate(obitos_covid_cada_100_cbo_4_sexo_cor = obitos_covid / obitos_cbo_4_sexo_cor * 100) %>%
  ungroup() %>%
  filter(!is.na(cbo_4) & obitos_cbo_4_sexo_cor > 50)

## Boxplots
sint_sexo_cor %>% 
  filter(RACACOR != "Indigena") %>%
  ggplot(aes(obitos_covid_cada_100_cbo_4_sexo_cor)) + 
  geom_boxplot() +
  facet_grid(SEXO ~ RACACOR) +
  theme_bw() +
  labs(x = "Óbitos covid a cada 100 da mesma cor e do mesmo sexo na categoria",
       caption = "* Ocupações com pelo menos 50 óbitos")

## Histogramas de densidade
sint_sexo_cor %>% 
  filter(RACACOR != "Indigena") %>%
  # ggplot(aes(log(obitos_covid_cada_100_cbo_4_sexo_cor), fill = RACACOR)) + 
  ggplot(aes(obitos_covid_cada_100_cbo_4_sexo_cor, fill = RACACOR)) + 
  geom_density(alpha = .3) +
  theme_bw() + 
  facet_wrap(~ SEXO) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(x = "Óbitos covid a cada 100 da mesma cor e do mesmo sexo na categoria",
       # x = "Óbitos covid a cada 100 da mesma cor e do mesmo sexo na categoria (log)",
       caption = "* Ocupações com pelo menos 50 óbitos")

# (7) Covid: dinâmica regional e urbana

## Síntese: óbitos pea e covid por centralidade urbana
sint_rm <- pea_sim_2020 %>%
  filter(COVID == 1) %>%
  filter(!is.na(metro)) %>%
  group_by(cbo_4, metro) %>%
  summarize(obitos_covid = n_distinct(id_obito)) %>%
  left_join(pea_cbo_4, by = "cbo_4") %>%
  rowwise() %>%
  mutate(obitos_covid_cada_100_cbo_4 = obitos_covid / obitos_cbo_4 * 100) %>%
  ungroup() %>%
  filter(!is.na(cbo_4) & obitos_cbo_4 > 50)

## Densidades
sint_rm %>%
  ggplot(aes(obitos_covid_cada_100_cbo_4, fill = metro)) + 
  geom_density(alpha = .3) +
  theme(legend.position = "bottom", legend.title = element_blank())


## 
sint_regioes <- pea_sim_2020 %>%
  filter(COVID == 1) %>%
  filter(!is.na(name_region)) %>%
  group_by(cbo_4, name_region) %>%
  summarize(obitos_covid = n_distinct(id_obito)) %>%
  left_join(pea_cbo_4, by = "cbo_4") %>%
  rowwise() %>%
  mutate(obitos_covid_cada_100_cbo_4 = obitos_covid / obitos_cbo_4 * 100) %>%
  ungroup() %>%
  filter(!is.na(cbo_4) & obitos_cbo_4 > 50)

##
sint_regioes %>%
  ggplot(aes(obitos_covid_cada_100_cbo_4)) +
  geom_histogram(aes(y = ..density..), alpha = .7) +
  geom_density(alpha = .3) +
  facet_wrap(~ name_region)

# (8) Covid PEA: escolaridade

##
sint_esc <- pea_sim_2020 %>%
  filter(COVID == 1) %>%
  group_by(cbo_4, ESC) %>%
  summarize(obitos_covid = n_distinct(id_obito)) %>%
  left_join(pea_cbo_4, by = "cbo_4") %>%
  rowwise() %>%
  mutate(obitos_covid_cada_100_cbo_4 = obitos_covid / obitos_cbo_4 * 100) %>%
  ungroup() %>%
  filter(!is.na(cbo_4) & obitos_cbo_4 > 50)

## Densidades
sint_esc %>%
  ggplot(aes(log(obitos_covid_cada_100_cbo_4))) + 
  geom_histogram(aes(y = ..density..), alpha = .7) +
  geom_density(alpha = .3) +
  facet_wrap(~ factor(ESC, 
                      levels = c("NA ou ignorado", "Nenhuma", 
                                 "1 a 3 anos de estudo", 
                                 "4 a 7 anos de estudo",
                                 "8 a 11 anos de estudo",
                                 "12 anos ou mais"))) +
  labs(caption = "* Ocupações com pelo menos 50 óbitos") 

# (9) Aplicando a AED a dados recodificados
## Incorporando a recodificação proposta por análise quali do especialista

##
# drive_auth("evandroluisalves13@gmail.com")
# api_cbo_familia_agregada <- drive_download(as_id("1pRDNqqY30lproDTn3rqSRUqrw7ZFyK9a"),
#                                            path = tempfile(fileext = ".xlsx"),
#                                            overwrite = TRUE)
# 

## Importando
cbo_familia_ajustada <- read_excel(here("data", "cbo_ajust.xlsx")) %>%
  transmute(id_cbo_4,
            id_cbo_4_agreg,
            cbo_4_agreg,
            grupo, hierarquia)

##
pea_sim_2020_ajust <- pea_sim_2020 %>%
  left_join(cbo_familia_ajustada, by = "id_cbo_4")

###
filter(pea_sim_2020_ajust, is.na(hierarquia)) %>%
  count() # 9259 óbitos sem informação ocupacional

###
list(n_ocup_pea = pea_sim_2020_ajust$id_cbo_4,
     n_ocup_ajust = cbo_familia_ajustada$id_cbo_4,
     n_ocup_pea_ajust = pea_sim_2020_ajust %>% filter(COVID == 1) 
     %>% select(id_cbo_4)) %>%
  map_dfr(., n_distinct) #494 famílias de ocupação ajustadas, faltando 291/380

###
pea_cbo_4_agreg <- pea_sim_2020_ajust %>%
  group_by(id_cbo_4_agreg, cbo_4_agreg) %>%
  summarize(obitos_cbo_4_agreg = n_distinct(id_obito)) %>%
  arrange(desc(obitos_cbo_4_agreg))

### Filtradas as ocupações com menos do que 50 óbitos registrados no SIM 2020
sint_covid_pea_ajust <- pea_sim_2020_ajust %>%
  filter(COVID == 1) %>%
  group_by(id_cbo_4, cbo_4, id_cbo_4_agreg, cbo_4_agreg, grupo, hierarquia) %>%
  summarize(obitos_covid = n_distinct(id_obito),
            prop_obitos_covid = obitos_covid / 206646) %>%
  ungroup() %>%
  left_join(pea_cbo_4, by = c("id_cbo_4", "cbo_4")) %>%
  mutate(obitos_covid_cada_100_cbo_4 = obitos_covid / obitos_cbo_4 * 100) %>%
  filter(obitos_cbo_4 > 50)  %>%
  group_by(id_cbo_4_agreg, cbo_4_agreg, grupo, hierarquia) %>%
  summarize(obitos_covid_agreg = sum(obitos_covid),
            prop_obitos_covid_agreg = obitos_covid_agreg / 206646) %>%
  ungroup() %>%
  left_join(pea_cbo_4_agreg, by = c("id_cbo_4_agreg", "cbo_4_agreg")) %>%
  mutate(obitos_covid_cada_100_cbo_4_agreg = obitos_covid_agreg/ obitos_cbo_4_agreg * 100,
         cbo_4_agreg = case_when(is.na(cbo_4_agreg) ~ "Não classificado",
                                 TRUE ~ cbo_4_agreg))

###
sint_covid_pea_grupos <- pea_sim_2020_ajust %>%
  group_by(grupo, hierarquia) %>%
  summarize(obitos_covid_grupo = sum(COVID == 1),
            prop_obitos_covid = obitos_covid_grupo / 206646,
            obitos_grupo = n_distinct(id_obito)) %>%
  ungroup() %>%
  mutate(obitos_covid_cada_100_grupo = obitos_covid_grupo / obitos_grupo,
         grupo = case_when(hierarquia == 5 ~ "Adm's, Contadores e afins",
                           hierarquia == 7 ~ "Comunicação",
                           hierarquia == 4 ~ "Profissionais ens. superior",
                           hierarquia == 18 ~ "Limpeza Urbana",
                           hierarquia == 16 ~ "Indústria de Transformação",
                           is.na(hierarquia) ~ "Outros",
                           TRUE ~ grupo),
         classif = case_when(obitos_covid_cada_100_grupo < .1 ~ "Menos que 10%",
                             obitos_covid_cada_100_grupo >= .1 &
                               obitos_covid_cada_100_grupo < .2 ~ "Entre 10% e 20%",
                             obitos_covid_cada_100_grupo >= .2 ~ "Mais que 20%")) 

###
sint_covid_pea_grupos %>%
  filter(!is.na(hierarquia)) %>%
  ggplot(aes(obitos_covid_grupo, obitos_covid_cada_100_grupo), 
         size = obitos_grupo, label = grupo) + 
  geom_point(aes(colour = classif, size = obitos_grupo), alpha = .5) +
  # geom_jitter(aes(colour = factor(grupo), size = obitos_grupo), alpha = .5) +
  # geom_text_repel(aes(label = grupo), size = 3) +
  # geom_text(aes(label = grupo), size = 3) +
  scale_size(range = c(3, 45), name="Total de óbitos") +
  geom_label_repel(aes(label = paste0(grupo, " ~ ", round(obitos_covid_cada_100_grupo*100), "%")), 
                   family = "Ubuntu",
                   size = 4,
                   segment.curvature = -1e-10,
                   arrow = arrow(length = unit(0.005, "npc"))) +
  # scale_x_continuous(breaks = seq(10, 50, 5)) +
  # scale_y_continuous(limits = c(-.05, .45), labels = percent) +
  scale_y_continuous(limits = c(-.01, .45), labels = function(x) paste0(x*100, "%")) +
  scale_colour_manual(values = c("yellow", "red", "green")) +
  # scale_fill_manual(palette = "RdYIGn") +
  labs(caption = "Fonte: SIM/DATASUS, 2020",
       y = "% óbitos Covid-19 no grupo ocupacional",
       x = "Óbitos Covid-19 no grupo ocupacional") +
  theme(plot.title = element_text(size=10), legend.position = "none",
        axis.text = element_text(size=12),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 15))

# (10) Exportação

##
wb_sint_covid_pea <- createWorkbook("sint_cbo_4_covid_pea")

##
addWorksheet(wb_sint_covid_pea, "grupo ocup. x Óbito PEA e Covid", gridLines = TRUE)
addWorksheet(wb_sint_covid_pea, "CBO_ajust x Óbito PEA e Covid", gridLines = TRUE)


##
writeData(wb_sint_covid_pea, sint_covid_pea_grupos, sheet = 1, rowNames = FALSE)
writeData(wb_sint_covid_pea, sint_covid_pea_ajust, sheet = 2, rowNames = FALSE)

##
saveWorkbook(wb_sint_covid_pea, here("outputs", "sint_covid_pea_cbo.xlsx"), 
             overwrite = TRUE)

