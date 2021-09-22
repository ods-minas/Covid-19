################################################################################
########## MORTOS POR COVID BRASIL 2020 - ANÁLISE DOS DADOS DO SIM #############
################################################################################

###########################
# MODELAGEM E ESTIMATIVAS #
###########################

library(tidyverse)
library(broom)
library(here)
library(googledrive)
library(ggrepel)
library(readxl)
library(openxlsx)

# (1) Estabelecendo o conjunto ajustado de dados para aplicação de modelo
## Conjunto ajustado de dados
pea_sim_2020_ajust <- pea_sim_2020 %>%
  filter(!is.na(SEXO) & RACACOR != "Indigena") %>%
  transmute(id_cbo_4,
            cbo_4,
            id_cbo_1,
            cbo_1 = str_to_sentence(cbo_1, locale = "br"),
            COVID = as.factor(COVID),
            IDADE = as.numeric(IDADE),
            SEXO = as.factor(SEXO),
            RACACOR = as.factor(RACACOR))

## Excluídos
### 1075 observações num universo de 288549 (0.4%)
pea_sim_2020 %>%
  filter(is.na(SEXO) | RACACOR == "Indigena") %>%
  count() 

# (2) Estabelecendo funções aplicadas aos modelos
##
foo <- function(bar) {
  glm(formula = COVID ~ IDADE + interaction(SEXO, RACACOR) + cbo_1, 
      data = bar, 
      family = "binomial")
  }      
##
foos <- function(bar) {
  glm(formula = COVID ~ IDADE + interaction(SEXO, RACACOR), 
      data = bar, 
      family = "binomial")
  }      


# (3) Modelagem geral

##
model <- foo(pea_sim_2020_ajust)

##
summary(model)

##
model_tidy <- tidy(model) %>%
  mutate(OR = exp(estimate),
         IC_inf = exp(estimate - 1.96 * std.error),
         IC_sup = exp(estimate + 1.96 * std.error))

##
view(model_tidy)

# (3) Modelagem por perfil ocupacional

## A) Aninhando os dados e aplicando múltiplas regressões

### Grande grupo ocupacional (CBO 1 dígito)
pea_sim_2020_nested_cbo_1 <- pea_sim_2020_ajust %>%
  group_by(id_cbo_1, cbo_1) %>%
  ungroup() %>%
  nest(-c(id_cbo_1, cbo_1)) %>%
  mutate(models = map(data, ~ foos(.)),
         tidied = map(models, tidy)) %>%
  unnest(tidied)

### Família ocupacional (CBO 4 dígitos)
pea_sim_2020_nested_cbo_4 <- pea_sim_2020_ajust %>%
  group_by(id_cbo_4, cbo_4, id_cbo_1, cbo_1) %>%
  filter(n_distinct(SEXO) >= 2 & n_distinct(RACACOR) >= 2) %>%
  ungroup() %>%
  nest(-c(id_cbo_4, cbo_4, id_cbo_1, cbo_1)) %>%
  mutate(models = map(data, ~ foos(.)),
         tidied = map(models, tidy)) %>%
  unnest(tidied)

## B) Criando conjuntos de dados com razões de chance e IC's

### Grande grupo ocupacional (CBO 1 dígito)
models_tidy_cbo_1 <- pea_sim_2020_nested_cbo_1 %>%
  select(-c(data, models)) %>%
  mutate(OR = exp(estimate),
         IC_inf = exp(estimate - 1.96 * std.error),
         IC_sup = exp(estimate + 1.96 * std.error)) ### IC de 5%

### Família ocupacional (CBO 4 dígitos)
models_tidy_cbo_4 <- pea_sim_2020_nested_cbo_4 %>%
  select(-c(data, models)) %>%
  mutate(OR = exp(estimate),
         IC_inf = exp(estimate - 1.96 * std.error),
         IC_sup = exp(estimate + 1.96 * std.error)) ### IC de 5%

## C) Filtrando modelos por significância estatística

##
models_tidy_cbo_1_signif <- models_tidy_cbo_1 %>%
  filter(!term %in% c("(Intercept)", "IDADE")) %>%
  # mutate(p.adjusted = p.adjust(p.value, method = "bonferroni")) %>%
  # mutate(p.adjusted = if_else(p.value >= .5, 1, p.value/4)) %>%
  # filter(p.adjusted < .1) %>%
  filter(p.value < .05)

##
models_tidy_cbo_4_signif <- models_tidy_cbo_4 %>%
  filter(!term %in% c("(Intercept)", "IDADE")) %>%
  mutate(p.adjusted = p.adjust(p.value)) %>%
  # filter(p.adjusted < .2) %>%
  filter(p.value < .05)

# (4) Checando resultados preliminares

## Checando aninhamento dos dados
# glimpse(pea_sim_2020_nested_cbo_1)
# glimpse(pea_sim_2020_nested_cbo_4)
# glimpse(pea_sim_2020_nested_cbo_1_agreg)
# glimpse(pea_sim_2020_nested_cbo_4_agreg)

## Checando dados excluídos
### ERRO: contrasts can be applied only to factors with 2 or more levels
### Ocupação deve ter pelo menos 2 valores distintos nas variáveis categóricas
pea_sim_2020_ajust %>%
  group_by(id_cbo_4, cbo_4) %>%
  filter(n_distinct(SEXO) < 2 & n_distinct(RACACOR) < 2) %>%
  group_nest() %>% 
  count() ## 32 famílias de ocupação excluídas

# (5) Aplicando modelos a dados recodificados
## Incorporando a recodificação proposta por análise quali do especialista
# 
# ##
# drive_auth("evandroluisalves13@gmail.com")
# api_cbo_familia_agregada <- drive_download(as_id("1pRDNqqY30lproDTn3rqSRUqrw7ZFyK9a"),
#                                            path = tempfile(fileext = ".xlsx"),
#                                            overwrite = TRUE)
# 
# ## Importando
# cbo_familia_ajustada <- read_excel(api_cbo_familia_agregada$local_path) %>%
#   transmute(id_cbo_4,
#             id_cbo_4_agreg,
#             cbo_4_agreg,
#             grupo, hierarquia)

## Importando
cbo_familia_ajustada <- read_excel(here("data", "cbo_ajust.xlsx")) %>%
  transmute(id_cbo_4,
            id_cbo_4_agreg,
            cbo_4_agreg,
            grupo, hierarquia)

## Acrescentando a recodificação ao conjunto de dados
pea_sim_2020_ajust <- pea_sim_2020_ajust %>%
  left_join(cbo_familia_ajustada, by = "id_cbo_4") 

### Excluídos
### 9115 observações num universo de 288549 (3.2%)
pea_sim_2020 %>%
  filter(!is.na(SEXO) & RACACOR != "Indigena") %>%
  anti_join(cbo_familia_ajustada, by = "id_cbo_4") %>%
  count() 

## A - Aninhando os dados e aplicando múltiplas regressões

### Novo grande grupo ocupacional proposto por especialista
pea_sim_2020_nested_cbo_1_agreg <- pea_sim_2020_ajust %>%
  group_by(hierarquia, grupo) %>%
  ungroup() %>%
  nest(-c(hierarquia, grupo)) %>%
  mutate(models = map(data, ~ foos(.)),
         tidied = map(models, tidy)) %>%
  unnest(tidied)

### Nova classificação ocupacional proposta por especialista (agregação cbo_4)
pea_sim_2020_nested_cbo_4_agreg <- pea_sim_2020_ajust %>%
  group_by(id_cbo_4_agreg, cbo_4_agreg) %>%
  filter(n_distinct(SEXO) >= 2 & n_distinct(RACACOR) >= 2) %>%
  ungroup() %>%
  nest(-c(id_cbo_4_agreg, cbo_4_agreg, hierarquia, grupo)) %>% ### Acresc. grupo
  mutate(models = map(data, ~ foos(.)),
         tidied = map(models, tidy)) %>%
  unnest(tidied)

## B - Criando conjuntos de dados com razões de chance e IC's

### Novo grande grupo proposto por especialista
models_tidy_cbo_1_agreg <- pea_sim_2020_nested_cbo_1_agreg %>%
  select(-c(data, models)) %>%
  mutate(OR = exp(estimate),
         IC_inf = exp(estimate - 1.96 * std.error), # IC de 5%
         IC_sup = exp(estimate + 1.96 * std.error)) %>%
  filter(!is.na(grupo))

### Nova classificação ocupacional proposta por especialista (agregação cbo_4)
models_tidy_cbo_4_agreg <- pea_sim_2020_nested_cbo_4_agreg %>%
  select(-c(data, models)) %>%
  mutate(OR = exp(estimate),
         IC_inf = exp(estimate - 1.96 * std.error), # IC de 5%
         IC_sup = exp(estimate + 1.96 * std.error)) %>%
  filter(!is.na(cbo_4_agreg))

## C - Filtrando modelos por significância estatística

### Novo grande grupo ocupacional proposto por especialista
models_tidy_cbo_1_agreg_signif <- models_tidy_cbo_1_agreg %>%
  filter(!term %in% c("(Intercept)", "IDADE")) %>%
  # mutate(p.adjusted = p.adjust(p.value, method = "bonferroni")) %>%
  # mutate(p.adjusted = if_else(p.value >= .5, 1, p.value/4)) %>%
  # filter(p.adjusted < .1) %>%
  filter(p.value < .05)

### Nova classificação ocupacional proposta por especialista (agregação cbo_4)
models_tidy_cbo_4_agreg_signif <- models_tidy_cbo_4_agreg %>%
  filter(!term %in% c("(Intercept)", "IDADE")) %>%
  # mutate(p.adjusted = p.adjust(p.value, method = "bonferroni")) %>%
  # mutate(p.adjusted = if_else(p.value >= .5, 1, p.value/4)) %>%
  # filter(p.adjusted < .1) %>%
  filter(p.value < .05)

# (6) Visualizando resultados

## Número de classificações ocupacionais com diferenças significativas
n_distinct(models_tidy_cbo_1_signif$id_cbo_1) # 9 de 10
n_distinct(models_tidy_cbo_4_signif$id_cbo_4) # 58 de 431
n_distinct(models_tidy_cbo_1_agreg_signif$hierarquia) # 17 de 20
n_distinct(models_tidy_cbo_4_agreg_signif$id_cbo_4_agreg) # 34 de 77 ocupações

## Ocupações recodificadas com diferença significativa no formato tabela
view(models_tidy_cbo_1_agreg_signif)
view(models_tidy_cbo_4_agreg_signif)

## Gráficos com razões de chance

### Novo grande grupo ocupacional proposto pelo especialista
plt_cbo_1 <- models_tidy_cbo_1_agreg_signif %>%
  mutate(yaxis = length(grupo):1,
         term = str_remove(term, "\\s*\\([^\\)]+\\)"),
         term = str_remove(term, "interaction"),
         term = toupper(gsub("\\.", " ", term)),
         term = str_to_sentence(term, locale = "br"),         
         categoria = paste(grupo, term, sep = " - "))

### Razões de chance por grupo ocupacional com diferença demográfica significativa
plt_cbo_1 %>% 
  ggplot(aes(x = OR, y = yaxis)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), size = .5, 
                 height = .2, color = "gray50") +
  geom_point(aes(colour = grupo), size = 3.5) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = plt_cbo_1$yaxis, 
                     labels = plt_cbo_1$categoria) +
  # scale_x_continuous(breaks = seq(0,7,1)) +
  ylab("") +
  xlab("Razão de chances") +
  ggtitle("Razões de chances e Intervalos de confiança \nGrupos Ocupacionais (elaboração própria)") +
  theme(legend.position = "none")

## Nova família de ocupação agregada proposta pelo especialista
plt_cbo_4 <- models_tidy_cbo_4_agreg_signif %>%
  mutate(term = case_when(term == "interaction(SEXO, RACACOR)Homem.Preto ou pardo" ~ "Homem preto ou pardo",
                          term == "interaction(SEXO, RACACOR)Mulher.Preto ou pardo" ~ "Mulher preta ou parda",
                          term == "interaction(SEXO, RACACOR)Mulher.Branco ou Amarelo" ~ "Mulher branca ou amarela",
                          TRUE ~ term),
         cbo_4 = cbo_4_agreg,
         grupo = as.factor(paste0(grupo, " - Grupo ", hierarquia)),
         grupo = fct_reorder(grupo, hierarquia))

### A - Homem negro vs Homem branco
# chances_homem_negro_por_ocupacao <- 
  
plt_cbo_4 %>% 
  filter(term == "Homem preto ou pardo") %>%
  # top_n(10, OR) %>%
  filter(!id_cbo_4_agreg %in% c(3522, 5191, 351, 223, 262, 721, 631, 5161, 3432)) %>%
  ggplot(aes(x = fct_reorder(cbo_4, OR), y = OR)) +
  geom_pointrange(aes(colour = grupo, ymin = IC_inf, ymax = IC_sup), alpha = .5,
                                   size=1) +
  geom_hline(yintercept = 1, colour = "black", linetype = 2, alpha = .2) +
  labs(title = "Estimativa de chances de óbito por covid - homens pretos e pardos comparados a homens brancos",
       # subtitle = " Agregação de famílias de ocupação CBO (4 dígitos)",
       x = "",
       y = "") +
  ggrepel::geom_text_repel(aes(label = paste0(cbo_4, 
                                              ": homens negros têm ",
                                              scales::percent(round(OR - 1, 2)),
                                              if_else(OR < 1, " de chance", " mais chance")
                                              )),
                           alpha = .7,
                           size = 4,
                           direction = "y",
                           # colour = "white",
                           # nudge_y = c(-3, 3),
                           # nudge_y = 3.5,
                           segment.size = 0,
                           max.overlaps = 15) +
  coord_flip() +
  # scale_y_continuous(limits = c(-.6, 6)) +
  scale_y_continuous(limits = c(-.5, 6),
                     breaks = seq(from = -.0, to = 6, by = 1),
                     labels = function(x) paste0(-(1-x)*100, "%")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(.8,.5), # .2 .5
        title = element_text(face = "bold"),
        # axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank())

### B - Mulheres negras vs homens brancos
# chances_mulher_negra_por_ocupacao <- 
  
plt_cbo_4 %>% 
  filter(term == "Mulher preta ou parda") %>%
  ggplot(aes(x = fct_reorder(cbo_4, OR), y = OR)) +
  geom_pointrange(aes(colour = grupo, ymin = IC_inf, ymax = IC_sup), alpha = .5,
                  size=1) +
  geom_hline(yintercept = 1, colour = "black", linetype = 2, alpha = .2) +
  labs(title = "Estimativa de chances de óbito por covid - mulheres negras comparadas a homens brancos",
       x = "",
       y = "") +
  ggrepel::geom_text_repel(aes(label = paste0(cbo_4, 
                                              ": mulheres negras têm ",
                                              scales::percent(round(OR - 1, 2)),
                                              if_else(OR < 1, " de chance", " mais chance"))),
                           # fontface = "bold",
                           alpha = .7,
                           size = 4,
                           direction = "y",
                           # colour = "white",
                           # nudge_y = c(-3, 3),
                           # nudge_y = 3.5,
                           segment.size = 0,
                           max.overlaps = 15) +
  coord_flip() +
  # scale_y_continuous(limits = c(-.6, 2.6)) +
  scale_y_continuous(limits = c(-.5, 6),
                     breaks = seq(from = -.0, to = 6, by = 1),
                     labels = function(x) paste0(-(1-x)*100, "%")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(.8,.4),
        title = element_text(face = "bold"),
        # axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank())

### C - Mulheres brancas vs homens brancos
# chances_mulher_branca_por_ocupacao <- 

plt_cbo_4 %>% 
  filter(term == "Mulher branca ou amarela") %>%
  filter(IC_sup < 4) %>%
  top_n(10, OR) %>%
  ggplot(aes(x = fct_reorder(cbo_4, OR), y = OR)) +
  geom_pointrange(aes(colour = grupo, ymin = IC_inf, ymax = IC_sup), alpha = .5,
                  size=1) +
  geom_hline(yintercept = 1, colour = "black", linetype = 2, alpha = .2) +
  labs(title = "Estimativa de chances de óbito por covid - mulheres brancas comparadas a homens brancos",
       x = "",
       y = "") +
  ggrepel::geom_text_repel(aes(label = paste0(cbo_4, 
                                              ": mulheres brancas têm ",
                                              scales::percent(round(OR - 1, 2)),
                                              if_else(OR < 1, " de chance", " mais chance"))),
                           # fontface = "bold",
                           alpha = .7,
                           size = 4,
                           direction = "y",
                           # colour = "white",
                           # nudge_y = c(-3, 3),
                           # nudge_y = 3.5,
                           segment.size = 0,
                           max.overlaps = 15) +
  coord_flip() +
  scale_y_continuous(limits = c(-.5, 6),
                     breaks = seq(from = -.0, to = 6, by = 1),
                     labels = function(x) paste0(-(1-x)*100, "%")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(.8,.4),
        title = element_text(face = "bold"),
        # axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank())

## Exportando resultados para arquivo no formato planilha

###
wb_glm <- createWorkbook("glm_covid_pea")

###
addWorksheet(wb_glm, "Geral", gridLines = TRUE)
addWorksheet(wb_glm, "CBO_1", gridLines = TRUE)
addWorksheet(wb_glm, "CBO_4", gridLines = TRUE)
addWorksheet(wb_glm, "CBO_4_ajust", gridLines = TRUE)

###
writeData(wb_glm, model_tidy, sheet = 1, rowNames = FALSE)
writeData(wb_glm, models_tidy_cbo_1, sheet = 2, rowNames = FALSE)
writeData(wb_glm, models_tidy_cbo_4, sheet = 3, rowNames = FALSE)
writeData(wb_glm, models_tidy_cbo_4_agreg, sheet = 4, rowNames = FALSE)

###
saveWorkbook(wb_glm, here("outputs", "glm_covid_pea.xlsx"), overwrite = TRUE)

################################################################################