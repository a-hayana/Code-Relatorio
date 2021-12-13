
# Acervo ------------------------------------------------------------------

# Carregando pacotes
library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)

# --------- Leitura dos dados históricos
dados_historico <- read_excel("dados/relatorio_historico.xlsx")


# --------- Leitura dos dados de 2021
acervo2021 <- read_excel("dados/dados2021.xlsx")


# Limpeza das variáveis
acervo2021 <- janitor::clean_names(acervo2021)


# Verificando NA
acervo2021 |>
  filter(is.na(classe))

# Separando variáveis desejadas
acervo2021_sep <- acervo2021 |>
  select(classe:meio_processo) |>
  group_by(classe) |>
  mutate(tipo = ifelse(classe %in% c("ARE","RE", "AI"), "recursal", "originario"))

options(tibble.print_max = 50)


# acervo2021_sep |>
#   group_by(tipo,classe) |>
#   summarise(n = n())

tab1 <- bind_rows(
acervo2021_sep |>
  group_by(tipo) |>
  summarise(n = n()),
acervo2021_sep |>
  group_by(meio_processo) |>
  summarise(n=n()) |>
  janitor::adorn_percentages("col") |>
  rename(tipo = meio_processo)) |>
  tidyr::pivot_wider(names_from = tipo, values_from = n) |>
  janitor::clean_names() |>
  mutate(ano = 2021, .before = originario) |>
  mutate(acervo_total = originario + recursal,
         eletronico = round(eletronico,3),
         fisico = round(fisico,3))

tabela_acervo <- rbind(dados_historico,tab1)



# Acervo por ano de autuação ----------------------------------------------

# Separando variáveis desejadas
acervo2021_sep <- acervo2021 |>
  select(classe:data_autuacao, ramo_direito_novo) |>
  group_by(classe) |>
  mutate(tipo = ifelse(classe %in% c("ARE","RE", "AI"), "recursal", "originario"))

options(tibble.print_max = 50)

acervo_2021 <- saveRDS(acervo2021, file ="data_raw/tabela_acervo2021.rds")

autuacao_levels = c('Anterior a 2017', '2017 a 2018', '2019 a 2020', '2021')

acervo_autuacao <- acervo2021_sep %>%
  mutate(data_autuacao = lubridate::ymd(data_autuacao),
         ano_autuacao = lubridate::year(data_autuacao),
         clas_ano_autuacao =
           case_when(
             ano_autuacao == 2021 ~ '2021',
             ano_autuacao == 2020 | ano_autuacao == 2019 ~ '2019 a 2020',
             ano_autuacao == 2018 | ano_autuacao == 2017 ~ '2017 a 2018',
             TRUE ~ 'Anterior a 2017'
           ),
         clas_ano_autuacao = factor(clas_ano_autuacao, levels = autuacao_levels)) |>
  select(link, data_autuacao, ano_autuacao, clas_ano_autuacao) |>
  group_by(clas_ano_autuacao) |>
  summarise(n = n())

# saveRDS(acervo_autuacao, file ="data_raw/acervo_autuacao.rds")

# Grafico
acervo_autuacao |>
  ggplot2::ggplot(aes(y = clas_ano_autuacao, x = n)) +
  geom_bar(aes(fill = autuacao_levels), color = 'black', stat = "identity",show.legend = FALSE) +
  geom_label(aes(label=n))+
  ggtitle("Acervo por ano de autuação") +
  labs(caption = "Fonte: Portal de Informações Gerenciais em 01/01/2021 e Relatório de Atividades 2021.")+
  scale_fill_brewer(palette = "Blues") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



# Acervo por ramo do direito ----------------------------------------------

# Tabela - qtd e %

ramo_direito <- acervo2021_sep |>
  filter(!is.na(ramo_direito_novo)) |>
  group_by(ramo_direito_novo) |>
  summarise(n = n()) |>
  #arrange(desc(n)) |>
  group_by(ramo_direito_novo = forcats::fct_lump(ramo_direito_novo, n=14, w = n)) |>
  summarise(n = sum(n)) |>
  arrange(desc(n)) |>
  mutate(ramo_direito_novo = ifelse(ramo_direito_novo == "Other", "DEMAIS RAMOS", as.character(ramo_direito_novo))) |>
  mutate(ramo_direito_novo = fct_reorder(ramo_direito_novo, -n),
         ramo_direito_novo = forcats::fct_relevel(ramo_direito_novo, "DEMAIS RAMOS", after = 14)) |>
  mutate(percent_ramo = paste0(round(n/sum(n),4)*100,"%")) |>
  arrange(ramo_direito_novo) |>
  janitor::adorn_totals('row')# |>
  # knitr::kable()


# saveRDS(ramo_direito, file = "data_raw/ramo_direito.rds")


# Apenas informar como nota de rodapé.
faltantes_ramo <- acervo2021_sep |>
  filter(is.na(ramo_direito_novo) | ramo_direito_novo == "NULL") |>
  nrow()



# Resumo ------------------------------------------------------------------
# Tabela 7: Evolução do Acervo --------------------------------------------
# Tabela 8: Evolução do Acervo - Originários e Recursais ------------------
# Tabela 11: Acervo Físico x Eletrônico -----------------------------------
# View(tabela_acervo)
# saveRDS(tabela_acervo, file = "data_raw/tabela_acervo.rds")


# Tabela 9: Acervo por Ano de Autuação
# View(acervo_autuacao)
# saveRDS(acervo_autuacao, file = "data_raw/acervo_autuacao.rds")


# Tabela 10: Acervo por Ramo do Direito -----------------------------------
# View(ramo_direito)
# saveRDS(ramo_direito, file = "data_raw/ramo_direito.rds")
