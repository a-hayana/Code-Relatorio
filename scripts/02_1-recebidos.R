
# Recebidos ---------------------------------------------------------------

# Carregando pacotes
library(readxl)
library(dplyr)

# --------- Leitura dos dados históricos
dados_historico <- read_excel("dados/relatorio_historico.xlsx",
                                 sheet = "recebidos")


# --------- Leitura dos dados de 2021
recebidos2021 <- read_excel("dados/dados2021.xlsx",
                           sheet = "RECEBIDOS")

# Limpeza das variáveis
recebidos2021 <- janitor::clean_names(recebidos2021)

# Verificando NA
recebidos2021 |>
  filter(is.na(classe)) |>
  nrow()

# Retirando NA
recebidos2021  <-  recebidos2021 |>
  filter(!is.na(classe))

# Separando variáveis desejadas
recebidos2021_sep <- recebidos2021 |>
  select(classe:meio_processo) |>
  group_by(classe) |>
  mutate(tipo = ifelse(classe %in% c("ARE","RE", "AI"), "recursal", "originario"))

#options(tibble.print_max = 50)

# Total geral - originario e recursal
recebidos_geral <- recebidos2021_sep |>
  group_by(tipo) |>
  summarise(n = n())

# Recebidos - Recursal - AI, ARE e RE
recebidos_rec <- recebidos2021 |>
  filter(classe %in% c("ARE","AI","RE")) |>
  group_by(classe) |>
  summarise(n = n())


# Tabela histórica - Recebidos
hist_rec <- dados_historico |>
  select(ano:RE)

originario <- recebidos_geral$n[1]
recursal <- recebidos_geral$n[2]
total_rec <- originario+recursal

ARE <- recebidos_rec$n[2]
AI <- recebidos_rec$n[1]
RE <- recebidos_rec$n[3]
ano <- 2021

# Linha 2021
tab_rec <- c(ano,total_rec,originario,recursal,
             ARE,AI,RE)

tabela_rec_total <- rbind(hist_rec,tab_rec)


# Recebimento de Processos Originários por Classe

# --------- Leitura dos dados históricos
rec_class_hist <- read_excel("dados/relatorio_historico.xlsx",
                             sheet = "recebidos_classe")

`%ni%` <- Negate(`%in%`)

# Organização da tabela histórica
tab_rec_class_hist <- recebidos2021 |>
  filter(classe %ni% c("ARE","RE","AI")) |>
  group_by(classe) |>
  summarise(ano_2021 = n())

# Tabela 2021
tab_total_class <- rec_class_hist |>
  full_join(
    tab_rec_class_hist, by = c("recebidos_classe" = "classe")
  )

tab_total_class[is.na(tab_total_class)] <- 0


# Resumo --------------------------------------------------------

# Tabela 12: Processos Recebidos ------------------------------------------
# Tabela 17: Recursos Recebidos por Classe --------------------------------
# Tabela 19: Processos Originários por ano --------------------------------
View(tabela_rec_total)

# Tabela 21: Recebimento de Processos Originários por Classe --------------
View(tab_total_class)


