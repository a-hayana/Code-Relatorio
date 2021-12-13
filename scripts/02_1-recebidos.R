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



# Gráfico 2021 - Porcentagem por classe recursal -----------------------------------------

class_rec <- c(ARE,AI,RE)

total_perc <- round(class_rec/sum(class_rec)*100,2)
class_names <- c("ARE","AI","RE")
df_classes <- data.frame(class_names,total_perc)

df_classes <- df_classes |>
  arrange(desc(total_perc))

# saveRDS(df_classes, file = "data_raw/df_classes.rds")



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



# Gráfico 2021 - Tabela - classes

grafico_classes <- tab_total_class |>
  group_by(grupo_de_classe) |>
  select(grupo_de_classe,ano_2021) |>
  summarise(qtd = sum(ano_2021)) |>
  mutate(percent_classes = round(qtd/sum(qtd),4)*100) #|>
  # janitor::adorn_totals()

# saveRDS(grafico_classes, file = "data_raw/grafico_classes.rds")


# Recebimentos e baixa ----------------------------------------------------

# Leituras

# Tabela histórica
produtiv_hist <- read_excel("dados/relatorio_historico.xlsx",
                            sheet = "produtividade")

# Baixados - 2021
baixados2021 <- read_excel("dados/dados2021.xlsx",
                           sheet = "BAIXADOS")

# Recebidos - 2021 ->  recebidos2021


# Separando variáveis desejadas - Tabela histórica
receb_baixa_hist <- produtiv_hist|>
  select(ano,recebimento:acervo_final) |>
  mutate(taxa_produtiv = paste0(round(taxa_produtiv,4)*100,"%"))


# Dados 2021 - recebidos, baixados e taxa de produtividade
recebimento2021 <- recebidos2021 |>
  nrow()

baixa2021 <- baixados2021 |>
  nrow()

taxa_produtividade <- paste0(round(baixa2021/recebimento2021,4)*100,"%")

ano <- 2021

rec_baixa_2021 <- c(ano,
                    recebimento2021,
                    baixa2021,
                    taxa_produtividade,
                    tab1$acervo_total)

# >>>>> NOTA: Carregar aba "01-acervo" para puxar tab1

tabela_rec_baixa_total <- rbind(receb_baixa_hist,rec_baixa_2021)


# Resumo --------------------------------------------------------

# Tabela 12: Processos Recebidos ------------------------------------------
# Tabela 17: Recursos Recebidos por Classe --------------------------------
# Tabela 19: Processos Originários por ano --------------------------------
# View(tabela_rec_total)
# saveRDS(tabela_rec_total,  file = "data_raw/tabela_rec_total.rds")

# Tabela 21: Recebimento de Processos Originários por Classe --------------
# View(tab_total_class)
# saveRDS(tab_total_class,  file = "data_raw/tab_total_class.rds")

# Tabela 30: Recebimento e Baixa de Processos -----------------------------
# View(tabela_rec_baixa_total)
#saveRDS(tabela_rec_baixa_total, file = "data_raw/tabela_rec_baixa_total.rds")


# Tabela 18: Percentual - Processos Recebidos por Classe - 2021 -----------
# View(grafico_classes)

