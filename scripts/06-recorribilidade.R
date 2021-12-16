# Recorribilidade ---------------------------------------------------------

# --------- Leitura dos dados históricos
recor_hist <- read_excel("dados/relatorio_historico.xlsx",
                         sheet = "recorribilidade")

# Tabela histórica - Recorribilidade ----------------------------
recor_geral <- recor_hist |>
  select(ano,taxa_rec) |>
  mutate(taxa_rec = round(taxa_rec,4)*100)

# Tabela 2021 -------------------------------------------------------------

recorr2021 <- read_excel("dados/dados2021.xlsx",
                         sheet = "RECURSO")

# Limpeza das variáveis
recorr2021 <- janitor::clean_names(recorr2021)

# Verificando NA
recorr2021 |>
  filter(is.na(classe)) |>
  summarise(n = sum(qtd_ocorrencias_processuais))

# Total de recursos - 2021
recursos2021 <- recorr2021 |>
  summarise(n = sum(qtd_ocorrencias_processuais))

# Total de decisões
# >>>>> NOTA: Carregar aba "03-julgamentos" p/ usar decisoes2021
decisoes_total <- decisoes2021 |>
  summarise(n = sum(qtd_ocorrencias_processuais))


taxa_recorr <- round(recursos2021/decisoes_total,4)*100

ano <- 2021

taxa_recorr_2021 <- c(ano,taxa_recorr$n)

tabela_recorr_2021_total <- rbind(recor_geral,taxa_recorr_2021)


# Texto - Recorribilidade - orginiário e recursal -------------------------

# Separando variáveis desejadas
recor_table_tipo <- recorr2021  |>
  select(id_processo:qtd_ocorrencias_processuais) |>
  group_by(classe) |>
  mutate(tipo = ifelse(classe %in% c("ARE","RE", "AI"), "recursal", "originario")) |>
  relocate(tipo, .after = classe)

saveRDS(recor_table_tipo, file = "data_raw/recor_table_tipo.rds")

rec_total <-  recor_table_tipo  |>
  group_by(tipo) |>
  summarise(n = sum(qtd_ocorrencias_processuais))


# Total de decisões - originario x recursal

decisoes_tipo_r <- decisoes2021 |>
  select(id_processo:qtd_ocorrencias_processuais) |>
  group_by(classe) |>
  mutate(tipo = ifelse(classe %in% c("ARE","RE", "AI"), "recursal", "originario")) |>
  relocate(tipo, .after = classe)

 saveRDS(decisoes_tipo_r, file = "data_raw/decisoes_tipo_r.rds")

dec_total_r <-  decisoes_tipo_r |>
  group_by(tipo) |>
  summarise(n = sum(qtd_ocorrencias_processuais))


taxa_recorr <- round(rec_total$n/dec_total_r$n,4)*100

perc <- cbind(rec_total$tipo,taxa_recorr)



# Tabela 29: Taxa de Recorribilidade --------------------------------------
# View(tabela_recorr_2021_total)
 saveRDS(tabela_recorr_2021_total, file = "data_raw/tabela_recorr_2021_total.rds")
