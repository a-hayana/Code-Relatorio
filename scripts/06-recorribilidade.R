
# Recorribilidade ---------------------------------------------------------

# --------- Leitura dos dados históricos
recor_hist <- read_excel("dados/relatorio_historico.xlsx",
                                 sheet = "recorribilidade")

# Tabela histórica - Recorribilidade ----------------------------
recor_geral <- recor_hist |>
  select(ano,taxa_rec) |>
  mutate(taxa_rec = paste0(round(taxa_rec,1)*100,"%"))

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


taxa_recorr <- paste0(round(recursos2021/decisoes_total,2)*100,"%")

ano <- 2021

taxa_recorr_2021 <- c(ano,taxa_recorr)

tabela_recorr_2021_total <- rbind(recor_geral,taxa_recorr_2021)


# Tabela 29: Taxa de Recorribilidade --------------------------------------
View(tabela_recorr_2021_total)

