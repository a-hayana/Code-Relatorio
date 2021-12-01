
# Recorribilidade ---------------------------------------------------------

# --------- Leitura dos dados históricos
recor_hist <- read_excel("dados/relatorio_historico.xlsx", 
                                 sheet = "recorribilidade")

# Tabela histórica - Recorribilidade ----------------------------
recor_geral <- recor_hist |> 
  select(ano,taxa_rec) |> 
  mutate(taxa_rec = paste0(round(taxa_rec,1)*100,"%"))

# Tabela 2021 -------------------------------------------------------------

