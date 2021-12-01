
# Provimento --------------------------------------------------------------

prov_hist <- read_excel("dados/relatorio_historico.xlsx", 
                              sheet = "taxa_provimento")


# Tabela histórica - Provimento/Não provimento ----------------------------
prov_geral <- prov_hist |> 
  select(ano,tx_prov,tx_nao_prov) |> 
  mutate(tx_prov = paste0(round(tx_prov,4)*100,"%"),
         tx_nao_prov = paste0(round(tx_nao_prov,4)*100,"%"))


# Tabela 2021 -------------------------------------------------------------


