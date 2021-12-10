
# Provimento --------------------------------------------------------------

provimento <- read_excel("dados/relatorio_historico.xlsx",
                              sheet = "taxa_provimento")


# Tabela histórica + 2021 - Provimento/Não provimento ----------------------------
prov_geral <- provimento |>
  select(ano,tx_prov:re_prov) |>
  mutate(tx_prov = round(tx_prov,3)*100,
         are_prov = round(are_prov,3)*100,
         re_prov = round(re_prov,3)*100,
         tx_nao_prov = round(tx_nao_prov,3)*100)

# saveRDS(prov_geral, file = "data_raw/prov_geral.rds")

# Falta o gráfico!
