
# Produtividade -----------------------------------------------------------

# --------- Leitura dos dados históricos
produtiv_hist <- read_excel("dados/relatorio_historico.xlsx", 
                         sheet = "produtividade")

# Tabela histórica - Produtividade ----------------------------
produtiv_geral_hist <- produtiv_hist |> 
  select(ano:acervo_final) |> 
  mutate(taxa_produtiv = paste0(round(taxa_produtiv,4)*100,"%"))


# Tabela histórica - Temas de RG no Plenário Virtual -----------------------------------
temas_rg_hist <- produtiv_hist |> 
  select(ano,rg_reconhecida:rg_negada)


# Tabela histórica - Temas julgados por ano  -----------------------------------
temas_julg_hist <- produtiv_hist |> 
  select(ano,merito_julgado:reaf_jurisp)


# Tabela 2021 -------------------------------------------------------------
