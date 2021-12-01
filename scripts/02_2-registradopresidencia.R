# Processos Registrados à Presidência -------------------------------------

# Resultados 2021
distribuidos2021 <- read_excel("dados/dados2021.xlsx",
                               sheet = "DISTRIBUIDOS")

# Limpeza das variáveis
distribuidos2021 <- janitor::clean_names(distribuidos2021)
# View(acervo2021)

# Verificando NA
distribuidos2021 |>
  filter(is.na(subgrupo_andamento_comissao)) |>
  nrow()

# Retirando NA
distribuidos2021  <-  distribuidos2021 |>
  filter(!is.na(subgrupo_andamento_comissao))


# Total (dist. ministros + reg. à presidência)
distribuidos2021 |>
  select(subgrupo_andamento_comissao) |>
  summarise(n = n())  # Total = 76403

# Separando variáveis desejadas
distribuidos2021_sep <- distribuidos2021 |>
  select(classe:meio_processo, subgrupo_andamento_comissao) |>
  group_by(classe) |>
  mutate(tipo = ifelse(classe %in% c("ARE","RE", "AI"), "recursal", "originario"))

# Registros à Presidência - Total
reg_presid <- distribuidos2021_sep |>
  group_by(subgrupo_andamento_comissao) |>
  filter(subgrupo_andamento_comissao == "Registro à Presidência") |>
  summarise(n = n()) # Registro à Presidência = 44865

# Registros à Presidência - Recursal
reg_presid_recursal <- distribuidos2021_sep |>
  group_by(subgrupo_andamento_comissao) |>
  filter(subgrupo_andamento_comissao == "Registro à Presidência") |>
  filter(tipo == "recursal") |>
  summarise(n = n()) # Registro à Presidência = 44048


# Registros à Presidência - Originário
reg_presid_originario <- distribuidos2021_sep |>
  group_by(subgrupo_andamento_comissao) |>
  filter(subgrupo_andamento_comissao == "Registro à Presidência") |>
  filter(tipo == "originario") |>
  summarise(n = n()) # Registro à Presidência = 817

# Resumo --------------------------------------------------------

# Tabela 13: Processos Registrados à Presidência e Distribuídos aos Ministros --------
View(tabela_perc_total)

# Tabela 14: Processos Registrados à Presidência - Quantidade -------------
# Tabela 15: Percentual de Recursos registrados à Presidência -------------
# Tabela 16: Quantidade de Recursos registrados à Presidência -------------
View(tabela_rec_pres_total)
