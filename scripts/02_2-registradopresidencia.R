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


# Processos Distribuídos aos Ministros ------------------------------------


# Separando recursal/originário e presidente/ministros
sep_subgrupo <- distribuidos2021 |>
  select(classe:meio_processo, subgrupo_andamento_comissao) |>
  group_by(subgrupo_andamento_comissao) |>
  mutate(tipo = ifelse(classe %in% c("ARE","RE", "AI"), "recursal", "originario")) |>
  mutate(destino = ifelse(subgrupo_andamento_comissao %in% "Registro à Presidência", "Registro à Presidência", subgrupo_andamento_comissao),
         destino = ifelse(subgrupo_andamento_comissao %in% c("Exclusão Ministro","Normal", "Prevenção Relator", "Prevenção Turma"), "Distribuído aos Ministros", subgrupo_andamento_comissao)
  )

# Distribuídos aos Ministros - Total
dist_min_total <- sep_subgrupo |>
  group_by(destino) |>
  filter(destino == "Distribuído aos Ministros") |>
  summarise(n = n())

# Distribuídos aos Ministros - Recursal
dist_min_recursal <- sep_subgrupo |>
  group_by(destino) |>
  filter(destino == "Distribuído aos Ministros") |>
  filter(tipo == "recursal") |>
  summarise(n = n())


# Registros aos Ministros - Originário
dist_min_orig <- sep_subgrupo |>
  group_by(destino) |>
  filter(destino == "Distribuído aos Ministros") |>
  filter(tipo == "originario") |>
  summarise(n = n())


# Percentagem - Recursal (Presidente + Ministros)
perc_presid_recur_2021 <- round(reg_presid_recursal$n/
                                         (reg_presid_recursal$n+dist_min_recursal$n),2)*100


perc_min_recur_2021 <- round(dist_min_recursal$n/
                                      (dist_min_recursal$n+reg_presid_recursal$n),2)*100


# Percentagem - Distribuídos geral

# % Presidência
perc_geral_presid <- round(reg_presid$n/
                                    (reg_presid$n+dist_min_total$n),2)*100

# % Ministros
perc_geral_min <- round(dist_min_total$n/
                                 (dist_min_total$n+reg_presid$n),2)*100


# Juntando tabela histórica + dados 2021 - Presidência

# Tabela 2020 - Registrados à Presidência
hist_presid <- dados_historico |>
  select(ano,reg_presid_orig:reg_presid_total) |>
  mutate(perc_presid_recur = round(perc_presid_recur,2)*100)

# Construindo tabela 2021
pres_orig <- reg_presid_originario$n
pres_recur <- reg_presid_recursal$n
total_rec_pres <- pres_orig+pres_recur
ano <- 2021

# Linha 2021
tab_pres_rec_final <- c(ano,pres_orig,pres_recur,
                        perc_presid_recur_2021,
                        total_rec_pres
)

tabela_rec_pres_total <- rbind(hist_presid,tab_pres_rec_final)


# Juntando tabela histórica + dados 2021 - Ministros

# Tabela 2020 - Distribuídos aos ministros
hist_min <- dados_historico |>
  select(ano,dist_min_orig:dist_min_total) |>
  mutate(perc_min_recur = round(perc_min_recur,2)*100)

# Construindo tabela 2021
min_orig <- dist_min_recursal$n
min_recur <- dist_min_orig$n
total_rec_min <- min_orig+min_recur
ano <- 2021

# Linha 2021
tab_min_rec_final <- c(ano,min_orig,min_recur,
                       perc_min_recur_2021,
                       total_rec_min)

tabela_rec_min_total <- rbind(hist_min,tab_min_rec_final)


# Tabela histórica - Percentagens
hist_percent_geral <- dados_historico |>
  select(ano,percent_presid:percent_dist) |>
  mutate(percent_presid = round(percent_presid,2)*100,
         percent_dist = round(percent_dist,2)*100)


# Construindo tabela 2021
percent_presid <- perc_geral_presid[1]
percent_min <- perc_geral_min[1]
ano <- 2021

# Linha 2021
tab_percent_geral <- c(ano,percent_presid,percent_min
)
tabela_perc_total <- rbind(hist_percent_geral,tab_percent_geral)



# Gráfico: Quantidade total - Presidência x Ministros ------------------------------

full_recebidos <- cbind(tabela_rec_pres_total,tabela_rec_min_total[,-1])

# Total presidencia x dist. ministros
qtd_total_pres_min <- full_recebidos |>
  select(ano, reg_presid_total,dist_min_total)
 saveRDS(qtd_total_pres_min, "qtd_total_pres_min.rds")

# Total (%) recursal presidencia x dist. ministros
qtd_recursal_pres_min <- full_recebidos |>
  select(ano, perc_presid_recur,perc_min_recur)
 saveRDS(qtd_recursal_pres_min, "qtd_recursal_pres_min.rds")

# Total (qtd) recursal presidencia x dist. ministros
qtd_recursal_pres_min_2 <- full_recebidos |>
  select(ano, reg_presid_recur,dist_min_recursal)
 saveRDS(qtd_recursal_pres_min_2, "qtd_recursal_pres_min_2.rds")

# Resumo --------------------------------------------------------

# Tabela 13: Processos Registrados à Presidência e Distribuídos aos Ministros --------
# View(tabela_perc_total)
 saveRDS(tabela_perc_total, "tabela_perc_total.rds")

# Tabela 14: Processos Distribuídos aos Ministros - Quantidade -------------
# Tabela 15: Percentual de Recursos Distribuídos aos Ministros ------------
# Tabela 16: Quantidade de Recursos  Distribuídos aos Ministros -----------
# View(tabela_rec_min_total)
 saveRDS(tabela_rec_min_total, "tabela_rec_min_total.rds")

# Tabela 14: Processos Registrados à Presidência - Quantidade -------------
# Tabela 15: Percentual de Recursos registrados à Presidência -------------
# Tabela 16: Quantidade de Recursos registrados à Presidência -------------
# View(tabela_rec_pres_total)
 saveRDS(tabela_rec_pres_total, "tabela_rec_pres_total.rds")


# Para gráfico: Quantidade total - Presidência x Ministros ------------------------------
# View(qtd_total_pres_min)
 saveRDS(qtd_total_pres_min, "qtd_total_pres_min.rds")





# Resumo --------------------------------------------------------

# Tabela 13: Processos Registrados à Presidência e Distribuídos aos Ministros --------
# View(tabela_perc_total)
# NOTA: Tabela disponível na aba "02_3-distribuicaoaosministros.R"

# Tabela 14: Processos Registrados à Presidência - Quantidade -------------
# Tabela 15: Percentual de Recursos registrados à Presidência -------------
# Tabela 16: Quantidade de Recursos registrados à Presidência -------------
# View(tabela_rec_pres_total)
# NOTA: Tabela disponível na aba "02_3-distribuicaoaosministros.R"
