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
perc_presid_recur_2021 <- paste0(round(reg_presid_recursal$n/
                                         (reg_presid_recursal$n+dist_min_recursal$n),4)*100,"%")


perc_min_recur_2021 <- paste0(round(dist_min_recursal$n/
                                      (dist_min_recursal$n+reg_presid_recursal$n),4)*100,"%")


# Percentagem - Distribuídos geral

# % Presidência
perc_geral_presid <- paste0(round(reg_presid$n/
                                    (reg_presid$n+dist_min_total$n),4)*100,"%")

# % Ministros
perc_geral_min <- paste0(round(dist_min_total$n/
                                 (dist_min_total$n+reg_presid$n),4)*100,"%")


# Juntando tabela histórica + dados 2021 - Presidência

# Tabela 2020 - Registrados à Presidência
hist_presid <- dados_historico |>
  select(ano,reg_presid_orig:reg_presid_total) |>
  mutate(perc_presid_recur = paste0(round(perc_presid_recur,4)*100,"%"))

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
  mutate(perc_min_recur = paste0(round(perc_min_recur,4)*100,"%"))

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
  mutate(percent_presid = paste0(round(percent_presid,4)*100,"%"),
         percent_dist = paste0(round(percent_dist,4)*100,"%"))


# Construindo tabela 2021
percent_presid <- perc_geral_presid[1]
percent_min <- perc_geral_min[1]
ano <- 2021

# Linha 2021
tab_percent_geral <- c(ano,percent_presid,percent_min
)
tabela_perc_total <- rbind(hist_percent_geral,tab_percent_geral)

# Resumo --------------------------------------------------------

# Tabela 13: Processos Registrados à Presidência e Distribuídos aos Ministros --------
View(tabela_perc_total)

# Tabela 14: Processos Distribuídos aos Ministros - Quantidade -------------
# Tabela 15: Percentual de Recursos Distribuídos aos Ministros ------------
# Tabela 16: Quantidade de Recursos  Distribuídos aos Ministros -----------
View(tabela_rec_min_total)


# Tabela 14: Processos Registrados à Presidência - Quantidade -------------
# Tabela 15: Percentual de Recursos registrados à Presidência -------------
# Tabela 16: Quantidade de Recursos registrados à Presidência -------------
View(tabela_rec_pres_total)
