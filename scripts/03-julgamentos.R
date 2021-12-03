
# Julgamentos e decisões --------------------------------------------------

# Carregando pacotes
library(readxl)
library(dplyr)


# Leitura dos dados históricos --------------------------------------------

# Tabela histórica - Aba "Julgamentos"
dados_historico <- read_excel("dados/relatorio_historico.xlsx",
                              sheet = "julgamentos")


# Tabela histórica - Aba "Espécies"
desc_especies <- read_excel("dados/relatorio_historico.xlsx",
                            sheet = "dec_especie")



# Leitura e limpeza dos dados 2021 - Decisões -----------------------------

# Organizando dados 2021
decisoes2021 <- read_excel("dados/dados2021.xlsx",
                           sheet = "DECISOES")

# Limpeza das variáveis
decisoes2021 <- janitor::clean_names(decisoes2021)

# Verificando NA
decisoes2021 |>
  filter(is.na(subgrupo_andamento_comissao)) |>
  nrow()

# Retirando NA
decisoes2021  <-  decisoes2021 |>
  filter(!is.na(tipo_decisao))



# Organização da tabela histórica - Decisões por Espécie --------------------------------------

tab_especies_hist <- desc_especies |>
  merge(desc_especies, all = TRUE) |>
  tidyr::pivot_wider(
    names_from = ano,
    values_from = resultados
  )

# Tabela 23: Quantitativo de Decisões por Espécie -------------------------

# Decisões por espécies --> (Todas as 7 espécies)
full_especies <- decisoes2021 |>
  group_by(subgrupo_andamento_comissao) |>
  summarise(n = sum(qtd_ocorrencias_processuais))

# Criação de subgrupo2 para mesclar "Decisão" em "Decisão Interlocutória"
dec_espec_sub2 <- decisoes2021 |>
  group_by(subgrupo_andamento_comissao) |>
  mutate(subgrupo2 = ifelse(subgrupo_andamento_comissao %in% "Decisão", "Decisão Interlocutória", subgrupo_andamento_comissao)) |>
  relocate(subgrupo2, .after = subgrupo_andamento_comissao)

# Total - Todas as SEIS espécies
dec_espec_2021 <- dec_espec_sub2 |>
  group_by(subgrupo2) |>
  summarise(n = sum(qtd_ocorrencias_processuais))


dec_sobrestamento <- dec_espec_2021$n[6]
dec_reperc_geral <- dec_espec_2021$n[5]
dec_final_tab <- dec_espec_2021$n[2]
dec_rec_interno <- dec_espec_2021$n[1]
dec_interloc <- dec_espec_2021$n[3]
dec_liminar <- dec_espec_2021$n[4]


dec_especies_2021 <- c(dec_sobrestamento,dec_reperc_geral,
             dec_final_tab,dec_rec_interno,dec_interloc,
             dec_liminar)


tabela_dec_especies_2021 <- cbind(tab_especies_hist,"2021" = dec_especies_2021)


# Tabela 22: Decisões - Finais --------------------------------------------

# Total por decisao final
dec_final <- dec_espec_sub2 |>
  filter(subgrupo2 == "Decisão Final") |>
  group_by(subgrupo2) |>
  summarise(n = sum(qtd_ocorrencias_processuais))
# Total = 66510


# Tabela 22: Decisões - Total ---------------------------------------------

# Total - monocráticas e colegiadas
dec_mon_col <- decisoes2021 |>
  filter(tipo_decisao %in% c("MONOCRÁTICA", "COLEGIADA")) |>
  group_by(tipo_decisao) |>
  summarise(n = sum(qtd_ocorrencias_processuais))
# Total = 86397

# >>>>>>> Nota "NÃO INFORMADO" definido como "MONOCRÁTICA"

# Total decisoes
total_dec <- decisoes2021 |>
  summarise(n = sum(qtd_ocorrencias_processuais))

monocraticas <- dec_mon_col$n[2]
colegiadas <- dec_mon_col$n[1]
total_dec <- monocraticas + colegiadas
ano <- 2021

tab_dec <- c(ano,dec_final$n,total_dec,monocraticas,colegiadas)

tabela_dec <- rbind(dados_historico,tab_dec)


# Quantitativo de Decisões por Orgão Julgador -----------------------------


dec_org_julg_hist <- read_excel("dados/relatorio_historico.xlsx",
                           sheet = "dec_orgaos")


# Organização da tabela histórica
tab_org_julg_hist <- dec_org_julg_hist |>
  merge(dec_org_julg_hist, all = TRUE) |>
  tidyr::pivot_wider(
    names_from = ano,
    values_from = resultados
  )

dec_org_julg2 <- decisoes2021 |>
  group_by(orgao_julgador) |>
  mutate(orgao_julgador2 = if_else(orgao_julgador %in% c("1ª TURMA", "1ª TURMA - SESSÃO VIRTUAL"), "Primeira Turma",
                         if_else(orgao_julgador %in% c("2ª TURMA", "2ª TURMA - SESSÃO VIRTUAL"), "Segunda Turma",
                                if_else(orgao_julgador %in% c("TRIBUNAL PLENO", "TRIBUNAL PLENO - SESSÃO VIRTUAL"), "Plenário",
                                    if_else(orgao_julgador %in% "PLENÁRIO VIRTUAL - RG", "Plenário Virtual - RG", orgao_julgador))))) |>
  relocate(orgao_julgador2, .after = orgao_julgador)



# Total - Por órgão - 2021
dec_orgao_2021 <- dec_org_julg2 |>
  group_by(orgao_julgador2) |>
  filter(orgao_julgador2 %in% c("Primeira Turma", "Segunda Turma",
                            "Plenário","Plenário Virtual - RG")) |>
  summarise(n = sum(qtd_ocorrencias_processuais))


# Juntando as tabelas (histórica + 2021)
plenario <- dec_orgao_2021$n[1]
plen_virtual_rg <- dec_orgao_2021$n[2]
primeira_turma <- dec_orgao_2021$n[3]
segunda_turma <- dec_orgao_2021$n[4]


tab_orgao <- c(plen_virtual_rg,
               plenario,
               primeira_turma,
               segunda_turma)

tabela_final_orgao <- cbind(tab_org_julg_hist,"2021" = tab_orgao)



# Quantitativo de Decisões do Plenário por Classe -------------------------


# Organização da tabela histórica - 2016 - 2020

dec_plen_class <- read_excel("dados/relatorio_historico.xlsx",
                             sheet = "dec_plen_class")



# Organizando tabela histórica
tab_pleno_hist <- dec_plen_class |>
  merge(dec_plen_class, all = TRUE) |>
  tidyr::pivot_wider(
    names_from = ano,
    values_from = resultados
  )

#
# dec_org_julg2 |>
#   group_by(orgao_julgador) |>
#   summarise(n = n())


# Organizando dados do plenário por classe - 2021
tab_plen_class <- dec_org_julg2 |>
  group_by(orgao_julgador2, classe) |>
  filter(orgao_julgador2 %in% c("Plenário","Plenário Virtual - RG")) |>
  mutate(plen_classe = if_else(classe %in% c("ADC","ADI","ADO","ADPF"), "Controle Concentrado",
                               if_else(classe %in% c("AP","EP","Ext","HC","Inq","PPE","RC","RHC","RvC"), "Classes Criminais",
                                       if_else(classe %in% c("ARE","RE", "AI"), "Classes Recursais", "Demais Classes Originárias")))) |>
  relocate(plen_classe, .after = classe) |>
  relocate(orgao_julgador2, .after = plen_classe)



# Total - Decisões do plenário por classe - 2021
dec_plen_classe_2021 <- tab_plen_class |>
  group_by(plen_classe) |>
  summarise(n = sum(qtd_ocorrencias_processuais)) #|>
  #janitor::adorn_totals("row")


# Juntando as tabelas (histórica + 2021)
class_criminais <- dec_plen_classe_2021$n[1]
class_recursais <- dec_plen_classe_2021$n[2]
control_concent <- dec_plen_classe_2021$n[3]
demais_class <- dec_plen_classe_2021$n[4]


tab_plen_2021 <- c(class_criminais,
                   class_recursais,
                   control_concent,
                   demais_class)

tabela_final_plen <- cbind(tab_pleno_hist,"2021" = tab_plen_2021)



# Resumo ------------------------------------------------------------------

# Tabela 23: Quantitativo de Decisões por Espécie -------------------------
View(tabela_dec_especies_2021)
# saveRDS(tabela_dec_especies_2021, "tabela_dec_especies_2021.rds")


# Tabela 22: Decisões - Finais e Total ------------------------------------
# Tabela 24: Quantitativo de Decisões Monocráticas ------------------------
# Tabela 25: Quantitativo de Decisões Colegiadas --------------------------
View(tabela_dec)
# saveRDS(tabela_dec, "tabela_dec.rds")

# Tabela 26: Quantitativo de Decisões por Orgão Julgador ------------------
View(tabela_final_orgao)
# saveRDS(tabela_final_orgao, "tabela_final_orgao.rds")


# Tabela 28: Quantitativo de Decisões do Plenário por Classe --------------
View(tabela_final_plen)
# saveRDS(tabela_final_plen, "tabela_final_plen.rds")
