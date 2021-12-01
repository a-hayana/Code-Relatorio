
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

# PENDENTE JUNTAR COM A TABELA HISTÓRICA

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


# Quantitativo de Decisões do Plenário por Classe -------------------------

# PENDENTE!

# Organização da tabela histórica - 2016 - 2020

dec_plen_class <- read_excel("dados/relatorio_historico.xlsx",
                             sheet = "dec_plen_class")


tab_pleno_hist <- dec_plen_class |>
  merge(dec_plen_class, all = TRUE) |>
  tidyr::pivot_wider(
    names_from = ano,
    values_from = resultados
  )

# Falta 2021!



# Quantitativo de Decisões por Orgão Julgador -----------------------------

# PENDENTE!

dec_org_julg_hist <- read_excel("dados/relatorio_historico.xlsx",
                           sheet = "dec_orgaos")


# Organização da tabela histórica
tab_org_julg_hist <- dec_org_julg_hist |>
  merge(dec_org_julg, all = TRUE) |>
  tidyr::pivot_wider(
    names_from = ano,
    values_from = resultados
  )


# Falta 2021!


# Resumo ------------------------------------------------------------------

# Tabela 23: Quantitativo de Decisões por Espécie -------------------------
View(tabela_dec_especies_2021)



# Tabela 22: Decisões - Finais e Total ------------------------------------
# Tabela 24: Quantitativo de Decisões Monocráticas ------------------------
# Tabela 25: Quantitativo de Decisões Colegiadas --------------------------
View(tabela_dec)


