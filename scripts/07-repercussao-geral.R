library(dplyr)
library(tidyverse)

link = 'dados/repercussao_geral_2021.xlsx'

tab_rg <- readxl::read_excel(link,
                             col_types = c("numeric", "text", "text",
                                           "skip", "skip", "text", "skip", "skip",
                                           "skip", "skip", "skip", "skip", "text",
                                           "text", "skip", "skip", "numeric",
                                           "numeric"), skip = 20)

#View(tab_rg)
tab_rg_view <- janitor::clean_names(tab_rg)

# Retirando NAS
tab_rg_view <- tab_rg_view |>
  filter(!is.na(link_leading_case))

tab_rg_tabela <- tab_rg_view

# tb andamento
# tb_andamento <-
#   tab_rg_view |>
#   group_by(andamento) |>
#   summarise(n = sum(qtd_processos))


# Poderia usar um casewhen, mas resolvi usar o excel para auxiliar na classifica??o

# Copiar os dados do R

#clipr::write_clip(tb_andamento)

#tb_de_para <- clipr::read_clip_tbl()

# Salvei a tabela de DE-PARA

#saveRDS(tb_de_para, file="data_raw/tabela_de_para_rg.rds")

tb_de_para <- readRDS(file="data_raw/tabela_de_para_rg.rds")

#clipr::write_clip(tb_de_para)

# saveRDS(tb_de_para, file = "data_raw/tabela_de_para_rg.rds")
#
# tb_de_para <-
# tb_de_para |>
#   mutate(de_para_andamento = ifelse(de_para_andamento == "Mérito julgado", "Mérito Julgado", de_para_andamento))

tb_de_para1 <-
  tb_de_para |>
  select(-de_para_andamento2)

# Tabela atualizadade RG
tab_rg_view <-
  tab_rg_view |>
  left_join(tb_de_para1, by = "andamento") |>
  relocate(de_para_andamento, .after = andamento)

# Tabela de andamentos
tab_rg_2021 <-
tab_rg_view |>
  group_by(de_para_andamento) |>
  summarise(`2021` = sum(qtd_processos))

# importar da tabela de hist?rico
file_rg = 'dados/relatorio_historico.xlsx'
rg_hist <- readxl::read_xlsx(path=file_rg, sheet = "rg")


tab_rg_final <-
rg_hist |>
  left_join(tab_rg_2021, by = c("RG" = "de_para_andamento"))


# Preciso conectar com a tabela antiga de RG

library(ggplot2)

# Gráfico
tab_rg_final |>
  pivot_longer(
    cols = `2016`:`2021`,
    names_to = 'ano',
    values_to = 'qtd'
  ) |>
  filter(RG %in% c('Repercussão Geral Negada', 'Repercussão Geral Reconhecida')) |>
  ggplot(aes(x = ano, y = qtd, group = RG, shape = RG)) +
  #geom_point() +
  geom_line(aes(colour=RG), size=1.4) +
  #geom_line(aes(y = qtd, shape=destino, fill = factor(destino), colour=destino)) +
  #coord_cartesian(xlim(20000, 110000)) +
  #geom_label(aes(label = qtd), hjust = 0.5, size = 3) +
  ggrepel::geom_label_repel(aes(label = qtd), size = 3) +
  labs(caption = "Fonte: Portal de Informaçõeses Gerenciais em 01/01/2021 e Relatório de Atividades 2021.") +
  scale_fill_brewer(palette = "Blues") +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position="bottom",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        legend.title = element_blank(),
        axis.ticks.y=element_blank()
  )


# Incluir a observação que repercussão geral reconhecida incluí em Repercussão Geral
# Reconhecida o andamento de "Reconhecida a repercussão geral e julgado o mérito com reafirmação de jurisprudência no PV"


# Agora vamos trabalhar com RG julgado mérito e Reafirmação  ------------

tb_de_para2 <-
  tb_de_para |>
  select(-de_para_andamento)

# Tabela atualizadade RG
tab_rg_view <-
  tab_rg_view |>
  left_join(tb_de_para2, by = "andamento") |>
  relocate(de_para_andamento, .after = andamento)

# Tabela de andamentos
tab_rg_2021 <-
  tab_rg_view |>
  group_by(de_para_andamento2) |>
  summarise(`2021` = sum(qtd_processos))

# importar da tabela de hist?rico
#file_rg = 'dados/relatorio_historico.xlsx'
#rg_hist <- readxl::read_xlsx(path=file_rg, sheet = "rg")


tab_rg_final <-
  rg_hist |>
  left_join(tab_rg_2021, by = c("RG" = "de_para_andamento2"))


# Preciso conectar com a tabela antiga de RG

library(ggplot2)

# Gráfico de Mérito e Reafirmação da Jurisprudência
tab_rg_final |>
  pivot_longer(
    cols = `2016`:`2021`,
    names_to = 'ano',
    values_to = 'qtd'
  ) |>
  filter(RG %in% c('Mérito Julgado', 'Reafirmação de Jurisprudência')) |>
  ggplot(aes(x = ano, y = qtd, group = RG, shape = RG)) +
  #geom_point() +
  geom_line(aes(colour=RG), size=1.4) +
  #geom_line(aes(y = qtd, shape=destino, fill = factor(destino), colour=destino)) +
  #coord_cartesian(xlim(20000, 110000)) +
  #geom_label(aes(label = qtd), hjust = 0.5, size = 3) +
  ggrepel::geom_label_repel(aes(label = qtd), size = 3) +
  labs(caption = "Fonte: Portal de Informaçõeses Gerenciais em 01/01/2021 e Relatório de Atividades 2021.") +
  scale_fill_brewer(palette = "Blues") +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position="bottom",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        legend.title = element_blank(),
        axis.ticks.y=element_blank()
  )


## Tabela de Mérito Julgado

tab_rg_final <-
tab_rg_tabela |>
  filter(andamento %in% c('Julgado mérito de tema com repercussão geral',
                          'Reconhecida a repercussão geral e julgado o mérito com reafirmação de jurisprudência no PV')) |>
  relocate(andamento, .after = 'link_processo') |>
  select(-qtd_processos)


# Sobrestados
# https://paineis.cnj.jus.br/QvAJAXZfc/opendoc.htm?document=qvw_l%2FPainelCNJ.qvw&host=QVS%40neodimio03&anonymous=true&sheet=STF

file = "dados/sobrestados.xlsx"
sobrestados <- readxl::read_excel(path = file) |>
  janitor::clean_names() |>
  mutate(tema = stringr::str_remove(tema, pattern = "STF RG "))


# Juntar as duas tabelas
tab_rg_final |>
  left_join(sobrestados, by = c("link_leading_case" = "tema")) |>
  select(-num_tema) |>
  mutate(data_andamento = lubridate::as_date(data_andamento, origin = "1899-12-30")) |>
  arrange(desc(qtd_processos)) |>
  rename(tema = link_leading_case,
         data = data_andamento,
         relator = relator_atual,
         processo = link_processo) |>
  janitor::adorn_totals('row') |>
  DT::datatable(options(pageLength = 60))
