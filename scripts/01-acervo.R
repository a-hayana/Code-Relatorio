
# Acervo ------------------------------------------------------------------

# Carregando pacotes
library(readxl)
library(dplyr)

# --------- Leitura dos dados históricos
dados_historico <- read_excel("dados/relatorio_historico.xlsx")


# --------- Leitura dos dados de 2021
acervo2021 <- read_excel("dados/dados2021.xlsx")


# Limpeza das variáveis
acervo2021 <- janitor::clean_names(acervo2021)


# Verificando NA
acervo2021 |> 
  filter(is.na(classe))

# Separando variáveis desejadas
acervo2021_sep <- acervo2021 |> 
  select(classe:meio_processo) |>
  group_by(classe) |> 
  mutate(tipo = ifelse(classe %in% c("ARE","RE", "AI"), "recursal", "originario")) 

options(tibble.print_max = 50)

# acervo2021_sep |> 
#   group_by(tipo,classe) |> 
#   summarise(n = n()) 

tab1 <- bind_rows(
acervo2021_sep |> 
  group_by(tipo) |>
  summarise(n = n()),
acervo2021_sep |> 
  group_by(meio_processo) |> 
  summarise(n=n()) |> 
  janitor::adorn_percentages("col") |> 
  rename(tipo = meio_processo)) |> 
  tidyr::pivot_wider(names_from = tipo, values_from = n) |> 
  janitor::clean_names() |> 
  mutate(ano = 2021, .before = originario) |> 
  mutate(acervo_total = originario + recursal, 
         eletronico = round(eletronico,3),
         fisico = round(fisico,3))
  
tabela_acervo <- rbind(dados_historico,tab1)


# Resumo ------------------------------------------------------------------
# Tabela 7: Evolução do Acervo --------------------------------------------
# Tabela 8: Evolução do Acervo - Originários e Recursais ------------------
# Tabela 11: Acervo Físico x Eletrônico -----------------------------------
View(tabela_acervo)






