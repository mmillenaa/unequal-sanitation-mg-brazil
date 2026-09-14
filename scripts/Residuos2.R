library(readxl)
library(dplyr)
library(stringr)

# 1. Caminho
caminho_receitas <- "C:/Users/.../SINISA_RESIDUOS_Informacoes_Formulario_Receitas_e_Cobrancas_2024.xlsx"

# 2. Leitura dos Dados
dados_receitas <- read_excel(caminho_receitas, skip = 11, col_names = FALSE)

# 3. Lista das Cidades
cidades_artigo <- c("Montes Claros", "Bocaiúva", "Bonito de Minas", "Capitão Enéas", 
                    "Claro dos Poções", "Cônego Marinho", "Coração de Jesus", 
                    "Glaucilândia", "Januária", "Juramento", "Juvenília", 
                    "Lontra", "Manga", "Mirabela", "Miravânia", 
                    "Montalvânia", "São João da Lagoa", "São João do Pacuí", 
                    "Francisco Sá", "Patis")

# 4. Raio-X Financeiro
tabela_financeira <- dados_receitas %>%
  select(
    Municipality = 3,         # nom_mun
    UF = 4,                   # uf
    Cobra_Taxa = 22,          # gfi1206 (Sim/Não)
    Forma_Cobranca = 23,      # gfi1207
    Arrecadacao_RS = 20       # gfi1204 (Valor arrecadado)
  ) %>%
  filter(Municipality %in% cidades_artigo & UF == "MG") %>%
  mutate(
    Arrecadacao_RS = suppressWarnings(as.numeric(Arrecadacao_RS)),
    Cobra_Taxa = str_to_title(Cobra_Taxa)
  ) %>%
  arrange(Cobra_Taxa, Municipality)

print("--- SUSTENTABILIDADE FINANCEIRA: COBRANÇA DE LIXO (2024) ---")
print(tabela_financeira, n = 20)