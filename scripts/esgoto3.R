library(readxl)
library(dplyr)

# 1. Caminho
caminho_consolidado <- "C:/Users/.../Esgoto - Consolidado UF_MR_BR/SINISA_ESGOTO_Indicadores_UF_MR_BR_2024.xlsx"

# 2. Leitura dos dados (Pulando as 11 linhas padrão)
# Usaremos as posições que o seu diagnóstico revelou: 1, 3 e 18
dados_mg <- read_excel(caminho_consolidado, skip = 11, col_names = FALSE)

# 3. Extração Direta do Benchmark de Minas Gerais
benchmark_minas <- dados_mg %>%
  # Filtra pela coluna 1 onde está o nome da UF
  filter(grepl("Minas Gerais", ...1)) %>%
  mutate(
    Atendimento_MG = as.numeric(as.character(...3)),  # IES0001
    Tratamento_MG = as.numeric(as.character(...18))   # IES2004
  ) %>%
  select(Atendimento_MG, Tratamento_MG) %>%
  slice(1)

print("--- BENCHMARK ESTADUAL ENCONTRADO (IES0001 e IES2004) ---")
print(benchmark_minas)