library(readxl)
library(dplyr)

# 1. Caminho
caminho_tecnico <- "C:/Users/.../SINISA_AGUA_Informações_Gestão Técnica de Água_Base Municipal_2024.xlsx"

# 2. Lendo os dados brutos (pulando até a linha 11, onde os dados começam)
dados_tecnicos_raw <- read_excel(caminho_tecnico, skip = 11, col_names = FALSE)

# 3. Nossas 20 cidades
cidades_artigo <- c("Montes Claros", "Bocaiúva", "Bonito de Minas", "Capitão Enéas", 
                    "Claro dos Poções", "Cônego Marinho", "Coração de Jesus", 
                    "Glaucilândia", "Januária", "Juramento", "Juvenília", 
                    "Lontra", "Manga", "Mirabela", "Miravânia", 
                    "Montalvânia", "São João da Lagoa", "São João do Pacuí", 
                    "Francisco Sá", "Patis")

# 4. Selecionando as colunas estratégicas (Baseado na sua inspeção visual)
# Col 3 = Município | Col 4 = UF
# GTA001 (Paralisações), GTA003 (Interrupções Sistemáticas), GTA006 (Dias em Colapso)
# Nota: Vamos usar a posição 78 que o seu teste achou para Interrupções
resumo_denuncia <- dados_tecnicos_raw %>%
  select(
    Municipality = 3,
    UF = 4,
    Paralisacoes_Total = 76,  # Geralmente GTA001
    Interrupcoes_Sist = 78,   # O que você achou (GTA003)
    Dias_Colapso = 81         # Geralmente GTA006
  ) %>%
  filter(Municipality %in% cidades_artigo & UF == "MG") %>%
  # Converter para número
  mutate(across(-c(Municipality, UF), ~suppressWarnings(as.numeric(.)))) %>%
  arrange(desc(Municipality == "Montes Claros"), desc(Interrupcoes_Sist))

# 5. Resultado Final
print("--- DADOS TÉCNICOS: O CENÁRIO DE INTERMITÊNCIA ---")
print(resumo_denuncia)
View(resumo_denuncia)