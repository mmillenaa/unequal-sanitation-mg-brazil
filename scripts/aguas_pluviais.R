# 1. Carregar pacotes
library(readxl)
library(dplyr)

# 2. Caminho da planilha
caminho_tecnicos <- "C:/Users/.../SINISA_AGUASPLUVIAIS_Informacoes_2024/SINISA_AGUASPLUVIAIS_Informacoes_Formularios_Tecnicos.xlsx"

# 3. Lendo os dados (pulando o cabeçalho sujo)
dados_drenagem <- read_excel(caminho_tecnicos, skip = 10)

# 4. As nossas 20 cidades do artigo
cidades_artigo <- c("Montes Claros", "Bocaiúva", "Bonito de Minas", "Capitão Enéas", 
                    "Claro dos Poções", "Cônego Marinho", "Coração de Jesus", 
                    "Glaucilândia", "Januária", "Juramento", "Juvenília", 
                    "Lontra", "Manga", "Mirabela", "Miravânia", 
                    "Montalvânia", "São João da Lagoa", "São João do Pacuí", 
                    "Francisco Sá", "Patis")

# 5. Filtrando e calculando o Déficit de Infraestrutura
resumo_infra <- dados_drenagem %>%
  filter(Nome_Mun %in% cidades_artigo & UF == "MG") %>%
  select(
    Municipality = Nome_Mun,
    Total_Ruas_Km = `GAP0304*`,
    Ruas_com_Drenagem_Km = `GAP0305*`
  ) %>%
  # Convertendo para número (caso venha como texto do Excel)
  mutate(
    Total_Ruas_Km = as.numeric(Total_Ruas_Km),
    Ruas_com_Drenagem_Km = as.numeric(Ruas_com_Drenagem_Km)
  ) %>%
  # Calculando a porcentagem de cobertura
  mutate(
    Cobertura_Percentual = round((Ruas_com_Drenagem_Km / Total_Ruas_Km) * 100, 1)
  ) %>%
  # Ordenando para Montes Claros ficar no topo
  arrange(desc(Municipality == "Montes Claros"), desc(Cobertura_Percentual))

# 6. Exibindo a tabela reveladora!
print(resumo_infra)