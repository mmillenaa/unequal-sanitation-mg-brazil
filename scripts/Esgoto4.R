# Caminho para a planilha de ETEs (Locais e Regionais)
caminho_etes <- "C:/Users/.../Esgoto - Locais + Regionais/SINISA_ESGOTO_Informações_Estações de Tratamento de Esgoto_Locais e Regionais_2024.xlsx"

# Ver as primeiras linhas (pular cabeçalhos)
etes <- read_excel(caminho_etes, skip = 7, col_names = TRUE)
names(etes)
head(etes, 10)

# Filtrar por município (se houver coluna "Município")
# Se não houver, procure por "Bocaiúva" em todas as colunas
bocaiuva_etes <- etes %>% filter(if_any(everything(), ~ grepl("Bocaiúva", .)))
francisco_etes <- etes %>% filter(if_any(everything(), ~ grepl("Francisco Sá", .)))

print(bocaiuva_etes)
print(francisco_etes)