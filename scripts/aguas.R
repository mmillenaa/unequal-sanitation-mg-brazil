library(readxl)

# 1. Caminho
caminho_agua <- "C:/Users/.../SINISA_AGUA_Indicadores_Base Municipal_2024.xlsx"

# 2. Verificar quais abas (sheets) existem no arquivo
abas <- excel_sheets(caminho_agua)
print("--- ABAS ENCONTRADAS NO ARQUIVO ---")
print(abas)

# 3. Ler o topo da PRIMEIRA aba para ver o que tem lá
topo_planilha <- read_excel(caminho_agua, sheet = abas[1], n_max = 30, col_names = FALSE)

print("--- VISUALIZANDO AS PRIMEIRAS 20 LINHAS E 5 COLUNAS ---")
print(as.data.frame(topo_planilha[1:20, 1:5]))

# 4. Tentar localizar o IN055 em qualquer lugar dessa amostra
localizacao <- which(topo_planilha == "IN055", arr.ind = TRUE)
print("--- LOCALIZAÇÃO DO CÓDIGO IN055 ---")
print(localizacao)