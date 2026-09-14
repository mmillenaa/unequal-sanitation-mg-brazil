# 6. Diagnóstico cirúrgico definitivo
diagnostico_consumo <- dados_agua_ind %>%
  rename(Municipio_Limpo = all_of(col_municipio)) %>%
  filter(Municipio_Limpo %in% municipios_amostra) %>%
  # Ampliando a busca: pega colunas com IN022, consumo ou capita
  select(Municipio_Limpo, matches("IN022|consumo|capita", ignore.case = TRUE)) %>%
  arrange(Municipio_Limpo)

# Imprimir o resultado final!
print(diagnostico_consumo)