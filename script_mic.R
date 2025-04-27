# Carregar pacotes necessários
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("openxlsx")) install.packages("openxlsx")

library(readxl)
library(dplyr)
library(openxlsx)

# Função para carregar e processar dados de uma placa
process_plate <- function(filepath, sheet_name) {
  # Carregar os dados brutos
  raw_data <- read_excel(filepath, sheet = sheet_name)
  
  # Remover as colunas cujos nomes terminam com 1 (nessa placa era água)
  dados_filtrados <- raw_data %>% select(-matches("^[A-H]1$"))
  
  # Remover todas as colunas que correspondem à linha A da placa
  dados_filtrados <- dados_filtrados %>% select(-starts_with("A"))
  
  # Converter todas as colunas (exceto Time) para numérico
  dados_filtrados[,-1] <- lapply(dados_filtrados[,-1], as.numeric)
  
  # Manter a coluna de tempo separada para que ela não seja alterada
  time_col <- dados_filtrados$`Time`
  
  # Subtrair o valor do blank correspondente para cada linha e concentração
  dados_corrigidos <- dados_filtrados
  
  # Iterar sobre cada coluna de blank (H2 a H12) e subtrair das colunas correspondentes
  for (blank_col in grep("^H[2-9]|^H1[0-2]$", colnames(dados_filtrados), value = TRUE)) {
    
    # Identificar o padrão da concentração associada ao blank (ex: H3 -> colunas com sufixo 3)
    conc_pattern <- gsub("H", "", blank_col)
    
    # Encontrar todas as colunas que correspondem à mesma concentração
    target_cols <- grep(paste0("[B-G]", conc_pattern, "$"), colnames(dados_corrigidos), value = TRUE)
    
    # Subtrair o valor do blank correspondente de cada linha
    for (col in target_cols) {
      dados_corrigidos[[col]] <- dados_corrigidos[[col]] - dados_filtrados[[blank_col]]
    }
  }
  
  # Adicionar de volta a coluna de tempo sem alterar o formato
  dados_corrigidos <- cbind(Time = time_col, dados_corrigidos[,-1])
  
  return(dados_corrigidos)
}

# Carregar e processar dados das três placas
#plate1 <- process_plate("/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento/mic_t11/mic_t11_16Ago24.xlsx", "Plate1_raw")
#plate2 <- process_plate("/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento/mic_t11/mic_t11_16Ago24.xlsx", "Plate2_raw")
#plate3 <- process_plate("/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento/mic_t11/mic_t11_16Ago24.xlsx", "Plate3_raw")


# Combinar os dados em um único data frame
dados_combined <- bind_rows(plate1, plate2, plate3, .id = "Placa")

# Transformar os dados em formato longo
dados_long <- dados_combined %>%
  select(Placa, Time, B2:G12) %>%  # Seleciona as colunas de interesse
  pivot_longer(
    cols = B2:G12,
    names_to = c("linhagem", "Concentracao"),
    names_pattern = "([B-G])(2|3|4|5|6|7|8|9|10|11|12)",
    values_to = "DO"
  ) %>%
  mutate(
    DO = ifelse(DO < 0, 0.0001, DO),  # Substitui valores negativos por um pequeno valor
    Concentracao = case_when(
      Concentracao == "2" ~ "0",
      Concentracao == "3" ~ "0.1",
      Concentracao == "4" ~ "0.2",
      Concentracao == "5" ~ "0.3",
      Concentracao == "6" ~ "0.4",
      Concentracao == "7" ~ "0.5",
      Concentracao == "8" ~ "0.6",
      Concentracao == "9" ~ "0.7",
      Concentracao == "10" ~ "0.8",
      Concentracao == "11" ~ "0.9",
      Concentracao == "12" ~ "1.0"
    ),
    # Substituir as linhagens
    linhagem = recode(linhagem, "B" = "ctl1", "C" = "ctl2", "D" = "ctl3", "E" = "perc1", "F" = "perc2", "G" = "perc3")
  )

dados_long <- read_excel("/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento_mic/mic_t11/dados_long_rep_t11.xlsx")

# Verificar o resultado
head(dados_long)

# Salvar os dados resultantes em um novo arquivo Excel
write.xlsx(dados_long, file = "/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento/mic_t11/dados_long_rep_t11.xlsx", rowNames = FALSE)

# Calcular a média e o desvio padrão por Time, linhagem e concentração
dados_estatisticas <- dados_long %>%
  group_by(Time, linhagem, Concentracao) %>%
  summarise(
    Media_DO = mean(DO, na.rm = TRUE),
    DesvioPadrao_DO = sd(DO, na.rm = TRUE)
  )

# Verificar o resultado
head(dados_estatisticas)

# Salvar os dados resultantes em um novo arquivo Excel
write.xlsx(dados_estatisticas, file = "/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento_mic/mic_t11/curva_media_t11.xlsx", rowNames = FALSE)







#PLOT MEDIAS

library(ggplot2)
library(dplyr)
library(readxl)
library(openxlsx)

# Carregar os dados de média e desvio padrão do arquivo Excel
#dados_medias <- read_excel("/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento/mic_t11/dados_media_t11.xlsx")


# Criar o gráfico com eixo y em escala logarítmica
plot_medias <- ggplot(dados_estatisticas, aes(x = Time, y = Media_DO, color = Concentracao, group = Concentracao)) +
  geom_point() +
  geom_errorbar(aes(ymin = Media_DO - DesvioPadrao_DO, ymax = Media_DO + DesvioPadrao_DO), width = 0.2) +
  scale_y_log10(limits = c(0.001, NA)) +  # Aplicar transformação logarítmica ao eixo y
  labs(
    title = "Curva de Crescimento Média por Concentração de Mg(ClO4)2",
    x = "Tempo",
    y = "Densidade Óptica (log)"
  ) +
  facet_wrap(~linhagem)+ #multi plot
  theme_minimal()

plot_medias

plot_medias <- ggsave("plot_medias_t11.png", plot = last_plot(), width = 8, height = 6, dpi = 300, bg = "white")























