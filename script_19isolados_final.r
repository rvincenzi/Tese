# Carregar pacotes necessários
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("openxlsx")) install.packages("openxlsx")
install.packages("writexl")
if (!require("viridis")) install.packages("viridis")
if (!require("scales")) install.packages("scales") 

library(scales)


library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(openxlsx)
library(writexl)




# ----------------- correçao da placa 9 -----------------------------

# Defina o caminho para o arquivo
filepath <- "/Users/rvincenzi/Documents/USP/Doutorado/Dados/Escolhidos/19isolados/replicatas.xlsx"

# Ler a aba Plate9_raw
plate9_raw <- read_excel(filepath, sheet = "Plate9_raw")

# Visualizar os dados importados
print(plate9_raw)




# Reorganizando os nomes das colunas (exceto a primeira "time")
rotate_plate <- c("time", rev(colnames(plate9_raw)[2:97]))

# Aplicando a mudança nos nomes das colunas
colnames(plate9_raw) <- rotate_plate

# Exibindo os novos nomes das colunas
print(colnames(plate9_raw))

# Visualizando a tabela reorganizada
print(plate9_raw)

# Definir o caminho e o nome do arquivo Excel
output_filepath <- "/Users/rvincenzi/Documents/USP/Doutorado/Dados/Escolhidos/19isolados/plate9_corrigido.xlsx"

# Exportar os dados para o arquivo Excel
write_xlsx(plate9_raw, path = output_filepath)

# Confirmar que a planilha foi exportada
cat("Planilha exportada para:", output_filepath)

# ---------------------------------------------------------------------------------------------


# Função para carregar e processar dados de uma placa
process_plate <- function(filepath, sheet_name, corrigir_invertida = FALSE, excluir_linha = NULL) {
  raw_data <- read_excel(filepath, sheet = sheet_name)
  
  if (corrigir_invertida) {
    raw_data <- corrigir_plate_invertida(raw_data)
  }
  
  dados_filtrados <- raw_data %>% select(-matches("^[A-H]1$"))
  dados_filtrados <- dados_filtrados %>% select(-starts_with("A"))
  
  if (!is.null(excluir_linha)) {
    dados_filtrados <- dados_filtrados %>% filter(!grepl(excluir_linha, rownames(raw_data)))
  }
  
  if ("Time" %in% colnames(dados_filtrados)) {
    time_col <- dados_filtrados$Time
    dados_filtrados <- dados_filtrados %>% select(-Time)
  } else {
    time_col <- NULL
  }
  
  dados_filtrados[] <- lapply(dados_filtrados, as.numeric)
  
  blanks <- raw_data %>% select(matches("^H[2-9]|^H1[0-2]$"))
  for (blank_col in colnames(blanks)) {
    conc_pattern <- gsub("H", "", blank_col)
    target_cols <- grep(paste0("[B-G]", conc_pattern, "$"), colnames(dados_filtrados), value = TRUE)
    for (col in target_cols) {
      dados_filtrados[[col]] <- dados_filtrados[[col]] - blanks[[blank_col]]
    }
  }
  
  if (!is.null(time_col)) {
    dados_filtrados <- cbind(Time = time_col, dados_filtrados)
  }
  
  return(dados_filtrados)
}

# Caminho do arquivo
filepath <- "/Users/rvincenzi/Documents/USP/Doutorado/Dados/Escolhidos/19isolados/plate_reader_escolhidos/replicatas.xlsx"

# Carregar e processar as placas
placas <- list(
  Plate1  = process_plate(filepath, "Plate1_raw"),
  Plate2  = process_plate(filepath, "Plate2_raw"),
  Plate3  = process_plate(filepath, "Plate3_raw"),
  Plate4  = process_plate(filepath, "Plate4_raw"),
  Plate5  = process_plate(filepath, "Plate5_raw"),
  Plate6  = process_plate(filepath, "Plate6_raw"),
  Plate7  = process_plate(filepath, "Plate7_raw"),
  Plate8  = process_plate(filepath, "Plate8_raw"),
  Plate9  = process_plate(filepath, "Plate9_raw"),
  Plate10 = process_plate(filepath, "Plate10_raw"),
  Plate11 = process_plate(filepath, "Plate11_raw"),
  Plate12 = process_plate(filepath, "Plate12_raw")
)

# Função para nomear isolados com base nas placas e linhas
nomear_isolados_replicata <- function(dados, placas) {
  dados %>%
    filter(Placa %in% placas) %>%
    mutate(
      Isolado = case_when(
        Placa == placas[1] & Linha == "B" ~ "tbe5.am1.d",
        Placa == placas[1] & Linha == "C" ~ "p4d",
        Placa == placas[1] & Linha == "D" ~ "tbe4.am2.d",
        Placa == placas[1] & Linha == "E" ~ "tbe2.ext2.g",
        Placa == placas[1] & Linha == "F" ~ "tlv6.am1.j",
        Placa == placas[1] & Linha == "G" ~ "tlv7.ext2.k",
        Placa == placas[2] & Linha == "B" ~ "tbe2.sob.d",
        Placa == placas[2] & Linha == "C" ~ "tlv7.am1.e",
        Placa == placas[2] & Linha == "D" ~ "colb",
        Placa == placas[2] & Linha == "E" ~ "tlv7.am4.h",
        Placa == placas[2] & Linha == "F" ~ "be6.ext3.d1",
        Placa == placas[2] & Linha == "G" ~ "be6.ext3.d2",
        Placa == placas[3] & Linha == "B" ~ "lv7.am1.l",
        Placa == placas[3] & Linha == "C" ~ "tbe4.ext1.b2",
        Placa == placas[3] & Linha == "D" ~ "be5.am1.g",
        Placa == placas[3] & Linha == "E" ~ "lv7.am3.b",
        Placa == placas[3] & Linha == "F" ~ "tlv7.am1.c",
        Placa == placas[3] & Linha == "G" ~ "tbe5.am1.e"
      )
    )
}

# Aplicando a função de nomeação dos isolados para cada conjunto de placas
dados_combined <- bind_rows(
  lapply(names(placas), function(nome) {
    placa <- placas[[nome]]
    placa$Placa <- nome
    placa
  }),
  .id = "Origem"
)

# Definindo as placas para cada conjunto de isolados (1, 2, 3, etc.)
conjuntos <- list(
  Conjunto1 = c("Plate1", "Plate2", "Plate3"),
  Conjunto2 = c("Plate4", "Plate5", "Plate6"),
  Conjunto3 = c("Plate7", "Plate8", "Plate9"),
  Conjunto4 = c("Plate10", "Plate11", "Plate12")
)

# Aplicando a função nomear_isolados_replicata para os dados
dados_long <- dados_combined %>%
  pivot_longer(
    cols = matches("^[B-G][2-9]|^[B-G]1[0-2]$"),
    names_to = c("Linha", "Concentracao"),
    names_pattern = "([B-G])(2|3|4|5|6|7|8|9|10|11|12)",
    values_to = "DO"
  ) %>%
  mutate(
    Concentracao = as.numeric(case_when(
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
    )),
    DO = ifelse(DO < 0, 0.001, DO),
    Replicata = case_when(
      grepl("Plate[1-3]", Placa)  ~ "Replicata 1",
      grepl("Plate[4-6]", Placa)  ~ "Replicata 2",
      grepl("Plate[7-9]", Placa)  ~ "Replicata 3",
      grepl("Plate1[0-2]", Placa) ~ "Replicata 4"
    )
  )

# Mantendo a coluna Tempo (Time) durante o processo
dados_long_com_isolados <- bind_rows(
  lapply(names(conjuntos), function(conjunto) {
    placas_conjunto <- conjuntos[[conjunto]]
    dados_conjunto <- nomear_isolados_replicata(dados_long, placas_conjunto)
    
    # Garantir que Tempo (Time) está preservado e associado ao Isolado
    dados_conjunto %>%
      select(Time, Placa, Linha, Concentracao, DO, Isolado, Replicata)
  })
)

# Visualizando os dados com a coluna Tempo (Time) já incluída
print(dados_long_com_isolados)

# Salvar os dados processados
write.xlsx(dados_long_com_isolados, file = "dados_long.xlsx", rowNames = FALSE)


# Gerar gráficos para todos os conjuntos com filtro de Time <= 1440
for (nome_conjunto in names(conjuntos)) {
  placas_conjunto <- conjuntos[[nome_conjunto]]
  
  # Nomear isolados no conjunto e filtrar Time <= 1440
  dados_conjunto <- nomear_isolados_replicata(dados_long_com_isolados, placas_conjunto) %>%
    filter(Time <= 1440)
  
  # Plotar o conjunto
  plot_conjunto <- ggplot(dados_conjunto, aes(x = Time, y = DO, color = as.factor(Concentracao), group = Concentracao)) +
    geom_point(size = 1.5) +
    #geom_line(size = 0.8) +
    facet_wrap(~ Isolado, ncol = 4) +  # 4 colunas para melhorar a visualização
    scale_y_log10() +  # Eixo y em log10
    scale_color_viridis_d(option = "viridis") +  # Paleta Viridis
    labs(
      title = paste0("Curvas de Crescimento - ", nome_conjunto),
      x = "Tempo",
      y = "Densidade Óptica (DO, log10)",
      color = "Concentração"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 10, face = "bold"),
      legend.position = "right"
    )
  
  # Salvar o gráfico
  ggsave(
    filename = paste0(tolower(nome_conjunto), "_log10_plot.png"),
    plot = plot_conjunto,
    width = 14,
    height = 10
  )
}



# Agrupar pelos isolados, concentrações e tempo, e calcular a média e o desvio padrão
dados_media <- dados_long_com_isolados %>%
  filter(Time <= 1440) %>%
  group_by(Isolado, Concentracao, Time) %>%
  summarise(
   Media_DO = mean(DO, na.rm = TRUE),
   sd_DO = sd(DO, na.rm = TRUE),
    .groups = "drop"
  )

# Exibindo os resultados
print(dados_media)


write.xlsx(dados_media, file = "dados_media.xlsx", rowNames = FALSE)
# ----------------------- GERAR GRÁFICOS MEDIA-----------------------------

# Carregar o pacote ggplot2, caso necessário
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
library(viridis)

# Plotando os dados com barras de erro para o desvio padrão
# Filtrar os dados para excluir o isolado 9 e os valores NA

  
  
 mean_plot <- ggplot(dados_media, aes(x = Time, y = Media_DO, color = as.factor(Concentracao))) +
  #geom_line(size = 1) +                     # Linha para as médias
  geom_point(size = 0.5) +                    # Pontos para as médias
  geom_errorbar(aes(ymin = pmax(Media_DO - sd_DO, 0.005), #se o sd min for <0,001, o código substitui o valor para 0,001
                    ymax = Media_DO + sd_DO), 
                width = 20,                  # Largura das barras de erro
                alpha = 0.6) +              # Transparência das barras de erro
  scale_y_continuous(trans = "log10",        # Aplica transformação log10 ao eixo Y
                     limits = c(0.001, NA)) +  # Define limite mínimo como 0.001
  scale_color_viridis(discrete = T) +
  facet_wrap(~ Isolado) + # Facetado por isolado
  labs(
    title = "Média Curva de crescimento",
    x = "Time (min)",
    y = "OD600 (log)",
    color = "Concentration"
  ) +
  theme_minimal() +                         # Tema minimalista
  theme(
    strip.text = element_text(size = 10),    # Tamanho do texto dos facetes
    legend.position = "right"               # Posição da legenda
  )

  
# Salvar o gráfico como PNG
ggsave("plot_mean.png", plot = mean_plot, width = 10, height = 8, dpi = 800)


  
  
  
  
  
  
  
  

