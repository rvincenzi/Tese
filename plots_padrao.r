# Carregar pacotes necessários
if (!require("readxl")) install.packages("readxl", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("openxlsx")) install.packages("openxlsx", dependencies = TRUE)

library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)

# Carregar configurações de cores, escalas e temas
source("config_padronizacao.R")

# Função para carregar dados
carregar_dados <- function(caminho_dados_long, caminho_curvas_media) {
  dados_long <- read_excel(caminho_dados_long)
  curvas_media <- read_excel(caminho_curvas_media)
  return(list(dados_long = dados_long, curvas_media = curvas_media))
}

# Função para gerar gráficos de curva de crescimento
gerar_graficos_curva_crescimento <- function(dados_long, nome_dataset) {
  # Filtrar dados até a concentração 0.5
  dados_long_filtrado <- dados_long %>% filter(Concentracao <= 0.5)
  
  for (i in unique(dados_long_filtrado$Placa)) {
    plot_replicata <- ggplot(dados_long_filtrado %>% filter(Placa == i), 
                             aes(x = Time, y = DO, color = Concentracao, group = interaction(Concentracao, linhagem))) +
      geom_point(size = 1) +
      #geom_line(size = 0.8) +
      facet_wrap(~ linhagem) +
      scale_y_log10(limits = limite_y) +
      scale_x_continuous(limits = limite_x) +
      scale_color_manual(values = cores_concentracao) +
      labs(
        title = paste("Curva de Crescimento por Replicata e Concentração (até 0.5) -", i, "-", nome_dataset),
        x = "Tempo (h)",
        y = "Densidade Óptica (log)"
      ) +
      tema_padrao
    
    # Salvar o gráfico
    ggsave(
      filename = paste0("plot_replicatas_", nome_dataset, "_", i, "_ate_0.5.png"),
      plot = plot_replicata,
      width = 8, height = 6, dpi = 300, bg = "white"
    )
  }
}

# Função para gerar gráficos de médias
gerar_graficos_medias <- function(curvas_media, nome_dataset) {
  # Filtrar dados até a concentração 0.5
  curvas_media_filtrado <- curvas_media %>% filter(Concentracao <= 0.5)
  
  plot_medias <- ggplot(curvas_media_filtrado, aes(x = Time, y = Media_DO, color = Concentracao, group = Concentracao)) +
    geom_point(size = 1) +
    #geom_line(size = 0.8) +
    geom_errorbar(aes(ymin = Media_DO - DesvioPadrao_DO, ymax = Media_DO + DesvioPadrao_DO), 
                  width = 0.2, size = 0.5) +
    scale_y_log10(limits = limite_y) +
    scale_x_continuous(limits = limite_x) +
    scale_color_manual(values = cores_concentracao) +
    labs(
      title = paste("Curva de Crescimento Média por Concentração (até 0.5) -", nome_dataset),
      x = "Tempo (h)",
      y = "Densidade Óptica (log)"
    ) +
    facet_wrap(~ linhagem) +
    tema_padrao
  
  # Salvar o gráfico
  ggsave(
    filename = paste0("plot_medias_", nome_dataset, "_ate_0.5.png"),
    plot = plot_medias,
    width = 8, height = 6, dpi = 300, bg = "white"
  )
}

# Função principal
processar_dataset <- function(caminho_dados_long, caminho_curvas_media, nome_dataset) {
  # Carregar dados
  dados <- carregar_dados(caminho_dados_long, caminho_curvas_media)
  
  # Gerar gráficos de curva de crescimento (até 0.5)
  gerar_graficos_curva_crescimento(dados$dados_long, nome_dataset)
  
  # Gerar gráficos de médias (até 0.5)
  gerar_graficos_medias(dados$curvas_media, nome_dataset)
}

# Exemplo de uso
caminho_dados_long <- "dados_long_t100C.xlsx"
caminho_curvas_media <- "curvas_media_t100C.xlsx"
nome_dataset <- "T100"

processar_dataset(caminho_dados_long, caminho_curvas_media, nome_dataset)