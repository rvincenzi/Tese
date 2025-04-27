# Carregar pacotes necessários
library(dplyr)
library(minpack.lm)
library(ggplot2)
library(writexl)

# Função para ajustar o modelo de Gompertz
ajustar_gompertz <- function(dados, concentracao) {
  mmax <- c()
  mmax_sd <- c()
  C <- c()
  M <- c()
  M_sd <- c()
  Y0 <- c()
  
  # Lista de linhagem únicos
  names <- unique(dados$linhagem)
  
  # Loop para ajustar o modelo para cada isolado
  for (n in seq_along(names)) {
    tabfit <- filter(dados, linhagem == names[n] & Concentracao == concentracao)
    d <- tabfit$Time
    y <- tabfit$Media_DO
    y0 <- min(y)
    
    # Ajustar o modelo de Gompertz
    fit <- tryCatch({
      nlsLM(y ~ y0 + C * exp(-exp(-mmax * (d - M))),
            start = list(mmax = 0.00001, C = (max(y) - min(y)), M = 200))
    }, error = function(e) {
      return(NULL)  # Retornar NULL se o ajuste falhar
    })
    
    if (!is.null(fit)) {
      # Armazenar os parâmetros ajustados
      Y0 <- append(Y0, y0, after = length(Y0))
      mmax <- append(mmax, summary(fit)$parameters[1, 1], after = length(mmax))
      mmax_sd <- append(mmax_sd, summary(fit)$parameters[1, 2], after = length(mmax_sd))
      C <- append(C, summary(fit)$parameters[2, 1], after = length(C))
      M <- append(M, summary(fit)$parameters[3, 1], after = length(M))
      M_sd <- append(M_sd, summary(fit)$parameters[3, 2], after = length(M_sd))
    } else {
      # Se o ajuste falhar, armazenar NA
      Y0 <- append(Y0, NA, after = length(Y0))
      mmax <- append(mmax, NA, after = length(mmax))
      mmax_sd <- append(mmax_sd, NA, after = length(mmax_sd))
      C <- append(C, NA, after = length(C))
      M <- append(M, NA, after = length(M))
      M_sd <- append(M_sd, NA, after = length(M_sd))
    }
  }
  
  # Retornar os parâmetros ajustados
  return(data.frame(
    linhagem = names,
    Concentracao = concentracao,
    Y0 = Y0,
    mmax = mmax,
    mmax_sd = mmax_sd,
    C = C,
    M = M,
    M_sd = M_sd
  ))
}

# Função para calcular os valores preditos
calcular_preditos <- function(dados, resultados) {
  # Converter a coluna Concentracao para o mesmo tipo (numérico)
  dados <- dados %>%
    mutate(Concentracao = as.numeric(Concentracao))
  
  # Fazer a junção e calcular os valores preditos
  dados_preditos <- dados %>%
    left_join(resultados, by = c("linhagem", "Concentracao")) %>%
    mutate(
      y_fit = Y0 + C * exp(-exp(-mmax * (Time - M)))
    )
  
  return(dados_preditos)
}

# Função para plotar os dados observados e preditos
plotar_dados <- function(dados_preditos, nome_dataset) {
  # Criar gráfico
  graf <- ggplot(dados_preditos, aes(x = Time)) +
    geom_point(size = 1, aes(y = Media_DO, color = "Observado")) +
    geom_line(aes(y = y_fit, color = "Predito")) +
    geom_errorbar(aes(ymin = Media_DO - sd_DO, ymax = Media_DO + sd_DO), width = .3, alpha = 0.5) +
    facet_grid(linhagem ~ Concentracao, scales = "free") +
    theme_bw() +
    labs(
      x = 'Time (min)',
      y = 'Optical Density',
      title = paste("Dataset:", nome_dataset),
      color = "Dados"
    ) +
    scale_color_manual(values = c("Observado" = "black", "Predito" = "red"))
  
  # Salvar o gráfico
  nome_arquivo <- paste0("Grafico_", nome_dataset, ".png")
  ggsave(nome_arquivo, graf, device = 'png', unit = 'cm', width = 25, height = 20, dpi = 600)
  
  cat("Gráfico salvo:", nome_arquivo, "\n")
}

# Lista de datasets processados
datasets <- list(
  curvas_media_t11 = read_excel("curvas_media_t11.xlsx"),
  curvas_media_t20 = read_excel("curvas_media_t20.xlsx"),
  curvas_media_t53 = read_excel("curvas_media_t53.xlsx"),
  curvas_media_t71 = read_excel("curvas_media_t71.xlsx"),
  curvas_media_t87 = read_excel("curvas_media_t87.xlsx"),
  curvas_media_t100c = read_excel("curvas_media_t100c.xlsx")
  )

# Lista de concentrações para ajustar
concentracoes <- c(0, 0.1, 0.2, 0.3, 0.4)

# Aplicar o ajuste, calcular preditos e plotar para cada dataset
for (nome_dataset in names(datasets)) {
  # Lista para armazenar os resultados de todas as concentrações para o dataset atual
  resultados_dataset <- list()
  dados_preditos <- data.frame()
  
  for (concentracao in concentracoes) {
    cat("Processando:", nome_dataset, "- Concentração:", concentracao, "\n")
    
    # Ajustar o modelo
    resultados <- ajustar_gompertz(datasets[[nome_dataset]], concentracao)
    
    # Armazenar os resultados na lista
    resultados_dataset[[paste0("Concentracao_", concentracao)]] <- resultados
    
    # Calcular os valores preditos
    dados_concentracao <- filter(datasets[[nome_dataset]], Concentracao == concentracao)
    dados_preditos_concentracao <- calcular_preditos(dados_concentracao, resultados)
    
    # Combinar os dados preditos
    dados_preditos <- bind_rows(dados_preditos, dados_preditos_concentracao)
    
    # Exibir os primeiros resultados
    print(head(resultados))
    cat("\n")
  }
  
  # Exportar os resultados para uma planilha Excel
  nome_arquivo <- paste0("Resultados_", nome_dataset, ".xlsx")
  write_xlsx(resultados_dataset, nome_arquivo)
  cat("Planilha salva:", nome_arquivo, "\n\n")
  
  # Plotar os dados observados e preditos
  plotar_dados(dados_preditos, nome_dataset)
}

# Exibir mensagem de conclusão
cat("Todos os datasets e concentrações foram processados.\n")



#----------------- Fit Ancestral -----------------------------------

# ============================
# Carregar pacotes necessários
# ============================
library(dplyr)
library(minpack.lm)
library(ggplot2)
library(writexl)
library(readxl)

# ==================================
# Função para ajustar o modelo Gompertz
# ==================================
ajustar_gompertz <- function(dados, concentracao) {
  mmax <- c()
  mmax_sd <- c()
  C <- c()
  M <- c()
  M_sd <- c()
  Y0 <- c()

  # Filtrar dados para a concentração
  tabfit <- filter(dados, Concentracao == concentracao)
  d <- tabfit$Time
  y <- tabfit$Media_DO
  y0 <- min(y)

  # Ajustar o modelo Gompertz
  fit <- tryCatch({
    nlsLM(y ~ y0 + C * exp(-exp(-mmax * (d - M))),
          start = list(mmax = 0.00001, C = (max(y) - min(y)), M = 200))
  }, error = function(e) {
    return(NULL) # Retorna NULL se o ajuste falhar
  })

  if (!is.null(fit)) {
    Y0 <- append(Y0, y0)
    mmax <- append(mmax, summary(fit)$parameters[1, 1])
    mmax_sd <- append(mmax_sd, summary(fit)$parameters[1, 2])
    C <- append(C, summary(fit)$parameters[2, 1])
    M <- append(M, summary(fit)$parameters[3, 1])
    M_sd <- append(M_sd, summary(fit)$parameters[3, 2])
  } else {
    Y0 <- append(Y0, NA)
    mmax <- append(mmax, NA)
    mmax_sd <- append(mmax_sd, NA)
    C <- append(C, NA)
    M <- append(M, NA)
    M_sd <- append(M_sd, NA)
  }

  # Retornar parâmetros ajustados
  return(data.frame(
    Concentracao = concentracao,
    Y0 = Y0,
    mmax = mmax,
    mmax_sd = mmax_sd,
    C = C,
    M = M,
    M_sd = M_sd
  ))
}

# ========================================
# Função para calcular os valores preditos
# ========================================
calcular_preditos <- function(dados, resultados) {
  dados <- dados %>%
    mutate(Concentracao = as.numeric(Concentracao))

  dados_preditos <- dados %>%
    left_join(resultados, by = "Concentracao") %>%
    mutate(
      y_fit = Y0 + C * exp(-exp(-mmax * (Time - M)))
    )

  return(dados_preditos)
}

# ==========================================
# Função para plotar os dados observados e preditos
# ==========================================
plotar_ajuste <- function(dados_preditos, nome_dataset) {
  graf <- ggplot(dados_preditos, aes(x = Time)) +
    geom_point(aes(y = Media_DO, color = "Observado"), size = 1) +
    geom_line(aes(y = y_fit, color = "Predito")) +
    geom_errorbar(aes(ymin = Media_DO - DesvioPadrao_DO, 
                      ymax = Media_DO + DesvioPadrao_DO), 
                  width = 0.3, alpha = 0.5) +
    facet_wrap(~ Concentracao, nrow = 1) +
    theme_bw() +
    labs(
      x = 'Tempo (min)',
      y = 'Densidade Óptica',
      title = paste("Ajuste do Modelo Gompertz -", nome_dataset),
      color = "Legenda"
    ) +
    scale_color_manual(values = c("Observado" = "black", "Predito" = "red"))

  # Salvar gráfico
  nome_arquivo <- paste0("Grafico_", nome_dataset, ".png")
  ggsave(nome_arquivo, graf, device = 'png', units = 'cm', width = 37, height = 7, dpi = 600)
  
  cat("Gráfico salvo:", nome_arquivo, "\n")
}

# =======================================
# Carregar o dataset do ancestral
# =======================================
ancestral <- read_excel("curvas_media_ancestral.xlsx")  # Substitua pelo caminho correto do arquivo

# Certifique-se de que as colunas estão no formato correto
ancestral <- ancestral %>%
  mutate(
    Time = as.numeric(Time),
    Concentracao = as.numeric(Concentracao),
    Media_DO = as.numeric(Media_DO),
    DesvioPadrao_DO = as.numeric(DesvioPadrao_DO)
  )

# Filtrar concentrações até 0.4 (como especificado)
ancestral <- ancestral %>%
  filter(Concentracao <= 0.4)

# =======================================
# Ajustar o modelo Gompertz para cada concentração
# =======================================
concentracoes <- c(0, 0.1, 0.2, 0.3, 0.4)

resultados_ancestral <- list()
dados_preditos_ancestral <- data.frame()

for (concentracao in concentracoes) {
  cat("\n-----------------------------------\n")
  cat("Processando concentração:", concentracao, "\n")
  
  # Ajuste do modelo
  resultados <- ajustar_gompertz(ancestral, concentracao)
  
  # Armazenar os resultados em lista
  resultados_ancestral[[paste0("Concentracao_", concentracao)]] <- resultados
  
  # Calcular preditos
  dados_concentracao <- filter(ancestral, Concentracao == concentracao)
  dados_preditos_concentracao <- calcular_preditos(dados_concentracao, resultados)
  
  # Combinar todos os dados preditos
  dados_preditos_ancestral <- bind_rows(dados_preditos_ancestral, dados_preditos_concentracao)
  
  # Exibir resultados principais
  print(head(resultados))
}

# =======================================
# Exportar resultados para Excel
# =======================================
nome_arquivo <- "fit_ancestral.xlsx"
write_xlsx(resultados_ancestral, nome_arquivo)
cat("\nResultados salvos em:", nome_arquivo, "\n")

# =======================================
# Gerar gráficos
# =======================================
plotar_ajuste(dados_preditos_ancestral, "ancestral")

cat("\nProcesso concluído com sucesso!\n")
