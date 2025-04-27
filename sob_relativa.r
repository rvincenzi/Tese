# Carregar pacotes
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)

# ======= PARTE 1: Processar Datasets ======= #

# Lista de gerações
geracoes <- c("media_sob_ancestral", "media_sob_t11", "media_sob_t20", "media_sob_t53", "media_sob_t71", "media_sob_t87", "media_sob_t100c")

# Função para carregar e estruturar os dados
carregar_geracao <- function(geracao) {
  dados <- read_excel(paste0(geracao, ".xlsx")) %>%
    mutate(Geracao = gsub("media_sob_", "", geracao))  # Ajusta o nome da geração
  return(dados)
}

# Carregar e unir todas as gerações
dados_todos <- bind_rows(lapply(geracoes, carregar_geracao))

# Verificar todas as sobrevivências ancestrais por linhagem e concentração
dados_todos %>%
  filter(Geracao == "ancestral") %>%
  select(linhagem, Concentracao, Media_sob) %>%
  arrange(linhagem, Concentracao)


# Verificar as primeiras linhas
head(dados_todos)

write_xlsx(dados_todos, "sob_relativa.xlsx")
cat("Dados processados salvos em: dados_todos.xlsx\n")

# ======= PARTE 2: Normalizar Sobrevivência ======= #
# Cores padrão para concentrações (suavizadas)
cores_concentracao <- c(
   "0"  = "black", # Cinza médio (suavizado)
  "0.1" = "#A8A8A8",  # Cinza médio (mais escuro que #D3D3D3)
  "0.2" = "#A88E82",  # Marrom café suavizado
  "0.3" = "#C55A5A",  # Vermelho fosco suavizado (mais avermelhado)
  "0.4" = "#C7A98C",  # Bege queimado suavizado
  "0.5" = "#A56E6E",  # Vinho fosco suavizado
  "0.6" = "#B5947A",  # Marrom noz suavizado
  "0.7" = "#E0966F",  # Vermelho terracota suavizado (mais alaranjado)
  "0.8" = "#C5B8A8",  # Bege acinzentado suavizado
  "0.9" = "#9C8A7F",  # Chocolate escuro suavizado
  "1.0" = "#8A8580"   # Cinza carvão suavizado
  )

# ======= PARTE 2: Normalizar Sobrevivência ======= #

# 1. Filtrar sobrevivência da linhagem ancestral (geração ancestral)
sob_ancestral <- dados_todos %>%
  filter(linhagem == "ancestral", Geracao == "ancestral") %>%
  select(Concentracao, Media_sob, DesvioPadrao_sob) %>%  # Incluir desvio padrão
  rename(Media_ancestral = Media_sob, Desvio_ancestral = DesvioPadrao_sob)

# 2. Filtrar as linhagens desejadas (ctl e perc) e concentração até 0.4
linhagens_desejadas <- c("ctl1", "ctl2", "ctl3", "perc1", "perc2", "perc3")

dados_normalizados <- dados_todos %>%
  filter(linhagem %in% linhagens_desejadas, Concentracao <= 0.4) %>%  # Filtra concentração até 0.4
  left_join(sob_ancestral, by = "Concentracao") %>%  # Une com ancestral pela concentração
  mutate(
    Sob_Normalizada = Media_sob / Media_ancestral,  # Faz a normalização
    Desvio_Normalizado = Sob_Normalizada * sqrt((DesvioPadrao_sob / Media_sob)^2 + (Desvio_ancestral / Media_ancestral)^2)  # Propaga o desvio padrão
  ) %>%
  arrange(linhagem, Geracao, Concentracao)

# 3. Definir a ordem das gerações no eixo x
dados_normalizados <- dados_normalizados %>%
  mutate(Geracao = factor(Geracao, levels = c("t11", "t20", "t53", "t71", "t87", "t100c")))

# 4. Visualizar o resultado
head(dados_normalizados)

# Suponha que você tenha o tamanho da amostra (n) para cada linhagem e concentração
# Se não tiver, você precisará obtê-lo dos dados brutos ou assumir um valor.
n <- 3  # Exemplo: assumindo que cada média foi calculada a partir de 3 réplicas

# 2. Calcular o erro padrão
dados_todos <- dados_normalizados %>%
  mutate(
    ErroPadrao_sob = DesvioPadrao_sob / sqrt(n),  # Erro padrão da sobrevivência da linhagem
    Erro_ancestral = Desvio_ancestral / sqrt(n)   # Erro padrão da sobrevivência ancestral
  )

# 3. Filtrar as linhagens desejadas (ctl e perc) e concentração até 0.4
linhagens_desejadas <- c("ctl1", "ctl2", "ctl3", "perc1", "perc2", "perc3")

dados_final <- dados_todos %>%
  filter(linhagem %in% linhagens_desejadas, Concentracao <= 0.3) %>%  # Filtra concentração até 0.4
  #left_join(sob_ancestral, by = "Concentracao") %>%  # Une com ancestral pela concentração
  mutate(
    Sob_Normalizada = Media_sob / Media_ancestral,  # Faz a normalização
    erro_Normalizado = Sob_Normalizada * sqrt((ErroPadrao_sob / Media_sob)^2 + (Erro_ancestral / Media_ancestral)^2)  # Propaga o erro padrão
  ) %>%
  arrange(linhagem, Geracao, Concentracao)

# 4. Definir a ordem das gerações no eixo x
dados_final <- dados_final %>%
  mutate(Geracao = factor(Geracao, levels = c("t11", "t20", "t53", "t71", "t87", "t100c")))

# 5. Visualizar o resultado
head(dados_final)


# 5. Gráfico com barras de erro e eixo y em escala logarítmica
ggplot(dados_final, aes(x = Geracao, y = Sob_Normalizada, color = factor(Concentracao), group = Concentracao)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = Sob_Normalizada - erro_Normalizado, ymax = Sob_Normalizada + erro_Normalizado), width = 0.2) +
  facet_wrap(~ linhagem, ncol = 3) +
  theme_bw() +
  labs(
    title = "Sobrevivência Relativa (Geração tx / Ancestral)",
    x = "Geração",
    y = "Sobrevivência Relativa (log10)",
    color = "Concentração (mol/L)"
  ) +
  scale_color_manual(values = cores_concentracao) +
  scale_y_log10() +  # Transforma o eixo y em escala logarítmica
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Salvar o gráfico
ggsave("sobrevivencia_relativaWT.png", width = 10, height = 8, dpi = 300)
cat("Gráfico salvo em: sobrevivencia_relativa_log.png\n")
print(dados_final, n=200)
# exportar dados uteis
library(dplyr)

# Excluindo colunas pelo nome
dados_uteis <- dados_final %>% select(-Media_ancestral, -Desvio_ancestral, -Erro_ancestral)
print(dados_uteis, n = 200)

write_xlsx(dados_uteis, "sob_relativaWT.xlsx")
