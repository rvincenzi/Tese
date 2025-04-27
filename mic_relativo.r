# Carregar pacotes
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)

# ======= PARTE 1: Processar Datasets ======= #

mic_all <- read_excel("mic_all.xlsx")

# Verificar as primeiras linhas
head(mic_all)


# ======= PARTE 2: Normalizar mic ======= #

# 1. Filtrar sobrevivência da linhagem ancestral (geração ancestral)
mic_ancestral <- mic_all %>%
  filter(linhagem == "ancestral", Geracao == "ancestral") %>%
  select(linhagem, LD10, Erro_Padrao) %>%  # Incluir desvio padrão
  rename(LD10_ancestral = LD10, Erro_Padrao_ancestral = Erro_Padrao)

# 2. Filtrar as linhagens desejadas (ctl e perc) e concentração até 0.4
linhagens_desejadas <- c("ctl1", "ctl2", "ctl3", "perc1", "perc2", "perc3")

dados_normalizados <- mic_all %>%
  filter(linhagem %in% linhagens_desejadas) %>%
  mutate(
    # Adiciona os valores do ancestral
    LD10_ancestral = mic_ancestral$LD10_ancestral,  # Replicando os valores do ancestral para todas as linhas
    Erro_Padrao_ancestral = mic_ancestral$Erro_Padrao_ancestral  # Replicando o erro do ancestral
  ) %>%
  mutate(
    mic_Normalizada = LD10 / LD10_ancestral,  # Normalização do mic
    Erro_Normalizado = mic_Normalizada * sqrt(
      (Erro_Padrao / LD10)^2 + (Erro_Padrao_ancestral / LD10_ancestral)^2  # Propaga o erro padrão
    )
  ) %>%
  arrange(linhagem, Geracao)  # Organiza os dados por linhagem, geração e concentração

# 3. Definir a ordem das gerações no eixo x
dados_normalizados <- dados_normalizados %>%
  mutate(Geracao = factor(Geracao, levels = c("t11", "t20", "t53", "t71", "t87", "t100c")))

# 4. Visualizar o resultado
head(dados_normalizados)


# 5. Gráfico com barras de erro e eixo y em escala logarítmica

# ======= PARTE 2: Normalizar Sobrevivência ======= #
# Cores padrão para concentrações (suavizadas)
cores_concentracao <- c(
  "ctl1"  = "black", # Cinza médio (suavizado)
  "ctl2" = "#A8A8A8",  # Cinza médio (mais escuro que #D3D3D3)
  "ctl3" = "#A88E82",  # Marrom café suavizado
  "perc1" = "#C55A5A",  # Vermelho fosco suavizado (mais avermelhado)
  "perc2" = "#E0966F",  # Vermelho terracota suavizado (mais alaranjado)"
  "perc3" = "#A56E6E"  # Vinho fosco suavizado
  )

ggplot(dados_normalizados, aes(x = Geracao, y = mic_Normalizada, group = linhagem, color = (linhagem))) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = mic_Normalizada - Erro_Normalizado, ymax = mic_Normalizada + Erro_Normalizado), width = 0.2) +
  #facet_wrap(~ linhagem, ncol = 3) +
  theme_bw() +
  labs(
    title = "Fitness Relativo (Geração tx / Ancestral)",
    x = "Geração",
    y = "Fitness (MIC) Relativo",
    color = "Linhagem"
  ) +
  scale_color_manual(values = cores_concentracao) +
  #scale_y_log10() +  # Transforma o eixo y em escala logarítmica
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Salvar o gráfico
ggsave("mic_relativaWT.png", width = 10, height = 8, dpi = 300)

# exportar dados uteis
library(dplyr)

# Excluindo colunas pelo nome
dados_uteis <- dados_normalizados %>% select(-LD10_ancestral, -Erro_Padrao_ancestral)
print(dados_uteis, n = 200)

write_xlsx(dados_uteis, "mic_relativoWT.xlsx")
