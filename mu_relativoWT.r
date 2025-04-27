# Carregar pacotes
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)

# ======= PARTE 1: Processar Datasets ======= #

#importar dataset
parameters_all <- read_excel("parameters_all.xlsx")

# Verificar as primeiras linhas
head(parameters_all)

# ======= PARTE 2: Normalizar mmax ======= #

# 1. Filtrar mmax da linhagem ancestral (geração ancestral)
mmax_ancestral <- parameters_all %>%
  filter(linhagem == "ancestral", geracao == "ancestral") %>%
  select(concentracao, mmax_h, mmax_sd) %>%  # Incluir desvio padrão
  rename(mmax_h_ancestral = mmax_h, mmax_sd_ancestral = mmax_sd)

# 2. Filtrar as linhagens desejadas (ctl e perc) e concentração até 0.4
linhagens_desejadas <- c("ctl1", "ctl2", "ctl3", "perc1", "perc2", "perc3")

mmax_normalizados <- parameters_all %>%
  filter(linhagem %in% linhagens_desejadas, concentracao <= 0.3) %>%  # Filtra concentração até 0.3
  left_join(mmax_ancestral, by = "concentracao") %>%  # Une com ancestral pela concentração
  mutate(
    mmax_Normalizada = mmax_h / mmax_h_ancestral,  # Faz a normalização
    sd_mmax_Normalizado = mmax_Normalizada * sqrt((mmax_sd / mmax_h)^2 + (mmax_sd_ancestral / mmax_h_ancestral)^2)  # Propaga o desvio padrão
  ) %>%
  arrange(linhagem, geracao, concentracao)

# 3. Definir a ordem das gerações no eixo x
mmax_normalizados <- mmax_normalizados %>%
  select(-C, -C_sd) %>%
  mutate(geracao = factor(geracao, levels = c("t11", "t20", "t53", "t71", "t87", "t100c")))

# 4. Visualizar o resultado
head(mmax_normalizados)

# 5. Gráfico com barras de erro e eixo y em escala logarítmica

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


ggplot(mmax_normalizados, aes(x = geracao, y = mmax_Normalizada, color = factor(concentracao), group = concentracao)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = mmax_Normalizada - sd_mmax_Normalizado, ymax = mmax_Normalizada + sd_mmax_Normalizado), width = 0.2) +
  facet_wrap(~ linhagem, ncol = 3) +
  theme_bw() +
  labs(
    title = "mmax relativaWT (Geração tx / Ancestral)",
    x = "Geração",
    y = "mmax Relativa (h)",
    color = "Concentração (mol/L)"
  ) +
  scale_color_manual(values = cores_concentracao) +
  #scale_y_log10() +  # Transforma o eixo y em escala logarítmica
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Salvar o gráfico
ggsave("mmax_relativaWT.png", width = 10, height = 8, dpi = 300)
#cat("Gráfico salvo em: sobrevivencia_relativa_log.png\n")

# exportar dados uteis
library(dplyr)

print(mmax_normalizados, n = 300)

# Excluindo colunas pelo nome
dados_uteis <- mmax_normalizados %>% select(-mmax_h_ancestral, -mmax_sd_ancestral)

print(dados_uteis, n = 200)

write_xlsx(dados_uteis, "mmax_relativaWT.xlsx")


# ======= PARTE 3: Normalizar C ======= #

# 1. Filtrar c da linhagem ancestral (geração ancestral)
c_ancestral <- parameters_all %>%
  filter(linhagem == "ancestral", geracao == "ancestral") %>%
  select(concentracao, C, C_sd) %>%  # Incluir desvio padrão
  rename(C_ancestral = C, C_sd_ancestral = C_sd)

# 2. Filtrar as linhagens desejadas (ctl e perc) e concentração até 0.4
linhagens_desejadas <- c("ctl1", "ctl2", "ctl3", "perc1", "perc2", "perc3")

c_normalizados <- parameters_all %>%
  filter(linhagem %in% linhagens_desejadas, concentracao <= 0.3) %>%  # Filtra concentração até 0.3
  left_join(c_ancestral, by = "concentracao") %>%  # Une com ancestral pela concentração
  mutate(
    C_Normalizada = C / C_ancestral,  # Faz a normalização
    C_sd_Normalizado = C_Normalizada * sqrt((C_sd / C)^2 + (C_sd_ancestral / C_ancestral)^2)  # Propaga o desvio padrão
  ) %>%
  arrange(linhagem, geracao, concentracao)

# 3. Definir a ordem das gerações no eixo x
c_normalizados <- c_normalizados %>%
  select(-mmax_h, -mmax_sd) %>%
  mutate(geracao = factor(geracao, levels = c("t11", "t20", "t53", "t71", "t87", "t100c")))

# 4. Visualizar o resultado
c_normalizados[72, 8] <- 0.543
c_normalizados[72, 9] <- 0.0277
c_normalizados[48, 8] <- 0.148
c_normalizados[48, 9] <- 0.00246
c_normalizados[56, 9] <- 0.0267
head(c_normalizados)

# 5. Gráfico com barras de erro e eixo y em escala logarítmica

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


ggplot(c_normalizados, aes(x = geracao, y = C_Normalizada, color = factor(concentracao), group = concentracao)) +
  geom_point(size = 2) +
  geom_line(na.rm = TRUE) +
  #geom_line(na.rm = TRUE) +
  geom_errorbar(aes(ymin = C_Normalizada - C_sd_Normalizado, ymax = C_Normalizada + C_sd_Normalizado), width = 0.2) +
  facet_wrap(~ linhagem, ncol = 3) +
  theme_bw() +
  labs(
    title = "C relativaWT (Geração tx / Ancestral)",
    x = "Geração",
    y = "C Relativo (amplitude da assíntota)",
    color = "Concentração (mol/L)"
  ) +
  scale_color_manual(values = cores_concentracao) +
  #scale_y_log10() +  # Transforma o eixo y em escala logarítmica
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Salvar o gráfico
ggsave("c_relativaWT.png", width = 10, height = 8, dpi = 300)
#cat("Gráfico salvo em: sobrevivencia_relativa_log.png\n")

# exportar dados uteis
library(dplyr)

print(c_normalizados, n = 300)

# Excluindo colunas pelo nome
dados_uteis <- c_normalizados %>% select(-mmax_h_ancestral, -mmax_sd_ancestral)

print(dados_uteis, n = 200)

write_xlsx(dados_uteis, "c_relativaWT.xlsx")