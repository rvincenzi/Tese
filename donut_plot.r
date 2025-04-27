# ============================================
#  1. Leitura do Arquivo
# ============================================
if(!require(readxl)) install.packages("readxl")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")

library(readxl)
library(ggplot2)
library(dplyr)

mutações <- read_excel("general_sum.xlsx", sheet = "donut")
head(mutações)

# ============================================
#  2. Preparação dos Dados
# ============================================
# Filtrar categorias com number > 0
mutações_filtradas <- mutações %>%
  filter(number > 0)

# Recalcular as proporções para cada linhagem
mutações_ajustadas <- mutações_filtradas %>%
  group_by(linhagem) %>%
  mutate(proporção = number / sum(number)) %>%
  ungroup()

# Somando mutações por linhagem (após ajuste)
total_mutações <- mutações_ajustadas %>%
  group_by(linhagem) %>%
  summarise(total = sum(number))

# Juntando totais ao dataset ajustado
mutações_ajustadas <- mutações_ajustadas %>%
  left_join(total_mutações, by = "linhagem")

# Criar uma coluna para posicionar os rótulos
mutações_ajustadas <- mutações_ajustadas %>%
  group_by(linhagem) %>%
  arrange(desc(mutation)) %>%  # Ordenar para garantir a ordem correta
  mutate(posição = cumsum(proporção) - 0.5 * proporção) %>%
  ungroup()

# ============================================
#  3. Gráfico Donut (Facetado por Grupo)
# ============================================
donut_plot <- ggplot(mutações_ajustadas, aes(x = 2, y = proporção, fill = mutation)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +  # Transformando em gráfico circular
  facet_wrap(~linhagem, ncol = 3) +  # Facetado por grupo
  geom_segment(aes(x = 2.2, xend = 3.0, y = posição, yend = posição), color = "black", linewidth = 0.3) +  # Linhas de conexão
  geom_text(aes(x = 3.6, y = posição, label = number), size = 5, color = "black", fontface = "bold") +  # Números nas fatias
  geom_text(aes(x = 0, y = 0, label = total), size = 5, fontface = "bold") +  # Total no centro
  theme_void() +  # Remove eixos
  scale_fill_manual(values = c(
    "SNPs silence" = "#6E6E6E",  # Cinza Médio
    "SNPs missense" = "#B22222",  # Vermelho Escuro
    "Deleção/Frameshift" = "#9C8A7F",  # Marrom Suave
    "Inserção/Frameshift" = "#E0C9A6",  # Bege Claro
    "Deleção" = "#A56E6E"  # Rosa Terroso
  )) +
  theme(
    legend.position = "right",  # Legenda à direita
    legend.text = element_text(size = 12),  # Tamanho da fonte da legenda
    legend.title = element_text(size = 14),
    strip.text = element_text(size = 12, face = "bold"),  # Estilo do texto das facetas
    plot.title = element_text(size = 16, face = "bold")  # Estilo do título
  ) +
  labs(
    fill = "Tipo de Mutação",  # Título da legenda
    #title = "Número de Mutações por Grupo"  # Título do gráfico
  )

# ============================================
#  4. Exibindo o Gráfico
# ============================================
print(donut_plot)

# ============================================
#  5. Salvando o Gráfico
# ============================================
ggsave("donut_mutacoes.png", plot = donut_plot, width = 12, height = 7, dpi = 500)