# ============================================
#  1. Carregar Pacotes
# ============================================
if(!require(readxl)) install.packages("readxl")  # Para ler arquivos Excel
if(!require(ggplot2)) install.packages("ggplot2")  # Para criar o heatmap
if(!require(tidyr)) install.packages("tidyr")  # Para transformar os dados
if(!require(dplyr)) install.packages("dplyr")  # Para manipulação de dados

library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(openxlsx)

# ============================================
#  2. Preparação dos Dados (se precisar)
# ============================================
# Transformar os dados para o formato longo
 dados_long <- dados %>%
   pivot_longer(cols = c(ctl1, ctl2, ctl3, perc1, perc2, perc3), 
                names_to = "amostra", 
                values_to = "mutacao")
   #filter(!is.na(mutacao))  # Remover valores NA

 # Mapear as letras para os tipos de mutação
 dados_long <- dados_long %>%
   mutate(
     tipo_mutacao = case_when(
       mutacao == "a" ~ "del/frameshift",
       mutacao == "b" ~ "in/frameshift",
       mutacao == "c" ~ "del",
       mutacao == "d" ~ "missense",
       mutacao == "NA" ~ "vazio"
     )
   )

 # Visualizar os dados transformados
 head(dados_long)

 arquivo_saida <- "dados_long.xlsx"

 # Exportar o dataframe para Excel
 write.xlsx(dados_long, file = arquivo_saida)

# ============================================
#  3. Ler os Dados da Aba "heat"
# ============================================

dados_long <- read_excel("dados_long.xlsx")
head(dados_long)
print(dados_long, n = 400)

# ============================================
#  4. Criar o Heatmap
# ============================================


heatmap_plot <- ggplot(dados_long, aes(x = amostra, y = `gene/produto`, fill = tipo_mutacao)) +
  geom_tile(color = "black", width = 1,  height = 1) +  # Criar o heatmap
  geom_text(aes(label = ifelse(is.na(num), "", num)), color = "white", size = 2) +  # Adicionar números nos quadrados
  scale_fill_manual(values = c(
    "del/frameshift" = "#9C8A7F",  # Marrom Suave
    "in/frameshift" = "#E0C9A6",  # Bege Claro
    "del" = "#6E6E6E",  # Cinza Médio
    "missense" = "#B22222", # Vermelho Escuro
    "NA" = "#FFFFFF") # Definir branco para valores ausentes
    ) +
  theme_minimal() +  # Tema minimalista
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotacionar rótulos do eixo x
    axis.title = element_blank(),  # Remover títulos dos eixos
    legend.position = "right",  # Posição da legenda
    legend.text = element_text(size = 12),  # Tamanho da fonte da legenda
    legend.title = element_text(size = 14),  # Tamanho da fonte do título da legenda
    #aspect.ratio = nrow(dados) / length(unique(dados_long$amostra))  # Ajustar proporção para tiles quadrados
  ) +
  labs(
    fill = "Tipo de Mutação"  # Título da legenda
  ) #+
  #coord_fixed(ratio = 0.25)  # Garantir que os tiles sejam quadrados

# ============================================
#  5. Exibir o Heatmap
# ============================================
print(heatmap_plot)

# ============================================
#  6. Salvar o Heatmap
# ============================================
ggsave("heatmap_mutacoes.png")




