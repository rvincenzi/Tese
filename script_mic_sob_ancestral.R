#-------MANIPULAÇÃO DAS TABELAS DO PLATE READER E PLOTS-----------
#------------AM1E perclorato de magnésio (Mg(ClO4)2-----------------------

# Carregar pacotes necessários
install.packages("readxl")
library(readxl)
library(dplyr)
install.packages("openxlsx")
library(openxlsx)
library(ggplot2)
# Carregar os dados brutos
raw_data <- read_excel("/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento_mic/mic_ancestral/mic_ancestral_12Jul24.xlsx", sheet = "raw_33")

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

# Verificar o resultado
head(dados_corrigidos)

write.xlsx(dados_corrigidos, file = "/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento_mic/mic_ancestral/dados_corrigidos.xlsx", rowNames = FALSE)

#-----GRÁFICO FACETADO REPLICATAS---------------

library(ggplot2)
library(tidyr)
library(dplyr)

#-----------TABELA PARA PLOT-------------
#time como numérico
dados_corrigidos$Time <- as.numeric(dados_corrigidos$Time)

# Transformar os dados em formato longo, agora focando nas colunas B2 a G12
dados_long <- dados_corrigidos %>%
  select(Time, B2:G12) %>%  # Seleciona explicitamente as colunas B2 a G12 junto com o tempo
  pivot_longer(
    cols = B2:G12,  # Seleciona as colunas de interesse (B2 a G12)
    names_to = c("Replicata", "Concentracao"),
     names_pattern = "([B-G])(2|3|4|5|6|7|8|9|10|11|12)",  # Define explicitamente os números de 2 a 12
    values_to = "DO"
  ) %>%
  mutate(
    DO = abs(DO),  # Transformar valores de DO em módulo
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
    )
  )# Verificar o resultado
head(dados_long)
write.xlsx(dados_long, file = "/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento_mic/mic_ancestral/dados_long.xlsx", rowNames = FALSE)



# Criar o gráfico com eixo y em escala logarítmica
plot_replicatas_perc <- ggplot(dados_long, aes(x = Time, y = DO, color = Concentracao, group = Concentracao)) +
  geom_point(size=1) +
  facet_wrap(~ Replicata) +
  scale_y_log10() +  # Aplicar transformação logarítmica ao eixo y
  labs(
    title = "Curva de Crescimento por Replicata e Concentração de Perclorato",
    x = "Tempo",
    y = "Densidade Óptica (DO - Escala Log)"
  ) +
  theme_minimal()

plot_replicatas_perc

ggsave("/Users/isabe/Downloads/nepalensis/Curva_perc_DO/am1E_12_7/plot_replicatas_perc.png", plot = last_plot(), width = 8, height = 6, dpi = 300, bg = "white")


#--------GRÁFICO DAS MÉDIAS----------------------

#TABELA DAS MÉDIAS
library(dplyr)
library(readxl)
library(openxlsx)


dados_long


# Calcular a média e o desvio padrão das replicatas por tempo e por concentração
dados_medias <- dados_long %>%
  group_by(Time, Concentracao) %>%
  summarise(
    Media_DO = mean(DO, na.rm = TRUE),
    DesvioPadrao_DO = sd(DO, na.rm = TRUE)
  )

# Verificar o resultado
print(head(dados_medias))

# Salvar os dados de média e desvio padrão em um novo arquivo Excel
write.xlsx(dados_medias, file = "/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento_mic/mic_ancestral/curvas_media_ancestral.xlsx", rowNames = FALSE)


#PLOT MEDIAS

library(ggplot2)
library(dplyr)
library(readxl)
library(openxlsx)

# Carregar os dados de média e desvio padrão do arquivo Excel
dados_medias <- read_excel("/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento_mic/mic_ancestral/curvas_media_ancestral.xlsx")

# Garantir que os valores de DO estejam em módulo
dados_medias <- dados_medias %>%
  mutate(
    Media_DO = abs(Media_DO), 
    DesvioPadrao_DO = abs(DesvioPadrao_DO)
  )

# Cores padrão para concentrações (suavizadas)
cores_concentracao <- c(
   "0"  = "#6E6E6E", # Cinza médio (suavizado)
  "0.1" = "#A8A8A8",  # Cinza médio (mais escuro que #D3D3D3)
  "0.2" = "#A88E82",  # Marrom café suavizado
  "0.3" = "#C55A5A",  # Vermelho fosco suavizado (mais avermelhado)
  "0.4" = "#A56E6E",  # Vinho fosco suavizado
  "0.5" = "#C7A98C",  # Bege queimado suavizado
  "0.6" = "#B5947A",  # Marrom noz suavizado
  "0.7" = "#E0966F",  # Vermelho terracota suavizado (mais alaranjado)
  "0.8" = "#C5B8A8",  # Bege acinzentado suavizado
  "0.9" = "#9C8A7F",  # Chocolate escuro suavizado
  "1.0" = "#8A8580"   # Cinza carvão suavizado
)
# Limites dos eixos
limite_x <- c(0, 2880)  # Tempo de 0 a 2880 minutos
limite_y <- c(0.001, 2) # DO de 0.001 a 2

# Tema padrão com contorno quadrado
tema_padrao <- theme_bw() +  # Usar theme_bw() como base para o contorno quadrado
  theme(
    text = element_text(size = 12),  # Tamanho da fonte
    axis.text = element_text(color = "black"),  # Cor do texto dos eixos
    legend.position = "right",  # Posição da legenda (esquerda)
    #panel.border = element_rect(color = "black", fill = NA, size = 0.8),  # Contorno quadrado
    panel.grid.major = element_line(color = "gray80", size = 0.5),  # Grade principal
    panel.grid.minor = element_line(color = "gray90", size = 0.2)   # Grade secundária
  )  
  


# Criar o gráfico com eixo y em escala logarítmica
plot_medias <- ggplot(dados_medias, aes(x = Time, y = Media_DO, color = Concentracao, group = Concentracao)) +
  geom_point() +
  geom_errorbar(aes(ymin = Media_DO - DesvioPadrao_DO, ymax = Media_DO + DesvioPadrao_DO), width = 0.2) +
  scale_y_log10(limits = limite_y) +
  scale_x_continuous(limits = limite_x) +
  scale_color_manual(values = cores_concentracao) +  
  labs(
    title = "Curva de Crescimento Média por Concentração de Mg(ClO4)2",
    x = "Tempo",
    y = "Densidade Óptica (DO - Escala Log)"
  ) +
  theme_minimal()

plot_medias

plot_medias <- ggsave("/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento_mic/mic_ancestral/plot_medias_ancestral.png", plot = last_plot(), width = 8, height = 6, dpi = 300, bg = "white")


# Carregar pacotes necessários
library(dplyr)
library(tidyr)

# Carregar pacotes necessários
library(dplyr)

# Supondo que dados_long já esteja carregado
# Se não estiver, importe os dados conforme explicado anteriormente

# Filtrar os dados para o tempo 2000
dados_tempo_2000 <- dados_long %>%
  filter(Time == 2000)

# Calcular a sobrevivência para cada réplica no tempo 2000
dados_sobrevivencia_2000 <- dados_tempo_2000 %>%
  group_by(Replicata) %>%  # Agrupar por réplica
  mutate(
    DO_controle = DO[Concentracao == "0"],  # Obter o DO do controle (concentração 0)
    Sobrevivencia = DO / DO_controle  # Calcular a sobrevivência
  ) %>%
  ungroup()  # Remover o agrupamento

# Verificar o resultado
head(dados_sobrevivencia_2000)

# Salvar os dados de sobrevivência no tempo 2000 em um novo arquivo Excel
write.xlsx(dados_sobrevivencia_2000, file = "/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento_mic/mic_ancestral/dados_sobrevivencia_2000.xlsx", rowNames = FALSE)


# Carregar pacotes necessários
library(dplyr)

# Supondo que dados_sobrevivencia_2000 já esteja carregado
# Se não estiver, carregue os dados conforme explicado anteriormente

# Calcular a média e o desvio padrão da sobrevivência por concentração
dados_media_desvio <- dados_sobrevivencia_2000 %>%
  group_by(Concentracao) %>%  # Agrupar por concentração
  summarise(
    Media_Sobrevivencia = mean(Sobrevivencia, na.rm = TRUE),  # Média da sobrevivência
    DesvioPadrao_Sobrevivencia = sd(Sobrevivencia, na.rm = TRUE)  # Desvio padrão da sobrevivência
  )

# Verificar o resultado
print(dados_media_desvio)

# Salvar os dados de média e desvio padrão em um novo arquivo Excel
write.xlsx(dados_media_desvio, file = "/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento_mic/mic_ancestral/dados_media_sob_2000.xlsx", rowNames = FALSE)


sob_ancestral<- ggplot(dados_media_desvio, aes(x=Concentracao, y=Media_Sobrevivencia, group = 1)) +
    geom_point(size=1.5)+
    geom_line(linewidth=1)+
    geom_hline(yintercept = 0.10, linetype = "dashed", color = "red") +
    geom_errorbar(aes(ymin=Media_Sobrevivencia-DesvioPadrao_Sobrevivencia, ymax=Media_Sobrevivencia+DesvioPadrao_Sobrevivencia), width=0.05, size=.5, color="#CC0000")+
    theme_bw()+
    labs(x='Concentração (mol.L-1)', y='Sobrevivência', color='')+
    scale_color_manual(values = c("black", "#CC0000"))+
    #facet_wrap(~isolados)+ #multi plot
    theme(
      panel.grid.major = element_line(size=.5),
      panel.grid.minor = element_line(size=.2)) #+
    #scale_color_manual(values = c("gray51", "black")) +
    #  legend.title = element_text(size=12, family='mono'),
    #  legend.text = element_text(size = 10, family='sans'),
    #  axis.text = element_text(colour = 'black', size=10, family='sans'),
    #  axis.title = element_text(size=12, family='sans'))+
   # scale_y_log10(#limits = c(10, NA),
                   #labels = trans_format("log10", math_format(10^.x)),
                   #breaks=trans_breaks("log10", function(x) 10^x, n=8)
                   #minor_breaks=log10_minor_break()
                   #)+
    
    #scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))

 ggsave("mic_ancestral.png")

# onde 
	# LD10 = 10% de sobrevivencia
	# LD1 = 1% de sobrevivencia
	# f = fator de hormesis
	# n = formato da curva


y <- dados_media_desvio$Media_Sobrevivencia
x <- as.numeric(dados_media_desvio$Concentracao)

fit_ancestral<- nls(y ~ (1+f*x)/(1+(9+10*f*LD10)*exp(n*log(x/LD10))),
			        start = list(LD10 = 0.3, n = 2, f = 1))
summary(fit_ancestral)

# Extraindo os parâmetros LD10 de cada ajuste
LD10_ancestral <- coef(fit_ancestral)["LD10"]
# Extraindo os erros padrão de LD10
LD10_ancestral_err <- sqrt(diag(vcov(fit_ancestral)))["LD10"]

# Criando um data frame com LD10 e erro padrão
parametros_LD10 <- data.frame(
  Ajuste = c("fit_ancestral"),
  LD10 = c(LD10_ancestral),
  Erro_Padrao = c(LD10_ancestral_err))

library(openxlsx)
library(writexl)


# Criar uma nova planilha
wb <- createWorkbook()

# Adicionar uma nova aba
addWorksheet(wb, "Parametros_LD10")

# Escrever o data frame na aba
writeData(wb, sheet = "Parametros_LD10", parametros_LD10)

# Salvar a planilha
saveWorkbook(wb, "ancestral_LD10.xlsx", overwrite = TRUE)

# Mensagem de sucesso
print("Arquivo Excel salvo com sucesso!")



# Gerar uma sequência suave de valores de concentração para a linha predita
x_fit <- seq(min(x), max(x), length.out = 100)

# Calcular os valores preditos usando o modelo ajustado
y_fit <- predict(fit_ancestral, newdata = list(x = x_fit))

# Criar um data frame com os valores preditos
dados_fit <- data.frame(Concentracao = x_fit, Sobrevivencia_Predita = y_fit)

# Criar o gráfico de média e desvio padrão com a linha predita
plot_media_desvio_fit <- ggplot(dados_media_desvio, aes(x = as.numeric(Concentracao), y = Media_Sobrevivencia)) +
  geom_point(size = 3) +  # Pontos para a média
  #geom_errorbar(aes(ymin = Media_Sobrevivencia - DesvioPadrao_Sobrevivencia, 
                    #ymax = Media_Sobrevivencia + DesvioPadrao_Sobrevivencia), 
                #width = 0.2, color = "red") +  # Barras de erro
  geom_line(data = dados_fit, aes(x = Concentracao, y = Sobrevivencia_Predita), 
            color = "red", size = 1) +  # Linha predita
  labs(
    title = "Média e Desvio Padrão da Sobrevivência no Tempo 2000 com Linha Predita",
    x = "Concentração de Perclorato",
    y = "Sobrevivência Média"
  ) +
  theme_minimal()

# Visualizar o gráfico
print(plot_media_desvio_fit)

# Salvar o gráfico como uma imagem PNG
ggsave("/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento_mic/mic_ancestral/plot_fitting_ancestral.png", plot = plot_media_desvio_fit, width = 8, height = 6, dpi = 300, bg = "white")


