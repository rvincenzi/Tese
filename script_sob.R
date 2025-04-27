

# calcular_sobrevivencia_1440.R

# ------- SCRIPT PARA CALCULAR SOBREVIVÊNCIA NO TEMPO 1440 COM BASE EM dados_medias_t11 --------

# Carregar pacotes necessários
if (!require("readxl")) install.packages("readxl", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("openxlsx")) install.packages("openxlsx", dependencies = TRUE)
if(!require("writexl")) install.packages("openxlsx", dependencies = TRUE)

library(readxl)
library(dplyr)
library(openxlsx)
library(writexl)
library(ggplot2)

# Definir o caminho do arquivo de dados
caminho_arquivo <- "/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento_mic/mic_t11/dados_long_rep_t11.xlsx"

# Carregar os dados de média e desvio padrão do arquivo Excel
dados_long <- read_excel(caminho_arquivo)

# Filtrar os dados para o tempo de interesse (por exemplo, 2880 minutos)
dados_time_2000 <- dados_long %>% filter(Time == 2000)

# Função para calcular a sobrevivência
calcular_sobrevivencia <- function(df) {
  # Obter os valores de controle (concentração 0 mol/L) para cada placa e linhagem
  controle <- df %>%
    filter(Concentracao == "0") %>%
    select(Placa, linhagem, DO) %>%
    rename(DO_Controle = DO)

  # Juntar os valores de controle com os dados e calcular a sobrevivência
  sobrevivencia <- df %>%
    left_join(controle, by = c("Placa", "linhagem")) %>%
    mutate(Sobrevivencia = DO / DO_Controle) %>%
    select(Placa, linhagem, Concentracao, Sobrevivencia)
  
  return(sobrevivencia)
}

# Aplicar a função aos dados filtrados por tempo
dados_sobrevivencia <- calcular_sobrevivencia(dados_time_2000)

# Verificar os resultados
head(dados_sobrevivencia)

print(dados_sobrevivencia, n=66)

# Agrupar por linhagem e concentração, e calcular a média e o desvio padrão
media_desvio_sob <- dados_sobrevivencia %>%
  group_by(linhagem, Concentracao) %>%
  summarise(
    Media_sob = mean(Sobrevivencia, na.rm = TRUE),  # Média da sobrevivência
    DesvioPadrao_sob = sd(Sobrevivencia, na.rm = TRUE)  # Desvio padrão da sobrevivência
  )

# Verificar os resultados
head(media_desvio_sob)

print(media_desvio_sob, n=66)

# Exportar para Excel
write_xlsx(media_desvio_sob, "media_sob_t11.xlsx")

# Mensagem de conclusão
cat("Arquivo com dados de sobrevivência no tempo 1440 foi salvo em:", caminho_saida, "\n")

# plot


sob_t11 <- ggplot(media_desvio_sob, aes(x = Concentracao, y = Media_sob, group = 1)) +
  geom_point(size = 1.5) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.10, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = Media_sob - DesvioPadrao_sob, ymax = Media_sob + DesvioPadrao_sob), 
                width = 0.05, size = 0.5, color = "#CC0000") +
  theme_bw() +
  labs(x = 'Concentration (mol.L-1)', y = 'Survival', color = '') +
  facet_wrap(~linhagem) +  # Multi plot
  theme(
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_line(size = 0.2)) #+
    #scale_color_manual(values = c("gray51", "black")) +
    #  legend.title = element_text(size=12, family='mono'),
    #  legend.text = element_text(size = 10, family='sans'),
    #  axis.text = element_text(colour = 'black', size=10, family='sans'),
    #  axis.title = element_text(size=12, family='sans'))+
    #scale_y_log10(#limits = c(10, NA),
                   #labels = trans_format("log10", math_format(10^.x)),
                   #breaks=trans_breaks("log10", function(x) 10^x, n=8)
                   #minor_breaks=log10_minor_break()
                   #)
    
    #scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))

 ggsave("mic_t11.png")


library(dplyr)

# Separamos o data frame por linhagem usando dplyr
dados_por_linhagem <- media_desvio_sob %>%
  group_split(linhagem)

# Obtemos os nomes das linhagens
linhagens <- unique(media_desvio_sob$linhagem)

# Criamos variáveis x e y para cada linhagem
for (i in seq_along(dados_por_linhagem)) {
  df <- dados_por_linhagem[[i]]
  linhagem_nome <- linhagens[i]
  assign(paste0("y_", linhagem_nome), df$Media_sob)
  assign(paste0("x_", linhagem_nome), as.numeric(df$Concentracao))
}

# Verificar a estrutura geral da lista de dados separados por linhagem
str(dados_por_linhagem)

# Visualizar o primeiro subconjunto de dados (para a primeira linhagem)
head(dados_por_linhagem[[1]])

# Verificar os nomes das linhagens
print(linhagens)

# Criar variáveis x e y para cada linhagem, verificando os valores dentro do loop
for (i in seq_along(dados_por_linhagem)) {
  df <- dados_por_linhagem[[i]]
  linhagem_nome <- linhagens[i]
  
  # Criar as variáveis dinamicamente
  assign(paste0("y_", linhagem_nome), df$Media_sob)
  assign(paste0("x_", linhagem_nome), as.numeric(df$Concentracao))
  
  # Imprimir os valores das primeiras linhas para verificar
  cat("\nLinhagem:", linhagem_nome, "\n")
  cat("x_", linhagem_nome, ": ", head(get(paste0("x_", linhagem_nome))), "\n")
  cat("y_", linhagem_nome, ": ", head(get(paste0("y_", linhagem_nome))), "\n")
}


# onde 
	# LD10 = 10% de sobrevivencia
	# LD1 = 1% de sobrevivencia
	# f = fator de hormesis
	# n = formato da curva

f = 0
fit_ctl1<- nls(y_ctl1 ~ (1+f*x_ctl1)/(1+(9+10*f*LD10)*exp(n*log(x_ctl1/LD10))),
			        start = list(LD10 = 0.2, n = 1))
summary(fit_ctl1)



fit_ctl2<- nls(y_ctl2 ~ (1+f*x_ctl2)/(1+(9+10*f*LD10)*exp(n*log(x_ctl2/LD10))),
			        start = list(LD10 = 0.2, n = 2, f = 1))
summary(fit_ctl2)



fit_ctl3<- nls(y_ctl3 ~ (1+f*x_ctl3)/(1+(9+10*f*LD10)*exp(n*log(x_ctl3/LD10))),
			        start = list(LD10 = 0.2, n = 2, f = 2))
summary(fit_ctl3)



fit_perc1<- nls(y_perc1 ~ (1+f*x_perc1)/(1+(9+10*f*LD10)*exp(n*log(x_perc1/LD10))),
			        start = list(LD10 = 0.3, n = 2, f = 1))
summary(fit_perc1)

fit_perc2<- nls(y_perc2 ~ (1+f*x_perc2)/(1+(9+10*f*LD10)*exp(n*log(x_perc2/LD10))),
			        start = list(LD10 = 0.3, n = 2, f = 1))
summary(fit_perc2)


fit_perc3<- nls(y_perc3 ~ (1+f*x_perc3)/(1+(9+10*f*LD10)*exp(n*log(x_perc3/LD10))),
			        start = list(LD10 = 0.3, n = 2, f = 1))
summary(fit_perc3)


# Extraindo os parâmetros LD10 de cada ajuste
LD10_ctl1 <- coef(fit_ctl1)["LD10"]
LD10_ctl2 <- coef(fit_ctl2)["LD10"]
LD10_ctl3 <- coef(fit_ctl3)["LD10"]
LD10_perc1 <- coef(fit_perc1)["LD10"]
LD10_perc2 <- coef(fit_perc2)["LD10"]
LD10_perc3 <- coef(fit_perc3)["LD10"]

# Extraindo os erros padrão de LD10
LD10_ctl1_err <- sqrt(diag(vcov(fit_ctl1)))["LD10"]
LD10_ctl2_err <- sqrt(diag(vcov(fit_ctl2)))["LD10"]
LD10_ctl3_err <- sqrt(diag(vcov(fit_ctl3)))["LD10"]
LD10_perc1_err <- sqrt(diag(vcov(fit_perc1)))["LD10"]
LD10_perc2_err <- sqrt(diag(vcov(fit_perc2)))["LD10"]
LD10_perc3_err <- sqrt(diag(vcov(fit_perc3)))["LD10"]

# Criando um data frame com LD10 e erro padrão
parametros_LD10 <- data.frame(
  Ajuste = c("fit_ctl1", "fit_ctl2", "fit_ctl3", "fit_perc1", "fit_perc2", "fit_perc3"),
  LD10 = c(LD10_ctl1, LD10_ctl2, LD10_ctl3, LD10_perc1, LD10_perc2, LD10_perc3),
  Erro_Padrao = c(LD10_ctl1_err, LD10_ctl2_err, LD10_ctl3_err, LD10_perc1_err, LD10_perc2_err, LD10_perc3_err)
)

# Instalar e carregar o pacote openxlsx (se necessário)
# install.packages("openxlsx")
library(openxlsx)

# Criar uma nova planilha
wb <- createWorkbook()

# Adicionar uma nova aba
addWorksheet(wb, "Parametros_LD10")

# Escrever o data frame na aba
writeData(wb, sheet = "Parametros_LD10", parametros_LD10)

# Salvar a planilha
saveWorkbook(wb, "t11_LD10.xlsx", overwrite = TRUE)

# Mensagem de sucesso
print("Arquivo Excel salvo com sucesso!")



# Carregar pacotes necessários
library(ggplot2)
library(dplyr)

# Criar um data frame com todos os dados observados
dados_graficos <- data.frame(
  Concentracao = c(x_ctl1, x_ctl2, x_ctl3, x_perc1, x_perc2, x_perc3),
  Sobrevivencia_Observada = c(y_ctl1, y_ctl2, y_ctl3, y_perc1, y_perc2, y_perc3),
  Linhagem = rep(c("ctl1", "ctl2", "ctl3", "perc1", "perc2", "perc3"), 
                 times = c(length(x_ctl1), length(x_ctl2), length(x_ctl3), 
                           length(x_perc1), length(x_perc2), length(x_perc3)))
)

# Gerar uma sequência suave de concentrações para cada linhagem
x_suave <- seq(min(dados_graficos$Concentracao), max(dados_graficos$Concentracao), length.out = 1000)

# Criar um novo data frame com os valores preditos suaves
dados_suaves <- data.frame(
  Concentracao = rep(x_suave, times = 6),  # Repetir para cada linhagem
  Linhagem = rep(c("ctl1", "ctl2", "ctl3", "perc1", "perc2", "perc3"), each = length(x_suave)),
  Sobrevivencia_Predita = c(
    predict(fit_ctl1, newdata = list(x_ctl1 = x_suave)),
    predict(fit_ctl2, newdata = list(x_ctl2 = x_suave)),
    predict(fit_ctl3, newdata = list(x_ctl3 = x_suave)),
    predict(fit_perc1, newdata = list(x_perc1 = x_suave)),
    predict(fit_perc2, newdata = list(x_perc2 = x_suave)),
    predict(fit_perc3, newdata = list(x_perc3 = x_suave))
  )
)

# Criar o gráfico com facet_wrap e o tema personalizado
grafico_facet <- ggplot() +
  # Dados observados
  geom_point(data = dados_graficos, aes(x = Concentracao, y = Sobrevivencia_Observada), color = "black", size = 2) +
  # Linha de predição suave
  geom_line(data = dados_suaves, aes(x = Concentracao, y = Sobrevivencia_Predita), color = "red", size = 1) +
  # Separar por linhagem
  facet_wrap(~ Linhagem) +  # Mesma escala para todos os painéis
  # Rótulos dos eixos
  labs(x = 'Concentração (mol.L-1)', y = 'Sobrevivência') +
  # Tema personalizado
  theme_bw() +
  theme(
    text = element_text(size = 12),  # Tamanho da fonte
    axis.text = element_text(color = "black"),  # Cor do texto dos eixos
    panel.grid.major = element_line(color = "gray80", size = 0.5),  # Grade principal
    panel.grid.minor = element_line(color = "gray90", size = 0.2)   # Grade secundária
  )

# Visualizar o gráfico
print(grafico_facet)

# Salvar o gráfico em um arquivo PNG
ggsave("painel_fitting_t11.png", grafico_facet, width = 10, height = 8)




