

# calcular_sobrevivencia 24h

# ------- SCRIPT PARA CALCULAR SOBREVIVÊNCIA NO TEMPO 1440 --------

# Carregar pacotes necessários
if (!require("readxl")) install.packages("readxl", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("openxlsx")) install.packages("openxlsx", dependencies = TRUE)
if (!require("scales")) install.packages("scales")


library(scales)
library(readxl)
library(dplyr)
library(openxlsx)

#setwd("/Users/rvincenzi/Documents/USP/Doutorado/Dados/Escolhidos/19isolados")

# Definir o caminho do arquivo de dados
caminho_arquivo <- "/Users/rvincenzi/Documents/USP/Doutorado/Dados/Escolhidos/19isolados/plate_reader_escolhidos/dados_long.xlsx"

# Carregar os dados de média e desvio padrão do arquivo Excel
dados_long <- read_excel(caminho_arquivo)

# Filtrar os dados para o tempo de interesse (por exemplo, 1440 minutos)
dados_time_1440 <- dados_long %>% filter(Time == 1440)

# Função para calcular a sobrevivência
calcular_sobrevivencia <- function(df) {
  # Obter os valores de controle (concentração 0 mol/L) para cada placa e isolado
  controle <- df %>%
    filter(Concentracao == "0") %>%
    select(Placa, Isolado, DO) %>%
    rename(DO_Controle = DO)

  # Juntar os valores de controle com os dados e calcular a sobrevivência
  sobrevivencia <- df %>%
    left_join(controle, by = c("Placa", "Isolado")) %>%
    mutate(Sobrevivencia = DO / DO_Controle) %>%
    select(Placa, Isolado, Concentracao, Sobrevivencia)
  
  return(sobrevivencia)
}

# Aplicar a função aos dados filtrados por tempo
dados_sobrevivencia <- calcular_sobrevivencia(dados_time_1440)

# Verificar os resultados
head(dados_sobrevivencia)


# Agrupar por isolado e concentração, e calcular a média e o desvio padrão
media_desvio_sob <- dados_sobrevivencia %>%
  group_by(Isolado, Concentracao) %>%
  summarise(
    Media_sob = mean(Sobrevivencia, na.rm = TRUE),  # Média da sobrevivência
    DesvioPadrao_sob = sd(Sobrevivencia, na.rm = TRUE)  # Desvio padrão da sobrevivência
  )

# Verificar os resultados
head(media_desvio_sob)
# Salvar os resultados em um arquivo Excel (opcional)
write.xlsx(media_desvio_sob, "media_desvio_sob.xlsx")


print(media_desvio_sob, n=66)


# plot

sob_19isolados_log10 <- ggplot(media_desvio_sob, aes(x = Concentracao, y = Media_sob, group = 1)) +
  geom_point(size = 1.5) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.10, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = pmax(Media_sob - DesvioPadrao_sob, 0.001), #se o sd min for <0,001, o código substitui o valor para 0,001
                    ymax = Media_sob + DesvioPadrao_sob), 
                	width = 0.05, size = 0.5, color = "black") +
  scale_y_continuous(trans = "log10") +       # Aplica transformação log10 ao eixo Y
  theme_bw() +
  labs(x = 'Concentration (mol.L-1)', y = 'Survival', color = '') +
  facet_wrap(~Isolado) +  # Multi plot
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

 ggsave("sob_19isolados_log10.png", width = 10, height = 8)
 
 sob_19isolados <- ggplot(media_desvio_sob, aes(x = Concentracao, y = Media_sob, group = 1)) +
  geom_point(size = 1.5) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.10, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = Media_sob - DesvioPadrao_sob, ymax = Media_sob + DesvioPadrao_sob), 
                width = 0.05, size = 0.5, color = "black") +
  theme_bw() +
  labs(x = 'Concentration (mol.L-1)', y = 'Survival', color = '') +
  facet_wrap(~Isolado) +  # Multi plot
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
                   )
    
    #scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))

 ggsave("sob_19isolados.png")




# Separamos o data frame por linhagem usando dplyr
dados_por_isolado <- media_desvio_sob %>%
  group_split(Isolado)

# Obtemos os nomes das linhagens
isolados <- unique(media_desvio_sob$Isolado)

# Criamos variáveis x e y para cada linhagem
for (i in seq_along(dados_por_isolado)) {
  df <- dados_por_isolado[[i]]
  isolado_nome <- isolados[i]
  assign(paste0("y_", isolado_nome), df$Media_sob)
  assign(paste0("x_", isolado_nome), as.numeric(df$Concentracao))
}

# Verificar a estrutura geral da lista de dados separados por linhagem
str(dados_por_isolado)

# Visualizar o primeiro subconjunto de dados (para a primeira linhagem)
head(dados_por_isolado[[1]])

# Verificar os nomes das linhagens
print(isolados)

# Criar variáveis x e y para cada linhagem, verificando os valores dentro do loop
for (i in seq_along(dados_por_isolado)) {
  df <- dados_por_isolado[[i]]
  isolado_nome <- isolados[i]
  
  # Criar as variáveis dinamicamente
  assign(paste0("y_", isolado_nome), df$Media_sob)
  assign(paste0("x_", isolado_nome), as.numeric(df$Concentracao))
  
  # Imprimir os valores das primeiras linhas para verificar
  cat("\nIsolado:", isolado_nome, "\n")
  cat("x_", isolado_nome, ": ", head(get(paste0("x_", isolado_nome))), "\n")
  cat("y_", isolado_nome, ": ", head(get(paste0("y_", isolado_nome))), "\n")
}


# onde 
	# LD10 = 10% de sobrevivencia
	# LD1 = 1% de sobrevivencia
	# f = fator de hormesis
	# n = formato da curva
	
	
# be5.am1.g # NAO FITOU
fit_be5.am1.g<- nls(y_be5.am1.g ~ (1+f*x_be5.am1.g)/(1+(9+10*f*LD10)*exp(n*log(x_be5.am1.g/LD10))),
			        start = list(LD10 = 0.2, n = 1, f = 1))
summary(fit_be5.am1.g)

# be6.ext3.d1
f = 0
fit_be6.ext3.d1<- nls(y_be6.ext3.d1 ~ (1+f*x_be6.ext3.d1)/(1+(9+10*f*LD10)*exp(n*log(x_be6.ext3.d1/LD10))),
			        start = list(LD10 = 0.2, n = 2))
summary(fit_be6.ext3.d1)

# be6.ext3.d2
f = 0
fit_be6.ext3.d2<- nls(y_be6.ext3.d2 ~ (1+f*x_be6.ext3.d2)/(1+(9+10*f*LD10)*exp(n*log(x_be6.ext3.d2/LD10))),
			        start = list(LD10 = 0.2, n = 2))
summary(fit_be6.ext3.d2)

# colb
fit_colb<- nls(y_colb ~ (1+f*x_colb)/(1+(9+10*f*LD10)*exp(n*log(x_colb/LD10))),
			        start = list(LD10 = 0.2, n = 2, f = 1))
summary(fit_colb)

# lv7.am1.l
f=0
fit_lv7.am1.l<- nls(y_lv7.am1.l ~ (1+f*x_lv7.am1.l)/(1+(9+10*f*LD10)*exp(n*log(x_lv7.am1.l/LD10))),
			        start = list(LD10 = 0.2, n = 2))
summary(fit_lv7.am1.l)

# lv7.am3.b #NAO FITOU
f = 0
fit_lv7.am3.b<- nls(y_lv7.am3.b ~ (1+f*x_lv7.am3.b)/(1+(9+10*f*LD10)*exp(n*log(x_lv7.am3.b/LD10))),
			        start = list(LD10 = 0.2, n = 2))
summary(fit_lv7.am3.b)

# p4d
fit_p4d<- nls(y_p4d ~ (1+f*x_p4d)/(1+(9+10*f*LD10)*exp(n*log(x_p4d/LD10))),
			        start = list(LD10 = 0.2, n = 2, f = 1))
summary(fit_p4d)

# tbe2.ext2.g
fit_tbe2.ext2.g<- nls(y_tbe2.ext2.g ~ (1+f*x_tbe2.ext2.g)/(1+(9+10*f*LD10)*exp(n*log(x_tbe2.ext2.g/LD10))),
			        start = list(LD10 = 0.3, n = 2, f = 1))
summary(fit_tbe2.ext2.g)

# tbe2.sob.d
fit_tbe2.sob.d<- nls(y_tbe2.sob.d ~ (1+f*x_tbe2.sob.d)/(1+(9+10*f*LD10)*exp(n*log(x_tbe2.sob.d/LD10))),
			        start = list(LD10 = 0.3, n = 2, f = 1))
summary(fit_tbe2.sob.d)

# tbe4.am2.d # NAO FITOU
fit_tbe4.am2.d<- nls(y_tbe4.am2.d ~ (1+f*x_tbe4.am2.d)/(1+(9+10*f*LD10)*exp(n*log(x_tbe4.am2.d/LD10))),
			        start = list(LD10 = 0.1, n = 2, f = 1))
summary(fit_tbe4.am2.d)

# tbe4.ext1.b2 
f = 0
fit_tbe4.ext1.b2<- nls(y_tbe4.ext1.b2 ~ (1+f*x_tbe4.ext1.b2)/(1+(9+10*f*LD10)*exp(n*log(x_tbe4.ext1.b2/LD10))),
			        start = list(LD10 = 0.2, n = 2))
summary(fit_tbe4.ext1.b2)

# tbe5.am1.d
f = 0
fit_tbe5.am1.d<- nls(y_tbe5.am1.d ~ (1+f*x_tbe5.am1.d)/(1+(9+10*f*LD10)*exp(n*log(x_tbe5.am1.d/LD10))),
			        start = list(LD10 = 0.2, n = 2))
summary(fit_tbe5.am1.d)

# tbe5.am1.e
fit_tbe5.am1.e<- nls(y_tbe5.am1.e ~ (1+f*x_tbe5.am1.e)/(1+(9+10*f*LD10)*exp(n*log(x_tbe5.am1.e/LD10))),
			        start = list(LD10 = 0.3, n = 2, f = 1))
summary(fit_tbe5.am1.e)

# tlv6.am1.j # NAO FITOU
fit_tlv6.am1.j<- nls(y_tlv6.am1.j ~ (1+f*x_tlv6.am1.j)/(1+(9+10*f*LD10)*exp(n*log(x_tlv6.am1.j/LD10))),
			        start = list(LD10 = 0.2, n = 1, f = 5))
summary(fit_tlv6.am1.j)

# tlv7.am1.c
fit_tlv7.am1.c<- nls(y_tlv7.am1.c ~ (1+f*x_tlv7.am1.c)/(1+(9+10*f*LD10)*exp(n*log(x_tlv7.am1.c/LD10))),
			        start = list(LD10 = 0.2, n = 2, f = 2))
summary(fit_tlv7.am1.c)

# tlv7.am1.e
fit_tlv7.am1.e<- nls(y_tlv7.am1.e ~ (1+f*x_tlv7.am1.e)/(1+(9+10*f*LD10)*exp(n*log(x_tlv7.am1.e/LD10))),
			        start = list(LD10 = 0.2, n = 2, f = 2))
summary(fit_tlv7.am1.e)

# tlv7.am4.h
f = 0
fit_tlv7.am4.h<- nls(y_tlv7.am4.h ~ (1+f*x_tlv7.am4.h)/(1+(9+10*f*LD10)*exp(n*log(x_tlv7.am4.h/LD10))),
			        start = list(LD10 = 0.2, n = 2))
summary(fit_tlv7.am4.h)

# tlv7.ext2.k
fit_tlv7.ext2.k<- nls(y_tlv7.ext2.k ~ (1+f*x_tlv7.ext2.k)/(1+(9+10*f*LD10)*exp(n*log(x_tlv7.ext2.k/LD10))),
			        start = list(LD10 = 0.3, n = 2, f = 2))
summary(fit_tlv7.ext2.k)

 
# Lista de novos isolados
ajustes_novos_isolados <- c("be5.am1.g", "be6.ext3.d1", "be6.ext3.d2", "colb", 
                            "lv7.am1.L", "lv7.am3.b", "p4d", "tbe2.ext2.g", 
                            "tbe2.sob.d", "tbe4.am2.d", "tbe4.ext1.b2", 
                            "tbe5.am1.d", "tbe5.am1.e", "tlv6.am1.j", 
                            "tlv7.am1.c", "tlv7.am1.e", "tlv7.am4.h", "tlv7.ext2.k")






# Inicializando um vetor vazio para armazenar os parâmetros LD10 e seus erros
parametros_LD10_novos_isolados <- vector("list", length(ajustes_novos_isolados))

# Loop para extrair os valores de LD10 e seus erros para cada ajuste
for (i in seq_along(ajustes_novos_isolados)) {
  # Montar o nome do modelo a ser usado dinamicamente
  ajuste_nome <- ajustes_novos_isolados[i]
  ajuste_modelo_nome <- paste0("fit_", ajuste_nome)
  
  # Verificar se o objeto do modelo existe
  if (exists(ajuste_modelo_nome)) {
    ajuste_modelo <- get(ajuste_modelo_nome)  # Acessar o modelo dinamicamente
    
    # Verificar se o parâmetro LD10 existe entre os coeficientes
    if ("LD10" %in% names(coef(ajuste_modelo))) {
      ld10_value <- coef(ajuste_modelo)["LD10"]
      
      # Obter o erro padrão do LD10
      ajuste_summary <- summary(ajuste_modelo)
      ld10_error <- ajuste_summary$coefficients["LD10", "Std. Error"]
    } else {
      ld10_value <- "no fit"  # Se o parâmetro LD10 não existir
      ld10_error <- NA  # Sem erro se não houver ajuste
    }
  } else {
    ld10_value <- "no fit"  # Se o objeto do modelo não existir
    ld10_error <- NA  # Sem erro se o modelo não existir
  }
  
  # Armazenar o valor de LD10 e o erro
  parametros_LD10_novos_isolados[[i]] <- c(ld10_value, ld10_error)
}

# Criar o data frame com os resultados
parametros_LD10_novos_isolados_df <- data.frame(
  Ajuste = ajustes_novos_isolados,
  LD10 = sapply(parametros_LD10_novos_isolados, "[", 1),
  Erro_LD10 = sapply(parametros_LD10_novos_isolados, "[", 2)
)

# Verificando o resultado
print(parametros_LD10_novos_isolados_df)

# Salvar os resultados em um arquivo Excel (opcional)
write.xlsx(parametros_LD10_novos_isolados_df, "LD10_19isolados_com_erro.xlsx")


















