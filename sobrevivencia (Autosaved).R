

# sobrevivencia.R

# ------- SCRIPT INDEPENDENTE PARA FILTRAGEM DE DADOS E CÁLCULO DE SOBREVIVÊNCIA (TIME 1440) --------

# Carregar pacotes necessários
if (!require("readxl")) install.packages("readxl", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("openxlsx")) install.packages("openxlsx", dependencies = TRUE)

library(readxl)
library(dplyr)
library(openxlsx)

# Definir o caminho para o arquivo de dados existente
caminho_arquivo <- "/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento/mic_ancestral/dados_media.xlsx"

# Carregar os dados de média e desvio padrão do arquivo Excel
dados_medias <- read_excel(caminho_arquivo)

# Filtrar os dados apenas para o tempo 2700
dados_time_2700 <- dados_medias %>%
  filter(Time == 2000)

# Encontrar o valor de Media_DO para a concentração zero (0) no tempo 1440
media_do_zero <- dados_time_2700 %>%
  filter(Concentracao == "0") %>%
  pull(Media_DO)  # Extrai o valor de Media_DO para a concentração 0

# Calcular a sobrevivência para cada concentração dividindo o Media_DO pela Media_DO da concentração zero
dados_time_2700 <- dados_time_2700 %>%
  mutate(Sobrevivencia = Media_DO / media_do_zero)

# Verificar o resultado
print(head(dados_time_2700))

# Definir o caminho para salvar o novo arquivo com os dados filtrados e a sobrevivência
caminho_arquivo_sobrevivencia <- "/Users/rvincenzi/Documents/USP/Doutorado/Dados/ALE/processamento/mic_ancestral/dados_sobrevivencia.xlsx"

# Salvar os dados filtrados com a sobrevivência em um novo arquivo Excel
write.xlsx(dados_time_2700, file = caminho_arquivo_sobrevivencia, rowNames = FALSE)

# Mensagem de conclusão
cat("Arquivo com dados filtrados e coluna de Sobrevivência para o tempo 1440 foi salvo em:", caminho_arquivo_sobrevivencia, "\n")

# plot


sob_ancestral<- ggplot(dados_time_2700, aes(x=Concentracao, y=Sobrevivencia, group = 1)) +
    geom_point(size=1.5)+
    geom_line(linewidth=1)+
    geom_hline(yintercept = 0.10, linetype = "dashed", color = "red") +
    #geom_errorbar(aes(ymin=sob-erro_sob, ymax=sob+erro_sob), width=0.05, size=.5, color="#CC0000")+
    theme_bw()+
    labs(x='Concentration (mol.L-1)', y='Survival', color='')+
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


y <- dados_time_2700$Sobrevivencia
x <- as.numeric(dados_time_2700$Concentracao)

fit_ancestral<- nls(y ~ (1+f*x)/(1+(9+10*f*LD10)*exp(n*log(x/LD10))),
			        start = list(LD10 = 0.3, n = 2, f = 1))
summary(fit_ancestral)

# Extraindo os parâmetros LD10 de cada ajuste
LD10_ancestral <- coef(fit_ancestral)["LD10"]


