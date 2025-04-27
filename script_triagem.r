## Análise congresso CLA dados preliminares triagem

# 1. Processamento da tabela

# REGRAS DE ENTRADA:
# ***** ORDEM DAS COLUNAS: 1. ID Trigem | 2.Isolado | 3. Condição do pré-inóculo | 4. DO inicial | 5. DO 3 dias | 6. DO 4 dias | 7. DO 5 dias | 8. DO 7 dias | 9. Variação DO 3 dias |
# 10. Variação DO 4 dias | 11. Variação DO 5 dias | 12. Variação DO 7 dias | 13. Observações

#PACOTES
install.packages("dplyr")   #Linha necessária apenas se não tiver pacote já instalado
library(dplyr)
install.packages("janitor")
library(janitor)
install.packages("ggplot2")
library(ggplot2)
install.packages("viridis")
library(viridis)
#leitura da tabela
  triagem_bruta <- read.table("triagem.tsv", header = TRUE, sep = "\t", stringsAsFactors = F, dec = ".")
	
	
#subset da tabela

	triagem_limpa <- triagem_bruta[,-c(5, 7, 9, 10, 11, 12, 13) ]     # Tira a coluna de DO 3 dias, DO 5 dias, colunas de variação de DO e observação

	triagem <- na.omit(triagem_limpa)
	
# variação de DO

	delta_0d <- triagem[,4]-triagem[,4] #vetor contendo do0

	delta_4d <- triagem[,5]-triagem[,4] #vetor contendo do4 - doinicial
	
	delta_7d <- triagem[,6]-triagem[,4] #vetor contendo do7 - doinicial
	
	
# adicionar os vetores de variação de DO na tabela

	triagem_final <- cbind(triagem, delta_0d, delta_4d, delta_7d) 
	
#seleção

##opção 1: todos os isolados que apresentam pelo menos um ponto com variação de DO maior que 0.01
escolhidos <- triagem_final %>% filter(delta_4d >= 0.01 | delta_7d >= 0.01)

##opção 2: todos os isolados que apresentam os dois pontos com variação de DO maior que 0.01
escolhidos <- triagem_final %>% filter(delta_4d>=0.0090 & delta_7d>=0.009)

# reorganizar a tabela para plot dos escolhidos

##criando vetores para fazer a tabela

id <- escolhidos$Isolado  #id isolado

do_t0 <- escolhidos$delta_0d #vetor com valores de T0

do_t4 <- escolhidos$delta_4d #vetor com valores de T4

do_t7 <- escolhidos$delta_7d #vetor com valores de T7


tempo <- rep(c(0, 4, 7),each=length(escolhidos$Isolado)) #vetor de repetição dos tempos
ID <- rep(id, times=3) # vetor de repetição dos ids dos isolados

DO <- append(do_t0, do_t4, after=length(do_t0)) #adicionei t4 ao t0
do <- append(DO, do_t7, after=length(DO)) #adicionei o t7

plot_esc <- cbind.data.frame(ID, tempo, do)

						
# reorganizar a tabela para plot geral

##criando vetores para fazer a tabela

nomes <- rep(triagem_final$Isolado, times=2)

tempos <- rep(c(4, 7),each=length(triagem_final$Isolado)) #vetor de repetição dos tempos

do_geral <- append(delta_4d, delta_7d, after=length(delta_4d))

plot_geral <- cbind.data.frame(nomes, tempos, do_geral)

plot_geral$tempos <- as.factor(plot_geral$tempos)

# PLOTS
## violin plot do triagem geral

violin <- ggplot(plot_geral, aes(x=tempos , y=do_geral, group=tempos))+
    geom_hline(yintercept=0, color="gray30")+
    geom_violin(fill="#A50021", alpha=0.95)+
    labs(
    x="Tempo (dias)",
    y=expression(Delta~DO[600]))+
    theme_bw()+
    theme(
    legend.title = element_text(size=12),
    legend.text = element_text(size = 11),
    axis.text = element_text(colour = "black", size=12),
    axis.title = element_text(size=12))+
    geom_hline(yintercept=0.01, linetype="dashed", size=0.9)
	
violin

ggsave("violin.png")	
	

##plot dos escolhidos


graf1<- ggplot(plot_esc, aes(x=tempo, y=do, group=ID, color=ID)) +
    geom_line(size=.8)+
    labs(
    x="Tempo (dias)",
    y=expression(Delta~DO[600]),
    color="Isolados")+
    theme_bw()+
    scale_color_viridis(discrete=TRUE, option="turbo")+
    theme(
    legend.title = element_text(size=12),
    legend.text = element_text(size = 11),
    axis.text = element_text(colour = "black", size=12),
    axis.title = element_text(size=12))+
    coord_cartesian(xlim=c(0, 7))+
    scale_x_continuous(breaks=c(0, 4, 7))

graf1
ggsave("escolhidos1.png")	

 ## multi plot escolhidos
 
graf <- ggplot(plot_esc, aes(x=tempo, y=do, group=ID, color=ID)) +
    geom_line(size=.8, )+
    labs(
    x="Tempo (dias)",
    y=expression(Delta~DO[600]),
    color="Isolados")+
	facet_wrap(~ID)+ #multi plot
    theme_bw()+
    scale_color_viridis(discrete=TRUE, option="turbo")+
    theme(
	legend.title = element_blank(),legend.position = "none",
    axis.text = element_text(colour = "black", size=12),
    axis.title = element_text(size=12))+
    coord_cartesian(xlim=c(0, 7))+
    scale_x_continuous(breaks=c(0, 4, 7))

graf
	
ggsave("escolhidos.png",  width=8.1)	
	
### highlight

highlight<- plot_esc %>% 
				mutate(highlight_flag = ifelse(ID=="lv7.am3.c"|ID=="lv7.am1.g", T, F)) %>%
    ggplot(aes(x=tempo, y=do, group=ID))+
    geom_line(size=.8, aes(color=highlight_flag))+
    labs(
    x="Tempo (dias)",
    y=expression(Delta~DO[600]),
    color="Isolados")+
    theme_bw()+
    theme(
    legend.position="none",
    axis.text = element_text(colour = "black", size=12),
    axis.title = element_text(size=12))+
    coord_cartesian(xlim=c(0, 7))+
    scale_x_continuous(breaks=c(0, 4, 7))+
    scale_color_manual(values = c("lightgrey", "#A50021"))
	
highlight
	
ggsave("highlight.png")	