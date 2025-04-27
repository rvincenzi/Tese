# Carregar os pacotes necessários
library(ggplot2)

# Ler a matriz de presença/ausência gerada pelo Panaroo
data <- read.table("gene_presence_absence.Rtab", 
                   sep = "\t", header = TRUE, row.names = 1, check.names = FALSE)

# Calcular o tamanho do pangenoma (número total de famílias gênicas)
pangenome_size <- nrow(data)

# Calcular o número de genes no core, shell e cloud
core_size <- sum(rowSums(data) > 0.99 * ncol(data))  # Genes presentes em >99% dos genomas
shell_size <- sum(rowSums(data) < 0.99 * ncol(data) & rowSums(data) > 0.15 * ncol(data))  # Genes intermediários (shell)
cloud_size <- sum(rowSums(data) < 0.15 * ncol(data))  # Genes raros (cloud)

# Criar um gráfico de pizza para visualizar a proporção do core, shell e cloud
slices <- c(core_size, shell_size, cloud_size)
pct <- round(slices / sum(slices) * 100, 2)
labels <- paste(c("Core", "Shell", "Cloud"), pct, "%", sep = " ")

png("pangenome_sum.png", width = 800, height = 600)
par(mfrow = c(1, 2), pin = c(2.5, 2.5))  # Configurar layout para dois gráficos lado a lado

# Figura 1A: Gráfico de pizza do pangenoma
pie(slices, labels = labels, main = "Pangenome Composition", cex = 0.8, col = c("#B22222", "white", "gray"))


# Figura 1B: Histograma da frequência gênica nos genomas
hist(rowSums(data), 
     xlab = "Number of genomes containing a gene", 
     ylab = "Number of genes", 
     main = "Gene Frequency", 
     ylim = c(0, 5000), 
     xlim = c(0, ncol(data) + 1), 
     breaks = seq(min(rowSums(data)) - 0.5, max(rowSums(data)) + 0.5, by = 1),
     col = "gray", border = "black")
dev.off()



# Carregar bibliotecas
install.packages("vegan")
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("micropan")

library(micropan)
library(vegan)

# Ler a matriz de presença/ausência gerada pelo Panaroo
data <- read.table("gene_presence_absence.Rtab", sep = "\t", header = TRUE, row.names = 1, check.names = FALSE)

# Transpor o dataframe
df_t <- t(data)
rownames(df_t) <- colnames(data)
colnames(df_t) <- rownames(data)

# Calcular os coeficientes 'k' e 'α' com 1000 permutações
heap <- heaps(df_t, n.perm = 1000)

# Calcular a curva de rarefação com 1000 permutações
rf <- specaccum(df_t, method = "random", permutations = 1000)

png("curva_pangenoma.png", width = 800, height = 600)
# Plotar a curva de acumulação com intervalos de confiança
plot(rf, ci.type = "poly", col = "#B22222", lwd = 2,
     ci.lty = 0, ci.col = "gray",
     xlab = "Number of genomes",
     ylab = "Number of gene families",
     ylim = c(4000, 6000))

# Adicionar legenda com o valor do coeficiente 'α'
legend(x = "bottomright", legend = paste("α =", round(heap[2], 2)), fill = "#B22222")
dev.off()



# plot venn
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("limma")
# Carregar a biblioteca limma
library(limma)

# Importar a matriz de presença/ausência gerada pelo Panaroo
data <- read.table("gene_presence_absence.Rtab", sep = "\t", header = TRUE, check.names = FALSE, row.names = 1)

# Vetor de nomes dos organismos
colnames(data) <- c(
  "am1d",  # Não há correspondência na tabela, mantido original
  "am1e",  # Não há correspondência na tabela, mantido original
  "colB",  # Não há correspondência na tabela, mantido original
  "GCF_000010125.1 - Staphylococcus saprophyticus",
  "GCF_000013425.1 - Staphylococcus aureus",
  "GCF_000709415.1 - Staphylococcus xylosus",
  "GCF_001558775.1 - Staphylococcus lugdunensis",
  "GCF_002442895.1 - Staphylococcus nepalensis",
  "GCF_002442935.1 - Staphylococcus nepalensis Ref",
  "GCF_003042875.1 - Staphylococcus nepalensis",
  "GCF_003097555.1 - Staphylococcus nepalensis",
  "GCF_003579615.1 - Staphylococcus nepalensis",
  "GCF_006094375.1 - Staphylococcus epidermidis",
  "GCF_006094395.1 - Staphylococcus haemolyticus",
  "GCF_016127455.1 - Staphylococcus equorum",
  "GCF_017368655.1 - Staphylococcus nepalensis",
  "GCF_017368675.1 - Staphylococcus nepalensis",
  "GCF_017368705.1 - Staphylococcus nepalensis",
  "GCF_017368735.1 - Staphylococcus nepalensis",
  "GCF_026626865.1 - Staphylococcus nepalensis",
  "GCF_029024945.1 - Staphylococcus succinus",
  "GCF_034030455.1 - Staphylococcus nepalensis",
  "GCF_041226205.1 - Staphylococcus nepalensis",
  "GCF_044740705.1 - Staphylococcus nepalensis",
  "GCF_902362425.1 - Staphylococcus nepalensis",
  "GCF_910574155.1 - Staphylococcus nepalensis",
  "GCF_910576055.1 - Staphylococcus nepalensis",
  "GCF_948995735.1 - Staphylococcus nepalensis",
  "GCF_949025715.1 - Staphylococcus nepalensis",
  "GCF_949841375.1 - Staphylococcus nepalensis"
)

# Selecionar apenas "Chromohalobacter israelensis DSM 3043" e "p4d"
selected_data <- data[, c("am1e", "am1d", "colB", "S. nepalensis Ref" )]

# Calcular as contagens para o diagrama de Venn
counts <- vennCounts(selected_data)

png("diagrama_venn.png", width = 1000, height = 900, res = 150)
# Plotar o diagrama de Venn
vennDiagram(counts, circle.col = c("#B22222", "#A9A9A9", "#228B22", "#1E90FF"), cex = 1)
dev.off()

# Heatmap
#install.packages("pheatmap")
library(pheatmap)

# Ler a matriz de presença/ausência gerada pelo Panaroo
data <- read.table("gene_presence_absence.Rtab", sep = "\t", header = TRUE, row.names = 1, check.names = FALSE)

# Vetor de nomes dos organismos
colnames(data) <- c(
  "am1d",  # Não há correspondência na tabela, mantido original
  "am1e",  # Não há correspondência na tabela, mantido original
  "colB",  # Não há correspondência na tabela, mantido original
  "GCF_000010125.1 - Staphylococcus saprophyticus",
  "GCF_000013425.1 - Staphylococcus aureus",
  "GCF_000709415.1 - Staphylococcus xylosus",
  "GCF_001558775.1 - Staphylococcus lugdunensis",
  "GCF_002442895.1 - Staphylococcus nepalensis",
  "GCF_002442935.1 - Staphylococcus nepalensis Ref",
  "GCF_003042875.1 - Staphylococcus nepalensis",
  "GCF_003097555.1 - Staphylococcus nepalensis",
  "GCF_003579615.1 - Staphylococcus nepalensis",
  "GCF_006094375.1 - Staphylococcus epidermidis",
  "GCF_006094395.1 - Staphylococcus haemolyticus",
  "GCF_016127455.1 - Staphylococcus equorum",
  "GCF_017368655.1 - Staphylococcus nepalensis",
  "GCF_017368675.1 - Staphylococcus nepalensis",
  "GCF_017368705.1 - Staphylococcus nepalensis",
  "GCF_017368735.1 - Staphylococcus nepalensis",
  "GCF_026626865.1 - Staphylococcus nepalensis",
  "GCF_029024945.1 - Staphylococcus succinus",
  "GCF_034030455.1 - Staphylococcus nepalensis",
  "GCF_041226205.1 - Staphylococcus nepalensis",
  "GCF_044740705.1 - Staphylococcus nepalensis",
  "GCF_902362425.1 - Staphylococcus nepalensis",
  "GCF_910574155.1 - Staphylococcus nepalensis",
  "GCF_910576055.1 - Staphylococcus nepalensis",
  "GCF_948995735.1 - Staphylococcus nepalensis",
  "GCF_949025715.1 - Staphylococcus nepalensis",
  "GCF_949841375.1 - Staphylococcus nepalensis"
)


# Transpor o dataframe
df_t <- t(data)
rownames(df_t) <- colnames(data)
colnames(df_t) <- rownames(data)

# Converter o dataframe para matriz numérica
pange <- as.matrix(df_t)
pange <- apply(pange, 2, as.numeric)  # Garantir que os valores sejam numéricos
rownames(pange) <- colnames(data)


png("heatmap_dendrograma.png", width = 1000, height = 800, res = 150)
# Plotar o heatmap
hm <- pheatmap(
    pange,
    clustering_distance_rows = "manhattan",
    clustering_method = "ward.D",
    color = c("white", "#808080"),
    clustering_distance_cols = "manhattan",
    show_colnames = FALSE,  # Ocultar nomes dos genes (colunas)
    show_rownames = TRUE,   # Mostrar nomes dos genomas (linhas)
    cluster_cols = FALSE,
    cluster_rows = TRUE,
    legend = FALSE,
    fontsize_row = 8  # Ajustar o tamanho da fonte para os rótulos das linhas
)
dev.off()

# Recuperar a matriz de dados após o clustering
reorder <- data[hm$tree_col[["order"]], ]
reorder <- reorder[, hm$tree_row[["order"]]]