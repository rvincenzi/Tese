#pacotes

BiocManager::install("ggtree")

# Carregar a biblioteca ggtree
library(ggtree)

# Ler o arquivo Newick
tree <- read.tree("staph_tree.treefile")

# Verificar os nomes das pontas da árvore
print(tree$tip.label, n = 40)


# Vetor de correspondência manual para Staphylococcus
tip_labels <- c(
  "GCF_016127455.1_ASM1612745v1_genomic.fna" = "GCF_016127455.1 - Staphylococcus equorum",
  "GCF_002442895.1_ASM244289v1_genomic.fna" = "GCF_002442895.1 - Staphylococcus nepalensis",
  "GCF_002442935.1_ASM244293v1_genomic.fna" = "GCF_002442935.1 - Staphylococcus nepalensis",
  "GCF_003097555.1_ASM309755v1_genomic.fna" = "GCF_003097555.1 - Staphylococcus nepalensis",
  "colB_scaffolds.fna" = "colb",
  "am1e.fna" = "am1e",
  "am1d.fna" = "am1d",
  "GCF_044740705.1_ASM4474070v1_genomic.fna" = "GCF_044740705.1 - Staphylococcus nepalensis",
  "GCF_003579615.1_ASM357961v1_genomic.fna" = "GCF_003579615.1 - Staphylococcus nepalensis",
  "GCF_034030455.1_ASM3403045v1_genomic.fna" = "GCF_034030455.1 - Staphylococcus nepalensis",
  "GCF_041226205.1_ASM4122620v1_genomic.fna" = "GCF_041226205.1 - Staphylococcus nepalensis",
  "GCF_949841375.1_MGG03237_1680015469_genomic.fna" = "GCF_949841375.1 - Staphylococcus nepalensis",
  "GCF_910576055.1_MGBC000231_genomic.fna" = "GCF_910576055.1 - Staphylococcus nepalensis",
  "GCF_902362425.1_MGYG-HGUT-00025_genomic.fna" = "GCF_902362425.1 - Staphylococcus nepalensis",
  "GCF_910574155.1_MGBC000081_genomic.fna" = "GCF_910574155.1 - Staphylococcus nepalensis",
  "GCF_948995735.1_MGBC102935_genomic.fna" = "GCF_948995735.1 - Staphylococcus nepalensis",
  "GCF_949025715.1_MGBC102939_genomic.fna" = "GCF_949025715.1 - Staphylococcus nepalensis",
  "GCF_017368675.1_ASM1736867v1_genomic.fna" = "GCF_017368675.1 - Staphylococcus nepalensis",
  "GCF_003042875.1_ASM304287v1_genomic.fna" = "GCF_003042875.1 - Staphylococcus nepalensis",
  "GCF_017368735.1_ASM1736873v1_genomic.fna" = "GCF_017368735.1 - Staphylococcus nepalensis",
  "GCF_017368655.1_ASM1736865v1_genomic.fna" = "GCF_017368655.1 - Staphylococcus nepalensis",
  "GCF_017368705.1_ASM1736870v1_genomic.fna" = "GCF_017368705.1 - Staphylococcus nepalensis",
  "GCF_026626865.1_ASM2662686v1_genomic.fna" = "GCF_026626865.1 - Staphylococcus nepalensis",
  "GCF_001558775.1_ASM155877v1_genomic.fna" = "GCF_001558775.1 - Staphylococcus lugdunensis",
  "GCF_006094395.1_ASM609439v1_genomic.fna" = "GCF_006094395.1 - Staphylococcus haemolyticus",
  "GCF_006094375.1_ASM609437v1_genomic.fna" = "GCF_006094375.1 - Staphylococcus epidermidis",
  "GCF_000709415.1_ASM70941v1_genomic.fna" = "GCF_000709415.1 - Staphylococcus xylosus",
  "GCF_000010125.1_ASM1012v1_genomic.fna" = "GCF_000010125.1 - Staphylococcus saprophyticus",
  "GCF_029024945.1_ASM2902494v1_genomic.fna" = "GCF_029024945.1 - Staphylococcus succinus"
)

# Renomear as pontas da árvore com os novos rótulos
tree$tip.label <- tip_labels[tree$tip.label]

# Verificar os novos nomes das pontas
print("Novos nomes das pontas:")
print(tree$tip.label)

# Plotar a árvore com espaço expandido à direita
gg <- ggtree(tree, layout = "rectangular") +
  geom_tiplab(size = 8, offset = 0.01, hjust = 0) +  # Nomes das pontas com tamanho 5
  geom_treescale() +  # Adicionar escala
  #theme_tree2() +  # Adicionar espaço à direita
  xlim(0, 2)  # Expandir o espaço à direita (ajuste o valor 10 conforme necessário)

# Visualizar a árvore
gg

# Exportar a árvore como imagem
ggsave("staphy_tree.png", gg, width = 30, height = 20, dpi = 300)


#subplot
organismos_interesse <- c(
  "GCF_002442895.1 - Staphylococcus nepalensis",
  "GCF_002442935.1 - Staphylococcus nepalensis",
  "GCF_003097555.1 - Staphylococcus nepalensis",
  "GCF_003579615.1 - Staphylococcus nepalensis",
  "GCF_034030455.1 - Staphylococcus nepalensis",
  "GCF_041226205.1 - Staphylococcus nepalensis",
  "GCF_044740705.1 - Staphylococcus nepalensis",
  "GCF_902362425.1 - Staphylococcus nepalensis",
  "GCF_910574155.1 - Staphylococcus nepalensis",
  "GCF_910576055.1 - Staphylococcus nepalensis",
  "GCF_948995735.1 - Staphylococcus nepalensis",
  "GCF_949025715.1 - Staphylococcus nepalensis",
  "GCF_949841375.1 - Staphylococcus nepalensis",
  "GCF_017368675.1 - Staphylococcus nepalensis",
  "GCF_003042875.1 - Staphylococcus nepalensis",
  "GCF_017368735.1 - Staphylococcus nepalensis",
  "GCF_017368655.1 - Staphylococcus nepalensis",
  "GCF_017368705.1 - Staphylococcus nepalensis",
  "GCF_026626865.1 - Staphylococcus nepalensis",
  "am1e",  # am1e
  "am1d",  # am1d
  "colb"   # colb
)

# Carregar bibliotecas necessárias
#install.packages("ape")
library(ggtree)
library(ape)


# Suponha que "tree" seja o objeto da sua árvore filogenética

# Extrair a subárvore com base nas pontas de interesse
subarvore <- keep.tip(tree, organismos_interesse)

# Plotar a subárvore
gg_sub <- ggtree(subarvore) +
  geom_tiplab(size = 8, offset = 0.00001) +
  geom_treescale() +  # Rótulos das pontas
  xlim(0, 0.008)  # Expandir o eixo X até 2

# Visualizar a subárvore
gg_sub

ggsave("staphy_subtree_nepa.png", gg_sub, width = 30, height = 20, dpi = 300)



ggtree(subarvore) +
  geom_tiplab(size = 3, offset = 0.0001) +  # Rótulos das pontas
  xlim(0, 0.01)  # Expandir o eixo X até 2




# Ler o arquivo Newick
tree <- read.tree("chromo_tree.treefile")

# Vetor de correspondência manual Chromo!!!
tip_labels <- c(
    "GCF_029478115.1_ASM2947811v1_genomic.fna" = "GCF_029478115.1 - Chromohalobacter israelensis",
    "GCF_000055785.1_ASM5578v1_genomic.fna" = "GCF_000055785.1 - Chromohalobacter israelensis DSM 3043",
    "GCF_013395975.1_ASM1339597v1_genomic.fna" = "GCF_013395975.1 - Chromohalobacter israelensis",
    "GCF_030499135.1_ASM3049913v1_genomic.fna" = "GCF_030499135.1 - Chromohalobacter israelensis",
    "p4d.fna" = "p4d",
    "GCF_003182525.1_ASM318252v1_genomic.fna" = "GCF_003182525.1 - Chromohalobacter israelensis",
    "GCF_023061285.1_ASM2306128v1_genomic.fna" = "GCF_023061285.1 - Chromohalobacter nigrandesensis",
    "GCF_023061135.1_ASM2306113v1_genomic.fna" = "GCF_023061135.1 - Chromohalobacter sarecensis",
    "GCF_034479555.1_ASM3447955v1_genomic.fna" = "GCF_034479555.1 - Chromohalobacter canadensis",
    "GCF_023061175.1_ASM2306117v1_genomic.fna" = "GCF_023061175.1 - Chromohalobacter japonicus",
    "GCF_023061185.1_ASM2306118v1_genomic.fna" = "GCF_023061185.1 - Chromohalobacter beijerinckii",
    "GCF_023091865.1_ASM2309186v1_genomic.fna" = "GCF_023091865.1 - Chromohalobacter moromii",
    "GCF_004364315.1_ASM436431v1_genomic.fna" = "GCF_004364315.1 - Chromohalobacter marismortui",
    "GCF_004102695.1_ASM410269v1_genomic.fna" = "GCF_004102695.1 - Chromohalobacter israelensis",
    "GCF_020076335.1_ASM2007633v1_genomic.fna" = "GCF_020076335.1 - Chromohalobacter israelensis",
    "GCF_042433435.1_ASM4243343v1_genomic.fna" = "GCF_042433435.1 - Chromohalobacter israelensis",
    "GCF_000761475.1_ASM76147v1_genomic.fna" = "GCF_000761475.1 - Chromohalobacter israelensis"
)

# Renomear as pontas da árvore com os novos rótulos
tree$tip.label <- tip_labels[tree$tip.label]

# Verificar os novos nomes das pontas
print("Novos nomes das pontas:")
print(tree$tip.label)


#subplot
organismos_interesse <- c(
  "GCF_029478115.1 - Chromohalobacter israelensis",
  "GCF_000055785.1 - Chromohalobacter israelensis DSM 3043",
  "GCF_013395975.1 - Chromohalobacter israelensis",
  "GCF_030499135.1 - Chromohalobacter israelensis",
  "p4d",
  "GCF_003182525.1 - Chromohalobacter israelensis",
  "GCF_004102695.1 - Chromohalobacter israelensis",
  "GCF_020076335.1 - Chromohalobacter israelensis",
  "GCF_042433435.1 - Chromohalobacter israelensis",
  "GCF_000761475.1 - Chromohalobacter israelensis"
)

# Carregar bibliotecas necessárias
#install.packages("ape")
library(ggtree)
library(ape)


# Suponha que "tree" seja o objeto da sua árvore filogenética

# Extrair a subárvore com base nas pontas de interesse
subarvore <- keep.tip(tree, organismos_interesse)

# Plotar a subárvore
gg_sub <- ggtree(subarvore) +
  geom_tiplab(size = 8, offset = 0.0001) +
  geom_treescale() +  # Rótulos das pontas
  xlim(0, 0.05)  # Expandir o eixo X até 2

# Visualizar a subárvore
gg_sub

ggsave("chromo_subtree_isra.png", gg_sub, width = 30, height = 20, dpi = 300)

