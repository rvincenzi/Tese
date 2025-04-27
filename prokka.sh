#!/bin/bash


# Verificar se a variável de ambiente INPUT_DIR está definida
if [ -z "$INPUT_DIR" ]; then
  echo "A variável de ambiente INPUT_DIR não está definida!"
  exit 1
fi

# Diretório de saída
OUTDIR="resultados_prokka"

# Verificar se a pasta de saída existe, se não, criar
mkdir -p $OUTDIR

# Rodar Prokka para todos os arquivos .fasta no diretório de entrada
for GENOME in $INPUT_DIR/*.fasta; do
    # Verificar se o arquivo de genoma existe
    if [ ! -f "$GENOME" ]; then
        echo "Arquivo $GENOME não encontrado!"
        continue
    fi

    # Definir o prefixo com o nome do arquivo genômico sem a extensão
    PREFIX=$(basename $GENOME .fasta)

    # Rodar o Prokka para o genoma atual
    prokka --outdir $OUTDIR/$PREFIX --prefix $PREFIX --cpus 4 $GENOME
done
