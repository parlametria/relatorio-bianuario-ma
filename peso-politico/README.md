## Módulo Peso Político

Para gerar a tabela com o peso político dos parlamentares, siga as seguintes etapas:

1. Execute o script que baixa esses dados. Para isto execute o seguinte comando:

```
Rscript export_peso_politico.R --o <peso_datapath> 
```

Com o seguinte argumento:

* `--o <peso_datapath> `: Caminho para o arquivo onde o csv com os dados de pesos políticos será salvo. O caminho default é "../../data/raw/peso_politico/peso_politico.csv".

Se preferir execute com os caminhos de saída default:

```
Rscript export_peso_politico.R
```
