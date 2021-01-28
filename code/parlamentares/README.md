## Módulo Parlamentares

Para gerar a tabela de parlamentares, siga as seguintes etapas:

1. Execute o script que processa os dados dos parlamentares. Para isto execute o seguinte comando:

```
Rscript export_parlamentares.R --o <parlamentares_datapath> 
```

Com o seguinte argumento:

* `--o <parlamentares_datapath> `: Caminho para o arquivo onde o csv de parlamentares será salvo. O caminho default é "../../data/raw/parlamentares/parlamentares.csv".

Se preferir execute com os caminhos de saída default:

```
Rscript export_parlamentares.R
```
