## Módulo Proposições

Para gerar a tabela com as proposições apresentadas sobre Meio Ambiente em 2019 e 2020, siga as seguintes etapas:

1. Execute o script que processa os dados das proposições. Para isto execute o seguinte comando:

```
Rscript export_proposicoes.R --o <proposicoes_datapath> 
```

Com o seguinte argumento:

* `--o <proposicoes_datapath> `: Caminho para o arquivo onde o csv de proposições será salvo. O caminho default é "../../data/raw/proposicoes/proposicoes.csv".

Se preferir execute com os caminhos de saída default:

```
Rscript export_proposicoes.R
```
