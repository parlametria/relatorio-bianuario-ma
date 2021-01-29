## Módulo Proposições

Para gerar a tabela com as proposições apresentadas sobre Meio Ambiente em 2019 e 2020, siga as seguintes etapas:

### Para o Senado

1. Execute o script que processa os dados das proposições. Para isto execute o seguinte comando:

```
Rscript export_proposicoes_senado.R --o <proposicoes_datapath>
```

Com o seguinte argumento:

* `--o <proposicoes_datapath> `: Caminho para o arquivo onde o csv de proposições será salvo. O caminho default é "../../../data/1-inputs/1-create-pre-input/proposicoes/proposicoes_senado.csv".

Se preferir execute com os caminhos de saída default:

```
Rscript export_proposicoes_senado.R
```

### Para a Câmara

1. Execute o script que processa os dados das proposições. Para isto execute o seguinte comando:

```
Rscript export_proposicoes_camara.R --o <proposicoes_datapath>
```

Com o seguinte argumento:

* `--o <proposicoes_datapath> `: Caminho para o arquivo onde o csv de proposições será salvo. O caminho default é "../../../data/1-inputs/1-create-pre-input/proposicoes/proposicoes_camara.csv".

Se preferir execute com os caminhos de saída default:

```
Rscript export_proposicoes_camara.R
```