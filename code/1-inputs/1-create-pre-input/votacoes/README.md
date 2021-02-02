## Módulo Votações

Para gerar a tabela com as votações das proposições sobre Meio Ambiente em 2019 e 2020 ocorridas em plenário, siga as seguintes etapas:

### Para o Senado

1. Execute o script que processa os dados das votações de plenário no Senado. Para isto execute o seguinte comando:

```
Rscript export_votacoes_senado.R -o <votacoes_datapath>
```

Com o seguinte argumento:

* `-o <votacoes_datapath> `: Caminho para o arquivo onde o csv de votações será salvo. O caminho default é "../../../data/inputs/1-create-pre-input/votacoes/votacoes_senado.csv".

Se preferir execute com os caminhos de saída default:

```
Rscript export_votacoes_senado.R
```

### Para a Câmara

1. Execute o script que processa os dados das votações de plenário na Câmara. Para isto execute o seguinte comando:

```
Rscript export_votacoes_camara.R -o <votacoes_datapath>
```

Com o seguinte argumento:

* `-o <votacoes_datapath> `: Caminho para o arquivo onde o csv de proposições será salvo. O caminho default é "../../../data/inputs/1-create-pre-input/votacoes/votacoes_camara.csv".

Se preferir execute com os caminhos de saída default:

```
Rscript export_votacoes_camara.R
```