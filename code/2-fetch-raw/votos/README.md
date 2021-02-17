## Módulo Votos

Para gerar a tabela de votos, siga as seguintes etapas:

## 1. Para a Câmara

1. Execute o script que processa os dados dos votos. Para isto execute o seguinte comando:

```
Rscript export_votos_camara.R --o <votos_camara_datapath> 
```

Com o seguinte argumento:

* `--o <votos_camara_datapath> `: Caminho para o arquivo onde o csv de parlamentares será salvo. O caminho default é "../../../data/raw/votos/votos_camara.csv".

Se preferir execute com os caminhos de saída default:

```
Rscript export_votos_camara.R
```

## 2. Para o Senado

1. Execute o script que processa os dados dos votos. Para isto execute o seguinte comando:

```
Rscript export_votos_senado.R --o <votos_senado_datapath> 
```

Com o seguinte argumento:

* `--o <votos_senado_datapath> `: Caminho para o arquivo onde o csv de votos será salvo. O caminho default é "../../../data/raw/votos/votos_senado.csv".

Se preferir execute com os caminhos de saída default:

```
Rscript export_votos_senado.R
```
