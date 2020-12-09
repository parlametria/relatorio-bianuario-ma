## Geração das tabelas relacionadas às Comissões

As tabelas de comissões possuem informações referentes a Comissões e a composição dessas Comissões (quais os parlamentares que são membros).

Para gerar esta tabela, siga as seguintes etapas:

1. Execute o script que processa os dados de Comissões. Para isto **execute do mesmo diretório deste readme** o seguinte comando:

```
Rscript export_comissoes.R --o <composicao_composicoes_folderpath> 
```

Com o seguinte argumento:

* `--o <composicao_composicoes_folderpath> `: Caminho para o diretório onde os csvs de de comissões e composições das comissões serão salvos. O caminho default é "../../data/comissoes/"

Se preferir execute com os caminhos de saída default:

```
Rscript export_comissoes.R
```
