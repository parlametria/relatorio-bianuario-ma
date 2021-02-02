# Como usar o parlametria para processar os dados de proposições

Este é um tutorial básico para usar o Parlametria para capturar, processar e exportar os dados relacionados a um conjunto de proposições de interesse. 

Neste tutorial será possível executar e gerar os dados que o Parlametria utiliza para criar os paineis de análise das proposições do Congresso.

## Instalação de dependências
Você precisará instalar o [docker](https://docs.docker.com/install/linux/docker-ce/ubuntu/#install-docker-ce) e o [docker-compose](https://docs.docker.com/compose/install/) para a execução do processamento.

Com tudo instalado, vamos prosseguir.

## Passo 1.1

O processamento dos dados do Parlametria assumem como entrada uma lista de proposições de interesse. Essa lista deve ser um CSV no formato dessa [planilha](https://docs.google.com/spreadsheets/d/1ErZBSZ6WlgUAQKfr5-X52JSdnbWgWQ9VQg3juh5WQEo/edit?usp=sharing).

As colunas `id_camara` e `id_senado` precisam estar preenchidas com os identificadores das proposições nas suas respectivas casas.

Neste passo você deve gerar ou atualizar uma lista de proposições neste formato. Essa lista será usada em passos seguintes.

Já existe uma lista processada com proposições de interesse para análise nesse repositório. Este csv está disponível em `data/inputs/3-transform-input/proposicoes/proposicoes_input.csv`. Ele é resultado da transformação contida no script `code/1-inputs/3-transform-input.R`. Esse csv será usado no passo 1.3.

## Passo 1.2

Baixe os repositórios [leggo-geral](https://github.com/parlametria/leggo-geral) e do [leggoR](https://github.com/parlametria/leggor). 

O repositório [rcongresso](https://github.com/analytics-ufcg/rcongresso) deve ser clonado dentro da pasta do leggoR para ser usado durante o processamento.

O **leggoR** é reponsável por empacotar as funções que capturam e processam os dados de proposições e das entidades envolvidas no processo (parlamentares, comissões).

O **leggo-geral** é responsável por orquestrar os diferentes módulos do processamento e executá-los de maneira coordenada.

## Passo 1.3

Baixe o CSV `proposicoes_input.csv` descrito no passo 1.1 e salve em `inst/extdata/proposicoes_local.csv` do leggoR

Crie um arquivo CSV em `inst/extdata/interesses_local.csv` com o conteúdo

```csv
interesse,url,nome,descricao
painel,./inst/extdata/proposicoes_local.csv,Proposições,Lista de proposições
```

## Passo 1.4

Crie um arquivo .env na raiz do repositório **leggo-geral** com o seguinte conteúdo:

```
URL_INTERESSES=./inst/extdata/interesses_local.csv
PLS_FILEPATH=./leggo_data/pls_interesses.csv
WORKSPACE_FOLDERPATH=../
EXPORT_FOLDERPATH=./leggo_data
LEGGOR_FOLDERPATH=./leggoR
LEGGOTRENDS_FOLDERPATH=./leggoTrends
VERSOESPROPS_FOLDERPATH=./versoes-de-proposicoes
LEGGOCONTENT_FOLDERPATH=./leggo-content
LOG_FOLDERPATH=./logs/
BACKUP_FOLDERPATH=./backups/
PROD_BACK_APP=production_app_name
DEV_BACK_APP=production_app_name
URL_LISTA_ANOTACOES=
```

## Passo 1.5

Realize o build do serviço de execução do processamento via docker.

Na raiz do leggo-geral, execute:
```
./update_leggo_data.sh -build-leggor
```

## Passo 1.6

Inicie o processamento dos dados para as proposições de interesse:

```
./update_leggo_data.sh -run-basic-pipeline
```

## Passo 1.7

Salve os CSV's processados.

Use o comando de gerar backup para exportar a pasta com os csvs processados pelo leggoR.

```
./update_leggo_data.sh -generate-backup
```

Note que a pasta com o csvs foi copiada para a pasta `./backup`, que está no mesmo nível do diretório que contém os repositórios do leggoR e do leggo-geral.

Copie os csvs para o diretório `data/raw/leggo_data`

Pronto! O processamento dos csvs do módulo de proposições usando o Parlametria foi concluído.


# Como usar o parlametria para processar os dados do Twitter

## Passo 2.1

Baixe o repositório [leggo-twitter-dados](https://github.com/parlametria/leggo-twitter-dados).

O **leggo-twitter-dados** é responsável por empacotar as funções que capturam e processam os dados de tweets dos parlamentares que falaram sobre proposições.

## Passo 2.2

De dentro do repositório do leggo-geral (baixado no passo 1.2) levante os serviços:

```sh
python3.8 compose/run twitter-dados up
```

Caso você não tenha o `python3.8` instalado é possível usar algum outro `python3.x` .

## Passo 2.3

Copie o csv de proposições de entrada para o repositório do leggo-twitter-dados em `data/proposicoes_input.csv`.

## Passo 2.4

Baixe o [zip](https://drive.google.com/file/d/1q0lW1vFrfEppgMG-wGmaxRhnB1JRLyVR/view) dos tweets dos parlamentares na legislatura atual para o diretório `data/tweets/` no repositório do leggo-twitter-dados;

## Passo 2.6

Execute o comando de exportação dos dados brutos
```sh
docker exec -it r-leggo-twitter bash -c "Rscript /leggo-twitter-dados/code/export_data.R -p data/proposicoes_input.csv"
```

Execute o comando de normalização dos dados
```sh
make r-export-data-db-format
```

Os dados processados estarão em `data/bd` dentro do repositório do leggo-twitter-dados.

Como os dados de tweets são grandes demais para o github. Recomenda-se subir num drive, para que o download seja feito conforme a necessidade.

Os dados gerados para o conjunto de proposições deste repositório está disponível [aqui](https://drive.google.com/drive/folders/197N0XPbo6JwMxeUBy527botZIB1MJ6AU?usp=sharing) com o nome **leggo_data_twitter_reacoes_legislativo.zip**.

Baixe o zip e extraia para a pasta `data/raw/leggo_twitter` neste repositório.
