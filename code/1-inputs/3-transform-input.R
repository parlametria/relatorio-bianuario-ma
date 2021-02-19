library(tidyverse)
library(here)
library(futile.logger)

source(here::here("code/1-inputs/transform-input-props.R"))
source(here::here("code/1-inputs/transform-input-votos.R"))

flog.threshold(TRACE)
transform_input_proposicoes()
transform_input_votos()
