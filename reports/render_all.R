files <- list.files(here::here("reports"), pattern = "[.]Rmd$")

for (f in files)
  rmarkdown::render(here::here("reports", f), output_dir = here::here("docs"))
