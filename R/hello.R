library("rstantools")
use_rstan(pkgdir = '/Users/javierrojas/Documents/GitHub/OneWayBY', license = TRUE, auto_config = TRUE)
setwd('/Users/javierrojas/Documents/GitHub/OneWayBY')
list.files(all.files = TRUE)
file.show("DESCRIPTION")
file.show("Read-and-delete-me")
file.show(file.path("R", "OneWayBY-package.R"))
example(source)
try(roxygen2::roxygenize(load_code = "/Users/javierrojas/Documents/GitHub/OneWayBY/R/Example.R"), silent = TRUE)
pkgbuild::compile_dll()
roxygen2::roxygenize()
rstan_create_package()

rstan_create_package(
  "/Users/javierrojas/Documents/GitHub/AnovaBYow",
  fields = NULL,
  rstudio = TRUE,
  open = TRUE,
  stan_files = character(),
  roxygen = TRUE,
  travis = TRUE,
  license = TRUE,
  auto_config = TRUE
)
