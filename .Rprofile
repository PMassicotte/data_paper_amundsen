if (Sys.info()[['sysname']] %in% c('Linux', 'Windows')) {
  options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/__linux__/focal/latest"))
} else {
  ## For Mac users, we'll default to installing from CRAN/MRAN instead, since
  ## RSPM does not yet support Mac binaries.
  options(repos = c(CRAN = "https://cran.rstudio.com/"))
  # options(renv.config.mran.enabled = TRUE) ## TRUE by default
}
options(renv.config.repos.override = getOption("repos"))

source("renv/activate.R")
