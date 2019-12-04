# Packages et fonctions utilisées
usethis::use_package("stats")
usethis::use_package("survival")
usethis::use_package("survminer")
usethis::use_package("utils") # Utile uniquement pour les fonctions de select

# Ignorer ces documents
usethis::use_build_ignore("devtools_history.R")
usethis::use_git_ignore("devtools_history.R")

usethis::use_build_ignore("workflow.R")
usethis::use_git_ignore("workflow.R")

usethis::use_build_ignore("cran-comments.md")

# Ignorer le .Rproj
usethis::use_git_ignore("YPJ.Rproj")

# Utiliser Travis
usethis::use_travis()

# Initialiser une vignette
usethis::use_vignette("manual")

# Initialiser tests
usethis::use_testthat()

# Initialiser raw data
usethis::use_data_raw()

# Checker sur win builder le package
devtools::check_win_devel()

release_questions <- function() {
  c(
    "Have you knitted Readme.rmd ?"
  )
}
