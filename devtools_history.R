# Packages et fonctions utilisées
usethis::use_package("stats")
usethis::use_package("survminer")
usethis::use_package("utils") # Utile uniquement pour les fonctions de select

# Ignorer ce document
usethis::use_build_ignore("devtools_history.R")
usethis::use_git_ignore("devtools_history.R")

# Ignorer le .Rproj
usethis::use_git_ignore("YPJ.Rproj")
