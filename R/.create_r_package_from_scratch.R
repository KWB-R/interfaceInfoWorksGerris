# repo_dir wurde erstellt, indem das Paket zuerst online bei github angelegt
# wurde und dann anschließend in R-Studio als "New Projekt/version Control/Git"
# lokal gespeichert wurde

# Paket erstellt mit den KWB-Paket "pakgbuild" und den Hinweisen von
# "https://kwb-r.github.io/kwb.pkgbuild/articles/tutorial.html"

package <- "kwb.infoworks"
repo_dir <- "C:/R_packages"

# Set the path to the package directory
pkg_dir <- file.path(repo_dir, package)

# die folgenden Punkte sind nicht notwendig, da sie mit R-Studio durchgeführt
# wurden
# ------------------------------------------------------------------------------
# Create directory for R package
kwb.pkgbuild::create_pkg_dir(pkg_dir)
#> /var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T//Rtmpi8EON0/kwb.newpackage is a valid 'root_dir' for pkg 'kwb.newpackage'
#> [1] "/var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T//Rtmpi8EON0/kwb.newpackage"
# ------------------------------------------------------------------------------
# Create a default package structure
withr::with_dir(pkg_dir, {kwb.pkgbuild::use_pkg_skeleton(package)})




author <- list(name = "Malte Zamzow",
               orcid = "0000-0002-8748-038X")

description <- list(
  name = package,
  title = "Interface from Infoworks to Gerris",
  desc  = "This package transforms the output files from InfoWorks (Mass flow data) into a readable format for the Gerris environment.The integrated outlet IDs are tailored to Berlin."
)

setwd(pkg_dir)

kwb.pkgbuild::use_pkg(
  author,
  description,
  version = "0.0.0.9000",
  stage = "experimental"
)



# -------------------
remotes::install_github("kwb-r/kwb.pkgbuild", upgrade = TRUE, force = TRUE)

usethis::create_package(".")
fs::file_delete(path = "DESCRIPTION")

author <- list(name = "Malte Zamzow",
               orcid = "0000-0002-8748-038X")

pkg <- list(
  name = "kwb.infoworks",
  title = "Interface from Infoworks to Gerris",
  desc  = "This package transforms the output files from InfoWorks (Mass flow data) into a readable format for the Gerris environment.The integrated outlet IDs are tailored to Berlin."
)

kwb.pkgbuild::use_pkg(author,
                      pkg,
                      version = "0.0.0.9000",
                      stage = "experimental")


usethis::use_vignette("tutorial")

usethis::use_pipe()
### R functions
if(FALSE) {
  ## add your dependencies (-> updates: DESCRIPTION)
  pkg_dependencies <- c('swmmr', 'reticulate')

  sapply(pkg_dependencies, usethis::use_package)

  desc::desc_add_remotes("kwb-r/kwb.utils",normalize = TRUE)
  usethis::use_pipe()
}

kwb.pkgbuild::create_empty_branch_ghpages("kwb.swmm")
# ---------------------

# erstellen einer Funktion mit Dokumentation
usethis::use_r("01_load_infoworks_output")
# erstellen einer zugeh?rigen Testdatei
usethis::use_test("01_load_infoworks_output")
# Daten hinzuf?gen: "internal" --> nicht aufrufbar (nur im Hintergrund), keine Dokumentation notwendig
usethis::use_data(pct, oct, internal = TRUE, overwrite = T)
# Rohdaten dazu am besten im Ordner "data-raw" speichern


devtools::document()
