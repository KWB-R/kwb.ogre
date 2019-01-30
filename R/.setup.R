# devtools::install_github("KWB-R/kwb.orcid")
#
# remotes::install_github("KWB-R/kwb.pkgbuild", build = TRUE, build_opts = c(
#   "--no-resave-data", "--no-manual"
# ))

package <- "kwb.ogre"

desc::desc_get_field("Title")

# Delete the original DESCRIPTION file
fs::file_delete(path = file.path(package_dir, "DESCRIPTION"))

author <- list(
  name = "Hauke Sonnenberg",
  orcid = kwb.orcid::get_kwb_orcids()["Hauke Sonnenberg"],
  url = "https://github.com/hsonne"
)


desc_file <- "..DESCRIPTION"

deps <- list(imports = desc::desc_get_field("Imports",
                                            file = desc_file)#, 
            # depends = desc::desc_get_field("Depends",
            #                                file = desc_file), 
            # suggests = desc::desc_get_field("Suggests",
            #                                file = desc_file)
            )
stringr::str_match_all(deps$imports, pattern = "^kwb")
description <- list(
  name = package,
  title = desc::desc_get_field("Title"),
  desc  = desc::desc_get_field("Description")
)

desc::desc_set("Imports" = deps$imports)

deps$remotes <- stringr::str_split(deps$imports, ", ", simplify = TRUE) %>% 
  as.character() %>% stringr::str_subset("^kwb")
desc::desc_add_remotes(stringr::str_c("github::", deps$remotes))

setwd(package_dir)

kwb.pkgbuild::use_pkg(
  author,
  description,
  version = "0.1.0.9000",
  stage = "experimental"
)

usethis::use_r("function")

pkg_dependencies <- c("digest", "kwb.fakin", "kwb.utils", "yaml")

sapply(pkg_dependencies, usethis::use_package)

# And now, let's do the first commit and upload everything to GitHub
# (manually)...