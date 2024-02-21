# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## the app have been develop with R 4.3.2

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!/ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!/
##
golem::fill_desc(
  pkg_name = "GranulatShiny", # The Name of the package containing the App
  pkg_title = "Interface Granulat", # The Title of the package containing the App
  pkg_description = "PKG_DESC.", # The Description of the package containing the App
  author_first_name = "X", # Your First Name
  author_last_name = "X", # Your Last Name
  author_email = "X", # Your Email
  repo_url = NULL # The URL of the GitHub Repo (optional)
)

desc::desc_set_authors(c(person(given = "Aurel",
                                family = "Hebert--Burggraeve",
                                role = c("aut"),
                                email = "aurelhb722@gmail.com",
                                comment = c(ORCID = "0009-0000-3031-4340")),
                         person(given = "Mathis",
                                family = "Cambreling",
                                role = "aut"),
                         person(given = "Jehanne",
                                family = "Rivet",
                                role = "aut"),
                         person(given = "Laure",
                                family = "Simplet",
                                role = "ctb"),
                         person(given = "Vincent",
                                family = "Badts",
                                role = "ctb"),
                         person(given = "Laurent",
                                family = "Dubroca",
                                role = "aut",
                                email = "Laurent.Dubroca@ifremer.fr"),
                         person(given = "Camille",
                                family = "Vogel",
                                role = "aut",
                                email = "camille.vogel@ifremer.fr")))

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license( "CeCILL-C" )  # You can set another license here
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
usethis::use_news_md( open = FALSE )

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()

## Favicon ----
# If you want to change the favicon (default is golem's one)
#golem::use_favicon("C:/Users/ahebertb/Downloads/favicon.ico", method = "curl")
#golem::use_favicon("C:/Users/ahebertb/Downloads/hex-GranulatShiny.png", method = "curl")
#golem::remove_favicon(path = "inst/app/www/favicon.ico")


## Add helper functions ----
golem::use_utils_ui()
golem::use_utils_server()

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

