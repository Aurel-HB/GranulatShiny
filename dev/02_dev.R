# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
#usethis::use_package( "thinkr" )
usethis::use_package( "dplyr" ) #
usethis::use_package( "tidyr" ) #
usethis::use_package( "stringr" ) #
usethis::use_package( "vegan" ) #
usethis::use_package( "shinydashboard" ) #
usethis::use_package( "shinyBS" ) #
usethis::use_package( "shiny" ) #
usethis::use_package( "shinyWidgets" ) #
usethis::use_package( "assertthat" ) #
usethis::use_package( "leaflet" ) #
usethis::use_package( "ggplot2" ) #
usethis::use_package( "lme4" ) #
usethis::use_package( "car" ) #
usethis::use_package( "stats" ) #
usethis::use_package( "MASS" ) #
usethis::use_package( "DHARMa" ) #
usethis::use_package( "multcomp" ) #
usethis::use_package( "multcompView" ) #
usethis::use_package( "lsmeans" ) #
usethis::use_package( "RColorBrewer" ) #
usethis::use_package( "plyr" ) #
usethis::use_package( "sf" ) #
usethis::use_package( "shinycssloaders" ) #
usethis::use_package( "spelling" )
usethis::use_package( "cowplot" ) #
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "Import_data" )
golem::add_module( name = "Ref" )
golem::add_module( name = "Impact" )
golem::add_module( name = "Map" )
golem::add_module( name = "Prepare_data" )
golem::add_module( name = "Tables" )
golem::add_module( name = "Show_tables" )
golem::add_module( name = "Stat" )
golem::add_module( name = "Analyse_choice")
golem::add_module( name = "Choix_loi")
golem::add_module( name = "Prepare_modelling" )
golem::add_module( name = "Modelling" )
golem::add_module( name = "Representation")
golem::add_module( name = "Puissance")
golem::add_module( name = "reception")
golem::add_module( name = "Indicator")
golem::add_module( name = "Structure")

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "traitement_maker" )
golem::add_fct( "traitement_maker_zones" )
golem::add_fct( "tutti_function_traitement" )
golem::add_fct( "indice_computing" )
golem::add_fct( "outlier_remove" )
golem::add_fct( "table_shapping" )
golem::add_fct( "global_function" )
golem::add_fct( "indice_computing" )
golem::add_fct("leaflet_maker")
golem::add_fct("pre_leaflet")
golem::add_fct("ecriture_modele")
golem::add_fct("ecriture_modele_log")
golem::add_fct("glmm_maker")
golem::add_fct("glm_maker")
golem::add_fct("permanova_maker")
golem::add_fct("puissance_maker")
golem::add_fct("glmm_maker_puissance")
golem::add_fct("glm_maker_puissance")
golem::add_fct("numeric_summary")
golem::add_fct("verification")
golem::add_fct("probability_distribution")
golem::add_fct("dataset_creation")
golem::add_fct("export_data")
golem::add_fct("diversity_table")
golem::add_fct("lineplot_creation")
golem::add_fct("structure_table")
golem::add_fct("summary_report")
golem::add_fct("text_list_translator")


## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "fonctions" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package

# this command are to remind that you to first import in your r environment
# to then save as data in the app ####
#catch <- readRDS("catch.rds")
#operation <- readRDS("operation.rds")
#polygon <- readRDS("polygon.rds")
#sauvegarde <- readRDS("sauvegarde.rds")
#list_translate <- readRDS("list_translate.rds")
# Read the content of the text file
#intro_content <- readLines("message_intro_fr.txt")
# Join the lines into a single string
#intro_text <- paste(intro_content, collapse = "\n")

usethis::use_data(catch)
usethis::use_data(operation)
usethis::use_data(polygon)
usethis::use_data(sauvegarde)
usethis::use_data(list_translate)#overwrite = T
usethis::use_data(intro_text_fr)
usethis::use_data(intro_text_en)
file.edit()

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("GranulatShiny")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

