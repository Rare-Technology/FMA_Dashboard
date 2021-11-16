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
usethis::use_package( "dplyr" )
usethis::use_package( "DT" )
usethis::use_package("shinyWidgets")
usethis::use_package("shinyjs")
usethis::use_package("lubridate")
usethis::use_package("rjson")
usethis::use_pipe()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "home" ) # Name of the module
golem::add_module( name = "fisheries_assessment" ) # Name of the module
golem::add_module( name = "side" )
golem::add_module( name = "side_geography" )
golem::add_module( name = "side_assessment" )
golem::add_module( name = "main_data" )
## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "geo" )
golem::add_utils( "species" )
golem::add_utils( "state" )
golem::add_utils( "data" )
golem::add_utils( 'table')
## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "main" )
golem::use_external_js_file('https://cdn.jsdelivr.net/npm/intro.js@4.2.2/minified/intro.min.js')
golem::use_external_css_file('https://cdn.jsdelivr.net/npm/intro.js@4.2.2/minified/introjs.min.css')
golem::add_js_file('tour')
## Add internal datasets ----
## If you have data in your package


## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("rarefma")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

