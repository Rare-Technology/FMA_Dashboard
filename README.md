# Fisheries Management Assessment (FMA) Dashboard

The FMA Dashboard is an R package called `{ rarefma }`

### To install for development

1. Make sure you have `{ devtools }` installed
2. Use git to pull the repository
3. In RStudio you can use CMD-SHIFT-L or `devtools::load_all()` to load the package
4. Then use `run_app()` to launch the app (note, do not use `runApp()` use `run_app()`)


### To install for use

Since this is a private repository, the user will need an auth_token from https://github.com/settings/tokens.

remotes::install_github("Rare-Technology/FMA_Dashboard", auth_token = '')
