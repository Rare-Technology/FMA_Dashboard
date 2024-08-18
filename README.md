# Fisheries Management Assessment (FMA) Dashboard

The FMA Dashboard is an R package called `{ rarefma }`

The portal can be accessed through https://portal.rare.org/en/tools-and-data/fma/

The current application sits in shinyapps.io under the ID "12308687" named "FMA_Dash_test"

### Installing for Development & Updating the data

***It is recommended to use RStudio for development and deployment purposes!***

Key steps to follow to update the the existing data and application.

1. Clone the repository to your desired location/folder.
    * You can either use git clone or download the repository from shinyapps.io
    * ! Recommended to download the repository from shinyapps.io !
2. Make sure you have `{ devtools }` installed
3. Load the package using CMD-SHIFT-L or `devtools::load_all()` (Command specific to RStudio) - this should install `rarefma` library to the R environment
4. To update the follow the steps mentioned in "Generating Updated data" below.
5. Once the data is updated in the `data` folder, test the app locally using the `run_app()` command. The app should launch on a separate window or on localhost (usually on http://127.0.0.1:5765)
    * Note, do not use `runApp()` use `run_app()`
    * This is only to test the application in development (local environment)

#### Generating updated data

There are scripts in the `data-raw` folder to create the underlying data. 

- The main one is `prepare-ourfish-data.R`

    * Info: The `prepare-ourfish-data.R` script utilizes the `prepare-historical-data.R` script.

Key Steps to be 

##### Data requirements

The current process of updating the data in the Fisheries Management (FMA) Dashboard uses *static datasets* downloaded from the respective data source.

- The `prepare-ourfish-data.R` script uses the `join_ourfish_footprint.csv` dataset (This is the only data that needs to be updated for the FMA dashboard to update the data)

    The dataset `join_ourfish_footprint.csv` is available on data.world -> https://data.world/rare/ourfish/workspace/file?filename=join_ourfish_footprint

    Key Steps:

    1. Download the `join_ourfish_footprint.csv` file (referred to as "data file" from here on") from data.world using the above link and save the data file in `data-raw` folder.
    2. Run the complete `prepare-ourfish-data.R` script
        This prepares the data and uploads the updated data to the `data` folder. The application reads the updated data from this folder.
    3. 


### Deploying the Shiny app to shinyapps.io

#### Prerequisities

Since this is a private repository, the user will need an auth_token from https://github.com/settings/tokens.

#### Recommended Steps for deploying the app

1. Restart the R session (recommended before deploying the application)
2. Use the command `remotes::install_github("Rare-Technology/FMA_Dashboard", auth_token = 'GITHUB_AUTH_TOKEN')`
3. if the devtools has been installed then use command `devtools::load_all()` -> This loads the rarefma library in the environment.
4. Prior to deployment Check the package before sending to production using: `devtools::check()`, followed by `golem::add_shinyappsio_file()`
    The golem::add_shinyappsio_file() command creates the `FMA_Dash_test.dcf` file in the rsconnect folder (Always check if it is present before deploying the app)
5. Make sure the library or package `rfishbase` is uninstalled from the environment 
    
    This doesn't let the app deploy to shinyapps.io

6. Open the `app.R` file and,
    
    Use the blue button on top of the file in RStudio, enter the name of the app "FMA_Dash_test" (Recommended!)

    OR

    Use command `rsconnect::deployApp()` in the RStudio console -> Use this only if the rsconnect folder is present in the repository (This throws an error at times)

        In case the `rsconnect` folder is not present in the repository use command golem::add_shinyappsio_file()

#### Potential Errors during deployment

1. `Connection was reset` errors - try deploying the app again (This issue is with the connection to shinyapps.io) and not the script
2. If the library `rfishbase` is present, the deployment never ends -> The process has to be force stopped in this case and the app needs to be redeployed again after unsinstalling the library.
3. If the rarefma library is not installed prior to deployment, the app doesn't work.