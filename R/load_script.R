library(rjson)

load_script <- function(sel_language) {
  www <- app_sys("app/www")
  json_path <- paste(www, "/", sel_language, ".json", sep = "")
  return(fromJSON(file = json_path))
}