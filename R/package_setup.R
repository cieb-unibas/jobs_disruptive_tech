package_setup <- function(packages){
  pkgs_available <- packages %in% installed.packages()
  for(p in seq(packages)){
    if(pkgs_available[p] == TRUE){
      library(packages[p], character.only = TRUE)
      print(paste("Package", packages[p], "loaded."))
    }else{
      install.packages(packages[p])
      library(packages[p], character.only = TRUE)
      print(paste("Package", packages[p], "installed and loaded."))
    }
  }
}