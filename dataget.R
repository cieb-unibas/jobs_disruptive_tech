# loading packages:
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

# retrieving results from JPOD:
jpodRetrieve <- function(jpod_conn, sql_statement){
  res <- dbSendQuery(conn = jpod_conn, statement = sql_statement)
  df <- dbFetch(res)
  dbClearResult(res)
  return(df)
}


ch_total_test <- function(){
  JPOD_QUERY <- "
    SELECT COUNT(*) as total_postings, nuts_2, 'nuts2' as level
    FROM position_characteristics 
    GROUP BY nuts_2
    "
  total_postings <- jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERY)
  CH_Total <- total_postings %>% filter(level == "nuts2") %>% pull(total_postings) %>% sum()
  if(CH_Total != 3211219){
    stop("Total number of calculated postings does not correspond to (known) number of postings in JPOD.")
  }else{
    print("Test for total number of postings in CH successfull.")
  }
}
