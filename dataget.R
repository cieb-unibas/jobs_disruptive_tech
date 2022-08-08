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

# creating a SQL-LIKE statement based on a selection of keywords:
sql_like_statement <- function(keywords, matching_var){
  keywords <- paste0("'%", keywords, "%'")
  if(length(keywords) > 1){
    like_statement <- paste(keywords, collapse = paste0(" OR ", matching_var, " LIKE "))
    like_statement <- paste(matching_var, "LIKE", like_statement)
  }else{
    like_statement <- paste(matching_var, "LIKE", keywords)
  }
  return(like_statement)
}

# retrieving information for job postings that contain certain keywords
keyword_jpod_query <- function(keywords, 
                               matching_var = "job_description", 
                               output_vars = c("uniq_id", "company_name", 
                                               "city", "nuts_2", "nuts_3")){
  
  output_vars <- paste(paste0("pc.", output_vars), collapse = ", ")
  like_statement <- sql_like_statement(keywords = keywords, matching_var = matching_var)
  
  jpod_query <- paste0("
  SELECT ", output_vars, "
  FROM (
    SELECT *
    FROM (
      SELECT uniq_id, lower(", matching_var, ") as ", matching_var, "
      FROM job_postings
      ) jp
    WHERE (", like_statement, ")) jp
    LEFT JOIN position_characteristics pc ON pc.uniq_id = jp.uniq_id")
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
