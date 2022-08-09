source("jpod_bloom/dataget.R")

#### load packages
package_setup(packages = c("RSQLite", "DBI", "tidyverse"))

#### load data
bloom_tech_keywords <- read.csv(
  "/scicore/home/weder/GROUP/Innovation/01_patent_data/raw data/new_keywords/key_words_tech.csv",
  sep = ";", header = FALSE, stringsAsFactors = FALSE
)
colnames(bloom_tech_keywords) <- c("id", "key_word")
bloom_tech_names <- c("3d printing",
                      "Autonomous Cars",
                      "Bispecific monoclonal antibody", 
                      "Cloud computing", 
                      "Computer vision", 
                      "Drug conjugates",
                      "Electronic gaming",
                      "Millimeter wave",
                      "Fingerprint sensor",
                      "Fracking", 
                      "GPS", 
                      "Hybrid vehicle electric car", 
                      "Lane departure warning",
                      "Lithium battery", 
                      "Machine Learning AI", 
                      "Mobile payment", 
                      "Oled display", 
                      "Online streaming", 
                      "Rfid tags", 
                      "Search Engine", 
                      "Smart devices",
                      "Social Networking", 
                      "Software defined radio",
                      "Solar Power", 
                      "Stent graft", 
                      "Touch screen", 
                      "Virtual Reality", 
                      "Wifi", 
                      "Wireless charging")
bloom_tech_keywords <- bloom_tech_keywords %>% 
  merge(data.frame(id = seq(1, 29, 1), tech_field = bloom_tech_names), by = "id") %>%
  mutate(key_word, key_word = str_replace_all(key_word, "\\s+", " ")) %>% 
  mutate(key_word = tolower(key_word))

#### connect do JPOD
DB_DIR <- "/scicore/home/weder/GROUP/Innovation/05_job_adds_data/jpod.db"
JPOD_CONN <- dbConnect(RSQLite::SQLite(), DB_DIR)
if(exists("JPOD_CONN")){print("Connection to JPOD successfull")}

#### Get jop postings that contain keywords of bloom technologies
res <- lapply(
  unique(bloom_tech_keywords$tech_field), function(x){
  
  print(paste("Searching job postings in the field of:", x))
    
  # define keywords for the technology
  KEYWORDS <- bloom_tech_keywords %>% 
    filter(tech_field == x) %>% 
    pull(key_word)
    
  # define query and retrieve results
  JPOD_QUERY <- keyword_jpod_query(keywords = KEYWORDS)
  df <- jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERY)
  
  # annotate and return if postings were matched
  if(nrow(df) > 0){
    df$bloom_field <- x
    print(paste("Searching for job postings in the field", x, "completed. Number of postings retrieved:", nrow(df)))
    }else{
    print(paste("Searching for job postings in the field", x, "completed. Number of postings retrieved: 0"))
    }

  # return the data
  return(df)
  }
)

res <- bind_rows(res) %>% na.omit()
print(paste("Total number of postings matched to technology fields from Bloom et al. (2021):", nrow(res)))
print("Distribution by technology:")
print(
  res %>% group_by(bloom_field) %>% summarise(count = n())
)
print(paste("Total number of unique postings matched to technology fields from Bloom et al. (2021):", 
      length(unique(res$uniq_id))))

#### save:
write.csv(x = res, file = "jpod_bloom/jpod_bloom.csv", row.names = FALSE)


