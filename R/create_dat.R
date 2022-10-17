#### load functions, objects and packages------------------------------------------------
for(x in c("package_setup", "connect_jpod", "jpod_queries")){
  source(paste0("R/", x, ".R"))
}
package_setup(packages = c("RSQLite", "DBI", "tidyverse"))

#### Connect to JPOD and test---------------------------------------------------
DB_DIR <- "/scicore/home/weder/GROUP/Innovation/05_job_adds_data/jpod.db"
JPOD_CONN <- dbConnect(RSQLite::SQLite(), DB_DIR)
if(exists("JPOD_CONN")){
  print("Connection to JPOD successfull")
  ch_total_test()
  }

#### Extract data from JPOD:----------------------------------------------------
# Total number of postings by NUTS-2 region:
nuts_total <- jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERIES[["total_nuts"]])
print("Total number of postings by NUTS-2 region retrieved")

# Number of postings with connection to technologies from Bloom et al. (2021) by NUTS-2 region:
nuts_bloom <- jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERIES[["bloom_nuts"]])
print("Number of postings with connection to overall technologies from Bloom et al. (2021) by NUTS-2 region retrieved")

# Number of postings in selected technologies from Bloom et al. (2021) by NUTS-2 region:
TOP_FIELDS <- jpodRetrieve(jpod_conn = JPOD_CONN, 
                           sql_statement = top_n_field_query(n = 4)) # retrieve largest 5 fields
nuts_bloom_top <- jpodRetrieve(jpod_conn = JPOD_CONN, 
                               sql_statement = nuts_selected_bloom_query(fields = TOP_FIELDS$field))
print("Number of postings with connection to selected technologies from Bloom et al. (2021) by NUTS-2 region retrieved")

# Companies with job postings having a connection to technologies from Bloom et al. (2021):
company_postings <- jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERIES[["bloom_companies"]])
company_postings <- company_postings %>% 
  group_by(bloom_field) %>% 
  mutate(total = sum(bloom_postings))
print("Companies with job postings having a connection to technologies from Bloom et al. (2021) retrieved")

#### Data for Figure 1 -----------------------------------------------------
# regional specialization in overall fields:
map_df <- merge(nuts_total, nuts_bloom, by = c("nuts_2", "Grossregion"))
map_df <- map_df %>%
    mutate(bloom_field = "overall",
           regio_bloom_share = bloom_postings / total_postings,
           ch_bloom_share = bloom_postings / sum(bloom_postings)) %>%
    arrange(-regio_bloom_share)
# regional specialization in selected fields:
nuts_bloom_top <- nuts_bloom_top %>%
  left_join(nuts_total, by = c("nuts_2")) %>%
  mutate(regio_bloom_share = bloom_postings / total_postings,
         ch_bloom_share = NA) %>%
  arrange(-regio_bloom_share)
map_df <- rbind(map_df, nuts_bloom_top[, names(map_df)]) %>% arrange(bloom_field, -regio_bloom_share)
write.csv(map_df, "data/plot1_df.csv", row.names = FALSE)
print("Data Figure 1 saved.")

#### Data for Figure 2 -----------------------------------------------------
AGENCIES <- c("rocken", "myitjob", "yellowshark", 
              "adecco", "randstad", "michael page",
              "digital minds", "personal sigma",
              "manpower", "tiger"
              )
n_companies <- jpodRetrieve(jpod_conn = JPOD_CONN, 
                            sql_statement = "SELECT COUNT(*) as n_companies FROM institutions")
n_companies <- n_companies$n_companies - length(AGENCIES) # 76'926

plot_df <- company_postings %>%
  filter(!company_name %in% AGENCIES) %>% # exclude agencies
  group_by(bloom_field) %>% 
  summarise(n_institutions = n(),
            share_total = n_institutions / n_companies) %>%
  filter(n_institutions >= 50) %>% 
  mutate(share_total_normed = share_total / mean(share_total))
write.csv(plot_df, "data/plot2_df.csv", row.names = FALSE)
print("Data for Figure 2 saved.")

#### Data for Figure 3 -----------------------------------------------------
AGENCIES <- c("rocken", "myitjob", "yellowshark", 
              "adecco", "randstad", "michael page",
              "digital minds", "personal sigma",
              "manpower", "tiger"
              )
n_companies <- jpodRetrieve(jpod_conn = JPOD_CONN, 
                            sql_statement = "SELECT COUNT(*) as n_companies FROM institutions")
n_companies <- n_companies$n_companies - length(AGENCIES)

plot_df <- company_postings %>% 
  filter(!company_name %in% AGENCIES) %>%
  group_by(bloom_field) %>%
  mutate(market_share = bloom_postings / sum(bloom_postings))

plot_df <- plot_df %>%
  mutate(market_share_sqrd = market_share^2) %>%
  group_by(bloom_field) %>%
  summarise(hhi = sum(market_share_sqrd), 
            companies = n()) %>%
  mutate(company_share = companies / n_companies) %>%
  arrange(-company_share) %>%
  head(10) %>% # only top-ten
  mutate(hhi_normed = hhi / mean(hhi))
write.csv(plot_df, "data/plot3_df.csv", row.names = FALSE)
print("Data for Figure 3 saved.")

# biggest companies per bloom field:
top_companies <- company_postings %>%   
  filter(!company_name %in% AGENCIES) %>%
  group_by(bloom_field) %>%
  arrange(-bloom_postings) %>%
  mutate(rank = seq(n()), market_share = bloom_postings / sum(bloom_postings)) %>%
  filter(rank <= 5) %>%
  arrange(bloom_field, rank) %>%
  select(-total)
write.csv(top_companies, "data/top_emp.csv", row.names = FALSE)
print("Data for top employers saved.")


## tests:
# # firm <- 'meag munich ergo assetmanagement gmbh'
# # firm <- 'login berufsbildung ag'
# # firm <- 'bouygues energies & services'
# firm <- 'galliker transport ag'
# # firm <- 'facebook'
# JPOD_QUERY <- paste0("
# SELECT jp.job_description
# --SELECT COUNT(*) AS total_postings, COUNT(DISTINCT(jp.job_description)) AS n_unique
# FROM job_postings jp
# WHERE jp.uniq_id IN (
#   SELECT uniq_id 
#   FROM position_characteristics 
#   WHERE company_name == '", firm, "'
#   ) 
# LIMIT 2
# ")
# jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERY)

