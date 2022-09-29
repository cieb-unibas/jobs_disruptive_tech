#### load functions, objects and packages------------------------------------------------
#setwd("/scicore/home/weder/nigmat01/Innoscape-GitHub-Repos/jobs_disruptive_tech/")
for(x in c("package_setup", "connect_jpod", "jpod_queries")){
  source(paste0("R/", x, ".R"))
}
package_setup(packages = c("RSQLite", "DBI", "tidyverse", "viridis"))

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
print("Number of postings with connection to technologies from Bloom et al. (2021) by NUTS-2 region retrieved")

# Companies with job postings having a connection to technologies from Bloom et al. (2021):
company_postings <- jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERIES[["bloom_companies"]])
company_postings <- company_postings %>% 
  group_by(bloom_field) %>% 
  mutate(total = sum(bloom_postings))
print("Companies with job postings having a connection to technologies from Bloom et al. (2021) retrieved")

#### Data for Figure 1 -----------------------------------------------------
map_df <- merge(nuts_total, nuts_bloom, by = c("nuts_2", "Grossregion"))
ch_total <- list("code" = "CH0", "Grossregion" = NA, 
                 "total_postings" = sum(map_df$total_postings), "bloom_postings" = sum(map_df$bloom_postings))
map_df[nrow(map_df) + 1, ] <- ch_total
map_df <- map_df %>%
    mutate(regio_bloom_share = bloom_postings / total_postings,
           ch_bloom_share = bloom_postings / ch_total$bloom_postings) %>%
    arrange(-regio_bloom_share)
write.csv(map_df, "data/plot1_df.csv", row.names = FALSE)
print("Data Figure 1 saved.")

#### Data for Figure 2 -----------------------------------------------------
AGENCIES <- c("rocken", "myitjob", "yellowshark", 
              "adecco", "randstad", "michael page",
              "digital minds", "personal sigma")
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
              "digital minds", "personal sigma")
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