source("jpod_bloom/dataget.R")

#### load packages and data -------------------------
package_setup(packages = c("RSQLite", "DBI", "tidyverse"))
df <- read.csv("/scicore/home/weder/GROUP/Innovation/05_job_adds_data/jpod_bloom.csv")

#### connect do JPOD -------------------------
DB_DIR <- "/scicore/home/weder/GROUP/Innovation/05_job_adds_data/jpod.db"
JPOD_CONN <- dbConnect(RSQLite::SQLite(), DB_DIR)
if(exists("JPOD_CONN")){print("Connection to JPOD successfull")}
# test:
ch_total_test()

#### Calculate share of technologies from Bloom et al. (2021) at regional level:
# NUTS-2
JPOD_QUERY <- "
    SELECT COUNT(*) as total_postings, nuts_2
    FROM position_characteristics 
    GROUP BY nuts_2
    "
total_postings <- jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERY)
plot_df <- jpodRetrieve(jpod_conn = JPOD_CONN, 
                            sql_statement = "SELECT nuts_2, name_en FROM regio_grid WHERE nuts_3 IS NULL AND nuts_2 IS NOT NULL")
plot_df <- total_postings %>% merge(plot_df, by = "nuts_2", all.x = TRUE)
plot_df <- df %>%
  group_by(nuts_2, bloom_field) %>%
  summarise(count = n()) %>%
  merge(plot_df, by = "nuts_2", all.x = TRUE) %>%
  mutate(share = count / total_postings,
         n_per_tsd = share * 1000) %>%
  na.omit()

# NUTS-3
JPOD_QUERY <- "
    SELECT COUNT(*) as total_postings, nuts_3
    FROM position_characteristics 
    GROUP BY nuts_3
    "
total_postings <- jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERY)
plot_df <- jpodRetrieve(jpod_conn = JPOD_CONN, 
                        sql_statement = "SELECT nuts_3, name_en FROM regio_grid WHERE nuts_3 IS NOT NULL")
plot_df <- total_postings %>% merge(plot_df, by = "nuts_3", all.x = TRUE)
plot_df <- df %>%
  group_by(nuts_3, bloom_field) %>%
  summarise(count = n()) %>%
  merge(plot_df, by = "nuts_3", all.x = TRUE) %>%
  mutate(share = count / total_postings,
         n_per_tsd = share * 1000) %>%
  na.omit() %>%
  arrange(-n_per_tsd)
