source("jpod_bloom/dataget.R")

#### load packages -------------------------
package_setup(packages = c("RSQLite", "DBI", "tidyverse"))

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

#### Largest companies per techfield from Bloom et al. (2021):


#### Number of companies per techfield from Bloom et al. (2021):
