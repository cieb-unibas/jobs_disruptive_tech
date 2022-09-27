#### load functions and packages------------------------------------------------
#setwd("/scicore/home/weder/nigmat01/Innoscape-GitHub-Repos/jobs_disruptive_tech/")
for(x in c("package_setup", "connect_jpod")){
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
# Total number of postings by NUTS-2 region
JPOD_QUERY <- "
    SELECT COUNT(*) as total_postings, pc.nuts_2, rg.name_en AS Grossregion
    FROM position_characteristics pc
    LEFT JOIN (
        SELECT nuts_2, name_en
        FROM regio_grid 
        WHERE nuts_3 IS NULL AND nuts_2 IS NOT NULL
        ) rg on pc.nuts_2 = rg.nuts_2
    GROUP BY pc.nuts_2
    "
nuts_total <- jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERY)

# Number of postings with connection to technologies from Bloom et al. (2021) by NUTS-2 region
JPOD_QUERY <- "
    SELECT COUNT(DISTINCT(pc.uniq_id)) as bloom_postings, pc.nuts_2, rg.name_en as Grossregion
    FROM position_characteristics pc 
    LEFT JOIN (
        SELECT nuts_2, name_en
        FROM regio_grid 
        WHERE nuts_3 IS NULL AND nuts_2 IS NOT NULL
        ) rg on pc.nuts_2 = rg.nuts_2
    WHERE pc.uniq_id IN (SELECT DISTINCT(bt.uniq_id) FROM bloom_tech bt)
    GROUP BY rg.name_en, pc.nuts_2
    "
nuts_bloom <- jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERY)

# Companies with job postings having a connection to technologies from Bloom et al. (2021):
JPOD_QUERY <- paste0("
    SELECT pc.company_name, bt.bloom_field, COUNT(DISTINCT(bt.uniq_id)) as bloom_postings
    FROM (
        SELECT uniq_id, bloom_field
        FROM bloom_tech
        ) bt
    LEFT JOIN (
        SELECT uniq_id, company_name
        FROM position_characteristics
        ) pc on pc.uniq_id = bt.uniq_id
    GROUP BY pc.company_name, bt.bloom_field
    ORDER BY bloom_postings DESC
    ")
company_postings <- jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERY)
company_postings <- company_postings %>% 
  group_by(bloom_field) %>% 
  mutate(total = sum(bloom_postings))

#### (A) Data for Figure 1 -----------------------------------------------------
map_df <- merge(nuts_total, nuts_bloom, by = c("nuts_2", "Grossregion"))
ch_total <- list("code" = "CH0", "Grossregion" = NA, 
                 "total_postings" = sum(map_df$total_postings), "bloom_postings" = sum(map_df$bloom_postings))
map_df[nrow(map_df) + 1, ] <- ch_total
map_df <- map_df %>%
    mutate(regio_bloom_share = bloom_postings / total_postings,
           ch_bloom_share = bloom_postings / ch_total$bloom_postings) %>%
    arrange(-regio_bloom_share)
#write.csv(map_df, "data/plot1_df.csv", row.names = FALSE)
#print("Data for plotting Swiss maps saved.")

#### (B) Data for Figure 2 -----------------------------------------------------
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
#write.csv(plot_df, "data/plot2_df.csv", row.names = FALSE)
#print("Data for plotting the number of active institutions by technology saved.")

#### (C) Data for Figure 3 -----------------------------------------------------
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
# test if company level market shares add up to 1:
test_n <- plot_df %>% 
  group_by(bloom_field) %>% 
  summarise(test_share = sum(market_share)) %>% 
  filter(test_share != 1) %>% 
  nrow()
if(test_n != 0){warning("Market shares do not add up to 1 across all technologies.")}

plot_df <- plot_df %>%
  mutate(market_share_sqrd = market_share^2) %>%
  group_by(bloom_field) %>%
  summarise(hhi = sum(market_share_sqrd), 
            companies = n()) %>%
  mutate(company_share = companies / n_companies) %>%
  arrange(-company_share) %>%
  head(10) %>% # only top-ten
  mutate(hhi_normed = hhi / mean(hhi))
#write.csv(plot_df, "data/plot3_df.csv", row.names = FALSE)
#print("Data for plotting the concentration of job postings across institutions saved.")