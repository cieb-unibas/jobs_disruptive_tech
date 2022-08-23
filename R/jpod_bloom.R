source("R/dataget.R")

#### load packages -------------------------
package_setup(packages = c("RSQLite", "DBI", "tidyverse", "bfsMaps"))

#### connect do JPOD -------------------------
DB_DIR <- "/scicore/home/weder/GROUP/Innovation/05_job_adds_data/jpod.db"
JPOD_CONN <- dbConnect(RSQLite::SQLite(), DB_DIR)
if(exists("JPOD_CONN")){print("Connection to JPOD successfull")}
ch_total_test() # test

#### Share of job postings with connection to technologies 
#### from Bloom et al. (2021) at regional level: -------------------------------

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
total_postings <- jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERY)

# Number of postings with connection to technologies 
# from Bloom et al. (2021) by NUTS-2 region
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
plot_df <- jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERY)

# combine and calculate shares:
plot_df <- plot_df %>%
    merge(total_postings, by = c("nuts_2", "Grossregion")) %>%
    mutate(bloom_share = bloom_postings / total_postings) %>%
    arrange(-bloom_share)

# add share for overall Switzerland:
c("CH_total_bloom" = sum(plot_df$bloom_postings), 
  "CH_total_postings" = sum(plot_df$total_postings),
  "CH_bloom_share" =sum(plot_df$bloom_postings) / sum(plot_df$total_postings)
  )

# plot map: -------------------------------------------------------------------
# https://cran.r-project.org/web/packages/bfsMaps/bfsMaps.pdf




#### Job postings with connection to technologies 
#### from Bloom et al. (2021) across companies 
#### per techfield from Bloom et al. (2021):-------------------

# define fields of interest
BLOOM_FIELDS <- c("Cloud computing", "Machine Learning AI", 
                  "Smart devices", "Solar Power", 
                  "Autonomous Cars", "Virtual Reality")
BLOOM_CODES <- jpodRetrieve(jpod_conn = JPOD_CONN, 
                            sql_statement = "SELECT DISTINCT(bloom_code), bloom_field FROM bloom_tech")
BLOOM_CODES <- BLOOM_CODES[BLOOM_CODES$bloom_field %in% BLOOM_FIELDS, ]$bloom_code 

# retrieve number of postings per company:
JPOD_QUERY <- paste0("
    SELECT pc.company_name, bt.bloom_field, COUNT(DISTINCT(bt.uniq_id)) as bloom_postings
    FROM (
        SELECT uniq_id, bloom_field
        FROM bloom_tech
        --WHERE bloom_code IN (", paste(BLOOM_CODES, collapse = ","), ")
        ) bt
    LEFT JOIN (
        SELECT uniq_id, company_name
        FROM position_characteristics
        ) pc on pc.uniq_id = bt.uniq_id
    GROUP BY pc.company_name, bt.bloom_field
    HAVING bloom_postings >= 5
    ORDER BY bloom_postings DESC
    ")
df <- jpodRetrieve(jpod_conn = JPOD_CONN, sql_statement = JPOD_QUERY)
df <- df %>% group_by(bloom_field) %>% mutate(total = sum(bloom_postings))

# A) number of institutions by technology field --------------------------------
AGENCIES <- c("rocken", "myitjob", "yellowshark", 
              "adecco", "randstad", "michael page")
plot_df <- df %>%
  filter(!company_name %in% AGENCIES) %>%
  group_by(bloom_field) %>% summarise(log_n_institutions = log(n()+1))

ggplot(data = plot_df, aes(y = log_n_institutions, x = bloom_field))+
  geom_col(position = "dodge") +
  labs(y = "Number of Institutions (log)",
       x ="Technology field") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)
        )
#ggsave("img/number_of_institutions.png")

# B)  Concentration of job postings 
# across institutions by technology field -------------------------------------
plot_df <- df %>% 
    group_by(bloom_field) %>%
    arrange(-bloom_postings) %>%
    filter(!company_name %in% AGENCIES) %>%
    mutate(
        cum_postings = cumsum(bloom_postings),
        cum_share =  cum_postings / sum(bloom_postings),
           company_index = seq(n()))

# plot:
ggplot(data = plot_df, aes(y = cum_share, x = company_index, 
                           color = bloom_field, fill = bloom_field))+
    geom_line() +
    geom_area(alpha = 0.5) +
    facet_wrap(.~bloom_field, scales = "free") +
    guides(color = "none", fill = "none") +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Share of All Postings",
         x ="Number of Institutions (Ranked by Number of Postings)")
#ggsave("img/company_dist.png")
