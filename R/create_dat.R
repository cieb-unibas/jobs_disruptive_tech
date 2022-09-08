#### load functions and packages ####
setwd("/scicore/home/weder/nigmat01/jobs_disruptive_tech/")
for(x in c("package_setup", "connect_jpod")){
  source(paste0("R/", x, ".R"))
}
PKGS <- c("RSQLite", "DBI", "tidyverse")
package_setup(packages = PKGS)

#### Connect to JPOD and test ####
DB_DIR <- "/scicore/home/weder/GROUP/Innovation/05_job_adds_data/jpod.db"
JPOD_CONN <- dbConnect(RSQLite::SQLite(), DB_DIR)
if(exists("JPOD_CONN")){
  print("Connection to JPOD successfull")
  ch_total_test()
  }

#### JPOD QUERIES: ####
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

# Number of postings with connection to technologies from Bloom et al. (2021) per institution:
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

#### Data for plotting a Swiss map
map_df <- merge(nuts_total, nuts_bloom, by = c("nuts_2", "Grossregion"))
ch_total <- list("CH0", NA, sum(map_df$total_postings), sum(map_df$bloom_postings))
map_df[nrow(map_df) + 1, ] <- ch_total
map_df <- map_df %>%
    mutate(bloom_share = bloom_postings / total_postings) %>%
    arrange(-bloom_share)
write.csv(map_df, "data/map_df.csv", row.names = FALSE)
print("Data for plotting a Swiss map saved.")

#### Data for plotting number of active institutions by technology:
AGENCIES <- c("rocken", "myitjob", "yellowshark", 
              "adecco", "randstad", "michael page",
              "digital minds", "personal sigma")
plot_df <- company_postings %>%
  filter(!company_name %in% AGENCIES) %>% # exclude agencies
  group_by(bloom_field) %>% summarise(n_institutions = n())
write.csv(plot_df, "data/plot2_df.csv", row.names = FALSE)
print("Data for plotting the number of active institutions by technology saved.")
# ggplot(data = plot_df, aes(y = n_institutions, x = reorder(bloom_field, desc(n_institutions))))+
#   geom_col(position = "dodge") +
#   labs(y = "Number of Institutions",
#        x ="Technology Field") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         panel.background = element_blank(),
#         panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
#         axis.line = element_line(),
#         axis.title = element_text(face="bold",size=10))
# ggsave("img/number_of_institutions.png")

#### Data for plotting the concentration of job postings across institutions
FIELDS <- c("Cloud computing", "Smart devices", "Machine Learning AI", 
            "Solar Power", "Virtual Reality", "3d printing")
plot_df <- company_postings %>% 
    group_by(bloom_field) %>%
    arrange(-bloom_postings) %>%
    filter(!company_name %in% AGENCIES) %>%
    mutate(
        cum_postings = cumsum(bloom_postings),
        cum_share = cum_postings / sum(bloom_postings),
        n_companies = n(), 
        company_index = seq(n_companies),
        company_share = company_index / n_companies
        ) %>%
  filter(bloom_field %in% FIELDS)
write.csv(plot_df, "data/plot3_df.csv", row.names = FALSE)
print("Data for plotting the concentration of job postings across institutions saved.")
# ggplot(data = plot_df,
#        aes(y = cum_share, x = company_share,
#            color = bloom_field, fill = bloom_field))+
#     geom_line() +
#     geom_area(alpha = 0.5) +
#     facet_wrap(.~bloom_field, scales = "free") +
#     guides(color = "none", fill = "none") +
#     scale_y_continuous(labels = scales::percent) +
#     scale_x_continuous(labels = scales::percent) +
#     labs(y = "Share of Postings",
#          x ="Share of Institutions")+
#   theme(panel.background = element_blank(),
#         panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
#         axis.line = element_line(),
#         axis.title = element_text(face="bold",size=10))
# ggsave("img/company_dist.png")
