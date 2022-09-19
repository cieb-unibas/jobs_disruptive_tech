#### load functions and packages ####
setwd("/scicore/home/weder/nigmat01/Innoscape-GitHub-Repos/jobs_disruptive_tech/")
for(x in c("package_setup")){
  source(paste0("R/", x, ".R"))
}        
package_setup(packages = c("tidyverse", "sf", "viridis"))

#### load data ####
map_df <- read.csv("data/map_df.csv")
comp_df <- read.csv(
  file = "/scicore/home/weder/GROUP/Innovation/05_job_adds_data/augmentation_data/ch_total_employed_people_02_2022.csv",
  sep = ";")
GEO_PATH <- "/scicore/home/weder/GROUP/ch_geo/2019_THK_PRO/PRO/03_ANAL/GesamtflДche_gf/K4_greg20001205_gf/"
geo <- st_read(paste0(GEO_PATH, "K4greg20001205gf_ch2007Poly.shp"))

#### Representation
# compare to employment data from BFS for Q2/2022: https://www.pxweb.bfs.admin.ch/pxweb/de/px-x-0602000000_102/-/px-x-0602000000_102.px/
map_df <- geo %>%
  mutate(nuts_2 = paste0("CH0", seq(7))) %>%
  select(geometry, nuts_2) %>%
  left_join(map_df, by = "nuts_2") %>%
  na.omit() %>%
  mutate(ch_total_share = total_postings / map_df[map_df$nuts_2 == "CH0", ]$total_postings[2])
comp_df <- merge(comp_df, 
             map_df[, c("nuts_2", "Grossregion", "ch_total_share")], 
             by = "nuts_2")
comp_df <- comp_df[, c("nuts_2", "Grossregion", "employed_share", "ch_total_share")] %>%
  mutate(diff_share = ch_total_share / employed_share) %>%
  arrange(diff_share)
comp_df
# nuts_2              Grossregion employed_share ch_total_share diff_share
# 1   CH07                   ticino     0.04544429     0.01018492  0.2241187
# 2   CH01         région lemanique     0.19204901     0.10411934  0.5421498
# 3   CH05      eastern switzerland     0.12749293     0.11130664  0.8730417
# 4   CH02        espace mittelland     0.20242806     0.17744788  0.8765972
# 5   CH03 northwestern switzerland     0.13296988     0.13513996  1.0163200
# 6   CH06      central switzerland     0.09873514     0.11616087  1.1764896
# 7   CH04                   zurich     0.20088068     0.28011824  1.3944508
# => Zürich is overrepresented, Lémanique and Ticino clearly underrepresenteted

# adjust shares of Grossregionen according to their bias
map_df <- map_df %>% 
  left_join(comp_df[, c("nuts_2", "diff_share")], by = "nuts_2") %>%
  mutate(ch_bloom_share_adj = ch_bloom_share / diff_share) %>%
  select(-diff_share)

#### Plot Geographical Distribution
text_labels <- map_df %>% left_join(comp_df[, c("nuts_2", "diff_share")], by = "nuts_2") %>%
  select(ch_bloom_share, diff_share) %>%
  mutate(ch_bloom_share_adj  = scales::percent(ch_bloom_share / diff_share, accuracy = 0.1),
         ch_bloom_share  = scales::percent(ch_bloom_share, accuracy = 0.1)
         ) %>%
  select(-diff_share) %>%
  cbind(st_coordinates(st_centroid(map_df$geometry)))

ggplot(data = map_df)+
  geom_sf(aes(fill = ch_bloom_share), alpha = 0.9) +
  geom_text(data = text_labels, aes(x = X, y = Y, label = ch_bloom_share), 
            size = 3, vjust = 2, hjust = -0.3) +
  scale_fill_viridis(name="Share", option = "plasma", 
                     begin = 0.3, end = 0.85,
                     labels = scales::percent) +
  theme_void()
#ggsave("img/nuts_postings_distribution.png")
# => this is strongly biased for some regions. Thus maybe better only mention in text. 

#### Plot Specialization
text_labels <- map_df %>% select(regio_bloom_share ) %>%
  mutate(regio_bloom_share  = scales::percent(regio_bloom_share , accuracy = 0.1)) %>%
  cbind(st_coordinates(st_centroid(map_df$geometry)))
ggplot(data = map_df)+
  geom_sf(aes(fill = regio_bloom_share ), alpha = 0.9) +
  geom_text(data = text_labels, aes(x = X, y = Y, label = regio_bloom_share ), 
            size = 3, vjust = 2, hjust = -0.3) +
  scale_fill_viridis(name="Job Postings Share", option = "plasma", 
                     begin = 0.3, end = 0.85,
                     labels = scales::percent) +
  # ggtitle("Share of Job Postings in Disruptive Technologies") +
  theme_void()
ggsave("img/nuts_postings_specialization.png")



#### SOURCES AND CREDITS ####
# Geographies for switzerland from: ---
# https://www.bfs.admin.ch/bfs/de/home/statistiken/regionalstatistik/kartengrundlagen/basisgeometrien.assetdetail.7546178.html

# Credits to --------- 
# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
# https://stackoverflow.com/questions/17757281/r-map-switzerland-according-to-npa-locality
# https://tlorusso.github.io/geodata_workshop/sf_package