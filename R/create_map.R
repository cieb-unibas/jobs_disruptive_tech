#### load functions and packages ####
setwd("/scicore/home/weder/nigmat01/jobs_disruptive_tech/")
for(x in c("package_setup")){
  source(paste0("R/", x, ".R"))
}        
package_setup(packages = c("tidyverse", "sf", "viridis"))

#### load data ####
map_df <- read.csv("data/map_df.csv")
GEO_PATH <- "/scicore/home/weder/GROUP/ch_geo/2019_THK_PRO/PRO/03_ANAL/GesamtflДche_gf/K4_greg20001205_gf/"
geo <- st_read(paste0(GEO_PATH, "K4greg20001205gf_ch2007Poly.shp"))
map_df <- geo %>%
  mutate(nuts_2 = paste0("CH0", seq(7))) %>%
  select(geometry, nuts_2) %>%
  left_join(map_df, by = "nuts_2") %>%
  na.omit() 


#### Plot Geographical Distribution
text_labels <- map_df %>% select(ch_bloom_share ) %>%
  mutate(ch_bloom_share  = scales::percent(ch_bloom_share , accuracy = 0.1)) %>%
  cbind(st_coordinates(st_centroid(map_df$geometry)))
ggplot(data = map_df)+
  geom_sf(aes(fill = ch_bloom_share ), alpha = 0.9) +
  geom_text(data = text_labels, aes(x = X, y = Y, label = ch_bloom_share ), 
            size = 3, vjust = 2, hjust = -0.3) +
  scale_fill_viridis(name="Share", option = "plasma", 
                     begin = 0.3, end = 0.85,
                     labels = scales::percent) +
  theme_void()
ggsave("img/nuts_postings_distribution.png")


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