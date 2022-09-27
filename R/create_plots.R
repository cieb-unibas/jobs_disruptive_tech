#### load functions and packages------------------------------------------------
#setwd("/scicore/home/weder/nigmat01/Innoscape-GitHub-Repos/jobs_disruptive_tech/")
for(x in c("package_setup")){
  source(paste0("R/", x, ".R"))
}        
package_setup(packages = c("tidyverse", "sf", "viridis"))

#### Figure 1: Spread of Disruptive Technologies Across Swiss Regions-----------
# Geo information is taken from:
# https://www.bfs.admin.ch/bfs/de/home/statistiken/regionalstatistik/kartengrundlagen/basisgeometrien.assetdetail.7546178.html
GEO_PATH <- "/scicore/home/weder/GROUP/ch_geo/2019_THK_PRO/PRO/03_ANAL/GesamtflДche_gf/K4_greg20001205_gf/"
geo <- st_read(paste0(GEO_PATH, "K4greg20001205gf_ch2007Poly.shp"))

map_df <- read.csv("data/plot1_df.csv")
map_df <- geo %>%
  mutate(nuts_2 = paste0("CH0", seq(7))) %>%
  select(geometry, nuts_2) %>%
  left_join(map_df, by = "nuts_2") %>%
  na.omit()

text_labels <- map_df %>% select(regio_bloom_share ) %>%
  mutate(regio_bloom_share  = scales::percent(regio_bloom_share , accuracy = 0.1)) %>%
  cbind(st_coordinates(st_centroid(map_df$geometry)))

ggplot(data = map_df)+
  geom_sf(aes(fill = regio_bloom_share ), alpha = 0.9) +
  geom_text(data = text_labels, aes(x = X, y = Y, label = regio_bloom_share ), 
            size = 3, vjust = 2, hjust = -0.3) +
  scale_fill_viridis(name=" Share of job postings\n with a connection to\n disruptive technologies", option = "plasma", 
                     begin = 0.3, end = 0.85,
                     labels = scales::percent) +
  theme_void() +
  theme(legend.position = "right")
#ggsave("img/nuts_postings_specialization.png")

# Credits to --------- 
# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
# https://stackoverflow.com/questions/17757281/r-map-switzerland-according-to-npa-locality
# https://tlorusso.github.io/geodata_workshop/sf_package


#### Figure 2: Spread of Disruptive Technologies Across Employers in CH---------
plot_df <- read.csv("data/plot2_df.csv")
ggplot(data = plot_df,
       aes(y = share_total_normed, x = reorder(bloom_field, desc(-share_total_normed)),
           fill = log(share_total_normed)))+
  geom_col(position = "dodge") +
  geom_hline(yintercept = 1, color = "red", linetype = "dotted") +
  annotate(
    geom = "text", x = 5, y = 1.15, 
    label = paste0("Average Across Technologies"),#: ", 
    # round(mean(plot_df$share_total) *100, 1),"%"), 
    color = "red", size = 3) +
  scale_fill_viridis(option = "plasma", begin = 0.3, end = 0.85) +
  scale_y_continuous(#labels = scales::percent, 
    breaks = c(0, 2, 4, 6)) +
  labs(y = "Normed Share of Employers\n Mentioning a Technology\n (Avergae across technolgies = 1)",
       x ="Technology Field") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "",
        panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))
#ggsave("img/share_institutions_tech.png")


#### Figure 3: Figure 3: Concentration of Job Postings Among Employers----------
plot_df <- read.csv("data/plot3_df.csv")
ggplot(data = plot_df,
       aes(y = hhi_normed, x = reorder(bloom_field, desc(-hhi_normed)),
           fill = hhi_normed))+
  scale_fill_viridis(option = "plasma", begin = 0.3, end = 0.85) +
  geom_col(position = "dodge") +
  scale_y_continuous(#labels = scales::percent, 
    breaks = seq(0.5, 2.5, 1)) +
  geom_hline(yintercept = 1, color = "red", linetype = "dotted") +
  annotate(
    geom = "text", x = 3, y = 1.05, 
    label = paste0("Average Across Technologies"),# (scaled HHI): "), 
    # round(mean(plot_df$hhi * 10000), 1)), 
    color = "red", size = 3) +
  labs(y = "Normed Herfindahl-Hirschman Coefficient\n (Average across technologies = 1) ",
       x ="Technology Field") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "",
        panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))
#ggsave("img/hhi_techfield.png")

