#### load functions and packages------------------------------------------------
for(x in c("package_setup")){
  source(paste0("R/", x, ".R"))
}        
package_setup(packages = c("tidyverse", "sf", "viridis"))

#### Figure 1: Spread of Disruptive Technologies Across Swiss Regions-----------
# Geo information is taken from:
# https://www.bfs.admin.ch/bfs/de/home/statistiken/regionalstatistik/kartengrundlagen/basisgeometrien.assetdetail.7546178.html
GEO_PATH <- "/scicore/home/weder/GROUP/ch_geo/2019_THK_PRO/PRO/03_ANAL/GesamtflДche_gf/K4_greg20001205_gf/"
geo <- st_read(paste0(GEO_PATH, "K4greg20001205gf_ch2007Poly.shp"))

# Credits to --------- 
# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
# https://stackoverflow.com/questions/17757281/r-map-switzerland-according-to-npa-locality
# https://tlorusso.github.io/geodata_workshop/sf_package
plot_df <- read.csv("data/plot1_df.csv") # connect to job postings data:
plot_df <- geo %>%
  mutate(nuts_2 = paste0("CH0", seq(7))) %>%
  select(geometry, nuts_2) %>%
  left_join(plot_df, by = "nuts_2") %>%
  filter(bloom_field == "overall") %>%
  na.omit()
text_labels <- plot_df %>% select(regio_bloom_share ) %>%
  mutate(regio_bloom_share  = scales::percent(regio_bloom_share , accuracy = 0.1)) %>%
  cbind(st_coordinates(st_centroid(plot_df$geometry)))
ggplot(data = plot_df)+
  geom_sf(aes(fill = regio_bloom_share), alpha = 0.9) +
  geom_text(data = text_labels, aes(x = X, y = Y, label = regio_bloom_share ), 
            size = 3, vjust = 2, hjust = -0.3) +
  scale_fill_viridis(name=" Share of job postings\n with a connection to\n disruptive technologies", 
                     option = "plasma", direction = -1,
                     begin = 0.3, end = 0.7,
                     labels = scales::percent) +
  theme_void() +
  theme(legend.position = "right")
ggsave("img/plot_1.png")

#### Figure 2: Spread of Disruptive Technologies Across Employers in CH---------
plot_df <- read.csv("data/plot2_df.csv")
ggplot(data = plot_df,
       aes(y = share_total, x = reorder(bloom_field, desc(share_total)),
           fill = share_total))+
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(option = "plasma", direction = -1, begin = 0.3, end = 0.7) +
  labs(y = "Share of Employers\n Mentioning a Technology",
       x ="Technology Field") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "",
        panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))
ggsave("img/plot_2.png")


#### Figure 3: Figure 3: Concentration of Job Postings Among Employers----------
plot_df <- read.csv("data/plot3_df.csv")
ggplot(data = plot_df,
       aes(y = hhi, x = reorder(bloom_field, desc(hhi)),
           fill = hhi))+
  scale_fill_viridis(option = "plasma", direction = -1, begin = 0.3, end = 0.7) +
  geom_col(position = "dodge") +
  labs(y = "Herfindahl-Hirschman Coefficient",
       x ="Technology Field") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "",
        panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))
ggsave("img/plot_3.png")

