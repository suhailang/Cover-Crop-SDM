#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("readxl")
#install.packages("maps")
#install.packages("readr")
#install.packages("tidyverse")
#install.packages("mapdata")

library(ggplot2)
library(dplyr)
library(readxl)
library(maps)
library(readr)
library(tidyverse)
library(mapdata)

world_map<-map_data("world")

#df <- read_csv("/Users/sng04/R/UH_Cover_crop_collab/Inputs/All_cover_together_clean.csv")
df <- read_csv("/home/suhailak/kantar_koastore/suhaila/cover_crop_project/Inputs/All_cover_together_clean.csv")
head(df)

df$species<-as.factor(df$species)
df$col_num<-as.numeric(df$species)

df2<-df%>%mutate(country=map.where("world", longitude, latitude))
df_clean<-df2%>%filter(!is.na(country))

p<- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "gray90", color = "gray70") +
  geom_point(data = df_clean, aes(x = longitude, y = latitude, color = species),
             alpha = 0.6, size = 0.5) +
  facet_wrap(~species, ncol = 5, scales = "free",
             labeller = labeller(species = label_wrap_gen(width = 20))) +
  coord_quickmap() +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(size = 20, margin = margin(b = 5)),
        panel.spacing = unit(1, "lines"),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
  labs(title = "Global Occurrence Points by Species", x = "Longitude", y = "Latitude")

ggsave("/home/suhailak/kantar_koastore/suhaila/cover_crop_project/Outputs/Figs/species_occurrence_map.pdf",
       plot = p, width = 50, height = 20,
       units = "in", limitsize = FALSE)

head(df_clean)
#write.csv(df_clean, "cc_points_clean_none_in_ocean.csv")
