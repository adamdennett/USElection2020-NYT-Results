##Some code to produce some maps

#Quite a lot of this code is borrowed from the git-fork

# Install these packages if you don't have them yet
#if (!require("pacman")) install.packages("pacman")
#devtools::install_github("favstats/tidytemplate")
#devtools::install_github("UrbanInstitute/urbnmapr")

pacman::p_load(tidyverse, urbnmapr, rvest, tmap, cowplot)

#read in some population data for the counties
counties_pop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv")

#spec(counties_pop)
#str(counties_pop)

#create a new county fips column
counties_pop <- counties_pop %>% 
  mutate(county_fips = str_c(STATE,COUNTY)) %>% 
  relocate(county_fips, .before = SUMLEV)

elex <- read_csv("https://raw.githubusercontent.com/favstats/USElection2020-NYT-Results/master/data/latest/presidential.csv")
elex <- rename(elex,county_fips = fips)

counties_sf <- get_urbn_map("counties", sf = TRUE)
#st_crs(counties_sf)

counties_alldata <- counties_sf %>% 
  left_join(elex) %>% 
  left_join(counties_pop) %>% 
  mutate(area_km=(st_area(.)/(1000^2))) %>% 
  mutate(popdensity2019=round((POPESTIMATE2019/area_km),3)) %>% 
  mutate(rankdensity=rank(desc(popdensity2019))) %>% 
  mutate(prop_trump = results_trumpd/votes) %>% 
  mutate(prop_biden = results_bidenj/votes) %>% 
  mutate(rank_trump=rank(desc(prop_trump))) %>% 
  mutate(rank_biden=rank(desc(prop_biden))) %>% 
  mutate(rank_weird=rankdensity+rank_trump)

#install.packages("biscale")
library(biscale)

sub <- drop_na(counties_alldata, margin2020)

# create classes
data <- bi_class(sub, x = prop_trump, y = popdensity2019, style = "quantile", dim = 3)
#unique(data$bi_class)

# create map
map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = NA, size = 0.1, show.legend = F) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Trumpy Townies and Biden Bumpkins",
    subtitle = "US Presidential Election 2020") +
  bi_theme()

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Trumpier",
                    ylab = "Townier",
                    size = 8)

# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, 0, 0.2, 0.2)

finalPlot

ggsave2("trump_plot.png",finalPlot, width = 10)

# create map
library(classInt)

breaks <- classIntervals(data$rank_weird, n = 5, style = "jenks")
breaks

data <- mutate(data, trumpytowns = cut(rank_weird, breaks$brks))

map1 <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = rank_weird), color = NA, size = 0.1, show.legend = T) +
  labs(title = "Trumpy Towns and Progressive Bumpkins") 

map1 + scale_fill_fermenter(palette = "RdPu") 
