#####
#week 39 
#Paul Beach
#####
library(librarian)
shelf(tidyverse,tidytuesdayR,elevatr,
      tidytext,usmap,sfheaders,
      sf,geodata, maptools,geogrid, ggthemes,easystats,MetBrewer,scales,ggtext,
      cowplot)

ttl <- tt_load(2022,37)
#hexgrid data
ushex <- st_read("us_states_hexgrid/us_states_hexgrid.shp")
raw_df <- ttl$bigfoot

#hist numeric
# raw_df |> 
#   select(where(is.numeric)) |> colnames() |> 
#   map(~raw_df |> pull(.x) |> hist(main = .x))



#sentiment analysis
sent <- get_sentiments("afinn")
sentnrc <- get_sentiments("nrc")

bfwords <- raw_df |> unnest_tokens(word, observed) |> 
  anti_join(stop_words) |> 
  left_join(sent) |> 
  left_join(sentnrc)

sentbf <- bfwords |> 
  group_by(state) |> 
  mutate(totwords = n()) |> 
  group_by(state,sentiment,totwords) |> 
  summarise(n = n()) |> 
  mutate(prop = n/totwords)


ush <- ushex |>
  st_transform(crs = st_crs(3857)) |> 
  select(iso = iso3166_2,state = google_nam,label, geometry) |> 
  mutate(state = str_remove(state, fixed("(United States)")) |> str_trim()) |> 
  left_join(bfw) |> 
  select(geometry, everything())

ush_emo <- 
  ushex |>
  st_transform(crs = st_crs(3857)) |> 
  select(iso = iso3166_2,state = google_nam,label, geometry) |> 
  mutate(state = str_remove(state, fixed("(United States)")) |> str_trim()) |> 
  left_join(sentbf) |> 
  select(geometry, everything()) |>
  #mutate(prop = ifelse(state %in% selstates, prop,NA)) |> 
  group_by(sentiment) |> 
  mutate(prop1 = percent_rank(prop)) |> 
  drop_na(sentiment)

labr <- c(`anger` = "ANGER",
          "disgust" = "DISGUST",
          "anticipation" = "ANTICIPATION",
          "joy" = "JOY",
          "sadness" = "SADNESS",
          'trust' = "TRUST")
caption1 <- "Source: data.world; brfo.net; NRC Word-Emotions | #tidytuesday week 37"
p4 <- ush_emo |>
  filter(sentiment %in% c("anger", "trust", "joy", "sadness", "disgust", "anticipation")) |>
  ggplot(aes(geometry = geometry))+
  labs(fill = "PERCENT RANK",
       title = "Bigfoot Encounters as Described by People From Different States" |> toupper(),
       subtitle = "These maps highlight the percent rank for each state of the proportion of words used to describe Bigfoot encounters
       associated with the selected sentiments",
       caption = caption1)+
  geom_sf(aes(geometry = geometry, fill = prop1)) +
  geom_sf_text(aes(label = iso), size = 3) +
  theme_map(font_family = "Gill Sans")+
  scale_fill_viridis_c(labels = c("0","50","100%"),
                       breaks = c(0,.5, 1))+
  facet_wrap(sentiment~.,nrow = 3,labeller = labeller(sentiment = labr),strip.position = "bottom")+
  guides(fill = guide_colorbar(title.position = "top",
                               hjust = .5,
                               barwidth = 10,
                               title.theme = element_text(hjust = .5, face = "bold")))+
  theme(plot.background = element_rect(fill = "cornsilk", color = NA),
        legend.position = "bottom",
        legend.justification = "center",
        plot.caption = element_text(hjust = .5),
        legend.title = element_text(size = 15, face = "bold"),
        strip.text = element_text(family = "Gill Sans", face = "bold"),
        plot.subtitle = element_textbox_simple(lineheight = 1,
                                               padding = margin(5.5, 5.5, 5.5, 5.5),
                                               margin = margin(5, 5.5, 5.5, 5.5),
                                               size = 16,
                                               family = "Bembo",
                                               hjust = .5,halign = .5),
        plot.title = element_textbox_simple(lineheight = 1,
                                            padding = margin(5.5, 5.5, 5.5, 5.5),
                                            margin = margin(5, 5.5, 5.5, 5.5),
                                            halign = .5,
                                            family = "Bembo",
                                            face = "plain",
                                            size = 18))
ggsave("bf.png",plot = p4, height = 10,width = 7.5,dpi = "retina")
        
  

