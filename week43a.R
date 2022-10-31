
# -------------------------------------------------------------------------
#TT week 43
#PB
# -------------------------------------------------------------------------


# Read in Data ------------------------------------------------------------
library(librarian)
shelf(tidyverse,tidytable,ggthemes,tidytuesdayR,tidytext, 
      hunspell,widyr,text2vec,igraph,Matrix,
      ggnetwork,economiccomplexity,ggraph,ggtext,ggrepel)
ttl <- tt_load(2022,week = 43)
stp <- get_stopwords()

ch_raw <- ttl$challenges
braw <- ttl$bakers
rraw <- ttl$ratings
epraw <- ttl$episodes


# Clean Data,  form token list --------------------------------------------

#starter token list cleaned with stems and likely dictionary words 
tf <- ch_raw |>
  mutate(across(c(signature,showstopper),~replace_na(.,"")),
         food = paste(signature,showstopper),
         id = 1:n()) |> 
  select(-c(signature,showstopper, technical)) |> 
  mutate(id = row_number()) %>% 
  unnest_tokens(word, food, to_lower = F) %>%
  bind_cols(suggestion =
              hunspell_stem(.[["word"]],dict= "en_GB") |>
              map( ~ ifelse(is.null(.), NA, .)) |>
              map( ~ pluck(., 1)) |>
              unlist() |> as_vector()) |> 
  mutate(words = ifelse(is.na(suggestion),word,suggestion) |> tolower())


#list of remaining non dictionary words
remain <- tf |> filter(is.na(suggestion) &!is.na(word)) |> pull(words) |> unique()

#takes forever but captures remaining likely spelling errors
hsv <- hunspell_suggest(remain)|> 
  map( ~ ifelse(is.null(.), NA, .)) |>
  map_chr( ~ pluck(., 1))

#lots of errors but clearly combined words are easy targets so will only tackle those
spellerrors <- tibble(remain = remain, hs = hsv) |> 
  mutate(cspell = ifelse(str_count(hs, "\\w+")>1,hs,remain)) |> 
  select(words = remain,cspell)

#put it all together 
tf2 <- tf |> 
  left_join(spellerrors) |> 
  mutate(word = ifelse(is.na(cspell), words, cspell)) |> 
  unnest_tokens(word,word) |> 
  select(-c(suggestion, words,cspell)) |> 
  mutate(winner = case_when(result %in% c("WINNER", "STAR BAKER") ~ "WINNER",
                            result %in% c("IN") ~ "IN",
                            result %in% c("OUT", "Runner-up") ~ "OUT"))
#word count (used later)
nwords <- tf2 |>
  anti_join(stp) |> 
  count(word)

#list of season winners
swin <- ch_raw |> filter(result == "WINNER") |> distinct(baker) |> pull()

# Create Sparce vocab matrix ----------------------------------------------


tfl <- tf2 |> 
  anti_join(stp) |> #removes stopword
  group_by(id) %>% 
  group_split()|> 
  map(~pull(.,word)) |> 
  itoken()

vocab <- create_vocabulary(tfl) |> prune_vocabulary(term_count_min = 2)
vectorizer <- vocab_vectorizer(vocab)

#sparce matrix 
tcm_recip <- create_tcm(tfl, vectorizer, skip_grams_window = 20)


# make igraph from matrix (need a tidy way to do this)
g2 <- tcm_recip |> 
  graph_from_adjacency_matrix(mode = "upper",weighted = T) |> 
  simplify(remove.loops = T,remove.multiple = T)

#simplify with minimum spanning tree
g3 <- mst(g2,weights = 1/E(g2)$weight>.5, algorithm = "prim")

#add counts (need tidy way)
V(g3)$count <- tibble("word" = V(g3)$name) |> 
  left_join(nwords, by = "word") |> 
  pull(n)



# EconComplexity to create rca and mcp matrices ---------------------------

#balassa index
sb <- 
  tf2 |> filter(n()>10, .by = word)%>%
  with(.,table(baker,word)) |> 
  Matrix()

rcam <- t(t(sb/rowSums(sb))/(colSums(sb)/sum(sb)))
mcpm <- rcam
mcpm[mcpm < 1] <- 0 #tidify
mcpm[mcpm >= 1] <- 1

cm <- complexity_measures(mcpm,method = "reflections")
prx <- proximity(mcpm)
prj <- projections(prx$proximity_country,prx$proximity_product)


# dont like this network approach for this problem 
g6 <- prj$network_product |> 
  simplify() |> 
  mst(weights = E(prj$network_product)$weight <.6,algorithm = "prim")

V(g6)$count <- tibble("word" = V(g6)$name) |> 
  left_join(nwords, by = "word") |> 
  pull(n)

# here is the graph
ggnetwork(g6, layout = layout_with_lgl(g6)) |>
  ggplot(aes(x = x,y = y, xend = xend,yend = yend))+
  geom_nodes(aes(size = count), shape = 21)+
  geom_edges(alpha = .4)+
  geom_nodetext_repel(data = .%>% filter(count > 30),aes(label = name), size = 3)+
  theme_graph()


# Quick look at complexity measures ---------------------------------------

mcpm |> 
  as.matrix() |> 
  as_tibble(rownames = "baker") |> 
  pivot_longer(-baker) |> 
  mutate(diversity = sum(value),.by = baker) |>
  mutate(ubiquity = sum(value),.by = name) |>
  filter(value == 1) |> 
  summarise(across(c(diversity,ubiquity),mean),.by = baker) |> 
  arrange(-diversity) |> 
  #mutate(winner = ifelse(baker %in% swin, "W", "N")) |> 
  ggplot(aes(x = diversity, y = ubiquity))+
  geom_point()+
  theme_tufte()+
  geom_rangeframe()+
  geom_text_repel(data = . %>% slice_max(diversity,5),
                   aes(label = baker),
                  family= "Gill Sans",
                  min.segment.length = 0)+
  labs(x = "Distinct Ingredients (RCA > 1)",
       y = "Average Ubiquity of Ingredients")



cm$complexity_index_country |> 
  as_tibble(rownames = "baker") |> 
  arrange(desc(value)) |> 
  filter(baker %in% swin)

cm$complexity_index_product|>
  as_tibble(rownames = "word") |> 
  arrange(value)


mcp <- mcpm |>
  as.matrix() |>
  as_tibble(rownames = "baker") |>
  pivot_longer(-baker) |>
  filter(baker %in% swin) |>
  rename(mcp = value) |>
  filter(mcp == 1)
  
 #need to get the rcas for labels 

  rca <- rcam |> 
    as.matrix() |>
    as_tibble(rownames = "baker") |>
    pivot_longer(-baker) |>
    filter(baker %in% swin) |>
    rename(rca = value)

#TODO prob a better way but sets a full network below each baker
base_network <- ggnetwork(g3,layout = layout_with_kk(g3))


# tt graph ----------------------------------------------------------------

gbbo <- ggnetwork(g3,layout = layout_with_kk(g3)) |>
  as_tibble() |> 
  left_join(mcp) |> 
  left_join(rca) |> 
  drop_na(baker) |>
  mutate(baker = fct_reorder(baker,mcp,sum)) |> 
ggplot(aes(x = x, y = y, xend = xend, yend = yend))+
  labs(title = 'The Ingredient Space of GBBO Reveals Some Difference in Cooking Styles of Season Winners',
       subtitle = 
       "The network shows the relationship bewteen words used to describe baker's creations over the 
       ten seasons of the show. Words that are often used together are likely clustered 
       and the most used words are ajusted for size. The words of each winner are highlighted if
       they have a 'revealed comparative advantage' (RCA) in the word compared to the whole field. 
       The words with the highest RCA, or the 'signature' words/indredient, are labeled for each baker",
       caption = "Paul Beach | #tidytuesday 2022.43 | Data: bakeoff package")+
  geom_nodes(data = base_network, aes(size = count), shape = 21,alpha = .2)+
  geom_nodes(aes(size = count, fill = baker, size = count),shape = 21, alpha = .8)+
  geom_edges(alpha = .1)+
  geom_nodetext_repel(data = . %>% slice_max(rca,n = 5,.by = "baker"),
                      aes(label = name),
                      nudge_x = -500,force = 100,
                      nudge_y = -500,
                      min.segment.length = 0,
                      family = "Gill Sans",
                      fontface = "italic")+
  #geom_nodetext_repel(data = . %>% filter(count > 10), aes(label= name),force = 10)+
  theme_graph(base_family = "Gill Sans")+
  guides(size = "none", fill = "none")+
  facet_wrap(~baker)+
  theme(plot.title = element_textbox_simple(halign = .5,
                                            family = "Gill Sans",
                                            margin = margin(5,5,15,5)),
        plot.subtitle = element_textbox_simple(
          family = "Gill Sans",
          margin = margin(5,5,5,5)),
        plot.caption = element_textbox_simple(halign = .5))


ggsave("gbbo.png",gbbo, width = 9, height = 9)

