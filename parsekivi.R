library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(igraph)

# Script modified from 
# Analyzing networks of characters in 'Love Actually' by David Robinson
# http://varianceexplained.org/r/love-actually-network/

# Seven Brothers text copied from Project Gutenberg
# http://www.gutenberg.org/ebooks/11940.txt.utf-8
raw <- readLines("kivi.txt", skipNul = T)

names <- data_frame(raw = raw) %>%
  filter(grepl('^[A-ZÄÖ-]+\\.', raw)) %>%
  separate(raw, c("speaker","text"), sep = "\\.", extra = "drop") %>%
  group_by(speaker) %>%
  summarize(name = speaker[1]) %>%
  select(name)

# Excluding names which refer to a group of people or a non-person
# names <- names[!names$name %in% c("MUUT", "VELJEKSET", "DAMAGE"),]
# write.csv(names, "names.csv", row.names = F)
#
# Edited descriptions manually, and imported again
names.df <- read.csv("names.csv", stringsAsFactors = F)

lines <- data_frame(raw = raw) %>%
  filter(raw != "") %>%
  mutate(is_chap = str_detect(raw, " LUKU"),
         chapter = cumsum(is_chap)) %>%
  filter(!is_chap) %>%
  mutate(raw_speaker = gsub("^([A-ZÄÖ-]+)(\\.)(.*)", "\\1%\\3", raw, perl=TRUE)) %>%  
  separate(raw_speaker, c("speaker", "dialogue"), sep = "%", extra = "drop", fill = "left") %>%
  group_by(chapter, line = cumsum(!is.na(speaker))) %>%
  summarize(name = speaker[1], dialogue = str_c(dialogue, collapse = " "))

# KERO is typo, should be EERO
lines$name[lines$name == 'KERO'] <- "EERO"

lines <- lines %>%
  inner_join(names.df) %>%
  mutate(character = paste0(name, " (", type, ")"))

by_name_chap <- lines %>%
  count(chapter, character)

by_name_chap

# Barchart of how much characters speak across chapters
png("br7bar.png", width=1280,height=800)

ggplot(by_name_chap, aes(x=factor(character), y=n, fill=character))+
  geom_bar(stat = "identity") +
  facet_grid(. ~ chapter) +
  coord_flip() +
  theme(legend.position="none")

dev.off()


# Cluster dendrogram
name_chap_matrix <- by_name_chap %>%
  acast(character ~ chapter, fun.aggregate = length)

dim(name_chap_matrix)
norm <- name_chap_matrix / rowSums(name_chap_matrix)
h <- hclust(dist(norm, method = "manhattan"))
png("b7dendr.png", width=1280,height=800)
plot(h)
dev.off()


# Timeline
ordering <- h$labels[h$order]
ordering

chaps <- by_name_chap %>%
  filter(n() > 1) %>%    # scenes with > 1 characters
  ungroup() %>%
  mutate(chapter = as.numeric(factor(chapter)),
         character = factor(character, levels = ordering))

png("b7timeline.png", width=1280,height=800)
ggplot(chaps, aes(chapter, character)) +
  geom_point(aes(colour = character), size = 5, show.legend = F) +
  scale_x_continuous(breaks=1:14) +
  geom_path(aes(group = chapter)) 
dev.off()


# Heatmap
cooccur <- name_chap_matrix %*% t(name_chap_matrix)

png("b7heat.png", width=1280,height=800)
heatmap(cooccur, margins = c(20,15))
dev.off()


# Network
g <- graph.adjacency(cooccur, weighted = TRUE, mode = "undirected", diag = FALSE)
V(g)$lec_community <- as.character(leading.eigenvector.community(g)$membership)
V(g)$vbetweenness <- igraph::betweenness(g, directed = F)
E(g)$ebetweenness <- edge.betweenness(g, directed = F)
V(g)$centrality <- igraph::evcent(g, weights = V(g)$weight)$vector
V(g)$Label <- V(g)$name
write.graph(g, 'b7.graphml', format = "graphml")

# Plotting network with  geomnet
library(geomnet)
gV <- get.data.frame(g, what=c("vertices"))
gE <- get.data.frame(g, what=c("edges"))

# Merge edges and vertices
gnet <- merge(
  gE, gV,
  by.x = "from", by.y = "Label", all = TRUE
)

gnet$shortname <- sapply(gnet$name, function(x) strsplit(x, " \\(")[[1]][1])

png("b7network.png", width=1280,height=800)
ggplot(data = gnet,
       aes(from_id = from, to_id = to)) +
  geom_net(
    ecolour = "lightyellow",
    aes(
      colour = lec_community, 
      group = lec_community,
      linetype = lec_community,
      fontsize = 6,
      linewidth = vbetweenness / 10 + 0.2,
      size = centrality,
      label = shortname
    ),
    vjust = -0.75, alpha = 0.2,
    layout = 'fruchtermanreingold'
  ) +
  theme_net() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "black")) +
  scale_colour_brewer("community", palette = "Set1") +
  guides(linetype = FALSE)



