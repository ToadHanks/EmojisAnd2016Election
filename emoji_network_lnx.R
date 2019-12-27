#Some of these you won't need it, but I am sure many of these are necessary
#setwd(getwd())
options(warn = -1)
options(stringsAsFactors = FALSE)

#install.packages("devtools")
#library(devtools)
library(igraph)
library(tidygraph) #maybe useful if igraph fails???
#install.packages("qgraph")
library(qgraph)
library(htmltools)
#install.packages("networkD3")
library(networkD3)
library(htmlwidgets)
library(ggplot2)
library(dplyr)
#install_github("juliasilge/tidytext")
library(tidytext)
#install.packages("textclean")
#library(textclean)
#install_github("datastorm-open/visNetwork")
library(visNetwork)
library(Unicode)
library(stringi)
library(readr)
library(data.table)
#install_github("geoffjentry/twitteR")
#library(twitteR)
library(stringr)
library(rjson)
#install.packages("remotes")
#remotes::install_github("hadley/emo")
library(emo)
library(remotes)
library(openxlsx)
library(utf8)
#update.packages()

#----------------------------------------------Read, write and load the tweets----------------------------------------------------

data_file <- openxlsx::read.xlsx("WhiteSupremacytweets.xlsx" , colNames= F)

colnames(data_file) <- c("username", "gender1F0M", "name", "bio", "dateTime", "NA", "tweet")

da_encoded <- data_file$tweet #tweet texts 
  
dt_result <- gsub("[\\x{0000}-\\x{FFFF}]+","",da_encoded, perl = TRUE) #Extacts all the unicode from a column

emoji_pouch <- c() #empty vector to collect all emojis
emoji_name_pouch <- c() #empty vector to collect all emojis names


#Manually add the new emojis in this json file. Download it (or push pull request in my github), read it in and same implmenetation.

emoji_json_file <- "https://raw.githubusercontent.com/ToadHanks/emojisLib_json/master/emojis.json"
json_data <- rjson::fromJSON(paste(readLines(emoji_json_file), collapse = "")) #read line by line make 

#-----------------------------------------------------json function-------------------------------------------------------------
#(optional)function to give emoji name if you pass the unicode or emoji as a character. 
#i.e. get_name_from_emoji("😋") output should be "yum"

get_name_from_emoji <- function(emoji_unicode, emoji_data = json_data) {
  
  emoji_evaluated <- stringi::stri_unescape_unicode(emoji_unicode) 

  vector_of_emoji_names_and_characters <- unlist(
    lapply(json_data, function(x){
      x$char
    })
  )
  
  name_of_emoji <- attr(
    which(vector_of_emoji_names_and_characters == emoji_evaluated)[1],
    "names"
  )

  return(name_of_emoji)
}


#extract all of the emojis line by line, and fill the emoji pouch
for(i in 1:length(dt_result)){
emoji_pouch <- c(emoji_pouch,emo::ji_extract_all(dt_result[i])) 
} 

emoji_pouch <-Filter(length, emoji_pouch) #Remove the character(0) or rows with NA, it occurs due to the package

emoji_pouch_copy<- unlist(emoji_pouch, recursive = TRUE) #makes a copy of emoji_pouch, and converts to a vector

#you can see the emo package content if your uncomment this (OPTIONAL)
#
#keywords <- emo::ji_keyword
#keywords <- keywords[length(keywords)>1]
#emojis <- purrr::map_chr(keywords, function(x) paste0(emo::ji_name[x], collapse = ""))
#emojis

#extract the keywords per emojis
for(i in 1: length(emoji_pouch_copy)){
  emoji_name_pouch <- c(emoji_name_pouch, get_name_from_emoji(emoji_pouch_copy[i]))
}

emoji_name_pouch[is.na(emoji_name_pouch)] <- "0" # This makes easy to spot if there is NA fields in the names section

#-----------------------------------------------Readying for Network Graph---------------------------------------------------

emo_nodes <- unique(emoji_pouch_copy) # This can be further shorten to only include emojis with freq. > 1 or somthing

emo_mat <- matrix(emoji_pouch_copy, ncol = 2, byrow = T) #Uncomment this one to get only emojis themselves
#emo_mat <- matrix(emoji_name_pouch, ncol = 2, byrow = T) #Uncomment this to get the emoji keywords (Mainly for Windows OS)

links <- data.frame(
          source= emo_mat[,1], target= emo_mat[,2]
        )

#links <- as.matrix(links)
#View(links)

#write the weighted edglist/network to a file
relations <- links %>%
  group_by(source, target) %>%
  count() 

colnames(relations) <- c("from", "to", "weight")
relations <- subset(relations, relations$weight != 1) #Further trimming the relations with higher edges

readr::write_excel_csv(relations,"edgelist_weighted_emojis.csv") #write a UTF file

#Gets the nodes, associated edges, and weight
links_g <- read.table(file = "edgelist_weighted_emojis.csv", header = T, sep = ",")
emogg <- graph_from_data_frame(links_g, directed = T) 

#get the vertcies with most amount of edges
V(emogg)$indeg <- degree(emogg, mode = "in")
print("Vertex/ices with most edges: ")
V(emogg)[V(emogg)$indeg == max(V(emogg)$indeg)]

write.graph(emogg, "graphml_edgeList.graphml", "graphml") #writes a graphml file (OPTIONAL, experimenting with gephi)

emogg <- decompose.graph(emogg) #splits all of the graph objects into subgroups
vcount_indices <- c() 

#Here we capture the biggest subgroup
for(i in 1:length(emogg)){
  vcount_indices <- c(vcount_indices, vcount(emogg[[i]]))
}
biggest_subgroup <- which.max(vcount_indices) #make the index of biggest subgroup

#Resolution has to be big to spread the nodes properly
png(filename = "static_networkGraph.png", width = 10000, height = 10000, res = 150)

#setting up the network attributes
#par(mfrow= c(1,2), cex= 1.25)
V(emogg[[biggest_subgroup]])$size <- 1
V(emogg[[biggest_subgroup]])$color <- "skyblue" 
V(emogg[[biggest_subgroup]])$frame.color <- "white"
E(emogg[[biggest_subgroup]])$arrow.mode <- 0
isolated <- degree(simplify(emogg[[biggest_subgroup]]))==0
#main <- induced_subgraph(emogg[[biggest_subgroup]], V(emogg[[biggest_subgroup]])[components(emogg[[biggest_subgroup]])$membership == which.max(components(emogg[[biggest_subgroup]])$csize)])

#plot the html version of the graph
clustors <- cluster_walktrap(emogg[[biggest_subgroup]])
members <- membership(clustors)
emogg_html <- igraph_to_networkD3(igraph::delete.vertices(simplify(emogg[[biggest_subgroup]]), isolated), group = members)

emogg_d3 <- forceNetwork(
           Links = emogg_html$links,
           Nodes = emogg_html$nodes,
           Source = 'source',
           Target = 'target',
           NodeID = 'name',
           Group = 'group',
           opacity = 0.9,
           opacityNoHover = 1,
           zoom = T,
           linkColour = "#ababab",
           colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);")
        )

htmlwidgets::saveWidget(emogg_d3, file= "interactive_networkGraph.html")

#Plot the static graph
plot.igraph(
     igraph::delete.vertices(simplify(emogg[[biggest_subgroup]]), isolated), 
     vertex.label= V(emogg[[biggest_subgroup]])$name, 
     rescale = FALSE,
     ylim = c(-18,18),
     xlim = c(-18,18),
     asp = 0,
     layout = layout_with_fr(emogg[[biggest_subgroup]]),
)
dev.off()
