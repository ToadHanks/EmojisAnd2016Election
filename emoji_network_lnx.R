#Some of these you won't need it, but I am sure many of these are necessary
#setwd(getwd())
options(warn = -1)
options(stringsAsFactors = FALSE)
#install.packages("openssl")
library(devtools)
library(NLP)
#install.packages("tm")
library(tm)
#install.packages("igraph")
library(igraph)
library(tidygraph) #maybe useful if igraph fails???
#install.packages("qgraph")
library(qgraph)
library(htmltools)
#install.packages("networkD3")
library(networkD3)
library(htmlwidgets)
library(ggplot2)
#install.packages("dplyr")
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
#install.packages("stringr")
library(stringr)
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
#devtools::install_github( "ThinkRstat/utf8splain")
library(utf8splain)
library(dplyr)
#update.packages()

#--------------------------------------------------------Read, write and load the tweets----------------------------------------------------

data_file <- openxlsx::read.xlsx("WhiteSupremacytweets.xlsx" , colNames= F)

colnames(data_file) <- c("username", "Pro0_Anti1", "name", "bio", "dateTime", "NA", "tweet")

#---------------------------------------------------------Split the Anti & Pro People------------------------------------------------------
data_file_all_pro <- subset(data_file, Pro0_Anti1 == 0)
data_file_all_anti <- subset(data_file, Pro0_Anti1 == 1)

# readr::write_excel_csv(data_file_all_pro,"Pro_WhiteSupremacytweets.csv", col_names = T) #write a PRO UTF file
# readr::write_excel_csv(data_file_all_anti,"Anti_WhiteSupremacytweets.csv", col_names = T) #write an ANTI UTF file

#------------------------------------------------------------------------------------------------------------------------------------------
data_context <- data_file$tweet #tweet texts COMBINED analysis!
#data_context <- data_file$bio
#data_context <- data_file_all_pro$tweet #Uncomment this for PRO tweets!
#data_context <- data_file_all_anti$tweet #Uncomment this for ANTI tweets!


dt_result <- gsub("[\\x{0000}-\\x{FFFF}]+","",data_context, perl = TRUE) #Extacts all the unicode from a column

emoji_pouch <- c() #empty vector to collect all emojis
emoji_keywords_pouch <- c() #empty vector to collect all emojis names


#Manually add the new emojis in this json file. Download it (or push pull request in my github), read it in and same implmenetation.

emoji_json_file <- "https://raw.githubusercontent.com/ToadHanks/emojisLib_json/master/emojis.json"
json_data <- rjson::fromJSON(paste(readLines(emoji_json_file), collapse = "")) #read line by line make 

#-----------------------------------------------------json function-------------------------------------------------------------
#(requied)function to give emoji name if you pass the unicode or emoji as a character. 
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

emoji_pouch <- Filter(length, emoji_pouch) #Remove the character(0) or rows with NA, it occurs due to the package

emoji_pouch_copy <- unlist(emoji_pouch, recursive = TRUE) #makes a copy of emoji_pouch, and converts to a vector

############### Gephi experiment! This whole portion gets the emoji unicode, their picture,& give them their unicode names ####################
# 
# all_runes_unicode <- c()
# #all_runes_desc<- c()
# 
# implode <- function(..., sep='') { #Helper function
#   paste(..., collapse= sep)
# }
# 
# for(i in 1:length(emoji_pouch_copy)){
#   
#   if(length(runes(emoji_pouch_copy[i])$rune) >= 2){
#     these <- c()
#     these <- c(these, runes(emoji_pouch_copy[i])$rune)
#     these <- implode(these, sep = " ")
#     all_runes_unicode <- c(all_runes_unicode, these)
#   }
#   else{
#     all_runes_unicode <- c(all_runes_unicode, runes(emoji_pouch_copy[i])$rune)
#   }
# }
# 
# all_runes_unicode[is.na(all_runes_unicode)] <- "U+MISSING" 
# 
# all_runes <- data.frame(all_runes_unicode)
# 
# emojis_with_unis <- data.frame(all_runes_unicode, emoji_pouch_copy)

#make the pictures and assign their unicodes into a folder in homedirectory-> pics
# emojiPlot <- function(photo_enc, emo_char){
#   par(bg= "transparent")
#   png(paste0("pics/",photo_enc, ".png"))
#   plot(photo_enc, xlim = c(0,0.025), ylim = c(0,0.025),type = 'n', axes = F, xaxt= "n", yaxt= "n", xlab = "", ylab = "")
#   text(x = 0.0125, y = 0.0125, paste(emo_char), cex = 22)
#   dev.off()
# }
# # save each picture
# for(i in 1:length(emojis_with_unis$all_runes_unicode)){
#   emojiPlot(emojis_with_unis$all_runes_unicode[i], emojis_with_unis$emoji_pouch_copy[i])
# }
# 
# with_png_ending <- paste(emojis_with_unis$all_runes_unicode, ".png", sep="")
# write.csv(with_png_ending, "node_identifiers.csv")

#############################################################################################################################################

#extract the keywords per emojis
for(i in 1: length(emoji_pouch_copy)){
  emoji_keywords_pouch <- c(emoji_keywords_pouch, get_name_from_emoji(emoji_pouch_copy[i]))
}

emoji_keywords_pouch[is.na(emoji_keywords_pouch)] <- "0" #This makes easy to spot if there are MISSING fields in the names section


#------------------------------------------------- Trim the colored emojis to reduce clutter -----------------------------------------------

#Function to remove the skin tones
remove_all_skins <- function(string, pattern) {
  str_replace_all(string, pattern, "000")
}

#remove these and their nativ renders at a positions
skin_tones <- c("medium_skin_tone", "fairly_dark_skin_tone", "dark_skin_tone", "fairly_light_skin_tone", "light_skin_tone", "_light","_dark","_medium","_fairly") 

emoji_keywords_pouch <- remove_all_skins(emoji_keywords_pouch, skin_tones[1])
emoji_keywords_pouch <- remove_all_skins(emoji_keywords_pouch, skin_tones[2])
emoji_keywords_pouch <- remove_all_skins(emoji_keywords_pouch, skin_tones[3])
emoji_keywords_pouch <- remove_all_skins(emoji_keywords_pouch, skin_tones[4])
emoji_keywords_pouch <- remove_all_skins(emoji_keywords_pouch, skin_tones[5])

emoji_keywords_pouch <- emoji_keywords_pouch[emoji_keywords_pouch != "000"] #free the memory

#It has to be this order, otherwise good strings will go bad in the variable containing keywords
emoji_keywords_pouch <- stringr::str_remove_all(emoji_keywords_pouch, skin_tones[6])
emoji_keywords_pouch <- stringr::str_remove_all(emoji_keywords_pouch, skin_tones[7])
emoji_keywords_pouch <- stringr::str_remove_all(emoji_keywords_pouch, skin_tones[8])
emoji_keywords_pouch <- stringr::str_remove_all(emoji_keywords_pouch, skin_tones[9])

#Reverse the function get_name... to get_emoji and rebuild the emoji_pouch
#(requied)function to give emoji name if you pass the unicode or emoji as a character. 
#i.e. get_name_from_emoji("yum") output should be "😋"

get_emoji_from_name <- function(emoji_name, emoji_data = json_data) {

  vector_of_emoji_names_and_characters <- unlist(
    lapply(json_data, function(x){
      x$char
    })
  )

  emoji_character <- unname(
    vector_of_emoji_names_and_characters[
      names(vector_of_emoji_names_and_characters) == emoji_name
    ]
  )

  return(emoji_character)
}

#reset the original emoji_...copy to include standard tones
emoji_pouch_copy <- c()

for(i in 1: length(emoji_keywords_pouch)){
  # print(emoji_keywords_pouch[i])
  # print(get_emoji_from_name(emoji_keywords_pouch[i]))
  # Sys.sleep(1)
  emoji_pouch_copy <- c(emoji_pouch_copy, get_emoji_from_name(emoji_keywords_pouch[i]))
}
emoji_pouch_copy[is.na(emoji_pouch_copy)] <- "0" #This makes easy to spot if there are MISSING fields in the names section

############################################################### Tf-idf stuff ##############################################

#library(dplyr)
#library(tidytext)
library(tm)
#install.packages("tidyr")
library(tidyr)

TagSet <- data.frame(emoji_pouch_copy)
colnames(TagSet) <- "emoticon"

TextSet <- data.frame(data_context)
colnames(TextSet) <- "tweet"

myCorpus <- tm::Corpus(tm::VectorSource(TextSet$tweet))
tdm <- tm::TermDocumentMatrix(myCorpus, control= list(stopwords=TRUE))

tdm_onlytags <- tdm[rownames(tdm)%in%TagSet$emoticon, ]

#tm::inspect(tdm_onlytags)
View(as.matrix(tdm_onlytags[1:tdm_onlytags$nrow, 1:tdm_onlytags$ncol]))

############################################################################################################################

#-----------------------------------------------Readying for Network Graph------------------------------------------------------------------------  

#emo_nodes <- unique(emoji_pouch_copy) # pouch can be further shorten to only include emojis with freq. > 1 or somthing

emo_mat <- matrix(emoji_pouch_copy, ncol = 2, byrow = T) #Uncomment this one to get only emojis themselves
#emo_mat <- matrix(emoji_keywords_pouch, ncol = 2, byrow = T) #Uncomment this to get the emoji keywords *windows*

################################# Gephi experiment! We make a csv edgelist file in emoji unicodes ####################################################

# emo_mat_uni_gephi <- matrix(emojis_with_unis$all_runes_unicode, ncol = 2, byrow = T) #Uncomment this to get the emoji keywords 
# 
# links <- data.frame(
#   source= emo_mat_uni_gephi[,1], target= emo_mat_uni_gephi[,2]
# )
# #links <- as.matrix(links)
# #View(links)
# 
# # write the weighted edglist/network to a file
# relations <- links %>%
#   group_by(source, target) %>%
#   count() 
# 
# colnames(relations) <- c("source", "target", "weight")
# relations <- subset(relations, relations$weight != 1) #Further trimming the relations with higher edges
# 
# write_csv(relations, "edgelist_weighted_emojis_gephi.csv") #write plain file
#readr::write_excel_csv(relations,"edgelist_weighted_emojis_gephi.csv") #write a UTF file

######################################################################################################################################################

links <- data.frame(
          source= emo_mat[,1], target= emo_mat[,2]
        )
#links <- as.matrix(links)
#View(links)

# write the weighted edglist/network to a file
relations <- links %>%
  group_by(source, target) %>%
  count() 

colnames(relations) <- c("from", "to", "weight")

relations <- subset(relations, relations$weight != 1) #Further trimming the relations with higher edges, 0 resets. Use 0 to get EVERYTHING!

readr::write_excel_csv(relations,"edgelist_weighted_emojis.csv") #write a UTF file WHOLE DATA
#readr::write_excel_csv(relations,"edgelist_weighted_emojis_keywords.csv") #write a UTF file with keywords WHOLE DATA *windows*


#Gets the nodes, associated edges, and weight
links_g <- read.table(file = "edgelist_weighted_emojis.csv", header = T, sep = ",")
#links_g <- read.table(file = "edgelist_weighted_emojis_keywords.csv", header = T, sep = ",") #Uncomment if you're making keywords file

emogg <- igraph::graph_from_data_frame(links_g, directed = T) 
emogg_copy <- emogg

#get the vertcies with most amount of edges
igraph::V(emogg)$indeg <- igraph::degree(emogg, mode = "in")
print("Vertex/ices with most edges: ")
igraph::V(emogg)[igraph::V(emogg)$indeg == max(igraph::V(emogg)$indeg)]

#################################################### Gephi Experiment. writes a graphml file ########################################################

# write.graph(emogg, "graphml_edgeList.graphml", "graphml")

#####################################################################################################################################################

emogg <- igraph::decompose.graph(emogg) #splits all of the graph objects into subgroups
vcount_indices <- c() 

#here we capture the biggest subgroup
for(i in 1:length(emogg)){
  vcount_indices <- c(vcount_indices, igraph::vcount(emogg[[i]]))
}
biggest_subgroup <- which.max(vcount_indices) #make the index of biggest subgroup

#Resolution has to be big to spread the nodes properly
png(filename = "static_networkGraph.png", width = 10000, height = 10000, res = 150)

#setting up the network attributes P.S. you can remove all of [[biggest_subgroup]] from down below to get a unified graph
# par(mfrow= c(1,2), cex= 1.25)
igraph::V(emogg[[biggest_subgroup]])$size <- 1
igraph::V(emogg[[biggest_subgroup]])$color <- "skyblue" 
igraph::V(emogg[[biggest_subgroup]])$frame.color <- "white"
igraph::E(emogg[[biggest_subgroup]])$arrow.mode <- 0
igraph::V(emogg[[biggest_subgroup]])$label.cex <- seq(0.5,5,length.out = 6)#XXXXX

isolated <- igraph::degree(igraph::simplify(emogg[[biggest_subgroup]]))==0
#main <- induced_subgraph(emogg[[biggest_subgroup]], V(emogg[[biggest_subgroup]])[components(emogg[[biggest_subgroup]])$membership == which.max(components(emogg[[biggest_subgroup]])$csize)])

#plot the html version of the graph
clustors <- igraph::cluster_walktrap(emogg[[biggest_subgroup]])
members <- igraph::membership(clustors)
emogg_html <- igraph_to_networkD3(igraph::delete.vertices(simplify(emogg[[biggest_subgroup]]), isolated), group = members)


emogg_d3 <- forceNetwork(
         Links = emogg_html$links,
         Nodes = emogg_html$nodes,
         Source = 'source',
         Target = 'target',
         NodeID = 'name',
         Group = 'group',
         opacity = 0.9,
         #Nodesize = #emogg_html$nodes$'value', missing, maybe igraph broke??
         opacityNoHover = 1,
         zoom = T,
         linkColour = "#ababab",
         colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);")
      )

htmlwidgets::saveWidget(emogg_d3, file= "interactive_networkGraph.html")

#degrees of network nodes
# deg <- degree(emogg_copy, mode = "all")
# View(deg)


#plot the static graph
plot.igraph(
     igraph::delete.vertices(simplify(emogg[[biggest_subgroup]]), isolated), 
     vertex.label= V(emogg[[biggest_subgroup]])$name, 
     #vertex.size = deg*6,
     rescale = FALSE,
     ylim = c(-18,18),
     xlim = c(-18,18),
     asp = 0,
     layout = layout_with_fr(emogg[[biggest_subgroup]]),
)
dev.off()
      