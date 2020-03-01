#setwd(getwd())
options(warn = -1)
options(stringsAsFactors = FALSE)
library(usethis)
library(devtools)
library(NLP)
#install.packages("igraph")
library(igraph)
library(htmltools)
#install.packages("networkD3")
library(networkD3)
library(htmlwidgets)
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
library(rjson)
#install.packages("remotes")
#remotes::install_github("hadley/emo")
library(emo)
library(remotes)
library(openxlsx)
library(utf8)
#devtools::install_github( "ThinkRstat/utf8splain")
library(utf8splain)
#devtools::install_github("quanteda/quanteda") 
library(quanteda)
library(readtext)
library(stm)
#devtools::install_github("kbenoit/quantedaData")
library(quantedaData)
#install.packages("rgl", dependencies = TRUE)
#devtools::install_url('https://cran.r-project.org/src/contrib/Archive/rowr/rowr_1.1.3.tar.gz')
library(rowr)
library(dplyr)
library(janitor)
#install.packages("visNetwork")
library(visNetwork)
#update.packages()

#--------------------------------------------------------Read, write and load the tweets----------------------------------------------------

data_file <- openxlsx::read.xlsx("WhiteSupremacytweets.xlsx" , colNames= F)

colnames(data_file) <- c("username", "group", "name", "bio", "dateTime", "max_id", "tweet")

#assign the group tag
data_file[data_file == 0] <- "Pro" 
data_file[data_file == 1] <- "Anti"

#---------------------------------------------------------Split the Anti & Pro People------------------------------------------------------
data_file_all_pro <- subset(data_file, group == "Pro")
data_file_all_anti <- subset(data_file, group == "Anti")

# readr::write_excel_csv(data_file_all_pro,"Pro_WhiteSupremacytweets.csv", col_names = T) #write a PRO UTF file
# readr::write_excel_csv(data_file_all_anti,"Anti_WhiteSupremacytweets.csv", col_names = T) #write an ANTI UTF file

#-----------------------------------------------------------quanteda structural topic models--------------------------------------------------------
#Structural Topic Models

data_file_all_pro <- data_file_all_pro %>% dplyr::select(tweet) #%>% dplyr::select(bio, tweet)

data_file_all_anti <- data_file_all_anti %>% dplyr::select(tweet) #%>% dplyr::select(bio, tweet)


#-------------------------------------------------------- extract the emojis-----------------------------------------------------
#this variable is VERY IMPORTANT if you're doing group based analysis comment and uncomment based on your analysis!

data_context <- data_file$tweet #tweet texts COMBINED analysis!
#data_context <-  data_file_all_pro$tweet #Uncomment this for PRO tweets!
#data_context <- data_file_all_anti$tweet #Uncomment this for ANTI tweets!

dt_result <- gsub("[\\x{0000}-\\x{FFFF}]+","",data_context, perl = TRUE) #Extacts all the unicode from a column

emoji_pouch <- c() #empty vector to collect all emojis
emoji_keywords_pouch <- c() #empty vector to collect all emojis names
emoji_pouch_corpus <- as.data.frame(dt_result)
emoji_pouch_corpus <- emoji_pouch_corpus %>% mutate_all(funs(na_if(., ""))) %>% remove_empty(which= c("rows", "cols"))
#View(emoji_pouch_corpus)

#Manually add the new emojis in this json file. Download it (or push pull request in my github), read it in and same implmenetation.

#lib comes from Muan
emoji_json_file <- "https://raw.githubusercontent.com/ToadHanks/urmojis/master/traitslib/in_progress_lib.json"
json_data <- rjson::fromJSON(paste(readLines(emoji_json_file), collapse = "")) #read line by line make 

#-----------------------------------------------------json function-------------------------------------------------------------
#(requied)function to give emoji name if you pass the unicode or emoji as a character. 
#i.e. get_name_from_emoji("😋") output should be "yum"

get_name_from_emoji <<- function(emoji_unicode, emoji_data = json_data) {
  
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

for(i in seq(dt_result)){
  emoji_pouch <- c(emoji_pouch,emo::ji_extract_all(dt_result[i])) 
}

emoji_pouch <- Filter(length, emoji_pouch) #Remove the character(0) or rows with NA, it occurs due to the package

emoji_pouch_copy <- unlist(emoji_pouch, recursive = TRUE) #makes a copy of emoji_pouch, and converts to a vector

#extract the keywords per emojis

for(i in seq(emoji_pouch_copy)){
  emoji_keywords_pouch <- c(emoji_keywords_pouch, get_name_from_emoji(emoji_pouch_copy[i]))
}
emoji_keywords_pouch[is.na(emoji_keywords_pouch)] <- "0" #This makes easy to spot if there are MISSING fields in the names section

#------------------------------------------------- Trim the colored emojis to reduce clutter -----------------------------------------------

#Function to remove the skin tones
remove_all_skins <<- function(string, pattern) {
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

get_emoji_from_name <<- function(emoji_name, emoji_data = json_data) {

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

for(i in seq(emoji_keywords_pouch)){
  # print(emoji_keywords_pouch[i])
  # print(get_emoji_from_name(emoji_keywords_pouch[i]))
  # Sys.sleep(1)
  emoji_pouch_copy <- c(emoji_pouch_copy, get_emoji_from_name(emoji_keywords_pouch[i]))
}

emoji_pouch_copy[is.na(emoji_pouch_copy)] <- "0" #This makes easy to spot if there are MISSING fields in the names section

#---------------------------------------------------------- quanteda SBS, HC, SDP-----------------------------------
#Sort of a dumb approach, you have to comment/uncomment relevant variable based on which you using and re-rerun from beginning with doing same thing with data$context. 
#One has to be commented otherwise they will get overwritten.

#pro_emojis <- emoji_pouch_copy
#anti_emojis <- emoji_pouch_copy

#Similarities between texts (watch for comment and uncomment group tags)

#similar_emojis <- data.frame(matches= dplyr::intersect(pro_emojis, anti_emojis))
#readr::write_excel_csv(similar_emojis,"group_similarity_texts.csv", col_names = T) #similarity files

#Unique emojis between groups

# outersect <<- function(x, y) {
#   sort(c(x[!x%in%y],
#          y[!y%in%x]))
# }

#unique_emojis <- data.frame(unique= unique(outersect(pro_emojis, anti_emojis)))
#readr::write_excel_csv(unique_emojis,"group_dissimilarity_texts.csv", col_names = T) #dissimilarity files

# unique_by_group <<- function(x, y) {
#   pro <- unique(x[!x%in%y])
#   anti <- unique(y[!y%in%x])
#   data.frame(rowr::cbind.fill(pro, anti, fill = ''))
# }

# unique_to_each <- unique_by_group(pro_emojis, anti_emojis)
# colnames(unique_to_each) <- c("Pro", "Anti")
#readr::write_excel_csv(unique_to_each,"unique_to_each_group.csv", col_names = T) #unique by group files

# View(unique_to_each)

#------------------------------------------------------------Tf-idf-stuff--------------------------------------------------------

TagSet <- data.frame(emoticon= emoji_pouch_copy, stringsAsFactors = F) #change emoticons based on group, unique() should do, pro/anti for group

TextSet <- data.frame(tweet = emoji_pouch_corpus$dt_result, stringsAsFactors = F) 

tweets_dfm <- quanteda::dfm(TextSet$tweet)

tf_idf_mat <- tweets_dfm %>% 
  quanteda::dfm_select(TagSet$emoticon) %>% # only leave emoticons in the dfm
  quanteda::dfm_tfidf() %>%                 # weight with tfidf
  quanteda::convert("data.frame")      # turn into data.frame to display more easily

#View(tf_idf_mat)

#------------------------------------------------------------- One Column tf-idf ------------------------------------------------

col_sum <- colSums(tf_idf_mat[,-1]) #get the column sum for emoitcons, tf_idf is big matrix of documetns and terms
col_sum_names <- names(col_sum) #get the emoticons
col_zeroes <- colSums(tf_idf_mat[,-1] > 0) #get the frequency of where emoticons appears
total_documents <- nrow(tf_idf_mat[1]) #get the total documents

one_tf_idf <- c() #empty vector to keep aggregated tf-idf

#do the Summation(of emoticon at i) * logbase10(total documents / freq(of emoticon at i))
for(i in seq(col_sum)){
  one_tf_idf <- c(one_tf_idf, col_sum[[i]]*(log10(total_documents/col_zeroes[[i]])))
}
one_tf_idf <- data.frame(emoticons= col_sum_names, tf_idf= one_tf_idf)  #This gives us info about most used vs most relevant
one_tf_idf <- one_tf_idf[order(one_tf_idf$tf_idf, decreasing = T),]

#View(one_tf_idf[one_tf_idf$tf_idf>30,]) 

one_tf_idf <- one_tf_idf[one_tf_idf$tf_idf>30,] #trim above >30

#------------------------------------------------------ Network Graph------------------------------------------------------------------------  

#emo_nodes <- unique(emoji_pouch_copy) # pouch can be further shorten to only include emojis with freq. > 1 or somthing

#trim the emoji_pouch_copy based on one_tf_idf column
emoji_pouch_copy <- emoji_pouch_copy[emoji_pouch_copy %in% one_tf_idf$emoticons]

emo_mat <- matrix(emoji_pouch_copy, ncol = 2, byrow = T) #Uncomment this one to get only emojis themselves 
#emo_mat <- matrix(emoji_keywords_pouch, ncol = 2, byrow = T) #Uncomment this to get the emoji keywords *windows*
#View(emo_mat)

links <- data.frame(
          source= emo_mat[,1], target= emo_mat[,2]
        )
#links <- as.matrix(links)
#View(links)

# write the weighted edglist/network to a file 
relations <- links %>%
  dplyr::group_by(source, target) %>%
  dplyr::count()


colnames(relations) <- c("from", "to", "weight") #interactive ones havel values instead of weight, look into that!

relations <- subset(relations, relations$weight != 1) #Further trimming (& relations$weight !=2) the relations with higher edges, 0 resets. Use 0 to get EVERYTHING!

#View(relations)

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

emogg <- igraph::decompose.graph(emogg) #splits all of the graph objects into subgroups

vcount_indices <- c() 

#here we capture the biggest subgroup

for(i in seq(emogg)){
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

#plot the d3js version of the graph
clustors <- igraph::cluster_walktrap(emogg[[biggest_subgroup]])

members <- igraph::membership(clustors)
emogg_html <- igraph_to_networkD3(igraph::delete.vertices(simplify(emogg[[biggest_subgroup]]), isolated), group = members)

emogg_d3 <- networkD3::forceNetwork(
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

# degrees of network nodes
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
     layout = igraph::layout_with_fr(emogg[[biggest_subgroup]]),
)

#plot the vs.js version of the graph
emogg_visNet <- visNetwork::toVisNetworkData(igraph::delete.vertices(igraph::simplify(emogg[[biggest_subgroup]]), isolated))
emogg_visNet$nodes$font.size <- 40
# emogg_visNet$nodes$color <- "maroon" #Pro people
emogg_visNet$nodes$color <- "darkblue" #Anti people

vis_it <- visNetwork::visNetwork(nodes = emogg_visNet$nodes, edges = emogg_visNet$edges, height = "1500px", width = "200%") %>%
  visNetwork::visInteraction(hideNodesOnDrag = T, dragView = T, dragNodes = T, zoomView = T) %>%
  visNetwork::visOptions(highlightNearest = T, nodesIdSelection = T, collapse = T) %>%
  visNetwork::visEdges(smooth = T, arrows = "to") %>% #, length = c(100,500)
  visNetwork::visPhysics(stabilization = T)

#FOR JITTERNESS, MESS WITH PHYSICS and, IGRAPH LAYOUT!!! OTHERWISEREDUCE SIZE
visNetwork::visSave(vis_it, file= "visIt_networkGraph.html")

dev.off()
 
