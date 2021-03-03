
rm(list=ls())
gc()
library(sqldf)
library(igraph)
library(SMUT)
##DBPEDIA download link for pagelink dataset
url_links <- "http://downloads.dbpedia.org/2016-10/core-i18n/sa/page_links_wkd_uris_sa.ttl.bz2"
download.file(url_links, "links.bz2")
rm(url_links)

##reading the data into links dataframe
links<-read.csv("links.bz2", header = TRUE, sep=" ", col.names = 1:4, encoding= "UTF-8")


##DBPedia download link for labels dataset which is devided into labels and category labels
url_labels_1 <- "http://downloads.dbpedia.org/2016-10/core-i18n/sa/category_labels_wkd_uris_sa.ttl.bz2"
url_labels_2 <- "http://downloads.dbpedia.org/2016-10/core-i18n/sa/labels_wkd_uris_sa.ttl.bz2" 
download.file(url_labels_1, "labels_1.bz2")
download.file(url_labels_2, "labels_2.bz2")
rm(url_labels_1, url_labels_2)

##reading the labels into labels dataframe
labels_1<-read.csv("labels_1.bz2", header = TRUE, sep=" ", col.names = 1:4, encoding="UTF-8")
labels_2<-read.csv("labels_2.bz2", header = TRUE, sep=" ", col.names = 1:4, encoding="UTF-8")
labels<- unique(rbind(labels_1, labels_2))


##Attaching labels to code-names
tab1<- sqldf("Select links.X3 as link2, labels.X3 as destination from links left join labels on links.X3=labels.X1")
tab2<-sqldf("Select links.X1 as link1, labels.X3 as source from links left join labels on links.X1=labels.X1")

##Creating Source-Destination Dataset
tab2<- tab2[-nrow(tab2),]
link_graph<- data.frame(tab2$source, tab1$destination)
link_graph<- link_graph[-nrow(link_graph),]
names(link_graph)<- c("Source", "Destination")

##Since one of the labels is NA (only one, need to check why)
link_graph<- link_graph[complete.cases(link_graph),]
r<- link_graph$Source==link_graph$Destination
link_graph<- link_graph[-r,]

##Creating Adjacency Matrix
new<- graph.edgelist(as.matrix(link_graph))
new1<- get.adjacency(new)

##emptying memory and ensuring that the RAM space is returned.
rm(new)
rm(link_graph)
rm(tab1)
rm(tab2)
rm(labels_1)
rm(labels_2)
rm(links)
rm(labels)
gc()

contingency_matrix<- as.matrix(new1)
rm(new1)

##Creating Contingency Matrix from Adjacency Matrix

##damping factor -> probability that a person would continue changing pages by clicking
##on the links. Alpha--> Probability of teleporting. alpha=1-damping factor. 
damping_factor = 0.85
N<- nrow(contingency_matrix)
r<-rowSums(contingency_matrix)==0
contingency_matrix[r,]<-1/N
rm(r)
gc()
contingency_matrix<- contingency_matrix/rowSums(contingency_matrix)
contingency_matrix<- (damping_factor)*contingency_matrix
contingency_matrix<- contingency_matrix+((1-damping_factor)/N)
##Hoot Hoot! Made the contingency matrix

##getting to steady state
x2<-matrix(c(1,rep(0, nrow(contingency_matrix)-1)), nrow=1)
for(i in 1:35)
{
  x2<- eigenMapMatMult(x2, contingency_matrix)
}
x2<- as.data.frame(x2)
colnames(x2)<-colnames(contingency_matrix)

suppressWarnings(Ordered_by_page_rank_probabilities<- x2[order(x2, decreasing = T)])
Top_Ten<-Ordered_by_page_rank_probabilities[1:10]
print("The Page Rank Value of the top ten pages in the network is in the Viewing Pane: \n")
suppressWarnings(View(Top_Ten))

##35th Iteration, stabilized to 6th decimal place
##Combined, based on page rank algorithm,the top 10 of the web-pages have 27% of the relevance of Sanskrit Wikipedia
##Confirm if you can make this conclusion.
##Steady State Probabilities of arriving at each page --> ordered by probability --> TOP 10. 
##Look into the translated names of the pages.

