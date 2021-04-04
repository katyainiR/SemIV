library(igraph)
library(SMUT)
rm(list=ls())
gc()
##Network Model created on excel
trial<- read.csv("example network.csv")

#damping_factor = 1 
##Probability that the random surfer will continue to follow the links)
##Trial 1 : Basic Model that only includes A, B, C : No dangling Nodes
#trial_1<- trial[1:4,]
#tempa<- graph.edgelist(as.matrix(trial_1))
#temp<- as.matrix(get.adjacency(tempa))
#N<- nrow(temp)
#temp<- t(apply(t(temp), 2, function(i) { i / sum(i)}))
#temp<- (damping_factor)*temp
#temp<- temp+(1-damping_factor)/N
#example_of_transition_matrix_without_dangling_nodes <- temp


##Trial 1 : Basic Model that only includes A, B, C : With damping factor
#damping_factor = 0.85 ##The standard value of damping 
#trial_1<- trial[1:4,]
#tempa<- graph.edgelist(as.matrix(trial_1))
#temp<- as.matrix(get.adjacency(tempa))
#N<- nrow(temp)
#temp<- t(apply(t(temp), 2, function(i) { i / sum(i)}))
#temp<- (damping_factor)*temp
#temp<- temp+(1-damping_factor)/N
#example_of_transition_matrix_with_damping_factor_nodes<- temp

#x<-matrix(c(0.5,0,0.5), nrow=1)

#for(i in 1:90)
#{
 # x<- eigenMapMatMult(x, temp)
#}
#colnames(x)<-rownames(temp)

##Organizing the Row of Page Rank Values
#x<- as.data.frame(x)
#colnames(x)<-colnames(temp)

#Ordered_by_page_rank_probabilities<- x[order(x, decreasing = T)]
#Top<-Ordered_by_page_rank_probabilities[1]
#print("The Page Rank Value of the top page in the network is:")
#Top

##We get a relatability-stable measure at around 60th iteration for the simple network.



##More complicated, with dangling nodes. 
trial_2<- trial
damping_factor = 0.85
tempa<- graph.edgelist(as.matrix(trial_2))
temp<- as.matrix(get.adjacency(tempa))
N<- nrow(temp)

r<-rowSums(temp)==0
temp[r,]<-1/N
temp<- t(apply(t(temp), 2, function(i) { i / sum(i)}))
temp<- (damping_factor)*temp +(1-damping_factor)/N


x2<-matrix(c(0,0.5,0,0,0.5,0), nrow=1)

##Reaching Stationarity
for(i in 1:90)
{
  x2<- eigenMapMatMult(x2, temp)
}

x3<- as.data.frame(x2)
colnames(x3)<-colnames(temp)

Ordered_by_page_rank_probabilities_2<- x3[order(x2, decreasing = T)]
print("The pages ordered by their page ranks: \n")
suppressWarnings(View(Ordered_by_page_rank_probabilities_2))
##60 iterations give accuracy up to 4 digits.
