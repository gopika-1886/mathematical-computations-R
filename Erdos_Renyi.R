#SWMS 2025
#Erdos-Renyi random graph
####################################################################################
# Consider n-vertices V= {1,2,3,..., n}. The set of Edges of E is a subset 
#of V x V. The Graph G= (V,E) is the set of vertices and the edges between them.
#In this code we shall generate a random graph

#Consider the following method-
#1. Choose a p between 0 and 1. 
#2. Then you are given a coin which has p- probability of head and (1-p) probability of tail
#3. Now on a blank sheet, you draw 10 points V1,V2,...,V10
#4. Now for each pair (Vi,Vj) you toss the coin you got in (3), draw a line between Vi and Vj if you get a "HEAD" in the toss 
#5. You don't draw a line between Vi and Vj if your toss lands "TAIL"
##################################################################################

# choose a random number between (0,1)
p <- runif(1)
# assign Probability of heads and tail
prob_of_head <- p
prob_of_head
prob_of_tail <- 1-prob_of_head
prob_of_tail

#Now let's make a coin which has the above probabilities of HEAD and TAIL respectively
coin_toss_outcome <- sample(c("HEAD","TAIL"), 1, prob = c(prob_of_head, prob_of_tail) )
coin_toss_outcome
#This variable would give a random outcome HEAD/TAIL with the above specified probability

#Now let's create the graph with 10 vertices
number_of_vertices<- 10

#Now constructing an adjacency matrix
adj_matrix <- matrix(NA, nrow=10, ncol=10)
adj_matrix
for(i in 1:(number_of_vertices-1)){
  for(j in (i+1):number_of_vertices) {
    print(c(i,j))
    junk = sample(c("HEAD","TAIL"), 1, prob = c(prob_of_head,prob_of_tail))
    if(junk=="HEAD")
    adj_matrix[i,j] = 1
    else
      adj_matrix[i,j]= 0
    adj_matrix[j,i] = adj_matrix[i,j]
  }
}
diag(adj_matrix) <- 0 #This is because a vertex cannot have an edge with itself
#Now let's look at the adjacency matrix
adj_matrix

#Now let's construct our graph using this adjacency matrix
#a#
library(igraph) #R package to visualize and work with graph/network like objects
ER_graph <- graph_from_adjacency_matrix(adj_matrix,mode="undirected")
ER_graph
##b## plot the graph
plot(ER_graph, edge.arrow.size=0.5, vertex.color="yellow", vertex.size=10, 
     vertex.frame.color="black", vertex.label.color="black")

#c#The number of edges in the graph
gsize(ER_graph) 
##d# degrees
degree(ER_graph)
#e#The degree distribution of the graph
hist(degree(ER_graph), main="Degree distribution of Erdos-Renyi graph") 

