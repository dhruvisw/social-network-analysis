  
# Install packages
install.packages(c("igraph","readr","tidyr","RColorBrewer"))

#1. Read data from the Github repository csv files
library (readr)
library(igraph)

#2. Manage dataset
data <- read.csv(file.choose(), header=T)
B<-as.data.frame(table(data))
B1<-subset(B,Freq>0)

#3. Create an igraph object from the dataframes
library(igraph)
df<-graph_from_data_frame(B1, directed = FALSE)
E(df)$weight<-E(df)$Freq 
df

#1. igraph summary
gsize(df)
gorder(df)

#2. Nodelist
V(df)

#3. Edgelist
E(df)

#5. Adjacency matrix
df[c(1:10),c(1:10)]

df_bw<-betweenness(df, directed = FALSE)
V(df)$betweenness<-df_bw
V(df)$betweenness
which.max(df_bw)
df[c(1:10),c(1:10)]


#==================================================================#
#===================== Measuring Centrality =======================#
#==================================================================#
# In this section, you will measure the centrality of the igraph   #
# object, "df". You will be able to see how the theoretical   #
# concept of each centrality such as degree, eigenvector, and      #
# betweenness centrality is measured by the igraph.                #
#==================================================================#


#1. Degree centrality
# Network measures
degree(df, mode='all')

df_deg<-degree(df,mode=c("All"))
V(df)$degree<-df_deg
V(df)$degree
which.max(df_deg)


#2. Eigenvector centrality
df_eig <- evcent(df)$vector
V(df)$Eigen<-df_eig
V(df)$Eigen
which.max(df_eig)

#3. Betweenness centrality
df_bw<-betweenness(df, directed = FALSE)
V(df)$betweenness<-df_bw
V(df)$betweenness
which.max(df_bw)

DF<-as_long_data_frame(df)
DF


hist(V(df)$degree,
     col = 'green',
     main = 'Histogram of Node Degree',
     ylab = 'Frequency',
     xlab = 'Degree of Vertices')

#==================================================================#
#================== Measuring Network Structure ===================#
#==================================================================#
# In this section, you will measure the indicators of the network  #
# structure such as network density
#==================================================================#

#1. Network Density
edge_density(df) # Global density


#1. Plotting a network with the degree centrality

set.seed(1001)
library(RColorBrewer) # This is the color library
pal<-brewer.pal(length(unique(V(df)$Sender)), "Set3") # Vertex color assigned per each class number
plot(df,edge.color = 'black',vertex.label.cex = 0.9,
     vertex.color='yellow',
     vertex.size = df_deg, edge.width=sqrt(E(df)$weight/800),
     layout = layout.fruchterman.reingold, main="Network graph with degree centrality")


#1. Plotting a network with the eigenvector centrality

set.seed(1001)
plot(df,vertex.label.color= "black", edge.color = 'green',vertex.label.cex = 1,
     vertex.color='orange', 
     vertex.size = df_eig*22, edge.width=sqrt(E(df)$weight/800),
     layout = layout.circle, main="Network graph with eigen vector centrality",)


#2. Plotting a network with the betweenness centrality

par(bg="white")
set.seed(1001)
plot(df,edge.color = 'lightblue',vertex.label.color= "black", vertex.label.cex =1, vertex.color='orange',
     vertex.color=pal[as.numeric(as.factor(vertex_attr(df, "Class")))],
     vertex.size = df_bw/4, edge.width=sqrt(E(df)$weight/800),
     layout = layout.sphere, main = "Network with betweenness centrality")


#3.1. between degree and betweenness centrality
plot(V(df)$degree, V(df)$betweenness)

#3.2. between degree and eigenvector centrality
plot(V(df)$degree, V(df)$Eigen)

#1. Louvain clustering
cnet <- cluster_edge_betweenness(df) # You can check which vertices belongs to which clusters.

#2. Plotting the Betweenness Centrality network with the community detection

set.seed(1001) # To duplicate the computer process and create exactly the same network repetitively you should set the seed.
plot(cnet, df, edge.color = 'lightblue',vertex.label.cex =0.9,
     vertex.color='yellow', vertex.label.color='black',
     vertex.size = df_bw/3, edge.width=sqrt(E(df)$weight/800),
     layout = layout.fruchterman.reingold, main = "Betweenness Centrality network with the community detection")

set.seed(1001) # To duplicate the computer process and create exactly the same network repetitively you should set the seed.
plot(cnet, df, edge.color = 'blue',vertex.label.cex =0.9,
     vertex.color='yellow', vertex.label.color='black',
     vertex.size = df_bw/3, edge.width=sqrt(E(df)$weight/800),
     layout = layout.circle, main = "Betweenness Centrality network with the community detection (part 2)")

#interactive visualization

-----#visualization
tkplot.fit.to.screen( width = NULL, height = NULL)
tkplot(df, vertex.color = rgb(0.8,0.2,0.2,0.9), directed = T)


set.seed(1001)
tkplot(df, edge.color ='blue', vertex.label.cex = 0.9, vertex.size = hs*50, 
       vertex.color = 'yellow', vertex.label.color = 'black', edge.color = 'lightblue', layout = layout.fruchterman.reingold)
#-------or-------


dfha<-graph_from_data_frame(B1, directed = T)

hs <- hub_score(dfha)$vector
as <- authority.score(dfha)$vector
par(mfrow=c(1,2))
set.seed(1001)
plot(dfha,
     vertex.label.color='black',
     vertex.size=hs*30,
     main = 'Hubs',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)
set.seed(1001)
plot(dfha,
     vertex.label.color='black',
     vertex.size=as*30,
     main = 'Authorities',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)
par(mfrow=c(1,1))






