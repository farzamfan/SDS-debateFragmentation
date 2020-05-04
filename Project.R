library(rtweet)
library(dplyr)
library(igraph)
library(networkD3)
library(ggplot2)
library(ggplot2)
library(reshape2)
library(devtools)
#install_github("knapply/homophily")
library(homophily)
###############
###############
token <- get_token()
setwd("./Desktop/Social Data Science/Project/")
###############
###############
rt <- search_tweets("#کرونا", n = 18000, include_rts = FALSE, retryonratelimit = TRUE,token = token)
rt %>% filter(!protected & lang == 'fa') -> rt
users <- data.frame(matrix(NA,nrow = length(unique(rt$user_id)), dimnames=list(NULL, c("user_id")),ncol = 1))
users$user_id <- unique(rt$user_id)
users$screen_name <- unique(rt$screen_name)
timelinesC <- NULL
i <- 0
for (userid in users$user_id) {
  limit <- rate_limit(token=token, "statuses/user_timeline")
  print(i)
  if(limit$remaining < 16)
  {
    print("rate limit reached, waiting")
    save(timelinesC, file="fullTimelines.RData")
    Sys.sleep(15*60)
  }
  usertimeline <- get_timelines(user=userid, n=2000, token=token, check=FALSE)
  timelinesC <- rbind(timelinesC, usertimeline)
  i <- i+1
}
save(timelinesC, file="CoronaTimelines1304.RData")
###############
###############
rt <- search_tweets("#روحانی", n = 18000, include_rts = FALSE, retryonratelimit = TRUE,token = token)
rt %>% filter(!protected & lang == 'fa') -> rt
users <- data.frame(matrix(NA,nrow = length(unique(rt$user_id)), dimnames=list(NULL, c("user_id")),ncol = 1))
users$user_id <- unique(rt$user_id)
users$screen_name <- unique(rt$screen_name)
timelinesR <- NULL
i <- 0
for (userid in users$user_id) {
  limit <- rate_limit(token=token, "statuses/user_timeline")
  print(i)
  if(limit$remaining < 16)
  {
    print("rate limit reached, waiting")
    save(timelinesR, file="fullTimelines.RData")
    Sys.sleep(15*60)
  }
  usertimeline <- get_timelines(user=userid, n=2000, token=token, check=FALSE)
  timelinesR <- rbind(timelinesR, usertimeline)
  i <- i+1
}
save(timelinesR, file="RouhaniTimelines1304.RData")
###############
###############
rt <- search_tweets("#پزشکان_بدون_مرز", n = 18000, include_rts = FALSE, retryonratelimit = TRUE,token = token)
rt %>% filter(!protected & lang == 'fa') -> rt
users <- data.frame(matrix(NA,nrow = length(unique(rt$user_id)), dimnames=list(NULL, c("user_id")),ncol = 1))
users$user_id <- unique(rt$user_id)
users$screen_name <- unique(rt$screen_name)
timelinesS <- NULL
i <- 469
for (userid in users$user_id) {
  limit <- rate_limit(token=token, "statuses/user_timeline")
  print(i)
  if(limit$remaining < 16)
  {
    print("rate limit reached, waiting")
    save(timelinesS, file="fullTimelines.RData")
    Sys.sleep(15*60)
  }
  usertimeline <- get_timelines(user=userid, n=2000, token=token, check=FALSE)
  timelinesS <- rbind(timelinesS, usertimeline)
  i <- i+1
}
save(timelinesS, file="SansTimelines1304.RData")
###############
###############
###############
###############
###############
###############
load(file = "RouhaniTimelines1304.RData")
timeline <- timelinesR
###############
###############
###############
###############
###############
###############
# retweet network
users <- unique(timeline %>% select(user_id=user_id, screen_name=screen_name))
timeline %>% filter(retweet_user_id %in% users$user_id) -> seledges
seledges %>% filter(retweet_user_id != user_id) -> seledges
users %>% select(id=user_id, name=screen_name) -> vertices
#vertices <- vertices[-159,]
seledges %>% select(from=retweet_user_id, to=user_id) -> edges 
g <- graph_from_data_frame(edges,vertices=vertices,directed = F)
###############
###############
# reply network
users <- unique(timeline %>% select(user_id=user_id, screen_name=screen_name))
timeline %>% filter(reply_to_user_id %in% users$user_id) -> seledges
seledges %>% filter(reply_to_user_id != user_id) -> seledges
users %>% select(id=user_id, name=screen_name) -> vertices
#vertices <- vertices[-159,]
seledges %>% select(from=reply_to_user_id, to=user_id) -> edges 
g <- graph_from_data_frame(edges,vertices=vertices,directed = F)
###############
###############
mean(degree(g))
edge_density(g)
diameter(g)
mean_distance(g)
transitivity(g,type="average")
components(g)
Compactness <- function(g) {
  gra.geo <- distances(g) ## get geodesics
  gra.rdist <- 1/gra.geo  ## get reciprocal of geodesics
  diag(gra.rdist) <- NA   ## assign NA to diagonal
  gra.rdist[gra.rdist == Inf] <- 0 ## replace infinity with 0
  # Compactness = mean of reciprocal distances
  comp.igph <- mean(gra.rdist, na.rm=TRUE) 
  return(comp.igph)
}
Compactness(g)
comms <- cluster_louvain(g)
modularity(comms)
sd(betweenness(g))
mean(betweenness(g))
sd(closeness(g))
sd(evcent(g)$vector)
assortativity_nominal(g, type = membership(comms) ,directed = FALSE)
###############
###############          
V(g)$name[degree(g,mode = "in")==max(degree(g,mode = "in"))]
V(g)$name[degree(g,mode = "out")==max(degree(g,mode = "out"))]

x <- data.frame(In=degree(g,mode="in"),Out=degree(g,mode="out"))
data<- melt(x)
ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.25)
ggplot(data,aes(x=value, fill=variable)) + geom_histogram(bins = 15) + geom_vline(xintercept = mean(data$value))
ggplot(data,aes(x=variable, y=value, fill=variable)) + geom_boxplot()
###############
###############
comms <- cluster_louvain(g)
membership <- data.frame(matrix(membership(comms),ncol = 1,dimnames=list(NULL, c("membership"))))
h <- hist(membership$membership, col="gray", labels = 1:73, ylim=c(0, 300),xlim = c(0,80),breaks = 73)
text(h$mids,h$counts,labels=h$breaks, adj=c(0.5, -0.5))
vertices[,3] = membership(comms)
modularity(comms)
###############
###############
sizes(comms)
algorithm(comms)
merges(comms)
crossing(comms, g)
code_len(comms)
is_hierarchical(comms)

wc <- cluster_walktrap(g)
modularity(wc)
membership(wc)
plot(wc, g)
###############
###############
# permutation testing
N <- 1000
rndmods <- rep(NA,N)
for (i in seq(1,N))
{
  #Your code here
  rndmods[i] <- modularity(cluster_louvain(sample_degseq(out.deg=degree(g))))
}

#Your code here
hist(rndmods,breaks = 100,xlim=c(0.0,1))
abline(v=modularity(comms),col="red")
sum(rndmods >= modularity(comms))
mean(rndmods)
###############
###############
plot(g, vertex.label.color="black", vertex.label.cex=0.4, layout=layout_nicely(g), vertex.color = membership(comms),
     vertex.size=5, edge.curved=0.1, edge.width=1, edge.arrow.size=0.3)

plot(g, vertex.label.color="black", vertex.label.cex=0.4, layout=layout_with_graphopt(g),vertex.color = membership(comms), 
     vertex.size=5, edge.curved=0.1, edge.width=1, edge.arrow.size=0.3)

plot(g, vertex.label.color="black", vertex.label.cex=0.4, layout= layout_with_fr(g), 
     vertex.size=5, edge.curved=0.1, edge.width=1, edge.arrow.size=0.3,vertex.color = membership(comms))
###############
###############
graph2 <- igraph_to_networkD3(g)
graph2$nodes$comm <- as.character(membership(comms))
forceNetwork(Links=graph2$links, Nodes=graph2$nodes, NodeID="name", Group="comm", zoom=T)
###############
###############
betws <- betweenness(g, v = V(g), directed = F)
hist(betws,breaks = 200)
###############
###############
logbetws <- log(betws+1)/max(log(betws+1))
plot(g, vertex.color = gray(1-logbetws), vertex.label.cex=0.01, layout=layout_with_fr(g), 
     vertex.size=5, edge.curved=0.1, edge.width=1,charge = 0.01)
###############
###############
write_graph(g, file = "/Users/farzamf/Desktop/Social Data Science/Project/Sans1304reply.graphml", format = "graphml")
###############
###############
# two-sided barplot

metricsG <- c("Diameter","Average Path")
metricsI <- c("Edge Density","Mean Degree","Compactness")
metricsC <- c("Betweenness (avg)","Betweenness (sd)","Compactness","Eigenvector  centrality (sd)")
metricsM <- c("Connected Components","Avg.  Clustering Coefficient","Modularity","Assortativity Coefficient")

datasets <- rep(c("Corona 26.03","Corona 13.04","Rouhani 26.03","Rouhani 13.04","MSF 26.03","MSF 13.04"),13)

valueG <- c(8,8,7,8,8,5,3.042,3.167,3.1632,3.4207,3.0243,2.7841)
valueI <- c(0.014,0.018,0.0228,0.0203,0.0511,0.1362,77.5,77.47,28.8,25.78,80.56,13.62,2397028,0.2249,0.1959,0.153,0.2587,0.0953)
valueC <- c(3605.4,3039.9,772.3,731.1,1126.7,20.2,16645.3,13035.6,2974.4,2872.7,3689.4,88.9,0.2397028,0.2249,0.1959,0.153,0.2587,0.0953,0.0257741,0.0222,0.0398,0.0495,0.0391,0.1399)
valueM <- c(910,791,312,387,250,44,0.196,0.2053,0.209,0.2073,0.2342,0.266,0.6321,0.675,0.6626,0.6619,0.601,0.6196,0.8188,0.8383,0.8181,0.8829,0.7732,0.9425)

RvalueG <- c(7,8,7,8,6,9,2.775,2.898,2.8286,3.0746,2.4971,3.6737)
RvalueI <- c(0.0186,0.01838,0.035,0.0237,0.0693,0.0304,97.036,78.04,44.12,30.10402,109.2069,3.049,0.2397028,0.2249,0.1959, 0.153,0.2587,0.0953)
RvalueC <- c(3555.9,2914.3,800.7,743.1,1000.7,28.7,25015.2,15899.1,3394.8,3390.5,10049.5,106.5996,0.297,0.2675,0.2655,0.1995,0.3635,0.0742,0.0232,0.0388,0.0573,0.04019,0.0489,0.14376)
RvalueM <- c(632,633,209,315,125,52,0.1489761,0.1369,0.1598,0.1527,0.2244,0.2967,0.484,0.5021,0.5103,0.5763,0.4632,0.5593,0.6088,0.6128,0.6227,0.6574,0.5383,0.7892)

df = data.frame(matrix(NA,nrow = 78 ,ncol = 3))
df[,1] <- c(rep(metricsG,each=6),rep(metricsI,each=6),rep(metricsC,each=6),rep(metricsM,each=6))
df[,2] <- c(valueG,valueI,valueC,valueM)
df[,3] <- c(RvalueG,RvalueI,RvalueC,RvalueM)
df[,4] <- (df[,2]-df[,3])/df[,2]
colnames(df) <- c("datasets","2603","1304","diff")           
df$Group<-ifelse(df$diff>0,"Endorsement Network","Reply Network")
df[,1] <- paste(1:78,df[,1],datasets)
df$datasets <- factor(df$datasets, levels = df$datasets)

df[df["diff"] < -1,4] <- -1

twoSidedBarPlot <- ggplot(df,aes(x=datasets,y=diff,fill=Group))+geom_bar(stat="identity")+ylim(-1, 1)+labs(x = "Metrics - Datasets",y="Relative Difference")+scale_x_discrete(limits = rev(levels(df$datasets)))+coord_flip()+theme(legend.position = c(0.2, 0.1))
twoSidedBarPlot
ggsave(filename="twoSidedBarPlot.pdf", plot=twoSidedBarPlot, width=12, height=8, units="in")
