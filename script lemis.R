install.packages('igraph')
library(igraph)
fa.data <- read.graph('lesmis.gml',format = 'gml')
g <- simplify(fa.data)
summary(fa.data)
V(g)$label
g$layout<-layout.fruchterman.reingold(g)
png(filename="lesmis_plot.png")
plot(g)
dev.off()

c<-g
clo <- closeness(c)
clo.score <- round( (clo - min(clo)) * length(clo) / max(clo) ) + 1
clo.colors <- rev(heat.colors(max(clo.score))) 
V(c)$color <- clo.colors[ clo.score ] 
png(filename="lesmis_plot_closeness.png")
plot(c)
dev.off()
maxCloseness = which((closeness(c))==max(closeness(c)))
V(c)[maxCloseness]$label

e<-g
eig<- evcent(e)
eig$vector<-eig$vector*1000
eig$colors <- rev(heat.colors(max(eig$vector))) 
V(e)$color<-eig$colors[eig$vector]
png(filename="lesmis_plot_evcent.png")
plot(e)
dev.off()
maxEv = which(max(evcent(e)$vector)==(evcent(e)$vector))
V(e)[maxEv]$label

b<-g
btw <- betweenness(b) 
btw.score <- (round(btw) + 1)
btw.colors <- rev(heat.colors(max(btw.score))) 
V(b)$color <- btw.colors[ btw.score ] 
png(filename="lesmis_plot_betweenness.png")
plot(b)
dev.off()
maxBetween=which(max(betweenness(b, v=V(b),directed=FALSE, weights=NULL,nobigint=FALSE,normalized=FALSE))==betweenness(b, v=V(b),directed=FALSE, weights=NULL,nobigint=FALSE,normalized=FALSE))
V(b)[maxBetween]$label


#Community
set.seed(100)
# Grivan-Newman algorithm
# 1st we calculate the edge betweenness, merges, etc...
ebc <- edge.betweenness.community(g, directed=F,weights = E(g)$weight)
V(g)$color<-ebc$membership

# Let's choose a layout for the graph
g$layout <- layout.fruchterman.reingold

# plot it
png(filename="lemis_plot_Grivan-Newman.png")
plot(g)
dev.off()

#amount of communities
i=max(ebc$membership)
x=1
while (x<=i)
{
  t<-table(ebc$membership)[x]
  print(t)
  x=x+1
}

#eigenvector community
lec<-leading.eigenvector.community(g)
lec
lec$membership
V(g)$color=lec$membership
g$layout <- layout.fruchterman.reingold
png(filename="lemis_plot_eigenvector.png")
plot(g)
dev.off()

#amount of communities
i=max(lec$membership)
x=1
while (x<=i)
{
  t<-table(lec$membership)[x]
  print(t)
  x=x+1
}
