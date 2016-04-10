setwd("~/information system/fourth year/second semester/application methods for data analysis/scond ass")
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
mods <- sapply(0:ecount(g), function(i){
  g2 <- delete.edges(g, ebc$removed.edges[seq(length=i)])
  cl <- clusters(g2)$membership
  modularity(g,cl)
})

# we can now plot all modularities
png(filename="lemis_mod_Grivan-Newman.png")
plot(mods, pch=20)
dev.off()
max(mods)

# Now, let's color the nodes according to their membership
g2<-delete.edges(g, ebc$removed.edges[seq(length=which.max(mods)-1)])
V(g)$color=clusters(g2)$membership

# Let's choose a layout for the graph
g$layout <- layout.fruchterman.reingold

# plot it
png(filename="lemis_plot_Grivan-Newman.png")
plot(g)
dev.off()

#amount of communities
i=max(clusters(g2)$membership)
x=1
while (x<=i)
{
  t<-table(clusters(g2)$membership)[x]
  print(t)
  x=x+1
}

#eigenvector community
leading.eigenvector.community(g)

lec$membership

mods1 <- sapply(0:ecount(g), function(i){
  cl <- lec$membership
  modularity(g,cl)
})
max(mods1)

png(filename="lemis_mod_eigenvector.png")
plot(mods1)
dev.off()

V(g)$color=lec$membership

# Let's choose a layout for the graph
g$layout <- layout.fruchterman.reingold

# plot it
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
