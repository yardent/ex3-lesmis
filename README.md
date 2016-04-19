#מטלה 3- עלובי החיים

הגרף שלנו מכיל את כל הדמויות מהרומן של הסופר הצרפתי ויקטור הוגו - עלובי החיים.
הגרף מייצג את הנוכחות של השחקנים בכל פרק. כלומר, אם שני שחקנים הופיעו באותו פרק, יופיע ביניהם קשת.

###הדמויות:
"Myriel"           
"Napoleon"         
"MlleBaptistine"   
"MmeMagloire"      
"CountessDeLo"    
"Geborand"         
"Champtercier"     
"Cravatte"         
"Count"            
"OldMan"          
"Labarre"          
"Valjean"          
"Marguerite"       
"MmeDeR"           
"Isabeau"         
"Gervais"          
"Tholomyes"        
"Listolier"        
"Fameuil"          
"Blacheville"     
"Favourite"        
"Dahlia"           
"Zephine"          
"Fantine"          
"MmeThenardier"   
"Thenardier"
"Cosette"          
"Javert"           
"Fauchelevent"     
"Bamatabois"      
"Perpetue"         
"Simplice"         
"Scaufflaire"      
"Woman1"           
"Judge"           
"Champmathieu"     
"Brevet"           
"Chenildieu"       
"Cochepaille"      
"Pontmercy"       
"Boulatruelle"     
"Eponine"          
"Anzelma"          
"Woman2"           
"MotherInnocent"  
"Gribier"          
"Jondrette"        
"MmeBurgon"        
"Gavroche"         
"Gillenormand"    
"Magnon"           
"MlleGillenormand" 
"MmePontmercy"     
"MlleVaubois"      
"LtGillenormand"  
"Marius"           
"BaronessT"        
"Mabeuf"           
"Enjolras"         
"Combeferre"      
"Prouvaire"        
"Feuilly"          
"Courfeyrac"       
"Bahorel"          
"Bossuet"         
"Joly"             
"Grantaire"        
"MotherPlutarch"   
"Gueulemer"        
"Babet"           
"Claquesous"       
"Montparnasse"     
"Toussaint"        
"Child1"           
"Child2"          
"Brujon"
"MmeHucheloup"    

הגרף:
```sh
fa.data <- read.graph('lesmis.gml',format = 'gml')
g <- simplify(fa.data)
summary(fa.data)
V(g)$label
g$layout<-layout.fruchterman.reingold(g)
png(filename="lesmis_plot.png")
plot(g)
dev.off()
```
![alt tag](/lesmis_plot.png)

##למי מהדמויות יש את מדד המרכזיות הגבוה ביותר

1. Betweeness- "Valjean"
```sh
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
```
![alt tag](/lesmis_plot_betweenness.png)  
2. Closeness- "Valjean"
```sh
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
```
![alt tag](/lesmis_plot_closeness.png)
3. Eigenceetor- "Gavroche"
```sh
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
```
![alt tag](/lesmis_plot_evcent.png)


##זיהוי קהילות

### Grivan-Newman algorithm
  1. רשת לפי קוד צבעים התואם את הקהילות
  ```
  set.seed(100)
  ebc <- edge.betweenness.community(g, directed=F,weights = E(g)$weight)
  V(g)$color<-ebc$membership
  g$layout <- layout.fruchterman.reingold
  png(filename="lemis_plot_Grivan-Newman.png")
  plot(g)
  dev.off()
  ```
  
   ![alt tag](/lemis_plot_Grivan-Newman.png)
  2. התקבלו 11 קהילות
  ```
  i=max(ebc$membership)
  x=1
  while (x<=i)
  {
    t<-table(ebc$membership)[x]
    print(t)
    x=x+1
  }
  ```
    קהילה 1- 10 קודקודים

    קהילה 2- 14 קודקודים
    
    קהילה 3- 10 קודקודים

    קהילה 4- 11 קודקודים

    קהילה 5- 10 קודקודים

    קהילה 6- 3 קודקודים

    קהילה 7- 1 קודקודים
    
    קהילה 8- 2 קודקודים
    
    קהילה 9- 13 קודקודים
    
    קהילה 10- 1 קודקודים
    
    קהילה 11- 2 קודקודים
  3. modularity 0.54
  ```
  ebc
  ```



### eigenvector community

  1. רשת לפי קוד צבעים התואם את הקהילות
  ```
  lec<-leading.eigenvector.community(g)
  lec$membership
  V(g)$color=lec$membership
  g$layout <- layout.fruchterman.reingold
  png(filename="lemis_plot_eigenvector.png")
  plot(g)
  dev.off()
  ```
  
   ![alt tag](/lemis_plot_eigenvector.png)
  2. התקבלו 8 קהילות
  ```
  i=max(lec$membership)
  x=1
  while (x<=i)
  {
    t<-table(lec$membership)[x]
    print(t)
    x=x+1
  }
  ```
    קהילה 1- 8 קודקודים

    קהילה 2- 19 קודקודים
    
    קהילה 3- 20 קודקודים

    קהילה 4- 11 קודקודים
    
    קהילה 5- 6 קודקודים
    
    קהילה 6- 1 קודקודים
    
    קהילה 7- 1 קודקודים
    
    קהילה 8- 11 קודקודים
    
  3. modularity max-0.53
  ```
  lec
  ```
