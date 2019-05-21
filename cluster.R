## Read in Data


## Why would I want to cluster food data?
##  One reason may be that I am a health eater and I want to eat a meal
##   that is balanced in calories, high in protein, etc. each day but
##   I want to eat different meals each day. So, I can pick one item
##    in each food cluster.

## Other reasons also exist.
data <- read.csv("foodnutrient.csv")
data

rownames(data)=data[,1]
data=data[,-1]
data=scale(data)  ## should i standardize?  calories, protein, fat, calcium, iron
data

## Explore the data
summary(data)
rho=cor(data)
rho

require(graphics)

## Single Linkage

## Step 1 - Compute a distance matrix
D=dist(data)

# Step 2 - Run hclust()
hc<-hclust(D,"single")
summary(hc)

# Step 3 - Plot dendrogram
plot(hc)

# here is the interpretation part, you can see that foods which are next to each other are most
#    similar.  there is "no cluster" so to speak, but if you look at canned chicken, that is really
#      close to canned tuna (they are very similar!), so you could replace one with the other

## note that this only works with a limited number of foods (not 5000 foods)

# Step 4 - choose number of clusters
memb<-cutree(hc,k=4)
memb

# Step 5 - get clustser centers
cent<-NULL
for (k in 1:4){cent<-rbind(cent,colMeans(data[memb==k,,drop=FALSE]))}
cent  # same as Table 7.21

## Step 6 - Calculate sum of total SS . within SS for each cluster (compare to k-means below)

one=sum((data[memb==1,,drop=FALSE]-cent[1,])^2)
two=sum((data[memb==2,,drop=FALSE]-cent[2,])^2)
three=sum((data[memb==3,,drop=FALSE]-cent[3,])^2)
four=sum((data[memb==4,,drop=FALSE]-cent[4,])^2)
  
tss_single=one+two+three+four  ## total sum of squares from cluster centroid (mean in this case)
# remember, single linkage has no guarantee for fit, this could be bad.

## kmeans clustering
library(flexclust)

# Step 1 - Run kmeans and analyze clusters
cl=kmeans(data,4)  ## let's keep same number of clusters as before
cl
## notice the within sum of squares...should be lower than single linkage  (George Foreman GUARANTEE)

# Step 2 - Plot clusters

plot(data, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)  ## this only does calories and protein


text(data,rownames(data),col=cl$cluster)



# Step 3 - Choose k  (plot total sum of squares)
tss<-rep(0,8)
for (k in 1:8){tss[k]=kmeans(data,k)$tot.withinss}
plot(1:8,tss)

## you want to see where it no longer decreases much (maybe 4 or 5 clusters), notice single linkage
## fits worse (higher tss) than all except 1 cluster.  it even does worse than 2!

# Step 4 - Interpret clusters
cl$centers

## here, you want to look at the centers...notice cluster 2 has really "high calorie food, high fat, low calclium"
## if we wanna get strong/lean, we want to pick from cluster 1, "high protein, low calorie"
#note that we don't know what the foods are in each cluster yet until we plot them or list them.
cl$cluster

## now we see that cluster 2 has things like hamburger and braised beef
#   cluster 3 has things like broiled chicken (healthy), canned salmon, baked bluefish (yum)

# Step 5  - Plot clusters with pca (this makes your intreptation even better)

## here we are putting food names to the clusters...it helps people understand.
library(pca3d)
?pca3d
pca <- prcomp(data, scale.= TRUE )
pca2d( pca, group=cl$cluster )
pca2d( pca, group=cl$cluster,show.labels=TRUE )
pca3d( pca, group=cl$cluster,show.labels=TRUE )

## make your statements in step 4 combine with step 5
## you may also want to do PCA to rename the axes. you should know how to do this!
# congratulations - very few people can explain PCA and clustering together. this will pay 
#         you dividends down the road in analytics.  Remember to B>0 (be positive!)

