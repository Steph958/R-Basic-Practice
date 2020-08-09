

#Hierarchical clustering

#�i�ֳt�s���[��ȻP�U��쪺���Y
#���s���G�i��Q�H�U�ѼƼv�T�G
    #�p��Z������k
    #���s�̾�
    #���ƭȪ��j�p
#�i��|�J�쪺���D�G
    #���ɷ|���ӲM���n�p����Τ��s���G



mtcars.mxs<-as.matrix(mtcars)
d<-dist(mtcars.mxs) #�w�]��euclidean
head(d)

d<-dist(mtcars.mxs, method="manhattan") #�p��manhattan�Z��
head(d)

par(mar=rep(2,4),mfrow=c(1,1))
hc<-hclust(dist(mtcars.mxs)) 
#�i��method�ѼƳ]�w�E�X��k�A�w�]��complete
plot(hc)

par(mar=rep(2,4),mfrow=c(1,1))
hc<-hclust(dist(mtcars.mxs),method="average") 
#�E�X��k���p�⥭���Z��
plot(hc)

clusterCut <- cutree(hc, k=5) #��5�s
sort(clusterCut)

ggplot()+geom_point(data=mtcars,
                    aes(x=hp,y=mpg,color=as.factor(clusterCut)))


clusterCut <- cutree(hc,h =4) #���b����=4���a��]�Z��=4�^
sort(clusterCut)



par(mar=rep(0.2,4),mfrow=c(1,1))
heatmap(mtcars.mxs)


distxy <- dist(mtcars.mxs)
hClustering <- hclust(distxy)
plot(hClustering)
