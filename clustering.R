##########################Airlines Assignment##########################
library(xlsx)
EastWestAirlines<-read.xlsx("D://ExcelR Data//Assignments//Clustering//EastWestAirlines.xlsx",sheetName = "data" )
View(EastWestAirlines)
km<-kmeans(EastWestAirlines,4)
str(km)
install.packages("animation")
library(animation)
km<-kmeans.ani(EastWestAirlines,4)
km$centers


#elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(nor_data)-1)*sum(apply(nor_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:11) wss[i] = sum(kmeans(nor_data, centers=i)$withinss)
plot(1:11, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

# selecting K for kmeans clustering using kselection
install.packages("kselection")
library(kselection)
k <- kselection(iris[,-5], parallel = TRUE, k_threshold = 0.9, max_centers=12)
?kselection
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=4)
k <- kselection(iris[,-5], parallel = TRUE, k_threshold = 0.9, max_centers=12)
k
##normalize the data using scale function
nor_data<-scale(airlinesdata[,1:11])
View(nor_data)
fit1<-kmeans(nor_data,5)
fit1
final2<-cbind(airlinesdata,fit1$cluster)
View(final2)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
final3
aggregate(airlinesdata[,1:11], by=list(fit1$cluster), FUN=mean)

write.xlsx(final3,file = "airlines_cluster.xlsx")


#####################Crime data assignment###################
crime_data<-read.csv(file.choose())
View(crime_data)
crimedata_new<-crime_data[,-1]
View(crimedata_new)
#normalize the data
norm_data<-scale(crimedata_new[,1:4])
View(norm_data)
##distance
dist11<-dist(norm_data,method = "euclidean")
dist11

fit11<-hclust(dist11,method = "complete")
#plot the dendrogram
plot(fit11)
plot(fit11,hang=-1)
groups11<-cutree(fit11,k=5)
rect.hclust(fit11,k=5,border = "red")
membership_crime<-as.matrix(groups11)
final11<-cbind(crime_data,membership_crime)
View(final11)
final12<-setcolorder(final11,neworder = c("membership_crime"))
View(final12)
write.xlsx(final12,file = "crime_cluster.xlsx")
