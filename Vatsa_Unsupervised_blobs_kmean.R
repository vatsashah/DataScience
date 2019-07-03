getwd()

blobs.data=read.csv("C:/Users/Vatsa Shah/Documents/blobs.csv")

#using kmeans algorithm
result=kmeans(blobs.data,3) #making 3 clusters
result$cluster

ans.data=blobs.data
ans.data$cluster=result$cluster #adding 3rd column as the cluster nos.

library(ggplot2)

#plotting graph of V1 vs V2 showing clustering of data by color. 
ggplot(ans.data,aes(x=V1,y=V2))+
  geom_point(aes(color=cluster))

write.csv(ans.data,"Vatsa_blobs_Unsupervised_Ans.csv",row.names = FALSE)



