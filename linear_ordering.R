#Importing data, using name of beers as rownames instead of column
beer<- read.csv(file="piwo.csv", header = TRUE, sep=";")
rownames(beer)<- beer[,1]
beer[,1]<- NULL

#Looking at data 
dim(beer)
tail(beer,3)
str(beer)


#The higher value of feature, the better 
beer[,1:2]<- beer[,1:2]*(-1)


##### 1.Standardized sum method

#Standardization data in order to secure comprasion x
beer_st<- scale(beer) #(x - mean(x)) / sd(x))

#Suming objects
sum_beer<- c()
for (i in 1:20){
sum_beer[i]<- mean(beer_st[i,])
}
names(sum_beer)<- rownames(beer)
sum_beer

#Transforming indicator to <0,1> values and sorting
rank<- c()
rank<- (sum_beer-min(sum_beer))/(max(sum_beer-min(sum_beer)))
rank_sort<- sort(rank, decreasing =  T)

#Result
rank_sort


##### 2.Hellwig method

#Standardization data, same as in first method
beer_st<- scale(beer)

#Finding max value of the features
best<-c()
for (i in 1:5){
  best[i]<- max(beer_st[,i])
}
best

#Finding distance beetwen objects and max value
di0<- c()
for( i in 1:20){
  data<- rbind(beer_st[i,],best)
  di0[i] <- dist(data)
}
names(di0)<- rownames(beer)
di0

#Finding "max" distance
d0<- mean(di0)+ 2*sd(di0)

#Calculating indicator value for objects
hellwig<- 1- di0/d0

#Result
rank_hellwig<- sort(hellwig, decreasing = T)
rank_hellwig


##### 3.Sum of ranks method

#Ranking features of the beers
beer_rank<- beer_st
for (i in 1:ncol(beer)){
  beer_rank[,i]<- rank(beer[,i])
}
beer_rank

#Mean ranking of objects
rank<- c()
for (i in 1:20){
  rank[i]<- mean(beer_rank[i,])
}
names(rank)<- rownames(beer_rank)
rank

#Result
final_rank<- sort(rank, decreasing = T)
final_rank


###### 4.TOPSIS

#Normalization, other formula than before
beer_norm<- beer

for (i in 1:20){
  for (j in 1:5){
    beer_norm[i,j]<- beer[i,j]/(sqrt (sum(beer[i,]^2)))
    
  }
}

#Finding min and max standard
best<- c()
worst<- c()
for (i in 1:5){
  best[i]<- max(beer_norm[,i])
  worst[i]<- min(beer_norm[,i])
}
best
worst

#Calculating distnace beetwen objects and max/min standard
di_plus<- c()
di_minus<- c()
for( i in 1:20){
  data<- rbind(beer_norm[i,], best)
  data2<- rbind(beer_norm[i,], worst)
  di_plus[i] <- dist(data)
  di_minus[i] <- dist(data2)
}
names(di_plus)<- rownames(beer)
names(di_minus)<- rownames(beer)
di_plus
di_minus

#Final rank, estimating objects similarity to perfect model
topsis_rank<- c()
topsis_rank<- di_minus/(di_plus+di_minus)
topsis_rank<- sort(topsis_rank, decreasing = T)
topsis_rank



