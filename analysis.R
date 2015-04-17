
wd<-"Documents/Misc/Pitchfork Reviews/"
setwd(wd)

#install.packages("jsonlite")
#install.packages("tm")

library(jsonlite)
library(tm)
library(xts)
library(zoo)
library(ggplot2)

addToMetaData <- function(corpus,vector,tag){
  
  if(length(corpus) != length(vector)){
    stop("Please provide a vector of equal length to your corpus")
  }
  
  i<-1
  while(i <= length(vector)){
    meta(corpus[[i]],tag,type="indexed")<-vector[[i]]
    i<-i+1
  }
  
  return(corpus)
}

data<-fromJSON(txt="Pitchfork_reviews.json")

data.subset<-as.data.frame(data$tiles$results)
data.subset<-as.data.frame(data.subset$pages)
data.subset<-data.subset$results

data.subset<-data.subset[-7637]

i<-1
data.filtered<-data.frame()
while(i<length(data.subset)){
  print(i)
  if(length(data.subset[[i]]["rating/_source"][[1]][[1]])>1){
    i<-i+1
    next
  }else{
    data.filtered<-append(data.filtered,data.subset[i])
  }
  i<-i+1
}

rm(data)

# review content
reviews<-lapply(data.filtered,function(x) x$review)

# meta data as data frames
years<-lapply(data.filtered,function(x) x$year)
albums<-lapply(data.filtered,function(x) x$album)
authors<-lapply(data.filtered,function(x) x$author)
ratings<-lapply(data.filtered,function(x) x$rating)
labels<-lapply(data.filtered,function(x) x$label)
covers<-lapply(data.filtered,function(x) x$album_cover)
bands<-lapply(data.filtered, function(x) x$band)
published<-lapply(data.filtered,function(x) as.POSIXct(x$publish_date,format="%B %d, %Y"))

rm(data.subset,data.filtered)

corp<-Corpus(VectorSource(reviews))

corp<-addToMetaData(corp,vector = years,tag = "year")
corp<-addToMetaData(corp,vector = albums,tag = "album")
corp<-addToMetaData(corp,vector = authors,tag = "author")
corp<-addToMetaData(corp,vector = ratings,tag = "rating")
corp<-addToMetaData(corp,vector = labels,tag = "label")
corp<-addToMetaData(corp,vector = covers,tag = "cover")
corp<-addToMetaData(corp,vector = bands,tag = "band")
corp<-addToMetaData(corp,vector = published,tag = "publish_date")

rm(reviews,years,albums,authors,ratings,labels,covers,bands,published)

corp<-tm_map(corp,content_transformer(removePunctuation))
corp<-tm_map(corp,content_transformer(removeNumbers))

corp<-tm_map(corp,content_transformer(tolower))

tdm<-TermDocumentMatrix(corp)

search.result<-as.matrix(t(tdm["terms",]))

search.xts<-xtsGenerate(unlist(meta(corp,"publish_date")),search.result)
search.xts.aggregate<-xtsAggregate(search.xts,time_aggregate = "month",normalisation = F)

ggplot
