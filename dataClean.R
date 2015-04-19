wd<-"C:/Users/Chris/Documents/Data Science Files/thickporth/"
#wd<-"Documents/Misc/Pitchfork Reviews/"
setwd(wd)

library(Narrative) # use install_github with private auth key to install
library(jsonlite)
library(tm)
library(xts)
library(zoo)
library(ggplot2)

data<-fromJSON(txt="Pitchfork_reviews.json")

data.subset<-as.data.frame(data$tiles$results)
data.subset<-as.data.frame(data.subset$pages)
data.subset<-data.subset$results

# missing meta data for this review, remove
data.subset<-data.subset[-7637]
# missing content for these reviews, remove
data.subset<-data.subset[-c(6818,7639,8464,15840)]

# filter out reviews of multiple albums, since these are usually historic, 
# and cause data issues later
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

rm(i,data,data.subset)

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

rm(data.filtered)

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

corp.clean<-tm::tm_map(corp,content_transformer(tolower))
corp.clean<-tm::tm_map(corp.clean,content_transformer(removeWords),stopwords("english"))
corp.clean<-tm::tm_map(corp.clean,content_transformer(removePunctuation),preserve_intra_word_dashes = T)
corp.clean<-tm::tm_map(corp.clean,content_transformer(removeNumbers))
corp.clean<-tm::tm_map(corp.clean,content_transformer(stripWhitespace))

#corp.clean<-tm::tm_map(corp.clean,stemDocument)

tdm<-TermDocumentMatrix(corp.clean)
tdm.2<-Narrative::tdmGenerator(seq(1,2,by=1),corp.clean)
#tdm<-tm::weightTfIdf(tdm)

#save.image(file = "corpus")

