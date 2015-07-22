
library("topicmodels")
library("tm")
library("LDAvis")

load("corpus")

not <- 20
N <- 100

td.mat <- as.matrix(t(tdm[,1:N]))
td.mat <- td.mat[rowSums(td.mat)>0,]

lda <- topicmodels::LDA(td.mat, k=not)#, method = "Gibbs")

phi<-topicmodels::posterior(lda)$terms
theta<-topicmodels::posterior(lda)$topics
doc.length<-rowSums(td.mat)
term.frequency<-colSums(td.mat)
vocab<-tm::Terms(tdm)

LDAvis.json <- LDAvis::createJSON(phi = phi,theta = theta,doc.length = doc.length,vocab = vocab,term.frequency = term.frequency)

rm(phi,theta,doc.length,term.frequency,vocab,not,lda)

#write.table(LDAvis.json[[1]],file = "topics.json")
LDAvis::serVis(LDAvis.json)

rm(LDAvis.json,td.mat)


