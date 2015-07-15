


load("documents/misc/pitchfork\ reviews/thickporth/corpus")

library("topicmodels")

not <- 5


library("tm")
library("LDAvis")

td.mat<-as.matrix(t(tdm[,1:20]))
td.mat<-td.mat[rowSums(td.mat)>0,]

lda <- topicmodels::LDA(td.mat, k=not)

phi<-topicmodels::posterior(lda)$terms
theta<-topicmodels::posterior(lda)$topics
doc.length<-rowSums(td.mat)
term.frequency<-colSums(td.mat)
vocab<-Terms(tdm)

LDAvis.json <- LDAvis::createJSON(phi = phi,theta = theta,doc.length = doc.length,vocab = vocab,term.frequency = term.frequency)

write.table(LDAvis.json[[1]],file = "documents/misc/pitchfork\ reviews/thickporth/topics.json")
LDAvis::serVis(LDAvis.json,out.dir = "documents/misc/pitchfork\ reviews/thickporth/topics/")
LDAvis::serVis(LDAvis.json)
