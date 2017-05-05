library("tm");
library("textreuse");
library("wordnet");
library("zipfR");
library("NLP");
library("openNLP");
library("wordcloud");
library("stringr");

data(acq);

# try the funtions in lecture 8
inspect(acq);
acq1<-acq[[1]];
acqdtm<-DocumentTermMatrix(acq);
inspect(acqdtm[1:15,1:5]);
acq1tf<-termFreq(acq1);


# clean the documents
Acq<-tm_map(acq,content_transformer(tolower));
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x);
removeBackslashN<-content_transformer(function(x,
pattern)gsub(pattern, " ", x));
Acq <- tm_map(Acq, content_transformer(removeNumPunct));
Acq <- tm_map(Acq, removeBackslashN, "\n");
Acq <- tm_map(Acq, removeWords, stopwords('english'))

# find the 15 longest documents 
wordNum<-c()
#find the 15 longest documents before cleaning
for(i in 1:length(acq)){
     wordNum<-c(wordNum,wordcount(acq[[i]]))
}
order(wordNum,decreasing = TRUE)[1:15]
#find the 15 longest documents after cleaning
wordNum<-c()
for(i in 1:length(Acq)){
     wordNum<-c(wordNum,wordcount(Acq[[i]]))
}
order(wordNum,decreasing = TRUE)[1:15]

# display the dendrogram, tdm1 could be the term-document-matrix of any document of 15 longest documents
Acqtdm<-TermDocumentMatrix(Acq,control=list(wordLengths=c(1,Inf)))
tdm1<- TermDocumentMatrix(Acq[1])
distMatrix<-dist(scale(tdm1))
fit<-hclust(distMatrix,method="ward.D2")
plot(fit)
graphics.off()

# display the wordcloud
m1<-as.matrix(tdm1)
word.freq<-sort(rowSums(m1),decreasing = T)
pal<-brewer.pal(9,"BuGn")
pal<-pal[5:9]
wordcloud(words=names(word.freq),freq=word.freq,min.freq=3,random.order = F,colors=pal)
graphics.off()

# find the longest word
content1<-Acq[[22]]$content
vector<-unlist(strsplit(content1,split="[ ][ ]*"))
vectorLen<-lapply(vector,nchar)
maxIndex<-which.max(vectorLen)
maxWord<-vector[maxIndex]

# find the longest sentence (most chars)
content2<-acq[[22]]$content  #document that has not been cleaned
vector2<-unlist(strsplit(content2,split="   "))
for( i in 1: length(vector2)){
	vector2[i]<-gsub("\n"," ",vector2[i])
}
vectorLen2<-lapply(vector2,nchar)
maxIndex2<-which.max(vectorLen2)
maxSentence<-vector2[maxIndex2]

# find the longest sentence (most words)
# first remove punctuation in every sentence. answer to question f.
vector3<-c()
for( i in 1: length(vector2)){
	vector3[i]<-gsub("[^[:alpha:][:space:]]*","",vector2[i])
}
vectorlen3<-c()
for(i in 1:length(vector3)){
	vectorlen3<-c(vectorlen3, wordcount(vector3[i]))
}
maxIndex3<-which.max(vectorlen3)
maxSentence2<-vector2[maxIndex3]

# print a table of the length of each sentence
table1<-data.frame(sentence=1:length(vector2), length=vectorlen3)

# For each sentence of each document, remove the punctuation.
# please see the vector3, whose punctuation was removed before.

# search words/phrase
key<-"computer"
ACQ<-tm_map(acq,content_transformer(tolower));
ACQ <- tm_map(ACQ, removeBackslashN, "\n");
ACQ <- tm_map(ACQ, removePunctuation);
for(i in 1:50){

 if(str_detect(ACQ[[i]]$content,key)){
     print("this word/phrase is shown in document: ")
     print(i)
     vectortemp<-ACQ[[i]]$content
     # deal with some sentences starting at space
     vector4<-unlist(strsplit(vectortemp, split="   "))
     for(j in 1: length(vector4)){
       if(vector4[j]!=""&&str_detect(vector4[j],key)){
         print("this word is shown in the sentence: ")
         print(j)
         sentence<-gsub("  ","", vector4[j])
         vector5<-unlist(strsplit(sentence,split=" "))
         for(k in 1:length(vector5)){
           if(vector5[k]!=""&&str_detect(vector5[k],key)){
              print("index: ")
              print(k)
            }
         }
         }
         }
         }
         }

# part of speech ##################################################
posText<-acq[[22]]$content
library(stringr)
#Spliting into sentence based on carriage return
s <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
# reference:from internet
tagPOS<-function(x, ...)
{
  str=as.String(x);
  annot=Annotation(1L,"sentence",1L,nchar(str));
  annot=annotate(str,Maxent_Word_Token_Annotator(),annot);
  annot=annotate(str,Maxent_POS_Tag_Annotator(),annot);
  annot=annot[annot$type=="word"];
  POStags=unlist(lapply(annot$features,`[[`,"POS"));
  POStagged=paste(sprintf("%s/%s", str[annot],POStags),collapse=" ");
  list(POStagged =POStagged, POStags=POStags);
}
# apply this func to every line of input
result=lapply(acq,tagPOS);

# get rid of these commas, spaces, stamps
acq<-tm_map(acq,removePunctuation);
acq<-tm_map(acq,stripWhitespace);
acq<-tm_map(acq,content_transformer(function(x) removeWords(x, stopwords("english"))))

# draw frequency cloud
acqdtm=as.matrix(DocumentTermMatrix(acq));
fcnt=sort(colSums(acqdtm),decreasing = TRUE);
wcl=data.frame(word=names(fcnt),freq=fcnt)
wordcloud(wcl$word,wcl$freq,randmom.order=FALSE,colors=brewer.pal(8,"Dark2"))