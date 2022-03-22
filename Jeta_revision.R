library(edgar)
library(text2vec)
library(stringr)
library(tidyverse)
library(tidytext)
library(tm)
library(Matrix)
library(mltools)
library(textmineR)

mda<- getMgmtDisc(cik.no = c(320193, 1018724), filing.year = c(2014:2018))

a  <-Corpus(DirSource("/Users/mvs/Desktop/Research/MD&A section text"), readerControl = list(language="en"))
a <- tm_map(a, removeNumbers)
a <- tm_map(a, removePunctuation)
a <- tm_map(a , stripWhitespace)
a <- tm_map(a, tolower)
a <- tm_map(a, removeWords, stopwords("english")) 

####tf-idf#######################################################################

report_words <- a() %>%
  unnest_tokens(word, text) %>%
  count(report, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

adtm <-DocumentTermMatrix(a,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE))) 
cosine <- sim2(as.matrix(adtm), method="cosine", norm="l2")
jaccard <- sim2(as(as.matrix(DocumentTermMatrix(a)), "sparseMatrix"), method="jaccard", norm="none")
eucledian <- dist(as.matrix(adtm),method="euclidean") 

amazn_year_wise_cosine <- c()
for(i in 1:4) {
  amazn_year_wise_cosine[i] <- cosine[i,1+i]
}

aapl_year_wise_cosine <- c()
for(i in 1:4) {
  aapl_year_wise_cosine[i] <- cosine[5+i,6+i]
}

amazn_vs_aapl_cosine <- c()
for(i in 1:5) {
  amazn_vs_aapl_cosine[i] <- cosine[i,5+i]
}

amazn_year_wise_jaccard <- c()
for(i in 1:4) {
  amazn_year_wise_jaccard[i] <- jaccard[i,1+i]
}

aapl_year_wise_jaccard <- c()
for(i in 1:4) {
  aapl_year_wise_jaccard[i] <- jaccard[5+i,6+i]
}

amazn_vs_aapl_jaccard <- c()
for(i in 1:5) {
  amazn_vs_aapl_jaccard[i] <- jaccard[i,5+i]
}

amazn_vs_aapl <- cbind(as.data.frame(amazn_vs_aapl_jaccard) , as.data.frame(amazn_vs_aapl_cosine))
amazn_vs_aapl$year <- 2014:2018


ggplot(amazn_vs_aapl %>% gather(amazn_vs_aapl_jaccard, amazn_vs_aapl_cosine), 
       aes(x=group,y = seasons, fill = S1, S2, S3, S4)) + 
  geom_bar(stat = 'identity', position = 'dodge')
p = ggplot() + 
  geom_bar(data = amazn_vs_aapl, aes(x = year, y = amazn_vs_aapl_jaccard), color = "blue",stat = "identity") +
  geom_bar(data = amazn_vs_aapl, aes(x = year, y = amazn_vs_aapl_cosine), color = "red",stat = "identity") +
  xlab('Years') +
  ylab('similarity')

print(p)

# Change the colors manually
simi_intra_company <- cbind(as.data.frame(amazn_year_wise_cosine), 
                            as.data.frame(amazn_year_wise_jaccard),
                            as.data.frame(aapl_year_wise_cosine),
                            as.data.frame(aapl_year_wise_jaccard)
)
simi_intra_company$year <- 2015:2018
p <- ggplot(data=simi_intra_company, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

#vegdist(as.matrix(adtm), method = 'jaccard')
#vegdist(as.matrix(adtm), method = 'cosine')


library(dplyr)
atidy<- tidy(adtm)

agrouped<- atidy%>%
  filter(!term %in% c("million", "billion"))%>%
  group_by(document)%>%
  top_n(10, count)

atidy$word<-atidy$term
asenti<- atidy%>%
  filter(!term %in% c("million", "billion"))%>%
  inner_join(get_sentiments("loughran"))%>%
  group_by(document)%>%
  filter(sentiment == 'negative')
asenti$company <- as.numeric(sapply(strsplit(asenti$document, "_"), "[[", 1))
asenti$year <- as.numeric(sapply(strsplit(asenti$document, "[_-]"), "[[", 4))


p <- ggplot(data=asenti[asenti$company == 1018724,], aes(x=year, y=n, group=sentiment, color=sentiment)) +
  geom_line()+
  theme_minimal()+xlab('Year')+ ylab("Number of words")
print(p)

q <- ggplot(data=asenti[asenti$company == 320193,], aes(x=year, y=n, group=sentiment, color=sentiment)) +
  geom_line()+
  theme_minimal()+xlab('Year')+ ylab("Number of words")
print(q)

senti.df <- getSentiment(cik.no = c(320193, 1018724), form.type = '10-K', filing.year = 2017)
senti.df <- LMMasterDictionary(cik.no = c(320193, 1018724), form.type = '10-K', filing.year = 2017)