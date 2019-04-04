#DATA EXTRACTION

library(rvest)
library(stringr)
library(purrr)
seq1<-seq(1,740,by=250)
runtime_func<-function(x){y<-gsub(" min",'',x) %>% as.numeric()
#z<-ifelse(length(y)==0,NA,y )}
#y<-ifelse(. == "", NA, .)
return(y)}
genre_func<-function(x){
  #gl<-ifelse(length(x)==0,NA,x)
  genre<-str_trim(gsub("\n",'',x))
  genre_data<-as.data.frame(genre) 
  sapply(strsplit(as.character(genre_data$genre), ","), `[`, 1) }
url_base <- "https://www.imdb.com/search/title?title_type=feature&year=2017-01-01,2017-12-31&start=%d&ref_=adv_nxt&count=250"
movies<-map_df (seq1,function(i) {
  cat("Boom!")
  pg <- read_html(sprintf(url_base, i))
  data.frame( name= html_nodes(pg,'.lister-item-header a') %>% html_text(),
              rating=html_nodes(pg,'.ratings-imdb-rating strong') %>%html_text()%>% as.numeric(),
              runtime=runtime_func(html_nodes(pg,'.runtime') %>%html_text()),
              genre=genre_func(html_nodes(pg,'.genre') %>%html_text()),
              stringsAsFactors=FALSE)
})
View(movies)

#DATA ANALYSIS
library(ggplot2)
library(plyr)
library(dplyr)
#Movie rating Histogram
#data+aesthetics+geometry
ggplot(movies,aes(x=rating))+geom_histogram(binwidth = 0.2,col='red',fill='pink',alpha=0.4) +
  xlab('Movie Ratings') + ylab('Count') +ggtitle("Movie Rating Histogram")
ggplot(movies,aes(x=rating))+geom_histogram(binwidth = 0.1,aes(fill=..count..)) +
  xlab('Movie Ratings') + ylab('Count') +ggtitle("Movie Rating Histogram")
#Genre wise most produced genre
genre_grp<-count(movies,genre)
View(genre_grp)
ggplot(genre_grp, aes(fill=genre_grp$genre, y=genre_grp$n , x=genre_grp$genre)) +
  geom_bar(stat="identity")+ xlab('Genre') + ylab("Number of movies")+
  labs(title  ="Popular genre", subtitle = "(Count based on genre)") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
