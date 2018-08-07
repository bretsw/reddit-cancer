## --------------------------------------------------------------
## set workspace and load libraries
## --------------------------------------------------------------

#ls()  ## lists variables in the workspace
rm(list=ls())  ## clears all variables in the workspace

library(tidyverse)  # for data manipulation; includes library(dplyr); library(ggplot2)
library(stringr)  # for ease of working with string and character varbiables
library(anytime); library(lubridate)  # for working with dates
library(sentimentr)  # for sentiment analysis https://github.com/trinker/sentimentr
library(igraph)  # for processing social network
library(ggraph)  # for visualizing social network



## --------------------------------------------------------------
## load, clean, view data
## --------------------------------------------------------------

## assumes filenames are of the form "subreddit_cancer_YEAR_MONTH.csv" and stored in same directory
m <- stringr::str_extract_all(dir(), "subreddit_cancer\\S+", simplify=TRUE)
m <- m[m[,1] != "", ]
cancer_og <- lapply(m, read.csv, header=TRUE, colClasses="character")  # cancer_og[[i]] = sample i  |  cancer_og[[i]][1,] = row 1 of sample i

## create dataframe
n_samples <- length(cancer_og)
cancer_df <- data.frame()
for (i in 1:n_samples) {
        print(dim(cancer_og[[i]]))
        cancer_df <- rbind(cancer_df, cancer_og[[i]])
}

#dim(cancer_df)  # 35,209 x 20
#length(unique(cancer_df$id))  # n = 35,085  # these are the specific posts
#length(unique(cancer_df$link_id))  # n = 4,059  # these are the specific threads
#length(unique(cancer_df$author))  # n = 4,538  # these are the contributors


## clean data
cancer <- cancer_df[(
        !duplicated(cancer_df$id) &
                cancer_df$id != "" &
                !is.na(cancer_df$id) &
                cancer_df$body != "[removed]" &
                cancer_df$author != "[deleted]"
), ]


## convert date/time from UNIX form to readable UTC timezone
cancer$date_time <- anytime(as.numeric(cancer$created_utc), asUTC=TRUE)
cancer$date_time <- lubridate::as_datetime(cancer$date_time)
cancer <- cancer %>% select(-created_utc)

#dim(cancer)  # 32,341 x 20
#length(unique(cancer$id))  # n = 32,341  # these are the specific posts now that dataset has been cleaned
#length(unique(cancer$link_id))  # n = 3,845  # these are the specific threads
#length(unique(cancer$author))  # n = 4,537  # these are the different authors



## --------------------------------------------------------------
## conceptually define variables
## --------------------------------------------------------------

## Node Centrality: POPULARITY = total number of other users interacted with
## Node Size: ACTIVITY = total number of posts by that user
## Edge Width: STRENGTH = rate of interactions per day between those two users
## Edge Color: SUPPORT = total sentiment polarity scores for all interactions between those two users



## --------------------------------------------------------------
## sentiment analysis
## --------------------------------------------------------------

cancer_sentimentR <- 
        sentiment(cancer$body, 
                  polarity_dt = lexicon::hash_sentiment_huliu)  # using the Hu-Liu lexicon

cancer <- cancer_sentimentR %>%
        group_by(element_id) %>%
        summarize(sentiment = sum(sentiment)) %>%
        cbind(cancer, .)

cancer_talk <- cancer %>%
        select(date_time, link_id, id, author, parent_id, sentiment, body) %>%
        mutate(link_id = str_remove(link_id, pattern="t[0-9]_")) %>%
        mutate(parent_id = str_remove(parent_id, pattern="t[0-9]_")) %>%
        mutate(parent_author = "")


## this is the logic to retrieve to the name of the parent author—there is a different id scheme for the first post of a thread
for (i in 1:length(cancer_talk$id)) {
        cancer_talk$parent_author[i] <-
                ifelse(cancer_talk$parent_id[i] == cancer_talk$link_id[i], 
                       filter(cancer_talk, link_id==cancer_talk$parent_id[i])[1,]$author,
                       filter(cancer_talk, id==cancer_talk$parent_id[i])[1,]$author
                )
}


## removing instances where contributors reply to themselves
cancer_talk_other <- cancer_talk %>% 
        filter(author != parent_author) %>%
        select(date_time, link_id, id, parent_id, author, parent_author, sentiment, body)
#dim(cancer_talk_other)  # 28,587 x 8


## sorting the columns in each row because we don't care about sender-receiver order
all_combinations <- cancer_talk_other %>% 
        select(author, parent_author) %>% 
        apply(1, sort) %>% 
        t %>% 
        as.data.frame(stringsAsFactors=FALSE)
colnames(all_combinations) = c("author1", "author2") 


cancer_talk_2way <- cancer_talk_other %>% 
        select(date_time, sentiment, body) %>% 
        cbind(., all_combinations) %>%
        add_count(author1, author2, sort=FALSE) %>%  # adds new column, 'n'
        group_by(author1, author2) %>% 
        mutate(support = sum(sentiment),
               time_span = max(date_time) - min(date_time),  # reports in seconds--span between first and last interaction
               strength = as.numeric(1 + ((n * time_span) / (60 * 60 * 24)))  # strength = posts/day = num posts * duration / seconds in day 
        ) %>%
        select(author1, author2, strength, support, -n, -time_span, -sentiment, -date_time, -body) %>%
        unique %>%
        arrange(-strength)
#dim(cancer_talk_2way)  # 19,327 x 6


## variable: ACTIVITY = total number of posts by that user
table_activity <- cancer_talk_other$author %>% 
        table %>%
        as.data.frame(stringsAsFactors=FALSE)
colnames(table_activity) = c("author", "posts") 
table_activity <- table_activity[(table_activity$author != ""), ]
table_activity <- table_activity[order(-table_activity[,"posts"], table_activity[,"author"]),]  #sort by frequency (descending) and hashtags (ascending)
#dim(table_activity)  # n = 4,225 authors who connect to others



## --------------------------------------------------------------
## Visualization
## create a network graph - see http://igraph.org/r/doc/aaa-igraph-package.html
## --------------------------------------------------------------

# Node Centrality: POPULARITY = total number of other users interacted with
# Node Size: ACTIVITY = total number of posts by that user
# Edge Width: STRENGTH = rate of interactions per day between those two users
# Edge Color: SUPPORT = total sentiment polarity scores for all interactions between those two users


actors <- table_activity %>% 
        filter(author %in% c(cancer_talk_2way$author1, cancer_talk_2way$author2)) %>% 
        mutate(posts = sqrt(posts))

activity <- deframe(actors)

relations <- cancer_talk_2way %>% 
        filter(author1 %in% actors$author) %>% 
        filter(author2 %in% actors$author) %>%
        mutate(strength = sqrt(as.numeric(strength)))


## create the social network graph
g <- graph_from_data_frame(relations, directed=FALSE, vertices=actors)



## create the visualization - see https://www.data-imaginist.com/2017/ggraph-introduction-layouts/
## layout options: 'nicely', 'linear', 'kk' (maxiter=1)
p <- ggraph(g, layout='kk') +  # Node Centrality: POPULARITY = total number of other users interacted with
        geom_edge_link(alpha=.3, # changes darkness/transparency of edges
                       aes(edge_width = strength,  # Edge Width: STRENGTH = rate of interactions per day between those two users
                           colour = support)) + # Edge Color: SUPPORT = total sentiment polarity scores for all interactions between those two users
        scale_edge_colour_gradient2(high="#00008B",  # red
                                    mid="#FFFACD",  # yellow
                                    low="#8B0000",  # blue
                                    limits=c(-3,3)) +
        scale_edge_width(range = c(.5, 10)) + # possible range of line widths
        geom_node_point(alpha=.3, aes(size=activity)) +  # Node Size: ACTIVITY = total number of posts by that user
        scale_size(range = c(.5, 10)) + # possible range of node sizes
        theme_bw() +  # makes background white (not gray)
        theme(panel.grid.major = element_blank()) # remove x and y major grid lines (because Tufte said so)
p  # display the plot



## --------------------------------------------------------------
## create a scatter plot: SUPPORT vs. STRENGTH
## --------------------------------------------------------------

## create the visualization 
scatter <- ggplot(data=cancer_talk_2way,
                  aes(x=strength, y=support)) + 
        geom_point(alpha=.5, color="#00BFFF") + 
        geom_smooth(method='lm') +
        theme_bw() + 
        theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank() )+
        theme(panel.border= element_blank())+
        theme(axis.line = element_line(color="black", size = .5))
scatter  # display the plot

## calculate correlation
relations.lm = lm(support ~ strength, data=cancer_talk_2way)
relations.lm$coefficients  # strength = .00075
