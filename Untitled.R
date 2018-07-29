library(tidyverse)
library(tidytext)
library(dplyr)
library(tidyr)
library(stringr)
library(quanteda)
library(stm)
library(devtools)
install_github('broman','kbroman')

files_dir<-"/Users/Rexon/World Resources Institute/Climate Watch - Documents/NDC Data/NDC file/NDC_text_files/NDC_text_files"
text_files<-list.files(path = files_dir)

text_2_df<-function(file_name){
  text<-read_lines(paste(files_dir,file_name,sep="/"))
  document<-rep(gsub(".md","",file_name),length(text))
  df<-data.frame(text,document)
  colnames(df)<-c("Text","Document")
  return(df)
}

data<-do.call(rbind,lapply(text_files,text_2_df))

data$Text<-as.character(data$Text)

tidy_data <- data %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words) %>%
  filter(word != "de")%>%
  filter(word != "la")%>%
  filter(word != "des")%>%
  filter(word != "en")%>%
  filter(word != "les")%>%
  filter(word != "le")%>%
  filter(word != "à")%>%
  filter(word != "el")%>%
  filter(word != "du")


tidy_data %>%
  dplyr::count(word, sort = TRUE)

data_dfm <- tidy_data %>%
  dplyr::count(Document, word, sort = TRUE) %>%
  cast_dfm(Document, word, n)

data_sparse <- tidy_data %>%
  dplyr::count(Document, word, sort = TRUE) %>%
  cast_sparse(Document, word, n)

topic_model <- stm(data_dfm, K = 20, 
                   verbose = FALSE, init.type = "Spectral")
summary(topic_model)
td_beta <- tidy(topic_model)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste("Topic ", topic)
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

  td_gamma <- tidy(topic_model, matrix = "gamma",                    
                   document_names = rownames(data_dfm))
  
  ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
    geom_histogram(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ topic, ncol = 3) +
    labs(title = "Distribution of document probabilities for each topic",
         subtitle = "Each topic is associated with 1-3 stories",
         y = "Number of stories", x = expression(gamma))