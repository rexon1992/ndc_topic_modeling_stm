install.packages("rvest")
library(rvest)
library(XML)
library(devtools)
install_github("wri/retrieveR")
library(retrieveR)
install_mac()

files_dir<-"/Users/Rexon/World Resources Institute/Climate Watch - Documents/NDC Data/HTML/ndc"
files<-list.files(files_dir,pattern="EN.html")

x<-read_html(paste(files_dir,files[1],sep="/"))
y<-htmlParse(paste(files_dir,files[1],sep="/"))

data<-y %>% html_nodes("p")

corpus<-prep_documents("ndc_data",ocr=F,clean=T,weights=F,type="html")

tidy_data <- corpus %>%
  unnest_tokens(word, sentences) %>%
  anti_join(stop_words)


tidy_data %>%
  dplyr::count(word, sort = TRUE)

names(tidy_data)[1]<-"Document"

data_dfm <- tidy_data %>%
  dplyr::count(Document, word, sort = TRUE) %>%
  cast_dfm(Document, word, n)

data_sparse <- tidy_data %>%
  dplyr::count(Document, word, sort = TRUE) %>%
  cast_sparse(Document, word, n)

topic_model <- stm(data_dfm, K = 100, 
                   verbose = FALSE, init.type = "Spectral")
summary(topic_model)
td_beta <- tidy(topic_model)


library(devtools)