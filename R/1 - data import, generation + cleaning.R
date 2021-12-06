setwd("~/My Files/University/Work/3rd year/BSc Research project/dissertation/Code")
library(tidyverse) #for general data manipulation and graphing
library(ratelimitr)
# Import metadata from Web of Science -----------------------------------------

files <- c("1988-1999.csv", "2000-2009.csv", "2010-2018.csv", "2019-2020.csv")
metadata <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("AU", "TI", "SO", "DI", "PD", "PY", "AB", "wc"))

# loop through the files in the list and import them, selecting only relevant columns
for (i in files){
  temp <- read.csv(i, na.strings = c("", "NA")) %>%
    select(AU, TI, SO, DI, PD, PY, AB, WC)
  metadata <- rbind(metadata, temp)
}

#write metadata to file
write.table(metadata, "metadata.csv")

# Extract papers from server using fulltext package -------------------------------
library(fulltext)

metadata <- metadata %>% slice(1:10)
# make a dataframe of just the DOIs
dois_list <- select(metadata, DI) %>%
  na.omit() %>%
  {as.vector(t(.))}

# work through the list of DOIs and extract from server
sources <- c("entrez", "elife", "pensoft", "arxiv", "biorxiv", "elsevier", "sciencedirect", "wiley")
grab_texts <- map(dois_list[1:length(dois_list)], 
                  try(limit_rate(ft_get), rate(n = 20, period = 60)),
                  from = sources,
                  progress = T) #iterate through each DOI using the ft_get function, ignoring errors
# in reality this command was run in increments due to the very large volume of articles to search and download

# put the texts into a df
text_table <- ft_table() 
# select only the text itself and put into a df; get rid of duplicates
text_only <- select(text_table, c(dois, text)) %>% 
  distinct() %>%
  rename(doc_id = dois)

#write text_table and text_only to file
write.table(text_table, "text_table.csv")
write.table(text_only, "text_only.csv")

# Use XML format to extract body text only --------------------------------
library(pubchunks)

# get the row indices of all .xml files
xml_texts_index <- str_which(as.vector(text_table$paths), ".xml")
# put the .xml file texts in their own dataframe (using indices)
xml_texts <- slice(text_only, xml_texts_index)
# get a vector of the file paths to the .xml files
xml_texts_paths <- slice(text_table, xml_texts_index)$paths

# use pub_chunks to extract just the "body" section of each .xml file
xml_texts_body <- pub_chunks(as.list(xml_texts_paths[1:8299]), sections = c("doi", "body"))

# convert the output of pub_chunks from a nested list to a dataframe which is easier to read
tab <- pub_tabularize(xml_texts_body) %>%
  bind_rows()
# remove any row with an NA
tab <- na.omit(tab)

# each paragraph is in a separate row 
# put all paragraphs of each paper together in one row
tab2 <- tab %>%
  group_by(doi, .publisher) %>%
  mutate(text = paste0(body, collapse = " ")) %>%
  slice(1) %>%
  select(-body) %>%
  rename(DOI = doi, Publisher = .publisher, Text = text)
# select and rename the DOI and Year Published columns in metadata
metadata_DOI_date <- select(metadata, DI, PY) %>%
  rename(DOI = DI, Year = PY)
# merge the metadata and text-containg DFs using the DOI column
data <- left_join(tab2, metadata_DOI_date, by.x = "DOI") 

#write data to file
write.table(data, "data.csv")

# Generate "lemmatised" dataframe -----------------------------------------------------------
library(udpipe)
library(hablar) #for data type conversion

#download and load the English-EWT treebank model
en <- udpipe_download_model(language = "english")
udmodel_en <- udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")

#method from udpipe annotation vignette: https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-annotation.html
# create a vector of the text to be lemmatised
text_vector <- data$Text
# set up a dataframe for the lemmatised data
annotated_text <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("doc_id", "paragraph_id", "sentence_id", "sentence", "token_id", "token","lemma", "upos", "xpos", "feats")) %>%
  convert(chr(doc_id, sentence, token, lemma, upos, xpos, feats),
          int(paragraph_id, sentence_id, token_id))

# loop through the text vector and annotate it, adding each entry to the annotated_text df
for (i in 1:4324){
  temp_text <- udpipe_annotate(udmodel_en, x = text_vector[i], tagger = "default", parser = "none", trace = T) %>%
    as.data.frame() %>%
    select(-c(head_token_id, dep_rel, deps, misc))
  temp_text$doc_id <- data$DOI[i]
  convert(temp_text, 
          chr(doc_id, sentence, token, lemma, upos, xpos, feats),
          int(paragraph_id, sentence_id, token_id))
  annotated_text <- rbind(annotated_text, temp_text)
  print(i)
}

# remove punctuation, duplicate rows, commonly used words
annotated_text_slim <- select(all_of(annotated_text), doc_id, lemma, upos) %>%
  rename(annotated_text_slim, word = lemma) %>%
  filter(upos != "PUNCT") %>%
  unique() %>%
  anti_join(annotated_text_slim, stop_words, by = "word")

# further cleaning - remove numbers etc
annotated_text_slim <- annotated_text_slim[-grep("[]!\"#$%&'()*+,./:;<=>?@[\\^_`{|}~]", annotated_text_slim$word),] #gets rid of all punctuation except -
annotated_text_slim <- annotated_text_slim[-grep('^[0-9]{1}[A-z]{1}$', annotated_text_slim$word),] #gets rid of strings matching number + letter
annotated_text_slim <- annotated_text_slim[-grep('^[A-z0-9]{1,3}$', annotated_text_slim$word),] #gets rid of strings that are fewer than 3 letters or numbers long
annotated_text_slim <- annotated_text_slim[-grep('^[0-9]', annotated_text_slim$word),] #gets rid of strings that start with a number
hyphens <- grep('^-', annotated_text_slim$word) #makes a list of the indices of all words starting with a hyphen
annotated_text_slim <- annotated_text_slim[-hyphens,] #drops rows containing words starting with a hyphen
annotated_text_slim <- annotated_text_slim[-grep('[\u00B0]', annotated_text_slim$word),] #remove all words containing degree symbol
annotated_text_slim <- annotated_text_slim[-grep('[\u00B5]', annotated_text_slim$word),] #remove all words containing 'μ'
annotated_text_slim <- annotated_text_slim[-grep('[\u00B1]', annotated_text_slim$word),] #remove all words containing '±'
annotated_text_slim <- annotated_text_slim[-grep('[\u00D7]', annotated_text_slim$word),] #remove all words containing '×'
annotated_text_slim <- annotated_text_slim[-grep('figure', annotated_text_slim$word, ignore.case = T),] #remove all words containing 'figure'
annotated_text_slim <- annotated_text_slim[-grep('table', annotated_text_slim$word, ignore.case = T),] #remove all words containing 'table'

#write annotated_text_slim to file
write.table(annotated_text_slim, "annotated_text_slim.csv") 
