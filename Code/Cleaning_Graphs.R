# Satoshi vs. Musk Data Cleaning
# Brian Wickman
# December 2022

# Load packages
  library(tidyverse)
  library(data.table)
  library(ggthemes)
  library(tm)  
  library(SnowballC)  

# Load Satoshi data
  # List files
    satoshi_folder <- "~/Research/Brian/Blog/BitcoinMusk/Data/Satoshi"
    musk_folder <- "~/Research/Brian/Blog/BitcoinMusk/Data/Musk"
    file_list1 <- list.files(path = satoshi_folder, pattern = "*.txt", full.names = T)
    file_list2 <- list.files(path = musk_folder, pattern = "*.txt", full.names = T)
  
  # Load files
    satoshi_list <- list()
    for (i in 1:length(file_list1)) {
      temp_df <- readLines(file_list1[i])
      satoshi_list[[i]] <- Corpus(VectorSource(temp_df))
    }
    musk_list <- list()
    for (i in 1:length(file_list2)) {
      temp_df1 <- readLines(file_list2[i])
      musk_list[[i]] <- Corpus(VectorSource(temp_df1))
    }

  # Clean files
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    
    satoshi_clean_list <- list()
    for (k in 1:length(satoshi_list)) {
      #Replacing "/", "@" and "|" with space
        TextDoc <- tm_map(satoshi_list[[k]], toSpace, "/")
        TextDoc <- tm_map(TextDoc, toSpace, "@")
        TextDoc <- tm_map(TextDoc, toSpace, "\\|")
      # Convert the text to lower case
        TextDoc <- tm_map(TextDoc, content_transformer(tolower))
      # Remove numbers
        TextDoc <- tm_map(TextDoc, removeNumbers)
      # Remove the word "page"
        TextDoc <- tm_map(TextDoc, removeWords, c("page")) 
      # Remove punctuations
        TextDoc <- tm_map(TextDoc, removePunctuation)
      # Eliminate extra white spaces
        TextDoc <- tm_map(TextDoc, stripWhitespace)
      # Text stemming - which reduces words to their root form
        TextDoc <- tm_map(TextDoc, stemDocument)
      # Save
        satoshi_clean_list[[k]] <- TextDoc
    }
    
    musk_clean_list <- list()
    for (k in 1:length(musk_list)) {
      #Replacing "/", "@" and "|" with space
      TextDoc <- tm_map(musk_list[[k]], toSpace, "/")
      TextDoc <- tm_map(TextDoc, toSpace, "@")
      TextDoc <- tm_map(TextDoc, toSpace, "\\|")
      # Convert the text to lower case
      TextDoc <- tm_map(TextDoc, content_transformer(tolower))
      # Remove numbers
      TextDoc <- tm_map(TextDoc, removeNumbers)
      # Remove the word "page"
      TextDoc <- tm_map(TextDoc, removeWords, c("page")) 
      # Remove punctuations
      TextDoc <- tm_map(TextDoc, removePunctuation)
      # Eliminate extra white spaces
      TextDoc <- tm_map(TextDoc, stripWhitespace)
      # Text stemming - which reduces words to their root form
      TextDoc <- tm_map(TextDoc, stemDocument)
      # Save
      musk_clean_list[[k]] <- TextDoc
    }
    
    
  # Analysis (only look at stopwords)
    satoshi_word_freq <- list()
    for (l in 1:length(satoshi_clean_list)) {
      TextDoc_dtm <- TermDocumentMatrix(satoshi_clean_list[[l]])
      dtm_m <- as.matrix(TextDoc_dtm)
      # Sort by descearing value of frequency
      dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
      satoshi_word_freq[[l]] <- data.frame(word = names(dtm_v),freq=dtm_v) %>%
        mutate(total_words = sum(freq),
               per1000 = freq * (1000/total_words)) %>%
        filter(word %in% stopwords("english")) 
    }
    musk_word_freq <- list()
    for (l in 1:length(musk_clean_list)) {
      TextDoc_dtm <- TermDocumentMatrix(musk_clean_list[[l]])
      dtm_m <- as.matrix(TextDoc_dtm)
      # Sort by descearing value of frequency
      dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
      musk_word_freq[[l]] <- data.frame(word = names(dtm_v),freq=dtm_v) %>%
        mutate(total_words = sum(freq),
               per1000 = freq * (1000/total_words)) %>%
        filter(word %in% stopwords("english")) 
    }

  # Clean dataframe
    # Include unused words (0 counts are important)
      satoshi_df <- rbindlist(satoshi_word_freq, idcol = "sample")%>%
        complete(sample, word)
    # Fill in total_words
      filler_df_satoshi <- satoshi_df %>% distinct(sample, total_words) %>% drop_na()
      satoshi_df1 <- satoshi_df %>%
        left_join(filler_df_satoshi, by = "sample") %>%
        select(-total_words.x) %>%
        rename(total_words = total_words.y) %>%
        replace(is.na(.), 0) %>%
        mutate(author = "satoshi")
      
    # Include unused words (0 counts are important)
      musk_df <- rbindlist(musk_word_freq, idcol = "sample")%>%
        complete(sample, word)
      # Fill in total_words
      filler_df_musk <- musk_df %>% distinct(sample, total_words) %>% drop_na()
      musk_df1 <- musk_df %>%
        left_join(filler_df_musk, by = "sample") %>%
        select(-total_words.x) %>%
        rename(total_words = total_words.y) %>%
        replace(is.na(.), 0) %>%
        mutate(author = "musk")
      
  # Combine dataframes
    data <- rbind(satoshi_df1, musk_df1)
    write_csv(data, file = "~/GitProjects/Blog/MuskSatoshi/Data/Clean/SatoshiMusk.csv")
    
  # Distribution of word counts
    data %>%
      distinct(sample, author, total_words) %>%
      filter(total_words < 2000) %>%
      ggplot() +
      geom_density(aes(total_words, color = author, fill = author), alpha = 0.6) +
      theme_clean() +
      labs(
        title = "Word Counts of Satoshi's 50 blog posts",
        caption = "Excludes the Bitcoin Whitepaper (>2000 words)"
      )
    
  # Most popular stopwords
    (stopword_freq <- data %>%
      group_by(author, word) %>%
      summarise(total_usage = sum(freq)) %>%
      pivot_wider(names_from = "author", values_from = "total_usage") %>%
      mutate(total_use = musk + satoshi) %>%
      arrange(desc(total_use))) %>%
      rename(nakamoto = satoshi)
    print(stopword_freq, n = 12)
    
  # Satoshi rates of the most popular words
    data %>%
      filter(author == "satoshi") %>%
      right_join(stopword_freq %>%
                  filter(author == "satoshi") %>%
                  filter(row_number() < 7), by = "word") %>%
      ggplot() +
      geom_density(aes(per1000, group = word, color = word, fill = word), alpha = 0.3) +
      theme_clean() +
      labs(
        title = "Rates of Use of Satoshi's Most Frequent Words"
      )
    
  # Musk rates of the most popular words
    clean_df %>%
      filter(author == "musk") %>%
      right_join(stopword_freq %>%
                   filter(author == "musk") %>%
                   filter(row_number() < 7), by = "word") %>%
      ggplot() +
      geom_density(aes(per1000, group = word, color = word, fill = word), alpha = 0.3) +
      theme_clean() +
      labs(
        title = "Rates of Use of Musk's Most Frequent Words"
      )
    
  # Book graphic #1
    data %>%
      mutate(author = as.factor(author)) %>%
      filter(word == "and") %>%
      ggplot() +
      geom_jitter(aes(x = rate, y = author), color = "royalblue", height = 0.2) +
      labs(
        x = "Rate per 1000 words",
        y = "Authorship", 
        title = "Observed usage rates of the word 'and'"
      )
  
