WordFlagSentences <- function(flagterms,files) {
    stemflagterms <- flagterms %>% stem_strings() %>% unique()
    SentenceWordFlagdf <- data.frame(Text=character(), Searched_Term=character(), Sentence_No=integer(), Sentence=character(), Sentence_Total = integer(), Sentence_Perc = numeric())
    
    for (i in 1:length(files)) {
      #first we print which file we are looking at, so we know how far in we are
      print(paste0(i," out of ",length(files)," files"))
      #for each document, we create a temporary data frame that we'll add to the final results
      TempDocdf <- data.frame(Text=character(), Searched_Term=character(), Sentence_No=integer(), Sentence=character())
      
      #Basically, we want to search for every sentence in which the character string that matches the stemflagtermsvector appears, and note where that occurs. This is a bit tricky, because we don't know how many times that term is going to appear.
      fileName <- files[i] %>% read_file()
      
      #since tokenize_sentences function requires things to be encoded in UTF-8, need to remove some data.
      #The Encoding function specifies that we want this in utf8
      Encoding(fileName) <- "UTF-8"
      #the incon function converts a character vector between encodings (the i is for internationalization)
      fileName <- iconv(fileName, "UTF-8", "UTF-8",sub='')
      #the stoken then splits it into sentences
      stoken <- tokenize_sentences(fileName, lowercase = FALSE, strip_punct = FALSE, simplify = FALSE)
      #lists are a bit annoying to work with, so we unlist that into a vector
      s2token <- stoken %>% unlist()
      
      #Now we're going to make a dataframe to keep track of the sentences
      Sentencedf <- data.frame(Sentence = as.character())
      Sentencedf <- data.frame(Sentence = as.character(s2token))
      #We need a 3rd stoken value to take out the punctuation.
      s3token <- tokenize_sentences(s2token, lowercase=TRUE, strip_punct=TRUE, simplify=FALSE)
      Sentencedf$SentenceStripped <- as.character(s3token)
      Sentencedf$StemSentence <- Sentencedf$SentenceStripped %>% stem_strings()
      
      for (q in 1:nrow(Sentencedf)) {
        for (p in 1:length(stemflagterms)) {
          
          print(paste0(p," out of ",length(stemflagterms)," search terms. ",q," out of ",length(s2token)," sentences. ",i," out of ",length(files)," files")) 
          
          #Note that below, grepl can return TRUE, FALSE, or logical(0). So we only want the true ones.
          templist <- strsplit(Sentencedf$StemSentence[q]," ")
          if(any(templist[[1]] == stemflagterms[p])) {
            tempdf <- data.frame(files[i],stemflagterms[p],q,Sentencedf$Sentence[q])
            names(tempdf) <- c("Text","Searched_Term","Sentence_No","Sentence")
            TempDocdf <- rbind(TempDocdf,tempdf)
          }
          
        }
      }
      
      if(nrow(TempDocdf) != 0) {TempDocdf$Sentence_Total <- nrow(Sentencedf)
      TempDocdf$Sentence_Perc <- TempDocdf$Sentence_No / TempDocdf$Sentence_Total *100
      SentenceWordFlagdf <- rbind(SentenceWordFlagdf,TempDocdf)}
    }
    SentenceWordFlagdf
}
