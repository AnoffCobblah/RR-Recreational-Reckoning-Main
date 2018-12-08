#WordFlagLS is a function that keeps track of sentences AND lemma numbers. It provides the fullest information, but is also INCREDIBLY slow.

WordFlagLS <- function(flagterms,files) {
    stemflagterms <- flagterms %>% wordStem() %>% unique()
    LSWordFlagdf <- data.frame(Text=character(), Searched_Term=character(), Lemma=integer(), Sentence=character(), Lemma_Length = integer(), Lemma_Perc = numeric())
    
    for (i in 1:length(files)) {
      #first we print which file we are looking at, so we know how far in we are
      print(paste0(i," out of ",length(files)," files"))
      #for each document, we create a temporary data frame that we'll add to the final results
      TempDocdf <- data.frame(Text=character(), Searched_Term=character(), Lemma=integer(), Sentence=character())
      
      #Basically, we want to search for every place in the tems that matches the stemflagtermsvector, and note where that occurs. This is a bit tricky, because we don't know how many times that term is going to appear.  That's where the iterp value comes in: for each loop which finds a match, "iterp"" increases by one and allows the value to be added to the list "lemma".
      iterp <- 1
      
      fileName <- files[i] %>% read_file()
      #since tokenize_sentences function requires things to be encoded in UTF-8, need to remove some data.
      #The Encoding function specifies that we want this in utf8
      Encoding(fileName) <- "UTF-8"
      #the incon function converts a character vector between encodings (the i is for internationalization)
      fileName <- iconv(fileName, "UTF-8", "UTF-8",sub='')
      #the stoken then splits it into sentences
      stoken <- tokenize_sentences(fileName, lowercase = FALSE, strip_punct = FALSE, simplify = FALSE)
      #lists are a bit annoying to work with, so we unlist that into a vector
      s2token <- unlist(stoken)
      
      for (q in 1:length(s2token)) {
        ltoken <- tokenize_words(s2token[q], lowercase = TRUE, stopwords = NULL, simplify = FALSE)
        l2token <- unlist(ltoken)
        #at some point, we're going to want the length of each sentence so we know how many words in the match occurred.
        TempLength <- length(l2token)
        #We're going to get an error later if it turns out that l2token is an empty character string. We can use TempLength to avoid that possibility, by only proceeding if the length of the character string is not 0.
        if(TempLength != 0) {
          #finally, we stem the words in this lemmatized sentence so it's easier to match them.
          steml2token <- wordStem(l2token)
          
          for (w in 1:length(l2token)) {
            
            #now that we're finally at the word level we can see if this word in this sentence matches any of our stemmed search terms
            for (p in 1:length(stemflagterms)) {
              print(paste0(p," out of ",length(stemflagterms)," search terms. ",q," out of ",length(s2token)," sentences. ",i," out of ",length(files)," files"))
              #we'll want to match the lemma to the terms in the list one at a time, so we create a searchedterm object in a loop
              searchedterm <- stemflagterms[p]
              
              if(steml2token[w] == searchedterm) {
                tempdf <- data.frame(files[i],searchedterm,iterp,s2token[q])
                names(tempdf) <- c("Text","Searched_Term","Lemma","Sentence")
                TempDocdf <- rbind(TempDocdf,tempdf)
              }
              
            }
            #finally, we add to iterp, so the value is essentially going up by one for each lemma
            iterp <- iterp+1
          }
        }
      }
      if(nrow(TempDocdf) != 0) {TempDocdf$Lemma_Length <- iterp
      TempDocdf$Lemma_Perc <- TempDocdf$Lemma / TempDocdf$Lemma_Length *100
      LSWordFlagdf <- rbind(WordFlagdf,TempDocdf)}
    }
    LSWordFlagdf
}
