WordFlagLemmas <- function(flagterms,files) {
    stemflagterms <- flagterms %>% wordStem() %>% unique()
    lemma <- list()
    lemma_perc <- list()
    data <- matrix(,ncol=7,nrow=1)
    colnames(data) <- c("Text","Text_ID", "Searched_Term","Searched_Term_ID","Lemma","Lemma_Length","Lemma_Perc")
    for (p in 1:length(stemflagterms)) {
      iterp <- 1
      tempdata <- matrix(,ncol=7,nrow=1) #this matrix is supposed to get wiped every loop.
      searchedterm <- stemflagterms[p]
      #Basically, we want to search for every place in the tems that matches the stemflagtermsvector, and note where that occurs. This is a bit tricky, because we don't know how many times that term is going to appear.  That's where the iterp value comes in: for each loop which finds a match, "iterp"" increases by one and allows the value to be added to the list "lemma".
      for (i in 1:length(files)) {
        print(files[i])
        fileName <- files[i] %>% read_file()
        #since tokenize_sentences function requires things to be encoded in UTF-8, need to remove some data.
        Encoding(fileName) <- "UTF-8"
        fileName <- iconv(fileName, "UTF-8", "UTF-8",sub='')
        ltoken <- tokenize_words(fileName, lowercase = TRUE, stopwords = NULL, simplify = FALSE)
        ltoken <- unlist(ltoken)
        stemltoken <- ltoken %>% wordStem()
        for (w in 1:length(ltoken)) {
          if(stemltoken[w] == searchedterm) {
            lemma[[iterp]] <- w
            iterp <- iterp+1
          }
        }
        #When comparing files, it is often useful to normalize somehow.  This cell changes the lemma ID # into the place in the text, by percentage, where the term appears.
        if (length(lemma) != 0){
          for (k in 1:length(lemma)) {
            lemma_perc[[k]] <- (lemma[[k]] / length(ltoken)) *100
          }
        }
        #This can get a bit messed up if we don't add in some NA values for files which have no references to the stemflagterms vector, so we add them here.
        lemma <- unlist(lemma)
        #there used to be an unnecessary step here where I added in some NAs.  But I might be wrong about it...
        lemma_perc <- unlist(lemma_perc)
        #Now it's time to start adding what we've found for this document into our data matrix.
        mat <- matrix(,ncol=7,nrow=length(lemma))
        mat[,1] <- gsub(paste0(ProcessedDataLocation,"/"),"",files[i]) #This grabs just the end of the file path.
        mat[,1] <- gsub(".txt","",mat[,1])
        mat[,2] <- i
        mat[,3] <- searchedterm
        mat[,4] <- p
        mat[,5] <- lemma
        mat[,6] <- length(ltoken)
        mat[,7] <- lemma_perc
        tempdata <- rbind(tempdata,mat)
        #Finally, we need to clear our lists again before running the next search term through this file.
        lemma <- list()
        lemma_perc <- list()
      }
      #Our tempdatamatrix begins with an emtpy row, so we want to get rid of that, and then add all of this data for this searchterm, across all the files, to our final data matrix. "tempdata" will be erased once the loop starts again.
      tempdata <- tempdata[-1,]
      data <- rbind(data,tempdata)
    }
    #Data also starts with an empty row, so we delete that, and turn it into a dataframe which is saved in a new directory, "wfoutput".
    data <- data[-1,]
    WordFlagdf <- as.data.frame(data)
    WordFlagdf
}
