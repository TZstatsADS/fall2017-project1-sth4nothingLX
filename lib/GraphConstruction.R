SplitText <- function(Phrase) { 
  unlist(strsplit(Phrase, " "))
}

trim <- function (x) 
  gsub("^\\s+|\\s+$", "", x)

IsPunctuated <- function(Phrase) {
  length(grep("\\.|,|!|\\?|;|:|\\)|]|}\\Z", Phrase, perl=TRUE))>0 
  # punctuation definition:    . , ! ? ; : ) ] }
}

SelectTaggedWords <- function(Words,tagID) {
  Words[grep(tagID, Words)]
}

RemoveTags <- function(Words) {
  sub("/[A-Z]{2,3}", "", Words)
}

IsSelectedWord <- function(Word) {
  ifelse(length(which(selected_words == Word))>0, TRUE, FALSE)
}

GetWordLinks <- function(position, scope) {
  scope <- ifelse(position+scope>length(words), length(words), position+scope)
  links <- ""
  for (i in (position+1):scope) {
    if ( IsSelectedWord(words[i]) ) links <- c(links,words[i])
  }
  
  if (length(links)>1) {
    links[2:length(links)]
  }
  else {
    links <- ""
  }
}

ConstructTextGraph <- function(n) { 
  word_graph <- new("graphNEL")
  i <- 1
  while (i < length(words) ) {
    if ( IsSelectedWord(words[i]) ) {                                   
      links <- GetWordLinks(i,n)                                
      if (links[1] != "") {                                     
        #cat(i," ",words[i]," - ",paste(c(links),collapse=" "),"\n")
        if ( length(which(nodes(word_graph)==words[i]))==0  ) {     
          word_graph <- addNode(words[i],word_graph)
        }                                               
        
        for (j in 1:length(links)) {
          if ( length(which(nodes(word_graph)==links[j]))==0 ) {
            word_graph <- addNode(links[j],word_graph)
            word_graph <- addEdge(words[i],links[j],word_graph,1)
          } 
          else {
            if ( length(which(edges(word_graph,links[j])[[1]]==words[i]))>0 ) { 
              prev_edge_weight <- as.numeric(edgeData(word_graph,words[i],links[j],"weight"))
              edgeData(word_graph,words[i],links[j],"weight") <- prev_edge_weight+1
            }
            else {
              word_graph <- addEdge(words[i],links[j],word_graph,1)
            }
          } 
        }
      }
    }
    i <- i+1
  }
  word_graph
}
