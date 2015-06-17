create_scoreSummary_parallel <- function(container, classification_results) {
	labels <- c()
	probs <- c()
	
	for (name in colnames(classification_results)) {
		if(strsplit(name,"_")[[1]][2] == "LABEL") {
			labels <- as.data.frame(c(labels,classification_results[name]));
		} else {
			probs <- as.data.frame(c(probs,classification_results[name]));
		}
	}
	
	best_labels <- c()
	best_probs <- c()
	agree_scores <- c()
  
	unique <- sort(unique(container@training_codes))

  cores_dump<-alply(.data = 1:nrow(classification_results),.margins = 1,.fun = function(x){
    row_labels <- labels[x,]
    row_probs <- probs[x,]
    dist <- c()
    
    for (code in unique) dist <- append(dist,length(row_labels[row_labels==code]))
    
    if(length(colnames(probs)) > 1) {
      best_prob_name <- colnames(t(which.max(probs[x,])))[1]
      parse_prob_name <- strsplit(best_prob_name,"_")
      create_label_name <- paste(parse_prob_name[[1]][1],"_LABEL",sep="")
    } else {
      best_prob_name <- colnames(probs)
      parse_prob_name <- strsplit(best_prob_name,"_")
      create_label_name <- paste(parse_prob_name[[1]][1],"_LABEL",sep="")
    }
    
    agree_score <- as.vector(max(dist))
    best_label <- as.vector(unique[which.max(dist)])
    best_prob <- as.vector(classification_results[create_label_name][x,])
    
    if (agree_score == 1) best_label <- best_prob
    
    list(best_prob,agree_score,best_label)
  },.parallel=T)

  best_probs<-do.call("rbind",lapply(cores_dump,"[[",1))
  agree_scores<-do.call("rbind",lapply(cores_dump,"[[",2))
  best_labels<-do.call("rbind",lapply(cores_dump,"[[",3))
	
	return(cbind(labels,BEST_LABEL=as.numeric(best_labels),BEST_PROB=best_probs, NUM_AGREE=agree_scores))
}
