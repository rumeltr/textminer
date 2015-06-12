################## METHODS NEEDED FOR SCRIPT TO RUN ##################

# recoderFunc: given a vector "data," returns new vector where all instances of "oldvalue"
# are recoded as "newvalue"

recoderFunc<-function(data, oldvalue, newvalue, noMatchValue=NA) {
  
  # Adapted from:
  # https://susanejohnston.wordpress.com/2012/10/01/find-and-replace-in-r-part-2-how-to-recode-many-values-simultaneously/
  
  # convert any factors to characters
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  
  # create the return vector
  newvec <- data
  
  # put recoded values into the correct position in the return vector
  newvec[!(newvec %in% oldvalue)]<-noMatchValue
  for (i in unique(oldvalue)){
    newvec[data == i] <- newvalue[oldvalue == i]
  }
  return(newvec)
}

# recodeWithNumericIndex: given a vector "data," returns a new vector where all factor levels
#  are recoded as numeric indices

recodeWithNumericIndex<-function(data,relevant_levels=unique(data)){
  relevant_levels<-relevant_levels[!("other"==relevant_levels)]
  lookup_table<-data.frame(INDEX=1:length(relevant_levels),VALUE=relevant_levels,stringsAsFactors = F)
  data<-recoderFunc(data,lookup_table$VALUE,lookup_table$INDEX,noMatchValue=(max(lookup_table$INDEX)+1))
  return(as.numeric(data))
}

# getLookupTable: given a vector "data," returns a lookup table dataframe that relates each factor
# level to a numeric index (see recodeWithNumericIndex)

getLookupTable<-function(data,relevant_levels=unique(data)){
  relevant_levels<-relevant_levels[!("other"==relevant_levels)]
  lookup_table<-data.frame(INDEX=1:length(relevant_levels),VALUE=relevant_levels,stringsAsFactors = F)
  lookup_table<-rbind(lookup_table,c(max(lookup_table$INDEX)+1,"other"))
  return(lookup_table)
}

# meetsCriteriaForStratifiedSample: given a dataframe "data," and a column "select this column"
# containing factor levels, returns a vector of factor levels that occur more frequenly in the
# dataset than the cutoff "min_num"

meetsCriteriaForStratifiedSample<-function(dataset,select_this_column,min_num=0,choose=40){
  if(!(select_this_column %in% colnames(dataset))) stop("Invalid column name") # make sure column exists
  if(choose<min_num){ # make sure chose is less than or equal to min-num
    choose<-min_num
    print("\"choose\" cannot be less than \"min_num,\" has been set equal to \"min_num\"")
  }
  counts<-as.data.frame(table(dataset[,select_this_column])) # determine how many rows of each behavior there are
  relevant_levels<-as.character(counts[counts$Freq>min_num & counts$Var1!="","Var1"])
  return(relevant_levels)
}

# stratifiedSample: given a dataframe "data," a column "select this column" containing factor
# levels, returns a data frame containing only the rows of "data" with associated factor
# levels that occur more frequently in the dataset than the cutoff "min_num." If "choose" is not
# "ALL," return "choose" instances of each factor level. If "upto" is true, select up to "chose" instances
# of each factor level

stratifiedSample<-function(dataset,select_this_column,min_num=0,choose=min_num,upto=F){
  if(!(select_this_column %in% colnames(dataset))) stop("Invalid column name")
  if(choose<min_num){
    choose<-min_num
    print("\"choose\" cannot be less than \"min_num,\" has been set equal to \"min_num\"")
  }
  counts<-as.data.frame(table(dataset[,select_this_column]))
  counts<-counts[order(counts$Freq,decreasing=T),]
  relevant_levels<-as.character(counts[counts$Freq>min_num & counts$Var1!="","Var1"])
  
  if(choose=="ALL" & upto==F){
    result_set<-do.call(rbind,lapply(relevant_levels,function(x){
      dataset[which(dataset[[select_this_column]]==x),]
    }))
    if(length(unique(dataset[[select_this_column]] %in% relevant_levels))==1 && unique(dataset[[select_this_column]] %in% relevant_levels)=="TRUE"){
    } else{
      result_set<-rbind(result_set,dataset[which(!(dataset[[select_this_column]] %in% relevant_levels)),])
    }
  } else if(choose!="ALL" & upto==T){
    result_set<-do.call(rbind,lapply(relevant_levels,function(x){
      if(nrow(dataset[which(dataset[[select_this_column]]==x),])>choose){
        dataset[sample(which(dataset[[select_this_column]]==x),size=choose,replace=F),]
      } else{
        dataset[which(dataset[[select_this_column]]==x),]
      }
    }))
    if(length(which(!(dataset[[select_this_column]] %in% relevant_levels)))<choose){
      if(length(unique(dataset[[select_this_column]] %in% relevant_levels))==1 && unique(dataset[[select_this_column]] %in% relevant_levels)=="TRUE"){
      } else{
        result_set<-rbind(result_set,dataset[which(!(dataset[[select_this_column]] %in% relevant_levels)),])
      }
    } else {
      if(length(unique(dataset[[select_this_column]] %in% relevant_levels))==1 && unique(dataset[[select_this_column]] %in% relevant_levels)=="TRUE"){
      } else{
        result_set<-rbind(result_set,dataset[sample(which(!(dataset[[select_this_column]] %in% relevant_levels)),choose),])
      }
    }
  } else if(choose!="ALL" & upto==F){
    result_set<-do.call(rbind,lapply(relevant_levels,function(x){
      dataset[sample(which(dataset[[select_this_column]]==x),size=choose,replace=F),]
    }))
    if(length(unique(dataset[[select_this_column]] %in% relevant_levels))==1 && unique(dataset[[select_this_column]] %in% relevant_levels)=="TRUE"){
    } else{
      result_set<-rbind(result_set,dataset[sample(which(!(dataset[[select_this_column]] %in% relevant_levels)),choose),])
    }
  }
  result_set<-result_set[order(eval(parse(text=paste("result_set$",select_this_column,sep="")))),]
  return(result_set)
}

# Text manipulation functions:

# create_pattern: given words, returns a regex pattern to find those words in a string

create_pattern<-function(...,full.words=T){
  terms<-as.character(c(...))
  pattern<-paste("(",paste(terms,collapse="|"),")",sep="")
  pattern<-gsub("\n","|",pattern)
  if(full.words){
    pattern<-paste("\\<",pattern,"\\>",sep="")
  }
  return(pattern)
}

# rm.blank.entries: returns the input vector with all
# blank elements removed

rm.blank.entries<-function(vector){
  vector<-as.character(vector)
  vector<-vector[!(vector=="")]
  return(vector)
}

# all.the.same: checks whether all of the elements of a vector are the same

all.the.same<-function(df,rm.blanks=TRUE){
  apply(df,1,function(x){
    if(rm.blanks){
      x<-rm.blank.entries(x)  
    }
    return(length(unique(x))<=1)
  })
}

# up_to_three_weighted_modes: uses a custom consensus algorithm to report up to three
# of the most likely classifications based on ensemble classification results. Each model
# gets to "vote" for the classification it returns, with the value of the "vote" being
# equal to the classification probability returned by the model. All of the "votes" for
# each classification are summed up to give a score, and up to three of the classifications
# with the highest scores are returned. Any four-way ties for first, three-way ties for second,
# and two-way ties for third are eliminated to ensure that only well-supported classifications
# are returned. The source code for this function, which was written in C++, may be examined
# at the following location, which must be set manually:

path_to_cpp="~/Desktop/Smithsonian Macaulay Project/Main/up_to_three_weighted_modes.cpp"

Rcpp::sourceCpp(path_to_cpp)

# Assorted statistical error functions (may be used eventually).
# Translate names as:
# T = True, F = False, P = Positive, N = Negative
# E.g. FP determines the number of false positive
# classifications.

TP<-function(predicted,actual,classname){
  return(length(predicted[predicted==classname & actual==classname]))
}

FP<-function(predicted,actual,classname){
  return(length(predicted[predicted==classname & actual!=classname]))
}

TN<-function(predicted,actual,classname){
  return(length(predicted[predicted!=classname & actual!=classname]))
}

FN<-function(predicted,actual,classname){
  return(length(predicted[predicted!=classname & actual==classname]))
}

tpr_overall<-function(predicted,actual,classes){
  return(mean(sapply(classes,tpr_per_class,predicted=predicted,actual=actual)))
}

tpr_per_class<-function(predicted,actual,classname){
  TP(predicted,actual,classname)/(TP(predicted,actual,classname)+FN(predicted,actual,classname))
}

fpr_overall<-function(predicted,actual,classes){
  return(mean(sapply(classes,fpr_per_class,predicted=predicted,actual=actual)))
}

fpr_per_class<-function(predicted,actual,classname){
  FP(predicted,actual,classname)/(FP(predicted,actual,classname)+TN(predicted,actual,classname))
}

# createCluster: initiates a socket cluster for plyr and doParallel calls

createCluster<-function(noCores, logfile = "/Users/thorongilcvi/Desktop/log.txt", export = NULL, lib = NULL) {
  
  # Adapted from:
  # http://www.numbertheory.nl/2011/11/14/parallelization-using-plyr-loading-objects-and-packages-into-worker-nodes/
  
  cl <- makeCluster(noCores, type = "PSOCK", outfile = logfile)
  if(!is.null(export)) clusterExport(cl, export)
  if(!is.null(lib)) {
    l_ply(lib, function(dum) { 
      clusterExport(cl, "dum", envir = environment())
      clusterEvalQ(cl, library(dum, character.only = TRUE))
    })
  }
  registerDoParallel(cl)
  return(cl)
}

# analyze_threshold_effects: creates lists of highest probability
# classifications at a range of t-values

analyze_threshold_effects<-function(index, manual, labels, probs, thresholdMin=0.01, thresholdMax=1, by=0.01){
  if(thresholdMin>1 | thresholdMin<0 | thresholdMin>thresholdMax){
    stop("thresholdMin not valid")
  }
  if(thresholdMax>1 | thresholdMax<0 | thresholdMax<thresholdMin){
    stop("thresholdMax not valid")
  }
  modes_list<-foreach(j=seq(from = thresholdMin, to = thresholdMax, by=by)) %do% {
    this_mode<-do.call(rbind,lapply(1:nrow(labels),function(x){
      up_to_three_weighted_modes(labels = labels[x,], probs = probs[x,],threshold=j)
    }))
    this_multiple_behavior_analysis<-data.frame(CATNUM=index,
                                                MANUAL=manual,
                                                FIRST=this_mode[,1],
                                                SECOND=this_mode[,2],
                                                THIRD=this_mode[,3],
                                                stringsAsFactors = F)
    this_multiple_behavior_analysis$MATCH_3<-(this_multiple_behavior_analysis[,"MANUAL"]==this_multiple_behavior_analysis[,"FIRST"]) |(this_multiple_behavior_analysis[,"MANUAL"]==this_multiple_behavior_analysis[,"SECOND"]) | (this_multiple_behavior_analysis[,"MANUAL"]==this_multiple_behavior_analysis[,"THIRD"])
    this_multiple_behavior_analysis$MATCH_2<-(this_multiple_behavior_analysis[,"MANUAL"]==this_multiple_behavior_analysis[,"FIRST"]) |(this_multiple_behavior_analysis[,"MANUAL"]==this_multiple_behavior_analysis[,"SECOND"])
    this_multiple_behavior_analysis$MATCH_1<-this_multiple_behavior_analysis[,"MANUAL"]==this_multiple_behavior_analysis[,"FIRST"]
    return(this_multiple_behavior_analysis)
  }
}