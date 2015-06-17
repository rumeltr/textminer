#   THIS FUNCTION RUNS A BUNCH OF DIFFERENT ROUTINES ON A WELL-FORMATED DATASET (*),
#   SAVES THE OUTPUT IN THE SPECIFIED DIRECTORY, AND IS CAPABLE OF RETURNING UP TO
#   TWO VARIABLES FOR FURTHER ANALYSIS IN R.

#   Information for the users of this code:
#     
#   Aside from using this project as a means of learning the basics of machine learning
#   and text mining, I also used it as an excuse to improve my competency in the R
#   language. Writing this code took a great deal of trial and error, and I found
#   myself referencing many helpful online resoures when I encountered conceptual challenges
#   or errors that I did not understand. The vast majority of the time I spent online
#   I spent on StackOverflow (stackoverflow.com), although I also referenced a number
#   of other sources, including inside-R (inside-r.org) and the personal blogs
#   of R users. In most cases, I tried to learn from the sources I found
#   rather than copying code that I found online verbatim. There were a few functions
#   which I  discovered while doing my research that suited needs particularly well,
#   and these I included in my code with attribution to the content creators. However,
#   there are many cases there are only a few ways of doing something in code, and as a result
#   some of my code may resemble code used in particular online examples that are not cited here,
#   whether or not I ever looked at them. If, while using  this code, you happen to
#   notice any section of code that is sufficiently similar to a particular online example,
#   to warrant a specific citation, especially if it resembles code you yourself have written,
#   please let me know as soon as possible. I will determine whether I looked at that example
#   at any point, and if I did, I will edit my code to add an attribution to the example in
#   question. It is not my goal to plagiarize from anyone, and I will happily address any
#   missing citations as soon as I am made aware of it. Thank you.

#   List of all code citations:
#     
#   A. Liaw and M. Wiener (2002). Classification and Regression by randomForest. R News 2(3), 18--22.
#   
#   Andrea Peters and Torsten Hothorn (2015). ipred: Improved Predictors. R package version 0.9-4.
#   http://CRAN.R-project.org/package=ipred
#   
#   Brian Ripley (2015). tree: Classification and Regression Trees. R package version 1.0-35.
#   http://CRAN.R-project.org/package=tree
#   
#   Christian Buchta, Kurt Hornik, Ingo Feinerer and David Meyer (2015). tau: Text Analysis Utilities. R package version
#   0.0-18. http://CRAN.R-project.org/package=tau
#   
#   David Meyer, Evgenia Dimitriadou, Kurt Hornik, Andreas Weingessel and Friedrich Leisch (2014). e1071: Misc Functions of the
#   Department of Statistics (e1071), TU Wien. R package version 1.6-4. http://CRAN.R-project.org/package=e1071
#     
#   Dirk Eddelbuettel and Romain Francois (2011). Rcpp: Seamless R and C++ Integration. Journal of Statistical Software, 40(8),
#   1-18. URL http://www.jstatsoft.org/v40/i08/.
#   
#   Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical Software, 40(1), 1-29.
#   URL http://www.jstatsoft.org/v40/i01/.
#     
#   Ingo Feinerer, Kurt Hornik, and David Meyer (2008). Text Mining Infrastructure in R. Journal of Statistical Software 25(5):
#   1-54. URL: http://www.jstatsoft.org/v25/i05/.
#   
#   Jarek Tuszynski (2014). caTools: Tools: moving window statistics, GIF, Base64, ROC AUC, etc.. R package version 1.17.1.
#   http://CRAN.R-project.org/package=caTools
#   
#   Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization Paths for Generalized Linear Models via Coordinate
#   Descent. Journal of Statistical Software, 33(1), 1-22. URL http://www.jstatsoft.org/v33/i01/.
#   
#   R Core Team (2015). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
#   Vienna, Austria. URL http://www.R-project.org/.
#   
#   Revolution Analytics and Steve Weston (2014). doParallel: Foreach parallel adaptor for the parallel package. R package
#   version 1.0.8. http://CRAN.R-project.org/package=doParallel
#   
#   Rinker, T. W. (2013). qdap: Quantitative Discourse Analysis Package. version 2.2.0. University at Buffalo. Buffalo, New
#   York. http://github.com/trinker/qdap
#
#   Sokolova, M., & Lapalme, G. (2009). A systematic analysis of performance measures for classification tasks.
#   Information Processing and Management, 45, p. 427-437.
#   
#   Timothy P. Jurka, Loren Collingwood, Amber E. Boydstun, Emiliano Grossman and Wouter van Atteveldt (2014). RTextTools:
#   Automatic Text Classification via Supervised Learning. R package version 1.4.2.
#   http://CRAN.R-project.org/package=RTextTools
#   
#   Timothy P. Jurka and Yoshimasa Tsuruoka (2013). maxent: Low-memory Multinomial Logistic Regression with Support for Text
#   Classification. R package version 1.3.3.1. http://CRAN.R-project.org/package=maxent
#   
#   Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition. Springer, New York. ISBN
#   0-387-95457-0
#   
#   Xavier Robin, Natacha Turck, Alexandre Hainard, Natalia Tiberti, Frédérique Lisacek, Jean-Charles Sanchez and Markus Müller
#   (2011). pROC: an open-source package for R and S+ to analyze and compare ROC curves. BMC Bioinformatics, 12, p. 77.  DOI:
#   10.1186/1471-2105-12-77 <http://www.biomedcentral.com/1471-2105/12/77/>

#   * = Information on how to use this function:
#   
#   The function model_run_as_function_generalized can be used to mine
#   categorical text classifications from a dataset that has at least three
#   types of columns.
#   
#   "index_column" specifies an index to be used to distinguish unique rows
#   in the dataset (if not set, a column called "INDEX" is created in the
#   dataset with a unique key given to each row).
#
#   "data_column" specifies a column containing free-form text
#   descriptions as a data source for mining.
#
#   "classify_column" specifies a column in the dataset containing categorical
#   variables  (one per row), which are typically ontological terms, describing each 
#   entry in the dataset.
#
#   Function arguments that are frequently used include:
#
#   "data_file_path": a path to the CSV file containing the dataset
#
#   "ontology_file_path": a path to a CSV file containing a list of all
#   possible variables in "classify_column"
#
#   "stopwords_file_path": a path to a text file containing custom stopwords
#
#   "analytics_base_dir": a path to a directory in which to store
#   analytics results.
#
#   Other arguments set script runtime variables:
#
#   "algorithms": specifies which text classification algorithms to use when
#   generating the model ensemble (currently, the default is all algorithms
#   supported by the RTextTools package).
#
#   "min_num_for_samples": specifies the dataset occurance threshold 
#   below which a given categorical variable will not be classified
#   (classifications that fit into this category are lumped and relabled
#   as "other").
#
#   "use.cores": specifies how many cores the machine running the code is
#   allowed to allocate for parallel processing.
#
#   "model_runs": specifies how many ensemble model runs shouldbe performed.
#   If "model_runs" is greater than 1, "model_runs" number of training sets
#   will be selected, and each will be used to fit a different collection of
#   models, which will then be used to classify the testing set (and any unclassified
#   data) and produce a consensus classification.
#
#   "prune_records_without_ontology_terms": specifies whether records
#   without ontology terms in "data_column" should be removed ("ontology_file_path"
#   must be set if this argument is set to true).
#
#   "search_full_ontology_words": specifies whether entries should be
#   removed from the dataset if they don't contain exact matches to any of the
#   ontology terms (e.g. does "replaying" count as a valid ontology term instance
#   if "play" is the base ontology term).
#   
#   "custom_stopword_removal": specifies whether stopwords contained in the custom
#   stopwords file should be stripped from the dataset ("stopwords_file_path" must
#   be set if this argument is set to true.
#
#   "number_of_subsamples": specifies whether to sample multiple times from
#   the dataset and combine the samples to create a larger training set, and if
#   so, how many samples are to be made.
#
#   "return_variable_1": specifies a variable in the environment of the function
#   to be returned as output. This function has no return value if this argument
#   is not set.
#
#   "return_variable_2": specifies a second variable to be returned. If specified,
#   this variable and "return_variable_1" will be returned as elements of a list.
#   If this argument is set, "return_variable_1" must also be set if either are
#   to be returned.

model_run_as_function_generalized<-function(source_code_base,
                                data_file_path,
                                ontology_file_path,
                                stopwords_file_path,
                                analytics_base_dir,
                                algorithms=c("BAGGING","GLMNET","NNET","RF","SVM","TREE","BOOSTING"),
                                index_column=NULL,
                                data_column="COMBINED_NOTES",
                                classify_column="PRIMARY_SPECIES_BEHAVIOR_1",
                                min_num_for_samples=40,
                                use.cores=1,
                                model_runs=1,
                                prune_records_without_ontology_terms=F,
                                search_full_ontology_words=F,
                                custom_stopword_removal=T,
                                number_of_subsamples=1,
                                return_variable_1=NULL,
                                return_variable_2=NULL){
  
  ################## INITALIZE SCRIPT ##################
  
  cat("Note: this script takes a long time to run when used on large training and testing sets\n")
  flush.console()
  
  # Begin timer...
  
  time_started <- proc.time()
  
  # Load packages:
  
  cat("Loading required packages...\n")
  flush.console()
  
  my_packages<-c("RTextTools","tm","doParallel","parallel","maxent","plyr","compiler","qdap","pROC","Rcpp")
  
  sapply(my_packages,library,character.only=TRUE)
  
  #   The R files listed below have been adapted from the RTextTools package,
  #   the use of which requires the following citation (also listed previously):
  #   
  #   Timothy P. Jurka, Loren Collingwood, Amber E. Boydstun, Emiliano Grossman and Wouter van Atteveldt (2012).
  #   RTextTools: Automatic Text Classification via Supervised Learning. R package version 1.3.9.
  #   http://CRAN.R-project.org/package=RTextTools
  #   
  #   The latest CRAN release of this package has a publication date of 2014-01-19.
  #   I have taken the opportunity to update a few of the core functions of this
  #   package to take advantage of relatively new methods of parallel processing
  #   that have easier to accomplish in R since this package was last
  #   updated. (Note... it would be a good idea to contact the primary pacakge
  #   author to check whether this package is still being actively maintained,
  #   and if so, whether they would like to include any of this work in a
  #   future package release).
  
  # Load source code:
  
  cat("Loading source code from external files...\n")
  flush.console()
  
  source(paste(source_code_base,"train_models_mod.R",sep="")) # Parallel model training
  source(paste(source_code_base,"classify_models_mod.R",sep="")) # Parallel model classification
  source(paste(source_code_base,"create_container_mod.R",sep="")) # Unused variable removed to reduce processing time
  source(paste(source_code_base,"create_matrix_mod.R",sep="")) # Typo corrected, allows classification of virgin data
  source(paste(source_code_base,"create_analytics_mod.R",sep="")) # Parallel analytics
  source(paste(source_code_base,"create_scoreSummary_mod.R",sep="")) # Parallel analytics
  source(paste(source_code_base,"create_precisionRecallSummary_mod.R",sep="")) # Parallel analytics
  
  source(paste(source_code_base,"helper_functions.R",sep="")) # Custom functions
  
  compileMyCppFunction(source_code_base)
  
  ################## MAIN BODY OF SCRIPT BEGINS HERE ##################
  
  options(error=dump.frames(),warn=1)
  
  cat("Setting model run type...\n")
  flush.console()
  
  # This string is used to create the analytics directory name
  
  if(model_runs==1){
    run_type="standard_run"
  } else{
    run_type="bootstrapping"
  }
  
  # Read CSV file:
  
  cat("Reading CSV file...\n")
  flush.console()
  
  data_original<-read.csv(data_file_path,header=T,stringsAsFactors=F,encoding="latin1")
  
  if(is.null(index_column)){
    index_column<-"INDEX"
    data_original<-cbind(INDEX=seq_along(data_original[[data_column]]),data_original)
  }
  
  # Recoding any data column values labeled "blank" as "":
  
  cat("Recode any data column values labeled \"blank\" as \"\"...\n")
  flush.console()
  
  data_original[data_original[[classify_column]]=="blank",classify_column]<-""
  
  # Copy rows with empty behaviors to a "virgin" data frame:
  
  cat("Copying rows with empty data column to \"virgin\" data frame...\n")
  flush.console()
  
  data_raw_blanks<-data_original[data_original[[classify_column]]=="",]
  
  if(nrow(data_raw_blanks)==0){
    classify_virgin_data=F
  } else{
    classify_virgin_data=T
  }
  
  data_not_blank<-data_original[!(data_original[[index_column]] %in% data_raw_blanks[[index_column]]),]
  
  # Recode either "song," "call," or both to "other" (caveat:
  # it seems that not recoding them increases the overall
  # prediction accuracy of the models)
  
  # Custom stopword removal (English stopwords plus a few fairly common words in the biological sciences.
  # Note that this setting will also be applied to the virgin data):
  
  if(custom_stopword_removal){
    
    # Based on the "Long Stopwords List" from Ranks NL:
    # http://www.ranks.nl/stopwords
    
    cat("Removing stopwords...\n")
    flush.console()
    
    stopwords<-read.table(stopwords_file_path,header=F,stringsAsFactors=F)$V1
    pattern<-create_pattern(stopwords)
    system.time({
      data_not_blank[[data_column]]<-mgsub(pattern,"",data_not_blank[[data_column]])
      data_not_blank[[data_column]]<-mgsub("(subject notes:|public notes:|notes:)","",data_not_blank[[data_column]])
      data_not_blank[[data_column]]<-mgsub("  "," ",data_not_blank[[data_column]])
    })[3] # Prune stopwords
  }
  
  # Remove records with comments that do not contain any of the ontology terms
  # (note that this setting will later be applied to the virgin data as well):
  
  if(prune_records_without_ontology_terms){
    
    cat("Removing records where comment text lacks ontology terms...\n")
    flush.console()
    
    ontology<-read.csv(ontology_file_path,header = T,stringsAsFactors=F,fileEncoding="ISO8859-1")[,1]
    data_not_blank<-data_not_blank[grepl(pattern = create_pattern(ontology,full.words=search_full_ontology_words),x = data_not_blank[[data_column]], ignore.case = T) & (data_not_blank[[classify_column]]!="blank" & data_not_blank[[classify_column]]!=""),]
  }
  
  # Split off training and testing sets:
  
  cat("Generating training and testing sets...\n")
  flush.console()
  
  if(model_runs>1){
    list_of_training_sets<-lapply(1:model_runs,function(x) {
      sample<-stratifiedSample(data_not_blank,classify_column,min_num=min_num_for_samples)
      sample$SET<-"training"
      return(sample)
    })
  } else{
    list_of_training_sets<-lapply(1:number_of_subsamples,function(x) {
      sample<-stratifiedSample(data_not_blank,classify_column,min_num=min_num_for_samples)
      sample$SET<-"training"
      return(sample)
    })
  }
  
  training_set<-do.call(rbind,list_of_training_sets)
  training_set<-training_set[!(duplicated(training_set)),]
  testing_set<-data_not_blank[
    !(data_not_blank[[index_column]] %in% training_set[[index_column]]) & (data_not_blank[[classify_column]] %in% training_set[[classify_column]]),
    ]
  testing_set$SET<-"testing"
  
  combined<-rbind(training_set,testing_set)
  
  # Specify algorithms (currently using all of the algorithms available in the RTextTools package):
  
  use<-algorithms
  
  # Note. In the following sections, the following warning:
  # Warning: <anonymous>: ... may be used in an incorrect context: ‘.fun(piece, ...)’
  # is supressed by supressWarnings() in the source code.
  # This is a known bug in the plyr package, and does not indicate altered performace.
  
  if(model_runs>1) { # This happens if we resample the training set when training models
    
    cat("Consolidating merged datasets into list...\n")
    flush.console()
    
    # Make list of training sets
    
    list_of_data_merges<-lapply(1:model_runs,function(x){
      rbind(list_of_training_sets[[x]],testing_set)
    })
    
    # Put terms from each training set into list
    # (important if they ever differ between training
    # sets... but not sure whether that is the case
    # very often)
    
    list_of_terms<-lapply(1:model_runs,function(x) {
      meetsCriteriaForStratifiedSample(list_of_data_merges[[x]],classify_column,min_num=min_num_for_samples)
    })
    
    # Create a lookup table for decoding numerical indices later
    
    lookup_table<-getLookupTable(combined$PRIMARY_SPECIES_BEHAVIOR_1,
                                 meetsCriteriaForStratifiedSample(combined,classify_column,min_num = min_num_for_samples))
    
    cat("Recoding text classifications with numerical index...\n")
    flush.console()
    
    # Recode text classifications with numerical indices
    
    for(x in 1:model_runs){
      list_of_data_merges[[x]][[classify_column]]<-recodeWithNumericIndex(data=list_of_data_merges[[x]][[classify_column]],
                                                                                  relevant_levels=meetsCriteriaForStratifiedSample(
                                                                                    combined,
                                                                                    classify_column,
                                                                                    min_num = min_num_for_samples))
    }
    
    cat("Creating parallel processing cluster...\n")
    flush.console()
    
    # Make a cluster of sockets (n = use.cores) for parallel processing.
    # Additionally, "spawn" the C++ code to each member of the cluster
    
    CL<-createCluster(use.cores, logfile=paste(analytics_base_dir,"Logs/logfile.txt",sep=""), export=ls(.GlobalEnv), lib=my_packages)
    clusterExport(CL,c("source_code_base"),envir = environment())
    clusterEvalQ(CL,compileMyCppFunction(source_code_base))
    
    cat("Converting text comments to RTextTools containers...\n")
    flush.console()
    
    # Make document matricies out of each merged dataset, send them to the
    # cluster, then create containers out of them (a la RTextTools)
    
    system.time({
      list_of_doc_matricies<-suppressWarnings(alply(.data = 1:model_runs, .margins = 1, .fun = function(x){
        create_matrix_mod(list_of_data_merges[[x]][[data_column]],
                          language="english",
                          removeNumbers=TRUE,
                          stemWords=TRUE,
                          removeSparseTerms=0.998,
                          weighting=tm::weightTfIdf)
      },.parallel=T))
    })[3] # Create Text Document Matrices
    
    clusterExport(CL,varlist=c("list_of_doc_matricies"), envir=environment())
    
    system.time({
      list_of_containers<-alply(1:model_runs,1,function(x){
        create_container_corrected(list_of_doc_matricies[[x]],
                                   list_of_data_merges[[x]][[classify_column]],
                                   trainSize=1:nrow(list_of_training_sets[[x]]),
                                   testSize=(nrow(list_of_training_sets[[x]])+1):nrow(list_of_data_merges[[x]]),
                                   virgin=FALSE)
      },.parallel = T)
    })[3] # Create RTextTools Container
    
    # Do garbage cleanup on the cluster, then send the containers
    # off to each member (the garbage cleanup line of code appears
    # a number of times thoughout the script, and for the sake of
    # timeliness will not be explained over and over again)
    
    clusterEvalQ(CL,gc())
    
    clusterExport(CL,varlist=c("list_of_containers"), envir=environment())
    
    cat("Training models on training sets...\n")
    flush.console()
    
    # Train models in parallel by splitting the job up between the
    # members of the clusters
    
    system.time({
      list_of_model_lists<-alply(1:model_runs,1,function(x){
        train_models_parallel(list_of_containers[[x]],algorithms)
      })
    })[3] # Train models
    
    clusterEvalQ(CL,gc())
    
    # Send returned models to clusters
    
    clusterExport(CL,varlist=c("list_of_model_lists"), envir=environment())
    
    cat("Classifying testing sets with new models...\n")
    flush.console()
    
    # Classify testing sets with models in parallel
    
    system.time({
      list_of_results<-alply(1:model_runs,1,function(x){
        classify_models_parallel(list_of_containers[[x]],list_of_model_lists[[x]])
      })
    })[3] # Classify test set
    
    clusterEvalQ(CL,gc())
    
    cat("Creating basic analytics...\n")
    flush.console()
    
    # Create basic analytics in parallel
    
    system.time({
      list_of_analytics<-alply(1:model_runs,1,function(x){
        create_analytics_parallel(list_of_containers[[x]],list_of_results[[x]])
      })  
    })[3] # Create Analytics object
    
    clusterEvalQ(CL,gc())
    
    # Collect document labels and probabilities for further analysis
    
    list_of_document_summaries<-alply(1:model_runs,1,function(x){
      return(as.data.frame(list_of_analytics[[x]]@document_summary))
    })
    
    label_headers<-alply(1:model_runs,1,function(x){
      return(colnames(list_of_document_summaries[[x]])[grep("(_LABEL|_CODE)",colnames(list_of_document_summaries[[x]]))])
    })
    
    a_ply(1:model_runs,1,function(x){
      eval(parse(text=paste(alply(label_headers[[x]],1,function(y){
        paste(
          "list_of_document_summaries[[",x,
          "]]$",y,
          "<<-recoderFunc(list_of_document_summaries[[",x,
          "]]$",y,
          ", oldvalue = lookup_table$INDEX, newvalue = lookup_table$VALUE)",
          sep="")
      }),sep="; ")))
    })
    
    cat("Extracting document labels and classification probabilities...\n")
    flush.console()
    
    for(i in 1:model_runs){
      list_of_document_summaries[[i]]<-cbind(CATNUM=testing_set[[index_column]],list_of_document_summaries[[i]])
    }
    
    big_document_summary<-as.matrix(do.call(cbind,alply(1:model_runs,1,function(x) {
      return(list_of_document_summaries[[x]])
    })))
    
    big_document_summary_labels<-as.matrix(do.call(cbind,alply(1:model_runs,1,function(x) {
      return(list_of_document_summaries[[x]][,grep("_LABEL",colnames(list_of_document_summaries[[x]]))])
    })))
    
    big_document_summary_probs<-as.matrix(do.call(cbind,alply(1:model_runs,1,function(x) {
      return(list_of_document_summaries[[x]][,grep("_PROB",colnames(list_of_document_summaries[[x]]))])
    })))
    
    cat("Finding maximum likelyhood classifications...\n")
    flush.console()
    
    # Determine maximum likelihood classifications using
    # the custom consensus function
    
    big_modes<-t(sapply(1:nrow(big_document_summary_labels),function(x){
      return(up_to_three_weighted_modes(big_document_summary_labels[x,],big_document_summary_probs[x,]))
    }))
    
    multiple_behavior_analysis<-data.frame(CATNUM=testing_set[[index_column]],
                                           MANUAL=testing_set[[classify_column]],
                                           FIRST=big_modes[,1],
                                           SECOND=big_modes[,2],
                                           THIRD=big_modes[,3],
                                           stringsAsFactors = F)
    
    # Send "terms" to cluster in preparation for decoding
    # the classifications
    
    terms<-lookup_table$VALUE
    clusterExport(CL,c("terms"),envir = environment())
    
  } else{
    # Merge training and testing sets into data frame:
    
    cat("Consolidating training and testing sets...\n")
    flush.console()
    
    data_merge<-rbind(training_set,testing_set)
    
    # Encode classes with numeric indicies:
    
    cat("Recoding text classifications with numerical index...\n")
    flush.console()
    
    terms<-meetsCriteriaForStratifiedSample(data_merge,
                                            classify_column,
                                            min_num=min_num_for_samples)
    data_merge[[classify_column]]<-recodeWithNumericIndex(
      data_merge[[classify_column]],
      relevant_levels=terms)
    
    cat("Converting text comments to RTextTools containers...\n")
    flush.console()
    
    system.time({
      doc_matrix<-create_matrix_mod(data_merge[[data_column]],
                                    language="english",
                                    removeNumbers=TRUE,
                                    stemWords=TRUE,
                                    removeSparseTerms=0.998,
                                    weighting=tm::weightTfIdf)
    })[3] # Create Text Document Matrix
    
    system.time({
      container<-create_container_corrected(doc_matrix,
                                            data_merge[[classify_column]],
                                            trainSize=1:nrow(training_set),
                                            testSize=(nrow(training_set)+1):nrow(data_merge),
                                            virgin=FALSE)
    })[3] # Create RTextTools Container
    
    cat("Creating parallel processing cluster...\n")
    flush.console()
    
    CL<-createCluster(use.cores, logfile=paste(analytics_base_dir,"Logs/logfile.txt",sep=""), export=ls(.GlobalEnv), lib=my_packages)
    clusterExport(CL,c("source_code_base"),envir = environment())
    clusterEvalQ(CL,compileMyCppFunction(source_code_base))
    
    cat("Training models on training sets...\n")
    flush.console()
    
    system.time({
      models<-train_models_parallel(container,algorithms)
    })[3] # Train models
    
    cat("Classifying testing set using new models...\n")
    flush.console()
    
    system.time({
      results<-classify_models_parallel(container,models)
    })[3] # Classify test set
    
    cat("Creating basic analytics...\n")
    flush.console()
    
    system.time({
      analytics<-create_analytics_parallel(container,results)
    })[3] # Create Analytics object
    
    # Dump to data frames:
    
    label_summary<-as.data.frame(analytics@label_summary)
    document_summary<-as.data.frame(analytics@document_summary)
    algorithm_summary<-as.data.frame(analytics@algorithm_summary)
    ensemble_summary<-as.data.frame(analytics@ensemble_summary)
    
    # Decode numeric labels and perform necessary formatting:
    
    lookup_table<-getLookupTable(data_merge[[classify_column]],relevant_levels = terms)
    
    row.names(algorithm_summary)<-recoderFunc(row.names(algorithm_summary),
                                              oldvalue = lookup_table$INDEX,
                                              newvalue = lookup_table$VALUE)
    
    label_headers<-colnames(document_summary)[grep("(_LABEL|_CODE)",colnames(document_summary))]
    
    eval(parse(text=paste(lapply(label_headers,function(x){
      paste("document_summary$",x,
            "<-recoderFunc(document_summary$",x,
            ", oldvalue = lookup_table$INDEX, newvalue = lookup_table$VALUE)",
            sep="")
    }),sep="; ")))
    
    row.names(label_summary)<-recoderFunc(row.names(label_summary),
                                          oldvalue = lookup_table$INDEX,
                                          newvalue = lookup_table$VALUE)
    
    document_summary<-cbind(CATNUM=testing_set[[index_column]],document_summary)
    
    cat("Extracting document labels and classification probabilities...\n")
    flush.console()
    
    document_summary_labels<-as.matrix(document_summary[,grep("_LABEL",colnames(document_summary))])
    document_summary_probs<-as.matrix(document_summary[,grep("_PROB",colnames(document_summary))])
    
    # Do custom consensus analysis to determine three most probable behavior classifications for each record:
    
    clusterExport(CL,varlist=c("document_summary_labels", "document_summary_probs"), envir = environment())
    
    cat("Finding maximum likelyhood classifications...\n")
    flush.console()
    
    system.time({
      modes<-aaply(.data = 1:nrow(document_summary), .margins = 1, .fun = function(x){
        up_to_three_weighted_modes(labels = document_summary_labels[x,],
                                   probs = document_summary_probs[x,])
      })
    })[3] # Performance test
    
    #return(document_summary)
    
    multiple_behavior_analysis<-data.frame(CATNUM=document_summary$CATNUM,
                                           MANUAL=document_summary$MANUAL_CODE,
                                           FIRST=modes[,1],
                                           SECOND=modes[,2],
                                           THIRD=modes[,3],
                                           stringsAsFactors = F)
  }
  
  cat("Calculating match probabilities...\n")
  flush.console()
  
  # Find how many records are correctly classified by the
  # highest likelihood classification, the first or second
  # highest classification, or by any of the three highest
  # classifications:
  
  multiple_behavior_analysis$MATCH_3<-(multiple_behavior_analysis[,"MANUAL"]==multiple_behavior_analysis[,"FIRST"]) | (multiple_behavior_analysis[,"MANUAL"]==multiple_behavior_analysis[,"SECOND"]) | (multiple_behavior_analysis[,"MANUAL"]==multiple_behavior_analysis[,"THIRD"])
  
  multiple_behavior_analysis$MATCH_2<-(multiple_behavior_analysis[,"MANUAL"]==multiple_behavior_analysis[,"FIRST"]) | (multiple_behavior_analysis[,"MANUAL"]==multiple_behavior_analysis[,"SECOND"]) # Get those that match in the top two
  
  multiple_behavior_analysis$MATCH_1<-multiple_behavior_analysis[,"MANUAL"]==multiple_behavior_analysis[,"FIRST"]
  
  # Print current capture rate for n most probable classifications and given probability threshold (currently, t=1.0):
  
  length(multiple_behavior_analysis[multiple_behavior_analysis$MATCH_3==TRUE,"CATNUM"])/nrow(multiple_behavior_analysis)
  length(multiple_behavior_analysis[multiple_behavior_analysis$MATCH_2==TRUE,"CATNUM"])/nrow(multiple_behavior_analysis)
  length(multiple_behavior_analysis[multiple_behavior_analysis$MATCH_1==TRUE,"CATNUM"])/nrow(multiple_behavior_analysis)
  
  cat("Determining the effect of incrementally increasing the classification probability threshold...\n")
  flush.console()
  
  # Determine the effect of incremementally increasing the classification
  # threshold on match probabilities, precision, and recall:
  
  if(model_runs>1){
    modes_list<-analyze_threshold_effects(index=testing_set[[index_column]],
                                          manual=testing_set[[classify_column]],
                                          labels=big_document_summary_labels,
                                          probs=big_document_summary_probs,
                                          thresholdMin=0.01,
                                          thresholdMax=1)
  } else{
    system.time({
      modes_list<-analyze_threshold_effects(index=document_summary$CATNUM,
                                            manual=document_summary$MANUAL_CODE,
                                            labels=document_summary_labels,
                                            probs=document_summary_probs,
                                            thresholdMin=0.01,
                                            thresholdMax=1)
    })[3] # Performance test
  }
  
  threshold=(1:100)/100
  
  threshold_eval<-data.frame(THRESHOLD=(1:100)/100)
  
  threshold_eval$MATCH_FREQ_3<-laply(.data = modes_list,.fun = function(x){
    length(x[x$MATCH_3==TRUE,"MATCH_3"])/nrow(x)
  }) # Get captured frequencies, three modes (n=3)
  
  threshold_eval$MATCH_FREQ_2<-laply(.data = modes_list,.fun = function(x){
    length(x[x$MATCH_2==TRUE,"MATCH_2"])/nrow(x)
  }) # Get captured frequencies, two modes (n=2)
  
  threshold_eval$MATCH_FREQ_1<-laply(.data = modes_list,.fun = function(x){
    length(x[x$MATCH_1==TRUE,"MATCH_1"])/nrow(x)
  }) # Get captured frequencies, one mode (n=1)
  
  cat("Calculating precision, recall, and F-scores...\n")
  flush.console()
  
  # After (Godbole):
  
  threshold_eval$PRECISION_3<-laply(.data = modes_list,.fun=function(x){
    precisionMultiLabel(x$MANUAL,x[,c("FIRST","SECOND","THIRD")])
  })
  threshold_eval$PRECISION_2<-laply(.data = modes_list,.fun=function(x){
    precisionMultiLabel(x$MANUAL,x[,c("FIRST","SECOND")])
  })
  threshold_eval$PRECISION_1<-laply(.data = modes_list,.fun=function(x){
    precisionMultiLabel(x$MANUAL,x[,"FIRST"])
  })
  
  threshold_eval$RECALL_3<-laply(.data = modes_list,.fun=function(x){
    recallMultiLabel(x$MANUAL,x[,c("FIRST","SECOND","THIRD")])
  })
  threshold_eval$RECALL_2<-laply(.data = modes_list,.fun=function(x){
    recallMultiLabel(x$MANUAL,x[,c("FIRST","SECOND")])
  })
  threshold_eval$RECALL_1<-laply(.data = modes_list,.fun=function(x){
    recallMultiLabel(x$MANUAL,x[,"FIRST"])
  })
  
  threshold_eval$F1_SCORE_3<-laply(.data = modes_list,.fun=function(x){
    F1MultiLabel(x$MANUAL,x[,c("FIRST","SECOND","THIRD")])
  })
  threshold_eval$F1_SCORE_2<-laply(.data = modes_list,.fun=function(x){
    F1MultiLabel(x$MANUAL,x[,c("FIRST","SECOND")])
  })
  threshold_eval$F1_SCORE_1<-laply(.data = modes_list,.fun=function(x){
    F1MultiLabel(x$MANUAL,x[,"FIRST"])
  })
  
  # Calculate AUC values for ROC curves at different threshold values
  
  cat("Calculating AUC values for ROC curves at various threshold values...\n")
  flush.console()
  
  roc<-data.frame(THRESHOLD=(1:100)/100)
  
  roc$AUC_1<-suppressWarnings(laply(.data = modes_list, .fun = function(x){
    as.numeric(multiclass.roc(recodeWithNumericIndex(x$MANUAL,terms),
                              recodeWithNumericIndex(x$FIRST,terms))$auc)
  },.parallel=T))
  
  roc$AUC_2<-suppressWarnings(laply(.data = modes_list, .fun = function(x){
    as.numeric(multiclass.roc(recodeWithNumericIndex(x$MANUAL,terms),
                              recodeWithNumericIndex(x$SECOND,terms))$auc)
  },.parallel=T))
  
  roc$AUC_3<-suppressWarnings(laply(.data = modes_list, .fun = function(x){
    as.numeric(multiclass.roc(recodeWithNumericIndex(x$MANUAL,terms),
                              recodeWithNumericIndex(x$THIRD,terms))$auc)
  },.parallel=T))
  
  clusterEvalQ(CL,gc())
  
  # Now, classify virgin data
  
  if(classify_virgin_data){
    cat("Moving to \"virgin\" data...\n")
    flush.console()
    
    if(custom_stopword_removal){
      
      cat("Removing stopwords from unclassified data...\n")
      flush.console()
      
      pattern<-create_pattern(stopwords)
      system.time({
        data_raw_blanks[[data_column]]<-mgsub(pattern,"",data_raw_blanks[[data_column]])
        data_raw_blanks[[data_column]]<-mgsub("(subject notes:|public notes:|notes:)","",data_raw_blanks[[data_column]])
        data_raw_blanks[[data_column]]<-mgsub("  "," ",data_raw_blanks[[data_column]])
      })[3] # Prune stopwords
    }
    
    if(prune_records_without_ontology_terms){
      
      cat("Removing records from unclassified data where comment text lacks ontology terms...\n")
      flush.console()
      
      data_raw_blanks<-data_raw_blanks[grepl(
        pattern = create_pattern(ontology,full.words=search_full_ontology_words),
        x = data_raw_blanks[[data_column]],
        ignore.case = T),]
    }
    
    if(model_runs>1){
      
      cat("Consolidating merged unclassified datasets into list...\n")
      flush.console()
      
      system.time({
        doc_matrices_list_virgin<-alply(1:model_runs,1,function(x){
          create_matrix_mod(data_raw_blanks[[data_column]],
                            language="english",
                            removeNumbers=TRUE,
                            stemWords=TRUE,
                            removeSparseTerms=0.998,
                            weighting=tm::weightTfIdf,
                            originalMatrix = list_of_doc_matricies[[x]])
        })
      })[3] # Create Text Document Matrix
      
      cat("Converting unclassified text comments to RTextTools containers...\n")
      flush.console()
      
      system.time({
        containers_list_virgin<-alply(1:model_runs,1,function(x){
          create_container_corrected(doc_matrices_list_virgin[[x]],
                                     data_raw_blanks[[classify_column]],
                                     testSize=1:nrow(data_raw_blanks),
                                     virgin=TRUE)
        })
      })[3] # Create RTextTools Container
      
      clusterExport(CL,varlist=c("containers_list_virgin"), envir=environment())
      
      cat("Classifying \"virgin\" data with previously-generated models...\n")
      flush.console()
      
      system.time({
        results_virgin_list<-alply(1:model_runs,1,function(x){
          classify_models_parallel(containers_list_virgin[[x]],list_of_model_lists[[x]])
        })
      })[3] # Classify virgin set
      
      clusterEvalQ(CL,gc())
      
      label_headers_list_virgin<-alply(1:model_runs,1,function(x){
        colnames(results_virgin_list[[x]])[grep("(_LABEL|_CODE)",colnames(results_virgin_list[[x]]))]
      })
      
      for(i in 1:model_runs){
        eval(parse(text=paste(lapply(label_headers_list_virgin[[x]],function(x){
          paste("results_virgin_list[[",i,
                "]]$",x,
                "<-recoderFunc(results_virgin_list[[",i,
                "]]$",x,", oldvalue = lookup_table$INDEX, newvalue = lookup_table$VALUE)"
                ,sep="")
        }),sep="; ")))
      }
      
      results_virgin_labels<-as.matrix(do.call(cbind,alply(1:model_runs,1,function(x) {
        return(results_virgin_list[[x]][,grep("_LABEL",colnames(results_virgin_list[[x]]))])
      })))
      
      results_virgin_probs<-as.matrix(do.call(cbind,alply(1:model_runs,1,function(x) {
        return(results_virgin_list[[x]][,grep("_PROB",colnames(results_virgin_list[[x]]))])
      })))
      
      cat("Finding maximum likelyhood classifications...\n")
      flush.console()
      
      system.time({
        modes_virgin<-aaply(.data = 1:nrow(results_virgin_labels), .margins = 1, .fun = function(x){
          up_to_three_weighted_modes(labels = results_virgin_labels[x,],probs = results_virgin_probs[x,])
        })
      })[3] # Performance test
      
    } else{
      
      cat("Converting unclassified text comments to RTextTools container...\n")
      flush.console()
      
      system.time({
        doc_matrix_virgin<-create_matrix_mod(data_raw_blanks[[data_column]],
                                             language="english",
                                             removeNumbers=TRUE,
                                             stemWords=TRUE,
                                             removeSparseTerms=0.998,
                                             weighting=tm::weightTfIdf,
                                             originalMatrix = doc_matrix)
      })[3] # Create Text Document Matrix
      
      system.time({
        container_virgin<-create_container_corrected(doc_matrix_virgin,
                                                     data_raw_blanks[[classify_column]],
                                                     testSize=1:nrow(data_raw_blanks),
                                                     virgin=TRUE)
      })[3] # Create RTextTools Container
      
      clusterExport(CL,varlist=c("container_virgin"), envir=environment())
      
      cat("Classifying \"virgin\" data with previously-generated models...\n")
      flush.console()
      
      system.time({
        results_virgin<-classify_models_parallel(container_virgin,models)
      })[3] # Classify virgin set
      
      label_headers_virgin<-colnames(results_virgin)[grep("(_LABEL|_CODE)",colnames(results_virgin))]
      
      eval(parse(text=paste(lapply(label_headers_virgin,function(x){
        paste("results_virgin$",x,
              "<-recoderFunc(results_virgin$",x,
              ", oldvalue = lookup_table$INDEX, newvalue = lookup_table$VALUE)"
              ,sep="")
      }),sep="; ")))
      
      results_virgin_labels<-as.matrix(results_virgin[,grep("_LABEL",colnames(results_virgin))])
      results_virgin_probs<-as.matrix(results_virgin[,grep("_PROB",colnames(results_virgin))])
      
      cat("Finding maximum likelyhood classifications...\n")
      flush.console()
      
      system.time({
        modes_virgin<-aaply(.data = 1:nrow(results_virgin), .margins = 1, .fun = function(x){
          up_to_three_weighted_modes(labels = results_virgin_labels[x,],probs = results_virgin_probs[x,])
        })
      })[3] # Performance test
    }
    
    multiple_behavior_virgin<-data.frame(
      CATNUM=data_raw_blanks[[index_column]],
      FIRST=modes_virgin[,1],
      SECOND=modes_virgin[,2],
      THIRD=modes_virgin[,3],
      stringsAsFactors = F
    )
  }
  
  # Dump to files:
  
  cat("Saving plots and structured data to analytics directory...\n")
  flush.console()
  
  stamp = gsub("( |:)","_",strftime(Sys.time()))
  
  analytics_dump_dir<-paste(analytics_base_dir,"analytics_",stamp,"_",run_type,"/",sep="")
  dir.create(analytics_dump_dir,recursive = T)
  
  png(paste(analytics_dump_dir,"alg_perf.png",sep=""),width=800,height=896,units="px")
  
  plot(y = as.numeric(threshold_eval$MATCH_FREQ_3),
       x = as.numeric(threshold_eval$THRESHOLD),
       pch=13,
       main="Match Frequency by Algorithm Confidence",
       xlab="Threshold",
       ylab="Proportion Captured (Top n Behavior Estimates)",
       col="green",
       ylim=c(min(threshold_eval$MATCH_FREQ_3,threshold_eval$MATCH_FREQ_2,threshold_eval$MATCH_FREQ_1),
              max(threshold_eval$MATCH_FREQ_3,threshold_eval$MATCH_FREQ_2,threshold_eval$MATCH_FREQ_1)))
  points(threshold_eval$MATCH_FREQ_2~threshold_eval$THRESHOLD,pch=12,col="blue")
  points(threshold_eval$MATCH_FREQ_1~threshold_eval$THRESHOLD,pch=10,col="red")
  legend('bottomleft',c("Top 3 behaviors","Top 2 behaviors","Top behavior"),lty=1,col=c("green","blue","red"),bty="n")
  
  dev.off()
  
  png(paste(analytics_dump_dir,"prec_3.png",sep=""),width=800,height=896,units="px")
  plot(threshold_eval$PRECISION_3~threshold_eval$THRESHOLD,
       col="green",
       main="Precision by Algorithm Confidence - Top 3 Behavior Estimates",
       xlab="Threshold",
       ylab="Precision (weighted by manual count)")
  dev.off()
  
  png(paste(analytics_dump_dir,"rec_3.png",sep=""),width=800,height=896,units="px")
  plot(threshold_eval$RECALL_3~threshold_eval$THRESHOLD,
       col="green",
       main="Recall by Algorithm Confidence - Top 3 Behavior Estimates",
       xlab="Threshold",
       ylab="Recall (weighted by manual count)")
  dev.off()
  
  png(paste(analytics_dump_dir,"f1_3.png",sep=""),width=800,height=896,units="px")
  plot(threshold_eval$F1_SCORE_3~threshold_eval$THRESHOLD,
       col="green",
       main="F1-Score by Algorithm Confidence - Top 3 Behavior Estimates",
       xlab="Threshold",
       ylab="Weighted F1-Score")
  dev.off()
  
  png(paste(analytics_dump_dir,"prec_2.png",sep=""),width=800,height=896,units="px")
  plot(threshold_eval$PRECISION_2~threshold_eval$THRESHOLD,
       col="green",
       main="Precision by Algorithm Confidence - Top 2 Behavior Estimates",
       xlab="Threshold",
       ylab="Precision (weighted by manual count)")
  dev.off()
  
  png(paste(analytics_dump_dir,"rec_2.png",sep=""),width=800,height=896,units="px")
  plot(threshold_eval$RECALL_2~threshold_eval$THRESHOLD,
       col="green",
       main="Recall by Algorithm Confidence - Top 2 Behavior Estimates",
       xlab="Threshold",
       ylab="Recall (weighted by manual count)")
  dev.off()
  
  png(paste(analytics_dump_dir,"f1_2.png",sep=""),width=800,height=896,units="px")
  plot(threshold_eval$F1_SCORE_2~threshold_eval$THRESHOLD,
       col="green",
       main="F1-Score by Algorithm Confidence - Top 2 Behavior Estimates",
       xlab="Threshold",
       ylab="Weighted F1-Score")
  dev.off()
  
  png(paste(analytics_dump_dir,"prec_1.png",sep=""),width=800,height=896,units="px")
  plot(threshold_eval$PRECISION_1~threshold_eval$THRESHOLD,
       col="green",
       main="Precision by Algorithm Confidence - Top Behavior Estimate",
       xlab="Threshold",
       ylab="Precision (weighted by manual count)")
  dev.off()
  
  png(paste(analytics_dump_dir,"rec_1.png",sep=""),width=800,height=896,units="px")
  plot(threshold_eval$RECALL_1~threshold_eval$THRESHOLD,
       col="green",
       main="Recall by Algorithm Confidence - Top Behavior Estimate",
       xlab="Threshold",
       ylab="Recall (weighted by manual count)")
  dev.off()
  
  png(paste(analytics_dump_dir,"f1_1.png",sep=""),width=800,height=896,units="px")
  plot(threshold_eval$F1_SCORE_3~threshold_eval$THRESHOLD,
       col="green",
       main="F1-Score by Algorithm Confidence - Top Behavior Estimate",
       xlab="Threshold",
       ylab="Weighted F1-Score")
  dev.off()
  
  png(paste(analytics_dump_dir,"multiclass_AUC.png",sep=""),width=800,height=896,units="px")
  min_auc<-min(roc$AUC_1,roc$AUC_2,roc$AUC_3)
  max_auc<-max(roc$AUC_1,roc$AUC_2,roc$AUC_3)
  plot(roc$AUC_1~roc$THRESHOLD,
       ylim = c(0,1),
       main="Multiclass AUC For Top n Behaviors by Threshold",
       xlab = "Threshold",
       ylab="AUC",
       col="red")
  points(roc$AUC_2~roc$THRESHOLD,ylim = c(0,1), col="blue")
  points(roc$AUC_3~roc$THRESHOLD,ylim = c(0,1), col="green")
  legend('bottomleft',c("Top 3 behaviors","Top 2 behaviors","Top behavior"),lty=1,col=c("green","blue","red"),bty="n")
  dev.off()
  
  if(model_runs>1){
    write.csv(big_document_summary,paste(analytics_dump_dir,"big_document_summary.csv",sep=""))
  } else{
    write.csv(algorithm_summary,paste(analytics_dump_dir,"algorithm_summary.csv",sep=""))
    write.csv(document_summary,paste(analytics_dump_dir,"document_summary.csv",sep=""))
    write.csv(ensemble_summary,paste(analytics_dump_dir,"ensemble_summary.csv",sep=""))
    write.csv(label_summary,paste(analytics_dump_dir,"label_summary.csv",sep=""))
  }
  
  write.csv(multiple_behavior_analysis,paste(analytics_dump_dir,"multiple_behavior_analysis.csv",sep=""))
  if(classify_virgin_data){
    write.csv(multiple_behavior_virgin,paste(analytics_dump_dir,"multiple_behavior_virgin.csv",sep=""))
  }
  
  cat("Dumping workspace to analytics directory for debugging purposes...\n")
  flush.console()
  
  save(list = ls(all = TRUE),file = paste(analytics_dump_dir,"raw_dump.RData",sep=""))
  
  cat("Saving runtime variables to analytics directory...\n")
  flush.console()
  
  prefs<-paste("source_code_base: ",source_code_base,"\n",
               "data_file_path: ",data_file_path,"\n",
               "ontology_file_path: ",ontology_file_path,"\n",
               "stopwords_file_path: ",stopwords_file_path,"\n",
               "analytics_base_dir: ",analytics_base_dir,"\n",
               "algorithms: ",paste(algorithms,collapse=", "),"\n",
               "min_num_for_samples: ",min_num_for_samples,"\n",
               "use.cores: ",use.cores,"\n",
               "prune_records_without_ontology_terms: ",prune_records_without_ontology_terms,"\n",
               "search_full_ontology_words: ",search_full_ontology_words,"\n",
               "custom_stopword_removal: ",custom_stopword_removal,"\n",
               "run_type/codebase: ",run_type,"\n",
               "number_of_subsamples: ",number_of_subsamples,"\n",
               "model_runs: ",model_runs,sep="")
  
  writeLines(prefs,paste(analytics_dump_dir,"prefs.txt",sep=""))
  stopCluster(CL)
  gc(verbose = F)
  
  end_time<-(proc.time() - time_started)
  cat(paste("Script ran for ",round(end_time[3],digits=2)," seconds\n",sep=""))
  
  if(classify_virgin_data){
    legal_output<-c("algorithm_summary", "algorithms", "combined", "container", "container_virgin", "document_summary", "document_summary_labels", "document_summary_probs", "ensemble_summary", "label_headers", "label_headers_virgin", "label_summary", "lookup_table", "min_auc", "max_auc", "multiple_behavior_analysis", "multiple_behavior_virgin", "results", "results_virgin", "results_virgin_labels", "results_virgin_probs", "roc", "terms", "testing_set", "threshold", "threshold_eval", "training_set")
  } else{
    legal_output<-c("algorithm_summary", "algorithms", "combined", "container", "container_virgin", "document_summary", "document_summary_labels", "document_summary_probs", "ensemble_summary", "label_headers", "label_summary", "lookup_table", "min_auc", "max_auc", "multiple_behavior_analysis", "results", "results_virgin", "roc", "terms", "testing_set", "threshold", "threshold_eval", "training_set")
  }
  
  if(!(is.null(return_variable_1))){
    if(is.character(return_variable_1) & return_variable_1 %in% legal_output){
      if(!(is.null(return_variable_2))){
        if(is.character(return_variable_2) & return_variable_2 %in% legal_output){
          return(list(x=eval(parse(text=return_variable_1)), y=eval(parse(text=return_variable_2))))
        }
      } else{
        return(eval(parse(text = return_variable_1)))
      }
    }
  }
}