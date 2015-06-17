train_models_parallel <- function(container, algorithms, ...) {
# helper method to make it easier to train models by algorithm name(s)
# output is a list(algorithm_name=model, ..) 
# hopefully, this method can disappear after refactoring train_model
  
  result<-suppressWarnings(alply(algorithms,.margins = 1,.fun = function(x){
    train_model(container, algorithm=x)
  },.parallel = T))
  names(result)<-algorithms
  
	return(result)
  
}