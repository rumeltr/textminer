classify_models_parallel <- function(container, models, ...) {
# helper method to make it easier to classify with models by algorithm name(s)
# output is a cbinded matrix of model predictions
# hopefully, this method can disappear after refactoring train_model
	pred_list<-suppressWarnings(alply(names(models), .margins = 1, .fun = function(x){
	  classify_model(container, models[[x]], ...)
	},.parallel = T))
	result<-do.call(cbind,pred_list)
	names(result)<-gsub("[0-9][.]","",names(result))
	return(result)
}