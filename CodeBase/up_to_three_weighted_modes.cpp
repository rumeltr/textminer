#include <Rcpp.h>
#include <string.h>

// Test two

// [[Rcpp::export]]
Rcpp::CharacterVector up_to_three_weighted_modes(Rcpp::CharacterVector labels, Rcpp::NumericVector probs, double threshold=0, bool inclusive=true)
{
	int original_labels_size=labels.size();
	int original_probs_size=probs.size();
	
	Rcpp::CharacterVector new_labels(original_labels_size);
	Rcpp::NumericVector new_probs(original_probs_size);
	
	if(inclusive)
	{
		for(int i=0; i<probs.length(); i++)
		{
			if(probs[i]>=threshold)
			{
				new_labels[i]=labels[i];
				new_probs[i]=probs[i];
			}
		}
	}
	else
	{
		for(int i=0; i<probs.length(); i++)
		{
			if(probs[i]>threshold)
			{
				new_probs[i]=probs[i];
				new_labels[i]=labels[i];
			}
		}
	}
	
	Rcpp::CharacterVector qualifiers=unique(new_labels);
  	int length_of_qualifiers=qualifiers.size();
	Rcpp::NumericVector qualifier_scores(qualifiers.size());
	
	for(int i=0; i<length_of_qualifiers; i++)
	{
	  std::string this_qualifier = Rcpp::as<std::string>(qualifiers[i]);
		
		for(int j=0; j<original_labels_size; j++)
		{
			std::string temp=Rcpp::as<std::string>(new_labels[j]);
			if(temp==this_qualifier)
			{
				qualifier_scores[i] += new_probs[j];
			}
		}
	}
	
	Rcpp::CharacterVector output(3);
	Rcpp::CharacterVector output_temp(original_labels_size);
	
	int counter=0;
	
	while(counter<3)
	{
		double top_score=max(qualifier_scores);
	  if(top_score==0)
	  {
	    break;
	  }
		
		for(int i=0; i<length_of_qualifiers; i++)
		{
			if(qualifier_scores[i]==top_score)
			{
				output_temp[counter]=qualifiers[i];
				qualifier_scores[i]=0;
				counter++;
			}
		}
		if(counter<=3)
		{
		  output=output_temp;
		}
	}
	
	Rcpp::CharacterVector subset_of_output(3);
	
	subset_of_output[0]=output[0];
	subset_of_output[1]=output[1];
	subset_of_output[2]=output[2];
	
	return(subset_of_output);
}

// [[Rcpp::export]]
Rcpp::CharacterMatrix modes_analysis(Rcpp::CharacterMatrix labels, Rcpp::NumericMatrix probs, double threshold=0, bool inclusive=true)
{
  int n=labels.nrow();
  Rcpp::CharacterMatrix output(n,3);
  for(int i=0; i<n; i++){
    Rcpp::CharacterVector resultForThisRow=up_to_three_weighted_modes(labels(i,Rcpp::_),probs(i,Rcpp::_),threshold,inclusive);
    output(i,Rcpp::_)=resultForThisRow;
  }
  return(output);
}
