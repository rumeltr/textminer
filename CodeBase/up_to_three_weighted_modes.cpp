#include <Rcpp.h>
#include <string.h>
using namespace Rcpp;

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

// [[Rcpp::export]]
CharacterVector findIntersect(CharacterVector a, CharacterVector b)
{
  return(intersect(a,b));
}

// [[Rcpp::export]]
CharacterVector rowWiseIntersect(CharacterMatrix a, CharacterMatrix b)
{
  int nrows_1=a.nrow();
  int nrows_2=b.nrow();
  
  int ncols=std::max(a.ncol(),b.ncol());
  
  CharacterMatrix temp_output(0,0);
  CharacterMatrix output(0,0);
  
  if(nrows_1==nrows_2)
  {
    int nrows=nrows_1;
    temp_output=CharacterMatrix(nrows,ncols);
    for(int i=0; i<nrows; i++)
    {
      CharacterVector thisRow=findIntersect(a(i,_),b(i,_));
      for(int j=0; j<thisRow.length(); j++)
      {
        temp_output(i,j)=thisRow(j);
      }
    }
    
    int columnsWithData=0;
    
    for(int i=0; i<ncols; i++)
    {
      CharacterVector thisColumn=temp_output(_,i);
      CharacterVector uniqueValues=unique(thisColumn);
      if(uniqueValues.size()==1)
      {
        std::string uniqueValuesString=Rcpp::as<std::string>(uniqueValues);
        
        if(uniqueValuesString!="")
        {
          columnsWithData++;
        }
      }
      else
      {
        columnsWithData++;
      }
    }
    
    output=CharacterMatrix(nrows,columnsWithData);
    
    for(int i=0; i<columnsWithData; i++)
    {
      output(_,i)=temp_output(_,i);
    }
  }
  return(output);
}

// [[Rcpp::export]]
NumericVector getLengthOfMatrixRowNonBlank(CharacterMatrix x)
{
  int nrows=x.nrow();
  NumericVector output(nrows);
  
  for(int i=0; i<nrows; i++)
  {
    CharacterVector thisRow=x(i,_);
    int numberOfNonBlankEntriesInRow=0;
    
    for(int j=0; j<thisRow.size(); j++)
    {
      if(thisRow[j]!="")
      {
        numberOfNonBlankEntriesInRow++;
      }
    }
    output[i]=numberOfNonBlankEntriesInRow;
  }
  return(output);
}