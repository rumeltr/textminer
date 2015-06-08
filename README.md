textminer
=========

A few text mining routines written in R using a slightly modified version of the RTextTools package

Information for the users of this code:
    
Aside from using this project as a means of learning the basics of machine learning and text mining, I also used it as an excuse to improve my competency in the R language. Writing this code took a great deal of trial and error, and I found myself referencing many helpful online resoures when I encountered conceptual challenges or errors that I did not understand. The vast majority of the time I spent online I spent on StackOverflow (stackoverflow.com), although I also referenced a number of other sources, including inside-R (inside-r.org) and the personal blogs of R users. In most cases, I tried to learn from the sources I found rather than copying code that I found online verbatim. There were a few functions which I  discovered while doing my research that suited needs particularly well, and these I included in my code with attribution to the content creators. However, there are many cases there are only a few ways of doing something in code, and as a result some of my code may resemble code used in particular online examples that are not cited here, whether or not I ever looked at them. If, while using  this code, you happen to notice any section of code that is sufficiently similar to a particular online example, to warrant a specific citation, especially if it resembles code you yourself have written, please let me know as soon as possible. I will determine whether I looked at that example at any point, and if I did, I will edit my code to add an attribution to the example in question. It is not my goal to plagiarize from anyone, and I will happily address any missing citations as soon as I am made aware of it. Thank you.

List of all code citations:
    
A. Liaw and M. Wiener (2002). Classification and Regression by randomForest. R News 2(3), 18--22.
  
Andrea Peters and Torsten Hothorn (2015). ipred: Improved Predictors. R package version 0.9-4.
  http://CRAN.R-project.org/package=ipred
  
Brian Ripley (2015). tree: Classification and Regression Trees. R package version 1.0-35.
  http://CRAN.R-project.org/package=tree
  
Christian Buchta, Kurt Hornik, Ingo Feinerer and David Meyer (2015). tau: Text Analysis Utilities. R package version
  0.0-18. http://CRAN.R-project.org/package=tau
  
David Meyer, Evgenia Dimitriadou, Kurt Hornik, Andreas Weingessel and Friedrich Leisch (2014). e1071: Misc Functions of the
  Department of Statistics (e1071), TU Wien. R package version 1.6-4. http://CRAN.R-project.org/package=e1071
    
Dirk Eddelbuettel and Romain Francois (2011). Rcpp: Seamless R and C++ Integration. Journal of Statistical Software, 40(8),
  1-18. URL http://www.jstatsoft.org/v40/i08/.
  
Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical Software, 40(1), 1-29.
  URL http://www.jstatsoft.org/v40/i01/.
    
Ingo Feinerer, Kurt Hornik, and David Meyer (2008). Text Mining Infrastructure in R. Journal of Statistical Software 25(5):
  1-54. URL: http://www.jstatsoft.org/v25/i05/.
  
Jarek Tuszynski (2014). caTools: Tools: moving window statistics, GIF, Base64, ROC AUC, etc.. R package version 1.17.1.
  http://CRAN.R-project.org/package=caTools
  
Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization Paths for Generalized Linear Models via Coordinate
  Descent. Journal of Statistical Software, 33(1), 1-22. URL http://www.jstatsoft.org/v33/i01/.
  
R Core Team (2015). R: A language and environment for statistical computing. R Foundation for Statistical Computing,
  Vienna, Austria. URL http://www.R-project.org/.
  
Revolution Analytics and Steve Weston (2014). doParallel: Foreach parallel adaptor for the parallel package. R package
  version 1.0.8. http://CRAN.R-project.org/package=doParallel
  
Rinker, T. W. (2013). qdap: Quantitative Discourse Analysis Package. version 2.2.0. University at Buffalo. Buffalo, New
  York. http://github.com/trinker/qdap
  
Timothy P. Jurka, Loren Collingwood, Amber E. Boydstun, Emiliano Grossman and Wouter van Atteveldt (2014). RTextTools:
  Automatic Text Classification via Supervised Learning. R package version 1.4.2.
  http://CRAN.R-project.org/package=RTextTools
  
Timothy P. Jurka and Yoshimasa Tsuruoka (2013). maxent: Low-memory Multinomial Logistic Regression with Support for Text
  Classification. R package version 1.3.3.1. http://CRAN.R-project.org/package=maxent
  
Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition. Springer, New York. ISBN
  0-387-95457-0
  
Xavier Robin, Natacha Turck, Alexandre Hainard, Natalia Tiberti, Frédérique Lisacek, Jean-Charles Sanchez and Markus Müller
  (2011). pROC: an open-source package for R and S+ to analyze and compare ROC curves. BMC Bioinformatics, 12, p. 77.  DOI:
  10.1186/1471-2105-12-77 <http://www.biomedcentral.com/1471-2105/12/77/>

The metadata used in this text mining exploration comes from the Macaulay Library, a large collection of sound and audio files maintained by the Cornell Lab of Ornithology (http://macaulaylibrary.org/about). Although the CSV file containing the data used in this script is not currently available to the public, each entry in said CSV file comes from an audio file that may be accessed on the Macaulay Library website by any internet user. Although the Macaulay Library hosts video media in addition to sounds, none of the entries in this dataset pertain to video.
