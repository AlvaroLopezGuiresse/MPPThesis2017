# MPPThesis2017

Repository for thesis on Political Finance Regulation by Alvaro Lopez


1) PFR Index
It is a panel database, on which for every latin american country the evolution of the 31 questions of the IDEA survey are tracked in time for each one of them. Thus, a panel of 20 years - 18 countries - 31 questions (11.160 onservations) is used in order to calculate the PFR Index.


1_appendix
  - Data management for the IDEA appendix. As input takes the references of the survey, on which every one of its questions,
  is linked to the legal source used to answer it (yes-no anwswer). Based on this legal source, is registered the year that each   question is enacted. 


2_PFRI_panel

The IDEA appendix and the survey are combined, to build the dataset panel of 20 years - 18 countries - 31 questions (11.160 onservations)

  Example: 
  1- For Argentina, 2009, Question_12, is coded with a YES in the survey. Thus, thhat question has a value of 1 from year 2009 till 2015 (end of the panel), and of 0 from 1996 to 2008.
  2- For Argentina, 2009, Question_11, is coded with a NO in the survey. Then, that question has a value of 0 all trough the panel.
  
  This is done for every question in the dataset.


3_PFRI_index_calculation
The Political Finance Index (PFR) index is calculated. Thus an average for all of the questions are done for every year for every country, the result is a dataset of 18 countries and 10 years.




2) DATABASE 
The PFR Index is merged with additional datasets.



3) FIGURES & ANALYSiS
Data is sumarized in order to build figures and conduct OLS analysis.




