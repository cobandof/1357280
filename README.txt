1357280
=======

Week 10: Text classification, clustering and topic models

Author: Catalina Obando Forero <C.Obando-Forero@warwick.ac.uk>
Student id number: 1357280

My code is divided in 7 sections:
1. R packages: list all the packages used and for which main task
2. Data preprocessing: This includes how to import the data to R into a corpus, 
                       cleaning, e.g. remove documents empty documents
                       split corpus into training and test sets, create binary classes for each of the most populous TOPIC tag
                       implemente preprocessing techniques discussed in the lectures,
                       creat document term matrix (tranning & test)
3. Feature Engineering: 3.1 Statistical techniques for feature selection,
                            Function: featureSele (written by author)
                        3.2 Topic models -> V-EM and Gibbs Sampling algorithms
4. Classification process: Function: resultsClssifiers (written by author)
                                     input: Dataframe (rows=documents, columns=weights of features)
                                     output: Accuracy, precision and recall overo 10-fold
                                             cross-validation and each of 10 populous classes
                                             for SVM, NB and RF.
5. Evaluation of clasification: Trains the "optimal" model on the complet TRAIN set and predicts the 
                                TEST set
6. Clustering process: Applies three clustering algorithms: k-means
                                                            Hierarchical 
                                                            Expectation-maximization
7. Evaluation of clustering: Report of clustering measures of quality,
                             report of correlation between clusters and original tags by function clusterCorr (written by author)


Example:

 Select section 1 and 2 and run the code. This will generate several variables (not all are important)
 Section 3.1 Use the function featureSele(method), where method=(chi.squared, linear.correlation, rank.correlation,
                                                      information.gain, gain.ratio, symmetrical.uncertainty,
                                                      oneR) <- All of these function are part of the FSelector pachage 
                       
                   In R: > Resfs<-featureSele(chi.squared)
                         > d<-d[,Resfs]
                         > d$Class1<-C[,3]
 Section section 3.2 and run the code. Run only one oh these at a time:
                                             > ResTm<-posterior(rts_TM$VEM)$topics
                                             > ResTm<-posterior(rts_TM$VEM_fixed)$topics
                                             > ResTm<-posterior(rts_TM$Gibbs)$topics 

 At the end of Section 3 you will have a matrix d [rows= trainng documents, columns features(features3.1 or features3.2)]
 Section  4 and 5. Run the first line > d$fold = cut(1:nrow(d), breaks=10, labels=F)
                   Then use the function resultsClssifiers(d, C) Input: (d, C[,3:12]). C[,1:2] containt other information not relevant at this point
                   They way to run the fucntion can be seen in line 264.

 Section 6 Select and run all the code from section 6. It will output several variables (some not very relevant or redundant)
                                                       it will also output plots of the clusters obtain by each algorithm 

 Section 7 Run the function clusterCorr(classes, clusters) where classes=BiClass[,3:12] (obtained in section 1)
                                                                 clusters = (clm, clhM, clEm) (obtained in section 6)
