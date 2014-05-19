1357280
=======

Week 10: Text classification, clustering and topic models\\
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
4. Classification process: Function: resultsClssifiers
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
                             report of correlation between clusters and original tags.

Examples:
 Select section 1 and 2 and run the code. This will generate several variables (not all are important)
 Use the function featureSele(method), where method=(chi.squared, linear.correlation, rank.correlation,
                                                      information.gain, gain.ratio, symmetrical.uncertainty,
                                                      oneR) <- All of these function are part of the FSelector pachage 
                   check point: dim(d) should be equal to x31  
