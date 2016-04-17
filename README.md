
# Note

This project started a toy project to learn programming in Scala. Then I realised that it would be fun to implement some algorithms that I know, especially those of machine learning.

Here is a list of algorithms that I did, which I cannot claim the originality of the ideas, but more of initiativity and maybe some non-so-trivial implementation.

## Machine learning
   - Naive Bayes classification (multinomial and Bernoulli models)
   - Decision Tree (with either entropy or Gini impurity as the partition criteria)
   - Linear regression (the inverse of the matrix is the main tricky point)
   - K-means classification (K-means ++ centroid selection optimization)

## Misc
   - Page ranking
   - Merge sort
   - Gradient descent (batch and stochastic), an algorithm to calculate the local minimum.
   - Closest pair of points ( with the complexity of O(n*lgn) )
   - Point in polygon 
   
# How to 

* Build the source files

   The script "build.sh" is intended to build an individual or all source files all together. The output of the compiling would be generated under the "target" folder.

   e.g.   > ./build.sh naive_bayes.scala

* Run the algorithm

   The script "run.sh" is used to run the compiled program together with the input from the "test" folder.

   e.g.   > ./run.sh naive_bayes.scala
