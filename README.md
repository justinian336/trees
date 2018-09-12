<p align="center">
  <img src ="https://thumbs.gfycat.com/SoupyAmazingHammerkop-size_restricted.gif" />
</p>

<h6 align="center">
  Check <a href="https://www.youtube.com/watch?v=AJvCnFqSViA&t=883s">steezyasfuck</a> while you read
</h6>
                                                                                
# Trees
A functional implementation of k-dimensional trees in Scala for extra-fast nearest neighbor search, based on the algorithm by Friedman, Bentley and Finkel (1975).

Persons interested in comparing the functional and imperative programming styles might find it interesting to compare the code in this repo with the Annex 2 of the [original paper](https://www.slac.stanford.edu/pubs/slacpubs/1500/slac-pub-1549.pdf).

## Concept
A KD Tree is a data structure designed to locate "similar" data points nearby, given a dissimilarity metric. By doing so, searching for the *m* closest points to a query point can be done at a higher speed than the benchmark *brute force* method (simply getting the distance from each point in the dataset to the query, sorting assendingly and taking the *m* top elements). Its usage is convenient for applications that make heavy use of the Nearest Neighbor algorithm, such as [Local Outlier Factor](http://www.dbs.ifi.lmu.de/Publikationen/Papers/LOF.pdf).

## Usage
Data units are represented by instances of the `Distance` trait. You can easily create your own classes by extending the trait. The following example defines the `Manhattan` trait, which implements the Manhattan distance metric:

```scala
trait Manhattan[T <: Distance[T]] extends Distance[T]{self:T=>

    override def distance(other: T): BigDecimal = {
      features
        .zip(other.features)
        .map{case (x, y)=>
          (x-y).abs
        }
        .foldLeft(BigDecimal(0))(_ + _)
    }
  }
```

You can now use it to construct your own case class:

```scala
case class DataPoint(features: List[BigDecimal]) extends Distance.Manhattan[DataPoint]
```

where the features argument contains the list of features, and where the size of the list defines the dimensionality of your data.

### Growing a tree

Given a `List[DataPoint]`, you can create your KDTree in the following way:

```scala
KDTree.grow(data)
```

which returns a `Future[KDTree[DataPoint]]`, since creating the tree can take some time depending on the size of your dataset.

### Finding the m-closest neighbors

For a given `query: DataPoint` and `tree: KDTree[DataPoint]`, you can find the *m* closest neighbors in the following way:

```scala
tree.nnSearch(m, query)
```

Which returns a List of tuples of the *m* nearest neighbors along with their distance to the target.

## Performance

For reference, results obtained with a simulated 4-dimensional dataset of 100,000 observations suggest that searching for the 20 nearest neighbors of a datapoint can take 0.33 seconds using the brute force approach, and only 0.09 seconds using the KDTree, for an efficiency gain of roughly 74%. These results may vary however depending on the hardware, the dimensionality of the dataset relative to its size (because curse of dimensionality), and the position of the query point.

## Running the Demo
The *Example* object in the root scala directory contains an example which creates a large dataset, and evaluates the performance gain of the KDTree over the benchmark brute force method. You can change the parameters, such as the size of the dataset and its dimensionality, and observe how performance changes. To run it, execute `sbt run` in your console, or run the application from your IDE. The results, including the nearest neighbors, the distance to the farthest nearest neighbor under both methods is shown for comparison. You should observe that, results are exactly the same using brute force and a KDTree, although performance should be superior for the tree, or equal in the worst case scenario (for example, where the dimensionality is too large relative to the size of the dataset).

## Future Work
- Type-level definitions of the Distance traits for extra type safety with case classes. A little annoying since implicits requirements are not fully propagated by Shapeless, which results in cluttered function definitions for libraries downstream.
- Operators such as map, flatMap, fold, as well as compositional operators such as append.
