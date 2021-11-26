# currencies-arbitrage
Scala script implementation to solve the [currencies arbitrage puzzle](https://priceonomics.com/jobs/puzzle/).

## Short Description
An MVC architecture has been used inside the script to seperate the different "layers" of the script. The **Model** object defines the core components of the script: the nodes, edges, graph and cycle enabling to represent and manipulate the edges and exchanges rates. The **View** object gathers the IO interactions functionnalities (requesting, decoding and verifying the raw data as well as preparing the output). Finally, the **Controller** object gathers data transformation and manipulation including the [Bellman-Ford algorithm](https://en.wikipedia.org/wiki/Bellman%E2%80%93Ford_algorithm) and negative cycles retracing used here to find the arbitrage loops.

## Algorithm 
A common approach to find arbitrage loops is making use of Bellman-Ford algorithm which enables to: 
- for all the nodes of a directed graph to find the shortest paths for any pair of nodes
- detect if negative cycles exist among the directed graph 

In our case, the currencies are the nodes and the rate between 2 currencies the weight of the edges of a directed graph. By composing the rate exchange between 2 currencies by f: x => -ln(x), we know that if the sum of the f-composed rates of a cycle is negative then the product of theses rates is superior to 1. Which proves the existence of an arbitrage opportunity. Through our modelization we make use of Bellman-Ford algorithm to detect the edges proving the existence of negative cycles among the directed graph. What remains is following the predecessors of the nodes of this "guilty" edge to find the arbitrage opportunities.

## Time complexity
The time complexity is the same as Bellman-Ford ALgorithm one: ```O(EV)```, where: 
- E is the total number of edges (the rates)
- V is the total number ofnodesin the graph (currencies)

We should note that the graphs representing the rates between currencies are generally densed graphs (as we often get the exchanges between every pair of currency). So in our case we often have ```E = O(V * V)```with an overall time complexity of ```O(V^3)```.

## Prerequesites
To run the script you need [Ammonite](http://ammonite.io/#Ammonite) installed locally. I ran it with the Java 11.0.12 and Scala 2.13.6.

## How to run
1. Clone the repository:
 ```sh
git clone https://github.com/k3lyan/currencies-arbitrage.git
```
2. From this repository, run:
```sh
amm arbitrageSearcher.sc
``` 
