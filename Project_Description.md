# Project goal:
Enhance an existing statistical programming language package for solving the Traveling Salesperson Problem (TSP)

## Existing Features:
1. TSPmeta feature set.
2. Additional features proposed by Pihera and Musliu.

## Missing Features:
1. Feature groups (15-32 and 33-43) proposed by Hutter et al.based on statistics of trajectories of short runs of the LK-algorithm
2. Statistics derived from 2s runs of the exact solver CONCORDE.

## Enhancement Focus:
Improving the completeness of the feature set addressing the missing components as suggested by Hutter et al.

## Normalization Options:
Consider options for normalization of feature values based on recently derived lower and upper bounds.

## Approach :
1. Understand  salesperson::getFeatureSet
2. Familiarize with the package by testing it using instances from TSPlib.
3. Or generate additional ones with  salesperson::generateRandomNetwork().
4. Read Hutter et al.'s paper, focusing on TSP features

## Implementation:

### 1. Local Search Features: Implement the LK algorithm.
feature calculation.

### 2. Branch and Cut Features: 
Execute CONCORDE for 2s and parse its output using R's system2() function.
Feature calculation.

## In presentation : 
Calculate features for different instance types and visualize results.
Eg : through pairwise scatter-plots, box-plots, etc.


# Algorithm runtime prediction: Methods & evaluation by FrankHutter

## 1. Local Search Probing Features∗ (expensive): are based on 20 short runs (1000 step search) of LK.
15–17. Tour Cost from construction heuristic: mean,variation coefficient,skew  
18–20. Local minimum tour length: mean, variation coefficient, skew  
21–23. Improvement Per Step:mean,variation coefficient,skew  
24–26. Steps To Local Minimum:mean,variation coefficient,skew  
27–29. Distance Between Local Minima:mean,variation coefficient, skew  
30–32. Probability Of Edges In Local Minima:mean,variation coefficient,skew

## 2. Branch and Cut Probing Features∗ (moderate):  
33–35. Improvement Per cut: mean,variation coefficient,skew  
36. Ratio Of Upper Bound And Lower Bound  
37–43. Solution After probing: Percentage Of integer values and non-integer value in the final solution after probing.For Non integer values,we compute static across nodes:min,max, 25%,50%,75%quantiles




