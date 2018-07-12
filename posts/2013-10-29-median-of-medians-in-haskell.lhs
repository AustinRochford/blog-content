---
title: Median-of-Medians in Haskell
tags: Algorithms, Haskell
---

This post is a followup to my [previous post](http://www.austinrochford.com/posts/2013-10-28-median-of-medians.html), about the median-of-median algorithm for selecting the $i$-th smallest element from an unsorted list of $n$ elements.  I had originally planned on including a Haskell implementation of the algorithm in that post, but, in order to increase the appeal of that post (Haskell is somewhat of a niche language), I decided to stick to a description of the algorithm and a theoretical analysis.  This post is an implementation of the median-of-medians algorithm in [literate Haskell](http://www.haskell.org/haskellwiki/Literate_programming).

For details about the median-of-medians algorithm, refer to my [previous post](http://www.austinrochford.com/posts/2013-10-28-median-of-medians.html).  Here we will only outline the algorithm for convenient reference.

1. Partition the list into sublists of length five.  (Note that the last list may have fewer than five elements.)
2. Calculate the median of each of the five-element sublists by sorting them.
3. Recursively compute the median of the medians from step 2, and call it $x$.
4. Partition the list using $x$ as a pivot.
5. Let the index of $x$ in the partitioned list be $k$.  If $i = k$, $x$ is the desired element.
6. If $i < k$, recurse into the sublist of elements smaller than $x$, looking for the $i$-th smallest element.
7. If $i > k$, recurse into the sublist of elements larger than $x$, looking for the $(i - k - 1)$-th smallest element.

We begin first by implementing step 2.

\begin{code}
import Control.Arrow ((&&&))

import Data.List ((!!), partition, sort)
import Data.List.Split (chunksOf)

indexOfMedian :: Int -> Int
indexOfMedian n = (n - 1) `div` 2

bruteForceMedian :: (Ord a) => [a] -> a
bruteForceMedian xs = (sort xs) !! (indexOfMedian $ length xs)
\end{code}

We now implement step 4.

\begin{code}
pivot :: (Ord a) => a -> [a] -> ([a], [a])
pivot x = filter (< x) &&& filter (> x)
\end{code}

We now use these two functions to implement the median-of-medians algorithm.

\begin{code}
select :: (Ord a, Show a) => Int -> [a] -> a
select 0 [x] = x
select i xs
    | i < k     = select i smaller
    | i == k    = x
    | otherwise = select (i - k - 1) larger
    where medians = map bruteForceMedian $ chunksOf 5 xs
          x = select (indexOfMedian $ length medians) medians
          (smaller, larger) = pivot x xs
          k = length smaller
\end{code}

We now write a simple test to validate `select`.

\begin{code}
testSelect :: Bool
testSelect = [1..100] == map (flip select (reverse [1..100])) [0..99]
\end{code}

This post is available as a [literate Haskell file](/resources/medians/MedianOfMedians.lhs).

