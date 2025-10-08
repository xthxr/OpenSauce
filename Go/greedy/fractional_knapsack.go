// Fractional Knapsack Problem
// ---------------------------
// Problem:
// Given weights and values of N items, and a knapsack with a weight capacity W,
// determine the maximum value that can be obtained. You can take fractions of items.
//
// Approach (Greedy):
// 1. Compute the value/weight ratio for each item.
// 2. Sort all items in decreasing order of this ratio.
// 3. Take items fully until the remaining capacity allows.
// 4. Take the fractional part of the next item if needed.
//
// Time Complexity: O(N log N)
// Space Complexity: O(1)

package main

import (
	"fmt"
	"sort"
)

type Item struct {
	value, weight float64
}

func fractionalKnapsack(capacity float64, items []Item) float64 {
	sort.Slice(items, func(i, j int) bool {
		return (items[i].value/items[i].weight) > (items[j].value/items[j].weight)
	})

	totalValue := 0.0
	for _, item := range items {
		if capacity >= item.weight {
			capacity -= item.weight
			totalValue += item.value
		} else {
			totalValue += item.value * (capacity / item.weight)
			break
		}
	}
	return totalValue
}

func main() {
	items := []Item{{60, 10}, {100, 20}, {120, 30}}
	capacity := 50.0
	fmt.Printf("Maximum value in knapsack = %.2f\n", fractionalKnapsack(capacity, items))
}
