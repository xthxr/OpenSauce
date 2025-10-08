// Job Sequencing with Deadlines
// ------------------------------
// Problem:
// Given N jobs where each job has a deadline and profit associated,
// schedule jobs to maximize total profit such that each job takes 1 unit of time
// and no two jobs overlap.
//
// Approach (Greedy):
// 1. Sort all jobs by decreasing profit.
// 2. Assign each job to the latest available slot before its deadline.
// 3. Keep track of total profit and job order.
//
// Time Complexity: O(N^2) (can be optimized using DSU to O(N log N))
// Space Complexity: O(N)

package main

import (
	"fmt"
	"sort"
)

type Job struct {
	id     string
	dead   int
	profit int
}

func jobSequencing(jobs []Job) ([]string, int) {
	sort.Slice(jobs, func(i, j int) bool {
		return jobs[i].profit > jobs[j].profit
	})

	maxDeadline := 0
	for _, job := range jobs {
		if job.dead > maxDeadline {
			maxDeadline = job.dead
		}
	}

	slots := make([]string, maxDeadline)
	totalProfit := 0

	for _, job := range jobs {
		for j := job.dead - 1; j >= 0; j-- {
			if slots[j] == "" {
				slots[j] = job.id
				totalProfit += job.profit
				break
			}
		}
	}

	return slots, totalProfit
}

func main() {
	jobs := []Job{
		{"a", 2, 100}, {"b", 1, 19}, {"c", 2, 27}, {"d", 1, 25}, {"e", 3, 15},
	}
	slots, profit := jobSequencing(jobs)
	fmt.Println("Job sequence:", slots)
	fmt.Println("Total Profit:", profit)
}
