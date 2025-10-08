// Activity Selection Problem
// ---------------------------
// Problem:
// Given N activities with start and end times, select the maximum number of activities
// that can be performed by a single person, assuming the person can work on only one
// activity at a time.
//
// Approach (Greedy):
// 1. Sort all activities by their finishing times.
// 2. Select the first activity and then choose the next activity whose start time
//    is greater than or equal to the finish time of the previously selected one.
//
// Time Complexity: O(N log N) due to sorting
// Space Complexity: O(N)

package main

import (
	"fmt"
	"sort"
)

// Activity represents a single task with a start and end time
type Activity struct {
	start, end int
}

// activitySelection selects the maximum number of non-overlapping activities
func activitySelection(activities []Activity) []Activity {
	sort.Slice(activities, func(i, j int) bool {
		return activities[i].end < activities[j].end
	})

	var result []Activity
	lastEnd := -1
	for _, act := range activities {
		if act.start >= lastEnd {
			result = append(result, act)
			lastEnd = act.end
		}
	}
	return result
}

func main() {
	activities := []Activity{
		{5, 9}, {1, 2}, {3, 4}, {0, 6}, {5, 7}, {8, 9},
	}
	selected := activitySelection(activities)
	fmt.Println("Selected Activities:")
	for _, a := range selected {
		fmt.Printf("(%d, %d) ", a.start, a.end)
	}
	fmt.Println()
}
