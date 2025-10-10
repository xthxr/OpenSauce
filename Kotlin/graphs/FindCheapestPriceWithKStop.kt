/**
 * LeetCode Problem. It can be seen as graph problem. Solved using modified BFS, Bellman-Ford
 * Algorithm (https://en.wikipedia.org/wiki/Bellman%E2%80%93Ford_algorithm).
 *
 * <p>
 *
 * Problem: There are n cities connected by some number of flights. You are given an array flights
 * where flights[i] = [from_i, to_i, price_i] indicates that there is a flight from city from_i to
 * city to_i with cost price_i. You are also given three integers src, dst, and k, return the
 * cheapest price from src to dst with at most k stops. If there is no such route, return -1.
 */
fun findCheapestPrice(n: Int, flights: Array<IntArray>, src: Int, dst: Int, k: Int): Int {
  // 1. Build an adjacency list for the graph.
  val adjList = mutableMapOf<Int, MutableMap<Int, Int>>()
  for (flight in flights) {
    adjList.getOrPut(flight[0]) { mutableMapOf() }[flight[1]] = flight[2]
  }

  // 2. Initialize a distance array to store the cheapest price to each city.
  val dist = IntArray(n) { Integer.MAX_VALUE }
  dist[src] = 0

  // 3. Set up a queue for the BFS-style traversal.
  // We store a Triple: (current city, cost to get there, stops made).
  val queue = ArrayDeque<Triple<Int, Int, Int>>()
  queue.add(Triple(src, 0, 0))

  // 4. Process the queue.
  while (queue.isNotEmpty()) {
    val (node, cost, stops) = queue.removeFirst()

    // Don't explore further if we've exceeded the stop limit.
    if (stops > k) {
      continue
    }

    // Explore all connecting flights from the current city.
    adjList[node]?.forEach { (neighbor, price) ->
      val newCost = cost + price

      // If we found a cheaper path to the neighbor, update it and add it to the queue.
      if (newCost < dist[neighbor]) {
        dist[neighbor] = newCost
        queue.add(Triple(neighbor, newCost, stops + 1))
      }
    }
  }

  // 5. Return the result.
  // If the destination's distance is still MAX_VALUE, it was never reached.
  // O(k . E)
  return if (dist[dst] == Integer.MAX_VALUE) -1 else dist[dst]
}

fun main() {
  val flights = arrayOf(intArrayOf(0, 1, 100), intArrayOf(1, 2, 100), intArrayOf(0, 2, 500))
  val result = findCheapestPrice(3, flights, 0, 2, 0)
  assert(result == 500)
}
