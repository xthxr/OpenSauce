#include<bits/stdc++.h>
using namespace std;
int maxvalue;
int W;
int cache[10000][10000]; // assuming the max size of n to be 99999 and the sum over all weight to be less than 10000
int knap(int loc, int curweight, vector<int> &value, vector<int> &weight) {
    if (loc == value.size()) return 0;
    if (cache[loc][curweight] != -1) {
        return cache[loc][curweight];
    }
    int take = 0;
    if (curweight + weight[loc] <= W)
        take = value[loc] + knap(loc + 1, curweight + weight[loc], value, weight);
    int not_take = knap(loc + 1, curweight, value, weight);
    return cache[loc][curweight] = max(take, not_take);
}
void resetparams() {
    maxvalue = INT_MIN;
    W = 0;
    memset(cache, -1, sizeof(cache));
}
int main() {
    resetparams();
    int n;
    // cout << "Enter the number of objects" << endl;
    cin >> n;
    // cout << "Enter the value" << endl;
    vector<int> value(n);
    for(auto &i : value) cin >> i;
    // cout << "Enter the weight" << endl;
    vector<int> weight(n);
    for(auto &i : weight) cin >> i;
    // cout << "Enter the weight threshold" << endl;
    cin >> W;
    int valueobtain = knap(0, 0, value, weight);
    cout << valueobtain << endl;
}
// EXAMPLE
// 3
// 60 10 120
// 10 20 30
// 50



// #include<bits/stdc++.h> 
// using namespace std;
// int main() {
// 	int n;	
// 	cout << "ENTER THE NUMBER OF ELEMENTS" << endl;
// 	cin >> n;
// 	cout << "ENTER THE ITEM VALUES" << endl;
// 	vector<int> values(n);
// 	for(auto &i : values) {
// 		cin >> i;
// 	}
// 	cout << "ENTER THE ITEM COST" << endl;
// 	vector<int> cost(n);
// 	for(auto &i : cost) {
// 		cin >> i;
// 	}
// 	cout << "ENTER THE CAPACITY" << endl;
// 	int C;
// 	cin >> C;
// 	vector<vector<int>> cache(n + 1, vector<int> (C + 1, -1));
// 	auto knap = [&](int loc, int curC, auto && ref) -> int {
// 		if(loc >= n) {
// 			return 0;
// 		}	
// 		if(cache[loc][curC] != -1) {
// 			return cache[loc][curC];
// 		}
// 		int take = 0;
// 		if(cost[loc] <= curC) {
// 			take = values[loc] + ref(loc + 1, curC - cost[loc], ref);
// 		}
// 		int nottake = ref(loc + 1, curC, ref);
// 		return cache[loc][curC] = max(take, nottake);
// 	};
// 	int maxProf = knap(0, C, knap);
// 	cout << "THE MAXIMUM PROFIT: " << maxProf << endl;
// }