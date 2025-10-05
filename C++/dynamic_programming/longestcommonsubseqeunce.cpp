#include<bits/stdc++.h>
using namespace std;
map<char,vector<int>> charLocation;
string lenmax(string &a, string &b) {
    if(a.size() > b.size()) {
        return a;
    } 
    return b;
}
string topdown(int i, int j, string &a, string &b, vector<vector<string>> &cache) {
    if(i == (int)a.size() || j == (int)b.size()) {
        return "";
    }
    if(cache[i][j] != "-1") {
        return cache[i][j];
    }
    vector<int> temp = charLocation[a[i]];
    string take = "";
    auto it = lower_bound(temp.begin(), temp.end(), j);
    if (it != temp.end()) {
        int k = *it;
        take = string(1, a[i]) + topdown(i + 1, k + 1, a, b,cache);
    }    
    string notake = topdown(i + 1, j, a, b, cache);
    return lenmax(take, notake);
}
void getinputs(int &n, int &m, string &a, string &b) {
    cout << "ENTER FIRST STRING" << endl;
    cin >> a;
    cout << "ENTER SECOND STRING" << endl;
    cin >> b;
    n = a.size();
    m = b.size();
}
int main(){
    int n, m;
    string a, b;
    getinputs(n, m, a, b);
    int loc = 0;
    for(auto &i : b) {
        charLocation[i].push_back(loc);
        loc++;
    }
    vector<vector<string>> cache(n + 1, vector<string>(m + 1, "0"));
    for(int i = 0 ; i < n; ++i) {
        for(int j = 0 ; j < m; ++j) {
            cache[i][j] = "-1";
        }
    }
    cout << topdown(0, 0, a, b, cache) << endl;
}


// #include<bits/stdc++.h>
// using namespace std;


// string lcs(string &s, string &t, vector<vector<int>> & charFreq, int i, int j, vector<vector<string>> &cache) {
// 	if(i >= (int)s.size() || j >= (int)t.size()) {
// 		return "";
// 	}
// 	if(cache[i][j] != "-1") {
// 		return cache[i][j];
// 	}
// 	string take = "";
// 	vector<int> thatCharFreq = charFreq[s[i] - 'a'];
// 	auto it = lower_bound(thatCharFreq.begin(), thatCharFreq.end(), j);
// 	if(it != thatCharFreq.end()) {
// 		int k = *it;
// 		take = string(1, s[i]) + lcs(s, t, charFreq, i + 1, k + 1, cache);
// 	}
// 	string nottake = lcs(s, t, charFreq, i + 1, j, cache);
// 	string ans;
// 	if(nottake.size() < take.size()) {
// 		ans = take;
// 	} else {
// 		ans = nottake;
// 	}
// 	return cache[i][j] = ans;
// }
// int main() {
// 	string s;
// 	cout << "ENTER THE STRING 1" << endl;
// 	cin >> s;
// 	cout << "ENTER THE STRING 2" << endl;
// 	string t;
// 	cin >> t;
// 	vector<vector<int>> charFreq(26, vector<int>());
// 	int loc = 0;
// 	for(auto &i : t) {
// 		charFreq[i - 'a'].push_back(loc);
// 		loc++;
// 	}
// 	vector<vector<string>> cache( (int)s.size() , vector<string>( (int)t.size(), "-1") );
// 	string longestCommon = lcs(s, t, charFreq, 0, 0, cache);
// 	cout << longestCommon << endl;
// }