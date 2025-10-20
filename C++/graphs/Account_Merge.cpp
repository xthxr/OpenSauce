// This problem is a very good implementation of Disjoint Set Union (DSU) or Union-Find data structure.
// Leetcode 721

/* Problem statement (brief):
   Given a list of accounts where each account is a list with the name first and then one or more emails, merge accounts that belong to the same person (accounts are the same if they share at least one email). Return the merged accounts with the name first and the emails sorted lexicographically.

   Algorithm —
   We will perform the following steps:

    1) We will map each unique email string to a unique integer id for efficient unions and lookups.
    2) We will initialize a Union-Find structure (parent + rank arrays) sized by the number of unique emails.
    3) We will iterate each account and union all emails in that account together (typically union the first email with every other email in that account).
    4) We will find the representative (root) for each email id and group emails by their root.
    5) We will sort each group's emails and attach the corresponding owner name (the name of any email that belongs to that group; we record the name when we first encounter an email).

Edge cases:
Accounts containing only one email — still becomes its own group.
Duplicate emails inside one account — mapping to the same id prevents double counting.
Multiple accounts with same name but no shared emails — they remain separate merged entries.

Time and space complexity —

Time complexity: O(E α(E) + E log E + A) where:
    E = total number of distinct emails,
    A = total number of email appearances across all accounts,
    α(E) is inverse-Ackermann (practically constant) for union-find operations.
Explanation: mapping and unions iterate over all email appearances (≈ A) and each union/find is α(E);
grouping visits each email once(E); sorting emails across groups costs O(E log E) in total.

Space complexity: O(E + A + N) where:
    E for maps and parent/rank arrays,
    A for input representation (already given but we store groups),
    N = number of accounts for intermediate structures.
Explanation: we store maps from email→id, id→name, parent/rank arrays of size E, and grouping lists that hold each email once.

Sample input and expected output —
    Sample input (accounts represented as list-of-lists):

    accounts = [
        ["John", "johnsmith@mail.com", "john_newyork@mail.com"],
        ["John", "johnnybravo@mail.com"],
        ["John", "johnsmith@mail.com", "john00@mail.com"],
        ["Mary", "mary@mail.com"]
    ]

    Expected output (one valid merged result; emails sorted in each account):

    [
        ["John","john00@mail.com","john_newyork@mail.com","johnsmith@mail.com"],
        ["John","johnnybravo@mail.com"],
        ["Mary","mary@mail.com"]
    ]

*/

#include <bits/stdc++.h>
using namespace std;

/*
We will implement accounts merge for LeetCode 721.
All logic lives in the top-level function 'solve'.
main() reads input in a simple CLI format, calls solve, and prints output.
*/

/*
UnionFind class at top-level for clarity and reusability.
We will use union by rank and path compression.
*/
class UnionFind
{
public:
    /* parent vector for each node id */
    vector<int> parent;
    /* rank vector to keep tree shallow */
    vector<int> rankArr;

    /* Constructor: allocate arrays for n elements */
    UnionFind(int n)
    {
        parent.resize(n);
        rankArr.resize(n);
        for (int i = 0; i < n; i = i + 1)
        {
            /* Initially every node is its own parent */
            parent[i] = i;
            /* Initially all ranks are zero */
            rankArr[i] = 0;
        }
    }

    /* Find with path compression */
    int findRoot(int x)
    {
        /* If x is its own parent, it's the root */
        if (parent[x] == x)
        {
            return x;
        }
        /* Otherwise, recursively find root and compress path */
        int root = findRoot(parent[x]);
        parent[x] = root;
        return root;
    }

    /* Union by rank - attach smaller rank tree under larger rank tree */
    void unify(int a, int b)
    {
        int rootA = findRoot(a);
        int rootB = findRoot(b);

        if (rootA == rootB)
        {
            /* Already in the same set */
            return;
        }

        if (rankArr[rootA] < rankArr[rootB])
        {
            parent[rootA] = rootB;
            return;
        }

        if (rankArr[rootA] > rankArr[rootB])
        {
            parent[rootB] = rootA;
            return;
        }

        /* Ranks equal -> make rootA the parent and increase its rank */
        parent[rootB] = rootA;
        rankArr[rootA] = rankArr[rootA] + 1;
    }
};

/*
AccountMerge function that performs the merge accounts logic.
Input: accounts as vector<vector<string>>& where each inner vector has name then emails.
Output: merged accounts as vector<vector<string>>.
*/
vector<vector<string>> accountMerge(vector<vector<string>> &accounts)
{
    /* Map each email to a unique integer id */
    unordered_map<string, int> emailToId;
    /* Map the first-seen id to the account owner's name */
    unordered_map<int, string> idToName;
    int nextId = 0;

    /* Assign ids to all unique emails and record owner names for first-seen */
    for (int i = 0; i < (int)accounts.size(); i = i + 1)
    {
        string ownerName = accounts[i][0];
        for (int j = 1; j < (int)accounts[i].size(); j = j + 1)
        {
            string email = accounts[i][j];
            if (emailToId.find(email) == emailToId.end())
            {
                emailToId[email] = nextId;
                idToName[nextId] = ownerName;
                nextId = nextId + 1;
            }
        }
    }

    /* Create union-find over all unique emails */
    UnionFind uf(nextId);

    /* Union emails within each account: union first email with each other */
    for (int i = 0; i < (int)accounts.size(); i = i + 1)
    {
        if ((int)accounts[i].size() <= 1)
        {
            /* No emails found (shouldn't happen per problem) */
        }
        else
        {
            /* Id of the first email in this account */
            string firstEmail = accounts[i][1];
            int firstId = emailToId[firstEmail];

            for (int j = 2; j < (int)accounts[i].size(); j = j + 1)
            {
                string otherEmail = accounts[i][j];
                int otherId = emailToId[otherEmail];
                /* Union the firstId and otherId */
                uf.unify(firstId, otherId);
            }
        }
    }

    /* Group emails by their root representative */
    unordered_map<int, vector<string>> groups;
    for (auto &kv : emailToId)
    {
        string email = kv.first;
        int id = kv.second;
        int rootId = uf.findRoot(id);
        groups[rootId].push_back(email);
    }

    /* Build final merged accounts */
    vector<vector<string>> merged;
    merged.clear();

    for (auto &kv : groups)
    {
        int rootId = kv.first;
        vector<string> emails = kv.second;

        /* Sort emails lexicographically as required */
        sort(emails.begin(), emails.end());

        /* Start merged entry with the owner's name */
        vector<string> mergedEntry;
        mergedEntry.clear();
        string ownerName = idToName[rootId];
        mergedEntry.push_back(ownerName);

        /* Append emails to the entry, skipping duplicates if any */
        for (int i = 0; i < (int)emails.size(); i = i + 1)
        {
            if (i == 0)
            {
                mergedEntry.push_back(emails[i]);
            }
            else
            {
                if (emails[i] != emails[i - 1])
                {
                    mergedEntry.push_back(emails[i]);
                }
            }
        }

        merged.push_back(mergedEntry);
    }

    return merged;
}

int main()
{
    int n;
    cin >> n;
    vector<vector<string>> accounts;
    accounts.clear();

    for (int i = 0; i < n; i = i + 1)
    {
        string name;
        cin >> name;
        int k;
        cin >> k;
        vector<string> account;
        account.clear();
        account.push_back(name);
        for (int j = 0; j < k; j = j + 1)
        {
            string email;
            cin >> email;
            account.push_back(email);
        }
        accounts.push_back(account);
    }

    vector<vector<string>> result = accountMerge(accounts);

    /* Print number of merged accounts followed by each merged entry */
    cout << result.size() << "\n";
    for (int i = 0; i < (int)result.size(); i = i + 1)
    {
        for (int j = 0; j < (int)result[i].size(); j = j + 1)
        {
            cout << result[i][j];
            if (j + 1 < (int)result[i].size())
            {
                cout << " ";
            }
        }
        cout << "\n";
    }

    return 0;
}
