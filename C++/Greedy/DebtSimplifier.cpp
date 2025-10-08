#include <iostream>
#include <map>
#include <vector>
#include <string>
#include <algorithm>
#include <iomanip>
using namespace std;

class DebtSimplifier {
public:
    static map<string, map<string, double>> simplifyDebts(
        const map<string, map<string, double>>& groupBalances) {
        
        // Step 1: Calculate net amount for each person
        map<string, double> netAmounts;

        // Initialize all users with 0
        for (const auto& userBalance : groupBalances) {
            netAmounts[userBalance.first] = 0;
        }

        // Step 2: Calculate net balances
        for (const auto& userBalance : groupBalances) {
            string creditorId = userBalance.first;
            for (const auto& balance : userBalance.second) {
                string debtorId = balance.first;
                double amount = balance.second;

                if (amount > 0) {
                    netAmounts[creditorId] += amount;   // creditor receives
                    netAmounts[debtorId] -= amount;     // debtor pays
                }
            }
        }

        // Step 3: Separate creditors and debtors
        vector<pair<string, double>> creditors;
        vector<pair<string, double>> debtors;

        for (const auto& net : netAmounts) {
            if (net.second > 0.01) {
                creditors.push_back({net.first, net.second});
            } else if (net.second < -0.01) {
                debtors.push_back({net.first, -net.second});
            }
        }

        // Sort both lists (largest first)
        sort(creditors.begin(), creditors.end(),
             [](const pair<string, double>& a, const pair<string, double>& b) {
                 return a.second > b.second;
             });
        sort(debtors.begin(), debtors.end(),
             [](const pair<string, double>& a, const pair<string, double>& b) {
                 return a.second > b.second;
             });

        // Step 4: Simplify debts
        map<string, map<string, double>> simplifiedBalances;

        // Initialize all users
        for (const auto& userBalance : groupBalances) {
            simplifiedBalances[userBalance.first] = map<string, double>();
        }

        int i = 0, j = 0;
        while (i < (int)creditors.size() && j < (int)debtors.size()) {
            string creditorId = creditors[i].first;
            string debtorId = debtors[j].first;
            double creditorAmount = creditors[i].second;
            double debtorAmount = debtors[j].second;

            double settleAmount = min(creditorAmount, debtorAmount);

            simplifiedBalances[creditorId][debtorId] = settleAmount;
            simplifiedBalances[debtorId][creditorId] = -settleAmount;

            creditors[i].second -= settleAmount;
            debtors[j].second -= settleAmount;

            if (creditors[i].second < 0.01) i++;
            if (debtors[j].second < 0.01) j++;
        }

        return simplifiedBalances;
    }
};

int main() {
    cout << fixed << setprecision(2);
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    // Example input (can be replaced with user input)
    map<string, map<string, double>> groupBalances;

    // A simple example setup:
    // groupBalances["A"]["B"] = 200; means B owes A ₹200
    groupBalances["A"]["B"] = 200;
    groupBalances["A"]["C"] = 300;
    groupBalances["B"]["C"] = 100;
    groupBalances["C"]["A"] = 50;

    cout << "=== Original Balances ===\n";
    for (const auto& user : groupBalances) {
        for (const auto& bal : user.second) {
            cout << user.first << " → " << bal.first << " : " << bal.second << "\n";
        }
    }
    cout << "\n";

    // Simplify debts
    auto simplified = DebtSimplifier::simplifyDebts(groupBalances);

    cout << "=== Simplified Balances ===\n";
    for (const auto& user : simplified) {
        for (const auto& bal : user.second) {
            if (bal.second > 0.01)
                cout << user.first << " → " << bal.first << " : " << bal.second << "\n";
        }
    }

    cout << "\nProgram finished successfully ✅\n";
    return 0;
}
