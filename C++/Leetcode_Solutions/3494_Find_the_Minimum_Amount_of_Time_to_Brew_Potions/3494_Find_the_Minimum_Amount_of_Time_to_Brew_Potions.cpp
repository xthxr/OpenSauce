#include <vector>
#include <numeric>
#include <algorithm>
#include <iostream>

using namespace std;

/*
 * 3494. Find the Minimum Amount of Time to Brew Potions
 *
 * Time Complexity: O(n * m), where n is the number of wizards (skill.size()) and m is the number of potions (mana.size()).
 * Space Complexity: O(1), excluding the input vectors.
 *
 * Example Usage:
 * Solution sol;
 * vector<int> skill = {1, 2, 3};
 * vector<int> mana = {1, 2};
 * long long result = sol.minTime(skill, mana); // Expected: some value based on calculation
 */

class Solution {
 public:
  long long minTime(vector<int>& skill, vector<int>& mana) {
    long sumSkill = accumulate(skill.begin(), skill.end(), 0L);
    long prevWizardDone = sumSkill * mana[0];

    for (int j = 1; j < mana.size(); ++j) {
      long prevPotionDone = prevWizardDone;
      for (int i = skill.size() - 2; i >= 0; --i) {
        // start time for wizard i brewing potion j
        // = max(end time for wizard i brewing potion j - 1,
        //       the earliest start time for wizard i + 1 brewing potion j
        //       (coming from previous iteration)
        //       - time for wizard i brewing potion j)
        prevPotionDone -= static_cast<long>(skill[i + 1]) * mana[j - 1];
        prevWizardDone =
            max(prevPotionDone,
                prevWizardDone - static_cast<long>(skill[i]) * mana[j]);
      }
      prevWizardDone += sumSkill * mana[j];
    }

    return prevWizardDone;
  }
};

int main() {
    Solution sol;
    
    // Test case 1: Simple case
    vector<int> skill1 = {1, 2};
    vector<int> mana1 = {1};
    cout << "Test 1: " << sol.minTime(skill1, mana1) << endl;
    
    // Test case 2: Multiple potions
    vector<int> skill2 = {1, 2, 3};
    vector<int> mana2 = {1, 2};
    cout << "Test 2: " << sol.minTime(skill2, mana2) << endl;
    
    return 0;
}