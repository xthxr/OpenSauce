/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     TreeNode *left;
 *     TreeNode *right;
 *     TreeNode() : val(0), left(nullptr), right(nullptr) {}
 *     TreeNode(int x) : val(x), left(nullptr), right(nullptr) {}
 *     TreeNode(int x, TreeNode *left, TreeNode *right) : val(x), left(left), right(right) {}
 * };
 */

class Solution {
public:
    std::vector<std::vector<int>> levelOrder(TreeNode* root) {
        std::vector<std::vector<int>> ans;
        if(root == nullptr) return ans;
        std::queue<TreeNode*> q;
        q.push(root);
        while(!q.empty()){
            std::vector<int> lvl;
            int n = q.size();
            for(int i = 0; i < n; i++){
                TreeNode* node = q.front();
                q.pop();
                if(node->left != nullptr) q.push(node->left);
                if(node->right != nullptr) q.push(node->right);
                lvl.push_back(node->val);
            }
            ans.push_back(lvl);
        }
        return ans;
    }
};