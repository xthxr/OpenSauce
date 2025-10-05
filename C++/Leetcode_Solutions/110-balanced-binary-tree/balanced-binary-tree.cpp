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
    int height(TreeNode* root){
        if(root == NULL) return 0;
        return 1 + max(height(root->left),height(root->right));
    }
    bool isBalanced(TreeNode* root) {
        if(root==NULL) return true;
        queue<TreeNode*> q;
        q.push(root);
        while(!q.empty()){
            TreeNode* temp = q.front();
            int heightLeft = 0;
            int heightRight = 0;
            q.pop();
            if(temp->left){
                heightLeft = height(temp->left);
                q.push(temp->left);
            }
            if(temp->right){
                heightRight = height(temp->right);
                q.push(temp->right);
            }
            if (abs(heightLeft - heightRight) > 1) return false;
        }
        return true;
    }
};