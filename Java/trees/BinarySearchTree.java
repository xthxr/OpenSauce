import java.util.Scanner;
class BinarySearchTree{
    class TreeNode{
        TreeNode left;
        int data;
        TreeNode right;
        TreeNode(int data){
            this.left = null;
            this.data = data;
            this.right = null;
        }
    }

    public TreeNode root;

    BinarySearchTree(){
        this.root = null;
    }

    int height(TreeNode node){
        int x,y;
        if (node!=null){
            x = height(node.left);
            y= height(node.right);
            if (x>y){
                return x+1;
            }else {
                return y+1;
            }
        }
        return 0;
    }

    TreeNode search(int key){
        TreeNode temp = root;
        while (temp!=null){
            if (temp.data == key){
                return temp;
            }else if (temp.data > key){
                temp = temp.left;
            }else {
                temp = temp.right;
            }
        }
        return null;
    }

    void insert(int data){
        TreeNode newNode = new TreeNode(data);
        TreeNode temp = root;
        TreeNode tail = null;
        if (root==null){
            root = newNode;
            return;
        }
        while (temp!=null){
            tail = temp;
            if (data == temp.data){
                System.out.println("Element Already Present.");
                return;
            }else if (temp.data>data){
                temp = temp.left;
            }else {
                temp = temp.right;
            }
        }
        if (data > tail.data){
            tail.right = newNode;
        }else{
            tail.left = newNode;
        }
    }

    void inOrer(TreeNode node){
        if (node==null){
            return;
        }
        inOrer(node.left);
        System.out.print(node.data+" ");
        inOrer(node.right);
    }

    TreeNode inOrderPredecssor(TreeNode node){
        while (node != null && node.right!=null){
            node = node.right;
        }
        return node;
    };

    TreeNode inOrderSuccessor(TreeNode node){
        while (node!=null && node.left!=null){
            node = node.left;
        }
        return node;
    }

    TreeNode remove(TreeNode node,int data){
        if (node == null){
            return null;
        }
        if (node.data>data){
            node.left = remove(node.left,data);
        } else if (node.data<data) {
            node.right = remove(node.right,data);
        }else {
            if (node.left==null){
                return node.right;
            } else if (node.right==null) {
                return node.left;
            }

            if (height(node.left) > height(node.right)){
                TreeNode predecssor = inOrderPredecssor(node.left);
                node.data = predecssor.data;
                node.left = remove(node.left,predecssor.data);
            }else {
                TreeNode successor = inOrderSuccessor(node.right);
                node.data = successor.data;
                node.right = remove(node.right,successor.data);
            }
        }
        return node;
    }

   public static void main(String[] args){
        Scanner scan = new Scanner(System.in);
        int[] nums = {30,15,50,10,20,40,60};
        BinarySearchTree bst = new BinarySearchTree();
        for(int num : nums){
            bst.insert(num);
        }

        while (true){
            System.out.println();
            System.out.print("1.Insert\n2.Search\n3.Inorder\n4.Remove\nYour Choice:- ");
            int choice = scan.nextInt();
            switch (choice){
                case 1:System.out.print("Enter the element:- ");
                    int data = scan.nextInt();
                    bst.insert(data);
                break;
                case 2:System.out.print("Enter the element to search:- ");
                    int key = scan.nextInt();
                    TreeNode result = bst.search(key);
                    if(result==null){
                        System.out.println("Element Not found");
                    }else{
                        System.out.println("Elment found "+result);
                    }
                    break;
                case 3:System.out.println("Inorder");
                bst.inOrer(bst.root);
                break;
                case 4: System.out.print("Enter the elment to remove:- ");
                    data = scan.nextInt();
                    bst.remove(bst.root, data);
                    break;
                default:System.out.println("Choose from above options only.");
            }
        }

    }
}