// https://leetcode.com/problems/trapping-rain-water/description/

// Problem 42: Trapping Rainwater

public class TrappingRainwater { // in leetcode instead of this there is class solution 
    
        public static int trap(int[] height) {
            int n=height.length;int c1=1;int c2=1;int WT=0;int width=1;
            int LM[]=new int[n];
            LM[0]=height[0]; // LeftMax starts from zero

            //LeftMax boundary calculation using helper array

            for(int i=1;i<n;i++){
                LM[i]=Math.max(height[i],LM[i-1]);
            }

            // RightMax boundary calculation using helper array

            int RM[]=new int[n];
            RM[n-1]=height[n-1];
            for(int i=n-2;i>=0;i--){
                RM[i]=Math.max(height[i],RM[i+1]);
            }

            

            for(int i=0;i<n;i++){
                //WaterTrapped=(waterLevel-height)* width for one block =(min(LM,RM) - height) *width for one block
                    WT+=( (Math.min(LM[i],RM[i]))-height[i]) * width;
            }
            return WT;
        }
        public static void main(String[] args) {
            int height[]={4,2,0,6,3,2,5};
             int ans=trap(height); 
             System.out.println(ans);
        }
}
