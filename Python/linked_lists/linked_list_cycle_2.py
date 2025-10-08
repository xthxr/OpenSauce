# TC=O(N)
# SC=O(1)
class Node:
    def __init__(self,val):
        self.val=val
        self.next=None
class LinkedList:
    def __init__(self):
        self.head=None
    def append(self,val):
        new_node=Node(val)
        if self.head==None:
            self.head=new_node
        else:
            temp=self.head
            while temp.next is not None:
                temp =temp.next
            temp.next=new_node
    def traverse(self):
        temp=self.head
        while temp:
            print(temp.val,end=" ")
            temp=temp.next
        print()
    def cycle(self):
        slow=self.head
        fast=self.head
        while fast is not None and fast.next is not None:
            slow=slow.next
            fast=fast.next.next
            if slow==fast:
                slow=self.head
                while slow!=fast:
                    slow=slow.next
                    fast=fast.next
                
                return slow
        return None
    