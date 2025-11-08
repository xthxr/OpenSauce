class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

    def sortList(head):
        arr = []
        temp1 = head
        while temp1 != None:
            arr.append(temp1.val)
            temp1 = temp1.next
        arr.sort()
        temp2 = head
        while temp2 != None:
            temp2.val = arr.pop(0)
            temp2 = temp2.next
        return head
    

def __main():
    head = ListNode(4)
    head.next = ListNode(2)
    head.next.next = ListNode(1)
    head.next.next.next = ListNode(3)

    temp = head

    while temp:
        print(temp.val, end=" -> ")
        temp = temp.next
    print("None")

    sorted_head = ListNode.sortList(head)
    while sorted_head:
        print(sorted_head.val, end=" -> ")
        sorted_head = sorted_head.next
    print("None")

    
__main()