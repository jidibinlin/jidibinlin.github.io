# Median of Two Sorted Arrays


<!--more-->


## Description {#description}

Given two sorted arrays nums1 and nums2 of size m and n respectively, return the median of the two
sorted arrays. The overall run time complexity should be O(log (m+n)).

来源：力扣（LeetCode）
链接：<https://leetcode-cn.com/problems/median-of-two-sorted-arrays>
著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

1.  Example 1:
    ```nil
    Input: nums1 = [1,3], nums2 = [2]
    Output: 2.00000
    Explanation: merged array = [1,2,3] and median is 2.
    ```

2.  Example 2:
    ```nil
    Input: nums1 = [1,2], nums2 = [3,4]
    Output: 2.50000
    Explanation: merged array = [1,2,3,4] and median is (2 + 3) / 2 = 2.5.
    ```

3.  Example 3:
    ```nil
    Input: nums1 = [0,0], nums2 = [0,0]
    Output: 0.00000
    ```

4.  Example 4:
    ```nil
    Input: nums1 = [], nums2 = [1]
    Output: 1.00000
    ```
5.  Example 5:
    ```nil
    Input: nums1 = [2], nums2 = []
    Output: 2.00000
    ```
6.  Constraints:
    ```nil
    nums1.length == m
    nums2.length == n
    0 <= m <= 1000
    0 <= n <= 1000
    1 <= m + n <= 2000
    -106 <= nums1[i], nums2[i] <= 106
    ```


## Analyse {#analyse}

这道题最简单的想法就是先归并到一个数组，然后再把中位数找到，但是此时的时间复杂度为\\(o(m+n)\\) 不符合题
意。题目要求的是 \\(o(\log(m+n))\\) 。此时我们能想到的应该就只有二分法了，对于有序数组，二分法总能非常有
效的降低算法的复杂度。但是如何二分成为一个问题。中位数指的是一个数列中间的数。设 len = len(array) 这
里的/为整除

\begin{equation}
\label{中位数公式}
    medium =\begin{cases}
    \dfrac{array[len/2-1] + array[len/2]}{2} \qquad &len\mod 2=0 && \\\\
    \dfrac{array[len/2-1]}{2} \qquad &len\mod 2\neq 0
\end{cases}
\end{equation}

这道题是寻找两个有序数组的中位数，我们可以姑且假设他们已经合并后的数组为 **nums3\*我们要在nums3中寻找中
位数。此时 \*nums3** 的长度我们是知道的(m+n) 那么其中位数的应该为第 k= \\(\frac{m+n}{2}\\) 个数（这里我们先
只看奇数情况。这时我们可以对k进行二分处理，分别找到两个s数组中第\\(\frac{k}{2}\\) 个数进行比较，然后排除
较小的以及它所在数组中在它前面的数。因为他们是不可能成为中位数的。对于 nums1[k/2-1] 和nums2[k/2-1]
在它们之前的只有 k/2-1 + k/2-1 = k -2 个数。即使算上较小的那个数，也只能到第k-1个数。所以他们是不可
能成为第k个数的。这时我们让 k = k-A(A为已经排除的数的个数) 然后继续对剩下的数组进行同样的操作。 这里
会出现两种情况

1.  如果 nums1[k/2-1] &gt;= nums2[k/2-1] 则直接排除nums1[k/2-1] 及其前面的数
2.  如果 nums1[k/2-1] &lt; nums2[k/2-1] 则直接排除nums2[k/2-1] 及其前面的数

在排除过程中我们还会遇到几种情况

1.  k/2-1 越界，这种情况取最后一个元素
2.  k=1 直接返回较小的元素
3.  数组为空，直接去非空数组中寻找即可


## Implement {#implement}

```cpp
//c++ version
#include <iostream>
#include <vector>
#include <algorithm>

int main(int argc, char *argv[]) {
    Solution s;
    vector<int> nums1 = new vector<int>();
    vector<int> nums2 = new vector<int>();

    for(int i = 1;i<10;i++){
        nums1.push_back(i);
    }
    for(int i= 1;i<10;i=i+2){
        nums2.push_back(i);
    }

    s.findMedianSortedArrays(nums1,nums2);
    return 0;
}

class Solution {
public:
    double findMedianSortedArrays(vector<int> &nums1, vector<int> &nums2) {
        int k = nums1.size() + nums2.size();
        if(k%2 == 0){
            return min(getKthElement(nums1, nums2, k/2+1),getKthElement(nums1, nums2,k/2))/2.0;
        }else{
            return getKthElement(nums1,nums2,k/2);
        }
    }

    double getKthElement(vector<int> &nums1, vector<int> &nums2,int k){
        int index1 = 0;
        int index2 = 0;

        int m = nums1.size();
        int n = nums2.size();

        while (true){
            if (index1 == m){
                return nums2[index2+k-1];
            }
            if(index2 == n){
                return nums1[index1 +k -1];
            }
            if(k == 1){
                return min(nums1[index1],nums2[index2]);
            }

            int newIndex1 = min(index1+k/2-1,m-1);
            int newIndex2 = min(index2+k/2-1,n-1);

            if(nums1[newIndex1] >= nums2[newIndex2]){
                k -= newIndex2 - index2 +1;
                index2 = newIndex2+1;
            }else{
                k -= newIndex1 - index1 +1;
                index1 = newIndex1+1;
            }

        }
    }
};
```

```go
//GO version
package main

import (
    "fmt"
    "math"
)

func findMedianSortedArrays(nums1 []int, nums2 []int) float64 {
    k := int(math.Ceil((float64(len(nums1)) + float64(len(nums2))) / 2))

    if (len(nums1)+len(nums2))%2 == 0 {
        foo1 := getKthElement(nums1, nums2, k)
        foo2 := getKthElement(nums1, nums2, k+1)
        return float64(foo1+foo2) / 2
    } else {
        return float64(getKthElement(nums1, nums2, k))
    }

}

func getKthElement(nums1 []int, nums2 []int, k int) int {
    if len(nums1) == 0 {
        return nums2[k-1]
    }

    if len(nums2) == 0 {
        return nums1[k-1]
    }

    compareIdx := k / 2

    if compareIdx == 0 {
        return min(nums1[0], nums2[0])
    }

    nums1Idx := min(len(nums1)-1, compareIdx-1)
    nums2Idx := min(len(nums2)-1, compareIdx-1)

    if nums1[nums1Idx] >= nums2[nums2Idx] {
        if len(nums2) <= compareIdx {
            return getKthElement(nums1, []int{}, k-(nums2Idx+1))
        }
        return getKthElement(nums1, nums2[compareIdx:], k-(nums2Idx+1))
    } else {
        if len(nums1) <= compareIdx {
            return getKthElement([]int{}, nums2, k-(nums1Idx+1))
        }
        return getKthElement(nums1[compareIdx:], nums2, k-(nums1Idx+1))
    }
}

func min(x, y int) int {
    if x < y {
        return x
    }
    return y
}
```


## summery {#summery}

这道题对二分的运用比较灵活，主要是二分的对象变了，但是思想还在。正常的二分是对数组的长度进行二分，而
此题的二分却是先确定中位数的位置，再利用二分的思想去到两个数组中分别寻找排除，非常巧妙，受益匪浅。

