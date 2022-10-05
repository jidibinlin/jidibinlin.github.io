# 3sum


<!--more-->


## Problem description {#problem-description}

{{< figure src="/ox-hugo/2022-02-12_15-24-53_screenshot.png" >}}


## Analyse {#analyse}

这道题难点在于不重复的三元组，但是三重枚举后不断通过哈希去去重的消耗太高不合适。解决这个难点的可以先将数组进行排序，然后按照顺序去枚举当遇到相同的元素时，直接跳过，这样就避免了之后需要哈希去重的麻烦事。
这道题的三重枚举可以优化成两重。当 a+b+c = 0(a&lt;=b&lt;=c)，在进行下一次二重枚举的若有满足条件的组合,会有 a+b\`+c\`=0(b\`&gt;b &amp;&amp; c\`&lt;c) 这意味着，我们不必在第三重枚举时枚举所有剩下的元素，只需要枚举比上一次满足要求组合中比c小的元素即可。
我们可以采用双指针的思想，让第二轮的second 固定，不断向左平移thrid下标


## Implement {#implement}

```go
package main

import (
    "fmt"
    "sort"
)

func threeSum(nums []int) [][]int {

    if len(nums) < 3 {
        return [][]int{}
    }

    result := [][]int{}
    sort.Ints(nums)

    for i := 0; i <= len(nums)-3; i++ {
        if i > 0 && nums[i] == nums[i-1] {
            continue
        }

        k := len(nums) - 1
        target := -1 * nums[i]

        for j := i + 1; j <= len(nums)-2; j++ {
            if j > i+1 && nums[j] == nums[j-1] {
                continue
            }

            for j < k && nums[j]+nums[k] > target {
                k--
            }

            if j == k {
                break
            }

            if nums[j]+nums[k] == target {
                result = append(result, []int{nums[i], nums[j], nums[k]})
            }

        }
    }
    return result
}
```

```c++
#include <algorithm>
#include <vector>

using namespace std;
class Solution {
public:
    vector<vector<int>> threeSum(vector<int>& nums) {

        vector<vector<int>> result;

        if (nums.size() < 3) {
            return result;
        }

        sort(nums.begin(),nums.end());

        for (int i=0; i<nums.size(); ++i) {
            if (i>0 && nums[i]==nums[i-1]) {
                continue;
            }

            int third = nums.size()-1;
            int target = -nums[i];

            for (int j=i+1; j<nums.size(); j++) {
                if (j>i+1 && nums[j] == nums[j-1]) {
                    continue;
                }

                while(j<third && nums[j]+nums[third] > target){
                    third--;
                }

                if (third == j) {
                    break;
                }

                if(nums[j]+nums[third] == target){
                    result.push_back({nums[i],nums[j],nums[third]});
                }
            }
        }
        return result;
    }
};
```


## Summery {#summery}

解题的时候还是应该多多思考题目已知条件所带来的一些性质，这题就用到了数字可以排序的性质来解决重复枚举的问题。新的第三轮的枚举值必定小于上一次成功的第三轮枚举值，可以用来优化代码。

