# leetcode-最接近目标值得三数之和


<!--more-->


## description {#description}

{{< figure src="/ox-hugo/2022-02-15_15-25-31_screenshot.png" >}}


## analyze {#analyze}

这道题先排序，防止重复枚举，并且利用有序性，来使双指针的逼近产生可能。双指针的逼近其实和那题最大容量是类似的。
假设每轮枚举到的数分别为a,b,c，第二轮第三轮枚举的指针分别为i,pb,pc,令pb指向i+1,pc指向len(nums)-1

-   若a+b+c &gt; target 则应该让pc左移，因为排完序后，pb不动的情况下 只有pc左移才会使a+b+c的值减小 才有可能更加靠近target
-   若a+b+c &lt; target 则应该让pb右移，因为pc不动的情况下，只有pb右移，a+b+c的值才会增大，才有可能更加靠近target


## implementation {#implementation}

```go
package main

import (
    "fmt"
    "math"
    "sort"
)

func threeSumClosest(nums []int, target int) int {
    result := math.MaxInt64

    distance := math.MaxFloat64
    sort.Ints(nums)
    for i := 0; i < len(nums); i++ {
        if i > 0 && nums[i] == nums[i-1] {
            continue
        }
        third := len(nums) - 1
        for j := i + 1; j < len(nums); j++ {

            if j > i+1 && nums[j] == nums[j-1] {
                continue
            }

            for nums[i]+nums[j]+nums[third] > target && third > j {
                if third < len(nums)-1 && nums[third] == nums[third+1] {
                    third--
                    continue
                }
                sum := nums[i] + nums[j] + nums[third]
                tmpDistance := math.Abs(float64(sum - target))
                if tmpDistance <= distance {
                    distance = tmpDistance
                    result = sum
                }
                third--
            }

            if third == j {
                break
            }

            if nums[i]+nums[j]+nums[third] <= target {
                sum := nums[i] + nums[j] + nums[third]
                tmpDistance := math.Abs(float64(sum - target))
                if tmpDistance <= distance {
                    distance = tmpDistance
                    result = sum
                }
            }
        }
    }
    return result
}
```


## summery {#summery}

这道题，乍一看，我就往dp上面去想了。看看这题，跟那题三数只和很相似，就想着能不能在第二轮和第三轮的时候来一次优化。但是我看了半天也没看出来，优先排序这个思路是有的，想着第二轮第三轮进行优化的思路也是对的。但是具体如何优化我却想不到。想着应该跟那个target有关系，但是我方向错了，根本就不是我想的那样。后来一看题解，恍然大悟，这些数组题，双指针的，尤其要三轮演绎的，一般都是想让你进行二三轮的优化，利用双指针逼近不断找到最优解。至于如何逼近，就是问题的关键点了，只要找到能够让指针逼近的关系，这道题自然就解开了。

