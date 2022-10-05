# longest palindromic substring


<!--more-->


## Longest-Palindromic-Substring {#longest-palindromic-substring}


### 题目描述 {#题目描述}

Given a string s,return the longest palindromic substring in s。
给定一个字符串s, 返回最长的回文子字符串。

-   example 1
    ```text
    Input: s = "babad"
    Output: "bab"
    Note: "aba" is also a valid answer.
    ```
-   example 2
    ```text
    Input: s = "cbbd"
    Output: "bb"
    ```
-   example 3
    ```text
    Input: s = "a"
    Output: "a"
    ```
-   example 4
    ```text
    Input: s = "ac"
    Output: "a"
    ```
-   限制
    -   1&lt;=s.length &lt;=1000
    -   s consist of only digits and english letters

<https://leetcode-cn.com/problems/longest-palindromic-substring/>


### 题目分析 {#题目分析}

这题应该用动态规划来做。为什么,首先要想使用动态规划是需要满足几个条件的。

1.  问题要有最优子结构 :: 最优子结构就是递归分解后，结构最小的问题
2.  问题要能够递归分解 :: 递归分解，又叫转移方程，问题被不断递归拆解成一个个小的问题
3.  子问题的最优解决定了父问题的最优解
4.  有很多的重叠问题 :: 当问题有重叠问题时，重叠的问题会被不断的计算，这时候可以将重叠
    问题的结果存储起来，下次在次计算这个问题的时候，我们可以直接去取结果，用牺牲少量内存
    的方法来加速计算


#### 具体分析 {#具体分析}

-   首先我们看看能不能递归拆解这个问题
    最长的回文字串，这里有一个性质: 最长的回文字符串，他的子串一定也是一个回文字符串。
    如果他有任意一个字串，他不是回文字符串，那他就不构成回文了。因为回文一定是"a","aa"
    "aba"这三种形式。所以到这里最优子结构也就出来了。
-   最优子结构

    1.  "a"
    2.  "aa"

    任意的回文字符串，一定是在这两个结构的基础上，通过在他们两端加上相同的字符复合而成
-   转移方程
    我们设 pol(i,j)表示从i到j的字符串是否为回文字符串，那么他可以转化成下面的形式
    \\[pol(i,j) = pol(i+1,j-1)\wedge(s[i]==s[j])\\]
    pol(i,j) 要想为真 那么他从i+1 到 j-1 的子字符串必须是回文字符串，s[i]==s[j] 也就是两端必须字符相同的时候。
    \\[pol(i,j) = pol(i+1,j-1)\wedge(s[i]==s[j])\\]所以这就是我们的转移方程式
-   这里，我们还要考虑边界情况
    1.  当字符串长度为1的时候，因为只有一个字符，所以他本身就是回文
    2.  当字符串长度为2的时候，只有两个字符都相等，才表示它是回文

        \begin{equation}
        \begin{cases}
        pol(i,i)=true\\\\
        p(i,i+1)=(s\_{i}==s\_{i+1})
        \end{cases}
        \end{equation}
-   整个递推表达式就是

    \begin{equation}
    pol(i,j)=\begin{cases}
    true &\text{i=j}\\\\
    s\_{i}==s\_{i+1} &\text{j-i=1}\\\\
    pol(i+1,j-i)\wedge(s[i]==s[j]) &j-i>=2
    \end{cases}
    \end{equation}


### 代码实现 {#代码实现}

```cpp
#include <vector>
#include <algorithm>
#include <iostream>
#include <string>
using namespace std;

class Solution {
public:
    string longestPalindrome(string s) {
        vector <vector<bool>> dp(s.length(),vector<bool>(s.length(),0));
        for (int i = 1; i < s.length(); ++i) {
            dp[i][i]=true;
        }
        int longest = 1;
        int start=0;

        for (int i=2; i <= s.length(); ++i) {
            for (int j=0 ; j+i<=s.length(); ++j) {
                if(i==2){
                    dp[j][j+1]=(s[j]==s[j+1]);
                    if(longest <=2 && dp[j][j+1]){
                        longest = i;
                        start=j;
                    }
                    continue;
                }

                dp[j][j+i-1]=dp[j+1][j+i-2] && (s[j]==s[j+i-1]);

                if(dp[j][j+i-1] && i>=longest){
                    longest = i;
                    start=j;
                }
            }
        }

        cout << longest<< endl;

        return s.substr(start, longest);
    }
};

int main() {
    Solution *s = new Solution();
    cout << s->longestPalindrome("bb") <<endl;

}
```

