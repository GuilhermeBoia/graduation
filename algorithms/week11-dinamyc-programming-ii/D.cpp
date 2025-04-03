#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

long long maxScore(int i, int j, const vector<int>& nums, vector<vector<long long>>& dp) {
    if (i > j) {
        return 0;
    }

    if (dp[i][j] != -1) {
        return dp[i][j];
    }

    long long pickLeft = nums[i] + min(maxScore(i + 2, j, nums, dp), maxScore(i + 1, j - 1, nums, dp));
    long long pickRight = nums[j] + min(maxScore(i + 1, j - 1, nums, dp), maxScore(i, j - 2, nums, dp));

    dp[i][j] = max(pickLeft, pickRight);
    return dp[i][j];
}

int main() {
    int n;
    cin >> n;
    vector<int> nums(n);

    for (int i = 0; i < n; ++i) {
        cin >> nums[i];
    }

    vector<vector<long long>> dp(n, vector<long long>(n, -1));
    long long result = maxScore(0, n - 1, nums, dp);

    cout << result << endl;
    return 0;
}