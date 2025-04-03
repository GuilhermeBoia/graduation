#include <iostream>
#include <vector>
using namespace std;

const int MOD = 1e9 + 7;

int solve(int n, int x, const vector<int>& coins) {
    vector<int> count(x + 1, 0);
    count[0] = 1;

    for (int i = 1; i <= x; ++i) {
        for (int coin : coins) {
            if (i - coin >= 0) {
                count[i] = (count[i] + count[i - coin]) % MOD;
            }
        }
    }

    return count[x];
}

int main() {
    int n, x;
    cin >> n >> x;
    vector<int> coins(n);
    for (int i = 0; i < n; ++i) {
        cin >> coins[i];
    }

    cout << solve(n, x, coins) << endl;
    return 0;
}