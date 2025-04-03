#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main() {
    int n, max_price;
    cin >> n >> max_price;

    vector<int> prices(n);
    vector<int> pages(n);
    
    for (int i = 0; i < n; i++) {
        cin >> prices[i];
    }
    
    for (int i = 0; i < n; i++) {
        cin >> pages[i];
    }

    vector<long long> dp(max_price + 1, 0);

    for (int i = 0; i < n; i++) {
        for (int j = max_price; j >= prices[i]; j--) {
            dp[j] = max(dp[j], dp[j - prices[i]] + pages[i]);
        }
    }

    cout << dp[max_price] << endl;

    return 0;
}