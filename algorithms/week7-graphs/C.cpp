#include <iostream>
#include <vector>

using namespace std;

void solve(int src, const vector<vector<int>>& tree, vector<int>& ans) {
    int subords = 0;
    for (int child : tree[src]) {
        solve(child, tree, ans);
        subords += (1 + ans[child]);
    }
    ans[src] = subords;
}

int main() {
    int n;
    cin >> n;
    
    vector<vector<int>> tree(n + 1);
    vector<int> ans(n + 1, 0);

    for (int i = 2; i <= n; ++i) {
        int x;
        cin >> x;
        tree[x].push_back(i);
    }

    solve(1, tree, ans);

    for (int i = 1; i <= n; ++i) {
        cout << ans[i] << " ";
    }
    cout << endl;

    return 0;
}