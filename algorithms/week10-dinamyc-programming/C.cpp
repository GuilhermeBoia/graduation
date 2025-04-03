#include <iostream>
#include <vector>
#include <climits>

int main() {
    int n, x;
    std::cin >> n >> x;
    std::vector<int> coins(n);
    
    for (int i = 0; i < n; ++i) {
        std::cin >> coins[i];
    }

    std::vector<int> valor(x + 1, INT_MAX);
    valor[0] = 0;

    for (int i = 1; i <= x; ++i) {
        for (int c : coins) {
            if (i - c >= 0 && valor[i - c] != INT_MAX && valor[i - c] + 1 < valor[i]) {
                valor[i] = valor[i - c] + 1;
            }
        }
    }

    if (valor[x] == INT_MAX) {
        std::cout << -1 << std::endl;
    } else {
        std::cout << valor[x] << std::endl;
    }

    return 0;
}