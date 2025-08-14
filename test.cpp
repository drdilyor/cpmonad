#include<bits/stdc++.h>
using ll = int64_t;
using namespace std;

int main() {
  cin.tie(0)->sync_with_stdio(false);

  int n;
  cin >> n;
  vector<int> arr(n);
  for (int& i : arr) cin >> i;
  vector<ll> pref(n+1);
  for (int i = 0; i < n; i++)
    pref[i+1] = pref[i] + arr[i];
  int q;
  cin >> q;
  while (q--) {
    int l, r;
    cin >> l >> r;
    cout << pref[r+1] - pref[l] << "\n";
  }

  return 0;
}
