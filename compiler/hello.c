
// int main() {
//   // const int x = 233, z = 777, y = 666;
//   // const int u = x;
//   // return x + z;
//   int i, j = 114, k;
//   return 0;
// }
// int main() {
//   int x;
//   {
//     int i = 114, j = 514;
//     x = (i + 233) * (j / 666);
//   }
//   return x;
// }
// int main() {
//   int i = 114;
//   if (i >= 233)
//     return 0;
//   return 1;
// }
int main() {
  int a = 2;
  if (a) {
    a = a + 1;
  } else a = 0;  // 在实际写 C/C++ 程序的时候别这样, 建议 if 的分支全部带大括号
  return a;
}

// int main() {
//   return 1 || 233 && 666;
// }
