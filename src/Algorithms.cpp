#include "Algorithms.h"
#include <cstdio>
#include <cstring>
#include <algorithm>


/*
template<typename T>
void insertion_sort(T array[], int length) {
  for (int i = 1; i < length; ++i) {
    T a = array[i];
    int j = i;
    for (; j > 0 && a < array[j - 1]; --j);
    if (j != i) {
      memmove(array + j + 1, array + j, (i - j) * sizeof(T));
    }
    array[j] = a;
  }
}
*/

// This version, not using the memmove, is significantly faster... why?
template<typename T>
void insertion_sort(T array[], int length) {
  for (int i = 1; i < length; ++i) {
    T a = array[i];
    int j = i;
    for (; j > 0 && a < array[j - 1]; --j) {
        array[j] = array[j - 1];
    }
    array[j] = a;
  }
}

void insertion_sort_i(int array[], int length) {
  insertion_sort<int>(array, length);
}

void introsort_i(int array[], int length) {
  std::sort(array, array + length);
}

/*
int main(int argc, char *argv[]) {
  int length = 5;
  int array[5] = {2, 1, 7, 1, 0};
  insertion_sort_i(array, length);
  for (int i = 0; i < length; ++i) {
    printf("%d ", array[i]);
  }
}
*/
