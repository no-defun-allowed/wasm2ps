void bubble_sort(char* array, int length) {
  for (int c = 0 ; c < length - 1; c++) {
    for (int d = 0 ; d < length - c - 1; d++) {
      if (array[d] > array[d+1]) {
        char swap = array[d];
        array[d] = array[d+1];
        array[d+1] = swap;
      }
    }
  }
}

int ps_main() {
  char array[] = { 3, 5, 1, 9, 7 };
  bubble_sort(array, 5);
  return array[2];
}
