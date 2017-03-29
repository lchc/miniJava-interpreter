class Deep {
    public static int j;
    public static int sum(int i) {
        if (i == 0) return 0;
        if (i % 3 == 0) {
            j = j + 1;
            return sum(i - 1) - i - j;
        }
        else {
            j = j - 1;
            return sum(i - 1) + i + j;
        }
    }
    public static void main(String[] args) {
        j = 0;
        System.out.println(sum(10010));
    }
}

