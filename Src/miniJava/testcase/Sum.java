class Sum {
    public static void main(String[] args) {
        int i = 1;
        int sum = 0;
        while (i <= 100000) {
            if (i % 3 == 0 || i % 5 == 1) sum = sum - i;
            else sum = sum + i;
            i = i + 1;
        }
        System.out.println(sum);
    }
}
