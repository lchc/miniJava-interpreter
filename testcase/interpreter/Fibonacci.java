class Fibonacci {
    public static int fib(int i) { // the i-th Fibonacci number
        if (i <= 2) return 1;
        return fib(i-1) + fib(i - 2);
    }
    public static void main(String[] args) {
        int i = 1;
        while (i<=10) {
            System.out.println(fib(i));
            i = i + 1;
        }
    }
}
