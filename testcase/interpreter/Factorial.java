class Factorial {
    public static int fact(int i) { //i * (i-1) * (i-2) * ... * 1
        if (i <= 0) return 1;
        return i * fact(i-1);
    }
    public static void main(String[] args) {
        System.out.println(fact(5));
        System.out.println(fact(10));
        System.out.println(fact(12));
    }
}
