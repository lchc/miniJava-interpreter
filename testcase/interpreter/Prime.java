class Prime {
    public static int j;
    public static void main(String[] args) {
        j = 0;
        int i = 1;
        while (i <= 100) {
            check_prime(i);
            i = i + 1;
        }
        System.out.println(j);
    }
    public static void check_prime(int i) {
        boolean flag = false;
        if (flag = prime(i)) {
            j = j + 1;
            System.out.println(i);
        }
    }
    public static boolean prime(int i) { // if i is a prime number
        if ( i < 2) return false;
        int j = 2;
        while (j < i) {
            if (i%j==0) {
                return false;
            }
            else {
                j = j + 1;
            }
        }
        return true;
    }
}
