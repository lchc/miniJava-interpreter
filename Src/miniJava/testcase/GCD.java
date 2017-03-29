class GCD { 
    public static void main(String[] args) {
        System.out.println(gcd(0,3));
        System.out.println(gcd(1,0));
        System.out.println(gcd(97,37));
        System.out.println(gcd(26,39));
        System.out.println(gcd(3,3));
        System.out.println(gcd(3,3));
        System.out.println(gcd(1000,500));
    }
    public static int gcd(int a,int b) { // Greatest Common Divisor of a and b
        if (a==0) return b;
        return gcd(b%a,a);
    }
}
