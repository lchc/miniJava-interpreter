class Power {
    public static void main(String[] a) {
        System.out.println(pow(2,10,10000));
        System.out.println(pow(2,1000000,131));
        System.out.println(pow(10,1000000,9));
        System.out.println(pow(1341,9023,31));
        System.out.println(pow(5,0,100));
        System.out.println(pow(123,987,3));
    }
    public static int pow(int a,int x,int p) { // a^x mod p
        int ret = 1;
        while (x > 0) {
            if (x % 2 == 1) ret = ret * a % p;
            a = a * a % p;
            x = x / 2;
        }
        return ret;
    }
}
