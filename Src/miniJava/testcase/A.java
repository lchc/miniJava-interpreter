class A{
    public static int i;
    public static int fib(int x) {
        if (x == 1) { return 1; }
        if (x == 2) return 1;
        return (fib(x-1) + fib(x-2)) % 23333;
    }
    public static void main(String[] a){
        int k = 1;
        while (k <= 100) {
            int i = k;
            if (prime(k)) System.out.println(k);
            k = k + 1;
        }
        k = 1;
        while (k <= 10) {
            System.out.println(b(k));
            k = k + 1;
        }
        int x = 1;
        while (x <= 25) {
            System.out.println(fib(x));
            x = x+1;
        }
        int t = 1;
        int i = 1;
        while (i <= 10000) {
            //if (!(i % 131 == 0)) t = t * i % 131;
            t = t * i % 23333;
            i = i + 1;
        }
        System.out.println(t);
        System.out.println(ComputeFac(10000));
    }
    public static boolean prime(int x) {
        if (x < 2) return false;
        int i = 2;
        while (i * i <= x) {
            if (x%i==0) return false;
            i = i+1;
        }
        return true;
    }
    public static int a(int x) {
        if (x == 1) return 1;
        return b(x-1);
    }
    public static int b(int x) {
        if (x == 1) return 1;
        return a(x-1) + b(x-1);
    }
    public static int ComputeFac(int num){
        int num_aux ;
        if (num < 1)
            num_aux = 1 ;
        else 
            num_aux = num * (ComputeFac(num-1)) % 23333;
        return num_aux ;
    }
}
