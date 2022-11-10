fn main() {
    let mut a = 1;
    let mut b = 0;
    while not(eq(b, 5)) {
        print(a);
        a = mul(a, 2);
        b = add(b, 1);
    }
    a
}
