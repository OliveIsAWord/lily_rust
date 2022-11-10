fn main() {
    let mut a = 0;
    let mut b = 1;
    let mut count = 10;
    while count {
        let aa = b;
        let bb = add(a, b);
        a = aa;
        b = bb;
        print(b);
        count = sub(count, 1);
    }
}
