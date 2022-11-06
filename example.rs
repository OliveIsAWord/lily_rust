fn main() {
    let b = 34;
    let a = {
        let a = b;
        add(a, 1)
    };
    add(a, a);
    add(a, b)
}
