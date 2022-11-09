fn main() {
    let b = 34;
    let c = if 0 {
        let eee = 333;
        let d = add(b, 1);
        let b = 2;
        d
    } else {
        add(b, 2)
    };
    c
}
