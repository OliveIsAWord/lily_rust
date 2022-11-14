fn main() {
    let len = 5;
    let x = alloc(len);
    set(x, 2, 420);
    let y = get(x, 2);
    print(y);
    print_slice(x, len);
    dealloc(x);
}
