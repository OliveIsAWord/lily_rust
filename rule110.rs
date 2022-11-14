fn main() {
    let rule = 110;
    let mut len = 1;
    let mut buffer = alloc(1);
    set(buffer, 0, 1);
    print_slice(buffer, len);
    while sub(len, 15) {
        let new_len = add(len, 1);
        let new_buffer = alloc(new_len);
        let mut i = 0;
        let mut a = 0;
        let mut b = 0;
        let mut c = 0;
        while sub(i, new_len) {
            a = b;
            b = c;
            c = if sub(i, len) { get(buffer, i) } else { 0 };
            let mut rule_index = add(add(mul(4, a), mul(2, b)), c);
            let mut rule_bit = 1;
            while rule_index {
                rule_bit = mul(rule_bit, 2);
                rule_index = sub(rule_index, 1);
            }
            let bit = mod(div(rule, rule_bit), 2);
            set(new_buffer, i, bit);
            i = add(i, 1);
        }
        dealloc(buffer);
        buffer = new_buffer;
        len = new_len;
        print_slice(buffer, len);
    }
    dealloc(buffer);
}
