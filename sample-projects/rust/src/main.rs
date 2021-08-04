use std::fs;
use std::path::PathBuf;

fn main() {
    println!("Hello, World!")
}

struct Foo {
    field_name: String,
    count: u8,
}

impl Foo {
    pub fn new(bar: String) -> Foo {
        Foo {
            field_name: bar,
            count: 0
        }
    }

    pub fn boop(&mut self) {
        let to_add = 1;
        self.count += to_add
    }
}

impl Drop for Foo {
    fn drop(&mut self) {
        println!("Dropping foo!")
    }
}

enum MyOption<T> {
    TheresNothingHereMan,
    WhoahTheresAThing(T)
}

