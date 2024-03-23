use selmr::selmr::SELMR;
use std::env;

fn main() {
    env::set_var("RUST_BACKTRACE", "1");

    let mut selmr = SELMR::new(1, 3, 1, 3, 5, 5, "en");

    let _ = selmr.read(
        "E:\\data\\dbpedia\\selmr\\dbpedia_0001_lang=en_test.zip",
        "zip",
    );

    println!(
        "{:?}",
        selmr.most_similar("Winston Churchill", None, 50, 50, 10,)
    );

    println!("{:?}", selmr.get_contexts(Some("France"), None, 50));
}
