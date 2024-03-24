use selmr::selmr::SELMR;
use std::env;

fn main() {
    env::set_var("RUST_BACKTRACE", "1");

    let mut selmr = SELMR::new(1, 3, 1, 3, 5, 5, "en");

    let _ = selmr.read(
        "E:\\data\\dbpedia\\selmr\\dbpedia_0010_lang=en_test.zip",
        "zip",
    );
    let binding = selmr.matches(("to", "^[a-z]*ize$", ".*")).expect("REASON");
    let words_ize = binding
        .keys()
        .map(|(_, p, _)| p.as_str())
        .collect::<Vec<_>>();
    println!("{:?}", selmr.get_contexts(None, Some(words_ize), 15));
    let binding = selmr
        .matches(("the|a|an", "^[a-z]*ion$", ".*"))
        .expect("REASON");
    let words_ion = binding
        .keys()
        .map(|(_, p, _)| p.as_str())
        .collect::<Vec<_>>();
    println!("{:?}", selmr.get_contexts(None, Some(words_ion), 15));
    println!("{:?}", selmr.get_phrases(("the", "of"), 15));
}
