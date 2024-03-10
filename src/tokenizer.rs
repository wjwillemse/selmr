use unicode_segmentation::UnicodeSegmentation;

/// Function used for tokenizing text data
pub fn tokenize(contents: &str) -> Vec<&str> {
    contents
        .unicode_words()
        .filter(|x| !x.is_empty()) // only tokens with non empty string len
        .collect::<Vec<&str>>()
    // contents.split(" ").into_iter()
    //     .filter(|x| x.len() > 0) // only tokens with non empty string len
    //     .collect::<Vec<&str>>()
}
