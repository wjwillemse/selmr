use unicode_segmentation::UnicodeSegmentation;
// use itertools::Itertools;
// use icu_segmenter::SentenceSegmenter;

/// Function used for tokenizing text data
pub fn tokenize(text: &Vec<String>) -> Vec<Vec<&str>> {
    let mut result = Vec::<Vec<&str>>::new();
    for sentence in text {
        let mut tokens = sentence
            .unicode_words()
            .filter(|x| !x.is_empty()) // only tokens with non empty string len
            .collect::<Vec<&str>>();
        tokens.insert(0, "SENTSTART");
        tokens.push("SENTEND");
        result.push(tokens);
    }
    result
}
